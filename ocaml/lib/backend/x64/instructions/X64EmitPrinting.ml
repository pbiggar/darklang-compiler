(* Printing.fs - Emit x64 instructions for printing operations. *)
[@@@warning "-4"]
open X64Operands
open X64Printing
open X64CodeGenTypes
module X=X86_64
let utf8Bytes str=
 let units=HostText.utf16Units str in let output=Buffer.create (Array.length units) in
 let rec loop index=if index<Array.length units then
 let value=units.(index) in
 if value>=0xd800 && value<=0xdbff && index+1<Array.length units && units.(index+1)>=0xdc00 && units.(index+1)<=0xdfff then
 (Uutf.Buffer.add_utf_8 output (Uchar.of_int (0x10000+((value-0xd800) lsl 10)+units.(index+1)-0xdc00));loop (index+2))
 else (Uutf.Buffer.add_utf_8 output (Uchar.of_int (if value>=0xd800 && value<=0xdfff then 0xfffd else value));loop (index+1)) in
 loop 0;Bytes.of_string (Buffer.contents output)
let emitPrintChars (_ctx:funcCtx) bytes=Ok (genPrintChars bytes)
let emitPrintInt64 (_ctx:funcCtx) reg=resolveReg reg |> Result.map (fun srcReg -> genPrintInt64 srcReg true)
let emitPrintUInt64 (_ctx:funcCtx) reg=resolveReg reg |> Result.map (fun srcReg -> genPrintUInt64 srcReg true)
let emitPrintInt64NoNewline (_ctx:funcCtx) reg=resolveReg reg |> Result.map (fun srcReg -> genPrintInt64 srcReg false)
let emitPrintUInt64NoNewline (_ctx:funcCtx) reg=resolveReg reg |> Result.map (fun srcReg -> genPrintUInt64 srcReg false)
(*
   Print "true\n" or "false\n" without exiting (Ret handles exit)
   "false\n"
   "true\n"
*)
let emitPrintBool (_ctx:funcCtx) reg=resolveReg reg |> Result.map (fun srcReg ->
 let trueLabel=freshLabel "bool_true" in let writeLabel=freshLabel "bool_write" in
 [X.TEST_reg (srcReg,srcReg);X.Jcc (X.NE,trueLabel);X.SUB_imm (X.RSP,8l)] @ loadImm64 scratch 0x0a65736c6166L
 @ [X.MOV_store (X.RSP,0l,scratch);X.MOV_reg (X.RSI,X.RSP);X.MOV_imm32 (X.RDX,6l);X.JMP writeLabel;X.Label trueLabel;X.SUB_imm (X.RSP,8l)] @ loadImm64 scratch 0x0a65757274L
 @ [X.MOV_store (X.RSP,0l,scratch);X.MOV_reg (X.RSI,X.RSP);X.MOV_imm32 (X.RDX,5l);X.Label writeLabel;X.MOV_imm32 (X.RDI,1l)] @ genWriteSyscall @ [X.ADD_imm (X.RSP,8l)])
(*
   TODO: implement bool printing without exit
*)
let emitPrintBoolNoNewline (_ctx:funcCtx) reg=resolveReg reg |> Result.map (fun _srcReg -> [])
(*
   Dynamic string format: [refcount:8][length:8][data:N]
   Print data + newline (exit handled by subsequent Ret → epilogue)
*)
let emitPrintHeapString (_ctx:funcCtx) reg=resolveReg reg |> Result.map (fun srcReg ->
 [X.PUSH srcReg] @ (if srcReg=X.RDX then [X.MOV_reg (X.R10,srcReg);X.MOV_load (X.RDX,X.R10,8l);X.LEA (X.RSI,X.R10,16l)] else [X.MOV_load (X.RDX,srcReg,8l);X.LEA (X.RSI,srcReg,16l)])
 @ [X.MOV_imm32 (X.RDI,1l)] @ genWriteSyscall @ [X.SUB_imm (X.RSP,8l)] @ loadImm64 scratch 10L
 @ [X.MOV_store (X.RSP,0l,scratch);X.MOV_imm32 (X.RDI,1l);X.MOV_reg (X.RSI,X.RSP);X.MOV_imm32 (X.RDX,1l)] @ genWriteSyscall @ [X.ADD_imm (X.RSP,8l);X.POP srcReg])
let emitPrintHeapStringNoNewline (_ctx:funcCtx) reg=resolveReg reg |> Result.map (fun srcReg ->
 [X.PUSH srcReg] @ (if srcReg=X.RDX then [X.MOV_reg (X.R10,srcReg);X.MOV_load (X.RDX,X.R10,8l);X.LEA (X.RSI,X.R10,16l)] else [X.MOV_load (X.RDX,srcReg,8l);X.LEA (X.RSI,srcReg,16l)]) @ [X.MOV_imm32 (X.RDI,1l)] @ genWriteSyscall @ [X.POP srcReg])
(*
   Write a literal string to stdout and exit(0)
   fd = stdout
*)
let emitPrintString (_ctx:funcCtx) str=
 let bytes=utf8Bytes (str^"\n") in let len=Bytes.length bytes in let padded=((len+7)/8)*8 in
 let paddedBytes=Bytes.make padded '\000' in Bytes.blit bytes 0 paddedBytes 0 len;
 let pushInstrs=List.init (padded/8) (fun index -> Bytes.get_int64_le paddedBytes (index*8)) |> List.rev |> List.concat_map (fun value -> loadImm64 scratch value@[X.PUSH scratch]) in
 Ok (pushInstrs@[X.MOV_imm32 (X.RDI,1l);X.MOV_reg (X.RSI,X.RSP)]@loadImm64 X.RDX (Int64.of_int len)@genWriteSyscall@[X.ADD_imm (X.RSP,Int32.of_int padded)]@loadImm64 X.RDI 0L@genExitSyscall)
(*
   Call Stdlib.Float.toString(D0), print result as heap string
*)
let emitPrintFloat (_ctx:funcCtx) freg=match freg with
 | LIR.FPhysical fp->let xmm=lirFRegToX86 fp in Ok ((if xmm<>X.XMM0 then [X.MOVSD_reg (X.XMM0,xmm)] else [])@[X.CALL "Darklang.Stdlib.Float.toString";X.MOV_load (X.RDX,X.RAX,8l);X.LEA (X.RSI,X.RAX,16l);X.MOV_imm32 (X.RDI,1l)]@genWriteSyscall@[X.SUB_imm (X.RSP,8l)]@loadImm64 scratch 10L@[X.MOV_store (X.RSP,0l,scratch);X.MOV_imm32 (X.RDI,1l);X.MOV_reg (X.RSI,X.RSP);X.MOV_imm32 (X.RDX,1l)]@genWriteSyscall@[X.ADD_imm (X.RSP,8l)])
 | _->Error "PrintFloat with virtual FP register"
(*
   Call Stdlib.Float.toString(D0), print result without newline
*)
let emitPrintFloatNoNewline (_ctx:funcCtx) freg=match freg with
 | LIR.FPhysical fp->let xmm=lirFRegToX86 fp in Ok ((if xmm<>X.XMM0 then [X.MOVSD_reg (X.XMM0,xmm)] else [])@[X.CALL "Darklang.Stdlib.Float.toString";X.MOV_load (X.RDX,X.RAX,8l);X.LEA (X.RSI,X.RAX,16l);X.MOV_imm32 (X.RDI,1l)]@genWriteSyscall)
 | _->Error "PrintFloatNoNewline with virtual FP register"
(*
   TODO: implement list printing
*)
let emitPrintList (_ctx:funcCtx) listPtr _elemType=resolveReg listPtr |> Result.map (fun _ -> loadImm64 X.RDI 0L@genExitSyscall)
let emitPrintSum (_ctx:funcCtx) sumPtr _variants=resolveReg sumPtr |> Result.map (fun _ -> loadImm64 X.RDI 0L@genExitSyscall)
let emitPrintRecord (_ctx:funcCtx) recordPtr _typeName _fields=resolveReg recordPtr |> Result.map (fun _ -> loadImm64 X.RDI 0L@genExitSyscall)
let emitPrintBlob (_ctx:funcCtx) reg=resolveReg reg |> Result.map (fun _ -> loadImm64 X.RDI 0L@genExitSyscall)
