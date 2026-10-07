(* X64EmitCalls.ml - Emit x64 instructions for calls operations. *)
open X64Operands
open X64Frames
open X64CodeGenTypes
(*
   Save caller-saved registers that are live across a call.
   PUSH each in order — first pushed is deepest on the stack.
   SUB RSP, 8; MOVSD [RSP], xmm
   Track save area size for RestoreRegs/ArgMoves
*)
let emitSaveRegs (_ctx:funcCtx) intRegs floatRegs=
 if intRegs=[] && floatRegs=[] then Ok [] else
 let intSaves=List.map (fun reg -> X86_64.PUSH (lirRegToX86 reg)) intRegs in
 let floatSaves=List.concat_map (fun freg -> let xmm=lirFRegToX86 freg in [X86_64.SUB_imm (X86_64.RSP,8l);X86_64.MOVSD_store (X86_64.RSP,0l,xmm)]) floatRegs in
 Ok (intSaves@floatSaves)
(*
   Restore in reverse order of saves
*)
let emitRestoreRegs (_ctx:funcCtx) intRegs floatRegs=
 if intRegs=[] && floatRegs=[] then Ok [] else
 let floatRestores=List.rev floatRegs |> List.concat_map (fun freg -> let xmm=lirFRegToX86 freg in [X86_64.MOVSD_load (xmm,X86_64.RSP,0l);X86_64.ADD_imm (X86_64.RSP,8l)]) in
 let intRestores=List.rev intRegs |> List.map (fun reg -> X86_64.POP (lirRegToX86 reg)) in
 Ok (floatRestores@intRestores)
(*
   Arguments are already in place from ArgMoves
*)
let emitCall ctx dest funcId _args=resolveReg dest |> Result.map (fun destReg -> [X86_64.CALL (functionName ctx funcId)]@(if destReg<>X86_64.RAX then [X86_64.MOV_reg (destReg,X86_64.RAX)] else []))
(*
   Restore stack frame before jumping (epilogue without RET)
*)
let emitTailCall ctx funcId _args=Ok (genEpilogue ctx.stackSize ctx.usedCalleeSaved@[X86_64.JMP (functionName ctx funcId)])
let emitIndirectCall (_ctx:funcCtx) dest func _args=match resolveReg func with Error error->Error error|Ok funcReg->resolveReg dest |> Result.map (fun destReg -> [X86_64.CALL_reg funcReg]@(if destReg<>X86_64.RAX then [X86_64.MOV_reg (destReg,X86_64.RAX)] else []))
let emitIndirectTailCall ctx func _args=resolveReg func |> Result.map (fun funcReg -> genEpilogue ctx.stackSize ctx.usedCalleeSaved@[X86_64.JMP_reg funcReg])
let emitLoadFuncAddr ctx dest funcId=resolveReg dest |> Result.map (fun destReg -> [X86_64.LEA_rip (destReg,functionName ctx funcId)])
(*
   The closure register contains the function pointer
   (LIR does HeapLoad to extract func_ptr before ClosureCall)
   Move to R10 if in scratch (R11) to avoid conflicts
*)
let emitClosureCall (_ctx:funcCtx) dest closure _args=match resolveReg closure with Error error->Error error|Ok closureReg->resolveReg dest |> Result.map (fun destReg ->
 let callReg=if closureReg=scratch then X86_64.R10 else closureReg in
 let setup=if callReg<>closureReg then [X86_64.MOV_reg (callReg,closureReg)] else [] in
 setup@[X86_64.CALL_reg callReg]@(if destReg<>X86_64.RAX then [X86_64.MOV_reg (destReg,X86_64.RAX)] else []))
let emitClosureTailCall ctx closure _args=resolveReg closure |> Result.map (fun closureReg -> let callReg=if closureReg=scratch then X86_64.R10 else closureReg in let setup=if callReg<>closureReg then [X86_64.MOV_reg (callReg,closureReg)] else [] in setup@genEpilogue ctx.stackSize ctx.usedCalleeSaved@[X86_64.JMP_reg callReg])
