(*
   Operands.fs - Materialize target operands, registers, and immediates.
   Resolve a LIR.FReg to x86-64 XMM register.
   Emit inline 8-byte-at-a-time copy of a UTF-8 byte array to heap memory.
   Stores bytes starting after the two-word dynamic-buffer header.
*)
[@@@warning "-4"]
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b=Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
let mul a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let syscalls=Platform.linuxX86_64SyscallNumbers
let physName = function
 | LIR.X0 -> "X0"
 | LIR.X1 -> "X1"
 | LIR.X2 -> "X2"
 | LIR.X3 -> "X3"
 | LIR.X4 -> "X4"
 | LIR.X5 -> "X5"
 | LIR.X6 -> "X6"
 | LIR.X7 -> "X7"
 | LIR.X8 -> "X8"
 | LIR.X9 -> "X9"
 | LIR.X10 -> "X10"
 | LIR.X11 -> "X11"
 | LIR.X12 -> "X12"
 | LIR.X13 -> "X13"
 | LIR.X14 -> "X14"
 | LIR.X15 -> "X15"
 | LIR.X16 -> "X16"
 | LIR.X17 -> "X17"
 | LIR.X19 -> "X19"
 | LIR.X20 -> "X20"
 | LIR.X21 -> "X21"
 | LIR.X22 -> "X22"
 | LIR.X23 -> "X23"
 | LIR.X24 -> "X24"
 | LIR.X25 -> "X25"
 | LIR.X26 -> "X26"
 | LIR.X27 -> "X27"
 | LIR.X29 -> "X29"
 | LIR.X30 -> "X30"
 | LIR.SP -> "SP"
let invalidX64PhysRegReason = function
 | LIR.X22 -> Some "X22 maps to the x64 heap pointer runtime register"
 | LIR.X23 -> Some "X23 maps to the x64 free-list runtime register"
 | (LIR.X24 | LIR.X25 | LIR.X26) as reg -> Some (physName reg^" has no allocatable x64 register mapping")
 | LIR.X27 -> Some "X27 is reserved runtime state and cannot be lowered on x64"
 | LIR.X0 | LIR.X1 | LIR.X2 | LIR.X3 | LIR.X4 | LIR.X5 | LIR.X6 | LIR.X7 | LIR.X8 | LIR.X9 | LIR.X10 | LIR.X11 | LIR.X12 | LIR.X13 | LIR.X14 | LIR.X15 | LIR.X16 | LIR.X17 | LIR.X19 | LIR.X20 | LIR.X21 | LIR.X29 | LIR.X30 | LIR.SP -> None
(*
   Map LIR.PhysReg to x86-64 register
   Return value
   Arg 1
   Arg 2
   Arg 3 (NOT RDX — RDX is reserved for IDIV)
   Arg 4
   Arg 5
   Arg 6 / caller-saved
   Caller-saved (only used when IDIV isn't active)
   Scratch
   Scratch (shared)
   Callee-saved 1
   Callee-saved 2
   Callee-saved 3
   Frame pointer
   Link register (not applicable on x86_64)
*)
let lirRegToX86 = function
 | LIR.X0 -> X86_64.RAX
 | LIR.X1 -> X86_64.RDI
 | LIR.X2 -> X86_64.RSI
 | LIR.X3 -> X86_64.RCX
 | LIR.X4 -> X86_64.R8
 | LIR.X5 -> X86_64.R9
 | LIR.X6 -> X86_64.R10
 | LIR.X7 -> X86_64.RDX
 | LIR.X8 -> X86_64.R11
 | LIR.X9 -> X86_64.R11
 | LIR.X10 -> X86_64.R11
 | LIR.X11 -> X86_64.R11
 | LIR.X12 -> X86_64.R11
 | LIR.X13 -> X86_64.R11
 | LIR.X14 -> X86_64.R11
 | LIR.X15 -> X86_64.R11
 | LIR.X16 -> X86_64.R11
 | LIR.X17 -> X86_64.R11
 | LIR.X19 -> X86_64.RBX
 | LIR.X20 -> X86_64.R12
 | LIR.X21 -> X86_64.R13
 | LIR.X29 -> X86_64.RBP
 | LIR.X30 -> X86_64.RAX
 | LIR.SP -> X86_64.RSP
 | (LIR.X22 | LIR.X23 | LIR.X24 | LIR.X25 | LIR.X26 | LIR.X27) as reg -> Crash.crash ("lirRegToX86: invalid x64 physical register "^physName reg)
let resolvePhysReg context reg =
 match invalidX64PhysRegReason reg with Some reason -> Error (context^": invalid x64 physical register "^physName reg^": "^reason) | None -> Ok (lirRegToX86 reg)
(*
   Map LIR.FReg to x86-64 XMM register
*)
let lirFRegToX86 = function
 | LIR.D0 -> X86_64.XMM0
 | LIR.D1 -> X86_64.XMM1
 | LIR.D2 -> X86_64.XMM2
 | LIR.D3 -> X86_64.XMM3
 | LIR.D4 -> X86_64.XMM4
 | LIR.D5 -> X86_64.XMM5
 | LIR.D6 -> X86_64.XMM6
 | LIR.D7 -> X86_64.XMM7
 | LIR.D8 -> X86_64.XMM8
 | LIR.D9 -> X86_64.XMM9
 | LIR.D10 -> X86_64.XMM10
 | LIR.D11 -> X86_64.XMM11
 | LIR.D12 -> X86_64.XMM12
 | LIR.D13 -> X86_64.XMM13
 | LIR.D14 -> X86_64.XMM14
 | LIR.D15 -> X86_64.XMM15
let[@warning "-32"] resolveFreg = function
 | LIR.FPhysical fp -> Ok (lirFRegToX86 fp)
 | LIR.FVirtual id -> Error (Printf.sprintf "Unresolved virtual float register f%d in x86-64 codegen" id)
(*
   Resolve a LIR.Reg (Physical or Virtual) to x86-64 register.
*)
let resolveReg = function
 | LIR.Physical phys -> resolvePhysReg "resolveReg" phys
 | LIR.Virtual id -> Error (Printf.sprintf "Unresolved virtual register v%d in x86-64 codegen" id)
(*
   Load a 64-bit immediate into a register.
*)
let loadImm64 dest value =
 if value=0L then [X86_64.XOR_reg (dest,dest)]
 else if value>=Int64.of_int32 Int32.min_int && value<=Int64.of_int32 Int32.max_int then [X86_64.MOV_imm32 (dest,Int64.to_int32 value)] else [X86_64.MOV_imm (dest,value)]
(*
   Scratch register for temporaries in codegen
*)
let scratch=X86_64.R11
let arithmeticTempExcluding excluded =
 let candidates=[X86_64.R11;X86_64.RCX;X86_64.R10;X86_64.RAX;X86_64.RDX;X86_64.RDI;X86_64.RSI;X86_64.R8;X86_64.R9;X86_64.RBX;X86_64.R12;X86_64.R13] in
 match List.find_opt (fun candidate -> not (List.mem candidate excluded)) candidates with Some temp -> temp | None -> Crash.crash "x64 arithmetic lowering could not find a temporary register"
let allFloatRegs=[X86_64.XMM0;X86_64.XMM1;X86_64.XMM2;X86_64.XMM3;X86_64.XMM4;X86_64.XMM5;X86_64.XMM6;X86_64.XMM7;X86_64.XMM8;X86_64.XMM9;X86_64.XMM10;X86_64.XMM11;X86_64.XMM12;X86_64.XMM13;X86_64.XMM14;X86_64.XMM15]
(*
   Borrow an XMM register for a short lowering sequence without reserving one
   globally from allocation. The 16-byte slot preserves stack alignment.
*)
let withPreservedFloatScratch excluded build =
 let temp=match List.find_opt (fun candidate -> not (List.mem candidate excluded)) allFloatRegs with Some temp -> temp | None -> Crash.crash "x64 float lowering has no scratch register" in
 [X86_64.SUB_imm (X86_64.RSP,16l);X86_64.MOVSD_store (X86_64.RSP,0l,temp)]@build temp@[X86_64.MOVSD_load (temp,X86_64.RSP,0l);X86_64.ADD_imm (X86_64.RSP,16l)]
(*
   Heap bump pointer register (codegen-internal, reserved; not allocatable).
*)
let heapPtr=X86_64.R14
(*
   Free list base register (codegen-internal, reserved; not allocatable).
*)
let freeListBase=X86_64.R15
(*
   Size of free list heads area (32 size classes × 8 bytes = 256 bytes)
*)
let freeListSize=256
(*
   The process table is raw runtime state, not a managed allocation. Keeping
   it at a fixed address avoids aliasing a free-list head in batched programs.
*)
let processTableOffset=freeListSize
let processTableSize=4096
(*
   Max payload size class for free list reuse (freeListSize - 8)
*)
let maxFreeListPayload=sub freeListSize 8
let[@warning "-32"] emitStringByteCopy valueReg destReg strBytes =
 let len=Bytes.length strBytes in if len=0 then [] else
 let chunks=add len 7 / 8 in
 List.init (max 0 chunks) (fun i ->
 let offset=add 16 (mul i 8) in let chunkLen=min 8 (sub len (mul i 8)) in
 let value=List.fold_left (fun acc j -> let byteIdx=add (mul i 8) j in if byteIdx<Bytes.length strBytes then Int64.logor acc (Int64.shift_left (Int64.of_int (Char.code (Bytes.get strBytes byteIdx))) (j*8)) else acc) 0L (List.init (max 0 chunkLen) Fun.id) in
 loadImm64 valueReg value@[X86_64.MOV_store (destReg,Int32.of_int offset,valueReg)]) |> List.concat
(*
   Load a string from the executable's immutable literal pool.
*)
let emitStringLiteral destReg value=[X86_64.LEA_rip (destReg,X86_64.stringLiteralLabel value)]
(*
   File-operation path buffers use the canonical static buffer layout.
*)
let emitStringLiteralNoRefCount=emitStringLiteral
(*
   Heap size for mmap (512 MB)
*)
let heapMmapSizeBytes=Int64.mul (Int64.mul 512L 1024L) 1024L
(*
   Generate x86-64 write(fd, buf, len) syscall
*)
let genWriteSyscall=loadImm64 X86_64.RAX (Int64.of_int syscalls.Platform.write)@[X86_64.SYSCALL]
(*
   Write a small compile-time byte sequence to stdout from a balanced stack
   buffer. The generated syscall may clobber RAX, RCX, and R11.
*)
let genPrintChars bytes =
 let len=List.length bytes in if len=0 then [] else
 let padded=mul ((add len 7)/8) 8 in let paddedBytes=bytes@List.init (sub padded len) (fun _ -> '\000') in
 let rec chunks acc=function [] -> List.rev acc | values -> let rec take n kept rest=if n=0 then List.rev kept,rest else match rest with [] -> List.rev kept,[] | x::xs -> take (n-1) (x::kept) xs in let chunk,rest=take 8 [] values in chunks (chunk::acc) rest in
 let pushes=chunks [] paddedBytes |> List.rev |> List.concat_map (fun chunk -> let value=List.mapi (fun index value -> Int64.shift_left (Int64.of_int (Char.code value)) (index*8)) chunk |> List.fold_left Int64.logor 0L in loadImm64 scratch value@[X86_64.PUSH scratch]) in
 pushes@[X86_64.MOV_imm32 (X86_64.RDI,1l);X86_64.MOV_reg (X86_64.RSI,X86_64.RSP)]@loadImm64 X86_64.RDX (Int64.of_int len)@genWriteSyscall@[X86_64.ADD_imm (X86_64.RSP,Int32.of_int padded)]
(*
   Generate x86-64 exit(code) syscall.
   Exit code must already be in RDI.
*)
let genExitSyscall=loadImm64 X86_64.RAX (Int64.of_int syscalls.Platform.exit)@[X86_64.SYSCALL]
(*
   Label for shared OOM handler (set per-program, not per-function)
*)
let oomHandlerLabel="__heap_oom"
let runtimeErrorHandlerLabel="__dark_runtime_error"
(*
   Generate a jump to the shared OOM handler
*)
let genOomJump ()=[X86_64.JMP oomHandlerLabel]
(*
   Generate the shared OOM handler code (placed once at end of program)
*)
let genOomHandler ()=[X86_64.Label oomHandlerLabel]@emitStringLiteral X86_64.R8 "Out of heap memory\n"@[X86_64.JMP runtimeErrorHandlerLabel]
(*
   Shared non-returning writer for canonical error-string buffers in R8.
*)
let genRuntimeErrorHandler ()=[X86_64.Label runtimeErrorHandlerLabel;X86_64.MOV_load (X86_64.RDX,X86_64.R8,8l);X86_64.LEA (X86_64.RSI,X86_64.R8,16l);X86_64.MOV_imm32 (X86_64.RDI,2l)]@genWriteSyscall@loadImm64 X86_64.RDI 1L@genExitSyscall
(*
   Mutable counter for generating unique labels within a compilation
*)
let labelCounter=ref 0
let freshLabel prefix=labelCounter:=add !labelCounter 1;Printf.sprintf "__%s_%d" prefix !labelCounter
