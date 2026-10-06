(*
   HeapAllocation.fs - Generate checked heap allocation and fatal allocation paths.
*)
open ARM64CodeGenTypes
let dataLabel name=Symbolic.DataLabel (Symbolic.Named name)
let stringDataLabel value=Symbolic.DataLabel (Symbolic.StringLiteral value)
let floatDataLabel value=Symbolic.DataLabel (Symbolic.FloatLiteral value)
let codeLabel name=Symbolic.CodeLabel name
let runtimeInstrs=Symbolic.ofARM64List
let utf8Len = String.length
let loadStringLiteralPointer destReg value =
 let labelRef=stringDataLabel value in [Symbolic.ADRP (destReg,labelRef);Symbolic.ADD_label (destReg,destReg,labelRef)]
let generateHeapOverflowTrapBody _target =
 loadStringLiteralPointer Symbolic.X0 heapOutOfMemoryMessage@[Symbolic.MOVZ (Symbolic.X3,0,0);Symbolic.B_label runtimeErrorHelperLabel]
(*
   Shared non-returning error writer. X0 is a fixed-header string buffer and X3
   selects the newline required by dynamically constructed exception text.
*)
let generateRuntimeErrorHelper target =
 let syscalls=ARM64.targetSyscalls target in let exitLabel=runtimeErrorHelperLabel^"_exit" in
 [Symbolic.Label runtimeErrorHelperLabel;Symbolic.LDR (Symbolic.X2,Symbolic.X0,8);Symbolic.ADD_imm (Symbolic.X1,Symbolic.X0,16);Symbolic.MOVZ (Symbolic.X0,2,0);Symbolic.MOVZ (syscalls.ARM64.syscallRegister,syscalls.ARM64.numbers.Platform.write,0);Symbolic.SVC syscalls.ARM64.svcImmediate;Symbolic.CBZ (Symbolic.X3,exitLabel)]
 @loadStringLiteralPointer Symbolic.X1 "\n"
 @[Symbolic.ADD_imm (Symbolic.X1,Symbolic.X1,16);Symbolic.MOVZ (Symbolic.X0,2,0);Symbolic.MOVZ (Symbolic.X2,1,0);Symbolic.MOVZ (syscalls.ARM64.syscallRegister,syscalls.ARM64.numbers.Platform.write,0);Symbolic.SVC syscalls.ARM64.svcImmediate;Symbolic.Label exitLabel;Symbolic.MOVZ (Symbolic.X0,1,0);Symbolic.MOVZ (syscalls.ARM64.syscallRegister,syscalls.ARM64.numbers.Platform.exit,0);Symbolic.SVC syscalls.ARM64.svcImmediate]
(*
   These cold paths depend only on the target ABI. Building their immutable
   instruction lists once avoids reconstructing the same message and syscall
   sequence for every executable in a compilation session.
*)
let macOSHeapOverflowTrapBody=generateHeapOverflowTrapBody (ARM64.targetConfigFor Platform.MacOSARM64)
let linuxHeapOverflowTrapBody=generateHeapOverflowTrapBody (ARM64.targetConfigFor Platform.LinuxARM64)
let preparedHeapOverflowTrapBody target=match ARM64.targetOS target with Platform.MacOS -> macOSHeapOverflowTrapBody | Platform.Linux -> linuxHeapOverflowTrapBody
let generateHeapOverflowTrapBlock body label=Symbolic.Label label::body
let withHeapBoundsCheck overflowLabel nextPtrInstrs allocInstrs =
 [Symbolic.MOVZ (Symbolic.X11,heapMmapSizeMovzImm16,16);Symbolic.ADD_reg (Symbolic.X11,Symbolic.X27,Symbolic.X11)]@nextPtrInstrs@[Symbolic.CMP_reg (Symbolic.X14,Symbolic.X11);Symbolic.B_cond_label (Symbolic.GT,overflowLabel)]@allocInstrs
let checkedBumpAllocReg overflowLabel destReg sizeReg =
 withHeapBoundsCheck overflowLabel [Symbolic.ADD_reg (Symbolic.X14,Symbolic.X28,sizeReg)] [Symbolic.MOV_reg (destReg,Symbolic.X28);Symbolic.ADD_reg (Symbolic.X28,Symbolic.X28,sizeReg)]
