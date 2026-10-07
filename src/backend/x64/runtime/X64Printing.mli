(* X64Printing.mli - Generate x64 scalar printing and heap initialization support. *)
val genPrintInt64 : X86_64.reg -> bool -> X86_64.instr list
val genPrintUInt64 : X86_64.reg -> bool -> X86_64.instr list
val genHeapInit : unit -> X86_64.instr list
