(* X64Fibers.mli - Fixed-address stack allocation and cooperative context switching. *)
val emit :
  X64CodeGenTypes.funcCtx ->
  X86_64.reg ->
  LIR.cliOperation ->
  (X86_64.instr list, string) result
