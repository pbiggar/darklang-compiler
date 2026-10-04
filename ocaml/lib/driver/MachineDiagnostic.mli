(* Typed structural instruction spelling for compiler assembly diagnostics. *)
val x64 : X86_64.instr -> string
val arm64 : ARM64.instr -> string
val symbolic : Symbolic.instr -> string
