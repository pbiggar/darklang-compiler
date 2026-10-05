(* Typed structural instruction spelling for compiler assembly diagnostics. *)
val x64 : X86_64.instr -> string
val arm64 : ARM64.instr -> string
val symbolic : Symbolic.instr -> string
val x64Instr : X86_64.instr -> StructuralValue.value
