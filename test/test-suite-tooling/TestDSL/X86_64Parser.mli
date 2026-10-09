(*
   X86_64Parser.mli - Parser for the x64 instruction subset used by encoding fixtures.
   Uses constructor-style syntax matching the x64 instruction discriminated union.
*)
val parseX64 : string -> (Dark_compiler.X86_64.instr list, string) result
