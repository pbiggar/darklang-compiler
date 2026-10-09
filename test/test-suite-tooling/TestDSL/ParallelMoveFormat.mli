(*
   ParallelMoveFormat.mli - Parser for parallel-move lowering fixtures.
   Converts compact destination/operand pairs and expected symbolic ARM64 into typed cases.
*)
type parallelMoveTest = {
  name : string;
  moves : (Dark_compiler.LIR.physReg * Dark_compiler.LIR.operand) list;
  expected : Dark_compiler.Symbolic.instr list;
  sourceFile : string;
}

val parseParallelMoveFileContent :
  string -> string -> (parallelMoveTest list, string) result
