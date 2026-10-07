(*
   LIRExecutionTestRunner.mli - Compiles and executes single-block LIR programs.
   Checks typed x64 failures and provides shared backend executable support.
*)
val executeProgram : Dark_compiler.Platform.target -> Dark_compiler.LIR.program -> LIRExecutionFormat.leakCheckMode -> (int*string*string,string) result
val runLIRExecutionTest : LIRExecutionFormat.lIRExecutionTest -> (unit,string) result
val loadLIRExecutionTests : string -> (LIRExecutionFormat.lIRExecutionTest list,string) result
val tests : string array -> (string*(unit -> (unit,string) result)) list
