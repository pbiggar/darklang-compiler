(*
   ParallelMoveTestRunner.mli - Executes parallel-move lowering fixtures.
   Lowers LIR TailArgMoves and compares the complete symbolic ARM64 sequence.
*)
val runParallelMoveTest : ParallelMoveFormat.parallelMoveTest -> TestOutcome.t

val loadParallelMoveTests :
  string -> (ParallelMoveFormat.parallelMoveTest list, string) result

val tests : string array -> (string * (unit -> (unit, string) result)) list
