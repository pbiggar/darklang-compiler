(*
   SSAOptimizationTests.ml - Execute plain-text SSA optimization cases.
*)
(* SSAOptimizationTests.ml - Execute original SSA text fixtures. *)
let tests =
  SSAInliningFormat.testsFromFile "test/fixtures/ssa-optimization/ssa.opt"
