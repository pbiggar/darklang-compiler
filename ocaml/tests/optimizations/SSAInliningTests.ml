(*
   The cases and expectations live in the text fixture, not in F# test code.
*)
(* SSAInliningTests.fs - Execute original SSA text fixtures. *)
let tests = SSAInliningFormat.testsFromFile "src/Tests/inlining/ssa.inline"
