(*
   The cases and expectations live in the text fixture, not in OCaml test code.
*)
(* SSAInliningTests.ml - Execute original SSA text fixtures. *)
let tests = SSAInliningFormat.testsFromFile "test/fixtures/inlining/ssa.inline"
