(* SSAInliningFormat.mli - Parse and run original SSA inlining and optimization fixtures. *)
val testsFromFile : string -> (string * (unit -> (unit, string) result)) list
