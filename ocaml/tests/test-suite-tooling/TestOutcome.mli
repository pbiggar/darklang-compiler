(* TestOutcome.mli - Shared complete compiler pass test outcomes. *)
type t = {success : bool; message : string; expected : string option; actual : string option}
