(* TestOutcome.ml - Preserve pass-test result evidence without runner dependencies. *)
type t = {
  success : bool;
  message : string;
  expected : string option;
  actual : string option;
}
