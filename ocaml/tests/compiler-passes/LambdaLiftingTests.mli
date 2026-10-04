(* Original LambdaLiftingTests declarations. *)
type testResult=(unit,string) result
val testLetBoundTupleReturnType : unit -> testResult
val tests : (string * (unit -> testResult)) list
