(* NativePortRegressionTests.mli - Behavior checks for native text and backend repairs. *)
val textTests : (string * (unit -> (unit, string) result)) list
val machoTests : (string * (unit -> (unit, string) result)) list
val printingTests : (string * (unit -> (unit, string) result)) list
