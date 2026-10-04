(* Original generic specialization tests. *)
type testResult=(unit,string) result
val testPreservesTypeVarsInSpecialization : unit -> testResult
val testReplaceTypeAppsWithRegistry : unit -> testResult
val testReplaceTypeAppsWithRegistryMissingSpec : unit -> testResult
val testSpecializeFromSpecs : unit -> testResult
val tests : (string * (unit -> testResult)) list
