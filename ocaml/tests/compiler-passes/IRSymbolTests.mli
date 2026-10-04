(* Original symbolic pool, function-identity and MIR-to-LIR tests. *)
type testResult=(unit,string) result
val testFunctionIdentitiesDoNotHashCollide : unit -> testResult
val testFunctionIdentitiesComposeAcrossUnits : unit -> testResult
val testMirToLirSymbolicOperands : unit -> testResult
val testMirToLirReportsMissingEntryBlock : unit -> testResult
val testMirToLirUsesNativeInt64ShiftMask : unit -> testResult
val testMirToLirAllocatesFloatHeapStoreTemporary : unit -> testResult
val testMirToLirUsesImmediateMaskForListToRawPtr : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
