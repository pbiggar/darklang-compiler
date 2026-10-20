(* E2EDiscovery.mli - Load fixture assertions and apply upstream enablement gates. *)
val applyUpstreamEnablementGate : E2EFormat.e2eTest -> E2EFormat.e2eTest
val loadE2ETests : string array -> E2EFormat.e2eTest array * (string * string) list
