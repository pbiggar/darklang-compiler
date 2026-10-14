(* E2EDiscovery.mli - Discover parser cases and effective gates without executing them. *)
val applyUpstreamEnablementGate : E2EFormat.e2eTest -> E2EFormat.e2eTest
val exclusionOrigin : E2EFormat.e2eTest -> string option
val loadE2ETests : string array -> E2EFormat.e2eTest array * (string * string) list
val yamlString : string -> string
val renderInventory : (string -> string) -> string array -> string * bool
