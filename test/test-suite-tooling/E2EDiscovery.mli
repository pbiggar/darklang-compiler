(* E2EDiscovery.mli - Discover fixtures using only per-test source metadata. *)
val loadE2ETests :
  string array -> E2EFormat.e2eTest array * (string * string) list
