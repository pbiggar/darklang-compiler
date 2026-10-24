(* SpecializationGrowth.mli - Check generic parameter flow before enumerating instances. *)
val check :
  SpecializationIdentity.genericFuncDefs ->
  (SpecializationIdentity.genericFunctionArtifact ->
  SpecializationIdentity.SpecSet.t) ->
  SpecializationIdentity.SpecSet.t ->
  (unit, string) result
