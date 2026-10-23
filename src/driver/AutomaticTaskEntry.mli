(* AutomaticTaskEntry.mli - Prepare a CLI root using the explicit runtime's drain policy. *)
val prepare :
  (CompilationContexts.pipelineContext ->
  SpecializationIdentity.SpecSet.t ->
  (CompilationContexts.pipelineContext * ANF.functionDef list, string) result) ->
  CompilationContexts.pipelineContext ->
  ANF.functionDef list ->
  ( CompilationContexts.pipelineContext
    * ANF.functionDef list
    * ANF.functionDef list,
    string )
  result
