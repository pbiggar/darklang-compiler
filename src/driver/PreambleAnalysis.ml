(* PreambleAnalysis.ml - Check reusable source preambles against explicit base environments. *)
(* Parse and check a preamble directly from interpreter syntax. *)
let analyzePreamble allowInternal (stdlib:CompilationContexts.stdlibResult) preamble=
 WrittenParsing.parse Validation.Script preamble
 |> Result.map_error (fun error->"Preamble parse error: "^error)
 |> fun parsed->Result.bind parsed (fun preambleAST->
  WrittenChecking.checkSourceUnitsWithBase stdlib.CompilationContexts.context.CompilationContexts.writtenEnvironment allowInternal false [preambleAST]
  |> Result.map_error (fun error->"Preamble type error: "^error)
  |> Result.map (fun (_,typedAST,writtenEnvironment)->
   let typeCheckEnv=Types.mergeTypeCheckEnv stdlib.CompilationContexts.context.CompilationContexts.typeCheckEnv (WrittenChecking.typeCheckEnvironment typedAST) in
   let genericFuncDefs=SpecializationIdentity.extractGenericFuncDefs typedAST in
   {CompilationContexts.typedAST;typeCheckEnv;writtenEnvironment=Some writtenEnvironment;genericFuncDefs}))
