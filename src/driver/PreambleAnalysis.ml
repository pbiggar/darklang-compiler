(* PreambleAnalysis.ml - Check reusable source preambles against explicit base environments. *)
(* Parse and check a preamble directly from interpreter syntax. *)
let analyzeSources allowInternal (stdlib : CompilationContexts.stdlibResult)
    sources =
  WrittenChecking.checkSourceUnitsWithBase
    stdlib.CompilationContexts.context.CompilationContexts.writtenEnvironment
    allowInternal false sources
  |> Result.map_error (fun error -> "Preamble type error: " ^ error)
  |> Result.map (fun (_, typedAST, writtenEnvironment) ->
      let typedAST =
        SourcePreparation.importInheritedValues None
          stdlib.CompilationContexts.context.CompilationContexts.checkedValues
          typedAST
      in
      let typeCheckEnv =
        Types.mergeTypeCheckEnv
          stdlib.CompilationContexts.context.CompilationContexts.typeCheckEnv
          (WrittenChecking.typeCheckEnvironment typedAST)
      in
      let genericFuncDefs =
        SpecializationIdentity.extractGenericFuncDefs typedAST
      in
      {
        CompilationContexts.typedAST;
        typeCheckEnv;
        writtenEnvironment = Some writtenEnvironment;
        genericFuncDefs;
      })

let analyzePreamble allowInternal stdlib preamble =
  WrittenParsing.parse Validation.Script preamble
  |> Result.map_error (fun error -> "Preamble parse error: " ^ error)
  |> fun parsed ->
  Result.bind parsed (fun source ->
      analyzeSources allowInternal stdlib [ source ])
