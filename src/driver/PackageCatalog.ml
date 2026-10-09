(* PackageCatalog.ml - Materialize reachable package values and source compilation plans. *)
[@@@warning "-4"]

module X = CompilationContexts
module C = CheckedAST
module M = StringOrder.Map
module S = StringOrder.Set
module F = SpecializationIdentity.FunctionSet

module T = Set.Make (struct
  type t = AST.semanticType

  let compare = AST.compareSemanticType
end)

let ( let* ) = Result.bind

type userCompileLabels = {
  parse : string;
  typeCheck : string;
  anf : string;
  stageSuffix : string;
}

type userCompilePlan = {
  allowInternal : bool;
  mode : CompilerOptions.compileMode;
  verbosity : int;
  options : CompilerOptions.compilerOptions;
  packageValues : X.packageValueCatalog;
  packageManager : PackageManager.config option;
  passTimingRecorder : CompilerOptions.passTimingRecorder option;
  session : CompilationSession.compilationSession option;
  stdlib : X.stdlibResult;
  baseContext : X.pipelineContext;
  monomorphization : SourcePreparation.monomorphizationMode;
  externalInlineCandidates : InliningCommon.functionInfo FunctionIdMap.t;
  prebuiltSymbolicFunctions : LIR.functionDef list;
  prebuiltCallGraphSummaries :
    CompilationCacheIdentity.functionSummary FunctionIdMap.t;
  prebuiltCallGraph : F.t FunctionIdMap.t;
  skipFunctionNames : S.t;
  emitFunctionEvents : bool;
  treeShakeUserFunctions : bool;
  labels : userCompileLabels;
  sources : X.sourceUnit AST.nonEmptyList;
}

(* Parse each source unit with the copied interpreter parser and enforce entry ownership. *)
let parseWrittenSourceProgram ?writtenSources _allowInternal requireEntry
    sources =
  let sources = NonEmptyList.toList sources in
  let* inputs =
    match writtenSources with
    | None -> Ok (List.map (fun source -> (source, None)) sources)
    | Some written when List.length written = List.length sources ->
        Ok (List.combine sources written)
    | Some _ -> Error "Written source count does not match source units"
  in
  let* parsed =
    ResultList.traverse
      (fun ((source : X.sourceUnit), written) ->
        let* name = NameSyntax.sourceUnitName source.X.name in
        let validated =
          match written with
          | None -> WrittenParsing.parse Validation.Script source.X.source
          | Some program ->
              Validation.validate Validation.Script program
              |> Result.map_error (fun issues ->
                  String.concat "\n"
                    (List.map
                       (fun (issue : Validation.issue) ->
                         issue.Validation.message)
                       (NonEmptyList.toList issues)))
        in
        Result.map
          (fun parsed ->
            (NameSyntax.sourceUnitNameText name, source.X.purpose, parsed))
          validated)
      inputs
  in
  WrittenSource.validateSourceUnits requireEntry parsed

let packageHashType = AST.TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])

let packageLocationType =
  AST.TRecord ("Darklang.LanguageTools.ProgramTypes.PackageLocation", [])

let runtimeValueType =
  AST.TSum ("Darklang.LanguageTools.RuntimeTypes.ValueType", [])

let optionType innerType =
  AST.TSum ("Darklang.Stdlib.Option.Option", [ innerType ])

let constructor typeName caseName payload =
  AST.Constructor
    (AST.UnresolvedConstructor (Some typeName), caseName, Option.to_list payload)

let packageHashExpr hash =
  constructor "Darklang.LanguageTools.ProgramTypes.Hash" "Hash"
    (Some (AST.StringLiteral hash))

let optionNoneExpr = constructor "Darklang.Stdlib.Option.Option" "None" None

let optionSomeExpr value =
  constructor "Darklang.Stdlib.Option.Option" "Some" (Some value)

let call name args = AST.applyNamed name (NonEmptyList.fromList args)

let orderedGroups entries =
  let groups = Hashtbl.create 16 in
  let order =
    List.fold_left
      (fun order (key, value) ->
        match Hashtbl.find_opt groups key with
        | Some values ->
            Hashtbl.replace groups key (value :: values);
            order
        | None ->
            Hashtbl.add groups key [ value ];
            key :: order)
      [] entries
  in
  List.rev order
  |> List.map (fun key ->
      let values =
        match Hashtbl.find_opt groups key with
        | Some values -> values
        | None -> Crash.crash "Ordered package group is missing"
      in
      (key, List.rev values))

let nestedIf cases fallback =
  List.fold_right
    (fun (condition, result) remaining -> AST.If (condition, result, remaining))
    cases fallback

let catalogFunction name parameters returnType body =
  {
    AST.name;
    typeParams = [];
    params = NonEmptyList.fromList parameters;
    returnType;
    body;
    recursion = None;
  }

let collectProgramSpecs program =
  let symbols, tops = C.viewProgram program in
  List.map
    (function
      | C.FunctionDef func when func.C.typeParams = [] ->
          Monomorphization.collectTypeAppsFromFunc symbols func
      | C.Expression expr -> Monomorphization.collectTypeApps symbols expr
      | _ -> SpecializationIdentity.SpecSet.empty)
    tops
  |> List.fold_left SpecializationIdentity.SpecSet.union
       SpecializationIdentity.SpecSet.empty

let collectProgramCalls program =
  let _, tops = C.viewProgram program in
  List.map
    (function
      | C.FunctionDef func ->
          Monomorphization.collectCalledFunctions func.C.body
      | C.ValueDef value -> Monomorphization.collectCalledFunctions value.C.body
      | C.Expression expr -> Monomorphization.collectCalledFunctions expr
      | C.TypeDef _ -> F.empty)
    tops
  |> List.fold_left F.union F.empty

let calledFunctionNames symbols calls =
  F.elements calls
  |> List.filter_map (fun id -> C.functionName id symbols)
  |> S.of_list

let validateDistinctCatalogHashes entries =
  Result.map
    (fun _ -> ())
    (List.fold_left
       (fun result (entry : X.packageValueCatalogEntry) ->
         let* hashes = result in
         if S.mem entry.X.valueHash hashes then
           Error
             ("Package value catalog contains duplicate value hash '"
            ^ entry.X.valueHash ^ "'")
         else Ok (S.add entry.X.valueHash hashes))
       (Ok S.empty) entries)

let materializeReachablePackageValueCatalog (baseContext : X.pipelineContext)
    warningSettings (X.PackageValueCatalog entries) typedProgram =
  let* () = validateDistinctCatalogHashes entries in
  let localGenericDefs =
    SpecializationIdentity.extractGenericFuncDefs typedProgram
  in
  let genericDefs =
    M.fold M.add localGenericDefs baseContext.X.genericFuncDefs
  in
  let specialization =
    Monomorphization.specializeFromSpecs
      (C.programSymbols typedProgram)
      genericDefs
      (collectProgramSpecs typedProgram)
  in
  let requestedEvaluatorTypes =
    SpecializationIdentity.SpecSet.elements
      specialization.SpecializationIdentity.externalSpecs
    |> List.filter_map (function
      | "Builtin.pmEvaluateValue", [ resultType ] -> Some resultType
      | _ -> None)
    |> T.of_list
  in
  let specializedCallNames =
    List.map
      (fun (artifact : SpecializationIdentity.genericFunctionArtifact) ->
        Monomorphization.collectCalledFunctions
          artifact.SpecializationIdentity.func.C.body
        |> calledFunctionNames artifact.SpecializationIdentity.symbols)
      specialization.SpecializationIdentity.specializedFuncs
    |> List.fold_left S.union S.empty
  in
  let symbols = C.programSymbols typedProgram in
  let reachableCallNames =
    S.union
      (collectProgramCalls typedProgram |> calledFunctionNames symbols)
      specializedCallNames
  in
  let needsFind = S.mem "Builtin.pmFindValuesByValueType" reachableCallNames in
  let needsLocations =
    S.mem "Builtin.pmGetLocationsByValue" reachableCallNames
  in
  let needsEvaluators = not (T.is_empty requestedEvaluatorTypes) in
  if (not needsFind) && (not needsLocations) && not needsEvaluators then
    Ok typedProgram
  else
    let reachableEntries =
      List.filter
        (fun (entry : X.packageValueCatalogEntry) ->
          T.mem entry.X.evaluator.X.resultType requestedEvaluatorTypes)
        entries
    in
    let findGroups =
      reachableEntries
      |> List.filter (fun (entry : X.packageValueCatalogEntry) ->
          entry.X.runtimeType.X.typeArguments = [])
      |> List.map (fun (entry : X.packageValueCatalogEntry) ->
          (entry.X.runtimeType, entry.X.valueHash))
      |> orderedGroups
    in
    let findCases =
      List.map
        (fun ((catalogType : X.packageCustomType), hashes) ->
          let condition =
            call
              "Darklang.LanguageTools.RuntimeTypes.__isCustomTypeWithNoTypeArguments"
              [ AST.Var "valueType"; AST.StringLiteral catalogType.X.hash ]
          in
          (condition, AST.ListLiteral (List.map packageHashExpr hashes)))
        findGroups
    in
    let findFunction =
      catalogFunction "Builtin.pmFindValuesByValueType"
        [ ("valueType", runtimeValueType) ]
        (AST.TList packageHashType)
        (nestedIf findCases (AST.ListLiteral []))
    in
    let visibleLocations =
      List.concat_map
        (fun (entry : X.packageValueCatalogEntry) ->
          List.concat_map
            (fun (location : X.catalogPackageLocation) ->
              List.map
                (fun branchId -> ((branchId, entry.X.valueHash), location))
                location.X.visibleInBranches)
            entry.X.locations)
        reachableEntries
    in
    let locationGroups = orderedGroups visibleLocations in
    let locationExpr (location : X.catalogPackageLocation) =
      AST.RecordLiteral
        ( AST.unresolvedRecordReference
            "Darklang.LanguageTools.ProgramTypes.PackageLocation" [],
          [
            ( AST.unresolvedRecordFieldReference "owner",
              AST.StringLiteral location.X.owner );
            ( AST.unresolvedRecordFieldReference "modules",
              AST.ListLiteral
                (List.map
                   (fun value -> AST.StringLiteral value)
                   location.X.modules) );
            ( AST.unresolvedRecordFieldReference "name",
              AST.StringLiteral location.X.name );
          ] )
    in
    let locationCases =
      List.map
        (fun ((branchId, valueHash), locations) ->
          let branchMatches =
            AST.BinOp (AST.Eq, AST.Var "branchId", AST.StringLiteral branchId)
          in
          let hashMatches =
            AST.BinOp (AST.Eq, AST.Var "hashText", AST.StringLiteral valueHash)
          in
          ( AST.BinOp (AST.And, branchMatches, hashMatches),
            AST.ListLiteral (List.map locationExpr locations) ))
        locationGroups
    in
    let locationsBody =
      AST.Let
        ( AST.LPVariable "hashText",
          call "Darklang.LanguageTools.ProgramTypes.hashToString"
            [ AST.Var "valueHash" ],
          nestedIf locationCases (AST.ListLiteral []) )
    in
    let locationsFunction =
      catalogFunction "Builtin.pmGetLocationsByValue"
        [ ("branchId", AST.TString); ("valueHash", packageHashType) ]
        (AST.TList packageLocationType) locationsBody
    in
    let evaluatorFunction resultType =
      let name =
        SpecializationIdentity.specName "Builtin.pmEvaluateValue" [ resultType ]
      in
      let cases =
        List.filter_map
          (fun (entry : X.packageValueCatalogEntry) ->
            if entry.X.evaluator.X.resultType <> resultType then None
            else
              match entry.X.evaluator.X.state with
              | X.Available value ->
                  Some
                    ( AST.BinOp
                        ( AST.Eq,
                          AST.Var "hashText",
                          AST.StringLiteral entry.X.valueHash ),
                      optionSomeExpr value )
              | X.Unavailable | X.EvaluationFailure -> None)
          reachableEntries
      in
      let body =
        AST.Let
          ( AST.LPVariable "hashText",
            call "Darklang.LanguageTools.ProgramTypes.hashToString"
              [ AST.Var "valueHash" ],
            nestedIf cases optionNoneExpr )
      in
      catalogFunction name
        [ ("valueHash", packageHashType) ]
        (optionType resultType) body
    in
    let generatedFunctions =
      (if needsFind then [ findFunction ] else [])
      @ (if needsLocations then [ locationsFunction ] else [])
      @ List.map evaluatorFunction (T.elements requestedEvaluatorTypes)
    in
    let syntheticProgram =
      AST.Program
        (List.map (fun func -> AST.FunctionDef func) generatedFunctions)
    in
    let* _, generated, _ =
      TypeChecking.checkDeclarationProgramWithBaseEnvAndSettings
        {
          baseContext.X.typeCheckEnv with
          Types.functionCatalog =
            C.functionCatalog (C.programSymbols typedProgram);
        }
        false warningSettings syntheticProgram
      |> Result.map_error (fun error ->
          "Package value catalog validation failed: "
          ^ CheckingDiagnostics.typeErrorToString error)
    in
    let generatedSymbols, generatedTops = C.viewProgram generated in
    let userSymbols, userTops = C.viewProgram typedProgram in
    let symbols, imported =
      C.composeTopLevels generatedSymbols userSymbols generatedTops
    in
    Ok (C.programFromCheckedParts (symbols, imported @ userTops))

let materializePackageValueCatalog (baseContext : X.pipelineContext)
    warningSettings catalog typedProgram =
  let programCalls = collectProgramCalls typedProgram in
  let symbols = C.programSymbols typedProgram in
  let programCallNames = calledFunctionNames symbols programCalls in
  let mightReachCatalog =
    S.exists
      (fun called ->
        S.mem called X.packageCatalogFunctionNames
        || S.mem called baseContext.X.packageCatalogGenericCallers)
      programCallNames
  in
  if mightReachCatalog then
    materializeReachablePackageValueCatalog baseContext warningSettings catalog
      typedProgram
  else
    let (X.PackageValueCatalog entries) = catalog in
    let* () = validateDistinctCatalogHashes entries in
    Ok typedProgram
