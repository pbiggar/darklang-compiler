(* AsyncRuntimeMaterialization.ml - Reuse source specialization for typed async helpers. *)
module C = CheckedAST
module X = CompilationContexts
module P = SourcePreparation
module I = SpecializationIdentity
module M = StringOrder.Map

let ( let* ) = Result.bind

type bodyKind = NativeRuntimeBodies | LogicalSourceBodies

let materializeBodies kind recorder (context : X.pipelineContext) requested =
  let refreshed =
    X.buildContext context.X.target context.X.symbols context.X.typeCheckEnv
      context.X.checkedValues context.X.genericFuncDefs context.X.specRegistry
      context.X.registries
      (X.buildBaseFuncNames context.X.registries)
      (P.extractReturnTypes context.X.registries.AST_to_ANF.funcReg)
  in
  let context =
    { refreshed with X.writtenEnvironment = context.X.writtenEnvironment }
  in
  let* specialization =
    Monomorphization.specializeFromSpecs context.X.symbols
      context.X.genericFuncDefs requested
  in
  let specs =
    P.mergeSpecRegistries context.X.specRegistry specialization.I.specRegistry
  in
  let symbols, functions =
    I.importSpecializedFunctions specialization.I.symbols
      specialization.I.specializedFuncs
  in
  let functions =
    List.filter
      (fun (fn : C.functionDef) ->
        match kind with
        | NativeRuntimeBodies ->
            not (M.mem fn.C.name context.X.registries.AST_to_ANF.functionIds)
        | LogicalSourceBodies -> true)
      functions
  in
  if functions = [] then
    Error "Async runtime specialization produced no missing helper bodies"
  else
    let env = context.X.typeCheckEnv in
    let rec prepare specs symbols topLevels =
      let symbols, topLevels =
        CheckedMaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums
          symbols env.Types.aliasReg env.Types.indexedTypeReg
          env.Types.variantLookup env.Types.indexedSumTypeReg topLevels
      in
      let program = C.programFromCheckedParts (symbols, topLevels) in
      let missing =
        P.collectLocalSpecs context.X.genericFuncDefs program
        |> I.SpecSet.filter (fun spec -> not (I.SpecMap.mem spec specs))
      in
      if I.SpecSet.is_empty missing then Ok (specs, symbols, topLevels)
      else
        let* specialization =
          Monomorphization.specializeFromSpecs symbols context.X.genericFuncDefs
            missing
        in
        let specs = P.mergeSpecRegistries specs specialization.I.specRegistry in
        let symbols, generated =
          I.importSpecializedFunctions specialization.I.symbols
            specialization.I.specializedFuncs
        in
        let existing =
          List.fold_left
            (fun names -> function
              | C.FunctionDef fn -> M.add fn.C.name () names
              | C.TypeDef _ | C.ValueDef _ | C.Expression _ -> names)
            M.empty topLevels
        in
        let generated =
          List.filter_map
            (fun (fn : C.functionDef) ->
              if
                M.mem fn.C.name existing
                || M.mem fn.C.name context.X.registries.AST_to_ANF.functionIds
              then None
              else Some (C.FunctionDef fn))
            generated
        in
        prepare specs symbols (topLevels @ generated)
    in
    let* specs, symbols, topLevels =
      prepare specs symbols (List.map (fun fn -> C.FunctionDef fn) functions)
    in
    let program = C.programFromCheckedParts (symbols, topLevels) in
    let* converted =
      P.convertTypedDeclarationsWithTrace recorder (Some context)
        (P.ReplaceTypeApps specs) program
    in
    List.iter
      (fun (fn : ANF.functionDef) ->
        match
          FunctionIdMap.tryFind fn.ANF.id
            context.X.registries.AST_to_ANF.functionNames
        with
        | Some previous when previous <> fn.ANF.name ->
            Crash.crash
              ("Async helper function identity collision: " ^ previous ^ " / "
             ^ fn.ANF.name)
        | _ -> ())
      converted.P.functions;
    let names = X.buildBaseFuncNames converted.P.registries in
    let returns =
      X.mergeReturnTypes context.X.returnTypes converted.P.localReturnTypes
    in
    let updated =
      X.buildContext context.X.target converted.P.symbols context.X.typeCheckEnv
        context.X.checkedValues context.X.genericFuncDefs specs
        converted.P.registries names returns
    in
    let updated =
      { updated with X.writtenEnvironment = context.X.writtenEnvironment }
    in
    let functions =
      match kind with
      | NativeRuntimeBodies -> converted.P.functions
      | LogicalSourceBodies -> converted.P.sourceFunctions
    in
    Ok (updated, functions)

let materialize = materializeBodies NativeRuntimeBodies
let materializeSource = materializeBodies LogicalSourceBodies
