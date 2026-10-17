(* AsyncRuntimeMaterialization.ml - Reuse source specialization for typed async helpers. *)
module C = CheckedAST
module X = CompilationContexts
module P = SourcePreparation
module I = SpecializationIdentity
module M = StringOrder.Map

let ( let* ) = Result.bind

let materialize recorder (context : X.pipelineContext) requested =
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
  let specialization =
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
        not (M.mem fn.C.name context.X.registries.AST_to_ANF.functionIds))
      functions
  in
  if functions = [] then
    Error "Async runtime specialization produced no missing helper bodies"
  else
    let program =
      C.programFromCheckedParts
        (symbols, List.map (fun fn -> C.FunctionDef fn) functions)
    in
    let* converted =
      P.convertTypedDeclarationsWithTrace recorder (Some context)
        (P.ReplaceTypeApps specs) program
    in
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
    Ok (updated, converted.P.functions)
