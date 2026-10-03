(* ResolvedProgram.fs - Check resolved declarations and expressions against explicit environments. *)
open! AST
open! Types
open! ResolveDeclarations
open CheckingDiagnostics
module M = StringOrder.Map
module S = StringOrder.Set
module Specs = CheckFunctions.SpecificationSet
let overlay base additions = M.fold M.add additions base
let bind = Result.bind
(* Internal: type-check a program and return the type checking environment
   This is the core implementation used by checkProgram, checkProgramWithEnv, and checkProgramWithBaseEnv
   When baseEnv is provided, registries are merged with it (for separate compilation) *)
let[@warning "-4"] checkResolvedProgramInternal baseEnv requireExplicitTypeArgsForBareCalls warningSettings requireEntry (Program topLevels) =
 let declarationSummary = Declarations.summarizeTopLevelDeclarations topLevels in
 let programTypeReg = resolveAliasesInTypeRegistry declarationSummary.aliasReg declarationSummary.typeReg in
 let programGenericFuncReg = {functions = declarationSummary.genericFuncs; requireExplicitTypeArgsForBareCalls} in
 (* Build module registry once (or reuse from base environment) *)
 let moduleRegistry = match baseEnv with Some existing -> existing.moduleRegistry | None -> DarkStdlib.buildModuleRegistry () in
 let programResolutionEnv = ResolveDeclarations.declarationResolutionEnvironment topLevels moduleRegistry (Option.is_none baseEnv) in
 let programSumTypeNames = sumTypeNamesFromVariantLookup declarationSummary.variantLookup in
 let availableSumTypeNames = match baseEnv with Some existing -> S.union existing.sumTypeNames programSumTypeNames | None -> programSumTypeNames in
 (* The base environment is already canonical. Canonicalize only this
    program's declarations, then overlay them on the immutable base instead
    of mapping and merging the complete base registry again. *)
 let canonicalProgramVariantLookup = M.map (fun (name, params, tag, fields) -> name, params, tag, List.map (canonicalizeBareSumTypeRefsWithNames availableSumTypeNames) fields) declarationSummary.variantLookup in
 let canonicalVariantLookup = match baseEnv with Some existing -> overlay existing.variantLookup canonicalProgramVariantLookup | None -> canonicalProgramVariantLookup in
 let programIndexedSumTypeReg = indexSumTypeRegistry canonicalProgramVariantLookup in
 let canonicalProgramTypeReg = M.map (List.map (fun (name, typ) -> name, canonicalizeDeclaredTypeRefsWithSumTypeNames programTypeReg availableSumTypeNames typ)) programTypeReg in
 let programAliasReg = M.map (fun (params, target) -> params, canonicalizeBareSumTypeRefsWithNames availableSumTypeNames target) declarationSummary.aliasReg in
 let functionAliasReg = match baseEnv with Some existing -> overlay existing.aliasReg programAliasReg | None -> programAliasReg in
 (* Function calls must expose the same canonical types as checked function
    bodies. In particular, interpreter fixtures commonly alias an external
    recursive sum and then return it from a small wrapper. *)
 let programFuncEnv = M.map (fun (params, returnType) ->
  let canonical typ = canonicalizeDeclaredTypeRefsWithSumTypeNames canonicalProgramTypeReg availableSumTypeNames typ |> resolveType functionAliasReg |> canonicalizeBareSumTypeRefsWithNames availableSumTypeNames in
  TFunction (List.map canonical params, canonical returnType)) declarationSummary.funcSigs in
 let programIndexedTypeReg = indexTypeRegistry canonicalVariantLookup declarationSummary.recordTypeParams canonicalProgramTypeReg in
 let initialValueFuncEnv = match baseEnv with Some existing -> overlay existing.funcEnv programFuncEnv | None -> programFuncEnv in
 let initialValueFuncParamNames = match baseEnv with Some existing -> overlay existing.funcParamNames declarationSummary.funcParamNames | None -> declarationSummary.funcParamNames in
 let initialValueIndexedTypeReg = match baseEnv with Some existing -> overlay existing.indexedTypeReg programIndexedTypeReg | None -> programIndexedTypeReg in
 let initialValueIndexedSumTypeReg = match baseEnv with Some existing -> overlay existing.indexedSumTypeReg programIndexedSumTypeReg | None -> programIndexedSumTypeReg in
 let initialValueGenericFuncReg = match baseEnv with Some existing -> {functions = overlay existing.genericFuncReg.functions programGenericFuncReg.functions; requireExplicitTypeArgsForBareCalls = existing.genericFuncReg.requireExplicitTypeArgsForBareCalls || programGenericFuncReg.requireExplicitTypeArgsForBareCalls} | None -> programGenericFuncReg in
 let initialValues = match baseEnv with Some existing -> existing.values | None -> M.empty in
 let checkedValuesResult = List.fold_left (fun result topLevel -> bind result (fun (valueFuncEnv, values, checkedDefs) -> match topLevel with
 | ValueDef (UncheckedValueDef (name, body)) ->
   Result.map (fun (typ, body) -> M.add name typ valueFuncEnv, M.add name typ values, M.add name (CheckedValueDef (name, typ, body)) checkedDefs)
    (CheckExpressions.checkExprWithParamNamesAndSumTypeNames initialValueFuncParamNames availableSumTypeNames initialValueIndexedSumTypeReg body valueFuncEnv initialValueIndexedTypeReg canonicalVariantLookup initialValueGenericFuncReg warningSettings moduleRegistry functionAliasReg None)
 | ValueDef (CheckedValueDef (name, typ, body)) -> Ok (M.add name typ valueFuncEnv, M.add name typ values, M.add name (CheckedValueDef (name, typ, body)) checkedDefs)
 | _ -> Ok (valueFuncEnv, values, checkedDefs))) (Ok (initialValueFuncEnv, initialValues, M.empty)) topLevels in
 bind checkedValuesResult (fun (valueFuncEnv, values, checkedValues) ->
 let topLevels = List.map (function ValueDef value -> (match M.find_opt (valueDefName value) checkedValues with Some checked -> ValueDef checked | None -> Crash.crash ("Checked value '" ^ valueDefName value ^ "' was not retained")) | other -> other) topLevels in
 (* Build the type check environment for THIS program *)
 let programEnv = {
  typeCatalog = (match baseEnv with Some existing -> existing.typeCatalog | None -> CheckedAST.emptyTypeCatalog);
  functionCatalog = (match baseEnv with Some existing -> existing.functionCatalog | None -> CheckedAST.emptyFunctionCatalog);
  typeReg = canonicalProgramTypeReg; indexedTypeReg = programIndexedTypeReg; recordTypeNames = S.of_list (List.map fst (M.bindings programIndexedTypeReg));
  variantLookup = canonicalProgramVariantLookup; indexedSumTypeReg = programIndexedSumTypeReg; sumTypeNames = programSumTypeNames;
  funcEnv = valueFuncEnv; values; funcParamNames = declarationSummary.funcParamNames; genericFuncReg = programGenericFuncReg;
  (* Checked bodies are installed after the function-definition pass. *)
  genericFuncDefs = M.empty; moduleRegistry; aliasReg = programAliasReg; resolutionEnv = programResolutionEnv} in
 (* Merge with base environment if provided (for separate compilation) *)
 let typeCheckEnv = match baseEnv with Some existing -> mergeTypeCheckEnv existing programEnv | None -> programEnv in
 (* Extract the merged registries for use in type checking *)
 let variantLookup = typeCheckEnv.variantLookup and typeReg = typeCheckEnv.indexedTypeReg and funcEnv = typeCheckEnv.funcEnv and funcParamNameReg = typeCheckEnv.funcParamNames and genericFuncReg = typeCheckEnv.genericFuncReg and mergedAliasReg = typeCheckEnv.aliasReg and sumTypeNames = availableSumTypeNames and indexedSumTypeReg = typeCheckEnv.indexedSumTypeReg in
 let checkFunction func = CheckFunctions.checkFunctionDefWithSumTypeNames funcParamNameReg sumTypeNames indexedSumTypeReg func funcEnv typeReg variantLookup genericFuncReg warningSettings moduleRegistry mergedAliasReg in
 (* Third pass: type check all function definitions and collect transformed top-levels
    The accumulator contains (type option * TopLevel) pairs where the type is Some for expressions *)
 let checkTopLevelWithType topLevel = match topLevel with
 | FunctionDef func -> Result.map (fun func -> None, FunctionDef func) (checkFunction func)
 | TypeDef _ | ValueDef _ -> Ok (None, topLevel)
 | Expression (modulePath, expr) -> Result.map (fun (typ, expr) -> Some typ, Expression (modulePath, expr)) (CheckExpressions.checkExprWithParamNamesAndSumTypeNames funcParamNameReg sumTypeNames indexedSumTypeReg expr funcEnv typeReg variantLookup genericFuncReg warningSettings moduleRegistry mergedAliasReg None) in
 let checkAllTopLevelsWithTypes = List.fold_left (fun result topLevel -> bind result (fun acc -> Result.map (fun checked -> checked :: acc) (checkTopLevelWithType topLevel))) (Ok []) topLevels |> Result.map List.rev in
 (* Type check all top-levels *)
 bind checkAllTopLevelsWithTypes (fun topLevelsWithTypes ->
  (* Extract just the top-levels *)
  let topLevels = List.map snd topLevelsWithTypes in
  let localGenericFuncDefs = M.of_list (List.filter_map (function FunctionDef func when func.typeParams <> [] -> Some (func.name, func) | _ -> None) topLevels) in
  let recursiveGroupsByMember = M.of_list (List.filter_map (function FunctionDef func -> (match func.recursion with Some (TypedRecursiveBinding typed) -> Some (func.name, typed.resolved.group) | Some (ResolvedRecursiveBinding resolved) -> Some (func.name, resolved.group) | _ -> None) | _ -> None) topLevels) in
  let validateMonomorphicRecursiveReferences () = List.fold_left (fun result (func : functionDef) -> bind result (fun () -> match M.find_opt func.name recursiveGroupsByMember with
  | None -> Ok ()
  | Some currentGroup ->
    let target = List.find_map (fun (targetName, typeArgs) -> match M.find_opt targetName recursiveGroupsByMember, M.find_opt targetName localGenericFuncDefs with
     | Some targetGroup, Some targetDef when targetGroup = currentGroup -> if typeArgs = List.map (fun name -> TVar name) targetDef.typeParams then None else Some targetName
     | _ -> None) (Specs.elements (CheckFunctions.collectTypeAppSpecs func.body)) in
    match target with Some targetName -> Error (PolymorphicRecursion targetName) | None -> Ok ())) (Ok ()) (List.filter_map (function FunctionDef func -> Some func | _ -> None) topLevels) in
  let checkedGenericFuncDefs = overlay typeCheckEnv.genericFuncDefs localGenericFuncDefs in
  let localExplicitSpecs = List.fold_left (fun specs topLevel -> Specs.union specs (match topLevel with FunctionDef func when func.typeParams = [] -> CheckFunctions.collectTypeAppSpecs func.body | Expression (_, expr) -> CheckFunctions.collectTypeAppSpecs expr | _ -> Specs.empty)) Specs.empty topLevels |> Specs.filter (fun (name, _) -> M.mem name checkedGenericFuncDefs) in
  let validateLocalSpecialization (name, typeArgs) = match M.find_opt name checkedGenericFuncDefs with None -> Ok () | Some func -> bind (CheckFunctions.specializeFunctionForTypeCheck func typeArgs) (fun specialized -> Result.map (fun _ -> ()) (checkFunction specialized)) in
  let validateAllSpecializations specs = List.fold_left (fun result spec -> bind result (fun () -> validateLocalSpecialization spec)) (Ok ()) (Specs.elements specs) in
  bind (validateMonomorphicRecursiveReferences ()) (fun () -> bind (validateAllSpecializations localExplicitSpecs) (fun () ->
   let checkedTypeCheckEnv = {typeCheckEnv with genericFuncDefs = checkedGenericFuncDefs} in
   let topLevelsWithEqHelpers = MaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums mergedAliasReg typeReg variantLookup typeCheckEnv.indexedSumTypeReg topLevels in
   let entryTypes = List.filter_map (function Some typ, Expression _ -> Some typ | _ -> None) topLevelsWithTypes in
   match requireEntry, entryTypes with
   | true, [typ] -> Ok (typ, Program topLevelsWithEqHelpers, checkedTypeCheckEnv)
   | true, [] -> Error (GenericError "Executable program must contain exactly one entry expression; found 0")
   | true, entries -> Error (GenericError ("Executable program must contain exactly one entry expression; found " ^ string_of_int (List.length entries)))
   | false, [] -> Ok (TUnit, Program topLevelsWithEqHelpers, checkedTypeCheckEnv)
   | false, entries -> Error (GenericError ("Declaration-only program must not contain entry expressions; found " ^ string_of_int (List.length entries)))))))
(* Check the common separate-compilation case without constructing and then
   merging an empty declaration environment. Name resolution has already run,
   and concrete generic specializations and equality helpers retain the same
   validation/materialization path as a general program. *)
let checkResolvedExpressionWithBaseEnv baseEnv resolutionEnv requireExplicitTypeArgsForBareCalls warningSettings expr =
 let genericFuncReg = {baseEnv.genericFuncReg with requireExplicitTypeArgsForBareCalls = baseEnv.genericFuncReg.requireExplicitTypeArgsForBareCalls || requireExplicitTypeArgsForBareCalls} in
 let sumTypeNames = baseEnv.sumTypeNames in
 bind (CheckExpressions.checkExprWithParamNamesAndSumTypeNames baseEnv.funcParamNames sumTypeNames baseEnv.indexedSumTypeReg expr baseEnv.funcEnv baseEnv.indexedTypeReg baseEnv.variantLookup genericFuncReg warningSettings baseEnv.moduleRegistry baseEnv.aliasReg None) (fun (exprType, typedExpr) ->
  let validateSpecialization (name, typeArgs) = match M.find_opt name baseEnv.genericFuncDefs with None -> Ok () | Some func -> bind (CheckFunctions.specializeFunctionForTypeCheck func typeArgs) (fun specialized -> Result.map (fun _ -> ()) (CheckFunctions.checkFunctionDefWithSumTypeNames baseEnv.funcParamNames sumTypeNames baseEnv.indexedSumTypeReg specialized baseEnv.funcEnv baseEnv.indexedTypeReg baseEnv.variantLookup genericFuncReg warningSettings baseEnv.moduleRegistry baseEnv.aliasReg)) in
  let specs = CheckFunctions.collectTypeAppSpecs typedExpr |> Specs.filter (fun (name, _) -> M.mem name baseEnv.genericFuncDefs) |> Specs.elements in
  let validation = List.fold_left (fun result spec -> bind result (fun () -> validateSpecialization spec)) (Ok ()) specs in
  Result.map (fun () ->
   let topLevelsWithEqHelpers = MaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums baseEnv.aliasReg baseEnv.indexedTypeReg baseEnv.variantLookup baseEnv.indexedSumTypeReg [Expression ([], typedExpr)] in
   let checkedEnv = {baseEnv with genericFuncReg; resolutionEnv} in
   exprType, Program topLevelsWithEqHelpers, checkedEnv) validation)
