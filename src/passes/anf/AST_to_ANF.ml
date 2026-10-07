(* AST_to_ANF.ml - Assemble typed function conversion and declaration registries. *)
[@@@warning "-4-30"]
(*
   Result type that includes registries needed for later passes
   Function name -> param list with types
*)
type conversionResult = {program : ANF.program; ownershipContracts : OwnedIR.callSignature FunctionIdMap.t; recursiveMembers : AST.loweredRecursiveMember FunctionIdMap.t; typeReg : TypeRegistries.typeRegistry; recordFieldsReg : (string * AST.semanticType) list StringOrder.Map.t; recordTypeParamsReg : string list StringOrder.Map.t; variantLookup : LoweringPrimitives.variantLookup; rcSumShapeReg : MemoryModel.rcSumShapeRegistry; funcReg : TypeRegistries.functionRegistry; funcParams : (string * AST.semanticType) list StringOrder.Map.t; moduleRegistry : AST.moduleRegistry}
(*
   Result type for user-only ANF conversion (functions not merged with stdlib)
   Used for compiling user code separately from the prebuilt stdlib
   Only user functions, not merged with stdlib
   Late external specializations compiled in this unit
   User's main expression
   Merged registries (for lookups)
*)
type userOnlyResult = {symbols : CheckedAST.symbols; scopeContracts : Destruction.functionScopeContract FunctionIdMap.t; inertFunctionScopes : SpecializationIdentity.FunctionSet.t; userFunctions : ANF.functionDef list; ownershipContracts : OwnedIR.callSignature FunctionIdMap.t; nonInlineableFunctionNames : SpecializationIdentity.FunctionSet.t; mainExpr : ANF.aExpr; typeReg : TypeRegistries.typeRegistry; typeNames : TypeRegistries.typeNameRegistry; recordFieldsReg : (string * AST.semanticType) list StringOrder.Map.t; recordTypeParamsReg : string list StringOrder.Map.t; variantLookup : LoweringPrimitives.variantLookup; sumMetadata : LoweringPrimitives.sumMetadata; localRecordFieldsReg : (string * AST.semanticType) list StringOrder.Map.t; localVariantLookup : LoweringPrimitives.variantLookup; rcSumShapeReg : MemoryModel.rcSumShapeRegistry; funcReg : TypeRegistries.functionRegistry; functionIds : TypeRegistries.functionIdRegistry; functionNames : TypeRegistries.functionNameRegistry; localReturnTypes : (string * AST.semanticType) FunctionIdMap.t; funcParams : (string * AST.semanticType) list StringOrder.Map.t; moduleRegistry : AST.moduleRegistry; recursiveMembers : AST.loweredRecursiveMember FunctionIdMap.t}
(*
   Registry bundle used during ANF conversion
*)
type registries = {scopeContracts : Destruction.functionScopeContract FunctionIdMap.t; inertFunctionScopes : SpecializationIdentity.FunctionSet.t; typeReg : TypeRegistries.typeRegistry; typeNames : TypeRegistries.typeNameRegistry; recordFieldsReg : (string * AST.semanticType) list StringOrder.Map.t; recordTypeParamsReg : string list StringOrder.Map.t; variantLookup : LoweringPrimitives.variantLookup; sumMetadata : LoweringPrimitives.sumMetadata; rcSumShapeReg : MemoryModel.rcSumShapeRegistry; funcReg : TypeRegistries.functionRegistry; functionIds : TypeRegistries.functionIdRegistry; functionNames : TypeRegistries.functionNameRegistry; funcParams : (string * AST.semanticType) list StringOrder.Map.t; moduleRegistry : AST.moduleRegistry; recursiveMembers : AST.loweredRecursiveMember FunctionIdMap.t}
(*
   Convert functions to ANF, returning updated VarGen
*)
type functionConversion = {functions : ANF.functionDef list; varGen : ANF.varGen; ownershipContracts : OwnedIR.callSignature FunctionIdMap.t}
module A = ANF
module C = CheckedAST
module R = TypeRegistries
module P = LoweringPrimitives
module T = TypeSubstitution
module S = SpecializationIdentity
module M = StringOrder.Map
module LowerOwnership = LowerOwnershipVariants.Make (ListLiveness.Identity)
let ( let* ) = Result.bind
let emptyScopes () = Destruction.inertFunctionScopes M.empty FunctionIdMap.empty
let measure recorder name operation =
 let start = (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in let result = operation () in let elapsed = (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. start in
 Option.iter (fun record -> record name elapsed) recorder; result
let toANFWithMetadata types expr gen env registry variants functions modules =
 let names = FunctionIdMap.map (fun _ (name, _) -> name) functions in
 LoweringExpressions.toANFCore (R.functionIdsFromNames names) (P.sumMetadataFromVariantLookup variants) types (emptyScopes ()) expr gen env registry variants functions names modules
let toANF expr gen env registry variants functions modules = toANFWithMetadata R.emptyTypeNames expr gen env registry variants functions modules
(*
   Convert a function definition to ANF
   VarGen is passed in and out to maintain globally unique TempIds across functions
   (needed for TypeMap which maps TempId -> Type across the whole program)
*)
let allocateTypedParams params gen =
 let rec loop params gen acc = match params with [] -> List.rev acc, gen | (_, typ) :: rest ->
  let id, gen = A.freshVar gen in loop rest gen ({A.id; typ} :: acc) in loop params gen []
(*
   Allocate TempIds for parameters, bundled with their types
   Build environment mapping param names to (TempId, Type)
   Convert body
*)
let convertFunctionWithSumTypeNames recordTiming symbols sums inert (func : C.functionDef) gen registry variants functions ids names modules =
 let params, typedParams, gen, env = measure recordTiming "AST -> ANF detail: Function parameter setup" (fun () ->
  let params = S.normalizeSyntheticNullaryParams symbols (S.paramsToList (C.functionParameterTypes func)) in
  let typedParams, gen = allocateTypedParams params gen in
  let env = List.map2 (fun (name, _) (param : A.typedParam) -> name, (param.A.id, param.A.typ)) params typedParams |> List.to_seq |> R.BindingMap.of_seq in
  params, typedParams, gen, env) in
 let unbound = measure recordTiming "AST -> ANF detail: Function free-variable analysis" (fun () -> ClosureAnalysis.freeVars func.C.body (ClosureAnalysis.BindingSet.of_list (List.map fst params))) in
 let* body, gen = if ClosureAnalysis.BindingSet.is_empty unbound then
  let types = measure recordTiming "AST -> ANF detail: Function type-name projection" (fun () -> R.typeNamesFromSymbols symbols) in
  let start = (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in
  let result = LoweringExpressions.toANFCore ids sums types inert func.C.body gen env registry variants functions names modules in
  let elapsed = (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. start in
  Option.iter (fun record -> record "AST -> ANF detail: Function expression lowering" elapsed; record ("AST -> ANF function: " ^ func.C.name) elapsed) recordTiming;
  result
 else let names = ClosureAnalysis.BindingSet.elements unbound |> List.map (fun id -> Option.value ~default:"<unknown-binding>" (C.bindingName id symbols)) |> String.concat ", " in
  Error ("Function '" ^ func.C.name ^ "' has unbound checked locals: " ^ names) in
 Ok ({A.id = func.C.id; name = func.C.name; typedParams; returnType = C.functionReturnType func; returnOwnership = A.OwnedReturn; body}, gen)
let convertFunction symbols func gen registry variants functions modules =
 let names = FunctionIdMap.map (fun _ (name, _) -> name) functions in
 convertFunctionWithSumTypeNames None symbols (P.sumMetadataFromVariantLookup variants) (emptyScopes ()) func gen registry variants functions (R.functionIdsFromNames names) names modules
(*
   Retain semantic recursive identities alongside lowered ANF. Native symbol
   strings remain presentation keys; recursive ownership and group layout are
   recovered exclusively from this registry.
*)
let loweredRecursiveMemberRegistry functions = List.filter_map (fun (func : C.functionDef) -> Option.map (fun member -> func.C.id, {AST.typed = C.semanticRecursiveMember member; environmentIndex = member.C.resolved.AST.groupIndex}) func.C.recursion) functions |> FunctionIdMap.ofList
(*
   Split program into type defs, function defs, and a single expression
*)
let splitDeclarations program =
 let _, tops = C.viewProgram program in
 let expressions = List.filter (function C.Expression _ -> true | _ -> false) tops in
 if expressions <> [] then Error ("Declaration-only program must not contain entry expressions; found " ^ string_of_int (List.length expressions)) else
 Ok (List.filter_map (function C.TypeDef (_, typ) -> Some (C.semanticTypeDef typ) | _ -> None) tops, List.filter_map (function C.FunctionDef func -> Some func | _ -> None) tops)
let splitTopLevels program =
 let _, tops = C.viewProgram program in
 let types = List.filter_map (function C.TypeDef (_, typ) -> Some (C.semanticTypeDef typ) | _ -> None) tops in
 let functions = List.filter_map (function C.FunctionDef func -> Some func | _ -> None) tops in
 let expressions = List.filter_map (function C.Expression expr -> Some expr | _ -> None) tops in
 if List.exists (fun (func : C.functionDef) -> func.C.name = "main") functions then Error "Function name 'main' is reserved"
 else if List.exists (fun (func : C.functionDef) -> func.C.name = "_start") functions then Error "Function name '_start' is reserved"
 else match expressions with [expr] -> Ok (types, functions, expr) | [] -> Error "Program must have a main expression" | _ -> Error "Multiple top-level expressions not allowed"
(*
   Build alias registry from type definitions
*)
let buildAliasRegistry types = List.filter_map (function AST.TypeAlias (name, params, target) -> Some (name, (params, target)) | _ -> None) types |> List.to_seq |> M.of_seq
(*
   Resolve type aliases inside function definitions
*)
let resolveAliasesInFunctions aliases functions = List.map (T.resolveAliasesInFunction aliases) functions
(*
   Overlay symbols already include the inherited namespace, but their
   reverse index is merged with the base registry immediately afterward.
   Retain only local entries here so small compilation units do not rebuild
   and merge the complete stdlib index.
*)
let buildRegistriesInternal phaseRecorder symbols includeModuleFunctionParams modules types aliases functions : registries =
 let base = List.filter_map (function AST.RecordDef (name, params, fields) -> Some (name, {R.typeParams = params; fields = T.firstDeclaredRecordFields fields}) | _ -> None) types |> List.to_seq |> M.of_seq in
 let rawVariants = List.filter_map (function AST.SumTypeDef (name, params, variants) -> Some (name, params, variants) | _ -> None) types |>
  List.fold_left (fun lookup (name, params, variants) -> List.fold_left (fun lookup (variant : AST.variant) ->
   let tag = match C.tryFindConstructorId name variant.AST.name symbols with Some id -> AST.constructorRuntimeTag id | None -> Crash.crash ("Missing constructor identity for '" ^ name ^ "." ^ variant.AST.name ^ "'") in
   let info = name, params, tag, variant.AST.fields in
   let lookup = if M.mem variant.AST.name lookup then lookup else M.add variant.AST.name info lookup in
   M.add (name ^ "." ^ variant.AST.name) info lookup) lookup variants) M.empty in
 let sums = P.sumTypeNamesFromVariantLookup rawVariants in
 let variants = M.map (fun (name, params, tag, fields) -> name, params, tag, List.map (R.canonicalizeBareSumTypeRefsWithNames sums) fields) rawVariants in
 let records = StringOrder.Set.of_list (List.map fst (M.bindings base)) in
 let registry = T.resolveAliasesInTypeRegistry aliases base |> fun registry -> R.expandTypeRegWithAliases registry aliases |> M.map (fun (info : R.recordTypeInfo) -> {info with R.fields = List.map (fun (name, typ) -> name, R.canonicalizeNamedTypeRefs records sums typ) info.R.fields}) in
 let funcReg = functions |> List.map (fun (func : C.functionDef) ->
  let params = C.functionParameterTypes func |> S.paramsToList |> S.normalizeSyntheticNullaryParams symbols |> List.map snd in
  func.C.id, (func.C.name, AST.TFunction (params, C.functionReturnType func))) |> FunctionIdMap.ofList in
 let localNames = functions |> List.map (fun (func : C.functionDef) -> func.C.id, func.C.name) |> FunctionIdMap.ofList in
 let localTypeNames : R.typeNameRegistry = {C.typeNames = types |> List.map (function AST.RecordDef (name, _, _) | AST.SumTypeDef (name, _, _) | AST.TypeAlias (name, _, _) ->
  let id = match C.tryFindTypeId name symbols with Some id -> id | None -> Crash.crash ("Alias type was not interned: " ^ name) in id, name) |> List.to_seq |> C.TypeIdMap.of_seq} in
 let functionNames = List.fold_left (fun names (func : C.functionDef) -> FunctionIdMap.add func.C.id func.C.name names) (C.functionNames symbols) functions in
 let functionIds = R.functionIdsFromNames (if includeModuleFunctionParams then functionNames else localNames) in
 let userParams = functions |> List.map (fun (func : C.functionDef) -> func.C.name, (C.functionParameterTypes func |> S.paramsToList |> List.mapi (fun index (id, typ) -> Option.value ~default:("arg" ^ string_of_int index) (C.bindingName id symbols), typ))) |> List.to_seq |> M.of_seq in
 let moduleParams = if includeModuleFunctionParams then M.map (fun (func : AST.moduleFunc) -> List.mapi (fun index typ -> "arg" ^ string_of_int index, typ) func.AST.paramTypes) modules else M.empty in
 let funcParams = M.fold (fun name value acc -> M.add name value acc) moduleParams userParams in
 let sumMetadata = P.sumMetadataFromVariantLookup variants in
 let scopeContracts = measure phaseRecorder "AST -> ANF Registry: Scope Contracts" (fun () -> ExtractListRegions.scopeContracts (fun types expr -> LoweringTypeInference.inferTypeCore sumMetadata (R.typeNamesFromSymbols symbols) expr types registry variants funcReg functionNames modules) functions) in
 let inertFunctionScopes = if includeModuleFunctionParams then Destruction.inertFunctionScopes functionIds scopeContracts else S.FunctionSet.empty in
 {typeReg = registry; typeNames = (if includeModuleFunctionParams then R.typeNamesFromSymbols symbols else localTypeNames); scopeContracts; inertFunctionScopes;
  recordFieldsReg = R.recordFieldsRegistry registry; recordTypeParamsReg = R.recordTypeParamsRegistry registry; variantLookup = variants; sumMetadata;
  rcSumShapeReg = R.rcSumShapeRegistryFromVariantLookup variants; funcReg; functionIds; functionNames = (if includeModuleFunctionParams then functionNames else localNames);
  funcParams; moduleRegistry = modules; recursiveMembers = loweredRecursiveMemberRegistry functions}
(*
   Build standalone registries from type and function definitions.
*)
let buildRegistriesWithTrace recorder symbols modules types aliases functions = buildRegistriesInternal recorder symbols true modules types aliases functions
let buildRegistries symbols modules types aliases functions = buildRegistriesWithTrace None symbols modules types aliases functions
(*
   Build only the declaration overlay for a context that already contains the
   module function parameters. Reconstructing that constant projection for
   every separately compiled user unit is both redundant and expensive.
*)
let buildOverlayRegistriesWithTrace recorder symbols modules types aliases functions = buildRegistriesInternal recorder symbols false modules types aliases functions
let buildOverlayRegistries symbols modules types aliases functions = buildOverlayRegistriesWithTrace None symbols modules types aliases functions
(*
   Merge registries with overlay taking precedence (module registry stays from base)
*)
let mergeRegistriesWithTrace recorder (base : registries) (overlay : registries) : registries =
 let measure name operation = match recorder with None -> operation () | Some _ -> measure recorder name operation in
 let merge base overlay = M.fold (fun key value acc -> M.add key value acc) overlay base in
 let functionNames = measure "AST -> ANF Registry: Function Name Merge" (fun () -> FunctionIdMap.merge base.functionNames overlay.functionNames) in
 let functionIds = merge base.functionIds overlay.functionIds in let scopeContracts = FunctionIdMap.merge base.scopeContracts overlay.scopeContracts in
 let localInert = measure "AST -> ANF Registry: Inert Scope Analysis" (fun () -> Destruction.inertFunctionScopesWithBase base.inertFunctionScopes functionIds overlay.scopeContracts) in
 measure "AST -> ANF Registry: Other Overlay Maps" (fun () -> {
  typeReg = merge base.typeReg overlay.typeReg;
  typeNames = {C.typeNames = C.TypeIdMap.union (fun _ _ right -> Some right) base.typeNames.C.typeNames overlay.typeNames.C.typeNames};
  scopeContracts; inertFunctionScopes = S.FunctionSet.union base.inertFunctionScopes localInert;
  recordFieldsReg = merge base.recordFieldsReg overlay.recordFieldsReg; recordTypeParamsReg = merge base.recordTypeParamsReg overlay.recordTypeParamsReg;
  variantLookup = merge base.variantLookup overlay.variantLookup; sumMetadata = P.mergeSumMetadata base.sumMetadata overlay.sumMetadata;
  rcSumShapeReg = merge base.rcSumShapeReg overlay.rcSumShapeReg; funcReg = FunctionIdMap.merge base.funcReg overlay.funcReg;
  functionIds; functionNames; funcParams = merge base.funcParams overlay.funcParams; moduleRegistry = base.moduleRegistry;
  recursiveMembers = FunctionIdMap.merge base.recursiveMembers overlay.recursiveMembers})
let mergeRegistries base overlay = mergeRegistriesWithTrace None base overlay
let extendFunctionRegistryWithConverted registry functions = List.fold_left (fun registry (func : A.functionDef) -> FunctionIdMap.add func.A.id (func.A.name, AST.TFunction (List.map (fun (param : A.typedParam) -> param.A.typ) func.A.typedParams, func.A.returnType)) registry) registry functions
let convertFunctionsWithOwnershipWithTrace recorder symbols (registries : registries) gen functions =
 let rec loop (conversionRegistries : registries) funcs gen acc = match funcs with [] -> Ok (List.rev acc, gen) | (func : C.functionDef) :: rest ->
  let* func, gen = convertFunctionWithSumTypeNames recorder symbols registries.sumMetadata registries.inertFunctionScopes func gen registries.typeReg registries.variantLookup conversionRegistries.funcReg conversionRegistries.functionIds conversionRegistries.functionNames registries.moduleRegistry |> Result.map_error (fun error -> "Function '" ^ func.C.name ^ "': " ^ error) in
  loop conversionRegistries rest gen (func :: acc) in
 let context : AnalyzeFunctionOwnership.context = {AnalyzeFunctionOwnership.typeReg = registries.typeReg; typeNames = registries.typeNames;
  recordFieldsReg = registries.recordFieldsReg; recordTypeParamsReg = registries.recordTypeParamsReg; variantLookup = registries.variantLookup;
  sumMetadata = registries.sumMetadata; rcSumShapeReg = registries.rcSumShapeReg; funcReg = registries.funcReg; functionNames = registries.functionNames; moduleRegistry = registries.moduleRegistry} in
 let* analysis = measure recorder "Ownership detail: Total" (fun () -> AnalyzeFunctionOwnership.analyzeWithTrace recorder context functions) |> Result.map_error (fun error -> "Whole-function ownership analysis failed: " ^ OwnershipDiagnostics.analysisError error) in
 let materialization = ScheduleOwnershipVariants.materialization (AnalyzeFunctionOwnership.schedule analysis) in
 let fusion = measure recorder "AST -> ANF detail: Ownership list-call fusion" (fun () -> FuseOwnershipListCalls.fuse registries.functionNames materialization functions) in
 let* anfFunctions, nextGen = measure recorder "AST -> ANF detail: Checked function lowering" (fun () ->
  let conversionRegistries = List.fold_left (fun (regs : registries) (func : C.functionDef) ->
   let params = C.functionParameterTypes func |> S.paramsToList |> List.map snd in
   {regs with funcReg = FunctionIdMap.add func.C.id (func.C.name, AST.TFunction (params, C.functionReturnType func)) regs.funcReg;
    functionIds = M.add func.C.name func.C.id regs.functionIds; functionNames = FunctionIdMap.add func.C.id func.C.name regs.functionNames}) registries fusion.FuseOwnershipListCalls.functions in
  loop conversionRegistries fusion.FuseOwnershipListCalls.functions gen []) in
 let* lowered = measure recorder "AST -> ANF detail: Ownership variant lowering" (fun () -> LowerOwnership.lower (AnalyzeFunctionOwnership.originalFunctions analysis) materialization anfFunctions nextGen fusion.FuseOwnershipListCalls.fusedSites) |> Result.map_error (fun error -> "Ownership lowering failed: " ^ OwnershipDiagnostics.loweringError error) in
 Ok {functions = lowered.LowerOwnershipVariants.functions; varGen = lowered.LowerOwnershipVariants.varGen; ownershipContracts = lowered.LowerOwnershipVariants.contracts}
let convertFunctionsWithOwnership symbols registries gen functions = convertFunctionsWithOwnershipWithTrace None symbols registries gen functions
let convertFunctions symbols registries gen functions = Result.map (fun converted -> converted.functions, converted.varGen) (convertFunctionsWithOwnership symbols registries gen functions)
(*
   Convert an expression to ANF with the given VarGen
*)
let convertExprToAnf (regs : registries) gen expr = LoweringExpressions.toANFCore regs.functionIds regs.sumMetadata regs.typeNames regs.inertFunctionScopes expr gen R.BindingMap.empty regs.typeReg regs.variantLookup regs.funcReg regs.functionNames regs.moduleRegistry
(*
   Synthesize an entrypoint function from a main expression
*)
let synthesizeEntryFunction id name returnType body = {A.id; name; typedParams = []; returnType; returnOwnership = A.OwnedReturn; body}
