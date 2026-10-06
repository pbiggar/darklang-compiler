[@@@warning "-4"]
open Dark_compiler
module C = InstrumentedCheckedAST
module A = InstrumentedMonomorphization
module P = InstrumentedPrepareFunctions
module S = InstrumentedSpecializationIdentity
module W = InstrumentedWrittenChecking
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [str error]
let attempt encode action = result encode (try Ok (action ()) with Failure message -> Error message | Invalid_argument message -> Error message)
let id value = let value = AST.functionIdValue value in SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind", `String "uint64"; "value", `String (Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value))]]
let deps set = `Assoc ["set", list id (S.FunctionSet.elements set)]
let artifact (value : S.genericFunctionArtifact) = SemanticJson.record "GenericFunctionArtifact" ["Symbols", C.observationGlobalCatalog value.S.symbols; "Function", C.observationFunctionDef value.S.func; "DirectDependencies", deps value.S.directDependencies]
let spec (name, args) = tuple [str name; list typ args]
let specs values = `Assoc ["set", list spec (S.SpecSet.elements values)]
let registry values = `Assoc ["map", list (fun (key, value) -> tuple [spec key; str value]) (S.SpecMap.bindings values)]
let specialization (value : S.specializationResult) = SemanticJson.record "SpecializationResult" ["SpecializedFuncs", list artifact value.S.specializedFuncs; "SpecRegistry", registry value.S.specRegistry; "ExternalSpecs", specs value.S.externalSpecs; "Symbols", C.observationGlobalCatalog value.S.symbols]
let observe source =
 let types = [[]; [AST.TInt64]; [AST.TList AST.TInt64]; [AST.TVar "a"]; [AST.TStream (AST.TVar "a")]; [AST.TFunction ([AST.TInt64], AST.TBool)]; [AST.TRecord ("R", [])]; [AST.TString; AST.TInt64]] in
 let sourceName = let units = HostText.scalars source in HostText.ofScalars (Array.sub units 0 (min 32 (Array.length units))) in
 let names = [InstrumentedLoweringPrimitives.eqHelperDispatchMarker; "__compare"; "__hash"; "__key_eq"; "Dict.fromList"; "Darklang.Stdlib.Dict.fromList"; "Darklang.Stdlib.Dict.empty"; "__raw_get"; "Builtin.pmEvaluateValue"; sourceName; "identity"] in
 let observeProgram program =
  let original = C.programSymbols program in
  let symbols = List.fold_left (fun symbols name -> snd (C.internFunction name symbols)) original (names @ ["Builtin.testRuntimeError"; "Darklang.Stdlib.Dict.__setOverwriting"] @ List.concat_map (fun name -> List.map (S.specName name) types) names @ List.concat_map (List.concat_map (fun typ -> [InstrumentedComparisonPlanning.eqHelperName typ; InstrumentedComparisonPlanning.compareHelperName typ])) types) in
  let targets = List.map (fun name -> match C.tryFindFunctionId name symbols with Some id -> id | None -> assert false) names in
  let bodies = C.programTopLevels program |> List.filter_map (function C.FunctionDef func -> Some func.C.body | C.ValueDef value -> Some value.C.body | C.Expression expr -> Some expr | C.TypeDef _ -> None) in
  let arguments = [NonEmptyList.singleton C.UnitLiteral; S.exprArgsFromList [C.Int64Literal 1L; C.Int64Literal 2L]; NonEmptyList.singleton (C.ListLiteral [])] in
  let synthetic = List.concat_map (fun target -> List.concat_map (fun types -> List.map (fun args -> C.TypeApp (target, C.checkedTypeArgs types, args)) arguments) types) targets in
  let registry = List.concat_map (fun name -> List.map (fun types -> (name, types), S.specName name types) types) names |> List.to_seq |> S.SpecMap.of_seq in
  let registries = [S.SpecMap.empty; registry] in
  let expression expr = tuple [attempt specs (fun () -> A.collectTypeApps symbols expr); deps (A.collectCalledFunctions expr); attempt C.observationExpr (fun () -> A.replaceTypeApps symbols expr); list (fun registry -> attempt (result C.observationExpr) (fun () -> A.replaceTypeAppsWithRegistry symbols registry expr)) registries] in
  let functions = C.programTopLevels program |> List.filter_map (function C.FunctionDef func -> Some func | _ -> None) in
  let func (func : C.functionDef) = tuple [attempt specs (fun () -> A.collectTypeAppsFromFunc symbols func); attempt C.observationFunctionDef (fun () -> A.replaceTypeAppsInFunc symbols func); list (fun registry -> attempt (result C.observationFunctionDef) (fun () -> A.replaceTypeAppsInFuncWithRegistry symbols registry func)) registries] in
  let definitions = S.extractGenericFuncDefs program in
  let initial = S.SpecSet.of_list ["identity", [AST.TInt64]; "external", [AST.TVar "a"]; "__hash", [AST.TString]] in
  let initials = [S.SpecSet.empty; initial; S.SpecSet.union initial (List.fold_left (fun specs expr -> match (try Ok (A.collectTypeApps symbols expr) with Failure message -> Error message) with Ok value -> S.SpecSet.union specs value | Error _ -> specs) S.SpecSet.empty bodies)] in
  let programWithSymbols = C.programFromCheckedParts (symbols, C.programTopLevels program) in
  tuple [list expression (bodies @ synthetic); list func functions; list (fun specs -> attempt specialization (fun () -> A.specializeFromSpecs symbols definitions specs)) initials;
   list (fun registry -> attempt (result C.observationProgram) (fun () -> A.replaceTypeAppsInProgramWithRegistry registry programWithSymbols)) registries;
   attempt C.observationProgram (fun () -> A.monomorphizeWithGenericFuncDefs definitions programWithSymbols); attempt C.observationProgram (fun () -> P.monomorphize programWithSymbols); attempt C.observationProgram (fun () -> P.monomorphizeWithExternalDefs definitions programWithSymbols);
   list (fun known -> `Bool (A.programNeedsLambdaLowering known program)) [StringOrder.Set.empty; StringOrder.Set.of_list names]] in
 let sourceProgram source = result (fun (_, program, _) -> observeProgram program) (Result.bind (WrittenParsing.parse Validation.Script source) (fun unit -> W.checkSourceUnitsWithBase None false false [unit])) in
 tuple [list (result observeProgram) (C.observationFixtures source); sourceProgram source;
  if source = "" then list sourceProgram ["let identity (x: 'a) : 'a = x\nidentity 1"; "let eq (x: 'a) (y: 'a) : Bool = x == y\neq [1] [2]"; "let f = fun (x: Int64) -> x\nf 1"; "let a (x: 'a) : 'a = x\nlet b (x: 'a) : 'a = a x\nb 1"; "Stdlib.Dict.fromList []"] else `List []]
