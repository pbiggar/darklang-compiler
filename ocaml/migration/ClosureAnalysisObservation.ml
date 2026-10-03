[@@@warning "-4"]
open Dark_compiler
module C = InstrumentedCheckedAST
module A = InstrumentedClosureAnalysis
module R = InstrumentedTypeRegistries
module W = InstrumentedWrittenChecking
module T = InstrumentedTypes
module M = StringOrder.Map
module B = C.BindingIdMap
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error message -> SemanticJson.union "FSharpResult" "Error" [str message]
let id value = let value = AST.functionIdValue value in SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind", `String "uint64"; "value", `String (Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value))]]
let map key encode values = `Assoc ["map", list (fun (key_, value) -> tuple [key key_; encode value]) values]
let bindingMap value = map C.observationBinding typ (B.bindings value)
let bindingSet value = `Assoc ["set", list C.observationBinding (A.BindingSet.elements value)]
let state (value : A.liftState) =
 let ordinal id_ = match id id_ with `Assoc fields -> (match List.assoc "fields" fields with `List [value] -> value | _ -> assert false) | _ -> assert false in
 let fnMap encode values = SemanticJson.union "FunctionIdMap" "FunctionIdMap" [map ordinal encode (FunctionIdMap.toList values)] in
 SemanticJson.record "LiftState" ["Symbols", C.observationGlobalCatalog value.A.symbols; "Counter", SemanticJson.int32 value.A.counter; "LiftedFunctions", list C.observationFunctionDef value.A.liftedFunctions;
 "ComparisonFuncs", map (fun (id_, args) -> tuple [id id_; list typ args]) str (A.ComparisonMap.bindings value.A.comparisonFuncs);
 "ComparableFunctionParams", `Assoc ["set", list (list typ) (A.TypeListSet.elements value.A.comparableFunctionParams)];
 "TypeEnv", bindingMap value.A.typeEnv; "FuncParams", fnMap (list typ) value.A.funcParams; "FuncReturnTypes", fnMap typ value.A.funcReturnTypes;
 "GenericFuncDefs", fnMap (fun (parameters, result) -> tuple [list str parameters; typ result]) value.A.genericFuncDefs;
 "TypeReg", map str (fun (info : R.recordTypeInfo) -> SemanticJson.record "RecordTypeInfo" ["TypeParams", list str info.R.typeParams; "Fields", list (fun (name, value) -> tuple [str name; typ value]) info.R.fields]) (M.bindings value.A.typeReg);
 "VariantLookup", map str (fun (owner, parameters, tag, fields) -> tuple [str owner; list str parameters; SemanticJson.int32 tag; list typ fields]) (M.bindings value.A.variantLookup);
 "RecursiveSelf", option (fun (self, closure, typ_, recursion) -> tuple [C.observationBinding self; C.observationBinding closure; typ typ_; C.observationRecursiveMember recursion]) value.A.recursiveSelf]
let observe source =
 let types = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar "a"; AST.TVar "b"; AST.TInferenceVar (source, "fixed"); AST.TRecord ("R", []); AST.TRecord ("R", [AST.TVar "a"]); AST.TSum ("S", [AST.TVar "a"]); AST.TSum ("S", []); AST.TTuple [AST.TVar "a"; AST.TString]; AST.TList (AST.TVar "a"); AST.TStream (AST.TVar "a"); AST.TDict (AST.TVar "a", AST.TString); AST.TFunction ([AST.TVar "a"], AST.TString)] in
 let ids = [AST.bindingId 0; AST.namedBindingId 0 "x"; AST.namedBindingId 1 "x"; AST.topLevelValueId source] in
 let bounds = [A.BindingSet.empty; A.BindingSet.of_list ids] in
 let program program =
  let symbols = C.programSymbols program in let tops = C.programTopLevels program in
  let functions = List.filter_map (function C.FunctionDef value -> Some value | _ -> None) tops in
  let bodies = List.filter_map (function C.FunctionDef value -> Some value.C.body | C.ValueDef value -> Some value.C.body | C.Expression value -> Some value | C.TypeDef _ -> None) tops in
  let env = W.typeCheckEnvironment program in
  let records = M.map (fun (info : T.recordTypeInfo) -> {R.typeParams = info.T.typeParams; fields = info.T.fields}) env.T.indexedTypeReg in
  let typeEnv = C.programValues program |> M.bindings |> List.map (fun (name, (typ, _)) -> AST.topLevelValueId name, typ) |> List.to_seq |> B.of_seq in
  let parameters = FunctionIdMap.ofList (List.map (fun (func : C.functionDef) -> func.C.id, C.functionParameterTypes func |> NonEmptyList.toList |> List.map snd) functions) in
  let returns = FunctionIdMap.ofList (List.map (fun (func : C.functionDef) -> func.C.id, C.functionReturnType func) functions) in
  let generic = FunctionIdMap.ofList (List.map (fun (func : C.functionDef) -> func.C.id, (func.C.typeParams, C.functionReturnType func)) functions) in
  let initial = {A.symbols; counter = 0; liftedFunctions = functions; comparisonFuncs = A.ComparisonMap.of_seq (List.to_seq [AST.functionId 9L, [AST.TVar "a"], source; AST.functionId (-1L), [AST.TInt64], "other"] |> Seq.map (fun (id, args, name) -> (id, args), name)); comparableFunctionParams = A.TypeListSet.of_list [[]; [AST.TInt64]; [AST.TVar "a"]]; typeEnv; funcParams = parameters; funcReturnTypes = returns; genericFuncDefs = generic; typeReg = records; variantLookup = env.T.variantLookup; recursiveSelf = None} in
  let parameters = NonEmptyList.singleton {C.pattern = C.LPVariable (AST.namedBindingId 0 "x"); typ = C.checkedType AST.TInt64} in
  let extra = [C.Local (AST.namedBindingId 0 "x"); C.Closure (AST.functionId 9L, [C.Local (AST.namedBindingId 0 "x")]); C.Call (AST.functionId 9L, NonEmptyList.singleton C.UnitLiteral); C.TypeApp (AST.functionId 9L, C.checkedTypeArgs [AST.TString], NonEmptyList.singleton C.UnitLiteral);
    C.Lambda (parameters, Some (C.checkedType AST.TString), C.Local (AST.namedBindingId 0 "x")); C.If (C.BoolLiteral true, C.ListLiteral [], C.ListLiteral [C.Int64Literal 1L])] in
  let states = [initial; {initial with A.typeEnv = B.of_seq (List.to_seq (List.map (fun id -> id, AST.TInt64) ids)); funcParams = FunctionIdMap.add (AST.functionId 9L) [AST.TInt64; AST.TBool] initial.A.funcParams; funcReturnTypes = FunctionIdMap.add (AST.functionId 9L) AST.TNever initial.A.funcReturnTypes; genericFuncDefs = FunctionIdMap.add (AST.functionId 9L) (["a"], AST.TList (AST.TVar "a")) initial.A.genericFuncDefs}] in
  let infer state expr = A.simpleInferType expr state.A.typeEnv state.A.funcParams state.A.funcReturnTypes state.A.genericFuncDefs state.A.typeReg state.A.variantLookup (R.typeNamesFromSymbols state.A.symbols) in
  let attempt encode action = result encode (try Ok (action ()) with Failure message -> Error message) in
  let expression expr = tuple [list (fun bound -> bindingSet (A.freeVars expr bound)) bounds; list (fun state -> tuple [attempt (option typ) (fun () -> infer state expr); attempt (result typ) (fun () -> A.inferLambdaReturnType expr state)]) states] in
  let cases = List.concat_map (function C.Match (_, cases) -> NonEmptyList.toList cases |> List.concat_map (fun (case : C.matchCase) -> NonEmptyList.toList case.C.patterns) | _ -> []) bodies in
  let symbols, functionId = let id, symbols = C.internFunction "__lift0" symbols in symbols, id in
  let fake = {C.id = functionId; name = "__lift1"; typeParams = []; params = NonEmptyList.singleton (AST.bindingId 0, C.checkedType AST.TInt64); returnType = C.checkedType AST.TInt64; body = C.UnitLiteral; recursion = None} in
  let collision = {initial with A.symbols; funcParams = FunctionIdMap.add functionId [] initial.A.funcParams; liftedFunctions = fake :: initial.A.liftedFunctions} in
  let fresh state_ prefix = let name, next = A.freshLiftedName state_ prefix in tuple [str name; state next] in
  tuple [state initial; list expression (bodies @ extra);
   list (fun pattern -> list (fun typ_ -> bindingMap (A.matchPatternBindingTypes records env.T.variantLookup (R.typeNamesFromSymbols initial.A.symbols) pattern typ_)) types) cases;
   list (fun state -> `Bool (A.lambdaNeedsComparison parameters state)) states;
   list (fun state -> list (fresh state) ["__lift"; source]) [initial; collision; {initial with A.counter = Int32.to_int Int32.max_int}]] in
 let sourceProgram source = result (fun (_, value, _) -> program value) (Result.bind (WrittenParsing.parse Validation.Script source) (fun unit -> W.checkSourceUnitsWithBase None false false [unit])) in
 tuple [list (fun left -> list (fun right -> option typ (A.reconcileBranchTypes left right)) types) types; list (result program) (C.observationFixtures source); sourceProgram source;
 if source = "" then list sourceProgram ["let id (x: 'a) : 'a = x
id 1"; "type S<'a> = A of 'a | B
let f (x: S<Int64>) : Int64 = match x with | S.A value -> value | S.B -> 0
f (S.A 1)"; "let recur (x: Int64) : Int64 = if x == 0 then x else recur (x - 1)
recur 1"] else `List []]
