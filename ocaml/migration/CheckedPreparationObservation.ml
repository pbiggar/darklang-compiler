[@@@warning "-4"]
open Dark_compiler
module C = InstrumentedCheckedAST
module S = InstrumentedSpecializationIdentity
module H = InstrumentedCheckedMaterializeHelpers
module W = InstrumentedWrittenChecking
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [str error]
let id value = let value = AST.functionIdValue value in SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind", `String "uint64"; "value", `String (Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value))]]
let deps set = `Assoc ["set", list id (S.FunctionSet.elements set)]
let artifact (value : S.genericFunctionArtifact) = SemanticJson.record "GenericFunctionArtifact" ["Symbols", C.observationGlobalCatalog value.S.symbols; "Function", C.observationFunctionDef value.S.func; "DirectDependencies", deps value.S.directDependencies]
let artifacts values = `Assoc ["map", list (fun (name, value) -> tuple [str name; artifact value]) (M.bindings values)]
let parameters values = list (fun (id, value) -> tuple [C.observationBinding id; typ value]) values
let observe source =
 let types = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar source; AST.TInferenceVar (source, "id"); AST.TRecord (source, []); AST.TRecord ("R", [AST.TVar source]); AST.TSum ("S", [AST.TInt64]); AST.TTuple [AST.TString; AST.TVar source]; AST.TList (AST.TVar source); AST.TStream (AST.TVar source); AST.TDict (AST.TString, AST.TVar source); AST.TFunction ([AST.TUnit; AST.TVar source], AST.TInt64)] in
 let mangled = list (fun value -> tuple [str (S.typeToMangledName value); `Bool (S.containsTypeVar value); str (S.specName source [value; AST.TList value])]) types in
 let perSource source = result (fun (_, program, _) ->
  let symbols = C.programSymbols program and topLevels = C.programTopLevels program in
  let env = W.typeCheckEnvironment program in
  let generic = S.extractGenericFuncDefs program in
  let values = List.map snd (M.bindings generic) in
  let imported target = let symbols, functions = S.importSpecializedFunctions target values in tuple [C.observationGlobalCatalog symbols; list C.observationFunctionDef functions] in
  let materialize indexed =
   let symbols, tops = if indexed then H.materializeEqHelpersInTopLevelsWithIndexedSums symbols env.InstrumentedTypes.aliasReg env.InstrumentedTypes.indexedTypeReg env.InstrumentedTypes.variantLookup env.InstrumentedTypes.indexedSumTypeReg topLevels else H.materializeEqHelpersInTopLevels symbols env.InstrumentedTypes.aliasReg env.InstrumentedTypes.indexedTypeReg env.InstrumentedTypes.variantLookup topLevels in
   tuple [C.observationGlobalCatalog symbols; list C.observationTopLevel tops] in
  let bodies = List.filter_map (function C.FunctionDef f -> Some f.C.body | C.ValueDef value -> Some value.C.body | C.Expression expr -> Some expr | C.TypeDef _ -> None) topLevels in
  tuple [C.observationProgram program; artifacts generic; imported symbols; imported (C.emptySymbols ()); list (fun expr -> deps (S.directDependencies expr)) bodies;
   list materialize [false; true];
   list (fun (f : C.functionDef) -> parameters (S.normalizeSyntheticNullaryParams symbols (S.paramsToList (C.functionParameterTypes f)))) (List.filter_map (function C.FunctionDef f -> Some f | _ -> None) topLevels)])
  (Result.bind (WrittenParsing.parse Validation.Script source) (fun unit -> W.checkSourceUnitsWithBase None false false [unit])) in
 let fixtures = if source <> "" then [] else ["let id (x: 'a) : 'a = x\nid 1"; "let eq (x: 'a) (y: 'a) : Bool = x == y\neq [1] [2]"; "[1] == [2]"; "(1, true) == (2, false)"; "type R = { x: Int64 }\nR { x = 1 } == R { x = 2 }"; "type S = A of Int64 | B\nS.A 1 == S.B"; "let recurse (x: Int64) : Int64 = if x == 0 then 0 else recurse (x - 1)\nrecurse 3"; "(fun (a, b) -> a + b) (1, 2)"] in
 tuple [mangled; perSource source; list perSource fixtures]
