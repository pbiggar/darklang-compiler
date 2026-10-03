[@@@warning "-4"]
open Dark_compiler
module C = InstrumentedCheckedAST
module I = InstrumentedLoweringTypeInference
module O = InstrumentedLoweringOperators
module P = InstrumentedLoweringPrimitives
module R = InstrumentedTypeRegistries
module W = InstrumentedWrittenChecking
module T = InstrumentedTypes
module A = InstrumentedANF
module M = StringOrder.Map
module B = C.BindingIdMap
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [str error]
let attempt encode action = result encode (try Ok (action ()) with Failure message -> Error message | Invalid_argument message -> Error message)
let id value = let value = AST.functionIdValue value in SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind", `String "uint64"; "value", `String (Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value))]]
let types = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar "a"; AST.TInferenceVar ("scope", "fixed"); AST.TRecord ("R", []); AST.TRecord ("Generic", [AST.TString]); AST.TSum ("S", []); AST.TSum ("Nullable", []); AST.TSum ("Transparent", []); AST.TSum ("Uuid", []); AST.TTuple []; AST.TTuple [AST.TInt64; AST.TTuple [AST.TString; AST.TInt128]]; AST.TList AST.TInt64; AST.TStream AST.TInt64; AST.TDict (AST.TString, AST.TInt64); AST.TFunction ([AST.TInt64], AST.TBool)]
let operators = [AST.Add; AST.Sub; AST.Mul; AST.Div; AST.Mod; AST.Pow; AST.Shl; AST.Shr; AST.BitAnd; AST.BitOr; AST.BitXor; AST.Eq; AST.Neq; AST.Lt; AST.Gt; AST.Lte; AST.Gte; AST.And; AST.Or; AST.StringConcat]
let observeTypes source =
 let intrinsics = ["Builtin.unwrap"; "Builtin.testRuntimeError"; "Builtin.crash"; "Builtin.pmEvaluateValue_i64"; "__raw_get_i64"; "__raw_get_list_i64"; "__raw_take_str"; "__stream_to_rawptr_a"; "__rawptr_to_stream_i64"; "__raw_slot_init_i64"; "__hash_i64"; "__key_eq_str"; "__empty_dict_str_i64"; "__dict_is_null_str_i64"; "__dict_get_tag_str_i64"; "__dict_to_rawptr_str_i64"; "__rawptr_to_dict_str_i64"; "__list_is_null_i64"; "__list_get_tag_i64"; "__list_to_rawptr_i64"; "__rawptr_to_list_i64"; "__list_empty_i64"; "__raw_get_invalid__"; "Darklang.Stdlib.File.exists"; "unknown"] in
 let program program =
  let symbols = C.programSymbols program in let tops = C.programTopLevels program in
  let env = W.typeCheckEnvironment program in
  let registry = M.map (fun (info : T.recordTypeInfo) -> {R.typeParams = info.T.typeParams; fields = info.T.fields}) env.T.indexedTypeReg in
  let variants = env.T.variantLookup in let sums = P.sumMetadataFromVariantLookup variants in
  let typeNames = R.typeNamesFromSymbols symbols in
  let funcs = List.filter_map (function C.FunctionDef value -> Some value | _ -> None) tops in
  let functions = FunctionIdMap.ofList (List.map (fun (func : C.functionDef) -> func.C.id, (func.C.name, AST.TFunction (C.functionParameterTypes func |> NonEmptyList.toList |> List.map snd, C.functionReturnType func))) funcs) in
  let names = FunctionIdMap.map (fun _ (name, _) -> name) functions in
  let environment = C.programValues program |> M.bindings |> List.map (fun (name, (typ, _)) -> AST.topLevelValueId name, typ) |> List.to_seq |> B.of_seq in
  let environments = [environment; List.fold_left (fun env (id, typ) -> B.add id typ env) environment [AST.bindingId 0, AST.TInt64; AST.bindingId 1, AST.TString; AST.namedBindingId 0 "x", AST.TInt64; AST.namedBindingId 1 "x", AST.TList AST.TString]] in
  let bodies = List.filter_map (function C.FunctionDef func -> Some func.C.body | C.ValueDef value -> Some value.C.body | C.Expression value -> Some value | C.TypeDef _ -> None) tops in
  let expression expr = list (fun environment -> list (fun modules -> attempt (result typ) (fun () -> I.inferTypeCore sums typeNames expr environment registry variants functions names modules)) [M.empty; DarkStdlib.buildModuleRegistry ()]) environments in
  let intrinsic name = let names = FunctionIdMap.add (AST.functionId 9L) name names in
   list (fun args -> list (fun functionRegistry -> attempt (result typ) (fun () -> I.inferTypeCore sums typeNames (C.Call (AST.functionId 9L, args)) environment registry variants functionRegistry names (DarkStdlib.buildModuleRegistry ()))) [FunctionIdMap.empty; FunctionIdMap.add (AST.functionId 9L) ("other", AST.TInt64) functions; FunctionIdMap.add (AST.functionId 9L) (name, AST.TFunction ([AST.TUnit], AST.TBool)) functions]) [NonEmptyList.singleton C.UnitLiteral; NonEmptyList.singleton (C.Int64Literal 1L); NonEmptyList.fromList [C.Int64Literal 1L; C.Int64Literal 2L]] in
  tuple [list expression bodies; if source = "" then list intrinsic intrinsics else `List [];
   if source = "" then list (fun typ_ -> let environment = B.add (AST.bindingId 0) typ_ (B.add (AST.bindingId 1) typ_ environment) in let infer expr = attempt (result typ) (fun () -> I.inferTypeCore sums typeNames expr environment registry variants functions names (DarkStdlib.buildModuleRegistry ())) in tuple [list (fun op -> infer (C.BinOp (op, C.Local (AST.bindingId 0), C.Local (AST.bindingId 1)))) operators; list (fun op -> infer (C.UnaryOp (op, C.Local (AST.bindingId 0)))) [AST.Neg; AST.Not; AST.BitNot]]) types else `List []] in
 let sourceProgram source = result (fun (_, value, _) -> program value) (Result.bind (WrittenParsing.parse Validation.Script source) (fun unit -> W.checkSourceUnitsWithBase None false false [unit])) in
 tuple [list (result program) (C.observationFixtures source); sourceProgram source;
  if source = "" then list sourceProgram ["type R<'a> = { value: 'a }\nR { value = 1 }"; "type S<'a> = A of 'a | B\nmatch S.A 1 with | S.A x -> x | S.B -> 0"; "if true then [] else [1]"; "let f = fun (x: Int64) -> x\nf 1"] else `List []]
let observeOperators source =
 if source <> "" then `List [] else
 let registry = M.of_list ["R", {R.typeParams = []; fields = ["a", AST.TInt64; "b", AST.TTuple [AST.TString; AST.TInt128]]}; "Generic", {R.typeParams = ["a"]; fields = ["value", AST.TVar "a"]}] in
 let variants = M.of_list ["S.A", ("S", [], 0, [AST.TInt64; AST.TBool]); "S.B", ("S", [], 1, []); "Nullable.None", ("Nullable", [], 0, []); "Nullable.Some", ("Nullable", [], 1, [AST.TString]); "Transparent.A", ("Transparent", [], 0, [AST.TChar]); "Uuid.Uuid", ("Uuid", [], 0, [AST.TUInt128])] in
 let cases = P.sumRepresentationIndex variants in
 let resolved encode action = let requests = ref [] in let resolve name = requests := !requests @ [name]; AST.functionId 9L in let value = attempt encode (fun () -> action resolve) in tuple [value; list str !requests] in
 tuple [list (fun op -> attempt SemanticANF.aNF_binOp (fun () -> O.convertBinOp op)) operators;
  list (fun typ -> list (fun op -> resolved (fun value -> match value with None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [id value]) (fun resolve -> O.integerFunctionForBinOp resolve typ op)) operators) types;
  list (fun op -> SemanticANF.aNF_unaryOp (O.convertUnaryOp op)) [AST.Neg; AST.Not; AST.BitNot]; list (fun typ -> `Bool (O.isCompoundType typ)) types;
  list (fun typ -> list (fun gen -> resolved (fun (bindings, atom, gen) -> tuple [list (fun (id, expr) -> tuple [SemanticANF.aNF_tempId id; SemanticANF.aNF_cExpr expr]) bindings; SemanticANF.aNF_atom atom; SemanticANF.aNF_varGen gen]) (fun resolve -> O.generateStructuralEquality resolve (A.Var (A.TempId 0)) (A.Var (A.TempId 1)) typ (A.VarGen gen) registry variants cases)) [0; -2147483648; 2147483647]) types]
let observeAggregates source =
 let module G = InstrumentedLoweringAggregates in
 let bindings = list (fun (id, expr) -> tuple [SemanticANF.aNF_tempId id; SemanticANF.aNF_cExpr expr]) in
 let environment value = `Assoc ["map", list (fun (id, (temp, typ_)) -> tuple [C.observationBinding id; tuple [SemanticANF.aNF_tempId temp; typ typ_]]) (R.BindingMap.bindings value)] in
 let initial = [A.TempId (-7), A.Atom (A.StringLiteral source); A.TempId (-6), A.TypedAtom (A.UnitLiteral, AST.TUnit)] in
 let ids = [AST.bindingId 0; AST.namedBindingId 0 "x"; AST.namedBindingId 1 "x"; AST.topLevelValueId source] in
 let patterns = [C.LPUnit; C.LPWildcard] @ List.map (fun id -> C.LPVariable id) ids @ [C.LPTuple (C.LPVariable (List.hd ids), C.LPVariable (List.hd ids), []); C.LPTuple (C.LPUnit, C.LPWildcard, []); C.LPTuple (C.LPTuple (C.LPVariable (List.hd ids), C.LPWildcard, []), C.LPVariable (List.nth ids 1), [C.LPUnit])] in
 let patternTypes = [AST.TUnit; AST.TInt64; AST.TTuple []; AST.TTuple [AST.TInt64; AST.TString]; AST.TTuple [AST.TUnit; AST.TString]; AST.TTuple [AST.TTuple [AST.TBool; AST.TString]; AST.TInt64; AST.TUnit]; AST.TTuple [AST.TInt64]] in
 let env = R.BindingMap.of_seq (List.to_seq [List.hd ids, (A.TempId (-2), AST.TBool); List.nth ids 1, (A.TempId (-3), AST.TString)]) in
 tuple [list (fun pattern -> list (fun typ_ -> tuple [`Bool (G.letPatternAcceptsType pattern typ_); list (fun gen -> result (fun (env, reversed, gen) -> tuple [environment env; bindings reversed; SemanticANF.aNF_varGen gen]) (G.lowerLetPatternBindings pattern (A.Var (A.TempId (-1))) typ_ env initial (A.VarGen gen))) [0; -2147483648; 2147483647]]) patternTypes) patterns;
  (if source = "" then list (fun typ_ -> list (fun count -> list (fun gen -> let elements = List.init count (fun index -> A.IntLiteral (A.Int64 (Int64.of_int index)), typ_) in let atom, prefix, gen = G.buildSkewListLiteral (AST.TList typ_) elements (A.VarGen gen) initial in tuple [SemanticANF.aNF_atom atom; bindings prefix; SemanticANF.aNF_varGen gen]) [0; -2147483648; 2147483647]) [0;1;2;3;6;7;14;15;31;32;64]) types else `List [])]
