open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
module C = Dark_compiler.ComparisonPlanning
let observe source =
 let open! AST in
 let lookup = StringOrder.Map.of_list ["C", ("S", [], 0, [TInt64]); "S.C", ("S", [], 0, [TInt64]); "D", ("S", [], 1, []); "S.D", ("S", [], 1, []);
  "None", ("Darklang.Stdlib.Option.Option", ["a"], 0, []); "Darklang.Stdlib.Option.Option.None", ("Darklang.Stdlib.Option.Option", ["a"], 0, []);
  "Some", ("Darklang.Stdlib.Option.Option", ["a"], 1, [TVar "a"]); "Darklang.Stdlib.Option.Option.Some", ("Darklang.Stdlib.Option.Option", ["a"], 1, [TVar "a"])] in
 let names = StringOrder.Set.of_list ["S"; "Darklang.Stdlib.Option.Option"] and sums = Types.indexSumTypeRegistry lookup in
 let registry = Types.indexTypeRegistry lookup (StringOrder.Map.of_list ["R", []; "Generic", ["a"]]) (StringOrder.Map.of_list ["R", ["field", TInt64; "field", TString]; "Generic", ["field", TVar "a"]]) in
 let aliases = StringOrder.Map.of_list ["AliasInt", ([], TInt64); "AliasString", ([], TString); "AliasRecord", ([], TRecord ("R", [])); "Pair", (["a"], TTuple [TVar "a"; TVar "a"])] in
 let env = StringOrder.Map.of_list [source, TInt64; "y", TString; "z", TBool; "value", TTuple [TInt64; TString]; "scrutinee", TSum ("S", []); "guard", TBool; "tail", TString;
  "record", TRecord ("R", []); "g", TFunction ([TVar "a"; TVar "a"], TVar "a"); "f", TFunction ([TInt64; TString], TBool);
  "Dict.fn", TFunction ([TVar "k"; TVar "v"], TVar "v"); "closure", TFunction ([TUnit; TInt64], TString)] in
 let generic : Types.genericFuncRegistry = {Types.functions = StringOrder.Map.of_list ["g", ["a"]; "Dict.fn", ["k"; "v"]]; requireExplicitTypeArgsForBareCalls = true} in
 let parsed : AST.parsedRecursiveMember = {binding = bindingId 1; boundary = scopeBoundaryId 2; member = recursiveMemberId 3; sourceName = "recursive"; kind = NamedLocalFunctionMember} in
 let recursive = ResolvedRecursiveBinding {parsed; group = singletonRecursiveGroupId parsed.member; groupIndex = 0; availability = SelfRecursiveMember} in
 let variable name = lambdaParameter (LPVariable name) in
 let field value = unresolvedRecordFieldReference "field", value in
 let ops = [Add; Sub; Mul; Div; Mod; Eq; Neq; Lt; Gt; Lte; Gte; And; Or; Pow; Shl; Shr; BitAnd; BitOr; BitXor; StringConcat] in
 let extra = List.concat_map (fun op -> [BinOp (op, Int64Literal 1L, Int64Literal 2L); BinOp (op, BoolLiteral true, BoolLiteral false)]) ops @
  [ListLiteral []; ListLiteral [Int64Literal 1L; Int64Literal 2L]; ListLiteral [Int64Literal 1L; StringLiteral source];
   Let (LPVariable "n", Int64Literal 1L, InterpolatedString [StringExpr (Var "n")]);
   Let (LPTuple (LPVariable "a", LPVariable "b", []), Int64Literal 1L, Var "missing");
   Let (LPVariable "fn", Lambda (NonEmptyList.singleton (variable "x"), None, BinOp (Add, Var "x", Int64Literal 1L)), AST.applyNamed "fn" (NonEmptyList.singleton (Int64Literal 2L)));
   RecursiveLet (recursive, Lambda (NonEmptyList.singleton {pattern = LPVariable "x"; sourceAnnotation = Some TString; inferredType = Some TInt64}, Some TInt64, Var "x"), AST.applyNamed "recursive" (NonEmptyList.singleton (Int64Literal 2L)));
   Apply (Var "g", [TInt64], NonEmptyList.fromList [Int64Literal 1L; Int64Literal 2L]); Apply (Var "g", [TString], NonEmptyList.singleton (StringLiteral source));
   Apply (Var "Dict.fn", [TInt64], NonEmptyList.fromList [StringLiteral source; Int64Literal 1L]);
   Apply (Var "__raw_get", [TInt64], NonEmptyList.fromList [RuntimeError source; Int64Literal 0L]);
   C.makeInternalTypeApp (C.EqHelperDispatchTypeApp (TInt64, Int64Literal 1L, Int64Literal 2L));
   RecordLiteral (unresolvedRecordReference "R" [], [field (Int64Literal 1L)]); RecordUpdate (Var "record", [field (Int64Literal 1L); field (Int64Literal 2L)]);
   RecordAccess (Var "record", unresolvedRecordFieldReference "field"); RecordAccess (Var "record", unresolvedRecordFieldReference "___");
   Constructor (UnresolvedConstructor None, "C", [Int64Literal 1L]); Constructor (UnresolvedConstructor None, "None", []);
   AST.applyNamed "Builtin.unwrap" (NonEmptyList.singleton (Constructor (UnresolvedConstructor None, "None", [])));
   If (BoolLiteral true, ListLiteral [], ListLiteral [Int64Literal 1L]); If (Int64Literal 1L, UnitLiteral, UnitLiteral);
   TupleLiteral [AST.applyNamed "Builtin.testRuntimeError" (NonEmptyList.singleton (StringLiteral source)); Int64Literal 1L];
   DictLiteral (TUnit, TUnit, [StringLiteral source, Int64Literal 1L; StringLiteral source, Int64Literal 2L]);
   DictLiteral (TUnit, TUnit, [FloatLiteral (Int64.float_of_bits 0xfff8000000000000L), UnitLiteral; FloatLiteral (Int64.float_of_bits 0x7ff8000000000001L), UnitLiteral]);
   DictLiteral (TUnit, TUnit, []); DictLiteral (TUnit, TUnit, [StringLiteral source, UnitLiteral]);
   Apply (Lambda (NonEmptyList.fromList [variable "x"; variable "y"], None, Var "x"), [], NonEmptyList.singleton (Int64Literal 1L)); Closure ("closure", [UnitLiteral]);
   Match (BoolLiteral true, [{patterns = NonEmptyList.singleton (PBool true); guard = None; body = Int64Literal 1L}; {patterns = NonEmptyList.singleton (PBool false); guard = None; body = Int64Literal 2L}])]
 in
 let expressions = CheckingTypesObservation.additionalExpressions source @ extra in
 let expectations = [None; Some TUnit; Some TInt64; Some TInt128; Some TInt; Some TBool; Some TString; Some TChar; Some TFloat64; Some (TVar source);
  Some (TRecord ("AliasInt", [])); Some (TRecord ("AliasString", [])); Some (TRecord ("R", [])); Some (TSum ("S", [])); Some (TList TInt64); Some (TTuple [TInt64; TString]); Some (TFunction ([TInt64], TInt64)); Some (TDict (TString, TInt64))] in
 let cases = if source = "" then List.concat_map (fun value -> List.map (fun expected -> value, expected) expectations) expressions else List.mapi (fun index value -> value, List.nth expectations (index mod List.length expectations)) expressions in
 list (fun (value, expected) ->
  try
   let result = CheckExpressions.checkExprWithParamNamesAndSumTypeNames (StringOrder.Map.singleton "f" ["first"; "second"]) names sums value env registry lookup generic AST.defaultWarningSettings (DarkStdlib.buildModuleRegistry ()) aliases expected in
   let encoded = match result with Ok (typ, value) -> SemanticJson.union "FSharpResult" "Ok" [tuple [SemanticAST.semanticType typ; SemanticAST.expr value]] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticDiagnostics.typeError error] in
   `Assoc ["result", encoded]
  with Failure message -> `Assoc ["crash", SemanticJson.string message]) cases
