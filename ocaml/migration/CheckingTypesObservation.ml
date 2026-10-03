open Dark_compiler
module C = Types
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let map encode values = `Assoc ["map", `List (List.map (fun (key, value) -> tuple [SemanticJson.string key; encode value]) (M.bindings values))]
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let option encode = function Some value -> SemanticJson.union "FSharpOption" "Some" [encode value] | None -> SemanticJson.union "FSharpOption" "None" []
let types = SemanticAST.semanticType
let typeList values = `List (List.map types values)
let fields values = `List (List.map (fun (name, typ) -> tuple [SemanticJson.string name; types typ]) values)
let recordInfo (info : C.recordTypeInfo) = SemanticJson.record "RecordTypeInfo" ["Fields", fields info.C.fields; "FieldTypes", map types info.C.fieldTypes; "TypeParams", `List (List.map SemanticJson.string info.C.typeParams)]
let sumInfo (info : C.sumTypeInfo) = SemanticJson.record "SumTypeInfo" ["TypeParams", `List (List.map SemanticJson.string info.C.typeParams); "Variants", `List (List.map (fun (variant : C.sumVariantInfo) -> SemanticJson.record "SumVariantInfo" ["Name", SemanticJson.string variant.C.name; "Tag", SemanticJson.int32 variant.C.tag; "Fields", typeList variant.C.fields]) info.C.variants)]
let additionalExpressions source =
  let open! AST in
  let x = Var source and y = Var "y" in
  let field = unresolvedRecordFieldReference "field" in
  let patterns = [PUnit; PWildcard; PVar source; PConstructor ("C", [PVar source; PVar "y"]); PResolvedConstructor ("M.T", "C", 3, [PVar source]); PInt64 1L; PBigInt Z.one; PInt128Literal Z.one; PInt8Literal 1; PInt16Literal 1; PInt32Literal 1l; PUInt8Literal 1; PUInt16Literal 1; PUInt32Literal 1L; PUInt64Literal 1L; PUInt128Literal Z.one; PBool true; PString source; PChar source; PFloat 1.; PTuple [PVar source; PVar "y"]; PList [PVar source]; PListCons ([PVar source], PVar "tail"); POr (NonEmptyList.fromList [PVar source; PVar "other"])] in
  let literalExpressions : AST.expr list = [UnitLiteral; Int64Literal 1L; Int128Literal Z.one; BigIntLiteral Z.one; Int8Literal 1; Int16Literal 1; Int32Literal 1l; UInt8Literal 1; UInt16Literal 1; UInt32Literal 1L; UInt64Literal 1L; UInt128Literal Z.one; BoolLiteral true; StringLiteral source; CharLiteral source; FloatLiteral 1.; RuntimeError source] in
  let expressions = literalExpressions @ [x; Var "Builtin.testNan"; Var "Builtin.testInfinity"; BoundaryRender (source, x); BinOp (Add, x, y); UnaryOp (Neg, x);
    Let (LPVariable source, x, TupleLiteral [x; y]); Let (LPTuple (LPVariable source, LPVariable "y", []), Var "value", TupleLiteral [x; y]);
    RecursiveLet (RecursiveBindingCandidate {sourceName = source; kind = NamedLocalFunctionMember}, Apply (x, [], NonEmptyList.singleton y), TupleLiteral [x; y]);
    If (x, y, Var "z"); Sequence (x, y); Apply (x, [], NonEmptyList.fromList [y; Var "z"]); TupleLiteral [x; y]; TupleAccess (x, 1);
    DictLiteral (TString, TString, [x, y]); RecordLiteral (unresolvedRecordReference "R" [], [field, x]); RecordUpdate (x, [field, y]); RecordAccess (x, field);
    Constructor (UnresolvedConstructor None, "C", [x; y]); ListLiteral [x; y]; Lambda (NonEmptyList.singleton (lambdaParameter (LPVariable source)), None, TupleLiteral [x; y]);
    Apply (TupleAccess (x, 0), [], NonEmptyList.singleton y); IndirectApply (x, NonEmptyList.singleton y); Closure (source, [x; y]);
    InterpolatedString [StringText source; StringExpr x; StringExpr y]] @ List.map (fun pattern -> Match (Var "scrutinee", [{patterns = NonEmptyList.singleton pattern; guard = Some (Var "guard"); body = TupleLiteral [x; y; Var "tail"]}])) patterns in
  expressions
let observe source =
 let samples = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr;
 AST.TVar source; AST.TVar "a"; AST.TInferenceVar (source, "a"); AST.TFunction ([AST.TVar "a"], AST.TVar "b"); AST.TTuple [AST.TVar "b"; AST.TVar "a"];
 AST.TRecord ("Outer", [AST.TVar "b"]); AST.TSum ("Outer", [AST.TVar "b"]); AST.TRecord ("Outer", []); AST.TSum ("Outer", []);
 AST.TRecord ("Outer", [AST.TUnit; AST.TBool]); AST.TSum ("Outer", [AST.TUnit; AST.TBool]); AST.TList (AST.TVar "a"); AST.TStream (AST.TVar "a"); AST.TDict (AST.TVar "a", AST.TVar "b"); AST.TRecord ("S", []); AST.TSum ("R", [AST.TVar "a"])] in
 let substitutions = [M.empty; M.of_list ["a", AST.TVar "b"; "b", AST.TInt64]; M.of_list ["a", AST.TVar "b"; "b", AST.TVar "a"]; M.singleton "a" (AST.TList (AST.TVar "a"))] in
 let aliases = M.of_list ["Outer", (["a"], AST.TRecord ("Inner", [AST.TString; AST.TVar "a"])); "Inner", (["a"; "b"], AST.TRecord ("R", [AST.TVar "a"; AST.TVar "b"])); "Plain", ([], AST.TRecord ("R", [])); "Number", ([], AST.TInt64)] in
 let lookup = M.of_list ["S.Second", ("S", [], 2, [AST.TString]); "S.First", ("S", [], 0, []); "S.FirstAlias", ("S", [], 0, [AST.TBool]); "First", ("S", [], 0, []); "T.First", ("T", [], 0, [AST.TInt64])] in
 let registry = M.of_list ["R", ["x", AST.TSum ("R", [AST.TVar "a"]); "x", AST.TBool; "y", AST.TRecord ("S", [AST.TVar "b"])]] in
 let indexed = C.indexTypeRegistry lookup (M.singleton "R" ["a"; "b"]) registry in
 let perType = List.map (fun typ -> tuple [
   `List (List.map (fun subst -> tuple [types (C.applySubst subst typ); types (C.applyTypeArguments subst typ)]) substitutions);
   `List (List.map SemanticJson.string (C.collectTypeVarsInType typ ["existing"])); types (C.resolveAliasTargetType aliases typ);
   types (C.resolveType aliases typ); types (C.canonicalizeBareSumTypeRefsWithNames (StringOrder.Set.singleton "S") typ);
   types (C.canonicalizeDeclaredTypeRefsWithSumTypeNames registry (StringOrder.Set.singleton "S") typ);
   `List (List.map (fun other -> `Bool (C.typesEqual aliases typ other)) samples)]) samples in
 let exprs = additionalExpressions source @ [AST.Apply (AST.Var source, [AST.TVar "a"], NonEmptyList.singleton (AST.Var "x"));
 AST.Lambda (NonEmptyList.singleton {AST.pattern = AST.LPVariable "x"; sourceAnnotation = Some (AST.TVar "a"); inferredType = Some (AST.TVar "b")}, Some (AST.TVar "a"), AST.Var "x");
 AST.RecordLiteral ({AST.sourceTypeName = "Outer"; resolvedTypeName = "R"; typeArgs = [AST.TVar "a"]}, []);
 AST.DictLiteral (AST.TVar "a", AST.TVar "b", [AST.Var source, AST.UnitLiteral]);
 AST.Constructor (AST.ResolvedConstructor ([], "S", [AST.TVar "a"]), "First", [])] in
 let arities = List.concat_map (fun expected -> List.map (fun actual ->
   let params = List.init expected (fun index -> "a" ^ string_of_int index) and args = List.init actual (fun _ -> AST.TInt64) in
   tuple [result (map types) (C.buildRecordFieldSubstitutionFromParams params args); result (map types) (C.buildSubstitution params args);
     SemanticJson.string (C.formatTypeArgumentArityError source expected actual); SemanticJson.string (C.formatValueArgumentArityError source expected actual)]) [0;1;2;3]) [0;1;2;3] in
 tuple [`List perType; map recordInfo indexed; map sumInfo (C.indexSumTypeRegistry lookup);
   map fields (C.resolveAliasesInTypeRegistry aliases registry); `List arities;
   `List (List.map (fun expr -> SemanticAST.expr (C.applySubstToExpr (List.nth substitutions 1) expr)) exprs);
   option (fun (name, resolvedFields) -> tuple [SemanticJson.string name; fields resolvedFields]) (C.tryResolveGenericRecordAliasFields aliases indexed "Outer");
   option (fun (name, args, info) -> tuple [SemanticJson.string name; typeList args; recordInfo info]) (C.tryResolveRecordLiteralInfo aliases indexed {AST.sourceTypeName = "Outer"; resolvedTypeName = "ignored"; typeArgs = [AST.TInt64]});
   SemanticJson.string (C.resolveTypeName aliases "Plain"); SemanticJson.int32 (C.unqualifiedVariantOwnerCount "First" lookup);
   `List (List.map (fun expr -> SemanticJson.string (C.formatLegacyRecordFieldTypeError aliases source AST.TString (AST.TRecord ("Number", [])) expr)) [AST.StringLiteral source; AST.Var source; AST.FloatLiteral (-0.)])]
