open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let observe source =
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
  let paramGroups = [NonEmptyList.singleton (lambdaParameter (LPVariable source));
    NonEmptyList.singleton (typedLambdaVariable source TString);
    NonEmptyList.singleton (lambdaParameter (LPTuple (LPVariable source, LPVariable "y", [])));
    NonEmptyList.fromList [lambdaParameter (LPVariable source); lambdaParameter (LPVariable "y")]] in
  let expectations = [None; Some TInt64; Some (TFunction ([], TUnit)); Some (TFunction ([TInt64], TInt64));
    Some (TFunction ([TVar source], TVar source)); Some (TFunction ([TInt64; TString], TBool))] in
  let annotations = [None; Some TUnit; Some TInt64; Some (TVar source)] in
  let env = StringOrder.Map.of_list ["y", TString; "f", TFunction ([TInt64], TString); "value", TInt64] in
  let modules = DarkStdlib.buildModuleRegistry () in
  let generic : Types.genericFuncRegistry = {Types.functions = StringOrder.Map.empty; requireExplicitTypeArgsForBareCalls = false} in
  let bodies = expressions @ [AST.applyNamed "f" (NonEmptyList.singleton x); AST.applyNamed "Darklang.Stdlib.Int64.toFloat" (NonEmptyList.singleton x)] in
  let cases = if source = "" then List.concat_map (fun params -> List.concat_map (fun expected -> List.concat_map (fun annotation -> List.map (fun body -> params, expected, annotation, body) bodies) annotations) expectations) paramGroups
   else List.mapi (fun index body -> List.nth paramGroups (index mod List.length paramGroups), List.nth expectations (index mod List.length expectations), List.nth annotations (index mod List.length annotations), body) bodies in
  let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value] in
  list (fun (params, expected, annotation, body) ->
    let trace = ref [] in
    let callback value env _registry _lookup _generic _warnings _modules _aliases expected =
      trace := !trace @ [value, env, expected];
      let typ = match value with Var name -> Option.value (StringOrder.Map.find_opt name env) ~default:TUnit | RuntimeError _ -> TNever | Int64Literal _ -> TInt64 | StringLiteral _ -> TString | BoolLiteral _ -> TBool | _ -> Option.value expected ~default:TUnit in
      Ok (typ, value) in
    let result = CheckLambdas.check callback env StringOrder.Map.empty StringOrder.Map.empty generic AST.defaultWarningSettings modules StringOrder.Map.empty expected params annotation body in
    let result = match result with Ok (typ, value) -> SemanticJson.union "FSharpResult" "Ok" [tuple [SemanticAST.semanticType typ; SemanticAST.expr value]] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticDiagnostics.typeError error] in
    tuple [result; list (fun (value, env, expected) -> tuple [SemanticAST.expr value; `Assoc ["map", list (fun (name, typ) -> tuple [SemanticJson.string name; SemanticAST.semanticType typ]) (StringOrder.Map.bindings env)]; option SemanticAST.semanticType expected]) !trace]) cases
[@@warning "-4"]
