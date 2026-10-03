open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let top = SemanticAST.observationTopLevelNode typ
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticDiagnostics.typeError error]
let observe source =
  let open! AST in
  let x = Var source and y = Var "y" in
  let field = unresolvedRecordFieldReference "field" in
  let patterns = [PUnit; PWildcard; PVar source; PConstructor ("C", [PVar source; PVar "y"]); PResolvedConstructor ("M.T", "C", 3, [PVar source]); PInt64 1L; PBigInt Z.one; PInt128Literal Z.one; PInt8Literal 1; PInt16Literal 1; PInt32Literal 1l; PUInt8Literal 1; PUInt16Literal 1; PUInt32Literal 1L; PUInt64Literal 1L; PUInt128Literal Z.one; PBool true; PString source; PChar source; PFloat 1.; PTuple [PVar source; PVar "y"]; PList [PVar source]; PListCons ([PVar source], PVar "tail"); POr (NonEmptyList.fromList [PVar source; PVar "other"])] in
  let literalExpressions : AST.expr list = [UnitLiteral; Int64Literal 1L; Int128Literal Z.one; BigIntLiteral Z.one; Int8Literal 1; Int16Literal 1; Int32Literal 1l; UInt8Literal 1; UInt16Literal 1; UInt32Literal 1L; UInt64Literal 1L; UInt128Literal Z.one; BoolLiteral true; StringLiteral source; CharLiteral source; FloatLiteral 1.; RuntimeError source] in
  let expressions = literalExpressions @ [x; Var "Builtin.testNan"; Var "Builtin.testInfinity"; BoundaryRender (source, x); BinOp (Add, x, y); UnaryOp (Neg, x);
    Let (LPVariable source, x, TupleLiteral [x; y]); Let (LPTuple (LPVariable source, LPVariable "y", []), Var "value", TupleLiteral [x; y]);
    RecursiveLet (ParsedRecursiveBinding {binding = bindingId 1; boundary = scopeBoundaryId 2; member = recursiveMemberId 3; sourceName = source; kind = NamedLocalFunctionMember}, Apply (x, [], NonEmptyList.singleton y), TupleLiteral [x; y]);
    If (x, y, Var "z"); Sequence (x, y); Apply (x, [], NonEmptyList.fromList [y; Var "z"]); TupleLiteral [x; y]; TupleAccess (x, 1);
    DictLiteral (TString, TString, [x, y]); RecordLiteral (unresolvedRecordReference "R" [], [field, x]); RecordUpdate (x, [field, y]); RecordAccess (x, field);
    Constructor (UnresolvedConstructor None, "C", [x; y]); ListLiteral [x; y]; Lambda (NonEmptyList.singleton (lambdaParameter (LPVariable source)), None, TupleLiteral [x; y]);
    Apply (TupleAccess (x, 0), [], NonEmptyList.singleton y); IndirectApply (x, NonEmptyList.singleton y); Closure (source, [x; y]);
    InterpolatedString [StringText source; StringExpr x; StringExpr y]] @ List.map (fun pattern -> Match (Var "scrutinee", [{patterns = NonEmptyList.singleton pattern; guard = Some (Var "guard"); body = TupleLiteral [x; y; Var "tail"]}])) patterns in
  let parsed name ordinal = {AST.binding = bindingId ordinal; boundary = scopeBoundaryId 0; member = recursiveMemberId ordinal; sourceName = name; kind = TopLevelFunctionMember} in
  let definition name ordinal body : AST.functionDef = {name; typeParams = []; params = NonEmptyList.fromList (List.map (fun name -> name, TUnit) [source; "y"; "z"; "value"; "scrutinee"; "guard"; "tail"]); returnType = TUnit; body; recursion = Some (ParsedRecursiveBinding (parsed name ordinal))} in
  let declarations = [TypeDef (RecordDef ("R", [], ["field", TInt64])); TypeDef (SumTypeDef ("S", [], [{name = "C"; fields = [TInt64]}]));
    TypeDef (TypeAlias ("Alias", [], TRecord ("R", []))); TypeDef (RecordDef ("M.T", [], ["field", TList TInt64]));
    FunctionDef (definition "M.f" 0 (AST.applyNamed "g" (NonEmptyList.singleton UnitLiteral)));
    FunctionDef (definition "M.g" 1 (AST.applyNamed "f" (NonEmptyList.singleton UnitLiteral)));
    FunctionDef (definition "loop" 2 (AST.applyNamed "loop" (NonEmptyList.singleton UnitLiteral)));
    FunctionDef (definition "ordinary" 3 UnitLiteral); ValueDef (UncheckedValueDef ("M.value", StringLiteral source))] in
  let moduleRegistry = StringOrder.Map.of_list ["Intrinsic.fn", {AST.name = "Intrinsic.fn"; typeParams = []; paramTypes = [TUnit]; returnType = TUnit}; "M.f", {AST.name = "M.f"; typeParams = []; paramTypes = [TUnit]; returnType = TUnit}] in
  let aliases = StringOrder.Map.singleton "Alias" ([], TRecord ("R", [])) in
  let environment = ResolveDeclarations.declarationResolutionEnvironment declarations moduleRegistry true in
  let candidate (candidate : NameResolution.candidate) = SemanticJson.record "Candidate" ["VisibleName", SemanticDiagnostics.qualified candidate.NameResolution.visibleName; "Identity", SemanticDiagnostics.identity candidate.NameResolution.identity; "Provenance", SemanticDiagnostics.provenance candidate.NameResolution.provenance] in
  let resolve topLevels = result (SemanticAST.observationProgramNode typ) (ResolveDeclarations.resolveProgramNames environment aliases (StringOrder.Set.of_list ["R"; "M.T"]) (Program topLevels)) in
  let validationCases = [declarations; []; [TypeDef (RecordDef ("Empty", [], []))]; [TypeDef (SumTypeDef ("Empty", [], []))];
    [TypeDef (RecordDef ("Unknown", [], ["field", TRecord (source, [])]))]; [TypeDef (RecordDef ("Undeclared", [], ["field", TVar source]))];
    [TypeDef (RecordDef ("Duplicate", [source; source], ["field", TVar source]))];
    [TypeDef (SumTypeDef ("Duplicate", [], [{name = source; fields = []}; {name = source; fields = []}]))];
    [TypeDef (TypeAlias ("Cycle", [], TRecord ("Cycle", [])))];
    [TypeDef (TypeAlias ("A", [], TRecord ("B", []))); TypeDef (TypeAlias ("B", [], TRecord ("A", [])))];
    [TypeDef (RecordDef ("R", ["a"], ["field", TVar "a"])); TypeDef (RecordDef ("Owner", [], ["field", TRecord ("R", [])]))];
    [TypeDef (RecordDef ("R", [], ["field", TInt64])); TypeDef (RecordDef ("R", [], ["other", TString]))];
    [TypeDef (SumTypeDef ("A", [], [{name = "C"; fields = []}])); TypeDef (SumTypeDef ("B", [], [{name = "C"; fields = []}]))]] in
  let summary (value : ResolveDeclarations.topLevelDeclarationSummary) =
    let map encode values = `Assoc ["map", list (fun (key, value) -> tuple [str key; encode value]) (StringOrder.Map.bindings values)] in
    let params = list str in let fields = list (fun (name, value) -> tuple [str name; typ value]) in
    SemanticJson.record "TopLevelDeclarationSummary" ["TypeReg", map fields value.ResolveDeclarations.typeReg; "RecordTypeParams", map params value.ResolveDeclarations.recordTypeParams;
      "AliasReg", map (fun (names, target) -> tuple [params names; typ target]) value.ResolveDeclarations.aliasReg;
      "VariantLookup", map (fun (name, names, tag, fields) -> tuple [str name; params names; SemanticJson.int32 tag; list typ fields]) value.ResolveDeclarations.variantLookup;
      "FuncSigs", map (fun (args, ret) -> tuple [list typ args; typ ret]) value.ResolveDeclarations.funcSigs;
      "FuncParamNames", map params value.ResolveDeclarations.funcParamNames; "GenericFuncs", map params value.ResolveDeclarations.genericFuncs] in
  tuple [list candidate (NameResolution.candidates environment);
    list candidate (NameResolution.candidates (ResolveDeclarations.declarationResolutionEnvironment declarations moduleRegistry false));
    list top (ResolveDeclarations.resolveRecursiveDeclarationGroups declarations); resolve declarations;
    list (fun expression -> resolve [FunctionDef (definition "probe" 4 expression)]) expressions;
    list (fun values -> tuple [result (fun () -> `Null) (Declarations.validateTopLevelTypeDeclarations None values); summary (Declarations.summarizeTopLevelDeclarations values)]) validationCases]
