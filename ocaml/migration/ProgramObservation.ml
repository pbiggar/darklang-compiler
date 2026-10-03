open Dark_compiler
open! InstrumentedTypes
module N = InstrumentedNameResolution
module D = InstrumentedSemanticDiagnostics
module C = InstrumentedCheckedAST
module T = InstrumentedTypeChecking
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let record = SemanticJson.record
let map encode values = `Assoc ["map", list (fun (name, value) -> tuple [str name; encode value]) (StringOrder.Map.bindings values)]
let strings = list str
let fields = list (fun (name, value) -> tuple [str name; typ value])
let candidate (value : N.candidate) = record "Candidate" ["VisibleName", D.qualified value.N.visibleName; "Identity", D.identity value.N.identity; "Provenance", D.provenance value.N.provenance]
let resolution value =
 let ordered, indexed, imported, importedIndex = N.observationParts value in
 let index values = `Assoc ["map", list (fun (name, candidates) -> tuple [D.qualified name; list candidate candidates]) values] in
 record "ResolutionEnvironment" ["OrderedCandidates", list candidate ordered; "CandidatesByVisibleName", index indexed; "ImportedOrderedCandidates", list candidate imported; "ImportedCandidatesByVisibleName", index importedIndex]
let env (value : typeCheckEnv) =
 let recordInfo (info : recordTypeInfo) = record "RecordTypeInfo" ["Fields", fields info.fields; "FieldTypes", map typ info.fieldTypes; "TypeParams", strings info.typeParams] in
 let sumInfo (info : sumTypeInfo) = record "SumTypeInfo" ["TypeParams", strings info.typeParams; "Variants", list (fun (variant : sumVariantInfo) -> record "SumVariantInfo" ["Name", str variant.name; "Tag", SemanticJson.int32 variant.tag; "Fields", list typ variant.fields]) info.variants] in
 let set values = `Assoc ["set", strings (StringOrder.Set.elements values)] in
 record "TypeCheckEnv" [
  "TypeCatalog", C.observationTypeCatalog value.typeCatalog; "FunctionCatalog", C.observationFunctionCatalog value.functionCatalog;
  "TypeReg", map fields value.typeReg; "IndexedTypeReg", map recordInfo value.indexedTypeReg; "RecordTypeNames", set value.recordTypeNames;
  "VariantLookup", map (fun (name, params, tag, args) -> tuple [str name; strings params; SemanticJson.int32 tag; list typ args]) value.variantLookup;
  "IndexedSumTypeReg", map sumInfo value.indexedSumTypeReg; "SumTypeNames", set value.sumTypeNames;
  "FuncEnv", map typ value.funcEnv; "Values", map typ value.values; "FuncParamNames", map strings value.funcParamNames;
  "GenericFuncReg", record "GenericFuncRegistry" ["Functions", map strings value.genericFuncReg.functions; "RequireExplicitTypeArgsForBareCalls", `Bool value.genericFuncReg.requireExplicitTypeArgsForBareCalls];
  "GenericFuncDefs", map (SemanticAST.observationFunctionDefNode typ) value.genericFuncDefs; "ModuleRegistry", map SemanticAST.observationModuleFunc value.moduleRegistry;
  "AliasReg", map (fun (params, target) -> tuple [strings params; typ target]) value.aliasReg; "ResolutionEnv", resolution value.resolutionEnv]
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [D.typeError error]
let checked (value, program, environment) = tuple [typ value; C.observationProgram program; env environment]
let typed (value, program, environment) = tuple [typ value; SemanticAST.observationProgramNode typ program; env environment]
let[@warning "-4"] observe source =
 let open! AST in
 let func name params ret body : functionDef = {name; typeParams = []; params = NonEmptyList.fromList params; returnType = ret; body; recursion = None} in
 let generic = { (func "identity" ["x", TVar "a"] (TVar "a") (Var "x")) with typeParams = ["a"]} in
 let declarations = [TypeDef (RecordDef ("R", [], ["field", TInt64])); TypeDef (SumTypeDef ("S", [], [{name = "C"; fields = [TInt64]}; {name = "D"; fields = []}]));
  TypeDef (TypeAlias ("Alias", [], TInt64)); FunctionDef generic; FunctionDef (func "constant" ["x", TUnit] TString (StringLiteral source));
  ValueDef (UncheckedValueDef ("value", Int64Literal 1L))] in
 let programs = [Program []; Program [Expression ([], UnitLiteral)]; Program declarations; Program (declarations @ [Expression ([], AST.applyNamed "identity" (NonEmptyList.singleton (Int64Literal 1L)))]);
  Program (declarations @ [Expression ([], Apply (Var "identity", [TString], NonEmptyList.singleton (StringLiteral source)))]);
  Program (declarations @ [Expression ([], RecordLiteral (unresolvedRecordReference "R" [], [unresolvedRecordFieldReference "field", Int64Literal 1L]))]);
  Program (declarations @ [Expression ([], BinOp (Eq, Var "value", Int64Literal 1L))]);
  Program [Expression ([], StringLiteral source); Expression ([], UnitLiteral)];
  Program [FunctionDef (func "broken" ["x", TUnit] TString (Int64Literal 1L)); Expression ([], UnitLiteral)];
  Program [ValueDef (UncheckedValueDef ("first", Int64Literal 1L)); ValueDef (UncheckedValueDef ("second", Var "first")); Expression ([], Var "second")]] in
 let base = T.checkDeclarationProgramWithEnv (Program declarations) in
 let perProgram program = tuple [result checked (T.checkProgramWithEnv program); result checked (T.checkDeclarationProgramWithEnv program);
  result (fun (value, program) -> tuple [typ value; C.observationProgram program]) (T.checkPublicProgram program);
  result typed (InstrumentedResolvedProgram.checkResolvedProgramInternal None false AST.defaultWarningSettings true program);
  result (fun (_, _, base) -> tuple [result checked (T.checkProgramWithBaseEnv base program); result checked (T.checkDeclarationProgramWithBaseEnv base program);
   result checked (T.checkPublicProgramWithBaseEnvAndSettings base true AST.defaultWarningSettings program); result checked (T.checkSyntheticPreambleWithBaseEnvAndSettings base false AST.defaultWarningSettings program)]) base] in
 tuple [result checked base; list perProgram programs]
