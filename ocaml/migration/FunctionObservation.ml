open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let[@warning "-4"] observe source =
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
 let functions : AST.functionDef list = [
  {name = source; typeParams = []; params = NonEmptyList.singleton ("x", TInt64); returnType = TInt64; body = Var "x"; recursion = None};
  {name = source; typeParams = []; params = NonEmptyList.singleton ("x", TInt64); returnType = TString; body = Int64Literal 1L; recursion = None};
  {name = source; typeParams = ["a"]; params = NonEmptyList.singleton ("x", TVar "a"); returnType = TVar "a"; body = Var "x"; recursion = None};
  {name = source; typeParams = ["a"]; params = NonEmptyList.singleton ("x", TVar "a"); returnType = TVar "a"; body = StringLiteral source; recursion = None};
  {name = source; typeParams = []; params = NonEmptyList.singleton ("x", TRecord ("AliasInt", [])); returnType = TRecord ("AliasInt", []); body = Var "x"; recursion = Some recursive};
  {name = source; typeParams = []; params = NonEmptyList.singleton ("x", TRecord ("S", [])); returnType = TSum ("S", []); body = Var "x"; recursion = Some (TypedRecursiveBinding {resolved = (match recursive with ResolvedRecursiveBinding value -> value | _ -> assert false); monomorphicType = TUnit})};
  {name = source; typeParams = []; params = NonEmptyList.singleton ("x", TUnit); returnType = TList TInt64; body = ListLiteral []; recursion = None};
  {name = source; typeParams = []; params = NonEmptyList.singleton ("x", TUnit); returnType = TString; body = RuntimeError source; recursion = None}]
 in
 let specs = [ []; [TInt64]; [TString]; [TVar source]; [TInt64; TString]] in
 let encodeResult result = match result with Ok value -> SemanticJson.union "FSharpResult" "Ok" [SemanticAST.observationFunctionDefNode SemanticAST.semanticType value] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticDiagnostics.typeError error] in
 tuple [
  list (fun func -> tuple [list (fun explicit -> encodeResult (CheckFunctions.checkFunctionDefWithSumTypeNames (StringOrder.Map.singleton "f" ["first"; "second"]) names sums func env registry lookup {generic with Types.requireExplicitTypeArgsForBareCalls = explicit} AST.defaultWarningSettings (DarkStdlib.buildModuleRegistry ()) aliases)) [false; true];
   list (fun args -> encodeResult (CheckFunctions.specializeFunctionForTypeCheck func args)) specs]) functions;
  list (fun expr -> list (fun (name, args) -> tuple [SemanticJson.string name; list SemanticAST.semanticType args]) (CheckFunctions.SpecificationSet.elements (CheckFunctions.collectTypeAppSpecs expr))) (CheckingTypesObservation.additionalExpressions source @ [Apply (Var source, [TInt64; TVar "a"], NonEmptyList.singleton (Apply (Var "g", [TString], NonEmptyList.singleton UnitLiteral))); Closure ("f", [Apply (Var "g", [TInt64], NonEmptyList.singleton UnitLiteral)])])]
