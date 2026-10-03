open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let observe source =
 let open! AST in
 let env = StringOrder.Map.of_list ["f", TFunction ([TInt64; TString], TBool); "g", TFunction ([TVar "a"; TVar "a"], TVar "a"); "higher", TVar "a"; "inferred", TInferenceVar ("a", "fixed"); "nonfunction", TInt64; "nullary", TFunction ([], TUnit); "Darklang.Stdlib.List.sort", TFunction ([TList (TVar "a")], TList (TVar "a"))] in
 let generic : Types.genericFuncRegistry = {Types.functions = StringOrder.Map.of_list ["g", ["a"]; "Darklang.Stdlib.List.sort", ["a"]]; requireExplicitTypeArgsForBareCalls = true} in
 let modules = DarkStdlib.buildModuleRegistry () |> StringOrder.Map.add "module.generic" {AST.name = "module.generic"; typeParams = ["a"]; paramTypes = [TVar "a"; TVar "a"]; returnType = TVar "a"}
  |> StringOrder.Map.add "module.concrete" {AST.name = "module.concrete"; typeParams = []; paramTypes = [TInt64; TString]; returnType = TBool} in
 let names = ["f"; "g"; "higher"; "inferred"; "nonfunction"; "nullary"; "missing"; "module.generic"; "module.concrete"; "Builtin.unwrap"; "Builtin.crash"; "Builtin.testRuntimeError"; "Darklang.Stdlib.List.sort"; "__compare"; "__empty_dict"] in
 let argLists = [[UnitLiteral]; [Int64Literal 1L]; [StringLiteral source]; [Int64Literal 1L; StringLiteral source]; [Int64Literal 1L; Int64Literal 2L]; [StringLiteral source; StringLiteral source]; [Int64Literal 1L; StringLiteral source; UnitLiteral]; [Var "generic"]; [Var "error"]; [Var "option"]; [Var "result"]; [Var "list"]] in
 let expectations = [None; Some TUnit; Some TBool; Some TInt64; Some TString; Some (TFunction ([TString], TBool)); Some (TVar source)] in
 let cases = if source = "" then List.concat_map (fun name -> List.concat_map (fun args -> List.map (fun expected -> name, args, expected) expectations) argLists) names
  else List.concat_map (fun name -> List.mapi (fun index args -> name, args, List.nth expectations (index mod List.length expectations)) argLists) names in
 let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value] in
 list (fun (name, args, expected) ->
  let trace = ref [] in
  let callback value _env _registry _lookup _generic _warnings _modules _aliases expected =
   trace := !trace @ [value, expected];
   let typ = match value with UnitLiteral -> TUnit | Int64Literal _ -> TInt64 | StringLiteral _ -> TString
    | Var "option" -> TSum ("Darklang.Stdlib.Option.Option", [TInt64]) | Var "result" -> TSum ("Darklang.Stdlib.Result.Result", [TInt64; TString])
    | Var "list" -> TList TInt64 | Var "generic" -> Option.value expected ~default:(TVar source) | _ -> TUnit in
   match value with Var "error" -> Error (CheckingDiagnostics.TypeMismatch (Option.value expected ~default:TString, TInt64, "callback")) | _ -> Ok (typ, value) in
  let result = CheckCalls.check callback (StringOrder.Map.singleton "f" ["first"; "second"]) StringOrder.Map.empty env StringOrder.Map.empty StringOrder.Map.empty generic AST.defaultWarningSettings modules StringOrder.Map.empty expected name (NonEmptyList.fromList args) in
  let result = match result with Ok (typ, value) -> SemanticJson.union "FSharpResult" "Ok" [tuple [SemanticAST.semanticType typ; SemanticAST.expr value]] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticDiagnostics.typeError error] in
  tuple [result; list (fun (value, expected) -> tuple [SemanticAST.expr value; option SemanticAST.semanticType expected]) !trace]) cases
[@@warning "-4"]
