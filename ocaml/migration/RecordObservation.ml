open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let observe source =
 let open! AST in
 let registry = Types.indexTypeRegistry StringOrder.Map.empty (StringOrder.Map.of_list ["R", []; "Generic", ["a"]]) (StringOrder.Map.of_list ["R", ["first", TInt64; "second", TString]; "Generic", ["first", TVar "a"; "second", TVar "a"]]) in
 let aliases = StringOrder.Map.of_list ["Alias", ([], TRecord ("R", [])); "GAlias", (["a"], TRecord ("Generic", [TVar "a"]))] in
 let generic : Types.genericFuncRegistry = {Types.functions = StringOrder.Map.empty; requireExplicitTypeArgsForBareCalls = false} in
 let trace = ref [] in
 let callback value _env _registry _lookup _generic _warnings _modules _aliases expected =
  trace := !trace @ [value, expected];
  match value with
  | RuntimeError message -> Error (CheckingDiagnostics.GenericError message)
  | Var "mismatch" -> Error (CheckingDiagnostics.TypeMismatch (TString, TInt64, "callback"))
  | Int64Literal _ -> Ok (TInt64, BoundaryRender ("checked", value))
  | StringLiteral _ -> Ok (TString, BoundaryRender ("checked", value))
  | Var "generic" -> Ok (TVar source, BoundaryRender ("checked", value))
  | _ -> Ok (TUnit, BoundaryRender ("checked", value)) in
 let field name value = unresolvedRecordFieldReference name, value in
 let fields = [[field "first" (Int64Literal 1L); field "second" (StringLiteral source)];
  [field "second" (StringLiteral source); field "first" (Int64Literal 1L)]; []; [field "first" UnitLiteral];
  [field "first" (Int64Literal 1L); field "first" UnitLiteral]; [field "___" UnitLiteral]; [field "" UnitLiteral];
  [field "first" (Int64Literal 1L); field "second" (StringLiteral source); field source UnitLiteral];
  [field "first" (RuntimeError source); field "second" (RuntimeError "second")];
  [field "second" (RuntimeError "second"); field "first" (RuntimeError source)];
  [field "first" (Var "mismatch"); field "second" (StringLiteral source)];
  [field "first" (Var "generic"); field "second" (Var "generic")];
  [field "first" (Int64Literal 1L); field "second" (Int64Literal 2L)]] in
 let references = List.map (fun (name, args) -> unresolvedRecordReference name args) ["", []; "Unknown", []; "R", []; "R", [TUnit]; "Generic", []; "Generic", [TString]; "Generic", [TString; TInt64]; "Alias", []; "GAlias", [TInt64]] in
 let expected = [None; Some TUnit; Some (TRecord ("R", [])); Some (TRecord ("Generic", [TInt64])); Some (TRecord ("Generic", [TString]))] in
 let cases = if source = "" then List.concat_map (fun reference -> List.concat_map (fun fields -> List.map (fun expected -> reference, fields, expected) expected) fields) references
  else List.concat_map (fun fields -> [unresolvedRecordReference "R" [], fields, None; unresolvedRecordReference "Generic" [], fields, Some (TRecord ("Generic", [TInt64]))]) fields in
 let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value] in
 list (fun (reference, fields, expected) ->
  trace := [];
  let result = CheckRecordLiterals.check callback StringOrder.Map.empty registry StringOrder.Map.empty generic AST.defaultWarningSettings StringOrder.Map.empty aliases expected reference fields in
  let encoded = match result with Ok (typ, value) -> SemanticJson.union "FSharpResult" "Ok" [tuple [SemanticAST.semanticType typ; SemanticAST.expr value]] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticDiagnostics.typeError error] in
  tuple [encoded; list (fun (value, expected) -> tuple [SemanticAST.expr value; option SemanticAST.semanticType expected]) !trace]) cases
[@@warning "-4"]
