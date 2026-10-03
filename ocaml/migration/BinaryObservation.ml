open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let observe source =
 let open! AST in
 let samples = [TInt8; TInt16; TInt32; TInt64; TInt128; TInt; TUInt8; TUInt16; TUInt32; TUInt64; TUInt128; TFloat64; TBool; TString; TChar; TUnit; TNever; TInternalRawPtr; TBlob; TDateTime; TVar source; TInferenceVar (source, "fixed"); TList TInt64; TRecord ("R", []); TSum ("S", []); TTuple [TInt64; TString]; TFunction ([TInt64], TString)] in
 let pairs = if source = "" then List.concat_map (fun left -> List.map (fun right -> left, right) samples) samples else
  [TInt64, TInt64; TInt128, TString; TBool, TBool; TString, TChar; TVar source, TInt64; TInt64, TVar source; TSum ("S", []), TSum ("Other", [])] in
 let ops = [Add; Sub; Mul; Div; Mod; Eq; Neq; Lt; Gt; Lte; Gte; And; Or; Pow; Shl; Shr; BitAnd; BitOr; BitXor; StringConcat] in
 let registry = Types.indexTypeRegistry StringOrder.Map.empty (StringOrder.Map.singleton "R" []) (StringOrder.Map.singleton "R" ["field", TInt64]) in
 let generic : Types.genericFuncRegistry = {Types.functions = StringOrder.Map.empty; requireExplicitTypeArgsForBareCalls = false} in
 let trace = ref [] in
 let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value] in
 let run leftType rightType op expected left right =
  trace := [];
  let callback value _env _registry _lookup _generic _warnings _modules _aliases expected =
   trace := !trace @ [value, expected];
   let typ = match value with Var "left" -> leftType | Var "right" -> rightType | BoolLiteral _ -> TBool | StringLiteral _ -> TString | _ -> TUnit in
   match value with
   | Var "error" when expected = None -> Error (CheckingDiagnostics.GenericError source)
   | RuntimeError message -> Error (CheckingDiagnostics.GenericError message)
   | _ -> let typ = if Unification.containsTVar typ then Option.value expected ~default:typ else typ in Ok (typ, value) in
  let result = CheckBinaryOperations.check callback StringOrder.Map.empty StringOrder.Map.empty registry StringOrder.Map.empty generic AST.defaultWarningSettings StringOrder.Map.empty StringOrder.Map.empty expected op left right in
  let encoded = match result with Ok (typ, value) -> SemanticJson.union "FSharpResult" "Ok" [tuple [SemanticAST.semanticType typ; SemanticAST.expr value]] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticDiagnostics.typeError error] in
  tuple [encoded; list (fun (value, expected) -> tuple [SemanticAST.expr value; option SemanticAST.semanticType expected]) !trace] in
 let ordinary = List.concat_map (fun (left, right) -> List.concat_map (fun op -> List.map (fun expected -> run left right op expected (Var "left") (Var "right")) [None; Some TBool; Some TInt64]) ops) pairs in
 let runtime = AST.applyNamed "Builtin.testRuntimeError" (NonEmptyList.singleton (StringLiteral source)) in
 let special = List.concat_map (fun op -> List.map (fun (left, right) -> run TInt64 (TVar source) op None left right)
  [runtime, Var "right"; Var "left", runtime; BoolLiteral false, runtime; BoolLiteral true, runtime; Var "left", Var "error"]) ops in
 `List (ordinary @ special)
[@@warning "-4"]
