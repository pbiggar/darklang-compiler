open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let observe source =
 let open! AST in
 let patterns = [PUnit; PWildcard; PVar source; PConstructor ("C", [PVar source; PVar "y"]); PResolvedConstructor ("M.T", "C", 3, [PVar source]); PInt64 1L; PBigInt Z.one; PInt128Literal Z.one; PInt8Literal 1; PInt16Literal 1; PInt32Literal 1l; PUInt8Literal 1; PUInt16Literal 1; PUInt32Literal 1L; PUInt64Literal 1L; PUInt128Literal Z.one; PBool true; PString source; PChar source; PFloat 1.; PTuple [PVar source; PVar "y"]; PList [PVar source]; PListCons ([PVar source], PVar "tail"); POr (NonEmptyList.fromList [PVar source; PVar "other"])] in
 let lookup = StringOrder.Map.of_list ["C", ("S", [], 0, [TInt64]); "S.C", ("S", [], 0, [TInt64]); "D", ("S", [], 1, []); "S.D", ("S", [], 1, []);
  "Ok", ("Outer", [], 0, [TSum ("Inner", [])]); "Outer.Ok", ("Outer", [], 0, [TSum ("Inner", [])]);
  "I1", ("Inner", [], 0, []); "Inner.I1", ("Inner", [], 0, []); "I2", ("Inner", [], 1, []); "Inner.I2", ("Inner", [], 1, [])] in
 let sums = Types.indexSumTypeRegistry lookup and names = StringOrder.Set.of_list ["S"; "Outer"; "Inner"] in
 let generic : Types.genericFuncRegistry = {Types.functions = StringOrder.Map.empty; requireExplicitTypeArgsForBareCalls = false} in
 let scrutinees = [TUnit, UnitLiteral; TInt64, Int64Literal 1L; TInt64, Var "scrutinee"; TInt128, Int128Literal Z.one; TInt, BigIntLiteral Z.one;
  TInt8, Int8Literal 1; TInt16, Int16Literal 1; TInt32, Int32Literal 1l; TUInt8, UInt8Literal 1; TUInt16, UInt16Literal 1; TUInt32, UInt32Literal 1L; TUInt64, UInt64Literal 1L; TUInt128, UInt128Literal Z.one;
  TBool, BoolLiteral false; TBool, Var "scrutinee"; TString, StringLiteral source; TChar, CharLiteral source; TFloat64, FloatLiteral 1.;
  TTuple [TInt64; TString], TupleLiteral [Int64Literal 1L; StringLiteral source]; TTuple [TBool; TBool], Var "scrutinee";
  TList TInt64, ListLiteral []; TList TInt64, ListLiteral [Int64Literal 1L; Int64Literal 1L]; TList TString, ListLiteral [StringLiteral source];
  TSum ("S", []), Constructor (UnresolvedConstructor None, "C", [Int64Literal 1L]); TSum ("S", []), Var "scrutinee";
  TSum ("Outer", []), Var "scrutinee"; TNever, RuntimeError source; TVar source, Var "scrutinee"; TInferenceVar (source, "fixed"), Var "scrutinee"] in
 let case patterns guard body : AST.matchCase = {patterns = NonEmptyList.fromList patterns; guard; body} in
 let run typ scrutinee cases expected =
  let trace = ref [] in
  let callback value env _registry _lookup _generic _warnings _modules _aliases expected =
   let first = !trace = [] in trace := !trace @ [value, env, expected];
   if first then Ok (typ, value) else
   match value with
   | Var "undefined" -> Error (CheckingDiagnostics.UndefinedVariable "undefined")
   | BoolLiteral _ -> (match expected with Some expected when expected <> TBool -> Error (CheckingDiagnostics.TypeMismatch (expected, TBool, "boolean literal")) | _ -> Ok (TBool, value))
   | Var "genericBody" -> Ok (Option.value expected ~default:(TVar source), value)
   | Var name -> Ok (Option.value (StringOrder.Map.find_opt name env) ~default:TUnit, value)
   | UnitLiteral -> Ok (TUnit, value) | Int64Literal _ -> Ok (TInt64, value) | StringLiteral _ -> Ok (TString, value) | RuntimeError _ -> Ok (TNever, value)
   | _ -> Ok (Option.value expected ~default:TUnit, value) in
  let result = CheckMatches.check callback names sums StringOrder.Map.empty StringOrder.Map.empty lookup generic AST.defaultWarningSettings StringOrder.Map.empty StringOrder.Map.empty expected scrutinee cases in
  let encoded = match result with Ok (typ, value) -> SemanticJson.union "FSharpResult" "Ok" [tuple [SemanticAST.semanticType typ; SemanticAST.expr value]] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticDiagnostics.typeError error] in
  let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value] in
  tuple [encoded; list (fun (value, env, expected) -> tuple [SemanticAST.expr value; `Assoc ["map", list (fun (name, typ) -> tuple [SemanticJson.string name; SemanticAST.semanticType typ]) (StringOrder.Map.bindings env)]; option SemanticAST.semanticType expected]) !trace] in
 let configurations pattern = [ [case [pattern] None UnitLiteral];
  [case [pattern] None UnitLiteral; case [PWildcard] None UnitLiteral];
  [case [pattern] (Some (BoolLiteral true)) UnitLiteral; case [PWildcard] None UnitLiteral];
  [case [pattern; PWildcard] None UnitLiteral];
  [case [pattern] (Some (Var "undefined")) UnitLiteral];
  [case [pattern] None (Var "genericBody"); case [PWildcard] None (Int64Literal 1L)];
  [case [pattern] None (Int64Literal 1L); case [PWildcard] None (BoolLiteral true)]] in
 let ordinary = if source = "" then List.concat_map (fun (typ, value) -> List.concat_map (fun pattern -> List.map (fun cases -> run typ value cases None) (configurations pattern)) patterns) scrutinees
  else List.mapi (fun index pattern -> let typ, value = List.nth scrutinees (index mod List.length scrutinees) in run typ value (List.nth (configurations pattern) (index mod 7)) None) patterns in
 let special = [TBool, Var "scrutinee", [case [PBool true] None UnitLiteral; case [PBool false] None UnitLiteral];
  TList TInt64, Var "scrutinee", [case [PList []] None UnitLiteral; case [PListCons ([PWildcard], PWildcard)] None UnitLiteral];
  TTuple [TBool; TBool], Var "scrutinee", [case [PTuple [PBool true; PWildcard]] None UnitLiteral; case [PTuple [PBool false; PBool true]] None UnitLiteral; case [PTuple [PBool false; PBool false]] None UnitLiteral];
  TSum ("Outer", []), Var "scrutinee", [case [PConstructor ("Ok", [PConstructor ("I1", [])])] None UnitLiteral; case [PConstructor ("Ok", [PConstructor ("I2", [])])] None UnitLiteral];
  TSum ("S", []), Var "scrutinee", [case [PConstructor ("C", [PWildcard])] None UnitLiteral; case [PConstructor ("D", [])] None UnitLiteral];
  TUnit, UnitLiteral, []; TUnit, UnitLiteral, [case [PWildcard] None UnitLiteral]] in
 `List (ordinary @ List.concat_map (fun (typ, value, cases) -> List.map (fun expected -> run typ value cases expected) [None; Some TInt64; Some (TVar source)]) special)
[@@warning "-4"]
