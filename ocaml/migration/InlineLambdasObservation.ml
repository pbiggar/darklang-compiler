[@@@warning "-4"]
open Dark_compiler
module C = InstrumentedCheckedAST
module I = InstrumentedInlineLambdas
module W = InstrumentedWrittenChecking
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let observe source =
 let ids = [AST.bindingId 0; AST.bindingId 1; AST.namedBindingId 0 "x"; AST.namedBindingId 1 "x"; AST.namedBindingId 0 source; AST.topLevelValueId source] in
 let lambda = C.Lambda (NonEmptyList.singleton {C.pattern = C.LPVariable (AST.namedBindingId 0 "x"); typ = C.checkedType AST.TInt64}, Some (C.checkedType AST.TInt64), C.Local (AST.namedBindingId 0 "x")) in
 let environments = [C.BindingIdMap.empty; C.BindingIdMap.of_seq (List.to_seq (List.map (fun id -> id, lambda) ids))] in
 let expression expr = tuple [list (fun id -> `Bool (I.varOccursInExpr id expr)) ids; list (fun env -> C.observationExpr (I.inlineLambdas expr env)) environments] in
 let program program =
  let tops = C.programTopLevels program in
  let bodies = List.filter_map (function C.FunctionDef value -> Some value.C.body | C.ValueDef value -> Some value.C.body | C.Expression value -> Some value | C.TypeDef _ -> None) tops in
  let functions = List.filter_map (function C.FunctionDef value -> Some value | _ -> None) tops in
  tuple [C.observationProgram (I.inlineLambdasInProgram program); list expression bodies; list (fun value -> C.observationFunctionDef (I.inlineLambdasInFunc value)) functions] in
 let sourceProgram source = result (fun (_, value, _) -> program value) (Result.bind (WrittenParsing.parse Validation.Script source) (fun unit -> W.checkSourceUnitsWithBase None false false [unit])) in
 let extra = [lambda; C.Let (C.LPVariable (AST.namedBindingId 0 "x"), C.Local (AST.namedBindingId 0 "x"), lambda);
 C.Closure (AST.functionId 9L, [C.TypeApp (AST.functionId 8L, C.checkedTypeArgs [AST.TVar "a"], NonEmptyList.singleton (C.Local (AST.namedBindingId 0 "x")))]);
 C.Match (C.UnitLiteral, NonEmptyList.singleton {C.patterns = NonEmptyList.singleton (C.PVariable (AST.namedBindingId 0 "x")); guard = Some (C.Local (AST.namedBindingId 0 "x")); body = C.Local (AST.namedBindingId 0 "x")})] in
 tuple [list (result program) (C.observationFixtures source); list expression extra; sourceProgram source; if source = "" then list sourceProgram ["let f = fun (x: Int64) -> x
f 1"; "let recur (x: Int64) : Int64 = if x == 0 then x else recur (x - 1)
recur 1"] else `List []]
