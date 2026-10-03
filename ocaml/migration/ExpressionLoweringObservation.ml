[@@@warning "-4"]
open Dark_compiler
module C = CheckedAST
module R = TypeRegistries
module P = LoweringPrimitives
module L = LoweringExpressions
module A = ANF
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let attempt encode action = result encode (try Ok (action ()) with Failure message -> Error message | Invalid_argument message -> Error message)
let expression (expr, gen) = tuple [ProductionANF.aNF_aExpr expr; ProductionANF.aNF_varGen gen]
let bindings values = list (fun (id, expr) -> tuple [ProductionANF.aNF_tempId id; ProductionANF.aNF_cExpr expr]) values
let atom (value, prefix, gen) = tuple [ProductionANF.aNF_atom value; bindings prefix; ProductionANF.aNF_varGen gen]
let bound (expr, value, gen) = tuple [ProductionANF.aNF_aExpr expr; ProductionANF.aNF_atom value; ProductionANF.aNF_varGen gen]
let helpers = ["Darklang.Stdlib.Int.__value"; "Darklang.Stdlib.Int.__equals"; "Darklang.Stdlib.Int.bitwiseNot"; "Darklang.Stdlib.Int128.__value"; "Darklang.Stdlib.UInt128.__value"; "Darklang.Stdlib.Int128.__equals"; "Darklang.Stdlib.UInt128.__equals"; "Darklang.Stdlib.Int128.bitwiseNot"; "Darklang.Stdlib.UInt128.bitwiseNot"; "Darklang.Stdlib.String.__normalizeAfterConcat"; "Darklang.Stdlib.List.__headUnsafe_i64"; "Darklang.Stdlib.List.__headUnsafeFloat"; "Darklang.Stdlib.List.__tail_i64"; "Darklang.Stdlib.List.__length_i64"; "Darklang.Stdlib.List.__lengthFloat"; "Darklang.Stdlib.List.__getAtInt64"; "Darklang.Stdlib.List.__getAtFloat"]
let examples = ["1L + 2L";
 "if true then 1L else 2L";
 "let (x, y) = (1L, 2L) in x + y";
 "\"é\" ++ \"😀\"";
 "match 0.0 with | -0.0 -> 1L | 0.0 -> 2L | _ -> 3L";
 "match 1L with | 1L when false -> 2L | x -> x";
 "match [1L] with | [] -> 0L | [x] -> x | _ -> 2L";
 "match [1L, 2L] with | [1L, x] -> x | _ -> 0L";
 "match [1L, 2L] with | x :: tail -> x | _ -> 0L";
 "match [1L, 2L] with | x :: y :: tail -> x + y | _ -> 0L";
 "match [1L, 2L] with | 1L :: [x] -> x | _ -> 0L";
 "match [1.0, 2.0] with | [x, y] -> x + y | _ -> 0.0";
 "match [(1L, 2L)] with | [(1L, x)] -> x | _ -> 0L";
 "match [(1L, 2L)] with | (1L, x) :: tail -> x | _ -> 0L";
 "match [\"é\"] with | [\"é\"] -> 1L | _ -> 0L";
 "match ([\"abc\"], 1L) with | (\"abc\" :: _, x) -> x | _ -> 0L";
 "match [[1L], [2L]] with | [x] :: rest -> x | _ -> 0L";
 "type S = A of String | B\nmatch S.A \"é\" with | A \"é\" -> 1L | A x when false -> 2L | B -> 3L | _ -> 0L";
 "type S = A of Int64 | B\nmatch S.A 1L with | A x -> x | B -> 0L";
 "type S = A of (Int64 * String) | B of Int64\nmatch S.A (1L, \"a\") with | A (1L, x) when true -> x | _ -> \"b\"";
 "type S = A of Int64\nmatch S.A 1L with | A x -> x";
 "type S = A | B\nmatch S.A with | A -> 1L | B -> 2L";
 "type R = { a: Int64; b: String }\nR { b = \"é\"; a = 1L }";
 "type R = { a: Int64; b: String }\nlet r = R { a = 1L; b = \"x\" } in { r with a = 2L }";
 "let f (x: Int64): Int64 = x + 1L\nf 2L";
 "let f (x: List<Int64>): Int64 = match x with | h :: t when h == 1L -> h | _ -> 0L\nf [1L]";
 "let x = match [1L] with | [h] -> h | _ -> 0L in x + 1L";
 "match 1L with | 0L | 1L -> 2L | _ -> 3L";
 "match (1L, 2L) with | (x, _) when x == 1L -> x | _ -> 0L";
 "match [] with | [] -> 1L | _ -> 0L";
 "match [1L] with | [x] when x == 1L -> x | _ -> 0L";
 "match [1L] with | x :: tail when x == 1L -> x | _ -> 0L"]
let observe source =
 let program program =
  let symbols = C.programSymbols program in let tops = C.programTopLevels program in let types = WrittenChecking.typeCheckEnvironment program in
  let registry = M.map (fun (info : Types.recordTypeInfo) -> {R.typeParams = info.Types.typeParams; fields = info.Types.fields}) types.Types.indexedTypeReg in
  let variants = types.Types.variantLookup in let sums = P.sumMetadataFromVariantLookup variants in let typeNames = R.typeNamesFromSymbols symbols in
  let funcs = List.filter_map (function C.FunctionDef func -> Some func | _ -> None) tops in
  let functions = FunctionIdMap.ofList (List.map (fun (func : C.functionDef) -> func.C.id, (func.C.name, AST.TFunction (C.functionParameterTypes func |> NonEmptyList.toList |> List.map snd, C.functionReturnType func))) funcs) in
  let names = C.functionNames symbols in
  let ids = AST.allocateFunctionIds (List.to_seq (List.map fst (FunctionIdMap.toList names))) (List.to_seq (List.filter (fun name -> not (M.mem name (C.functionIds symbols))) helpers)) in
  let names = M.fold (fun name id names -> FunctionIdMap.add id name names) ids names in let ids = R.functionIdsFromNames names in
  let globals = C.programValues program |> M.bindings |> List.mapi (fun index (name, (typ, _)) -> AST.topLevelValueId name, (A.TempId (-100 - index), typ)) |> List.to_seq |> R.BindingMap.of_seq in
  let bodies = List.filter_map (function
   | C.FunctionDef func -> let env = C.functionParameterTypes func |> NonEmptyList.toList |> List.mapi (fun index (id, typ) -> id, (A.TempId (-1000 - index), typ)) |> List.fold_left (fun env (id, value) -> R.BindingMap.add id value env) globals in Some (func.C.body, env)
   | C.ValueDef value -> Some (value.C.body, globals)
   | C.Expression value -> Some (value, globals) | _ -> None) tops in
  list (fun (expr, env) -> list (fun first ->
   let gen = A.VarGen first in let inert = SpecializationIdentity.FunctionSet.empty in let modules = DarkStdlib.buildModuleRegistry () in
   tuple [attempt (result expression) (fun () -> L.toANFCore ids sums typeNames inert expr gen env registry variants functions names modules);
    attempt (result atom) (fun () -> L.toAtomCore ids sums typeNames inert expr gen env registry variants functions names modules);
    attempt (result bound) (fun () -> L.toANFBoundAtomCore ids sums typeNames inert expr gen env registry variants functions names modules)]) (if source = "" then [0; 2147483647] else [0])) bodies in
 let sourceProgram source = result (fun (_, value, _) -> program value) (Result.bind (WrittenParsing.parse Validation.Script source) (fun unit -> WrittenChecking.checkSourceUnitsWithBase None false false [unit])) in
 tuple [sourceProgram source; if source = "" then list sourceProgram examples else `List []]
