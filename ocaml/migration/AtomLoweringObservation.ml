[@@@warning "-4"]
open Dark_compiler
module C = InstrumentedCheckedAST
module L = InstrumentedAtomLowering
module P = InstrumentedLoweringPrimitives
module R = InstrumentedTypeRegistries
module W = InstrumentedWrittenChecking
module T = InstrumentedTypes
module A = InstrumentedANF
module S = InstrumentedSpecializationIdentity
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let attempt encode action = result encode (try Ok (action ()) with Failure message -> Error message | Invalid_argument message -> Error message)
let bindings values = list (fun (id, expr) -> tuple [SemanticANF.aNF_tempId id; SemanticANF.aNF_cExpr expr]) values
let environment values = `Assoc ["map", list (fun (id, (temp, typ)) -> tuple [C.observationBinding id; tuple [SemanticANF.aNF_tempId temp; SemanticAST.semanticType typ]]) (R.BindingMap.bindings values)]
let encode (atom, prefix, gen) = tuple [SemanticANF.aNF_atom atom; bindings prefix; SemanticANF.aNF_varGen gen]
let observe source =
 let program program =
  let symbols = C.programSymbols program in let tops = C.programTopLevels program in let types = W.typeCheckEnvironment program in
  let registry = M.map (fun (info : T.recordTypeInfo) -> {R.typeParams = info.T.typeParams; fields = info.T.fields}) types.T.indexedTypeReg in
  let variants = types.T.variantLookup in let sums = P.sumMetadataFromVariantLookup variants in let typeNames = R.typeNamesFromSymbols symbols in
  let funcs = List.filter_map (function C.FunctionDef func -> Some func | _ -> None) tops in
  let functions = FunctionIdMap.ofList (List.map (fun (func : C.functionDef) -> func.C.id, (func.C.name, AST.TFunction (C.functionParameterTypes func |> NonEmptyList.toList |> List.map snd, C.functionReturnType func))) funcs) in
  let names = C.functionNames symbols in let extras = ["Darklang.Stdlib.Int.__value"; "Darklang.Stdlib.Int.__equals"; "Darklang.Stdlib.Int.bitwiseNot"; "Darklang.Stdlib.Int128.__value"; "Darklang.Stdlib.UInt128.__value"; "Darklang.Stdlib.Int128.__equals"; "Darklang.Stdlib.UInt128.__equals"; "Darklang.Stdlib.Int128.bitwiseNot"; "Darklang.Stdlib.UInt128.bitwiseNot"; "Darklang.Stdlib.String.__normalizeAfterConcat"] in
  let ids = AST.allocateFunctionIds (List.to_seq (List.map fst (FunctionIdMap.toList names))) (List.to_seq (List.filter (fun name -> not (M.mem name (C.functionIds symbols))) extras)) in
  let names = M.fold (fun name id names -> FunctionIdMap.add id name names) ids names in let ids = R.functionIdsFromNames names in
  let globals = C.programValues program |> M.bindings |> List.mapi (fun index (name, (typ, _)) -> AST.topLevelValueId name, (A.TempId (-100 - index), typ)) |> List.to_seq |> R.BindingMap.of_seq in
  let environments = [globals; List.fold_left (fun env (id, value) -> R.BindingMap.add id value env) globals [AST.bindingId 0, (A.TempId (-2), AST.TInt64); AST.bindingId 1, (A.TempId (-3), AST.TString); AST.namedBindingId 0 "x", (A.TempId (-4), AST.TInt64); AST.namedBindingId 1 "x", (A.TempId (-5), AST.TList AST.TString)]] in
  let bodies = List.filter_map (function C.FunctionDef func -> Some func.C.body | C.ValueDef value -> Some value.C.body | C.Expression value -> Some value | _ -> None) tops in
  let expression expr = list (fun env -> list (fun gen ->
   let requests = ref [] in
   let rec atom sums types inert expr gen env registry variants functions names modules =
    requests := !requests @ [tuple [C.observationExpr expr; SemanticANF.aNF_varGen gen; environment env]];
    L.lowerAtom anf atom bound ids sums types inert expr gen env registry variants functions names modules
   and anf _ _ _ _ _ _ _ _ _ _ _ = Error "observation expression callback"
   and bound _ _ _ _ _ _ _ _ _ _ _ = Error "observation bound-atom callback" in
   let value = attempt (result encode) (fun () -> L.lowerAtom anf atom bound ids sums typeNames S.FunctionSet.empty expr (A.VarGen gen) env registry variants functions names (DarkStdlib.buildModuleRegistry ())) in
   tuple [value; `List !requests]) (if source = "" then [0;2147483647] else [0])) environments in
  let extra = if source <> "" then [] else [C.StringLiteral "e\204\129"; C.CharLiteral "e\204\129"; C.BigIntLiteral (Z.neg (Z.shift_left Z.one 62)); C.BigIntLiteral (Z.shift_left Z.one 62); C.Int128Literal (Z.neg (Z.shift_left Z.one 127)); C.UInt128Literal (Z.pred (Z.shift_left Z.one 128)); C.UnaryOp (AST.Neg, C.Int64Literal Int64.min_int); C.ListLiteral (List.init 32 (fun index -> C.Int64Literal (Int64.of_int index))); C.BinOp (AST.StringConcat, C.StringLiteral "", C.BinOp (AST.StringConcat, C.StringLiteral "a", C.StringLiteral "")); C.If (C.BoolLiteral true, C.TupleLiteral (C.tupleElementsOfList [C.UnitLiteral;C.UnitLiteral]), C.UnitLiteral)] in
  list expression (bodies @ extra) in
 let sourceProgram source = result (fun (_, value, _) -> program value) (Result.bind (WrittenParsing.parse Validation.Script source) (fun unit -> W.checkSourceUnitsWithBase None false false [unit])) in
 tuple [list (result program) (C.observationFixtures source); sourceProgram source;
  (if source = "" then list sourceProgram ["type R = { a: Int64; b: String }\nR { b = \"é\"; a = 1 }"; "type S = A of Int64 | B\nS.A 1"; "let f = fun (x: Int64) -> x\nf 1"; "let (x, y) = (1, 2)\nx + y"] else `List [])]
