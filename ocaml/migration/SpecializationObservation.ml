[@@@warning "-4"]
open Dark_compiler
module A = ANF
module S = SSAANF
module D = SSADirectCallSpecialization
module H = SSAHigherOrderSpecialization
module F = DirectCallFacts
module M = F.TempMap
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let attempt encode action = result encode (try Ok (action ()) with Failure message -> Error message | Invalid_argument message -> Error message)
let fid index = AST.functionId (Int64.of_int index)
let id index = A.TempId index
let v index = A.Var (id index)
let int value = A.IntLiteral (A.Int64 value)
let functionId value = let value = AST.functionIdValue value in SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind", `String "uint64"; "value", `String (if value < 0L then Z.to_string (Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)) else Int64.to_string value)]]
let origins values = SemanticJson.union "FunctionIdMap" "FunctionIdMap" [`Assoc ["map", list (fun (id, original) -> tuple [`Assoc ["kind", `String "uint64"; "value", `String (Int64.to_string (AST.functionIdValue id))]; functionId original]) (FunctionIdMap.toList values)]]
let direct (value : D.specialization) = SemanticJson.record "Specialization" ["Functions", list RcObservation.ssaFunction value.D.functions; "CloneOrigins", origins value.D.cloneOrigins]
let higher (value : H.specialization) = SemanticJson.record "Specialization" ["Functions", list RcObservation.ssaFunction value.H.functions; "CloneOrigins", origins value.H.cloneOrigins]
let fn index name params ret operations terminator extraTypes : S.functionDef =
 let params = List.map (fun (index, typ) -> {A.id = id index; typ}) params in
 let label = S.Label 0 in
 {S.id = fid index; name; typedParams = params; returnType = ret; returnOwnership = A.OwnedReturn; entry = label; blocks = S.LabelMap.singleton label {S.label; parameters = []; operations; terminator}; freshValueTypes = List.fold_left (fun types (param : A.typedParam) -> M.add param.A.id param.A.typ types) (M.of_list (List.map (fun (index, typ) -> id index, typ) extraTypes)) params}
let observe source =
 let literalCases = [AST.TInt64, [int 1L; int 2L; int 1L]; AST.TUInt64, [A.IntLiteral (A.UInt64 Int64.min_int); A.IntLiteral (A.UInt64 (-1L)); A.IntLiteral (A.UInt64 0L)]; AST.TFloat64, [A.FloatLiteral 0.; A.FloatLiteral (-0.); A.FloatLiteral (Int64.float_of_bits 0x7ff8000000000001L)]; AST.TString, [A.StringLiteral source; A.StringLiteral "😀"; A.StringLiteral "é"]; AST.TInt64, [int 1L; int 1L]] in
 let directCases = List.map (fun (typ, args) ->
  let helper = fn 200 ("helper" ^ source) [1, typ] typ [] (S.Return (v 1)) [] in
  let caller = fn 400 "caller" [] typ (List.mapi (fun index arg -> id (20 + index), A.Call (fid 200, [arg])) args) (S.Return (v (19 + List.length args))) (List.mapi (fun index _ -> 20 + index, typ) args) in
  list (fun indirect ->
   let operations = (S.LabelMap.find (S.Label 0) caller.S.blocks).S.operations in
   let caller = if indirect then {caller with S.blocks = S.LabelMap.singleton (S.Label 0) {(S.LabelMap.find (S.Label 0) caller.S.blocks) with S.operations = (id 90, A.Atom (A.FuncRef (fid 200))) :: operations}} else caller in
   attempt direct (fun () -> D.specializeProgramWithFunctionNames FunctionIdMap.empty [helper; caller])) [false; true]) literalCases in
 let tupleType = AST.TTuple [AST.TInt64; AST.TBool] in
 let helper = fn 210 "tupleHelper" [1, tupleType] AST.TInt64 [id 2, A.TupleGet (v 1, 0)] (S.Return (v 2)) [2, AST.TInt64] in
 let caller = fn 410 "tupleCaller" [] AST.TInt64 [id 10, A.TupleAlloc [int 1L; A.BoolLiteral false]; id 11, A.TupleAlloc [int 2L; A.BoolLiteral true]; id 20, A.Call (fid 210, [v 10]); id 21, A.Call (fid 210, [v 11])] (S.Return (v 21)) [10, tupleType; 11, tupleType; 20, AST.TInt64; 21, AST.TInt64] in
 let tupleClones = attempt direct (fun () -> D.specializeProgramWithFunctionNames FunctionIdMap.empty [helper; caller]) in
 let closureType = AST.TTuple [AST.TInt64; AST.TInt64] in let callbackType = AST.TFunction ([AST.TInt64], AST.TInt64) in
 let target = fn 500 ("target" ^ source) [0, closureType; 1, AST.TInt64] AST.TInt64 [id 2, A.TupleGet (v 0, 1); id 3, A.Prim (A.Add, v 2, v 1)] (S.Return (v 3)) [2, AST.TInt64; 3, AST.TInt64] in
 let static = fn 501 ("static" ^ source) [1, AST.TInt64] AST.TInt64 [id 2, A.Prim (A.Add, v 1, int 1L)] (S.Return (v 2)) [2, AST.TInt64] in
 let helper = fn 502 ("apply" ^ source) [10, callbackType; 11, AST.TInt64] AST.TInt64 [id 20, A.ClosureCall (v 10, [v 11])] (S.Return (v 20)) [20, AST.TInt64] in
 let factory = fn 504 "factory" [1, AST.TInt64] callbackType [id 2, A.ClosureAlloc (fid 500, [v 1])] (S.Return (v 2)) [2, callbackType] in
 let caller kind =
  let operations = match kind with
   | 0 -> [id 13, A.ClosureAlloc (fid 500, [v 12]); id 20, A.Call (fid 502, [v 13; int 3L])]
   | 1 -> [id 20, A.Call (fid 502, [A.FuncRef (fid 501); int 3L])]
   | _ -> [id 13, A.Call (fid 504, [v 12]); id 20, A.BorrowedCall (fid 502, [v 13; int 3L])] in
  fn 503 "caller" [12, AST.TInt64] AST.TInt64 operations (S.Return (v 20)) [13, callbackType; 20, AST.TInt64] in
 let higherCases = list (fun kind -> list (fun collision ->
  let reserved = if collision then StringOrder.Map.singleton ("apply" ^ source ^ "__known_target" ^ source ^ "_0") (fid 900) else StringOrder.Map.empty in
  attempt higher (fun () -> H.specializeProgramWithExternalFunctionsAndNames reserved 1000L [] [target; static; helper; factory; caller kind])) [false; true]) [0; 1; 2] in
 tuple [`List directCases; tupleClones; higherCases]
