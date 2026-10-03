(* Compare complete typed SSA inlining results, including fresh types and CFGs. *)
[@@@warning "-4"]
open Dark_compiler
module A = ANF
module I = InliningCommon
let id index = A.TempId index
let v index = A.Var (id index)
let int value = A.IntLiteral (A.Int64 value)
let fid value = AST.functionId (Int64.of_int value)
let encodeResult values = SemanticJson.union "FSharpResult" "Ok" [`List (List.map RcObservation.ssaFunction values)]
let error message = SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let observe source =
 let parameter index typ = {A.id = id index; typ} in
 let func index name params ret body : A.functionDef = {A.id = fid index; name; typedParams = List.map (fun (index, typ) -> parameter index typ) params; returnType = ret; returnOwnership = A.OwnedReturn; body} in
 let finish bindings result = List.fold_right (fun (index, operation) body -> A.Let (id index, operation, body)) bindings (A.Return result) in
 let tuple = AST.TTuple [AST.TInt64; AST.TInt64] in
 let option = AST.TSum ("Darklang.Stdlib.Option.Option", [AST.TInt64]) in
 let descriptor : A.recordDescriptor = {A.sourceTypeName = "Darklang.Stdlib.Option.Option"; runtimeTypeName = "Darklang.Stdlib.Option.Option"; typeArgs = [AST.TInt64]; fields = ["tag", AST.TInt64; "payload", AST.TInt64]; valueType = option} in
 let branch typ operation = func 200 ("helper" ^ source) [1, AST.TInt64] typ (A.Let (id 2, A.Prim (A.Gte, v 1, int 0L), A.If (v 2, finish [3, operation (int 0L)] (v 3), finish [4, operation (int 1L)] (v 4)))) in
 let cases = [
  func 200 ("helper" ^ source) [1, AST.TInt64] AST.TInt64 (finish [2, A.Prim (A.Add, v 1, int 1L)] (v 2)), [2, AST.TInt64], AST.TInt64;
  branch AST.TInt64 (fun atom -> A.Prim (A.Mul, v 1, atom)), [2, AST.TBool; 3, AST.TInt64; 4, AST.TInt64], AST.TInt64;
  func 200 ("helper" ^ source) [1, AST.TInt64] tuple (finish [2, A.Prim (A.Add, v 1, int 1L); 3, A.TupleAlloc [v 1; v 2]] (v 3)), [2, AST.TInt64; 3, tuple], tuple;
  branch option (fun tag -> A.RecordAlloc (descriptor, [tag; v 1])), [2, AST.TBool; 3, option; 4, option], option;
  func 200 ("helper" ^ source) [1, AST.TInt64; 5, AST.TInt64] AST.TInt64 (A.Let (id 2, A.Prim (A.Gte, v 1, int 4L), A.If (v 2, A.Return (v 5), finish [3, A.Prim (A.Add, v 1, int 1L); 4, A.Call (fid 200, [v 3; v 5])] (v 4)))), [2, AST.TBool; 3, AST.TInt64; 4, AST.TInt64], AST.TInt64;
  func 200 ("helper" ^ source) [] AST.TString (A.Return (A.StringLiteral source)), [], AST.TString] in
 let configs = [I.defaultConfig; {I.defaultConfig with I.maxFunctionSize = 0}; {I.defaultConfig with I.maxInlineDepth = 0}; {I.defaultConfig with I.maxExternalInlineSites = 0}; {I.defaultConfig with I.maxBoundedLoopIterations = 0}; {I.defaultConfig with I.maxProjectedTupleInlineSites = 0}; {I.defaultConfig with I.maxProjectedTupleInlineSize = 0}] in
 let list encode values = `List (List.map encode values) in
 list (fun (helper, types, ret) -> list (fun argument -> list (fun isExternal -> list (fun excluded -> list (fun config ->
  let args = match helper.A.typedParams with [] -> [] | [_] -> [argument] | _ -> [argument; int 7L] in
  let after, bodyTypes, callerReturn = match ret with
   | AST.TTuple _ -> finish [21, A.TupleGet (v 20, 0); 22, A.TypedAtom (v 21, AST.TInt64); 23, A.TupleGet (v 20, 1); 24, A.Prim (A.Add, v 22, v 23)] (v 24), [21, AST.TInt64; 22, AST.TInt64; 23, AST.TInt64; 24, AST.TInt64], AST.TInt64
   | AST.TSum _ -> A.Let (id 21, A.RecordGet (descriptor, v 20, 1), A.Let (id 22, A.Prim (A.Gte, v 21, int 0L), A.If (v 22, A.Return (v 21), finish [23, A.Prim (A.Add, v 21, int 1L)] (v 23)))), [21, AST.TInt64; 22, AST.TBool; 23, AST.TInt64], AST.TInt64
   | AST.TInt64 -> finish [21, A.Prim (A.Add, v 20, int 2L)] (v 21), [21, AST.TInt64], AST.TInt64
   | _ -> A.Return (v 20), [], ret in
  let caller = func 400 "caller" [10, AST.TInt64] callerReturn (A.Let (id 20, A.Call (fid 200, args), after)) in
  let convert func types = let types = List.map (fun (param : A.typedParam) -> param.A.id, param.A.typ) func.A.typedParams @ List.map (fun (index, typ) -> id index, typ) types in SSAANF.convertFunction 100 (A.TypeMap.ofSeq (List.to_seq types)) func in
  match convert helper types, convert caller ((20, ret) :: bodyTypes) with
  | Error message, _ | _, Error message -> error message
  | Ok helperSSA, Ok callerSSA ->
   try let externals, externalSSA, locals, localSSA = if isExternal then [helper], [helperSSA], [caller], [callerSSA] else [], [], [helper; caller], [helperSSA; callerSSA] in
    let excluded = if excluded then SpecializationIdentity.FunctionSet.singleton helper.A.id else SpecializationIdentity.FunctionSet.empty in
    encodeResult (SSAInlining.inlineProgramWithExternalCandidatesAndExclusions config (I.buildExternalCandidateInfoMap config externals) externalSSA excluded locals localSSA)
   with Failure message | Invalid_argument message -> error message) configs) [false; true]) [false; true]) [int (-1L); int 0L; int 3L; v 10]) cases
