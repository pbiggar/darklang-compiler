(* JoinTests.fs - Verify lexical join interfaces and branch cleanup. *)
[@@@warning "-4-42"]
open Dark_compiler
module A = ANF
module F = RcTypeFacts
module M = F.TempMap
let context funcs types : F.typeContext = {F.typeReg = StringOrder.Map.empty; variantLookup = StringOrder.Map.empty; sumShapeReg = StringOrder.Map.empty; funcReg = funcs; funcParams = StringOrder.Map.empty; tempTypes = types; closureFuncs = M.empty; typePlanning = F.createRcTypePlanningContext ()}
let verifyJoin body = RefCountInsertion.verifyJoinInterfaces (context FunctionIdMap.empty (M.of_list [A.TempId 1, AST.TInt64; A.TempId 2, AST.TInt64])) (A.Program ([], body))
let contains text part = let rec at index = index + String.length part <= String.length text && (String.sub text index (String.length part) = part || at (index + 1)) in at 0
let rejectsJoin expected body () = match verifyJoin body with Error message when contains message expected -> Ok () | result -> Error ("Expected join-interface error '" ^ expected ^ "', got " ^ (match result with Ok () -> "Ok ()" | Error error -> HostStructuralFormat.format (StructuralValue.Union ("Error", [StructuralValue.Text error]))))
let joinParameter : A.typedParam = {A.id = A.TempId 1; typ = AST.TInt64}
let joinValue = A.IntLiteral (A.Int64 7L)
let tempText (A.TempId id) = "TempId " ^ string_of_int id
let testJoinCleanupPaths () =
 let child = AST.TTuple [AST.TInt64] in
 let observe = TestIds.functionIdForName "observe" in
 let ctx = context (FunctionIdMap.ofList [observe, ("observe", AST.TFunction ([AST.TInt64], AST.TUnit))]) M.empty in
 let owner = A.TempId 10 and local = A.TempId 11 and borrowed = A.TempId 12 and flag = A.TempId 13 in
 let target : A.typedParam = {A.id = A.TempId 14; typ = AST.TInt64} in
 let definition : A.functionDef = {A.id = TestIds.functionIdForName "joinCleanup"; name = "joinCleanup"; returnType = child; returnOwnership = A.OwnedReturn; typedParams = [{A.id = borrowed; typ = child}; {A.id = flag; typ = AST.TBool}]; body = A.Let (owner, A.TupleAlloc [joinValue], A.Join (target, A.Let (A.TempId 15, A.Call (observe, [A.Var target.A.id]), A.If (A.Var flag, A.Return (A.Var owner), A.Return (A.Var borrowed))), A.Let (local, A.TupleAlloc [joinValue], A.Jump (target.A.id, joinValue))))} in
 let transformed, _, _ = RefCountInsertion.insertRCInFunction ctx definition (A.VarGen 100) in
 let rec paths joins events = function
  | A.Return atom -> [List.rev events, atom]
  | A.Jump (target, _) -> (match M.find_opt target joins with Some body -> paths (M.remove target joins) events body | None -> Crash.crash "RC cleanup test: missing join target")
  | A.Join (parameter, continuation, entry) -> paths (M.add parameter.A.id continuation joins) events entry
  | A.If (_, yes, no) -> paths joins events yes @ paths joins events no
  | A.Let (_, operation, body) -> let event = match operation with A.RefCountDec (A.Var id, _, _, _) -> Some ("dec:" ^ tempText id) | A.RefCountInc (A.Var id, _, _, _) -> Some ("inc:" ^ tempText id) | A.Call (name, _) when name = observe -> Some "observe" | _ -> None in paths joins (match event with Some event -> event :: events | None -> events) body in
 let actual = paths M.empty [] transformed.A.body in
 let expected = [["dec:" ^ tempText local; "observe"], A.Var owner; ["dec:" ^ tempText local; "observe"; "dec:" ^ tempText owner; "inc:" ^ tempText borrowed], A.Var borrowed] in
 if actual = expected then Ok () else Error ("Unexpected join cleanup paths: " ^ HostStructuralFormat.format (StructuralValue.Sequence (List.map (fun (events, atom) -> StructuralValue.Tuple [StructuralValue.Sequence (List.map (fun event -> StructuralValue.Text event) events); ANFTestFormatting.aNF_atom atom]) actual)))
