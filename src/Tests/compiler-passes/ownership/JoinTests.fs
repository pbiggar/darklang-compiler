// JoinTests.fs - Verify lexical join interfaces and branch cleanup.

module RcJoinTests

open MemoryModel
open ANF
open RcTypeFacts
open RcCleanup
open RefCountInsertion

let internal verifyJoin body =
    let ctx : TypeContext = {
        TypeReg = Map.empty; VariantLookup = Map.empty; SumShapeReg = Map.empty
        FuncReg = Map.empty; FuncParams = Map.empty; ClosureFuncs = Map.empty
        TempTypes = Map.ofList [TempId 1, AST.TInt64; TempId 2, AST.TInt64]
        TypePlanning = createRcTypePlanningContext ()
    }
    verifyJoinInterfaces ctx (Program ([], body))

let internal rejectsJoin expected body () =
    match verifyJoin body with
    | Error message when message.Contains(expected: string) -> Ok ()
    | result -> Error $"Expected join-interface error '{expected}', got {result}"

let internal joinParameter : TypedParam = { Id = TempId 1; Type = AST.TInt64 }
let internal joinValue = IntLiteral (Int64 7L)

let internal testJoinCleanupPaths () =
    let childType = AST.TTuple [AST.TInt64]
    let ctx : TypeContext = {
        TypeReg = Map.empty; VariantLookup = Map.empty; SumShapeReg = Map.empty
        FuncReg = Map.ofList [AST.functionIdForName "observe", ("observe", AST.TFunction ([AST.TInt64], AST.TUnit))]
        FuncParams = Map.empty; ClosureFuncs = Map.empty; TempTypes = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }
    let owner = TempId 10
    let local = TempId 11
    let borrowed = TempId 12
    let flag = TempId 13
    let target = { Id = TempId 14; Type = AST.TInt64 }
    let func : Function = {
        Id = AST.functionIdForName "joinCleanup"; Name = "joinCleanup"; ReturnType = childType; ReturnOwnership = OwnedReturn
        TypedParams = [{ Id = borrowed; Type = childType }; { Id = flag; Type = AST.TBool }]
        Body = Let (owner, TupleAlloc [joinValue],
            Join (target,
                Let (TempId 15, Call (AST.functionIdForName "observe", [Var target.Id]),
                    If (Var flag, Return (Var owner), Return (Var borrowed))),
                Let (local, TupleAlloc [joinValue], Jump (target.Id, joinValue))))
    }
    let transformed, _, _ = insertRCInFunction ctx func (VarGen 100)
    let rec paths joins events expr =
        match expr with
        | Return atom -> [List.rev events, atom]
        | Jump (target, _) ->
            match Map.tryFind target joins with
            | Some body -> paths (Map.remove target joins) events body
            | None -> Crash.crash "RC cleanup test: missing join target"
        | Join (parameter, continuation, entry) ->
            paths (Map.add parameter.Id continuation joins) events entry
        | If (_, yes, no) -> paths joins events yes @ paths joins events no
        | Let (_, operation, body) ->
            let event =
                match operation with
                | RefCountDec (Var id, _, _, _) -> Some ($"dec:{id}")
                | RefCountInc (Var id, _, _, _) -> Some ($"inc:{id}")
                | Call (name, _) when name = AST.functionIdForName "observe" -> Some "observe"
                | _ -> None
            paths joins (match event with Some value -> value :: events | None -> events) body
    let actual = paths Map.empty [] transformed.Body
    let expected = [
        [$"dec:{local}"; "observe"], Var owner
        [$"dec:{local}"; "observe"; $"dec:{owner}"; $"inc:{borrowed}"], Var borrowed
    ]
    if actual = expected then Ok () else Error $"Unexpected join cleanup paths: {actual}"
