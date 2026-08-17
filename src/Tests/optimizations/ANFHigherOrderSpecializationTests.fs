// ANFHigherOrderSpecializationTests.fs - Tests known higher-order call cloning.
//
// The fixture mirrors List.filter's recursive helper shape while keeping an
// independent generic caller, so tests prove both direct-call specialization
// and preservation of the original closure calling convention.

module ANFHigherOrderSpecializationTests

open ANF

type TestResult = Result<unit, string>

let private param id typ = { Id = TempId id; Type = typ }

let private functionByName (name: string) (functions: Function list) : Function option =
    functions |> List.tryFind (fun func -> func.Name = name)

let rec private findCall (target: string) (expr: AExpr) : CExpr option =
    match expr with
    | Return _ -> None
    | Let (_, Call (name, _), _) when name = target -> Some(Call (name, []))
    | Let (_, TailCall (name, _), _) when name = target -> Some(TailCall (name, []))
    | Let (_, _, body) -> findCall target body
    | If (_, thenBranch, elseBranch) ->
        match findCall target thenBranch with
        | Some call -> Some call
        | None -> findCall target elseBranch

let rec private callArgs (target: string) (expr: AExpr) : Atom list option =
    match expr with
    | Return _ -> None
    | Let (_, Call (name, args), _) when name = target -> Some args
    | Let (_, TailCall (name, args), _) when name = target -> Some args
    | Let (_, _, body) -> callArgs target body
    | If (_, thenBranch, elseBranch) ->
        match callArgs target thenBranch with
        | Some args -> Some args
        | None -> callArgs target elseBranch

let rec private containsClosureCall (expr: AExpr) : bool =
    let cexprContains cexpr =
        match cexpr with
        | ClosureCall _ -> true
        | _ -> false
    match expr with
    | Return _ -> false
    | Let (_, cexpr, body) -> cexprContains cexpr || containsClosureCall body
    | If (_, thenBranch, elseBranch) -> containsClosureCall thenBranch || containsClosureCall elseBranch

let rec private containsClosureAlloc (expr: AExpr) : bool =
    let cexprContains cexpr =
        match cexpr with
        | ClosureAlloc _ -> true
        | _ -> false
    match expr with
    | Return _ -> false
    | Let (_, cexpr, body) -> cexprContains cexpr || containsClosureAlloc body
    | If (_, thenBranch, elseBranch) -> containsClosureAlloc thenBranch || containsClosureAlloc elseBranch

let private fixture () : Program =
    let closureType = AST.TTuple [AST.TInt64; AST.TInt64]
    let predicate =
        {
            Name = "predicate"
            TypedParams = [param 0 closureType; param 1 AST.TInt64]
            ReturnType = AST.TBool
            ReturnOwnership = OwnedReturn
            Body =
                Let (
                    TempId 2,
                    TupleGet (Var (TempId 0), 1),
                    Let (
                        TempId 13,
                        Prim (Lt, Var (TempId 1), Var (TempId 2)),
                        Return (Var (TempId 13))
                    )
                )
        }
    let functionType = AST.TFunction ([AST.TInt64], AST.TBool)
    let helper =
        {
            Name = "filter"
            TypedParams = [param 3 AST.TInt64; param 4 functionType; param 5 AST.TInt64]
            ReturnType = AST.TInt64
            ReturnOwnership = OwnedReturn
            Body =
                Let (
                    TempId 6,
                    ClosureCall (Var (TempId 4), [Var (TempId 3)]),
                    If (
                        Var (TempId 6),
                        Let (
                            TempId 7,
                            Prim (Add, Var (TempId 3), IntLiteral (Int64 1L)),
                            Let (
                                TempId 8,
                                TailCall ("filter", [Var (TempId 7); Var (TempId 4); Var (TempId 5)]),
                                Return (Var (TempId 8))
                            )
                        ),
                        Return (Var (TempId 5))
                    )
                )
        }
    let knownCallerBody =
        Let (
            TempId 9,
            ClosureAlloc ("predicate", [IntLiteral (Int64 7L)]),
            Let (
                TempId 10,
                Call ("filter", [IntLiteral (Int64 1L); Var (TempId 9); IntLiteral (Int64 0L)]),
                Return (Var (TempId 10))
            )
        )
    let genericCaller =
        {
            Name = "genericCaller"
            TypedParams = [param 11 functionType]
            ReturnType = AST.TInt64
            ReturnOwnership = OwnedReturn
            Body =
                Let (
                    TempId 12,
                    Call ("filter", [IntLiteral (Int64 1L); Var (TempId 11); IntLiteral (Int64 0L)]),
                    Return (Var (TempId 12))
                )
        }
    Program ([predicate; helper; genericCaller], knownCallerBody)

let testKnownCapturingClosureSpecializesRecursiveHelper () : TestResult =
    let (Program (functions, main)) = ANF_HigherOrderSpecialization.specializeProgram (fixture ())
    let added =
        functions
        |> List.filter (fun func ->
            func.Name <> "predicate" && func.Name <> "filter" && func.Name <> "genericCaller")
    match added with
    | [specializedPredicate; specializedHelper] ->
        let originalHelper =
            functionByName "filter" functions
            |> Option.map (fun func -> func.Body)
            |> Option.defaultValue (Return UnitLiteral)
        if containsClosureCall specializedHelper.Body then
            Error "Expected specialized recursive helper to use direct predicate calls"
        elif not (containsClosureCall originalHelper) then
            Error "Expected original generic helper to retain ClosureCall"
        elif containsClosureAlloc main then
            Error "Expected known call site to pass captures without allocating a closure"
        else
            match callArgs specializedHelper.Name main with
            | Some [IntLiteral (Int64 1L); IntLiteral (Int64 0L); IntLiteral (Int64 7L)] ->
                if List.length specializedPredicate.TypedParams <> 2 then
                    Error $"Expected specialized predicate to have captures and arguments, got {List.length specializedPredicate.TypedParams}"
                elif Option.isNone (findCall specializedPredicate.Name specializedHelper.Body) then
                    Error "Expected specialized helper to call its specialized predicate directly"
                elif Option.isNone (findCall specializedHelper.Name specializedHelper.Body) then
                    Error "Expected specialized helper to recurse into itself directly"
                elif callArgs specializedPredicate.Name specializedHelper.Body
                     <> Some [Var (List.last specializedHelper.TypedParams).Id; Var (TempId 3)] then
                    let actual = callArgs specializedPredicate.Name specializedHelper.Body
                    Error $"Expected specialized predicate call to pass its capture before the value argument; got {actual}"
                else
                    Ok ()
            | Some args ->
                Error $"Expected main direct call to pass value, accumulator, and capture; got {List.length args} args"
            | None ->
                Error "Expected main to call the specialized helper"
    | _ ->
        Error $"Expected one predicate and one helper clone, got {List.length added}"

let tests = [
    ("Known capturing closure specializes a recursive helper", testKnownCapturingClosureSpecializesRecursiveHelper)
]
