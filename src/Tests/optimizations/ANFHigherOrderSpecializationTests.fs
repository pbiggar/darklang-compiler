// ANFHigherOrderSpecializationTests.fs - Tests known higher-order call cloning.
//
// The fixture mirrors List.filter's recursive helper shape while keeping an
// independent generic caller, so tests prove both direct-call specialization
// and preservation of the original closure calling convention.

module ANFHigherOrderSpecializationTests

open ANF

type TestResult = Result<unit, string>

let private fid = AST.functionIdForName

let private param id typ = { Id = TempId id; Type = typ }

let private functionByName (name: string) (functions: Function list) : Function option =
    functions |> List.tryFind (fun func -> func.Name = name)

let rec private findCall (target: string) (expr: AExpr) : CExpr option =
    match expr with
    | Jump _ | Return _ -> None
    | Let (_, Call (name, _), _) when name = (fid target) -> Some(Call (name, []))
    | Let (_, TailCall (name, _), _) when name = (fid target) -> Some(TailCall (name, []))
    | Let (_, _, body) -> findCall target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        match findCall target thenBranch with
        | Some call -> Some call
        | None -> findCall target elseBranch

let rec private callArgs (target: string) (expr: AExpr) : Atom list option =
    match expr with
    | Jump _ | Return _ -> None
    | Let (_, Call (name, args), _) when name = (fid target) -> Some args
    | Let (_, TailCall (name, args), _) when name = (fid target) -> Some args
    | Let (_, _, body) -> callArgs target body
    | Join (_, thenBranch, elseBranch)
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
    | Jump _ | Return _ -> false
    | Let (_, cexpr, body) -> cexprContains cexpr || containsClosureCall body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) -> containsClosureCall thenBranch || containsClosureCall elseBranch

let rec private containsClosureAlloc (expr: AExpr) : bool =
    let cexprContains cexpr =
        match cexpr with
        | ClosureAlloc _ -> true
        | _ -> false
    match expr with
    | Jump _ | Return _ -> false
    | Let (_, cexpr, body) -> cexprContains cexpr || containsClosureAlloc body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) -> containsClosureAlloc thenBranch || containsClosureAlloc elseBranch

let private fixture () : Program =
    let closureType = AST.TTuple [AST.TInt64; AST.TInt64]
    let predicate =
        {
            Id = fid "predicate"
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
            Id = fid "filter"
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
                                TailCall (fid "filter", [Var (TempId 7); Var (TempId 4); Var (TempId 5)]),
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
            ClosureAlloc (fid "predicate", [IntLiteral (Int64 7L)]),
            Let (
                TempId 10,
                Call (fid "filter", [IntLiteral (Int64 1L); Var (TempId 9); IntLiteral (Int64 0L)]),
                Return (Var (TempId 10))
            )
        )
    let genericCaller =
        {
            Id = fid "genericCaller"
            Name = "genericCaller"
            TypedParams = [param 11 functionType]
            ReturnType = AST.TInt64
            ReturnOwnership = OwnedReturn
            Body =
                Let (
                    TempId 12,
                    Call (fid "filter", [IntLiteral (Int64 1L); Var (TempId 11); IntLiteral (Int64 0L)]),
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

let private aliasFixture () : Program =
    let (Program (functions, _)) = fixture ()
    let main =
        Let (
            TempId 20,
            ClosureAlloc (fid "predicate", [IntLiteral (Int64 7L)]),
            Let (
                TempId 21,
                Atom (Var (TempId 20)),
                Let (
                    TempId 22,
                    Call (fid "filter", [IntLiteral (Int64 1L); Var (TempId 21); IntLiteral (Int64 0L)]),
                    Return (Var (TempId 22))
                )
            )
        )
    Program (functions, main)

let testKnownClosureFlowsThroughAlias () : TestResult =
    let (Program (functions, main)) =
        ANF_HigherOrderSpecialization.specializeProgram (aliasFixture ())
    let specializedHelpers =
        functions
        |> List.filter (fun func -> func.Name.StartsWith("filter__known_"))
    match specializedHelpers with
    | [helper] when Option.isSome (findCall helper.Name main) && not (containsClosureAlloc main) -> Ok ()
    | _ -> Error "Expected an aliased known closure to select one allocation-free helper clone"

let private targetFunction name operation =
    {
        Id = fid name
        Name = name
        TypedParams = [param 30 (AST.TTuple [AST.TInt64]); param 31 AST.TInt64]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                TempId 32,
                Prim (operation, Var (TempId 31), IntLiteral (Int64 1L)),
                Return (Var (TempId 32))
            )
    }

let private twoFunctionFixture () : Program =
    let functionType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let helper =
        {
            Id = fid "applyBoth"
            Name = "applyBoth"
            TypedParams = [param 40 functionType; param 41 functionType; param 42 AST.TInt64]
            ReturnType = AST.TInt64
            ReturnOwnership = OwnedReturn
            Body =
                Let (
                    TempId 43,
                    ClosureCall (Var (TempId 40), [Var (TempId 42)]),
                    Let (
                        TempId 44,
                        ClosureCall (Var (TempId 41), [Var (TempId 42)]),
                        Let (
                            TempId 45,
                            Prim (Add, Var (TempId 43), Var (TempId 44)),
                            Return (Var (TempId 45))
                        )
                    )
                )
        }
    let main =
        Let (
            TempId 46,
            ClosureAlloc (fid "increment", []),
            Let (
                TempId 47,
                ClosureAlloc (fid "decrement", []),
                Let (
                    TempId 48,
                    Call (fid "applyBoth", [Var (TempId 46); Var (TempId 47); IntLiteral (Int64 10L)]),
                    Return (Var (TempId 48))
                )
            )
        )
    Program ([targetFunction "increment" Add; targetFunction "decrement" Sub; helper], main)

let testMultipleKnownArgumentsShareOneClone () : TestResult =
    let (Program (functions, main)) =
        ANF_HigherOrderSpecialization.specializeProgram (twoFunctionFixture ())
    let added =
        functions
        |> List.filter (fun func ->
            func.Name <> "increment" && func.Name <> "decrement" && func.Name <> "applyBoth")
    let specializedHelpers =
        added |> List.filter (fun func -> func.Name.StartsWith("applyBoth__known_"))
    match specializedHelpers with
    | [helper]
        when List.length added = 3
             && not (containsClosureCall helper.Body)
             && not (containsClosureAlloc main)
             && Option.isSome (findCall helper.Name main) -> Ok ()
    | _ -> Error "Expected both known functional arguments to share one fully devirtualized helper clone"

let private captureFreeTarget name =
    {
        Id = fid name
        Name = name
        TypedParams = [param 50 (AST.TTuple [AST.TInt64]); param 51 AST.TInt64]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body = Return (Var (TempId 51))
    }

let private applyOneHelper () =
    let functionType = AST.TFunction ([AST.TInt64], AST.TInt64)
    {
        Id = fid "applyOne"
        Name = "applyOne"
        TypedParams = [param 52 functionType; param 53 AST.TInt64]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                TempId 54,
                ClosureCall (Var (TempId 52), [Var (TempId 53)]),
                Return (Var (TempId 54))
            )
    }

let testKnownClosureFlowsThroughBranchValue () : TestResult =
    let main =
        Let (
            TempId 55,
            ClosureAlloc (fid "identityClosure", []),
            Let (
                TempId 56,
                Atom (Var (TempId 55)),
                Let (
                    TempId 57,
                    IfValue (BoolLiteral true, Var (TempId 55), Var (TempId 56)),
                    Let (
                        TempId 58,
                        Call (fid "applyOne", [Var (TempId 57); IntLiteral (Int64 42L)]),
                        Return (Var (TempId 58))
                    )
                )
            )
        )
    let (Program (functions, rewrittenMain)) =
        Program ([captureFreeTarget "identityClosure"; applyOneHelper ()], main)
        |> ANF_HigherOrderSpecialization.specializeProgram
    let helper = functions |> List.tryFind (fun func -> func.Name.StartsWith("applyOne__known_"))
    match helper with
    | Some specialized when Option.isSome (findCall specialized.Name rewrittenMain) -> Ok ()
    | _ -> Error "Expected a known closure selected through a branch value to specialize"

let testReturnedKnownClosureSpecializes () : TestResult =
    let factory =
        {
            Id = fid "makeIdentity"
            Name = "makeIdentity"
            TypedParams = []
            ReturnType = AST.TFunction ([AST.TInt64], AST.TInt64)
            ReturnOwnership = OwnedReturn
            Body =
                Let (
                    TempId 60,
                    ClosureAlloc (fid "identityClosure", []),
                    Return (Var (TempId 60))
                )
        }
    let main =
        Let (
            TempId 61,
            Call (fid "makeIdentity", []),
            Let (
                TempId 62,
                Call (fid "applyOne", [Var (TempId 61); IntLiteral (Int64 42L)]),
                Return (Var (TempId 62))
            )
        )
    let (Program (functions, rewrittenMain)) =
        Program ([captureFreeTarget "identityClosure"; applyOneHelper (); factory], main)
        |> ANF_HigherOrderSpecialization.specializeProgram
    let helper = functions |> List.tryFind (fun func -> func.Name.StartsWith("applyOne__known_"))
    match helper with
    | Some specialized when Option.isSome (findCall specialized.Name rewrittenMain) -> Ok ()
    | _ -> Error "Expected a statically known closure returned by a local function to specialize"

let private plainTarget =
    {
        Id = fid "plainIdentity"
        Name = "plainIdentity"
        TypedParams = [param 70 AST.TInt64]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body = Return (Var (TempId 70))
    }

let testStaticFunctionReferenceNeedsNoClosure () : TestResult =
    let main =
        Let (
            TempId 71,
            Call (fid "applyOne", [FuncRef (fid "plainIdentity"); IntLiteral (Int64 42L)]),
            Return (Var (TempId 71))
        )
    let (Program (functions, rewrittenMain)) =
        Program ([plainTarget; applyOneHelper ()], main)
        |> ANF_HigherOrderSpecialization.specializeProgram
    let helper = functions |> List.tryFind (fun func -> func.Name.StartsWith("applyOne__known_"))
    match helper with
    | Some specialized
        when not (containsClosureCall specialized.Body)
             && Option.isSome (findCall "plainIdentity" specialized.Body)
             && Option.isSome (findCall specialized.Name rewrittenMain) -> Ok ()
    | _ -> Error "Expected a static function reference to devirtualize without a closure target clone"

let testExternalDefinitionsCanSpecializeLocalCall () : TestResult =
    let main =
        Let (
            TempId 80,
            ClosureAlloc (fid "externalIdentity", []),
            Let (
                TempId 81,
                Call (fid "externalApply", [Var (TempId 80); IntLiteral (Int64 42L)]),
                Return (Var (TempId 81))
            )
        )
    let externalTarget = { captureFreeTarget "externalIdentity" with TypedParams = [param 82 (AST.TTuple [AST.TInt64]); param 83 AST.TInt64]; Body = Return (Var (TempId 83)) }
    let externalHelper =
        { applyOneHelper () with
            Id = fid "externalApply"
            Name = "externalApply" }
    let (Program (functions, rewrittenMain)) =
        ANF_HigherOrderSpecialization.specializeProgramWithExternalFunctions
            [externalTarget; externalHelper]
            (Program ([], main))
    let helper = functions |> List.tryFind (fun func -> func.Name.StartsWith("externalApply__known_"))
    match helper with
    | Some specialized when Option.isSome (findCall specialized.Name rewrittenMain) -> Ok ()
    | _ -> Error "Expected external ANF definitions to support cross-unit specialization"

let private partialTarget =
    {
        Id = fid "partialTarget"
        Name = "partialTarget"
        TypedParams = [param 90 (AST.TTuple [AST.TInt64; AST.TInt64]); param 91 AST.TInt64]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                TempId 92,
                TupleGet (Var (TempId 90), 1),
                Let (
                    TempId 93,
                    Prim (Add, Var (TempId 92), Var (TempId 91)),
                    Return (Var (TempId 93))
                )
            )
    }

let testReturnedPartialApplicationSpecializes () : TestResult =
    let factory =
        {
            Id = fid "bindPartial"
            Name = "bindPartial"
            TypedParams = [param 94 AST.TInt64]
            ReturnType = AST.TFunction ([AST.TInt64], AST.TInt64)
            ReturnOwnership = OwnedReturn
            Body =
                Let (
                    TempId 95,
                    ClosureAlloc (fid "partialTarget", [Var (TempId 94)]),
                    Return (Var (TempId 95))
                )
        }
    let main =
        Let (
            TempId 96,
            Call (fid "bindPartial", [IntLiteral (Int64 10L)]),
            Let (
                TempId 97,
                Call (fid "applyOne", [Var (TempId 96); IntLiteral (Int64 32L)]),
                Return (Var (TempId 97))
            )
        )
    let (Program (functions, rewrittenMain)) =
        Program ([partialTarget; factory; applyOneHelper ()], main)
        |> ANF_HigherOrderSpecialization.specializeProgram
    match functions |> List.tryFind (fun func -> func.Name.StartsWith("applyOne__known_")) with
    | Some helper
        when callArgs helper.Name rewrittenMain
             = Some [IntLiteral (Int64 32L); IntLiteral (Int64 10L)] -> Ok ()
    | _ -> Error "Expected a returned partial application to pass its bound argument directly"

let testReturnedClosureWithLocalCaptureStaysGeneric () : TestResult =
    let factory =
        {
            Id = fid "computeThenBind"
            Name = "computeThenBind"
            TypedParams = [param 100 AST.TInt64]
            ReturnType = AST.TFunction ([AST.TInt64], AST.TInt64)
            ReturnOwnership = OwnedReturn
            Body =
                Let (
                    TempId 101,
                    Prim (Add, Var (TempId 100), IntLiteral (Int64 1L)),
                    Let (
                        TempId 102,
                        ClosureAlloc (fid "partialTarget", [Var (TempId 101)]),
                        Return (Var (TempId 102))
                    )
                )
        }
    let main =
        Let (
            TempId 103,
            Call (fid "computeThenBind", [IntLiteral (Int64 9L)]),
            Let (
                TempId 104,
                Call (fid "applyOne", [Var (TempId 103); IntLiteral (Int64 32L)]),
                Return (Var (TempId 104))
            )
        )
    let (Program (functions, _)) =
        Program ([partialTarget; factory; applyOneHelper ()], main)
        |> ANF_HigherOrderSpecialization.specializeProgram
    if functions |> List.exists (fun func -> func.Name.StartsWith("applyOne__known_")) then
        Error "Expected a returned closure with a callee-local capture to stay generic"
    else
        Ok ()

let tests = [
    ("Known capturing closure specializes a recursive helper", testKnownCapturingClosureSpecializesRecursiveHelper)
    ("Known closure flows through alias", testKnownClosureFlowsThroughAlias)
    ("Multiple known functional arguments share one clone", testMultipleKnownArgumentsShareOneClone)
    ("Known closure flows through branch value", testKnownClosureFlowsThroughBranchValue)
    ("Returned known closure specializes", testReturnedKnownClosureSpecializes)
    ("Static function reference needs no closure", testStaticFunctionReferenceNeedsNoClosure)
    ("External definitions specialize local call", testExternalDefinitionsCanSpecializeLocalCall)
    ("Returned partial application specializes", testReturnedPartialApplicationSpecializes)
    ("Returned closure with local capture stays generic", testReturnedClosureWithLocalCaptureStaysGeneric)
]
