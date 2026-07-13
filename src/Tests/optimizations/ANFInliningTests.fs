// ANFInliningTests.fs - Unit tests for ANF inlining behavior.
//
// Covers literal argument binding, local inlining eligibility, and external
// stdlib candidate filtering/budgeting.

module ANFInliningTests

open ANF

type TestResult = Result<unit, string>

let private intAtom (value: int64) : Atom =
    IntLiteral (Int64 value)

let rec private containsCall (target: string) (expr: AExpr) : bool =
    match expr with
    | Return _ -> false
    | Let (_, Call (name, _), body) ->
        name = target || containsCall target body
    | Let (_, _, body) ->
        containsCall target body
    | If (_, thenBranch, elseBranch) ->
        containsCall target thenBranch || containsCall target elseBranch

let rec private countCalls (target: string) (expr: AExpr) : int =
    match expr with
    | Return _ -> 0
    | Let (_, Call (name, _), body) ->
        (if name = target then 1 else 0) + countCalls target body
    | Let (_, _, body) ->
        countCalls target body
    | If (_, thenBranch, elseBranch) ->
        countCalls target thenBranch + countCalls target elseBranch

let rec private hasLiteralLet (expr: AExpr) : bool =
    match expr with
    | Return _ -> false
    | Let (_, Atom (IntLiteral _), _) -> true
    | Let (_, _, body) -> hasLiteralLet body
    | If (_, thenBranch, elseBranch) ->
        hasLiteralLet thenBranch || hasLiteralLet elseBranch

let testInliningWithLiteralArgumentsRemovesCall () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let addBody =
        Let (
            TempId 1,
            Prim (Add, Var param.Id, intAtom 1L),
            Return (Var (TempId 1))
        )
    let addOne =
        { Name = "addOne"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = addBody }
    let main =
        Let (
            TempId 2,
            Call ("addOne", [intAtom 41L]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([addOne], main))
    if containsCall "addOne" inlinedMain then
        Error "Expected literal-argument call to be inlined, but Call remained in main expression"
    else
        Ok ()

let testInliningWithLiteralArgumentsBindsTemp () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let identity =
        { Name = "id"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (Var param.Id) }
    let main =
        Let (
            TempId 1,
            Call ("id", [intAtom 7L]),
            Return (Var (TempId 1))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([identity], main))
    if hasLiteralLet inlinedMain then
        Ok ()
    else
        Error "Expected inlined literal argument to be bound to a fresh TempId"

let testInliningUnderscoreFunctionName () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let addBody =
        Let (
            TempId 1,
            Prim (Add, Var param.Id, intAtom 1L),
            Return (Var (TempId 1))
        )
    let addOne =
        { Name = "_addOne"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = addBody }
    let main =
        Let (
            TempId 2,
            Atom (intAtom 41L),
            Let (
                TempId 3,
                Call ("_addOne", [Var (TempId 2)]),
                Return (Var (TempId 3))
            )
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([addOne], main))
    if containsCall "_addOne" inlinedMain then
        Error "Expected underscore-named function to be inlined, but Call remained in main expression"
    else
        Ok ()

let testExternalInlineCandidateRemovesShiftCall () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let stdlibShiftLeft =
        { Name = "Stdlib.Int64.shiftLeft"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Prim (Shl, Var param.Id, intAtom 1L),
                Return (Var (TempId 1))
            ) }
    let main =
        Let (
            TempId 2,
            Call ("Stdlib.Int64.shiftLeft", [intAtom 41L]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            [stdlibShiftLeft]
            (Program ([], main))
    if containsCall "Stdlib.Int64.shiftLeft" inlinedMain then
        Error "Expected external shift wrapper to be inlined, but Call remained in main expression"
    else
        Ok ()

let testExternalInlineCandidateRemovesFloatToIntCall () : TestResult =
    let param = { Id = TempId 0; Type = AST.TFloat64 }
    let stdlibFloatToInt =
        { Name = "Stdlib.Float.toInt"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                FloatToInt64 (Var param.Id),
                Return (Var (TempId 1))
            ) }
    let main =
        Let (
            TempId 2,
            Call ("Stdlib.Float.toInt", [FloatLiteral 41.0]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            [stdlibFloatToInt]
            (Program ([], main))
    if containsCall "Stdlib.Float.toInt" inlinedMain then
        Error "Expected external float-to-int wrapper to be inlined, but Call remained in main expression"
    else
        Ok ()

let testExternalInlineCandidateRejectsRawAllocBody () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let stdlibAllocate =
        { Name = "Stdlib.Test.allocate"
          TypedParams = [param]
          ReturnType = AST.TRawPtr
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                RawAlloc (Var param.Id),
                Return (Var (TempId 1))
            ) }
    let main =
        Let (
            TempId 2,
            Call ("Stdlib.Test.allocate", [intAtom 41L]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            [stdlibAllocate]
            (Program ([], main))
    if containsCall "Stdlib.Test.allocate" inlinedMain then
        Ok ()
    else
        Error "Expected external raw allocation candidate to remain a call"

let testExternalInlineCandidateRejectsControlFlowBody () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let stdlibAbs =
        { Name = "Stdlib.Int64.abs"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            If (
                Var param.Id,
                Return (Var param.Id),
                Let (
                    TempId 1,
                    Prim (Sub, intAtom 0L, Var param.Id),
                    Return (Var (TempId 1))
                )
            ) }
    let main =
        Let (
            TempId 2,
            Call ("Stdlib.Int64.abs", [intAtom 41L]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            [stdlibAbs]
            (Program ([], main))
    if containsCall "Stdlib.Int64.abs" inlinedMain then
        Ok ()
    else
        Error "Expected external control-flow stdlib candidate to remain a call"

let testExternalInliningHonorsCallerBudget () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let stdlibShiftLeft =
        { Name = "Stdlib.Int64.shiftLeft"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Prim (Shl, Var param.Id, intAtom 1L),
                Return (Var (TempId 1))
            ) }
    let rec calls remaining nextTid body =
        if remaining = 0 then
            body
        else
            calls
                (remaining - 1)
                (nextTid + 1)
                (Let (TempId nextTid, Call ("Stdlib.Int64.shiftLeft", [intAtom 1L]), body))
    let main = calls 9 2 (Return (Var (TempId 10)))
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            [stdlibShiftLeft]
            (Program ([], main))
    let remainingCalls = countCalls "Stdlib.Int64.shiftLeft" inlinedMain
    if remainingCalls = 9 then
        Ok ()
    else
        Error $"Expected 9 external calls to remain over caller budget, but found {remainingCalls}"

let testBorrowedSelfCallBlocksInlining () : TestResult =
    let param = { Id = TempId 0; Type = AST.TString }
    let borrowedSelf =
        { Name = "borrowSelf"
          TypedParams = [param]
          ReturnType = AST.TString
          ReturnOwnership = BorrowedReturn
          Body =
            Let (
                TempId 1,
                BorrowedCall ("borrowSelf", [Var param.Id]),
                Return (Var (TempId 1))
            ) }
    let main =
        Let (
            TempId 2,
            Call ("borrowSelf", [StringLiteral "x"]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([borrowedSelf], main))
    if containsCall "borrowSelf" inlinedMain then
        Ok ()
    else
        Error "Expected function with borrowed self-call to remain recursive and not be inlined"

let tests = [
    ("Inlining literal args removes call", testInliningWithLiteralArgumentsRemovesCall)
    ("Inlining literal args binds literal TempId", testInliningWithLiteralArgumentsBindsTemp)
    ("Inlining underscore-named functions", testInliningUnderscoreFunctionName)
    ("Inlining external shift candidate", testExternalInlineCandidateRemovesShiftCall)
    ("Inlining external float-to-int candidate", testExternalInlineCandidateRemovesFloatToIntCall)
    ("External raw allocation candidates are not inlined", testExternalInlineCandidateRejectsRawAllocBody)
    ("External control-flow candidates are not inlined", testExternalInlineCandidateRejectsControlFlowBody)
    ("External inlining honors caller budget", testExternalInliningHonorsCallerBudget)
    ("Borrowed self-call blocks inlining", testBorrowedSelfCallBlocksInlining)
]
