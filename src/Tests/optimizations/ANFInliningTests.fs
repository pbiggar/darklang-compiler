// ANFInliningTests.fs - Unit tests for ANF inlining behavior.
//
// Covers literal argument binding, local inlining eligibility, and external
// stdlib candidate filtering/budgeting.

module ANFInliningTests

open MemoryModel

open ANF

type TestResult = Result<unit, string>

let private intAtom (value: int64) : Atom =
    IntLiteral (Int64 value)

let private externalCandidates
    (functions: Function list)
    : Map<AST.FunctionId, ANF_Inlining.FunctionInfo> =
    ANF_Inlining.buildExternalCandidateInfoMap ANF_Inlining.defaultConfig functions

let rec private containsCall (target: string) (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ -> false
    | Let (_, Call (name, _), body) ->
        name = AST.functionIdForName target || containsCall target body
    | Let (_, _, body) ->
        containsCall target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        containsCall target thenBranch || containsCall target elseBranch

let rec private countCalls (target: string) (expr: AExpr) : int =
    match expr with
    | Jump _ | Return _ -> 0
    | Let (_, Call (name, _), body) ->
        (if name = AST.functionIdForName target then 1 else 0) + countCalls target body
    | Let (_, _, body) ->
        countCalls target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        countCalls target thenBranch + countCalls target elseBranch

let rec private countPrimOps (target: BinOp) (expr: AExpr) : int =
    match expr with
    | Jump _ | Return _ -> 0
    | Let (_, Prim (op, _, _), body) ->
        (if op = target then 1 else 0) + countPrimOps target body
    | Let (_, _, body) -> countPrimOps target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        countPrimOps target thenBranch + countPrimOps target elseBranch

let rec private hasLiteralLet (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ -> false
    | Let (_, Atom (IntLiteral _), _) -> true
    | Let (_, _, body) -> hasLiteralLet body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        hasLiteralLet thenBranch || hasLiteralLet elseBranch

let testJoinInliningFreshensTargets () : TestResult =
    let parameter = { Id = TempId 1; Type = AST.TInt64 }
    let body = Join (parameter, Return (Var parameter.Id), Jump (parameter.Id, intAtom 7L))
    let first, next = ANF_Inlining.renameExpr Map.empty (VarGen 100) body
    let second, _ = ANF_Inlining.renameExpr Map.empty next body
    match first, second with
    | Join (a, Return (Var av), Jump (at, _)), Join (b, Return (Var bv), Jump (bt, _))
        when a.Id = av && a.Id = at && b.Id = bv && b.Id = bt && a.Id <> b.Id && a.Id <> parameter.Id -> Ok ()
    | _ -> Error $"Inlining did not consistently freshen join targets: {first}, {second}"

let testInliningWithLiteralArgumentsRemovesCall () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let addBody =
        Let (
            TempId 1,
            Prim (Add, Var param.Id, intAtom 1L),
            Return (Var (TempId 1))
        )
    let addOne =
        { Id = AST.functionIdForName "addOne"
          Name = "addOne"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = addBody }
    let main =
        Let (
            TempId 2,
            Call (AST.functionIdForName "addOne", [intAtom 41L]),
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
        { Id = AST.functionIdForName "id"
          Name = "id"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (Var param.Id) }
    let main =
        Let (
            TempId 1,
            Call (AST.functionIdForName "id", [intAtom 7L]),
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
        { Id = AST.functionIdForName "_addOne"
          Name = "_addOne"
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
                Call (AST.functionIdForName "_addOne", [Var (TempId 2)]),
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
        { Id = AST.functionIdForName "Darklang.Stdlib.Int64.shiftLeft"
          Name = "Darklang.Stdlib.Int64.shiftLeft"
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
            Call (AST.functionIdForName "Darklang.Stdlib.Int64.shiftLeft", [intAtom 41L]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            (externalCandidates [stdlibShiftLeft])
            (Program ([], main))
    if containsCall "Darklang.Stdlib.Int64.shiftLeft" inlinedMain then
        Error "Expected external shift wrapper to be inlined, but Call remained in main expression"
    else
        Ok ()

let testExcludedLocalFunctionRemainsCall () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let lateExternalSpecialization =
        { Id = AST.functionIdForName "Darklang.Stdlib.List.pushBack__String"
          Name = "Darklang.Stdlib.List.pushBack__String"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Prim (Add, Var param.Id, intAtom 1L),
                Return (Var (TempId 1))
            ) }
    let main =
        Let (
            TempId 2,
            Call (lateExternalSpecialization.Id, [intAtom 41L]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidatesAndExclusions
            ANF_Inlining.defaultConfig
            Map.empty
            (Set.singleton lateExternalSpecialization.Id)
            (Program ([lateExternalSpecialization], main))
    if containsCall lateExternalSpecialization.Name inlinedMain then
        Ok ()
    else
        Error "Expected excluded late external specialization to remain a call"

let testExternalInlineCandidateRemovesFloatConversionCall () : TestResult =
    let param = { Id = TempId 0; Type = AST.TFloat64 }
    let stdlibFloatConversion =
        { Id = AST.functionIdForName "Darklang.Stdlib.Float.__toInt64ForInliningTest"
          Name = "Darklang.Stdlib.Float.__toInt64ForInliningTest"
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
            Call (stdlibFloatConversion.Id, [FloatLiteral 41.0]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            (externalCandidates [stdlibFloatConversion])
            (Program ([], main))
    if containsCall stdlibFloatConversion.Name inlinedMain then
        Error "Expected external float-to-int wrapper to be inlined, but Call remained in main expression"
    else
        Ok ()

let testExternalInlineCandidateRejectsRawAllocBody () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let stdlibAllocate =
        { Id = AST.functionIdForName "Darklang.Stdlib.Test.allocate"
          Name = "Darklang.Stdlib.Test.allocate"
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
            Call (AST.functionIdForName "Darklang.Stdlib.Test.allocate", [intAtom 41L]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            (externalCandidates [stdlibAllocate])
            (Program ([], main))
    if containsCall "Darklang.Stdlib.Test.allocate" inlinedMain then
        Ok ()
    else
        Error "Expected external raw allocation candidate to remain a call"

let testExternalInlineCandidateRejectsControlFlowBody () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let stdlibAbs =
        { Id = AST.functionIdForName "Darklang.Stdlib.Int64.abs"
          Name = "Darklang.Stdlib.Int64.abs"
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
            Call (AST.functionIdForName "Darklang.Stdlib.Int64.abs", [intAtom 41L]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            (externalCandidates [stdlibAbs])
            (Program ([], main))
    if containsCall "Darklang.Stdlib.Int64.abs" inlinedMain then
        Ok ()
    else
        Error "Expected external control-flow stdlib candidate to remain a call"

let testExternalInliningHonorsCallerBudget () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let stdlibShiftLeft =
        { Id = AST.functionIdForName "Darklang.Stdlib.Int64.shiftLeft"
          Name = "Darklang.Stdlib.Int64.shiftLeft"
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
                (Let (TempId nextTid, Call (AST.functionIdForName "Darklang.Stdlib.Int64.shiftLeft", [intAtom 1L]), body))
    let main = calls 9 2 (Return (Var (TempId 10)))
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            (externalCandidates [stdlibShiftLeft])
            (Program ([], main))
    let remainingCalls = countCalls "Darklang.Stdlib.Int64.shiftLeft" inlinedMain
    if remainingCalls = 9 then
        Ok ()
    else
        Error $"Expected 9 external calls to remain over caller budget, but found {remainingCalls}"

let testExternalConstantCandidateBypassesCallerBudget () : TestResult =
    let tagFunction =
        { Id = AST.functionIdForName "Darklang.Stdlib.__FingerTree.__TAG_SINGLE"
          Name = "Darklang.Stdlib.__FingerTree.__TAG_SINGLE"
          TypedParams = []
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (intAtom 1L) }
    let rec calls remaining nextTid body =
        if remaining = 0 then
            body
        else
            calls
                (remaining - 1)
                (nextTid + 1)
                (Let (TempId nextTid, Call (AST.functionIdForName "Darklang.Stdlib.__FingerTree.__TAG_SINGLE", []), body))
    let main = calls 9 2 (Return (Var (TempId 10)))
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramWithExternalCandidates
            ANF_Inlining.defaultConfig
            (ANF_Inlining.buildExternalCandidateInfoMap ANF_Inlining.defaultConfig [tagFunction])
            (Program ([], main))
    if containsCall "Darklang.Stdlib.__FingerTree.__TAG_SINGLE" inlinedMain then
        Error "Expected zero-argument external constant candidate to inline even when caller exceeds external budget"
    else
        Ok ()

let testBorrowedSelfCallBlocksInlining () : TestResult =
    let param = { Id = TempId 0; Type = AST.TString }
    let borrowedSelf =
        { Id = AST.functionIdForName "borrowSelf"
          Name = "borrowSelf"
          TypedParams = [param]
          ReturnType = AST.TString
          ReturnOwnership = BorrowedReturn
          Body =
            Let (
                TempId 1,
                BorrowedCall (AST.functionIdForName "borrowSelf", [Var param.Id]),
                Return (Var (TempId 1))
            ) }
    let main =
        Let (
            TempId 2,
            Call (AST.functionIdForName "borrowSelf", [StringLiteral "x"]),
            Return (Var (TempId 2))
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([borrowedSelf], main))
    if containsCall "borrowSelf" inlinedMain then
        Ok ()
    else
        Error "Expected function with borrowed self-call to remain recursive and not be inlined"

let private boundedHashLoop (bound: int64) : Function =
    let hashParam = { Id = TempId 0; Type = AST.TInt64 }
    let dataParam = { Id = TempId 1; Type = AST.TInt64 }
    let indexParam = { Id = TempId 2; Type = AST.TInt64 }
    { Id = AST.functionIdForName "hashLoop"
      Name = "hashLoop"
      TypedParams = [hashParam; dataParam; indexParam]
      ReturnType = AST.TInt64
      ReturnOwnership = OwnedReturn
      Body =
        Let (
            TempId 3,
            Prim (Gte, Var indexParam.Id, intAtom bound),
            If (
                Var (TempId 3),
                Return (Var hashParam.Id),
                Let (
                    TempId 4,
                    Prim (BitAnd, Var dataParam.Id, intAtom 255L),
                    Let (
                        TempId 5,
                        Prim (BitXor, Var hashParam.Id, Var (TempId 4)),
                        Let (
                            TempId 6,
                            Prim (Mul, Var (TempId 5), intAtom 1099511628211L),
                            Let (
                                TempId 7,
                                Prim (Add, Var indexParam.Id, intAtom 1L),
                                Let (
                                    TempId 8,
                                    Call (
                                        AST.functionIdForName "hashLoop",
                                        [Var (TempId 6); Var dataParam.Id; Var (TempId 7)]
                                    ),
                                    Return (Var (TempId 8))
                                )
                            )
                        )
                    )
                )
            )
        ) }

let private boundedHashCall () : AExpr =
    Let (
        TempId 9,
        Call (
            AST.functionIdForName "hashLoop",
            [intAtom -3750763034362895579L; intAtom 42L; intAtom 0L]
        ),
        Return (Var (TempId 9))
    )

let testBoundedRecursiveLoopUnrollsEightIterations () : TestResult =
    let hashLoop = boundedHashLoop 8L
    let main =
        boundedHashCall ()
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([hashLoop], main))
    match containsCall "hashLoop" inlinedMain, countPrimOps Mul inlinedMain with
    | false, 8 -> Ok ()
    | hasCall, multiplyCount ->
        Error $"Expected an eight-round straight-line hash, but call={hasCall} and multiplies={multiplyCount}"

let testBoundedRecursiveLoopHonorsIterationLimit () : TestResult =
    let hashLoop = boundedHashLoop 9L
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([hashLoop], boundedHashCall ()))
    if containsCall "hashLoop" inlinedMain then
        Ok ()
    else
        Error "Expected a nine-iteration recursive loop to remain over the unrolling limit"

let testBoundedRecursiveLoopHonorsExpansionLimit () : TestResult =
    let hashLoop = boundedHashLoop 8L
    let config =
        { ANF_Inlining.defaultConfig with MaxBoundedLoopExpansion = 31 }
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgram config (Program ([hashLoop], boundedHashCall ()))
    if containsCall "hashLoop" inlinedMain then
        Ok ()
    else
        Error "Expected bounded recursive loop to remain over the expansion cost cap"

let testSequentialCallsDoNotConsumeInlineDepth () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let addOne =
        { Id = AST.functionIdForName "addOne"
          Name = "addOne"
          TypedParams = [param]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Prim (Add, Var param.Id, intAtom 1L),
                Return (Var (TempId 1))
            ) }
    let rec calls remaining argumentTid resultTid =
        if remaining = 0 then
            Return (Var argumentTid)
        else
            Let (
                TempId resultTid,
                Call (AST.functionIdForName "addOne", [Var argumentTid]),
                calls (remaining - 1) (TempId resultTid) (resultTid + 1)
            )
    let main =
        Let (
            TempId 2,
            Atom (intAtom 0L),
            calls 5 (TempId 2) 3
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([addOne], main))
    let remainingCalls = countCalls "addOne" inlinedMain
    if remainingCalls = 0 then
        Ok ()
    else
        Error $"Expected all sequential calls to inline, but found {remainingCalls}"

let testImmediateTupleProjectionsInlineLargeProducer () : TestResult =
    let param = { Id = TempId 0; Type = AST.TInt64 }
    let rec bindings remaining previousId =
        if remaining = 0 then
            Let (
                TempId 30,
                TupleAlloc [Var previousId; Var param.Id],
                Return (Var (TempId 30))
            )
        else
            let nextId = TempId (10 + remaining)
            Let (nextId, Prim (Add, Var previousId, intAtom 1L), bindings (remaining - 1) nextId)
    let producer =
        { Id = AST.functionIdForName "largePair"
          Name = "largePair"
          TypedParams = [param]
          ReturnType = AST.TTuple [AST.TInt64; AST.TInt64]
          ReturnOwnership = OwnedReturn
          Body = bindings 21 param.Id }
    let main =
        Let (
            TempId 40,
            Call (AST.functionIdForName "largePair", [intAtom 1L]),
            Let (
                TempId 41,
                TupleGet (Var (TempId 40), 0),
                Let (
                    TempId 42,
                    TupleGet (Var (TempId 40), 1),
                    Return (Var (TempId 42))
                )
            )
        )
    let (Program (_, inlinedMain)) =
        ANF_Inlining.inlineProgramDefault (Program ([producer], main))
    if containsCall "largePair" inlinedMain then
        Error "Expected an immediately projected tuple result to inline across the call boundary"
    else
        Ok ()

let tests = [
    "Inlining freshens join targets and values consistently", testJoinInliningFreshensTargets
    ("Inlining literal args removes call", testInliningWithLiteralArgumentsRemovesCall)
    ("Inlining literal args binds literal TempId", testInliningWithLiteralArgumentsBindsTemp)
    ("Inlining underscore-named functions", testInliningUnderscoreFunctionName)
    ("Excluded local functions remain calls", testExcludedLocalFunctionRemainsCall)
    ("Inlining external shift candidate", testExternalInlineCandidateRemovesShiftCall)
    ("Inlining external float conversion candidate", testExternalInlineCandidateRemovesFloatConversionCall)
    ("External raw allocation candidates are not inlined", testExternalInlineCandidateRejectsRawAllocBody)
    ("External control-flow candidates are not inlined", testExternalInlineCandidateRejectsControlFlowBody)
    ("External inlining honors caller budget", testExternalInliningHonorsCallerBudget)
    ("External constant candidates bypass caller budget", testExternalConstantCandidateBypassesCallerBudget)
    ("Borrowed self-call blocks inlining", testBorrowedSelfCallBlocksInlining)
    ("Bounded recursive loops unroll eight iterations", testBoundedRecursiveLoopUnrollsEightIterations)
    ("Bounded recursive loops honor iteration limit", testBoundedRecursiveLoopHonorsIterationLimit)
    ("Bounded recursive loops honor expansion limit", testBoundedRecursiveLoopHonorsExpansionLimit)
    ("Sequential calls do not consume inline depth", testSequentialCallsDoNotConsumeInlineDepth)
    ("Immediate tuple projections inline large producer", testImmediateTupleProjectionsInlineLargeProducer)
]
