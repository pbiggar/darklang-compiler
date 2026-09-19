// TransferTests.fs - Verify recursive ownership transfers and borrowed projections.

module RcTransferTests

open MemoryModel
open ANF
open RcTypeFacts
open RcCleanup
open RefCountInsertion
open MemoryShapeTests
open RcCleanupTests

let private fid = AST.functionIdForName
let private functionRegistry entries : TypeRegistries.FunctionRegistry =
    entries |> List.map (fun (name, typ) -> fid name, (name, typ)) |> Map.ofList

let testMapHelperAccumulatorReturnDoesNotRetainOwnedAccumulator () : TestResult =
    let sourceListType = AST.TList AST.TInt64
    let mappedListType = AST.TList (AST.TFunction ([AST.TInt64], AST.TInt64))
    let mapperType = AST.TFunction ([AST.TInt64], AST.TFunction ([AST.TInt64], AST.TInt64))
    let helperName = "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_to_i64"
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            (helperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let sourceParam = TempId 0
    let mapperParam = TempId 1
    let accParam = TempId 2
    let func : Function = {
        Id = fid helperName
        Name = helperName
        TypedParams = [
            { Id = sourceParam; Type = sourceListType }
            { Id = mapperParam; Type = mapperType }
            { Id = accParam; Type = mappedListType }
        ]
        ReturnType = mappedListType
        ReturnOwnership = OwnedReturn
        Body = Return (Var accParam)
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp accParam transformed.Body then
        Error "Darklang.Stdlib.List.__mapHelper should transfer its owned accumulator return without retaining it"
    else
        Ok ()

let testMapHelperSelfTailCallReleasesReplacedAccumulator () : TestResult =
    let sourceListType = AST.TList AST.TInt64
    let mappedListType = AST.TList (AST.TFunction ([AST.TInt64], AST.TInt64))
    let mapperType = AST.TFunction ([AST.TInt64], AST.TFunction ([AST.TInt64], AST.TInt64))
    let helperName = "Darklang.Stdlib.List.__mapHelper"
    let specializedHelperName = "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_to_i64"
    let pushBackName = "Darklang.Stdlib.List.__pushBack_fn_i64_to_i64"
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            (helperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
            (specializedHelperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
            ("mappedClosure", AST.TFunction ([AST.TInt64], AST.TInt64))
            (pushBackName, AST.TFunction ([mappedListType; AST.TFunction ([AST.TInt64], AST.TInt64)], mappedListType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let sourceParam = TempId 0
    let mapperParam = TempId 1
    let accParam = TempId 2
    let closureTemp = TempId 3
    let newAccTemp = TempId 4
    let tailTemp = TempId 5
    let func : Function = {
        Id = fid helperName
        Name = helperName
        TypedParams = [
            { Id = sourceParam; Type = sourceListType }
            { Id = mapperParam; Type = mapperType }
            { Id = accParam; Type = mappedListType }
        ]
        ReturnType = mappedListType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                closureTemp,
                ClosureAlloc (fid "mappedClosure", []),
                Let (
                    newAccTemp,
                    Call (fid pushBackName, [Var accParam; Var closureTemp]),
                    Let (
                        tailTemp,
                        TailCall (fid specializedHelperName, [Var sourceParam; Var mapperParam; Var newAccTemp]),
                        Return (Var tailTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp accParam transformed.Body then
        Ok ()
    else
        Error "Darklang.Stdlib.List.__mapHelper self tail-call should release the replaced owned accumulator"

let private testBorrowedProjectionRecursiveArgsAreRetained (recursiveCExpr: AST.FunctionId -> Atom list -> CExpr) : TestResult =
    let state1Type = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64]
    let state2Type = AST.TTuple [AST.TInt64; AST.TInt64]
    let resultType = AST.TTuple [state1Type; state2Type]
    let helperName = "loop"
    let roundName = "round"
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            (helperName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
            (roundName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let state1Param = TempId 0
    let state2Param = TempId 1
    let iParam = TempId 2
    let resultTemp = TempId 3
    let nextState1Temp = TempId 4
    let nextState2Temp = TempId 5
    let tailTemp = TempId 6
    let func : Function = {
        Id = fid helperName
        Name = helperName
        TypedParams = [
            { Id = state1Param; Type = state1Type }
            { Id = state2Param; Type = state2Type }
            { Id = iParam; Type = AST.TInt64 }
        ]
        ReturnType = resultType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                resultTemp,
                Call (fid roundName, [Var state1Param; Var state2Param; Var iParam]),
                Let (
                    nextState1Temp,
                    TupleGet (Var resultTemp, 0),
                    Let (
                        nextState2Temp,
                        TupleGet (Var resultTemp, 1),
                        Let (
                            tailTemp,
                            recursiveCExpr (fid helperName) [Var nextState1Temp; Var nextState2Temp; Var iParam],
                            Return (Var tailTemp)
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if pathHasRetainsBeforeDec [nextState1Temp; nextState2Temp] resultTemp transformed.Body then
        Ok ()
    else
        Error "Borrowed tuple projections passed as self-tail-call accumulators should be retained before parent cleanup"

let testBorrowedProjectionSelfTailCallArgsAreRetained () : TestResult =
    testBorrowedProjectionRecursiveArgsAreRetained (fun funcName args -> TailCall (funcName, args))

let testBorrowedProjectionSelfRecursiveCallArgsAreRetained () : TestResult =
    testBorrowedProjectionRecursiveArgsAreRetained (fun funcName args -> Call (funcName, args))

let testBorrowedProjectionAliasSelfRecursiveCallArgsAreRetained () : TestResult =
    let state1Type = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64]
    let state2Type = AST.TTuple [AST.TInt64; AST.TInt64]
    let resultType = AST.TTuple [state1Type; state2Type]
    let helperName = "loop"
    let roundName = "round"
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            (helperName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
            (roundName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let state1Param = TempId 0
    let state2Param = TempId 1
    let iParam = TempId 2
    let resultTemp = TempId 3
    let nextState1Temp = TempId 4
    let nextState2Temp = TempId 5
    let nextState1Alias = TempId 6
    let nextState2Alias = TempId 7
    let tailTemp = TempId 8
    let func : Function = {
        Id = fid helperName
        Name = helperName
        TypedParams = [
            { Id = state1Param; Type = state1Type }
            { Id = state2Param; Type = state2Type }
            { Id = iParam; Type = AST.TInt64 }
        ]
        ReturnType = resultType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                resultTemp,
                Call (fid roundName, [Var state1Param; Var state2Param; Var iParam]),
                Let (
                    nextState1Temp,
                    TupleGet (Var resultTemp, 0),
                    Let (
                        nextState2Temp,
                        TupleGet (Var resultTemp, 1),
                        Let (
                            nextState1Alias,
                            TypedAtom (Var nextState1Temp, state1Type),
                            Let (
                                nextState2Alias,
                                TypedAtom (Var nextState2Temp, state2Type),
                                Let (
                                    tailTemp,
                                    Call (fid helperName, [Var nextState1Alias; Var nextState2Alias; Var iParam]),
                                    Return (Var tailTemp)
                                )
                            )
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp nextState1Temp transformed.Body
       && hasRefCountIncForTemp nextState2Temp transformed.Body then
        Ok ()
    else
        Error "Borrowed tuple projection aliases passed as self-recursive accumulators should be retained before parent cleanup"

let testBorrowedProjectionIfBranchSelfRecursiveCallArgsAreRetained () : TestResult =
    let state1Type = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64]
    let state2Type = AST.TTuple [AST.TInt64; AST.TInt64]
    let resultType = AST.TTuple [state1Type; state2Type]
    let helperName = "loop"
    let roundName = "round"
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            (helperName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
            (roundName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let state1Param = TempId 0
    let state2Param = TempId 1
    let iParam = TempId 2
    let baseResultTemp = TempId 3
    let roundResultTemp = TempId 4
    let roundResultAliasTemp = TempId 5
    let nextState1Temp = TempId 6
    let nextState1AliasTemp = TempId 7
    let nextState1SecondAliasTemp = TempId 8
    let nextState2Temp = TempId 9
    let nextState2AliasTemp = TempId 10
    let nextState2SecondAliasTemp = TempId 11
    let nextI = TempId 12
    let recursiveResultTemp = TempId 13
    let func : Function = {
        Id = fid helperName
        Name = helperName
        TypedParams = [
            { Id = state1Param; Type = state1Type }
            { Id = state2Param; Type = state2Type }
            { Id = iParam; Type = AST.TInt64 }
        ]
        ReturnType = resultType
        ReturnOwnership = OwnedReturn
        Body =
            If (
                Var iParam,
                Let (
                    baseResultTemp,
                    TupleAlloc [Var state1Param; Var state2Param],
                    Return (Var baseResultTemp)
                ),
                Let (
                    roundResultTemp,
                    Call (fid roundName, [Var state1Param; Var state2Param; Var iParam]),
                    Let (
                        roundResultAliasTemp,
                        Atom (Var roundResultTemp),
                        Let (
                            nextState1Temp,
                            TupleGet (Var roundResultAliasTemp, 0),
                            Let (
                                nextState1AliasTemp,
                                TypedAtom (Var nextState1Temp, state1Type),
                                Let (
                                    nextState1SecondAliasTemp,
                                    TypedAtom (Var nextState1AliasTemp, state1Type),
                                    Let (
                                        nextState2Temp,
                                        TupleGet (Var roundResultAliasTemp, 1),
                                        Let (
                                            nextState2AliasTemp,
                                            TypedAtom (Var nextState2Temp, state2Type),
                                            Let (
                                                nextState2SecondAliasTemp,
                                                TypedAtom (Var nextState2AliasTemp, state2Type),
                                                Let (
                                                    nextI,
                                                    Prim (Add, Var iParam, IntLiteral (Int64 1L)),
                                                    Let (
                                                        recursiveResultTemp,
                                                        Call (
                                                            fid helperName,
                                                            [
                                                                Var nextState1SecondAliasTemp
                                                                Var nextState2SecondAliasTemp
                                                                Var nextI
                                                            ]
                                                        ),
                                                        Return (Var recursiveResultTemp)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if pathHasRetainsBeforeDec [nextState1Temp; nextState2Temp] roundResultTemp transformed.Body then
        Ok ()
    else
        Error "Borrowed tuple projections in recursive if branches should be retained before parent cleanup"

let testBorrowedProjectionFromParameterSelfRecursiveCallStaysBorrowed () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let parentType = AST.TTuple [childType]
    let helperName = "loop"
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            (helperName, AST.TFunction ([parentType; childType], childType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let parentParam = TempId 0
    let childParam = TempId 1
    let projectedTemp = TempId 2
    let resultTemp = TempId 3
    let func : Function = {
        Id = fid helperName
        Name = helperName
        TypedParams = [
            { Id = parentParam; Type = parentType }
            { Id = childParam; Type = childType }
        ]
        ReturnType = childType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                projectedTemp,
                TupleGet (Var parentParam, 0),
                Let (
                    resultTemp,
                    Call (fid helperName, [Var parentParam; Var projectedTemp]),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp projectedTemp transformed.Body then
        Error "Borrowed projection from a parameter should not be retained solely because it feeds a self-recursive call"
    else
        Ok ()

let testMapHelperClosureProducingCallRetainsBorrowedSource () : TestResult =
    let sourceListType = AST.TList AST.TInt64
    let mappedListType = AST.TList (AST.TFunction ([AST.TInt64], AST.TInt64))
    let mapperType = AST.TFunction ([AST.TInt64], AST.TFunction ([AST.TInt64], AST.TInt64))
    let helperName = "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_to_i64"
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            (helperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let sourceParam = TempId 0
    let mapperParam = TempId 1
    let accParam = TempId 2
    let mappedTemp = TempId 3
    let func : Function = {
        Id = fid "caller"
        Name = "caller"
        TypedParams = [
            { Id = sourceParam; Type = sourceListType }
            { Id = mapperParam; Type = mapperType }
            { Id = accParam; Type = mappedListType }
        ]
        ReturnType = mappedListType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                mappedTemp,
                Call (fid helperName, [Var sourceParam; Var mapperParam; Var accParam]),
                Return (Var mappedTemp)
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp sourceParam transformed.Body then
        Ok ()
    else
        Error "Callers entering closure-producing Stdlib.List.__mapHelper should retain the borrowed source list"

let testMapHelperClosureSourceToValueKeepsSourceBorrowed () : TestResult =
    let sourceListType = AST.TList (AST.TFunction ([AST.TInt64], AST.TInt64))
    let mappedListType = AST.TList AST.TInt64
    let mapperType = AST.TFunction ([AST.TFunction ([AST.TInt64], AST.TInt64)], AST.TInt64)
    let helperName = "Darklang.Stdlib.List.__mapHelper_fn_i64_to_i64_i64"
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            (helperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let sourceParam = TempId 0
    let mapperParam = TempId 1
    let accParam = TempId 2
    let func : Function = {
        Id = fid helperName
        Name = helperName
        TypedParams = [
            { Id = sourceParam; Type = sourceListType }
            { Id = mapperParam; Type = mapperType }
            { Id = accParam; Type = mappedListType }
        ]
        ReturnType = mappedListType
        ReturnOwnership = OwnedReturn
        Body = Return (Var accParam)
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp sourceParam transformed.Body then
        Error "Darklang.Stdlib.List.__mapHelper over closure source to value should not retain an unreturned borrowed source parameter"
    elif hasRefCountDecForTemp sourceParam transformed.Body then
        Error "Darklang.Stdlib.List.__mapHelper over closure source to value should not release a borrowed source parameter"
    else
        Ok ()

let testClosurePushBackRetainsImmediateClosureCallResult () : TestResult =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let makerType = AST.TFunction ([AST.TInt64], closureType)
    let listType = AST.TList closureType
    let pushBackName = "Darklang.Stdlib.List.__pushBack_fn_i64_to_i64"
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            ("makeClosure", makerType)
            ("mappedClosure", closureType)
            ("returnedClosure", closureType)
            (pushBackName, AST.TFunction ([listType; closureType], listType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let makerTemp = TempId 0
    let returnedTemp = TempId 1
    let listParam = TempId 2
    let pushedTemp = TempId 3
    let func : Function = {
        Id = fid "caller"
        Name = "caller"
        TypedParams = [
            { Id = listParam; Type = listType }
        ]
        ReturnType = listType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                makerTemp,
                ClosureAlloc (fid "makeClosure", []),
                Let (
                    returnedTemp,
                    ClosureCall (Var makerTemp, [IntLiteral (Int64 5L)]),
                    Let (
                        pushedTemp,
                        Call (fid pushBackName, [Var listParam; Var returnedTemp]),
                        Return (Var pushedTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp returnedTemp transformed.Body then
        Ok ()
    else
        Error "ClosureCall result passed directly to typed closure-list pushBack should get a local dec because raw storage retains the edge"
