// CleanupTests.fs - Verify alias transfers and cleanup ordering through ANF scopes.

module RcCleanupTests

open MemoryModel
open ANF
open RcTypeFacts
open RcCleanup
open RefCountInsertion
open MemoryShapeTests

let rec private hasDecAfterNonSelfTailCall (funcName: string) (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ ->
        false
    | Let (_, TailCall (target, _), Let (_, RefCountDec _, _)) when target <> funcName ->
        true
    | Let (_, TailCall (target, _), Let (_, RefCountDecString _, _)) when target <> funcName ->
        true
    | Let (_, _, body) ->
        hasDecAfterNonSelfTailCall funcName body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        hasDecAfterNonSelfTailCall funcName thenBranch
        || hasDecAfterNonSelfTailCall funcName elseBranch

let rec internal hasRefCountIncForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ ->
        false
    | Let (_, RefCountInc (Var tempId, _, _, _), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasRefCountIncForTemp target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        hasRefCountIncForTemp target thenBranch
        || hasRefCountIncForTemp target elseBranch

let rec private countRefCountIncsForTemps (targets: Set<TempId>) (expr: AExpr) : int =
    match expr with
    | Jump _ | Return _ ->
        0
    | Let (_, RefCountInc (Var tempId, _, _, _), body) ->
        let current = if Set.contains tempId targets then 1 else 0
        current + countRefCountIncsForTemps targets body
    | Let (_, _, body) ->
        countRefCountIncsForTemps targets body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        countRefCountIncsForTemps targets thenBranch
        + countRefCountIncsForTemps targets elseBranch

let rec internal hasRefCountDecForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ ->
        false
    | Let (_, RefCountDec (Var tempId, _, _, _), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasRefCountDecForTemp target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        hasRefCountDecForTemp target thenBranch
        || hasRefCountDecForTemp target elseBranch

let rec private hasRawSlotInitForValue (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ ->
        false
    | Let (_, RawSlotInit (_, _, Var valueId, _), _) when valueId = target ->
        true
    | Let (_, _, body) ->
        hasRawSlotInitForValue target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        hasRawSlotInitForValue target thenBranch
        || hasRawSlotInitForValue target elseBranch

let rec private hasRawWriteWordForValue (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ ->
        false
    | Let (_, RawWriteWord (_, _, Var valueId), _) when valueId = target ->
        true
    | Let (_, _, body) ->
        hasRawWriteWordForValue target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        hasRawWriteWordForValue target thenBranch
        || hasRawWriteWordForValue target elseBranch

let rec private hasStringRetainForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ ->
        false
    | Let (_, RefCountIncString (Var tempId), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasStringRetainForTemp target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        hasStringRetainForTemp target thenBranch
        || hasStringRetainForTemp target elseBranch

let rec private hasStringReleaseForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ ->
        false
    | Let (_, RefCountDecString (Var tempId), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasStringReleaseForTemp target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        hasStringReleaseForTemp target thenBranch
        || hasStringReleaseForTemp target elseBranch

let internal pathHasRetainsBeforeDec
    (retainTargets: TempId list)
    (decTarget: TempId)
    (expr: AExpr)
    : bool =
    let required = Set.ofList retainTargets

    let rec loop joins (seenRetains: Set<TempId>) (expr: AExpr) : bool =
        match expr with
        | Return _ ->
            false
        | Jump (target, _) ->
            match Map.tryFind target joins with
            | Some continuation -> loop joins seenRetains continuation
            | None -> Crash.crash "RC path check: jump target outside lexical scope"
        | Join (parameter, continuation, entry) ->
            loop (Map.add parameter.Id continuation joins) seenRetains entry
        | Let (_, RefCountInc (Var tempId, _, _, _), body) ->
            loop joins (Set.add tempId seenRetains) body
        | Let (_, RefCountDec (Var tempId, _, _, _), _) when tempId = decTarget ->
            Set.isSubset required seenRetains
        | Let (_, _, body) ->
            loop joins seenRetains body
        | If (_, thenBranch, elseBranch) ->
            loop joins seenRetains thenBranch || loop joins seenRetains elseBranch

    loop Map.empty Set.empty expr

let private rawSlotTransferTestFunction
    (valueType: AST.Type)
    (usesValueAfterSlot: bool)
    : TypeContext * Function * TempId =
    let listType = AST.TList valueType
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("makeValue", AST.TFunction ([], valueType))
            ("observeValue", AST.TFunction ([valueType], AST.TUnit))
            ("makeList", AST.TFunction ([], listType))
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

    let valueTemp = TempId 0
    let ptrTemp = TempId 1
    let slotTemp = TempId 2
    let countTemp = TempId 3
    let taggedTemp = TempId 4
    let listTemp = TempId 5
    let observedTemp = TempId 6
    let alternateTaggedTemp = TempId 7
    let alternateListTemp = TempId 8
    let listReturn taggedId listId =
        Let (
            taggedId,
            Prim (BitOr, Var ptrTemp, IntLiteral (Int64 2L)),
            Let (listId, TypedAtom (Var taggedId, listType), Return (Var listId))
        )
    let tail =
        if usesValueAfterSlot then
            listReturn taggedTemp listTemp
        else
            If (
                BoolLiteral true,
                listReturn taggedTemp listTemp,
                listReturn alternateTaggedTemp alternateListTemp
            )
    let afterSlot =
        if usesValueAfterSlot then
            Let (observedTemp, Call ("observeValue", [Var valueTemp]), tail)
        else
            tail
    let func : Function = {
        Name = "makeList"
        TypedParams = []
        ReturnType = listType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                valueTemp,
                Call ("makeValue", []),
                Let (
                    ptrTemp,
                    RawAlloc (IntLiteral (Int64 16L)),
                    Let (
                        slotTemp,
                        RawSlotInit (Var ptrTemp, IntLiteral (Int64 0L), Var valueTemp, valueType),
                        Let (
                            countTemp,
                            RawWriteWord (Var ptrTemp, IntLiteral (Int64 8L), IntLiteral (Int64 1L)),
                            afterSlot
                        )
                    )
                )
            )
    }
    (ctx, func, valueTemp)

let testFreshOwnedValueTransfersIntoRawSlot () : TestResult =
    let (ctx, func, valueTemp) = rawSlotTransferTestFunction AST.TString false
    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRawSlotInitForValue valueTemp transformed.Body then
        Error "Fresh raw-slot payload should not retain a copied ownership edge"
    elif not (hasRawWriteWordForValue valueTemp transformed.Body) then
        Error "Fresh raw-slot payload should move its owned edge with an unmanaged store"
    elif hasStringReleaseForTemp valueTemp transformed.Body then
        Error "Raw-slot ownership transfer should remove the producer's pending release"
    else
        Ok ()

let testRawSlotRetainsValueUsedAfterInitialization () : TestResult =
    let (ctx, func, valueTemp) = rawSlotTransferTestFunction AST.TString true
    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if not (hasRawSlotInitForValue valueTemp transformed.Body) then
        Error "Raw slot must retain a payload that remains in use after initialization"
    elif not (hasStringReleaseForTemp valueTemp transformed.Body) then
        Error "Raw-slot payload used afterward must keep the producer's pending release"
    else
        Ok ()

let testRawSlotRetainsFreshStreamValue () : TestResult =
    let streamType = AST.TStream AST.TInt64
    let (ctx, func, valueTemp) = rawSlotTransferTestFunction streamType false
    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if not (hasRawSlotInitForValue valueTemp transformed.Body) then
        Error "Fresh Stream raw-slot payload must keep the retain that establishes its first owned edge"
    elif not (hasRefCountDecForTemp valueTemp transformed.Body) then
        Error "Fresh Stream raw-slot payload must retain its local pending release"
    else
        Ok ()

let rec internal tryRefCountDecSourceTypeForTemp (target: TempId) (expr: AExpr) : AST.Type option =
    match expr with
    | Jump _ | Return _ ->
        None
    | Let (_, RefCountDec (Var tempId, _, _, metadata), _) when tempId = target ->
        metadata |> Option.bind (fun value -> value.SourceType)
    | Let (_, _, body) ->
        tryRefCountDecSourceTypeForTemp target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        match tryRefCountDecSourceTypeForTemp target thenBranch with
        | Some typ -> Some typ
        | None -> tryRefCountDecSourceTypeForTemp target elseBranch

let testBranchLocalTempReuseUsesCurrentTypeContext () : TestResult =
    let listType = AST.TList (AST.TTuple [AST.TInt64; AST.TFloat64])
    let expectedWrapperType = AST.TTuple [AST.TInt64; listType]
    let conditionTemp = TempId 0
    let payloadTemp = TempId 1
    let wrapperTemp = TempId 2
    let wrapperExpr = TupleAlloc [IntLiteral (Int64 0L); Var payloadTemp]
    let branch payloadExpr =
        Let (
            payloadTemp,
            payloadExpr,
            Let (
                wrapperTemp,
                wrapperExpr,
                Return (IntLiteral (Int64 0L))
            )
        )
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }
    let func : Function = {
        Name = "branchLocalTypeContext"
        TypedParams = [{ Id = conditionTemp; Type = AST.TBool }]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            If (
                Var conditionTemp,
                branch (Atom (IntLiteral (Int64 1L))),
                branch (TypedAtom (IntLiteral (Int64 0L), listType))
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen
    match transformed.Body with
    | If (_, _, elseBranch) ->
        match tryRefCountDecSourceTypeForTemp wrapperTemp elseBranch with
        | Some actual when actual = expectedWrapperType -> Ok ()
        | Some actual ->
            Error $"Expected branch-local wrapper type {expectedWrapperType}, got {actual}"
        | None ->
            Error "Expected branch-local wrapper to receive an automatic RefCountDec"
    | _ ->
        Error "Expected branch-local type fixture to retain its conditional body"

let testReturnedAggregateTransfersOwnedValueThroughAlias () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType]
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("wrapChild", AST.TFunction ([], outerType))
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

    let childTemp = TempId 0
    let aliasTemp = TempId 1
    let outerTemp = TempId 2
    let func : Function = {
        Name = "wrapChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    aliasTemp,
                    Atom (Var childTemp),
                    Let (
                        outerTemp,
                        TupleAlloc [Var aliasTemp],
                        Return (Var outerTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp aliasTemp transformed.Body then
        Error "Returned aggregate should adopt owned value through a pure alias without retaining it"
    elif hasRefCountDecForTemp childTemp transformed.Body then
        Error "Returned aggregate alias transfer should remove the original owner's pending release"
    else
        Ok ()

let testReturnedAggregateTransfersOwnedValueThroughTypedAlias () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType]
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("wrapChild", AST.TFunction ([], outerType))
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

    let childTemp = TempId 0
    let aliasTemp = TempId 1
    let outerTemp = TempId 2
    let func : Function = {
        Name = "wrapChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    aliasTemp,
                    TypedAtom (Var childTemp, childType),
                    Let (
                        outerTemp,
                        TupleAlloc [Var aliasTemp],
                        Return (Var outerTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp aliasTemp transformed.Body then
        Error "Returned aggregate should adopt owned value through a typed alias without retaining it"
    elif hasRefCountDecForTemp childTemp transformed.Body then
        Error "Returned aggregate typed-alias transfer should remove the original owner's pending release"
    else
        Ok ()

let testReturnedAggregateRetainsOwnershipProducingStreamAlias () : TestResult =
    let streamType = AST.TStream AST.TInt64
    let outerType = AST.TTuple [streamType]
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("wrapStream", AST.TFunction ([], outerType))
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

    let ptrTemp = TempId 0
    let streamTemp = TempId 1
    let outerTemp = TempId 2
    let func : Function = {
        Name = "wrapStream"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                ptrTemp,
                RawAlloc (IntLiteral (Int64 32L)),
                Let (
                    streamTemp,
                    TypedAtom (Var ptrTemp, streamType),
                    Let (
                        outerTemp,
                        TupleAlloc [Var streamTemp],
                        Return (Var outerTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp streamTemp transformed.Body then
        Ok ()
    else
        Error "RawPtr-to-Stream creates ownership at the aggregate boundary and must retain the Stream alias"

let testReturnedAggregateTransfersOwnedValueAfterBorrowedUse () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType; AST.TInt64]
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("inspectChild", AST.TFunction ([childType], AST.TInt64))
            ("wrapChild", AST.TFunction ([], outerType))
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

    let childTemp = TempId 0
    let aliasTemp = TempId 1
    let inspectedTemp = TempId 2
    let outerTemp = TempId 3
    let func : Function = {
        Name = "wrapChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    aliasTemp,
                    Atom (Var childTemp),
                    Let (
                        inspectedTemp,
                        Call ("inspectChild", [Var aliasTemp]),
                        Let (
                            outerTemp,
                            TupleAlloc [Var aliasTemp; Var inspectedTemp],
                            Return (Var outerTemp)
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp aliasTemp transformed.Body then
        Error "Returned aggregate should adopt an owned value after its earlier borrowed uses"
    elif hasRefCountDecForTemp childTemp transformed.Body then
        Error "Borrowed uses before returned aggregate transfer should not preserve the owner's release"
    else
        Ok ()

let testExplicitReleaseBlocksLaterAggregateTransfer () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType]
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("wrapChild", AST.TFunction ([], outerType))
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

    let childTemp = TempId 0
    let releaseTemp = TempId 1
    let outerTemp = TempId 2
    let func : Function = {
        Name = "wrapChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    releaseTemp,
                    RefCountDec (Var childTemp, 8, GenericHeap, None),
                    Let (
                        outerTemp,
                        TupleAlloc [Var childTemp],
                        Return (Var outerTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp childTemp transformed.Body then
        Ok ()
    else
        Error "An explicit release must block ownership transfer at a later aggregate use"

let testReturnedAggregateTransfersOwnedValueAcrossBranches () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType]
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("wrapChild", AST.TFunction ([AST.TBool], outerType))
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

    let conditionTemp = TempId 0
    let childTemp = TempId 1
    let thenOuterTemp = TempId 2
    let elseOuterTemp = TempId 3
    let func : Function = {
        Name = "wrapChild"
        TypedParams = [{ Id = conditionTemp; Type = AST.TBool }]
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                If (
                    Var conditionTemp,
                    Let (
                        thenOuterTemp,
                        TupleAlloc [Var childTemp],
                        Return (Var thenOuterTemp)
                    ),
                    Let (
                        elseOuterTemp,
                        TupleAlloc [Var childTemp],
                        Return (Var elseOuterTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp childTemp transformed.Body then
        Error "Every returning branch should adopt the owned value without retaining it"
    elif hasRefCountDecForTemp childTemp transformed.Body then
        Error "Branch-complete aggregate transfer should remove the original owner's release"
    else
        Ok ()

let testReturnedAggregateRequiresEveryBranchToTransferOwnedValue () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType]
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("wrapChild", AST.TFunction ([AST.TBool], outerType))
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

    let conditionTemp = TempId 0
    let childTemp = TempId 1
    let replacementTemp = TempId 2
    let thenOuterTemp = TempId 3
    let elseOuterTemp = TempId 4
    let func : Function = {
        Name = "wrapChild"
        TypedParams = [{ Id = conditionTemp; Type = AST.TBool }]
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                If (
                    Var conditionTemp,
                    Let (
                        thenOuterTemp,
                        TupleAlloc [Var childTemp],
                        Return (Var thenOuterTemp)
                    ),
                    Let (
                        replacementTemp,
                        Call ("makeChild", []),
                        Let (
                            elseOuterTemp,
                            TupleAlloc [Var replacementTemp],
                            Return (Var elseOuterTemp)
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp childTemp transformed.Body then
        Ok ()
    else
        Error "A value absent from one returning branch must keep its retain in the branch that packages it"

let testReturnedAggregateTransfersNestedOwnedAliases () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let innerType = AST.TTuple [childType]
    let outerType = AST.TTuple [innerType]
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("wrapChild", AST.TFunction ([], outerType))
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

    let childTemp = TempId 0
    let childAlias = TempId 1
    let innerTemp = TempId 2
    let innerAlias = TempId 3
    let outerTemp = TempId 4
    let func : Function = {
        Name = "wrapChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    childAlias,
                    Atom (Var childTemp),
                    Let (
                        innerTemp,
                        TupleAlloc [Var childAlias],
                        Let (
                            innerAlias,
                            Atom (Var innerTemp),
                            Let (
                                outerTemp,
                                TupleAlloc [Var innerAlias],
                                Return (Var outerTemp)
                            )
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen
    let retainedAlias =
        hasRefCountIncForTemp childAlias transformed.Body
        || hasRefCountIncForTemp innerAlias transformed.Body
    let releasedOwner =
        hasRefCountDecForTemp childTemp transformed.Body
        || hasRefCountDecForTemp innerTemp transformed.Body

    if retainedAlias then
        Error "Nested returned aggregates should adopt owned values through pure aliases without retaining them"
    elif releasedOwner then
        Error "Nested returned aggregate alias transfer should remove each original owner's pending release"
    else
        Ok ()

let testReturnedAggregateDoesNotTransferDuplicatedAliases () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType; childType]
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("duplicateChild", AST.TFunction ([], outerType))
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

    let childTemp = TempId 0
    let firstAlias = TempId 1
    let secondAlias = TempId 2
    let outerTemp = TempId 3
    let func : Function = {
        Name = "duplicateChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    firstAlias,
                    Atom (Var childTemp),
                    Let (
                        secondAlias,
                        TypedAtom (Var childTemp, childType),
                        Let (
                            outerTemp,
                            TupleAlloc [Var firstAlias; Var secondAlias],
                            Return (Var outerTemp)
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    let retainCount =
        countRefCountIncsForTemps (Set.ofList [firstAlias; secondAlias]) transformed.Body

    if retainCount = 1 && not (hasRefCountDecForTemp childTemp transformed.Body) then
        Ok ()
    else
        Error $"Duplicated aliases should transfer one owned edge and retain one shared edge; got {retainCount} retains"

let testStaticStringBindingSkipsNoOpRcTraffic () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let stringTemp = TempId 0
    let resultTemp = TempId 1
    let func : Function = {
        Name = "staticString"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                stringTemp,
                Atom (StringLiteral "static"),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasStringReleaseForTemp stringTemp transformed.Body then
        Error "Static string binding should not emit a runtime no-op release"
    else
        Ok ()

let testKnownEmptyListBindingSkipsNoOpRcTraffic () : TestResult =
    let listType = AST.TList AST.TInt64
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let listTemp = TempId 0
    let resultTemp = TempId 1
    let func : Function = {
        Name = "emptyList"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                listTemp,
                TypedAtom (IntLiteral (Int64 0L), listType),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp listTemp transformed.Body then
        Error "Known empty-list binding should not emit a runtime no-op release"
    else
        Ok ()

let testAggregateSkipsRetainsForKnownNonRcSentinels () : TestResult =
    let listType = AST.TList AST.TInt64
    let resultType = AST.TTuple [AST.TString; listType]
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let stringTemp = TempId 0
    let listTemp = TempId 1
    let resultTemp = TempId 2
    let func : Function = {
        Name = "sentinelTuple"
        TypedParams = []
        ReturnType = resultType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                stringTemp,
                Atom (StringLiteral "static"),
                Let (
                    listTemp,
                    TypedAtom (IntLiteral (Int64 0L), listType),
                    Let (
                        resultTemp,
                        TupleAlloc [Var stringTemp; Var listTemp],
                        Return (Var resultTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasStringRetainForTemp stringTemp transformed.Body then
        Error "Aggregate should not retain a known static string field"
    elif hasRefCountIncForTemp listTemp transformed.Body then
        Error "Aggregate should not retain a known empty-list field"
    else
        Ok ()

let testAggregateSkipsRetainForConditionalStaticString () : TestResult =
    let resultType = AST.TTuple [AST.TString]
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let conditionTemp = TempId 0
    let stringTemp = TempId 1
    let resultTemp = TempId 2
    let func : Function = {
        Name = "conditionalStaticString"
        TypedParams = [{ Id = conditionTemp; Type = AST.TBool }]
        ReturnType = resultType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                stringTemp,
                IfValue (
                    Var conditionTemp,
                    StringLiteral "first",
                    StringLiteral "second"
                ),
                Let (
                    resultTemp,
                    TupleAlloc [Var stringTemp],
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasStringRetainForTemp stringTemp transformed.Body then
        Error "Aggregate should not retain a conditional whose alternatives are both static strings"
    else
        Ok ()

let testNonSelfTailCallDoesNotLeaveDecAfterTailCall () : TestResult =
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("callee", AST.TFunction ([AST.TInt64], AST.TInt64))
            ("caller", AST.TFunction ([AST.TInt64], AST.TInt64))
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

    let p0 = TempId 0
    let tupleTmp = TempId 1
    let callTmp = TempId 2
    let func : Function = {
        Name = "caller"
        TypedParams = [{ Id = p0; Type = AST.TInt64 }]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                tupleTmp,
                TupleAlloc [Var p0; IntLiteral (Int64 1L)],
                Let (
                    callTmp,
                    TailCall ("callee", [Var p0]),
                    Return (Var callTmp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasDecAfterNonSelfTailCall transformed.Name transformed.Body then
        Error "Found RefCountDec after non-self TailCall; dec should execute before tailcall"
    else
        Ok ()

let testAliasReturnMaterializesOwnershipEvenIfFunctionMarkedBorrowed () : TestResult =
    let nodeType = AST.TList AST.TInt64
    let funcReg : TypeRegistries.FunctionRegistry =
        Map.ofList [
            ("Darklang.Stdlib.List.__node2GetChild_i64", AST.TFunction ([nodeType; AST.TInt64], nodeType))
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

    let nodeParam = TempId 0
    let indexParam = TempId 1
    let childTemp = TempId 2

    let func : Function = {
        Name = "Darklang.Stdlib.List.__node2GetChild_i64"
        TypedParams = [
            { Id = nodeParam; Type = nodeType }
            { Id = indexParam; Type = AST.TInt64 }
        ]
        ReturnType = nodeType
        ReturnOwnership = BorrowedReturn
        Body =
            Let (
                childTemp,
                RawGet (Var nodeParam, IntLiteral (Int64 0L), Some nodeType),
                Return (Var childTemp)
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp childTemp transformed.Body then
        Ok ()
    else
        Error "Alias return should materialize ownership with RefCountInc even when function is marked BorrowedReturn"
