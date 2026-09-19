// InsertExpression.fs - Elaborate ANF expression ownership using return and alias facts.

module RcInsertExpression

open MemoryModel
open MemoryPlanning
open ANF
open LiftExpressions
open LiftFunctions
open LoweringExpressions
open RcTypeFacts
open RcReturnAnalysis
open RcShapePlanning
open RcCleanup

let rec insertRCWithAnalysis
    (joinScopes: Map<TempId, Set<TempId>>)
    (inheritedBranchDecs: ReturnDec list)
    (ctx: TypeContext)
    (currentFuncName: AST.FunctionId option)
    (expr: ReturnAnnotatedExpr)
    (varGen: VarGen)
    (returnDecs: ReturnDec list)
    (inheritedTransferableOwnership: ReturnDec list)
    (paramIncs: (TempId * AST.Type * RcShape) list)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let pendingIds = returnDecs |> List.map (fun (id, _, _, _, _) -> id) |> Set.ofList
    let branchDecs =
        inheritedBranchDecs
        |> List.filter (fun (id, _, _, _, _) ->
            not (Set.contains id (returnedSet expr)) && not (Set.contains id pendingIds))
    let returnDecs = branchDecs @ returnDecs
    let ctxWithTypes = withTempTypes ctx types
    let functionReturnsNestedRecordListDict =
        let isSingleListDictRecord (name: string) : bool =
            ctx.TypeReg
            |> Map.tryFind name
            |> Option.map (fun recordInfo ->
                match recordInfo.Fields |> List.map snd with
                | [ AST.TList (AST.TDict _) ] -> true
                | _ -> false)
            |> Option.defaultValue false

        match currentFuncName |> Option.bind (tryGetFuncReturnTypeFromReg ctx) with
        | Some (AST.TList (AST.TRecord (name, _))) ->
            isSingleListDictRecord name
        | _ ->
            false
    let mapHelperTransfersSecondParam =
        let isMapHelper (funcName: AST.FunctionId) : bool =
            match Map.tryFind funcName ctx.FuncReg with
            | Some (name, _) ->
                name = "Darklang.Stdlib.List.__mapHelper"
                || name.StartsWith("Darklang.Stdlib.List.__mapHelper_")
            | None -> false
        let secondParamTransfersOwnership (funcName: AST.FunctionId) : bool =
            match Map.tryFind funcName ctx.FuncReg with
            | Some (_, AST.TFunction (_ :: secondParamType :: _, _)) ->
                secondParamType
                |> rcShapeForType ctx
                |> rcShapeIsOwnershipTransferRoot
            | _ ->
                false

        currentFuncName
        |> Option.exists (fun funcName ->
            isMapHelper funcName && secondParamTransfersOwnership funcName)
    let rec descend
        (ctx: TypeContext)
        (expr: ReturnAnnotatedExpr)
        (varGen: VarGen)
        (returnDecs: ReturnDec list)
        (inheritedTransferableOwnership: ReturnDec list)
        (inheritedBranchDecs: ReturnDec list)
        (frames: LetFrame list)
        (types: Map<TempId, AST.Type>)
        : AExpr * VarGen * Map<TempId, AST.Type> =
        match expr with
        | RJump (target, atom, _) ->
            let deferred =
                match Map.tryFind target joinScopes with
                | Some ids -> ids
                | None -> Crash.crash $"RC insertion: join target {target} is not in scope"
            let localDecs = returnDecs |> List.filter (fun (id, _, _, _, _) -> not (Set.contains id deferred))
            insertReturnDecs localDecs (Jump (target, atom)) varGen types
            |> applyLetFrames ctx frames

        | RJoin (parameter, continuation, entry, _) ->
            let conditional = (frames |> List.choose (fun frame -> frame.BranchDec)) @ inheritedBranchDecs
            let transferable = (frames |> List.choose (fun frame -> frame.TransferableOwnership)) @ inheritedTransferableOwnership
            let deferred =
                conditional @ returnDecs |> List.map (fun (id, _, _, _, _) -> id) |> Set.ofList
            let continuationTypes = Map.add parameter.Id parameter.Type types
            let body, afterBody, bodyTypes =
                insertRCWithAnalysis joinScopes conditional ctx currentFuncName continuation varGen returnDecs transferable paramIncs continuationTypes
            let entry', final, finalTypes =
                insertRCWithAnalysis (Map.add parameter.Id deferred joinScopes) conditional ctx currentFuncName entry afterBody returnDecs transferable paramIncs bodyTypes
            applyLetFrames ctx frames (Join (parameter, body, entry'), final, finalTypes)

        | RReturn (atom, returned) ->
            let baseExpr = Return atom
            let (withParamIncs, varGen1, types1) =
                insertParamIncsAtReturn ctx paramIncs returned baseExpr varGen types
            let (withDecs, varGen2, types2) = insertReturnDecs returnDecs withParamIncs varGen1 types1
            let (finalExpr, finalVarGen, finalTypes) = applyLetFrames ctx frames (withDecs, varGen2, types2)
            (finalExpr, finalVarGen, finalTypes)

        | RIf (cond, thenBranch, elseBranch, _) ->
            let branchConditionalOwnership =
                (frames |> List.choose (fun frame -> frame.BranchDec)) @ inheritedBranchDecs
            let branchTransferableOwnership =
                frames
                |> List.choose (fun frame -> frame.TransferableOwnership)
                |> fun local -> local @ inheritedTransferableOwnership
            let returnDecTemps =
                returnDecs
                |> List.map (fun (tempId, _, _, _, _) -> tempId)
                |> Set.ofList
            let branchLocalDecs (branchReturned: Set<TempId>) : ReturnDec list =
                let frameDecs =
                    frames
                    |> List.choose (fun frame ->
                        match frame.BranchDec with
                        | Some (tempId, _, _, _, _ as dec)
                            when not (Set.contains tempId branchReturned)
                                 && not (Set.contains tempId returnDecTemps) ->
                            Some dec
                        | _ ->
                            None)
                frameDecs
            let (thenBranch', varGen1, types1) =
                insertRCWithAnalysis
                    joinScopes
                    branchConditionalOwnership
                    ctx
                    currentFuncName
                    thenBranch
                    varGen
                    (branchLocalDecs (returnedSet thenBranch) @ returnDecs)
                    branchTransferableOwnership
                    paramIncs
                    types
            let (elseBranch', varGen2, types2) =
                insertRCWithAnalysis
                    joinScopes
                    branchConditionalOwnership
                    ctx
                    currentFuncName
                    elseBranch
                    varGen1
                    (branchLocalDecs (returnedSet elseBranch) @ returnDecs)
                    branchTransferableOwnership
                    paramIncs
                    types1
            let (finalExpr, finalVarGen, finalTypes) =
                applyLetFrames ctx frames (If (cond, thenBranch', elseBranch'), varGen2, types2)
            (finalExpr, finalVarGen, finalTypes)

        | RLet (tempId, (Print _ as printExpr), RReturn (atom, returned), _) ->
            // Generated result printing consumes the returned root after
            // lowering. Finalize every other ownership obligation first,
            // matching the ordinary return boundary while keeping the output
            // effect visible to ownership analysis.
            let printed = Let (tempId, printExpr, Return atom)
            let typesWithPrint = Map.add tempId AST.TUnit types
            let (withParamIncs, varGen1, types1) =
                insertParamIncsAtReturn ctx paramIncs returned printed varGen typesWithPrint
            let (withDecs, varGen2, types2) =
                insertReturnDecs returnDecs withParamIncs varGen1 types1
            applyLetFrames ctx frames (withDecs, varGen2, types2)

        | RLet (tempId, cexpr, bodyInfo, _) ->
            let (TempId tempIdInt) = tempId

            // This walk visits each binding once. Do not memoize by CExpr:
            // sibling branches may reuse TempIds, making structurally equal
            // expressions resolve to different types in their local contexts.
            let maybeType = inferCExprType ctx cexpr

            // When a temp is aliased through one or more let-bound vars, infer its type
            // from the first concrete use-site (typically a call argument position).
            let rec inferAliasedVarTypeFromUse (aliasedTemp: TempId) (nextBody: ReturnAnnotatedExpr) : AST.Type option =
                let inferFromCall (funcName: AST.FunctionId) (args: Atom list) : AST.Type option =
                    match Map.tryFind funcName ctx.FuncReg with
                    | Some (_, AST.TFunction (paramTypes, _)) ->
                        args
                        |> List.mapi (fun idx atom -> (idx, atom))
                        |> List.tryPick (fun (idx, atom) ->
                            match atom with
                            | Var tid when tid = aliasedTemp && idx < List.length paramTypes ->
                                Some (List.item idx paramTypes)
                            | _ ->
                                None)
                    | _ ->
                        None

                match nextBody with
                | RLet (_, RawSlotInit (_, _, Var valueTemp, valueType), _, _) when valueTemp = aliasedTemp ->
                    Some valueType
                | RLet (_, Call (funcName, args), _, _) ->
                    inferFromCall funcName args
                | RLet (_, BorrowedCall (funcName, args), _, _) ->
                    inferFromCall funcName args
                | RLet (_, TailCall (funcName, args), _, _) ->
                    inferFromCall funcName args
                | RLet (nextAliasTemp, Atom (Var sourceId), nextNextBody, _) when sourceId = aliasedTemp ->
                    inferAliasedVarTypeFromUse nextAliasTemp nextNextBody
                | RLet (nextAliasTemp, TypedAtom (Var sourceId, aliasType), nextNextBody, _) when sourceId = aliasedTemp ->
                    if shapeNeedsManagedAliasRootPreservation ctx aliasType then
                        Some aliasType
                    else
                        inferAliasedVarTypeFromUse nextAliasTemp nextNextBody
                | RIf (Var tid, _, _, _) when tid = aliasedTemp ->
                    Some AST.TBool
                | _ ->
                    None

            // Use a TypedAtom alias in the body to preserve the intended payload type
            // when TupleGet cannot infer it from a multi-parameter sum.
            let inferredType =
                match maybeType with
                | Some t ->
                    let aliasTypeFromBody =
                        match bodyInfo with
                        | RLet (_, TypedAtom (Var sourceId, aliasType), _, _) when sourceId = tempId ->
                            Some aliasType
                        | RLet (aliasTemp, Atom (Var sourceId), nextBody, _) when sourceId = tempId ->
                            inferAliasedVarTypeFromUse aliasTemp nextBody
                        | _ ->
                            None

                    match aliasTypeFromBody with
                    | Some inferredAliasType when shapeNeedsManagedAliasRootPreservation ctx inferredAliasType && not (shapeNeedsManagedAliasRootPreservation ctx t) ->
                        inferredAliasType
                    | _ ->
                        t
                | None ->
                    match cexpr, bodyInfo with
                    | TupleGet _, RLet (_, TypedAtom (Var sourceId, aliasType), _, _) when sourceId = tempId ->
                        aliasType
                    | RawGet (_, _, None), RLet (_, TypedAtom (Var sourceId, aliasType), _, _) when sourceId = tempId ->
                        // RawGet without an explicit type is often immediately re-typed via TypedAtom.
                        // Preserve that alias type instead of guessing Int64.
                        aliasType
                    | RawGet (_, _, None), RLet (aliasTemp, Atom (Var sourceId), nextBody, _) when sourceId = tempId ->
                        match inferAliasedVarTypeFromUse aliasTemp nextBody with
                        | Some inferredAliasType ->
                            inferredAliasType
                        | None ->
                            // Keep unresolved rather than guessing Int64.
                            AST.TVar $"raw_get_{tempIdInt}"
                    | RawGet (_, _, None), _ ->
                        // Unknown RawGet payload type: preserve as unresolved type variable.
                        AST.TVar $"raw_get_{tempIdInt}"
                    | _ ->
                        // Preserve unresolved type information instead of defaulting to Int64.
                        AST.TVar $"inferred_{tempIdInt}"

            let typesWithBinding =
                match cexpr with
                | TypedAtom (Var sourceId, aliasType) ->
                    types |> Map.add tempId inferredType |> Map.add sourceId aliasType
                | _ ->
                    Map.add tempId inferredType types

            let ctxWithTypes = withTempTypes ctx typesWithBinding

            // Track closure function names for later ClosureCall type resolution
            let ctx'' =
                match cexpr with
                | ClosureAlloc (funcName, _) -> addClosureFunc ctxWithTypes tempId funcName
                | _ -> ctxWithTypes
            let inferredShape = rcShapeForType ctx inferredType

            let bodyReturned = returnedSet bodyInfo
            let consumedByImmediateI64Push =
                let isI64Push (funcName: AST.FunctionId) : bool =
                    funcName = AST.functionIdForName "Darklang.Stdlib.List.__push_i64"
                    || funcName = AST.functionIdForName "Darklang.Stdlib.List.__pushBack_i64"
                let consumesSecondArg (args: Atom list) : bool =
                    match args with
                    | _listAtom :: Var valueTemp :: _ -> valueTemp = tempId
                    | _ -> false
                match bodyInfo with
                | RLet (_, Call (funcName, args), _, _)
                | RLet (_, TailCall (funcName, args), _, _) ->
                    isI64Push funcName && consumesSecondArg args
                | _ ->
                    false
            let skipReturnDecForMapHelperLists =
                mapHelperTransfersSecondParam
                && match inferredType with
                   | AST.TList _ -> true
                   | _ -> false

            let bindingDec =
                let materializesBorrowedCall =
                    match cexpr with
                    | BorrowedCall _ -> true
                    | _ -> false
                if bindingNeedsShapeAutomaticDec ctx cexpr inferredType inferredShape
                   && (not (isBorrowingExpr cexpr) || materializesBorrowedCall)
                   && not (cexprProducesNonRcSentinel cexpr)
                   && not skipReturnDecForMapHelperLists
                   && not consumedByImmediateI64Push then
                    let kindOverride =
                        match inferredType with
                        | AST.TList (AST.TFunction _) -> Some TaggedList
                        | _ -> None
                    Some (
                        createReturnDec
                            ctx
                            tempId
                            inferredType
                            inferredShape
                            kindOverride)
                else
                    None

            // TempIds are unique within the function, so the current binding's
            // pending-release multiplicity is known while prepending it. Keep
            // that fact instead of rescanning the growing release stack.
            let (returnDecs', currentBindingHasSinglePendingDec) =
                let needsNestedRecordListDictDictDec =
                    functionReturnsNestedRecordListDict
                    && match inferredType with
                       | AST.TDict _ -> true
                       | _ -> false

                match bindingDec with
                | Some dec when not (Set.contains tempId bodyReturned) ->
                    if needsNestedRecordListDictDictDec then
                        // List<Record { List<Dict<_, _>> }> construction retains the
                        // dict once for the inner list payload and once for the returned graph.
                        // The current shape-specific ARM64 helpers release that graph, but the
                        // local dict temp still needs both ownership edges balanced.
                        (dec :: dec :: returnDecs, false)
                    else
                        (dec :: returnDecs, true)
                | _ ->
                    (returnDecs, false)

            let rec tempProducesNonRcSentinel (targetId: TempId) : bool =
                frames
                |> List.tryFind (fun frame -> frame.TempId = targetId)
                |> Option.map (fun frame ->
                    match tryOwnershipPreservingAliasSource frame.CExpr with
                    | Some sourceId -> tempProducesNonRcSentinel sourceId
                    | None ->
                        match frame.CExpr with
                        | IfValue (_, thenValue, elseValue) ->
                            atomProducesNonRcSentinel thenValue
                            && atomProducesNonRcSentinel elseValue
                        | _ -> cexprProducesNonRcSentinel frame.CExpr)
                |> Option.defaultValue false

            and atomProducesNonRcSentinel (atom: Atom) : bool =
                match atom with
                | StringLiteral _ -> true
                | Var sourceId -> tempProducesNonRcSentinel sourceId
                | _ -> false

            let allocationIncTargets =
                let compoundAllocationTargets =
                    match cexpr with
                    | TupleAlloc elems ->
                        elems
                        |> List.fold (fun acc atom ->
                            match atom with
                            | Var tid ->
                                match tryGetType ctxWithTypes tid with
                                | Some t ->
                                    let shape = rcShapeForType ctx t
                                    if rcShapeNeedsBorrowedRetain shape
                                       && not (tempProducesNonRcSentinel tid) then
                                        (tid, t, shape) :: acc
                                    else
                                        acc
                                | _ -> acc
                            | _ -> acc
                        ) []
                        |> List.rev
                    | RecordAlloc (_, fields)
                    | RecordClone (_, _, fields) ->
                        fields
                        |> List.fold (fun acc atom ->
                            match atom with
                            | Var tid ->
                                match tryGetType ctx tid with
                                | Some t ->
                                    let shape = rcShapeForType ctx t
                                    if rcShapeNeedsBorrowedRetain shape
                                       && not (tempProducesNonRcSentinel tid) then
                                        (tid, t, shape) :: acc
                                    else
                                        acc
                                | _ -> acc
                            | _ -> acc
                        ) []
                        |> List.rev
                    | ClosureAlloc (_, captures) ->
                        captures
                        |> List.fold (fun acc atom ->
                            match atom with
                            | Var tid ->
                                match tryGetType ctxWithTypes tid with
                                | Some t ->
                                    let shape = rcShapeForType ctx t
                                    if rcShapeNeedsBorrowedRetain shape
                                       && not (tempProducesNonRcSentinel tid) then
                                        (tid, t, shape) :: acc
                                    else
                                        acc
                                | _ -> acc
                            | _ -> acc
                        ) []
                        |> List.rev
                    | _ -> []
                let erasedSkewListElementTargets =
                    match cexpr with
                    | Call (funcName, [_; Var valueTemp])
                    | TailCall (funcName, [_; Var valueTemp]) when
                        funcName = AST.functionIdForName "Darklang.Stdlib.List.__push_i64"
                        || funcName = AST.functionIdForName "Darklang.Stdlib.List.__pushBack_i64" ->
                        let transfersImmediateOwnedValue =
                            match frames with
                            | previous :: _ when previous.TempId = valueTemp ->
                                not (isBorrowingExpr previous.CExpr)
                            | _ ->
                                false
                        match transfersImmediateOwnedValue, tryGetType ctxWithTypes valueTemp with
                        | false, Some valueType ->
                            let shape = rcShapeForType ctx valueType
                            if rcShapeNeedsBorrowedRetain shape
                               && not (tempProducesNonRcSentinel valueTemp) then
                                [ valueTemp, valueType, shape ]
                            else
                                []
                        | _ ->
                            []
                    | _ ->
                        []
                compoundAllocationTargets @ erasedSkewListElementTargets

            let rawSlotRetainTargets =
                match cexpr with
                | RawSlotInit (_, _, _, AST.TStream _) ->
                    // A materialized Stream seeds its RC word at zero; the
                    // first typed slot retain establishes the owning edge.
                    []
                | RawSlotInit (_, _, Var valueTemp, valueType) ->
                    let shape = rcShapeForType ctx valueType
                    if rcShapeNeedsBorrowedRetain shape
                       && not (tempProducesNonRcSentinel valueTemp) then
                        [valueTemp, valueType, shape]
                    else
                        []
                | _ ->
                    []

            let transferableOwnership =
                match bindingDec, currentBindingHasSinglePendingDec with
                | Some dec, true
                    when transfersIntoReturnedAggregate tempId bodyInfo
                         || transfersIntoRawSlot tempId bodyInfo ->
                    Some dec
                | _ ->
                    None

            let rec resolveAliasOwner (targetId: TempId) : TempId =
                frames
                |> List.tryFind (fun frame -> frame.TempId = targetId)
                |> Option.map (fun frame ->
                    match tryOwnershipPreservingAliasSource frame.CExpr with
                    | Some sourceId -> resolveAliasOwner sourceId
                    | None -> targetId)
                |> Option.defaultValue targetId

            let transferredOwnership =
                (allocationIncTargets @ rawSlotRetainTargets)
                |> List.fold (fun (transfers, transferredOwners) (targetId, _, _) ->
                    let ownerId = resolveAliasOwner targetId
                    if Set.contains ownerId transferredOwners then
                        (transfers, transferredOwners)
                    else
                        frames
                        |> List.tryPick (fun candidate ->
                            match candidate.TransferableOwnership with
                            | Some ((candidateOwnerId, _, _, _, _) as pendingDec) when candidateOwnerId = ownerId ->
                                Some pendingDec
                            | _ ->
                                None)
                        |> Option.orElseWith (fun () ->
                            inheritedTransferableOwnership
                            |> List.tryFind (fun (candidateOwnerId, _, _, _, _) -> candidateOwnerId = ownerId))
                        |> Option.map (fun pendingDec ->
                            ((targetId, pendingDec) :: transfers, Set.add ownerId transferredOwners))
                        |> Option.defaultValue (transfers, transferredOwners)
                ) ([], Set.empty)
                |> fst
                |> List.rev

            let cexprAfterTransfers =
                match cexpr, transferredOwnership with
                | RawSlotInit (ptr, byteOffset, Var valueTemp, _), transfers when
                    transfers
                    |> List.exists (fun (targetId, _) -> targetId = valueTemp) ->
                    // RawSlotInit's backend retain is unnecessary when the slot
                    // adopts the producer's existing owned edge.
                    RawWriteWord (ptr, byteOffset, Var valueTemp)
                | _ ->
                    cexpr

            let transferredOwnerIds =
                transferredOwnership
                |> List.map (fun (_, (ownerId, _, _, _, _)) -> ownerId)
                |> Set.ofList

            let allocationIncTargetsAfterTransfers =
                let removeFirstTarget
                    (targetId: TempId)
                    (targets: (TempId * AST.Type * RcShape) list)
                    : (TempId * AST.Type * RcShape) list =
                    let rec loop prefix remaining =
                        match remaining with
                        | [] -> List.rev prefix
                        | (candidateId, _, _) :: tail when candidateId = targetId ->
                            List.rev prefix @ tail
                        | head :: tail ->
                            loop (head :: prefix) tail
                    loop [] targets

                transferredOwnership
                |> List.fold (fun targets (targetId, _) -> removeFirstTarget targetId targets) allocationIncTargets

            let rec removePendingDec
                (target: ReturnDec)
                (pending: ReturnDec list)
                : ReturnDec list =
                match pending with
                | [] -> []
                | head :: tail when head = target -> tail
                | head :: tail -> head :: removePendingDec target tail

            let returnDecsAfterTransfers =
                transferredOwnership
                |> List.fold (fun pending (_, target) -> removePendingDec target pending) returnDecs'

            let inheritedTransferableOwnershipAfterTransfers =
                transferredOwnership
                |> List.fold
                    (fun pending (_, target) -> removePendingDec target pending)
                    inheritedTransferableOwnership

            let framesAfterTransfers =
                // Ownership transfers are rare. Preserve the existing frame
                // spine when there is nothing to clear instead of rebuilding
                // every preceding frame for every ordinary binding.
                if Set.isEmpty transferredOwnerIds then
                    frames
                else
                    frames
                    |> List.map (fun candidate ->
                        if Set.contains candidate.TempId transferredOwnerIds then
                            {
                                candidate with
                                    TransferableOwnership = None
                                    BranchDec = None
                            }
                        else
                            candidate)

            let returnInc =
                let retainedTypeFromAtom (atom: Atom) : (AST.Type * RcShape) option =
                    match atom with
                    | Var tid ->
                        match tryGetType ctx tid with
                        | Some t ->
                            let shape = rcShapeForType ctx t
                            if rcShapeNeedsBorrowedRetain shape then
                                Some (t, shape)
                            else
                                None
                        | _ -> None
                    | _ -> None

                let borrowedProjectionFeedsSelfTailCall =
                    let sourceParentIsOwnedLocal (sourceId: TempId) : bool =
                        let rec loop (visited: Set<TempId>) (candidateId: TempId) : bool =
                            if Set.contains candidateId visited then
                                false
                            else
                                frames
                                |> List.tryFind (fun frame -> frame.TempId = candidateId)
                                |> Option.exists (fun frame ->
                                    match frame.CExpr with
                                    | Atom (Var aliasedId)
                                    | TypedAtom (Var aliasedId, _) ->
                                        loop (Set.add candidateId visited) aliasedId
                                    | _ ->
                                        not (isBorrowingExpr frame.CExpr))

                        loop Set.empty sourceId

                    match currentFuncName, cexpr with
                    | Some funcName, TupleGet (Var sourceId, _)
                    | Some funcName, RecordGet (_, Var sourceId, _) ->
                        sourceParentIsOwnedLocal sourceId
                        && isTempUsedAsSelfTailCallArg ctx funcName tempId bodyInfo
                    | _ ->
                        false

                match cexpr with
                | BorrowedCall _ when rcShapeNeedsBorrowedRetain inferredShape ->
                    // A borrowed call has no owned result edge to transfer from
                    // its callee. Materialize one for the local binding; its
                    // ordinary pending decrement then balances this retain.
                    Some (inferredType, inferredShape)
                | IfValue (_, thenAtom, elseAtom) ->
                    // IfValue selects one of two existing heap values.
                    // Materialize ownership on the selected temp before source temps are decref'd.
                    match retainedTypeFromAtom thenAtom, retainedTypeFromAtom elseAtom with
                    | Some info, _ -> Some info
                    | None, Some info -> Some info
                    | None, None -> None
                | _ when borrowedProjectionFeedsSelfTailCall
                         && rcShapeNeedsBorrowedRetain inferredShape ->
                    Some (inferredType, inferredShape)
                | Atom (Var sourceId)
                | TypedAtom (Var sourceId, _) ->
                    // Returning a pure alias of an already-returned owned value should not inc again.
                    if rcShapeNeedsBorrowedRetain inferredShape
                       && Set.contains tempId bodyReturned
                       && isBorrowingExpr cexpr then
                        if Set.contains sourceId bodyReturned then
                            None
                        else
                            Some (inferredType, inferredShape)
                    else
                        None
                | _ ->
                    if rcShapeNeedsBorrowedRetain inferredShape
                       && Set.contains tempId bodyReturned
                       && isBorrowingExpr cexpr then
                        Some (inferredType, inferredShape)
                    else
                        None

            let frame = {
                TempId = tempId
                CExpr = cexprAfterTransfers
                TupleIncTargets = allocationIncTargetsAfterTransfers
                TransferableOwnership = transferableOwnership
                ReturnInc = returnInc
                BranchDec = bindingDec
            }

            // Process the body iteratively, then rebuild on the way back out
            descend
                ctx''
                bodyInfo
                varGen
                returnDecsAfterTransfers
                inheritedTransferableOwnershipAfterTransfers
                (inheritedBranchDecs |> List.filter (fun (id, _, _, _, _) -> not (Set.contains id transferredOwnerIds)))
                (frame :: framesAfterTransfers)
                typesWithBinding

    descend ctxWithTypes expr varGen returnDecs inheritedTransferableOwnership inheritedBranchDecs [] types

/// Insert reference counting operations into an AExpr
/// Returns (transformed expr, varGen, accumulated TempTypes)
let internal insertRCInternal
    (ctx: TypeContext)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let ctxWithTypes = withTempTypes ctx types
    let analyzed = analyzeReturns Map.empty Map.empty expr
    insertRCWithAnalysis Map.empty [] ctxWithTypes None analyzed varGen [] [] [] types

/// Insert reference counting operations into an AExpr
/// Returns (transformed expr, varGen, accumulated TempTypes)
let insertRC (ctx: TypeContext) (expr: AExpr) (varGen: VarGen) : AExpr * VarGen * Map<TempId, AST.Type> =
    insertRCInternal ctx expr varGen Map.empty
