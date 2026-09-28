// SSARefCountInsertion.fs - Insert ownership operations directly in SSA ANF blocks.

module RcSSARefCountInsertion

open ANF
open MemoryModel
open MemoryPlanning
open RcTypeFacts
open RcShapePlanning
open RcCleanup

type private Definitions = {
    Operations: Map<TempId, CExpr>
    Owned: Set<TempId>
    OwnedParams: Set<TempId>
    Types: Map<TempId, AST.SemanticType>
    FuncNames: Map<AST.FunctionId, string>
}

let private isEmptyListCall (funcNames: Map<AST.FunctionId, string>) operation =
    match operation with
    | Call (target, []) ->
        Map.tryFind target funcNames
        |> Option.exists (fun name -> name.StartsWith("Darklang.Stdlib.List.__empty"))
    | _ -> false

let private definitions
    (ctx: TypeContext)
    (frontierParams: Set<TempId>)
    (func: SSAANF.Function)
    : Definitions =
    let types = func.FreshValueTypes
    let funcNames = ctx.FuncReg |> Map.map (fun _ (name, _) -> name)
    let operations =
        func.Blocks
        |> Map.fold (fun state _ block ->
            block.Operations
            |> List.fold (fun current (id, operation) -> Map.add id operation current) state)
            Map.empty
    let ownedOperations =
        operations
        |> Map.fold (fun state id operation ->
            match Map.tryFind id types with
            | Some typ ->
                let shape = rcShapeForType ctx typ
                let adoptsRawAllocation =
                    match operation with
                    | TypedAtom (Var source, resultType) ->
                        Map.tryFind source types = Some AST.TInternalRawPtr
                        && (match resultType with AST.TStream _ -> false | _ -> true)
                    | _ -> false
                let isNullContainer =
                    match operation, typ with
                    | Atom (IntLiteral (Int64 0L)), AST.TList _
                    | Atom (IntLiteral (Int64 0L)), AST.TDict _ -> true
                    | _ -> false
                if bindingNeedsShapeAutomaticDec ctx operation typ shape
                   && (not (RcReturnAnalysis.isBorrowingExpr operation)
                       || adoptsRawAllocation
                       || match operation with
                          | BorrowedCall _ | IfValue _ -> true
                          | _ -> false)
                   && not (cexprProducesNonRcSentinel operation)
                   && not isNullContainer
                   && not (isEmptyListCall funcNames operation) then
                    Set.add id state
                else state
            | None -> state) Set.empty
    let ownedBlockParams =
        func.Blocks
        |> Map.fold (fun state _ block ->
            block.Parameters
            |> List.fold (fun state parameter ->
                if rcShapeForType ctx parameter.Type |> rcShapeNeedsOwnedScopeRelease then
                    Set.add parameter.Id state
                else state) state) ownedOperations
    let returned = RcSSAReturnAnalysis.analyze func
    let hasReturningSelfCall =
        operations
        |> Map.exists (fun id operation ->
            match operation with
            | Call (target, _) when target = func.Id ->
                Map.tryFind id returned.AfterDefinition
                |> Option.exists (Set.contains id)
            | _ -> false)
    let returnedAtEntry =
        if hasReturningSelfCall then
            Map.tryFind func.Entry returned.AtEntry |> Option.defaultValue Set.empty
        else Set.empty
    let ownedParams =
        func.TypedParams
        |> List.filter (fun parameter ->
            (Set.contains parameter.Id frontierParams
             || (Set.contains parameter.Id returnedAtEntry
                 && (match parameter.Type with
                     | AST.TRecord _ -> true
                     | _ -> func.Name.Contains("$trmo"))))
            && (rcShapeForType ctx parameter.Type |> rcShapeNeedsOwnedScopeRelease))
        |> List.map (fun parameter -> parameter.Id)
        |> Set.ofList
    { Operations = operations
      Owned = Set.union ownedBlockParams ownedParams
      OwnedParams = ownedParams
      Types = types
      FuncNames = funcNames }

let private sourceOfAlias operation =
    RcReturnAnalysis.tryOwnershipPreservingAliasSource operation

let private borrowedSource (definitions: Definitions) operation =
    match operation with
    // Pattern lowering reuses scalar getAt wrappers for erased payload types.
    // Their result can still point into the source list after the call returns.
    | Call (target, Var source :: _)
        when Map.tryFind target definitions.FuncNames
             |> Option.exists (fun name -> name.StartsWith("Darklang.Stdlib.List.__getAt")) ->
        Some source
    | Prim ((BitAnd | BitOr), Var source, _) -> Some source
    | TupleGet (Var source, _)
    | RecordGet (_, Var source, _)
    | RawGet (Var source, _, _)
    | RawTake (Var source, _, _)
    | StringToRawPtr (Var source)
    | BlobToRawPtr (Var source)
    | DictToRawPtr (Var source)
    | ListToRawPtr (Var source)
    | FixedBlockToRawPtr (Var source) -> Some source
    | _ -> None

let private ownerOf (definitions: Definitions) (id: TempId) : TempId option =
    let rec follow visited id =
        if Set.contains id visited then None
        elif Set.contains id definitions.Owned then Some id
        else
            Map.tryFind id definitions.Operations
            |> Option.bind sourceOfAlias
            |> Option.bind (follow (Set.add id visited))
    follow Set.empty id

let private isNonRcSentinel (definitions: Definitions) (id: TempId) : bool =
    let rec follow visited id =
        if Set.contains id visited then false
        else
            match Map.tryFind id definitions.Operations with
            | Some (Atom (StringLiteral _))
            | Some (TypedAtom (StringLiteral _, _)) -> true
            | Some (Atom (IntLiteral (Int64 0L))) ->
                match Map.tryFind id definitions.Types with
                | Some (AST.TList _ | AST.TDict _) -> true
                | _ -> false
            | Some operation when isEmptyListCall definitions.FuncNames operation -> true
            | Some operation ->
                sourceOfAlias operation
                |> Option.exists (follow (Set.add id visited))
            | None -> false
    follow Set.empty id

let private liveOwners (definitions: Definitions) (live: Set<TempId>) : Set<TempId> =
    let rec roots visited id =
        if Set.contains id visited then Set.empty
        else
            let visited = Set.add id visited
            let own = ownerOf definitions id |> Option.toList |> Set.ofList
            let borrowed =
                Map.tryFind id definitions.Operations
                |> Option.bind (fun operation ->
                    borrowedSource definitions operation
                    |> Option.orElseWith (fun () -> sourceOfAlias operation))
                |> Option.map (roots visited)
                |> Option.defaultValue Set.empty
            Set.union own borrowed
    live |> Set.fold (fun state id -> Set.union state (roots Set.empty id)) Set.empty

let private release
    (ctx: TypeContext)
    (definitions: Definitions)
    (id: TempId)
    (next: VarGen)
    : (TempId * CExpr) * VarGen * (TempId * AST.SemanticType) =
    let typ =
        match Map.tryFind id definitions.Types with
        | Some typ -> typ
        | None -> Crash.crash $"SSA RC: owned value {id} has no type"
    let shape = rcShapeForType ctx typ
    let (_, _, _, kind, metadata, nullableString) = createReturnDec ctx id typ shape None
    let operation = releaseExprForShape id typ shape kind metadata nullableString
    let dummy, after = freshVar next
    (dummy, operation), after, (dummy, AST.TUnit)

let private retain
    (ctx: TypeContext)
    (definitions: Definitions)
    (id: TempId)
    (next: VarGen)
    : (TempId * CExpr) * VarGen * (TempId * AST.SemanticType) =
    let typ =
        match Map.tryFind id definitions.Types with
        | Some typ -> typ
        | None -> Crash.crash $"SSA RC: retained value {id} has no type"
    let shape = rcShapeForType ctx typ
    let operation = retainExprForShape ctx id typ shape
    let dummy, after = freshVar next
    (dummy, operation), after, (dummy, AST.TUnit)

let private maxValueId (func: SSAANF.Function) =
    let allIds =
        func.TypedParams |> List.map (fun parameter -> parameter.Id)
        |> fun ids ->
            func.Blocks
            |> Map.fold (fun current _ block ->
                current
                @ (block.Parameters |> List.map (fun parameter -> parameter.Id))
                @ (block.Operations |> List.map fst)) ids
    allIds |> List.fold (fun largest (TempId id) -> max largest id) -1

let private releaseSource operation =
    match operation with
    | RefCountDec (Var id, _, _, _)
    | RefCountDecString (Var id)
    | RefCountDecBlob (Var id)
    | RefCountDecInt (Var id) -> Some id
    | _ -> None

let private capturedValues (definitions: Definitions) (operation: CExpr) : Atom list =
    match operation with
    | TupleAlloc values
    | RecordAlloc (_, values)
    | ClosureAlloc (_, values)
    | RecordClone (_, _, values)
    | RecordReuse (_, _, _, values) -> values
    | Call (target, [_; value])
    | TailCall (target, [_; value])
        when Map.tryFind target definitions.FuncNames
             |> Option.exists (fun name ->
                 name.StartsWith("Darklang.Stdlib.List.__push_i64")
                 || name.StartsWith("Darklang.Stdlib.List.__pushBack_i64")) ->
        [value]
    | _ -> []

/// Insert retains and releases using SSA value liveness and edge ownership.
let insertBlockLocal
    (ctx: TypeContext)
    (frontierParams: Set<TempId>)
    (func: SSAANF.Function)
    : SSAANF.Function =
    let definitions = definitions (withTempTypes ctx func.FreshValueTypes) frontierParams func
    let liveness = RcSSAValueLiveness.analyze func
    let returned = RcSSAReturnAnalysis.analyze func
    let returnConstructionIds =
        let directlyReturned =
            definitions.Operations
            |> Map.fold (fun ids id _ ->
                if Map.tryFind id returned.AfterDefinition |> Option.exists (Set.contains id) then
                    Set.add id ids
                else ids) Set.empty
        let rec includeCaptured ids =
            let next =
                definitions.Operations
                |> Map.fold (fun known id operation ->
                    if Set.contains id ids then
                        capturedValues definitions operation
                        |> List.fold (fun state atom ->
                            match atom with
                            | Var source -> Set.add source state
                            | _ -> state) known
                    else known) ids
            if next = ids then ids else includeCaptured next
        includeCaptured directlyReturned
    let isSelfRecursive =
        func.Blocks
        |> Map.exists (fun _ block ->
            block.Operations
            |> List.exists (fun (_, operation) ->
                match operation with
                | Call (target, _) -> target = func.Id
                | _ -> false))
    let initial = VarGen (max 4000 (maxValueId func + 1))
    let blocks, next, types =
        func.Blocks
        |> Map.fold (fun (blocks, next, types) label block ->
            let operations, next, types, deferred =
                block.Operations
                |> List.fold (fun (operations, next, types, deferred) ((id, operation) as original) ->
                    let liveAfter =
                        Map.tryFind id liveness.AfterDefinition
                        |> Option.defaultValue Set.empty
                    let neededOwners = liveOwners definitions liveAfter
                    let captures = capturedValues definitions operation
                    let returnsAggregate = Set.contains id returnConstructionIds
                    let captureRetains, transferredOwners =
                        captures
                        |> List.fold (fun (retains, transferred) atom ->
                            match atom with
                            | Var source when not (isNonRcSentinel definitions source) ->
                                match Map.tryFind source definitions.Types with
                                | Some typ when rcShapeForType ctx typ |> rcShapeNeedsBorrowedRetain ->
                                    match ownerOf definitions source with
                                    | Some owner when returnsAggregate
                                                      && not (Set.contains owner neededOwners)
                                                      && not (Set.contains owner transferred) ->
                                        retains, Set.add owner transferred
                                    | _ -> source :: retains, transferred
                                | _ -> retains, transferred
                            | _ -> retains, transferred)
                            ([], Set.empty)
                    let captureBindings, next, types =
                        List.rev captureRetains
                        |> List.fold (fun (bindings, next, types) source ->
                            let binding, after, (newId, typ) = retain ctx definitions source next
                            binding :: bindings, after, Map.add newId typ types)
                            ([], next, types)
                    let captureBindings = List.rev captureBindings
                    let reuseCleanupBindings, next, types =
                        match operation with
                        | RecordReuse (sourceDescriptor, _, Var source, _) ->
                            sourceDescriptor.Fields
                            |> List.mapi (fun index (_, typ) -> index, typ)
                            |> List.filter (fun (_, typ) ->
                                rcShapeForType ctx typ |> rcShapeNeedsOwnedScopeRelease)
                            |> List.fold (fun (bindings, current, types) (index, typ) ->
                                let fieldId, afterField = freshVar current
                                let releaseId, afterRelease = freshVar afterField
                                let shape = rcShapeForType ctx typ
                                let (_, _, _, kind, metadata, nullableString) =
                                    createReturnDec ctx fieldId typ shape None
                                let release =
                                    releaseExprForShape fieldId typ shape kind metadata nullableString
                                bindings
                                @ [ fieldId, RecordGet (sourceDescriptor, Var source, index)
                                    releaseId, release ],
                                afterRelease,
                                types
                                |> Map.add fieldId typ
                                |> Map.add releaseId AST.TUnit)
                                ([], next, types)
                        | RecordReuse _ ->
                            Crash.crash "SSA RC: record reuse source must be a variable"
                        | _ -> [], next, types
                    let operation =
                        match operation with
                        | RawSlotInit (ptr, offset, Var source, valueType)
                            when (match valueType with AST.TStream _ -> false | _ -> true)
                                 && (ownerOf definitions source
                                     |> Option.exists (fun owner ->
                                         not (Set.contains owner neededOwners))) ->
                            RawWriteWord (ptr, offset, Var source)
                        | _ -> operation
                    let rawTransfer =
                        match original, operation with
                        | (_, RawSlotInit (_, _, Var source, _)), RawWriteWord _ ->
                            ownerOf definitions source |> Option.toList |> Set.ofList
                        | _ -> Set.empty
                    let recursiveFrontierCandidates =
                        match operation with
                        | Call (target, args) when target = func.Id ->
                            List.zip func.TypedParams args
                            |> List.choose (fun (parameter, argument) ->
                                if Set.contains parameter.Id frontierParams then
                                    match argument with
                                    | Var source -> ownerOf definitions source
                                    | _ -> None
                                else None)
                            |> Set.ofList
                        | _ -> Set.empty
                    let usedOwners =
                        ANFEffects.cexprTempUses operation
                        |> liveOwners definitions
                    // A live cleanup after a recursive call prevents tail-call
                    // conversion. In that case the callee retains its arguments,
                    // so the caller must release its own frontier values.
                    let recursiveFrontierTransfer =
                        let otherDeadOwners =
                            Set.difference usedOwners neededOwners
                            |> fun owners -> Set.difference owners recursiveFrontierCandidates
                            |> fun owners -> Set.difference owners transferredOwners
                            |> fun owners -> Set.difference owners rawTransfer
                        if Set.isEmpty otherDeadOwners then recursiveFrontierCandidates
                        else Set.empty
                    let transferredOwners =
                        Set.unionMany [transferredOwners; rawTransfer; recursiveFrontierTransfer]
                    let deadOwners =
                        Set.difference usedOwners neededOwners
                        |> fun owners -> Set.difference owners transferredOwners
                    let deadOwners =
                        if Set.contains id definitions.Owned
                           && not (Set.contains id neededOwners)
                           && not (Set.contains id transferredOwners) then
                            Set.add id deadOwners
                        else deadOwners
                    // Source lifetime continues through the block's remaining
                    // effects. Finalizers and reference-count inspection can
                    // observe whether cleanup precedes those effects.
                    let deferredHere = if isSelfRecursive then Set.empty else deadOwners
                    let deadOwners = Set.difference deadOwners deferredHere
                    let borrowedResultRetain, next, types =
                        match operation with
                        | BorrowedCall _ | IfValue _ when Set.contains id definitions.Owned ->
                            let binding, after, (newId, typ) = retain ctx definitions id next
                            [binding], after, Map.add newId typ types
                        | _ -> [], next, types
                    let releases, next, types =
                        deadOwners
                        |> Set.fold (fun (bindings, current, types) owner ->
                            let binding, after, (newId, typ) = release ctx definitions owner current
                            binding :: bindings, after, Map.add newId typ types)
                            ([], next, types)
                    operations
                    @ captureBindings
                    @ reuseCleanupBindings
                    @ ((id, operation) :: borrowedResultRetain)
                    @ List.rev releases,
                    next,
                    types,
                    Set.union deferred deferredHere)
                    ([], next, types, Set.empty)
            let operations, next, types =
                deferred
                |> Set.toList
                |> List.sortByDescending (fun (TempId id) -> id)
                |> List.fold (fun (operations, current, types) owner ->
                    let binding, after, (newId, typ) = release ctx definitions owner current
                    operations @ [binding], after, Map.add newId typ types)
                    (operations, next, types)
            Map.add label { block with Operations = operations } blocks, next, types)
            (Map.empty, initial, func.FreshValueTypes)
    let blocks, next, types =
        match Map.tryFind func.Entry func.Blocks, Map.tryFind func.Entry blocks with
        | Some originalEntry, Some entryBlock when not isSelfRecursive ->
            match entryBlock.Terminator with
            | SSAANF.Branch _ ->
                let entryOwners =
                    originalEntry.Operations
                    |> List.map fst
                    |> Set.ofList
                    |> Set.intersect definitions.Owned
                let moveToReturns =
                    entryBlock.Operations
                    |> List.choose (fun (_, operation) -> releaseSource operation)
                    |> Set.ofList
                    |> Set.intersect entryOwners
                let entryBlock =
                    { entryBlock with
                        Operations =
                            entryBlock.Operations
                            |> List.filter (fun (_, operation) ->
                                releaseSource operation
                                |> Option.exists (fun id -> Set.contains id moveToReturns)
                                |> not) }
                let blocks = Map.add func.Entry entryBlock blocks
                blocks
                |> Map.fold (fun (blocks, next, types) label block ->
                    match block.Terminator with
                    | SSAANF.Return _ ->
                        let operations, next, types =
                            moveToReturns
                            |> Set.toList
                            |> List.sortByDescending (fun (TempId id) -> id)
                            |> List.fold (fun (operations, current, types) owner ->
                                let binding, after, (newId, typ) = release ctx definitions owner current
                                operations @ [binding], after, Map.add newId typ types)
                                (block.Operations, next, types)
                        Map.add label { block with Operations = operations } blocks, next, types
                    | _ -> blocks, next, types)
                    (blocks, next, types)
            | _ -> blocks, next, types
        | _ -> blocks, next, types
    let appendBindings
        (operations: (TempId * CExpr) list)
        (owners: Set<TempId>)
        (next: VarGen)
        (types: Map<TempId, AST.SemanticType>) =
        owners
        |> Set.fold (fun (operations, current, types) owner ->
            let binding, after, (newId, typ) = release ctx definitions owner current
            operations @ [binding], after, Map.add newId typ types)
            (operations, next, types)
    let finishBlock
        ((blocks: Map<SSAANF.Label, SSAANF.Block>), next, types)
        label
        (_: SSAANF.Block) =
        let block =
            match Map.tryFind label blocks with
            | Some block -> block
            | None -> Crash.crash "SSA RC: block disappeared during cleanup"
        match block.Terminator with
        | SSAANF.Return (Var id) ->
            let returningOwner = ownerOf definitions id
            let needsRetain =
                match Map.tryFind id definitions.Types with
                | Some typ ->
                    rcShapeForType ctx typ |> rcShapeNeedsBorrowedRetain
                    && Option.isNone returningOwner
                | None -> false
            let withReturnRetain, afterRetain, types =
                if needsRetain then
                    let binding, after, (newId, typ) = retain ctx definitions id next
                    block.Operations @ [binding], after, Map.add newId typ types
                else block.Operations, next, types
            let live =
                Map.tryFind label liveness.AtTerminator
                |> Option.defaultValue Set.empty
                |> liveOwners definitions
            let releaseOwners =
                returningOwner
                |> Option.map (fun owner -> Set.remove owner live)
                |> Option.defaultValue live
            let operations, next, types =
                appendBindings withReturnRetain releaseOwners afterRetain types
            Map.add label { block with Operations = operations } blocks, next, types
        | SSAANF.Return _ ->
            Map.add label block blocks, next, types
        | SSAANF.Jump (successor, arguments) ->
            let target =
                match Map.tryFind successor blocks with
                | Some target -> target
                | None -> Crash.crash "SSA RC: missing jump successor"
            if List.length target.Parameters <> List.length arguments then
                Crash.crash "SSA RC: jump argument count does not match parameters"
            let bindings, next, types, transferred =
                List.zip target.Parameters arguments
                |> List.fold (fun (bindings, next, types, transferred) (parameter, argument) ->
                    if not (Set.contains parameter.Id definitions.Owned) then
                        bindings, next, types, transferred
                    else
                        match argument with
                        | Var source when not (isNonRcSentinel definitions source) ->
                            match ownerOf definitions source with
                            | Some owner when not (Set.contains owner transferred) ->
                                bindings, next, types, Set.add owner transferred
                            | _ ->
                                let binding, after, (id, typ) = retain ctx definitions source next
                                bindings @ [binding], after, Map.add id typ types, transferred
                        | _ -> bindings, next, types, transferred)
                    ([], next, types, Set.empty)
            let before =
                Map.tryFind label liveness.AtTerminator
                |> Option.defaultValue Set.empty
                |> liveOwners definitions
            let immediateScalar = function
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128
                | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
                | AST.TInt | AST.TBool | AST.TFloat64 | AST.TChar
                | AST.TDateTime | AST.TUnit -> true
                | _ -> false
            let borrowedOwners =
                List.zip target.Parameters arguments
                |> List.fold (fun owners (parameter, argument) ->
                    match argument with
                    | Var source when immediateScalar parameter.Type ->
                        Set.union owners (liveOwners definitions (Set.singleton source))
                    | Var source
                        when Set.contains parameter.Id definitions.Owned
                             && Option.isNone (ownerOf definitions source)
                             && not (isNonRcSentinel definitions source) ->
                        // The successor owns a retained borrowed projection.
                        // Its aggregate source no longer needs to survive the edge.
                        Set.union owners (liveOwners definitions (Set.singleton source))
                    | _ -> owners) Set.empty
            let required =
                Map.tryFind successor liveness.AtEntry
                |> Option.defaultValue Set.empty
                |> liveOwners definitions
            let releaseOwners =
                Set.difference (Set.union before borrowedOwners) required
                |> fun owners -> Set.difference owners transferred
            let operations, next, types =
                appendBindings (block.Operations @ bindings) releaseOwners next types
            Map.add label { block with Operations = operations } blocks,
            next,
            types
        | SSAANF.Branch (_, yes, no) ->
            let before =
                Map.tryFind label liveness.AtTerminator
                |> Option.defaultValue Set.empty
                |> liveOwners definitions
            let releaseOnEdge successor =
                let required =
                    Map.tryFind successor liveness.AtEntry
                    |> Option.defaultValue Set.empty
                    |> liveOwners definitions
                Set.difference before required
            let blocks = Map.add label block blocks
            let addEntryRelease
                ((blocks: Map<SSAANF.Label, SSAANF.Block>), next, types)
                successor
                owners =
                match Map.tryFind successor blocks with
                | None -> Crash.crash "SSA RC: missing branch successor"
                | Some successorBlock ->
                    let bindings, next, types = appendBindings [] owners next types
                    Map.add successor
                        { successorBlock with Operations = bindings @ successorBlock.Operations }
                        blocks,
                    next,
                    types
            let blocks, next, types = addEntryRelease (blocks, next, types) yes (releaseOnEdge yes)
            addEntryRelease (blocks, next, types) no (releaseOnEdge no)
    let blocks, next, types =
        blocks |> Map.fold finishBlock (blocks, next, types)
    let entry =
        match Map.tryFind func.Entry blocks with
        | Some block -> block
        | None -> Crash.crash "SSA RC: missing entry block"
    let entryRetains, _, types =
        definitions.OwnedParams
        |> Set.fold (fun (bindings, current, types) parameter ->
            let binding, after, (id, typ) = retain ctx definitions parameter current
            binding :: bindings, after, Map.add id typ types)
            ([], next, types)
    let blocks =
        Map.add func.Entry
            { entry with Operations = List.rev entryRetains @ entry.Operations }
            blocks
    { func with Blocks = blocks; FreshValueTypes = types }
