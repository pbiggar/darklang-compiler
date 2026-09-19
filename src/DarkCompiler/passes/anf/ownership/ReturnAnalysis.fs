// ReturnAnalysis.fs - Analyze aliases and ownership escaping through returns and aggregates.

module RcReturnAnalysis

open MemoryModel
open ANF
open LiftExpressions
open LiftFunctions
open LoweringExpressions

type ReturnAnnotatedExpr =
    | RReturn of Atom * Set<TempId>
    | RLet of TempId * CExpr * ReturnAnnotatedExpr * Set<TempId>
    | RIf of Atom * ReturnAnnotatedExpr * ReturnAnnotatedExpr * Set<TempId>
    | RJoin of TypedParam * ReturnAnnotatedExpr * ReturnAnnotatedExpr * Set<TempId>
    | RJump of TempId * Atom * Set<TempId>

/// Get the set of returned TempIds for a return-annotated expression
let returnedSet (expr: ReturnAnnotatedExpr) : Set<TempId> =
    match expr with
    | RReturn (_, returned) -> returned
    | RLet (_, _, _, returned) -> returned
    | RIf (_, _, _, returned) -> returned
    | RJoin (_, _, _, returned) -> returned
    | RJump (_, _, returned) -> returned

/// Collect the alias chain for a TempId (includes the TempId itself)
let rec collectAliasChain (aliases: Map<TempId, TempId>) (tempId: TempId) : Set<TempId> =
    match Map.tryFind tempId aliases with
    | Some nextId -> Set.add tempId (collectAliasChain aliases nextId)
    | None -> Set.singleton tempId

/// Most TypedAtoms only preserve type information for an existing value. A
/// RawPtr-to-Stream TypedAtom is different: Stream construction seeds its RC
/// word at zero and relies on the first owning use to materialize an edge.
/// Following that cast back to the RawPtr would invent ownership that does not
/// yet exist and incorrectly remove the required retain.
let internal tryOwnershipPreservingAliasSource (cexpr: CExpr) : TempId option =
    match cexpr with
    | Atom (Var sourceId) -> Some sourceId
    | TypedAtom (Var _, AST.TStream _) -> None
    | TypedAtom (Var sourceId, _) -> Some sourceId
    | RecordReuse (_, Var sourceId, _) -> Some sourceId
    | _ -> None

/// Analyze return values and track alias chains in a single pass
let rec analyzeReturns
    (joins: Map<TempId, Set<TempId>>)
    (aliases: Map<TempId, TempId>)
    (expr: AExpr)
    : ReturnAnnotatedExpr =
    match expr with
    | Jump (target, atom) ->
        let returned =
            match Map.tryFind target joins with
            | None -> Crash.crash $"Return analysis: join target {target} is not in scope"
            | Some returned when Set.contains target returned ->
                let argumentAliases = match atom with Var id -> collectAliasChain aliases id | _ -> Set.empty
                Set.union (Set.remove target returned) argumentAliases
            | Some returned -> returned
        RJump (target, atom, returned)
    | Join (parameter, continuation, entry) ->
        let body = analyzeReturns joins (Map.remove parameter.Id aliases) continuation
        let entryInfo = analyzeReturns (Map.add parameter.Id (returnedSet body) joins) aliases entry
        RJoin (parameter, body, entryInfo, returnedSet entryInfo)
    | Return atom ->
        let returned =
            match atom with
            | Var tid -> collectAliasChain aliases tid
            | _ -> Set.empty
        RReturn (atom, returned)
    | Let (tempId, cexpr, body) ->
        let aliases' =
            match tryOwnershipPreservingAliasSource cexpr with
            | Some sourceId -> Map.add tempId sourceId aliases
            | _ -> aliases
        let bodyInfo = analyzeReturns joins aliases' body
        RLet (tempId, cexpr, bodyInfo, returnedSet bodyInfo)
    | If (cond, thenBranch, elseBranch) ->
        let thenInfo = analyzeReturns joins aliases thenBranch
        let elseInfo = analyzeReturns joins aliases elseBranch
        let returned = Set.union (returnedSet thenInfo) (returnedSet elseInfo)
        RIf (cond, thenInfo, elseInfo, returned)

let private atomOccurrenceCount (target: TempId) (atoms: Atom list) : int =
    atoms
    |> List.sumBy (fun atom ->
        match atom with
        | Var tempId when tempId = target -> 1
        | _ -> 0)

/// Count ownership-bearing aggregate fields drawn from an alias family.
/// RecordClone's source record is borrowed by the clone operation, so it cannot
/// be the ownership-transfer use.
let private aggregateAliasFieldCount
    (aliases: Set<TempId>)
    (cexpr: CExpr)
    : int =
    let countFields (fields: Atom list) : int =
        aliases
        |> Set.toList
        |> List.sumBy (fun target -> atomOccurrenceCount target fields)

    match cexpr with
    | TupleAlloc elements
    | RecordAlloc (_, elements)
    | ClosureAlloc (_, elements) ->
        countFields elements
    | RecordClone (_, source, fields) ->
        let sourceUsesAlias =
            aliases
            |> Set.exists (fun target -> ANFEffects.atomUsesTemp target source)
        if sourceUsesAlias then 0 else countFields fields
    | _ ->
        0

let private rawSlotAliasValueCount
    (aliases: Set<TempId>)
    (cexpr: CExpr)
    : int =
    match cexpr with
    | RawSlotInit (_, _, Var valueId, _) when Set.contains valueId aliases -> 1
    | _ -> 0

let rec private returnAnnotatedExprUsesAnyAlias
    (aliases: Set<TempId>)
    (body: ReturnAnnotatedExpr)
    : bool =
    let atomUsesAnyAlias atom =
        aliases
        |> Set.exists (fun target -> ANFEffects.atomUsesTemp target atom)

    match body with
    | RReturn (atom, _) -> atomUsesAnyAlias atom
    | RJump (_, atom, _) -> atomUsesAnyAlias atom
    | RJoin (parameter, continuation, entry, _) ->
        returnAnnotatedExprUsesAnyAlias (Set.remove parameter.Id aliases) continuation
        || returnAnnotatedExprUsesAnyAlias aliases entry
    | RLet (_, cexpr, nextBody, _) ->
        (aliases |> Set.exists (fun target -> ANFEffects.cexprUsesTemp target cexpr))
        || returnAnnotatedExprUsesAnyAlias aliases nextBody
    | RIf (cond, thenBranch, elseBranch, _) ->
        atomUsesAnyAlias cond
        || returnAnnotatedExprUsesAnyAlias aliases thenBranch
        || returnAnnotatedExprUsesAnyAlias aliases elseBranch

let private cexprReleasesAnyAlias (aliases: Set<TempId>) (cexpr: CExpr) : bool =
    match cexpr with
    | RefCountDec (atom, _, _, _)
    | RefCountDecString atom
    | RefCountDecBlob atom
    | RefCountDecInt atom ->
        aliases
        |> Set.exists (fun target -> ANFEffects.atomUsesTemp target atom)
    | _ ->
        false

let private terminalPrintConsumesReturnedAlias
    (aliases: Set<TempId>)
    (cexpr: CExpr)
    (nextBody: ReturnAnnotatedExpr)
    : bool =
    match cexpr, nextBody with
    | Print (atom, _), RReturn (Var returnedId, _)
        when Set.contains returnedId aliases ->
        aliases
        |> Set.exists (fun target -> ANFEffects.atomUsesTemp target atom)
    | _ ->
        false

/// Once ownership is transferred into an aggregate, do not permit observable
/// work before that aggregate is returned. This preserves internal refcount
/// probes while allowing nested tuple/record/closure construction suffixes.
let rec private aggregateFlowsDirectlyToReturn
    (aggregateId: TempId)
    (body: ReturnAnnotatedExpr)
    : bool =
    let rec loop (aliases: Set<TempId>) (body: ReturnAnnotatedExpr) : bool =
        match body with
        | RReturn (Var returnedId, _) ->
            Set.contains returnedId aliases
        | RLet (nextId, nextExpr, nextBody, _) ->
            match tryOwnershipPreservingAliasSource nextExpr with
            | Some sourceId when Set.contains sourceId aliases ->
                loop (Set.add nextId aliases) nextBody
            | _ when terminalPrintConsumesReturnedAlias aliases nextExpr nextBody ->
                loop aliases nextBody
            | _ ->
                aggregateAliasFieldCount aliases nextExpr > 0
                && aggregateFlowsDirectlyToReturn nextId nextBody
        | RIf (_, thenBranch, elseBranch, _) ->
            loop aliases thenBranch && loop aliases elseBranch
        | RJoin _ | RJump _ -> false
        | RReturn _ ->
            false

    loop (Set.singleton aggregateId) body

/// Find the aggregate use that begins a closed construction suffix ending in
/// Return. Earlier borrowed uses preserve the candidate's owned edge; an
/// explicit release makes the proof ineligible.
let rec internal transfersIntoReturnedAggregate
    (candidateId: TempId)
    (body: ReturnAnnotatedExpr)
    : bool =
    let rec loop (aliases: Set<TempId>) (body: ReturnAnnotatedExpr) : bool =
        match body with
        | RLet (aggregateId, cexpr, nextBody, _) ->
            match tryOwnershipPreservingAliasSource cexpr with
            | Some sourceId when Set.contains sourceId aliases ->
                loop (Set.add aggregateId aliases) nextBody
            | _ ->
                if aggregateAliasFieldCount aliases cexpr > 0 then
                    aggregateFlowsDirectlyToReturn aggregateId nextBody
                elif rawSlotAliasValueCount aliases cexpr > 0 then
                    false
                elif cexprReleasesAnyAlias aliases cexpr then
                    false
                else
                    loop aliases nextBody
        | RIf (_, thenBranch, elseBranch, _) ->
            loop aliases thenBranch && loop aliases elseBranch
        | RJoin _ | RJump _ -> false
        | RReturn _ ->
            false

    loop (Set.singleton candidateId) body

/// Move a locally-owned edge into its first ownership-bearing raw slot use
/// when no alias remains live afterward. RawSlotInit normally retains a copied
/// edge; the move instead lets the slot adopt the binding's pending ownership.
let rec internal transfersIntoRawSlot
    (candidateId: TempId)
    (body: ReturnAnnotatedExpr)
    : bool =
    let rec loop (aliases: Set<TempId>) (body: ReturnAnnotatedExpr) : bool =
        match body with
        | RLet (nextId, cexpr, nextBody, _) ->
            match tryOwnershipPreservingAliasSource cexpr with
            | Some sourceId when Set.contains sourceId aliases ->
                loop (Set.add nextId aliases) nextBody
            | _ ->
                let rawSlotUses = rawSlotAliasValueCount aliases cexpr
                if rawSlotUses > 0 then
                    rawSlotUses = 1
                    && not (returnAnnotatedExprUsesAnyAlias aliases nextBody)
                elif aggregateAliasFieldCount aliases cexpr > 0
                     || cexprReleasesAnyAlias aliases cexpr then
                    false
                else
                    loop aliases nextBody
        | RIf (_, thenBranch, elseBranch, _) ->
            loop aliases thenBranch && loop aliases elseBranch
        | RJoin _ | RJump _ -> false
        | RReturn _ ->
            false

    loop (Set.singleton candidateId) body

/// Check if a CExpr is a borrowing/aliasing operation
/// Borrowed/aliased values should NOT get their own RefCountDec - the original value owns the memory
let isBorrowingExpr (cexpr: CExpr) : bool =
    match cexpr with
    | IfValue _ -> true            // Selects one of two existing values; no ownership transfer
    | TupleGet _ -> true           // Extracts pointer from tuple/list - borrowed from parent
    | RecordGet _ -> true          // Record projections borrow from the owning record
    | RecordReuse _ -> true        // Reuses the source allocation and transfers its ownership
    | RawGet _ -> true             // RawGet reads existing memory; it does not transfer ownership
    | RawTake _ -> false           // RawTake transfers the slot's existing ownership to the result
    | StringToRawPtr _ -> true     // RawPtr view is borrowed from the dynamic buffer
    | BlobToRawPtr _ -> true      // RawPtr view is borrowed from the dynamic buffer
    | DictToRawPtr _ -> true       // RawPtr view is borrowed from the tagged container
    | ListToRawPtr _ -> true       // RawPtr view is borrowed from the tagged container
    | FixedBlockToRawPtr _ -> true // RawPtr view is borrowed from the fixed block
    | BorrowedCall _ -> true       // Callee returns an alias kept alive by one of its arguments
    | Atom (Var _) -> true         // Alias/copy of existing variable - don't double-dec
    | TypedAtom (Var _, _) -> true // TypedAtom wrapping a variable - also borrowed
    | _ -> false
