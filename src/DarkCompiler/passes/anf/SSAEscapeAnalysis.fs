// SSAEscapeAnalysis.fs - Scalar replacement and unique fixed-block reuse on SSA ANF.

module SSAEscapeAnalysis

open ANF

type private Aggregate = { Fields: Atom list }

let private atomUses id atom = ANFEffects.atomsUseTemp id [atom]
let private operationUses id operation = ANFEffects.cexprUsesTemp id operation

let private terminatorUses id = function
    | SSAANF.Return atom -> atomUses id atom
    | SSAANF.Jump (_, arguments) -> ANFEffects.atomsUseTemp id arguments
    | SSAANF.Branch (condition, _, _) -> atomUses id condition

let private allOperations (func: SSAANF.Function) =
    func.Blocks
    |> Map.toList
    |> List.collect (fun (_, block) -> block.Operations)

let private scalarAtom (types: TypeMap) = function
    | UnitLiteral | IntLiteral _ | BoolLiteral _ | FloatLiteral _ -> true
    | Var id ->
        Map.tryFind id types
        |> Option.map EscapeAnalysisFacts.isScalarType
        |> Option.defaultValue false
    | StringLiteral _ | FuncRef _ -> false

let private scalarAggregate types = function
    | TupleAlloc fields when List.forall (scalarAtom types) fields ->
        Some { Fields = fields }
    | RecordAlloc (descriptor, fields)
    | RecordClone (descriptor, _, fields)
        when List.length descriptor.Fields = List.length fields
             && List.forall (snd >> EscapeAnalysisFacts.isScalarType) descriptor.Fields
             && List.forall (scalarAtom types) fields ->
        Some { Fields = fields }
    | _ -> None

let private aliasesOf (operations: (TempId * CExpr) list) source =
    let rec collect known =
        let next =
            operations
            |> List.fold (fun current (id, operation) ->
                match operation with
                | Atom (Var origin) | TypedAtom (Var origin, _)
                    when Set.contains origin current -> Set.add id current
                | _ -> current) known
        if next = known then known else collect next
    collect (Set.singleton source)

let private aggregateHasOnlyLocalUses
    (func: SSAANF.Function)
    (operations: (TempId * CExpr) list)
    source =
    let tracked = aliasesOf operations source
    let validOperation (id, operation) =
        if id = source then true
        else
            let uses = Set.exists (fun trackedId -> operationUses trackedId operation) tracked
            if not uses then true
            else
                match operation with
                | Atom (Var origin) | TypedAtom (Var origin, _)
                    when Set.contains origin tracked -> true
                | TupleGet (Var origin, _)
                | RecordGet (_, Var origin, _)
                    when Set.contains origin tracked -> true
                | RecordClone (_, Var origin, fields)
                    when Set.contains origin tracked ->
                    not (Set.exists (fun trackedId ->
                        ANFEffects.atomsUseTemp trackedId fields) tracked)
                | _ -> false
    List.forall validOperation operations
    && func.Blocks
       |> Map.forall (fun _ block ->
           not (Set.exists (fun id -> terminatorUses id block.Terminator) tracked))

let private tryField index (aggregate: Aggregate) =
    match List.tryItem index aggregate.Fields with
    | Some field -> field
    | None -> Crash.crash $"SSA escape analysis: projection index {index} is out of bounds"

let private replaceScalars (func: SSAANF.Function) =
    let operations = allOperations func
    let aggregates =
        operations
        |> List.choose (fun (id, operation) ->
            scalarAggregate func.FreshValueTypes operation
            |> Option.bind (fun aggregate ->
                if aggregateHasOnlyLocalUses func operations id then
                    Some (id, aggregate)
                else None))
        |> Map.ofList
    let aliases =
        aggregates
        |> Map.fold (fun known source aggregate ->
            aliasesOf operations source
            |> Set.fold (fun current id -> Map.add id aggregate current) known) Map.empty
    let rewrite (id, operation) =
        if Map.containsKey id aliases then None
        else
            let replacement =
                match operation with
                | TupleGet (Var source, index)
                | RecordGet (_, Var source, index) ->
                    Map.tryFind source aliases
                    |> Option.map (tryField index >> Atom)
                | RecordClone (descriptor, Var source, fields)
                    when Map.containsKey source aliases ->
                    Some (RecordAlloc (descriptor, fields))
                | _ -> None
            Some (id, Option.defaultValue operation replacement)
    { func with
        Blocks =
            func.Blocks
            |> Map.map (fun _ block ->
                { block with Operations = List.choose rewrite block.Operations }) }

let private usesAny ids operation =
    Set.exists (fun id -> operationUses id operation) ids

let private reusesInBlock
    (typeReg: TypeRegistries.TypeRegistry)
    (sumReg: MemoryModel.RcSumShapeRegistry)
    (func: SSAANF.Function)
    (block: SSAANF.Block) =
    // The ANF reuse rule stops at a branch or join. The matching SSA scope is
    // one block, with a whole-function use check to rule out escaping aliases.
    let rec visit processedRev remaining =
        match remaining with
        | [] -> List.rev processedRev
        | (sourceId, sourceOperation) :: tail ->
            let descriptor =
                match sourceOperation with
                | RecordAlloc (descriptor, _)
                | RecordClone (descriptor, _, _)
                | RecordReuse (_, descriptor, _, _) -> Some descriptor
                | _ -> None
            let replacement =
                descriptor
                |> Option.bind (fun sourceDescriptor ->
                    if not (EscapeAnalysisFacts.descriptorHasNonObservableDestruction
                                typeReg sumReg sourceDescriptor) then None
                    else
                        let isSum =
                            match sourceDescriptor.ValueType with
                            | AST.TSum _ -> true
                            | _ -> false
                        let rec scan index tracked projections remaining =
                            match remaining with
                            | [] -> None
                            | (candidateId, candidate) :: later ->
                                let matchingTarget =
                                    match candidate with
                                    | RecordClone (target, Var origin, fields)
                                        when not isSum
                                             && target = sourceDescriptor
                                             && Set.contains origin tracked
                                             && not (Set.exists (fun id ->
                                                 ANFEffects.atomsUseTemp id fields) tracked)
                                        -> Some (target, Var origin, fields)
                                    | RecordAlloc (target, fields)
                                        when isSum
                                             && target.ValueType = sourceDescriptor.ValueType
                                             && List.length target.Fields = List.length sourceDescriptor.Fields
                                             && List.forall
                                                 (snd >> EscapeAnalysisFacts.hasNonObservableDestruction
                                                             typeReg sumReg true)
                                                 target.Fields
                                             && not (usesAny tracked candidate) ->
                                        Some (target, Var sourceId, fields)
                                    | _ -> None
                                match matchingTarget with
                                | Some (target, origin, fields) ->
                                    let escaped =
                                        func.Blocks
                                        |> Map.exists (fun label other ->
                                            label <> block.Label
                                            && (other.Operations
                                                |> List.exists (snd >> usesAny
                                                    (Set.union tracked projections))
                                                || Set.exists
                                                    (fun id -> terminatorUses id other.Terminator)
                                                    (Set.union tracked projections)))
                                    let liveAfter =
                                        List.exists (snd >> usesAny tracked) later
                                        || List.exists (snd >> usesAny projections) later
                                        || Set.exists (fun id -> terminatorUses id block.Terminator)
                                            (Set.union tracked projections)
                                        || escaped
                                    if liveAfter then None
                                    else Some (index, RecordReuse (sourceDescriptor, target, origin, fields))
                                | None ->
                                    match candidate with
                                    | Atom (Var origin) | TypedAtom (Var origin, _)
                                        when Set.contains origin tracked ->
                                        scan (index + 1) (Set.add candidateId tracked) projections later
                                    | Atom (Var origin) | TypedAtom (Var origin, _)
                                        when Set.contains origin projections ->
                                        scan (index + 1) tracked (Set.add candidateId projections) later
                                    | TupleGet (Var origin, _)
                                    | RecordGet (_, Var origin, _)
                                        when Set.contains origin tracked ->
                                        scan (index + 1) tracked (Set.add candidateId projections) later
                                    | _ when not (usesAny tracked candidate) ->
                                        scan (index + 1) tracked projections later
                                    | _ -> None
                        scan 0 (Set.singleton sourceId) Set.empty tail)
            let tail' =
                match replacement with
                | None -> tail
                | Some (targetIndex, operation) ->
                    tail
                    |> List.mapi (fun index (id, current) ->
                        if index = targetIndex then id, operation else id, current)
            visit ((sourceId, sourceOperation) :: processedRev) tail'
    { block with Operations = visit [] block.Operations }

let optimizeFunction typeReg sumReg func =
    let scalarReplaced = replaceScalars func
    { scalarReplaced with
        Blocks =
            scalarReplaced.Blocks
            |> Map.map (fun _ block -> reusesInBlock typeReg sumReg scalarReplaced block) }
