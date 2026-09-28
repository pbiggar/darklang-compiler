// SSAInlining.fs - Inline eligible typed SSA functions at direct call sites.
//
// Each copied value receives a fresh identity. Single-block callees stay in
// the caller's block so ownership cleanup can follow the remaining effects.
// Multi-block callees use a typed continuation. Small scalar Option projections
// may copy that continuation at each return to expose branch-local values.

module SSAInlining

open ANF

type private Candidate = {
    Info: InliningCommon.FunctionInfo
    Body: SSAANF.Function
}

type private State = {
    Function: SSAANF.Function
    NextValue: int
    NextLabel: int
    Processed: Set<TempId>
    Depths: Map<TempId, int>
}

let private nextValue (state: State) typ =
    let id = TempId state.NextValue
    id,
    { state with
        NextValue = state.NextValue + 1
        Function =
            { state.Function with
                FreshValueTypes = Map.add id typ state.Function.FreshValueTypes } }

let private nextLabel (state: State) =
    let label = SSAANF.Label state.NextLabel
    label, { state with NextLabel = state.NextLabel + 1 }

let private valueType (func: SSAANF.Function) id =
    match Map.tryFind id func.FreshValueTypes with
    | Some typ -> typ
    | None -> Crash.crash $"SSA inlining lost the type of {id} in '{func.Name}'"

let private required key values =
    match Map.tryFind key values with
    | Some value -> value
    | None -> Crash.crash "SSA inlining lost a verified mapping"

let private renameAtom mapping atom = InliningCommon.renameAtom mapping atom

let private renameTerminator labels mapping continuation = function
    | SSAANF.Return atom ->
        SSAANF.Jump (continuation, [renameAtom mapping atom])
    | SSAANF.Jump (target, arguments) ->
        let target' =
            match Map.tryFind target labels with
            | Some label -> label
            | None -> Crash.crash "SSA inlining lost a callee jump target"
        SSAANF.Jump (target', List.map (renameAtom mapping) arguments)
    | SSAANF.Branch (condition, yes, no) ->
        let label old =
            match Map.tryFind old labels with
            | Some value -> value
            | None -> Crash.crash "SSA inlining lost a callee branch target"
        SSAANF.Branch (renameAtom mapping condition, label yes, label no)

let private maxValue (func: SSAANF.Function) =
    func.FreshValueTypes
    |> Map.keys
    |> Seq.map (fun (TempId id) -> id)
    |> Seq.fold max 4000

let private maxLabel (func: SSAANF.Function) =
    func.Blocks
    |> Map.keys
    |> Seq.map (fun (SSAANF.Label id) -> id)
    |> Seq.fold max 0

let private initialState func =
    { Function = func
      NextValue = maxValue func + 1
      NextLabel = maxLabel func + 1
      Processed = Set.empty
      Depths = Map.empty }

let private splitAtOperation index operations =
    operations
    |> List.indexed
    |> List.fold (fun (before, at, after) (current, operation) ->
        if current < index then operation :: before, at, after
        elif current = index then before, Some operation, after
        else before, at, operation :: after) ([], None, [])
    |> fun (before, at, after) -> List.rev before, at, List.rev after

type private BoundedLoop = {
    Parameters: TempId list
    InductionIndex: int
    Bound: int64
    Exit: Atom
    Iteration: (TempId * CExpr) list
    RecursiveArguments: Atom list
}

let private boundedPrimitive = function
    | Atom _ | TypedAtom (_, AST.TInt64) | Prim _ | UnaryPrim _ -> true
    | _ -> false

let private tryBoundedLoop (candidate: Candidate) =
    let source = candidate.Info.Func
    let rec iteration accumulated = function
        | Let (id, Call (name, arguments), Return (Var returned))
            when name = source.Id && id = returned ->
            Some (List.rev accumulated, arguments)
        | Let (id, operation, rest) when boundedPrimitive operation ->
            iteration ((id, operation) :: accumulated) rest
        | _ -> None
    match source.TypedParams |> List.forall (fun param -> param.Type = AST.TInt64),
          source.ReturnType, source.Body with
    | true, AST.TInt64,
      Let (guard, Prim (Gte, Var induction, IntLiteral (Int64 bound)),
           If (Var condition, Return exit, body)) when guard = condition ->
        let parameters = source.TypedParams |> List.map (fun param -> param.Id)
        match List.tryFindIndex ((=) induction) parameters, iteration [] body with
        | Some index, Some (bindings, arguments)
            when List.length arguments = List.length parameters
                 && (match exit with
                     | Var id -> List.contains id parameters
                     | IntLiteral _ -> true
                     | _ -> false) ->
            let advances =
                match List.item index arguments with
                | Var next ->
                    bindings
                    |> List.exists (fun (id, operation) ->
                        id = next && operation = Prim (Add, Var induction, IntLiteral (Int64 1L)))
                | _ -> false
            if advances then
                Some { Parameters = parameters; InductionIndex = index; Bound = bound
                       Exit = exit; Iteration = bindings; RecursiveArguments = arguments }
            else None
        | _ -> None
    | _ -> None

let private boundedTripCount maximum start bound =
    let rec count current completed =
        if current >= bound then Some completed
        elif completed = maximum || current = System.Int64.MaxValue then None
        else count (current + 1L) (completed + 1)
    count start 0

let private substituteAtom (mapping: Map<TempId, Atom>) = function
    | Var id as atom -> Map.tryFind id mapping |> Option.defaultValue atom
    | atom -> atom

let private substituteBounded mapping operation =
    let value = substituteAtom mapping
    match operation with
    | Atom atom -> Atom (value atom)
    | TypedAtom (atom, typ) -> TypedAtom (value atom, typ)
    | Prim (op, left, right) -> Prim (op, value left, value right)
    | UnaryPrim (op, atom) -> UnaryPrim (op, value atom)
    | _ -> Crash.crash "SSA inlining encountered an unsupported bounded-loop operation"

let private renameCallerTerminator labels mapping = function
    | SSAANF.Return atom -> SSAANF.Return (renameAtom mapping atom)
    | SSAANF.Jump (label, arguments) ->
        SSAANF.Jump (Map.tryFind label labels |> Option.defaultValue label,
                     List.map (renameAtom mapping) arguments)
    | SSAANF.Branch (condition, yes, no) ->
        SSAANF.Branch (renameAtom mapping condition,
                       Map.tryFind yes labels |> Option.defaultValue yes,
                       Map.tryFind no labels |> Option.defaultValue no)

let private terminatorUses id = function
    | SSAANF.Return atom -> ANFEffects.atomsUseTemp id [atom]
    | SSAANF.Jump (_, arguments) -> ANFEffects.atomsUseTemp id arguments
    | SSAANF.Branch (condition, _, _) -> ANFEffects.atomsUseTemp id [condition]

let private successors = function
    | SSAANF.Return _ -> []
    | SSAANF.Jump (label, _) -> [label]
    | SSAANF.Branch (_, yes, no) -> [yes; no]

let private reachable (func: SSAANF.Function) excluded start =
    let rec visit pending seen =
        match pending with
        | [] -> seen
        | label :: rest when Set.contains label seen || Some label = excluded ->
            visit rest seen
        | label :: rest ->
            match Map.tryFind label func.Blocks with
            | Some block ->
                visit (successors block.Terminator @ rest) (Set.add label seen)
            | None -> Crash.crash "SSA inlining found a missing caller block"
    visit [start] Set.empty

let private continuationRegion
    (func: SSAANF.Function)
    (block: SSAANF.Block)
    resultId
    after
    returns =
    let fromSource = reachable func None block.Label
    let withoutSource = reachable func (Some block.Label) func.Entry
    let region =
        Set.difference fromSource withoutSource
        |> Set.remove block.Label
    let regionBlocks =
        region |> Set.toList |> List.map (fun label -> required label func.Blocks)
    let definitions =
        resultId :: (after |> List.map fst)
        @ (regionBlocks
           |> List.collect (fun body ->
               (body.Parameters |> List.map (fun parameter -> parameter.Id))
               @ (body.Operations |> List.map fst)))
        |> Set.ofList
    let usedOutside =
        func.Blocks
        |> Map.exists (fun label body ->
            label <> block.Label && not (Set.contains label region)
            && (body.Operations
                |> List.exists (fun (_, operation) ->
                    Set.exists (fun id -> ANFEffects.cexprUsesTemp id operation) definitions)
                || Set.exists (fun id -> terminatorUses id body.Terminator) definitions))
    let size =
        1 + List.length after
        + (regionBlocks |> List.sumBy (fun body -> 1 + List.length body.Operations))
    if returns > 1 && (returns - 1) * size <= 1024 && not usedOutside then
        Some regionBlocks
    else None

let private canShareLargeContinuation = function
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
    | AST.TBool | AST.TDateTime | AST.TUnit
    | AST.TInternalRawPtr -> true
    | _ -> false

type private ProjectionPlan = {
    TupleId: TempId
    Fields: Atom list
    Parameters: TypedParam list
    Remaining: (TempId * CExpr) list
}

let private scalarType = function
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
    | AST.TBool | AST.TFloat64 | AST.TDateTime | AST.TUnit
    | AST.TNever | AST.TInternalRawPtr -> true
    | _ -> false

let private eligibleProjectedResult
    (candidate: Candidate)
    (config: InliningCommon.InliningConfig) =
    let returnedTuples =
        candidate.Body.Blocks
        |> Map.toList
        |> List.choose (fun (_, block) ->
            match List.tryLast block.Operations, block.Terminator with
            | Some (tupleId, TupleAlloc fields), SSAANF.Return (Var returned)
                when tupleId = returned -> Some fields
            | _, SSAANF.Return _ -> Some []
            | _ -> None)
    let bindings =
        candidate.Body.Blocks
        |> Map.fold (fun known _ block ->
            block.Operations
            |> List.fold (fun values (id, operation) ->
                Map.add id operation values) known) Map.empty
    let info = candidate.Info
    match candidate.Body.ReturnType, returnedTuples with
    | AST.TTuple elements, [fields] when not (List.isEmpty fields) ->
        let ids = fields |> List.choose (function Var id -> Some id | _ -> None)
        let freshScalarRecords =
            List.length ids = List.length fields
            && Set.count (Set.ofList ids) = List.length ids
            && (ids
                |> List.forall (fun id ->
                    match Map.tryFind id bindings with
                    | Some (RecordAlloc (descriptor, _))
                    | Some (RecordClone (descriptor, _, _)) ->
                        descriptor.Fields |> List.forall (snd >> scalarType)
                    | _ -> false))
        info.Size <= config.MaxProjectedTupleInlineSize
        && not info.IsRecursive && not info.HasClosures && not info.HasTailCalls
        && (List.forall scalarType elements
            || freshScalarRecords)
    | _ -> false

let private tryProjectedPlan
    (config: InliningCommon.InliningConfig)
    (func: SSAANF.Function)
    (block: SSAANF.Block)
    operationIndex
    resultId
    siteCount
    (candidate: Candidate) =
    if candidate.Info.IsExternal
       || siteCount > config.MaxProjectedTupleInlineSites
       || not (eligibleProjectedResult candidate config) then None
    else
        let _, _, after = splitAtOperation operationIndex block.Operations
        let returnBlocks =
            candidate.Body.Blocks
            |> Map.toList
            |> List.choose (fun (_, body) ->
                match List.tryLast body.Operations, body.Terminator with
                | Some (tupleId, TupleAlloc fields), SSAANF.Return (Var returned)
                    when tupleId = returned -> Some (tupleId, fields)
                | _, SSAANF.Return _ -> Some (TempId -1, [])
                | _ -> None)
        let usedOutside =
            func.Blocks
            |> Map.exists (fun label body ->
                label <> block.Label
                && (body.Operations
                    |> List.exists (snd >> ANFEffects.cexprUsesTemp resultId)
                    || terminatorUses resultId body.Terminator))
        match returnBlocks, candidate.Body.ReturnType with
        | [(tupleId, fields)], AST.TTuple types
            when tupleId <> TempId -1 && List.length fields = List.length types
                 && not usedOutside ->
            let rec projections found aliases kept remaining =
                match remaining with
                | (id, TupleGet (Var source, index)) :: tail when source = resultId ->
                    projections ((index, id) :: found) (Set.add id aliases) kept tail
                | (id, (TypedAtom (Var projected, _) as operation)) :: tail
                    when Set.contains projected aliases ->
                    projections found (Set.add id aliases) ((id, operation) :: kept) tail
                | rest -> List.rev found, List.rev kept @ rest
            let found, remaining = projections [] Set.empty [] after
            let indexes = found |> List.map fst |> List.sort
            let expected = [0 .. List.length types - 1]
            let resultUsedLater =
                remaining |> List.exists (snd >> ANFEffects.cexprUsesTemp resultId)
                || terminatorUses resultId block.Terminator
            if indexes <> expected || resultUsedLater then None
            else
                let projectionIds = found |> Map.ofList
                let parameters =
                    types
                    |> List.mapi (fun index typ ->
                        { Id = required index projectionIds; Type = typ })
                Some
                    { TupleId = tupleId; Fields = fields
                      Parameters = parameters; Remaining = remaining }
        | _ -> None

let private tryExpandBoundedCall
    (config: InliningCommon.InliningConfig)
    (state: State)
    (block: SSAANF.Block)
    operationIndex
    resultId
    arguments
    (candidate: Candidate) =
    match tryBoundedLoop candidate with
    | None -> None
    | Some loop when List.length arguments = List.length loop.Parameters ->
        match List.item loop.InductionIndex arguments with
        | IntLiteral (Int64 start) ->
            match boundedTripCount config.MaxBoundedLoopIterations start loop.Bound with
            | Some rounds
                when rounds * List.length loop.Iteration <= config.MaxBoundedLoopExpansion
                     && (loop.Iteration
                         |> List.forall (fun (id, _) ->
                             Map.containsKey id candidate.Body.FreshValueTypes)) ->
                let rec expand remaining currentArguments reversed state =
                    let parameterValues = List.zip loop.Parameters currentArguments |> Map.ofList
                    if remaining = 0 then
                        let returned = substituteAtom parameterValues loop.Exit
                        List.rev reversed, returned, state
                    else
                        let cloned, mapping, state =
                            loop.Iteration
                            |> List.fold (fun (cloned, mapping, state) (oldId, operation) ->
                                let typ = valueType candidate.Body oldId
                                let fresh, state = nextValue state typ
                                let renamed = substituteBounded mapping operation
                                ((fresh, renamed) :: cloned,
                                 Map.add oldId (Var fresh) mapping,
                                 state)) ([], parameterValues, state)
                        let nextArguments =
                            loop.RecursiveArguments |> List.map (substituteAtom mapping)
                        expand (remaining - 1) nextArguments (cloned @ reversed) state
                let expanded, returned, state = expand rounds arguments [] state
                let before, _, after = splitAtOperation operationIndex block.Operations
                let replacement =
                    { block with Operations = before @ expanded @ [resultId, Atom returned] @ after }
                Some
                    { state with
                        Function =
                            { state.Function with
                                Blocks = Map.add block.Label replacement state.Function.Blocks } }
            | _ -> None
        | _ -> None
    | Some _ -> None

let private cloneAt
    (state: State)
    (block: SSAANF.Block)
    operationIndex
    resultId
    arguments
    (candidate: Candidate)
    depth
    (projection: ProjectionPlan option) =
    let before, _, after = splitAtOperation operationIndex block.Operations
    let after = projection |> Option.map (fun plan -> plan.Remaining) |> Option.defaultValue after
    let parameters = candidate.Body.TypedParams
    let mappedParameters, bindings, state =
        List.zip parameters arguments
        |> List.fold (fun (mapping, bindings, state) (parameter, argument) ->
            match argument with
            | Var id -> Map.add parameter.Id id mapping, bindings, state
            | atom ->
                let id, state = nextValue state parameter.Type
                Map.add parameter.Id id mapping, bindings @ [id, Atom atom], state)
            (Map.empty, [], state)
    let labels, state =
        candidate.Body.Blocks
        |> Map.keys
        |> Seq.fold (fun (labels, state) old ->
            let fresh, state = nextLabel state
            Map.add old fresh labels, state) (Map.empty, state)
    let definitions =
        candidate.Body.Blocks
        |> Map.toList
        |> List.collect (fun (_, bodyBlock) ->
            (bodyBlock.Parameters |> List.map (fun parameter -> parameter.Id))
            @ (bodyBlock.Operations |> List.map fst))
    let returnedValues =
        candidate.Body.Blocks
        |> Map.toList
        |> List.choose (fun (_, bodyBlock) ->
            match bodyBlock.Terminator with
            | SSAANF.Return (Var id) -> Some id
            | _ -> None)
        |> Set.ofList
    let mapping, state =
        definitions
        |> List.fold (fun (mapping, state) old ->
            let typ =
                if Set.contains old returnedValues then candidate.Body.ReturnType
                else valueType candidate.Body old
            let id, state = nextValue state typ
            Map.add old id mapping, state) (mappedParameters, state)
    let continuation, state = nextLabel state
    let remapParameter (parameter: TypedParam) =
        match Map.tryFind parameter.Id mapping with
        | Some id -> { parameter with Id = id }
        | None -> Crash.crash "SSA inlining lost a block parameter"
    let clonedBlocks =
        candidate.Body.Blocks
        |> Map.toList
        |> List.map (fun (old, bodyBlock) ->
            let label =
                match Map.tryFind old labels with
                | Some value -> value
                | None -> Crash.crash "SSA inlining lost a callee block"
            label,
            { bodyBlock with
                Label = label
                Parameters = List.map remapParameter bodyBlock.Parameters
                Operations =
                    bodyBlock.Operations
                    |> List.choose (fun (oldId, operation) ->
                        if projection |> Option.exists (fun plan -> oldId = plan.TupleId) then
                            None
                        else
                            let id =
                                match Map.tryFind oldId mapping with
                                | Some value -> value
                                | None -> Crash.crash "SSA inlining lost a callee value"
                            Some (id, InliningCommon.renameCExpr mapping operation))
                Terminator =
                    match projection, bodyBlock.Terminator with
                    | Some plan, SSAANF.Return (Var returned) when returned = plan.TupleId ->
                        SSAANF.Jump (continuation, List.map (renameAtom mapping) plan.Fields)
                    | _ -> renameTerminator labels mapping continuation bodyBlock.Terminator })
    let entry =
        match Map.tryFind candidate.Body.Entry labels with
        | Some label -> label
        | None -> Crash.crash "SSA inlining lost a callee entry"
    let source =
        { block with
            Operations = before @ bindings
            Terminator = SSAANF.Jump (entry, []) }
    let continuationBlock: SSAANF.Block =
        { Label = continuation
          Parameters =
            projection
            |> Option.map (fun plan -> plan.Parameters)
            |> Option.defaultValue [{ Id = resultId; Type = candidate.Body.ReturnType }]
          Operations = after
          Terminator = block.Terminator }
    let blocks =
        clonedBlocks
        |> List.fold (fun blocks (label, cloned) -> Map.add label cloned blocks)
            (state.Function.Blocks
             |> Map.add block.Label source
             |> Map.add continuation continuationBlock)
    let depths =
        clonedBlocks
        |> List.fold (fun depths (_, cloned) ->
            cloned.Operations
            |> List.fold (fun current (id, _) -> Map.add id (depth + 1) current) depths)
            state.Depths
    let result =
        { state with
            Function =
                { state.Function with
                    Blocks = blocks
                    FreshValueTypes =
                        Map.add resultId candidate.Body.ReturnType state.Function.FreshValueTypes }
            Depths = depths
            Processed = Set.add resultId state.Processed }
    let returns =
        clonedBlocks
        |> List.choose (fun (label, cloned) ->
            match cloned.Terminator with
            | SSAANF.Jump (target, [_]) when target = continuation -> Some label
            | _ -> None)
    match projection, continuationRegion state.Function block resultId after (List.length returns) with
    | None, Some region ->
        let copied =
            returns
            // SSA labels are assigned before nested branches are traversed;
            // allocate continuation copies in return-substitution order.
            |> List.rev
            |> List.fold (fun state returnLabel ->
                let returnBlock = required returnLabel state.Function.Blocks
                let returned =
                    match returnBlock.Terminator with
                    | SSAANF.Jump (target, [atom]) when target = continuation -> atom
                    | _ -> Crash.crash "SSA inlining lost a copied return"
                let alias, state = nextValue state candidate.Body.ReturnType
                let labels, state =
                    region
                    |> List.fold (fun (labels, state) original ->
                        let fresh, state = nextLabel state
                        Map.add original.Label fresh labels, state) (Map.empty, state)
                let originals =
                    (after |> List.map fst)
                    @ (region
                       |> List.collect (fun body ->
                           (body.Parameters |> List.map (fun parameter -> parameter.Id))
                           @ (body.Operations |> List.map fst)))
                let mapping, state =
                    originals
                    |> List.fold (fun (mapping, state) oldId ->
                        let fresh, state = nextValue state (valueType state.Function oldId)
                        Map.add oldId fresh mapping,
                        { state with
                            Depths =
                                Map.add fresh
                                    (Map.tryFind oldId result.Depths |> Option.defaultValue 0)
                                    state.Depths })
                        (Map.ofList [resultId, alias], state)
                let renameOperations operations =
                    operations
                    |> List.map (fun (oldId, operation) ->
                        required oldId mapping, InliningCommon.renameCExpr mapping operation)
                let regionCopies =
                    region
                    |> List.map (fun original ->
                        let fresh = required original.Label labels
                        fresh,
                        { original with
                            Label = fresh
                            Parameters =
                                original.Parameters
                                |> List.map (fun parameter ->
                                    { parameter with Id = required parameter.Id mapping })
                            Operations = renameOperations original.Operations
                            Terminator =
                                renameCallerTerminator labels mapping original.Terminator })
                let blocks =
                    regionCopies
                    |> List.fold (fun blocks (label, copied) -> Map.add label copied blocks)
                        (state.Function.Blocks
                         |> Map.add returnLabel
                             { returnBlock with
                                 Operations =
                                     returnBlock.Operations
                                     @ [alias, Atom returned]
                                     @ renameOperations after
                                 Terminator =
                                     renameCallerTerminator labels mapping block.Terminator })
                let processed =
                    mapping
                    |> Map.fold (fun processed oldId newId ->
                        if Set.contains oldId result.Processed then Set.add newId processed
                        else processed) state.Processed
                { state with Processed = processed
                             Function = { state.Function with Blocks = blocks } }) result
        let blocks =
            region
            |> List.fold (fun blocks body -> Map.remove body.Label blocks)
                (Map.remove continuation copied.Function.Blocks)
        { copied with Function = { copied.Function with Blocks = blocks } }
    | _ -> result

let private cloneLinearAt
    (state: State)
    (block: SSAANF.Block)
    operationIndex
    resultId
    arguments
    (candidate: Candidate)
    depth
    (projection: ProjectionPlan option) =
    match Map.toList candidate.Body.Blocks with
    | [(label, body)] when label = candidate.Body.Entry && List.isEmpty body.Parameters ->
        match body.Terminator with
        | SSAANF.Return returned ->
            let before, _, after = splitAtOperation operationIndex block.Operations
            let after =
                projection |> Option.map (fun plan -> plan.Remaining) |> Option.defaultValue after
            let mapping, bindings, state =
                List.zip candidate.Body.TypedParams arguments
                |> List.fold (fun (mapping, bindings, state) (parameter, argument) ->
                    match argument with
                    | Var id -> Map.add parameter.Id id mapping, bindings, state
                    | atom ->
                        let id, state = nextValue state parameter.Type
                        Map.add parameter.Id id mapping, bindings @ [id, Atom atom], state)
                    (Map.empty, [], state)
            let mapping, state =
                body.Operations
                |> List.fold (fun (mapping, state) (oldId, _) ->
                    let id, state = nextValue state (valueType candidate.Body oldId)
                    Map.add oldId id mapping, state) (mapping, state)
            let operations =
                body.Operations
                |> List.choose (fun (oldId, operation) ->
                    if projection |> Option.exists (fun plan -> oldId = plan.TupleId) then
                        None
                    else
                        Some (required oldId mapping, InliningCommon.renameCExpr mapping operation))
            let resultOperations =
                match projection with
                | Some plan ->
                    List.zip plan.Parameters plan.Fields
                    |> List.map (fun (parameter, field) ->
                        parameter.Id, Atom (renameAtom mapping field))
                | None -> [resultId, Atom (renameAtom mapping returned)]
            let replacement =
                { block with
                    Operations =
                        before @ bindings @ operations
                        @ resultOperations @ after }
            let depths =
                operations
                |> List.fold (fun depths (id, _) -> Map.add id (depth + 1) depths)
                    state.Depths
            Some
                { state with
                    Function =
                        { state.Function with
                            Blocks = Map.add block.Label replacement state.Function.Blocks }
                    Depths = depths
                    Processed = Set.add resultId state.Processed }
        | _ -> None
    | _ -> None

let private callSites (state: State) =
    state.Function.Blocks
    |> Map.toList
    |> List.collect (fun (label, block) ->
        block.Operations
        |> List.indexed
        |> List.choose (fun (index, (id, operation)) ->
            match operation with
            | Call (name, arguments) when not (Set.contains id state.Processed) ->
                Some (label, index, id, name, arguments)
            | _ -> None))

let private countExternalCalls externalNames (func: SSAANF.Function) =
    func.Blocks
    |> Map.toSeq
    |> Seq.sumBy (fun (_, block) ->
        block.Operations
        |> List.sumBy (fun (_, operation) ->
            match operation with
            | Call (name, _) | BorrowedCall (name, _)
                when Set.contains name externalNames -> 1
            | _ -> 0))

let private isMandatoryExternal (candidate: Candidate) =
    match candidate.Info.Func.TypedParams, candidate.Info.Func.Body with
    | [], Return (IntLiteral _)
    | [], Return (BoolLiteral _)
    | [], Return (FloatLiteral _)
    | [], Return (StringLiteral _)
    | [], Return UnitLiteral -> true
    | _ -> false

let private inlineFunction
    (config: InliningCommon.InliningConfig)
    (candidates: Map<AST.FunctionId, Candidate>)
    (externalNames: Set<AST.FunctionId>)
    (func: SSAANF.Function) =
    let useExternal =
        countExternalCalls externalNames func <= config.MaxExternalInlineSites
    let siteCounts =
        func.Blocks
        |> Map.toSeq
        |> Seq.collect (fun (_, block) -> block.Operations)
        |> Seq.fold (fun counts (_, operation) ->
            match operation with
            | Call (name, _) ->
                let previous = Map.tryFind name counts |> Option.defaultValue 0
                Map.add name (previous + 1) counts
            | _ -> counts) Map.empty
    let rec visit state =
        match callSites state with
        | [] -> state.Function
        | sites ->
            let label, index, id, name, arguments = List.last sites
            let state = { state with Processed = Set.add id state.Processed }
            let depth = Map.tryFind id state.Depths |> Option.defaultValue 0
            match Map.tryFind name candidates, Map.tryFind label state.Function.Blocks with
            | Some candidate, Some block when candidate.Info.IsRecursive ->
                match tryExpandBoundedCall config state block index id arguments candidate with
                | Some expanded -> visit expanded
                | None -> visit state
            | Some candidate, Some block
                when (not candidate.Info.IsExternal || useExternal || isMandatoryExternal candidate)
                     && List.length candidate.Body.TypedParams = List.length arguments ->
                let count = Map.tryFind name siteCounts |> Option.defaultValue 0
                match tryProjectedPlan config state.Function block index id count candidate with
                | Some projection ->
                    match cloneLinearAt state block index id arguments candidate depth (Some projection) with
                    | Some inlined -> visit inlined
                    | None -> cloneAt state block index id arguments candidate depth (Some projection) |> visit
                | None when InliningCommon.shouldInline candidate.Info config depth ->
                    match cloneLinearAt state block index id arguments candidate depth None with
                    | Some inlined -> visit inlined
                    | None ->
                        let returns =
                            candidate.Body.Blocks
                            |> Map.toSeq
                            |> Seq.sumBy (fun (_, body) ->
                                match body.Terminator with
                                | SSAANF.Return _ -> 1
                                | _ -> 0)
                        let _, _, after = splitAtOperation index block.Operations
                        if returns <= 1
                           || canShareLargeContinuation candidate.Body.ReturnType
                           || Option.isSome
                               (continuationRegion state.Function block id after returns) then
                            cloneAt state block index id arguments candidate depth None |> visit
                        else visit state
                | None -> visit state
            | _ -> visit state
    visit (initialState func)

let inlineProgramWithExternalCandidatesAndExclusions
    (config: InliningCommon.InliningConfig)
    (externalCandidates: Map<AST.FunctionId, InliningCommon.FunctionInfo>)
    (externalSSA: SSAANF.Function list)
    (excludedLocalNames: Set<AST.FunctionId>)
    (localSource: ANF.Function list)
    (functions: SSAANF.Function list) =
    let withSSAFacts (body: SSAANF.Function) (info: InliningCommon.FunctionInfo) =
        let operations =
            body.Blocks
            |> Map.toList
            |> List.collect (fun (_, block) -> block.Operations |> List.map snd)
        { info with
            Size = List.length operations
            HasClosures =
                operations
                |> List.exists (function
                    | ClosureAlloc _ | ClosureCall _ | ClosureTailCall _ -> true
                    | _ -> false)
            HasTailCalls =
                operations
                |> List.exists (function
                    | TailCall _ | IndirectTailCall _ | ClosureTailCall _ -> true
                    | _ -> false) }
    let localInfo =
        InliningCommon.buildFunctionInfoMap localSource
        |> Map.filter (fun name _ -> not (Set.contains name excludedLocalNames))
    let allSSA =
        externalSSA @ functions
        |> List.map (fun func -> func.Id, func)
        |> Map.ofList
    let candidates =
        externalCandidates
        |> Map.fold (fun current name info ->
            match Map.tryFind name allSSA with
            | Some body -> Map.add name { Info = withSSAFacts body info; Body = body } current
            | None -> current) Map.empty
        |> fun external ->
            localInfo
            |> Map.fold (fun current name info ->
                match Map.tryFind name allSSA with
                | Some body -> Map.add name { Info = withSSAFacts body info; Body = body } current
                | None -> current) external
    let externalNames = externalCandidates |> Map.keys |> Set.ofSeq
    functions |> List.map (inlineFunction config candidates externalNames)
