// SSAOptimization.fs - Simplify typed high-level SSA before specialization.

module SSAOptimization

open ANF
open ANFConstants
open ANFEffects
open ANFSubstitution

let private termUses = function
    | SSAANF.Return atom -> addAtomUse atom Set.empty
    | SSAANF.Jump (_, arguments) ->
        arguments |> List.fold (fun uses atom -> addAtomUse atom uses) Set.empty
    | SSAANF.Branch (condition, _, _) -> addAtomUse condition Set.empty

let private reachableBlocks (func: SSAANF.Function) =
    let rec visit seen label =
        if Set.contains label seen then seen
        else
            let seen = Set.add label seen
            match Map.tryFind label func.Blocks with
            | None -> Crash.crash "SSA optimization: missing successor block"
            | Some block ->
                match block.Terminator with
                | SSAANF.Return _ -> seen
                | SSAANF.Jump (target, _) -> visit seen target
                | SSAANF.Branch (_, yes, no) -> visit (visit seen yes) no
    visit Set.empty func.Entry

let private useCounts (func: SSAANF.Function) =
    func.Blocks
    |> Map.fold (fun uses _ block ->
        let addUses current ids =
            ids |> Set.fold (fun counts id ->
                Map.add id (1 + (Map.tryFind id counts |> Option.defaultValue 0)) counts) current
        let uses = addUses uses (termUses block.Terminator)
        block.Operations
        |> List.fold (fun current (_, operation) ->
            addUses current (cexprTempUses operation)) uses) Map.empty

let private rewriteTerminator options env = function
    | SSAANF.Return atom -> SSAANF.Return (substAtom env atom)
    | SSAANF.Jump (target, arguments) ->
        SSAANF.Jump (target, List.map (substAtom env) arguments)
    | SSAANF.Branch (condition, yes, no) ->
        match substAtom env condition with
        | BoolLiteral true when options.EnableConstFolding -> SSAANF.Jump (yes, [])
        | BoolLiteral false when options.EnableConstFolding -> SSAANF.Jump (no, [])
        | condition when yes = no -> SSAANF.Jump (yes, [])
        | condition -> SSAANF.Branch (condition, yes, no)

let private rewriteOperations context options typeEnv tupleEnv env operations =
    operations
    |> List.fold (fun (rewritten, known, tuples, cse) (id, operation) ->
        let operation', _ = optimizeCExpr context options known typeEnv tuples operation
        let operation', cse' =
            if options.EnableCSE then
                match ANFExpressionOptimization.tryCSEKey operation' with
                | Some key ->
                    match Map.tryFind key cse with
                    | Some existing -> Atom (Var existing), cse
                    | None -> operation', Map.add key id cse
                | None -> operation', cse
            else operation', cse
        let known' =
            match operation' with
            | Atom atom when options.EnableCopyProp && not (mustPreserveEvaluation context operation') ->
                Map.add id atom known
            | _ -> known
        let tuples' =
            match operation' with
            | TupleAlloc elements ->
                let fields =
                    elements
                    |> List.indexed
                    |> List.choose (fun (index, atom) ->
                        if canForwardTupleElement context typeEnv atom then
                            Some (index, atom)
                        else None)
                    |> Map.ofList
                Map.add id fields tuples
            | _ -> tuples
        ((id, operation') :: rewritten, known', tuples', cse'))
        ([], env, tupleEnv, Map.empty)
    |> fun (reversed, known, tuples, _) -> List.rev reversed, known, tuples

let private rewriteIndexCalls (context: OptimizeContext) uses operations =
    let hasName id name = Map.tryFind id context.FunctionNames = Some name
    let resolve name = Map.tryFind name context.FunctionIds
    let rec rewrite = function
        | (indexId, Call (fromInt64, [nativeIndex])) ::
          (resultId, Call (getAt, [listValue; Var usedIndex])) :: rest
            when indexId = usedIndex
                 && Map.tryFind indexId uses = Some 1
                 && hasName fromInt64 "Darklang.Stdlib.Int.fromInt64"
                 && (Map.tryFind getAt context.FunctionNames
                     |> Option.exists (fun name ->
                         name = "Darklang.Stdlib.List.getAt"
                         || name.StartsWith("Darklang.Stdlib.List.getAt_"))) ->
            let target =
                Map.tryFind getAt context.FunctionNames
                |> Option.bind (fun name ->
                    let publicPrefix = "Darklang.Stdlib.List.getAt"
                    if name = publicPrefix || name.StartsWith(publicPrefix + "_") then
                        let internalName =
                            "Darklang.Stdlib.List.__getAt"
                            + name.Substring(publicPrefix.Length)
                        resolve internalName
                    else None)
            match target with
            | Some internalGetAt ->
                (resultId, Call (internalGetAt, [listValue; nativeIndex])) :: rewrite rest
            | None ->
                (indexId, Call (fromInt64, [nativeIndex])) ::
                rewrite ((resultId, Call (getAt, [listValue; Var usedIndex])) :: rest)
        | (indexId, Call (fromInt64, [nativeIndex])) ::
          (resultId, Call (getByteAt, [value; Var usedIndex])) :: rest
            when indexId = usedIndex
                 && Map.tryFind indexId uses = Some 1
                 && hasName fromInt64 "Darklang.Stdlib.Int.fromInt64"
                 && hasName getByteAt "Darklang.Stdlib.String.getByteAt" ->
            match resolve "Darklang.Stdlib.String.__getByteAtInt64" with
            | Some target ->
                (resultId, Call (target, [value; nativeIndex])) :: rewrite rest
            | None ->
                (indexId, Call (fromInt64, [nativeIndex])) ::
                rewrite ((resultId, Call (getByteAt, [value; Var usedIndex])) :: rest)
        | first :: rest -> first :: rewrite rest
        | [] -> []
    rewrite operations

let private rewriteByteOptionMatches (context: OptimizeContext) (func: SSAANF.Function) =
    let resolve name = Map.tryFind name context.FunctionIds
    match resolve "Darklang.Stdlib.String.__getByteAtInt64",
          resolve "Darklang.Stdlib.String.__byteLength",
          resolve "Darklang.Stdlib.String.__byteAtUnchecked" with
    | Some checkedByte, Some byteLength, Some uncheckedByte
        when func.Blocks
             |> Map.exists (fun _ block ->
                 block.Operations
                 |> List.exists (function
                     | _, Call (target, _) when target = checkedByte -> true
                     | _ -> false)) ->
        let predecessors =
            func.Blocks
            |> Map.fold (fun counts _ block ->
                let successors =
                    match block.Terminator with
                    | SSAANF.Return _ -> []
                    | SSAANF.Jump (target, _) -> [target]
                    | SSAANF.Branch (_, yes, no) -> [yes; no]
                successors
                |> List.fold (fun current target ->
                    Map.add target
                        (1 + (Map.tryFind target current |> Option.defaultValue 0))
                        current) counts) Map.empty
        let uses = useCounts func
        let nextValue =
            func.FreshValueTypes
            |> Map.keys
            |> Seq.map (fun (TempId id) -> id)
            |> Seq.fold max 4000
            |> (+) 1
        let rewrite (current: SSAANF.Function, nextValue) label (block: SSAANF.Block) =
            let candidate =
                match List.rev block.Operations, block.Terminator with
                | (conditionId, Prim (Neq, Var optionId, IntLiteral (Int64 256L))) ::
                  (callId, Call (target, [value; index])) :: reversedPrefix,
                  SSAANF.Branch (Var branchId, someLabel, _)
                    when conditionId = branchId && optionId = callId && target = checkedByte ->
                    Some (conditionId, callId, value, index, someLabel,
                          List.rev reversedPrefix, false)
                | (conditionId, Prim (Eq, Var optionId, IntLiteral (Int64 256L))) ::
                  (callId, Call (target, [value; index])) :: reversedPrefix,
                  SSAANF.Branch (Var branchId, _, someLabel)
                    when conditionId = branchId && optionId = callId && target = checkedByte ->
                    Some (conditionId, callId, value, index, someLabel,
                          List.rev reversedPrefix, true)
                | _ -> None
            match candidate with
            | None -> current, nextValue
            | Some (conditionId, callId, value, index, someLabel, prefix, inverted) ->
                match Map.tryFind someLabel current.Blocks with
                | None -> Crash.crash "SSA optimization: byte match successor is missing"
                | Some someBlock ->
                    let payload =
                        match someBlock.Operations with
                        | (payloadId, TypedAtom (Var source, AST.TUInt8)) :: rest
                            when source = callId ->
                            Some (payloadId, rest)
                        | _ -> None
                    let expectedUses = if Option.isSome payload then 2 else 1
                    if (Map.tryFind callId uses |> Option.defaultValue 0) <> expectedUses
                       || (Option.isSome payload
                           && Map.tryFind someLabel predecessors <> Some 1) then
                        current, nextValue
                    else
                        let lengthId = TempId nextValue
                        let nonnegativeId = TempId (nextValue + 1)
                        let lessThanLengthId = TempId (nextValue + 2)
                        let operations =
                            prefix
                            @ [ lengthId, Call (byteLength, [value])
                                nonnegativeId, Prim (Gte, index, IntLiteral (Int64 0L))
                                lessThanLengthId, Prim (Lt, index, Var lengthId)
                                conditionId, Prim (And, Var nonnegativeId, Var lessThanLengthId) ]
                        let someBlock =
                            match payload with
                            | None -> someBlock
                            | Some (payloadId, rest) ->
                                { someBlock with
                                    Operations =
                                        (payloadId, Call (uncheckedByte, [value; index])) :: rest }
                        let blocks =
                            current.Blocks
                            |> Map.add label
                                { block with
                                    Operations = operations
                                    Terminator =
                                        if inverted then
                                            match block.Terminator with
                                            | SSAANF.Branch (condition, yes, no) ->
                                                SSAANF.Branch (condition, no, yes)
                                            | _ -> Crash.crash "SSA optimization: expected byte branch"
                                        else block.Terminator }
                            |> Map.add someLabel someBlock
                        let valueTypes =
                            current.FreshValueTypes
                            |> Map.add lengthId AST.TInt64
                            |> Map.add nonnegativeId AST.TBool
                            |> Map.add lessThanLengthId AST.TBool
                        { current with Blocks = blocks; FreshValueTypes = valueTypes },
                        nextValue + 3
        func.Blocks
        |> Map.fold rewrite (func, nextValue)
        |> fst
    | _ -> func

let private eliminateDominatedDuplicatesWithCandidates candidates (func: SSAANF.Function) =
    let labels = func.Blocks |> Map.keys |> Set.ofSeq
    let predecessors =
        func.Blocks
        |> Map.fold (fun preds source block ->
            let successors =
                match block.Terminator with
                | SSAANF.Return _ -> []
                | SSAANF.Jump (target, _) -> [target]
                | SSAANF.Branch (_, yes, no) -> [yes; no]
            successors
            |> List.fold (fun current target ->
                let existing = Map.tryFind target current |> Option.defaultValue Set.empty
                Map.add target (Set.add source existing) current) preds) Map.empty
    let initial =
        labels
        |> Set.fold (fun dom label ->
            Map.add label
                (if label = func.Entry then Set.singleton label else labels)
                dom) Map.empty
    let rec settle known =
        let next =
            labels
            |> Set.fold (fun dom label ->
                if label = func.Entry then Map.add label (Set.singleton label) dom
                else
                    let incoming =
                        Map.tryFind label predecessors
                        |> Option.defaultValue Set.empty
                        |> Set.toList
                        |> List.choose (fun source -> Map.tryFind source known)
                    let common =
                        match incoming with
                        | [] -> Set.empty
                        | first :: rest -> List.fold Set.intersect first rest
                    Map.add label (Set.add label common) dom) Map.empty
        if next = known then known else settle next
    let dominators = settle initial
    { func with
        Blocks =
            func.Blocks
            |> Map.map (fun label block ->
                let dominates =
                    Map.tryFind label dominators |> Option.defaultValue Set.empty
                { block with
                    Operations =
                        block.Operations
                        |> List.mapi (fun index (id, operation) ->
                            match ANFExpressionOptimization.tryCSEKey operation with
                            | None -> id, operation
                            | Some key ->
                                let prior =
                                    Map.tryFind key candidates
                                    |> Option.defaultValue []
                                    |> List.tryPick (fun (sourceLabel, sourceIndex, sourceId) ->
                                        if sourceId <> id
                                           && Set.contains sourceLabel dominates
                                           && (sourceLabel <> label || sourceIndex < index) then
                                            Some sourceId
                                        else None)
                                match prior with
                                | Some source -> id, Atom (Var source)
                                | None -> id, operation) }) }

let private eliminateDominatedDuplicates (func: SSAANF.Function) =
    let candidates =
        func.Blocks
        |> Map.toList
        |> List.collect (fun (label, block) ->
            block.Operations
            |> List.mapi (fun index (id, operation) ->
                ANFExpressionOptimization.tryCSEKey operation
                |> Option.map (fun key -> key, (label, index, id)))
            |> List.choose id)
        |> List.groupBy fst
        |> List.map (fun (key, values) -> key, List.map snd values)
        |> Map.ofList
    if candidates |> Map.forall (fun _ values -> List.length values < 2) then func
    else eliminateDominatedDuplicatesWithCandidates candidates func

let private mergeSinglePredecessorJumps (func: SSAANF.Function) =
    let predecessors =
        func.Blocks
        |> Map.fold (fun counts _ block ->
            let successors =
                match block.Terminator with
                | SSAANF.Return _ -> []
                | SSAANF.Jump (target, _) -> [target]
                | SSAANF.Branch (_, yes, no) -> [yes; no]
            successors
            |> List.fold (fun known target ->
                Map.add target
                    (1 + (Map.tryFind target known |> Option.defaultValue 0))
                    known) counts) Map.empty
    let rec merge (current: SSAANF.Function) (pending: (SSAANF.Label * SSAANF.Block) list) =
        match pending with
        | [] -> current
        | (label, _) :: rest when not (Map.containsKey label current.Blocks) ->
            merge current rest
        | (label, block) :: rest ->
            match block.Terminator with
            | SSAANF.Jump (target, [])
                when label <> target && target <> func.Entry
                     && Map.tryFind target predecessors = Some 1 ->
                match Map.tryFind target current.Blocks with
                | Some successor when List.isEmpty successor.Parameters ->
                    let combined =
                        { block with
                            Operations = block.Operations @ successor.Operations
                            Terminator = successor.Terminator }
                    { current with
                        Blocks =
                            current.Blocks
                            |> Map.remove target
                            |> Map.add label combined }
                    |> fun updated -> merge updated rest
                | _ -> merge current rest
            | _ -> merge current rest
    func.Blocks |> Map.toList |> merge func

let private simplifyBooleanReturnBranches (func: SSAANF.Function) =
    let nextValue =
        func.FreshValueTypes
        |> Map.keys
        |> Seq.map (fun (TempId id) -> id)
        |> Seq.fold max 4000
        |> (+) 1
    let rewrite (blocks, types, nextValue) label (block: SSAANF.Block) =
        let literalReturn target =
            match Map.tryFind target func.Blocks with
            | Some successor when List.isEmpty successor.Parameters
                                  && List.isEmpty successor.Operations ->
                match successor.Terminator with
                | SSAANF.Return (BoolLiteral value) -> Some value
                | _ -> None
            | _ -> None
        match block.Terminator with
        | SSAANF.Branch (condition, yes, no) when yes <> no ->
            match literalReturn yes, literalReturn no with
            | Some true, Some false ->
                Map.add label { block with Terminator = SSAANF.Return condition } blocks,
                types, nextValue
            | Some false, Some true ->
                let invertedId = TempId nextValue
                let rewritten =
                    { block with
                        Operations =
                            block.Operations @ [invertedId, UnaryPrim (Not, condition)]
                        Terminator = SSAANF.Return (Var invertedId) }
                Map.add label rewritten blocks,
                Map.add invertedId AST.TBool types,
                nextValue + 1
            | _ -> blocks, types, nextValue
        | _ -> blocks, types, nextValue
    let blocks, types, _ =
        func.Blocks
        |> Map.fold rewrite (func.Blocks, func.FreshValueTypes, nextValue)
    { func with Blocks = blocks; FreshValueTypes = types }

let private devirtualizeCaptureFreeClosures (func: SSAANF.Function) =
    let allocations =
        func.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) ->
            block.Operations
            |> List.choose (function
                | id, ClosureAlloc (target, []) -> Some (id, target)
                | _ -> None))
    if List.isEmpty allocations then func
    else
        let counts = useCounts func
        let callsThrough =
            func.Blocks
            |> Map.fold (fun calls _ block ->
                block.Operations
                |> List.fold (fun calls (_, operation) ->
                    match operation with
                    | ClosureCall (Var id, arguments)
                    | ClosureTailCall (Var id, arguments)
                        when not (List.exists (ANFEffects.atomUsesTemp id) arguments) ->
                        let count = Map.tryFind id calls |> Option.defaultValue 0
                        Map.add id (count + 1) calls
                    | _ -> calls) calls) Map.empty
        let candidates =
            allocations
            |> List.choose (fun (id, target) ->
                let calls = Map.tryFind id callsThrough |> Option.defaultValue 0
                if calls > 0 && Map.tryFind id counts = Some calls then
                    Some (id, target)
                else None)
            |> Map.ofList
        if Map.isEmpty candidates then func
        else
            { func with
                Blocks =
                    func.Blocks
                    |> Map.map (fun _ block ->
                        { block with
                            Operations =
                                block.Operations
                                |> List.choose (fun (id, operation) ->
                                    match operation with
                                    | ClosureAlloc (_, []) when Map.containsKey id candidates ->
                                        None
                                    | ClosureCall (Var closureId, arguments) ->
                                        match Map.tryFind closureId candidates with
                                        | Some target ->
                                            Some (id, Call (target, UnitLiteral :: arguments))
                                        | None -> Some (id, operation)
                                    | ClosureTailCall (Var closureId, arguments) ->
                                        match Map.tryFind closureId candidates with
                                        | Some target ->
                                            Some (id, TailCall (target, UnitLiteral :: arguments))
                                        | None -> Some (id, operation)
                                    | _ -> Some (id, operation)) }) }

let private rewriteOnce context options (func: SSAANF.Function) =
    let typeEnv =
        func.TypedParams
        |> List.fold (fun types parameter -> Map.add parameter.Id parameter.Type types)
            func.FreshValueTypes
    // SSA identities are unique. A known scalar definition may be substituted
    // globally even when it lies inside a branch: every valid use is dominated
    // by that definition. Repeating the pass resolves definitions in any block
    // label order without changing the fixed-point iteration limit.
    let known =
        func.Blocks
        |> Map.fold (fun env _ block ->
            let _, env', _ =
                rewriteOperations context options typeEnv Map.empty env block.Operations
            env') Map.empty
    let blocks =
        func.Blocks
        |> Map.map (fun _ block ->
            let operations, _, _ =
                rewriteOperations context options typeEnv Map.empty known block.Operations
            { block with
                Operations = operations
                Terminator = rewriteTerminator options known block.Terminator })
    let rewritten = { func with Blocks = blocks }
    let rewritten =
        if options.EnableConstFolding then simplifyBooleanReturnBranches rewritten
        else rewritten
    let reachable = reachableBlocks rewritten
    let rewritten =
        { rewritten with
            Blocks = rewritten.Blocks |> Map.filter (fun label _ -> Set.contains label reachable) }
    let rewritten =
        if options.EnableCSE then eliminateDominatedDuplicates rewritten
        else rewritten
    let uses = useCounts rewritten
    let rewritten =
        { rewritten with
            Blocks =
                rewritten.Blocks
                |> Map.map (fun _ block ->
                    { block with
                        Operations = rewriteIndexCalls context uses block.Operations }) }
    let rewritten = rewriteByteOptionMatches context rewritten
    let rewritten = mergeSinglePredecessorJumps rewritten
    if not options.EnableDCE then rewritten
    else
        let uses = useCounts rewritten
        { rewritten with
            Blocks =
                rewritten.Blocks
                |> Map.map (fun _ block ->
                    { block with
                        Operations =
                            block.Operations
                            |> List.filter (fun (id, operation) ->
                                (Map.tryFind id uses |> Option.defaultValue 0) > 0
                                || mustPreserveEvaluation context operation) }) }

let optimizeFunction context options func =
    let compactLabels (current: SSAANF.Function) =
        let labels =
            current.Blocks
            |> Map.keys
            |> Seq.mapi (fun index old -> old, SSAANF.Label index)
            |> Map.ofSeq
        let mapped label =
            match Map.tryFind label labels with
            | Some value -> value
            | None -> Crash.crash "SSA optimization: compacted edge target is missing"
        let blocks =
            current.Blocks
            |> Map.toList
            |> List.map (fun (oldLabel, block) ->
                let label = mapped oldLabel
                let terminator =
                    match block.Terminator with
                    | SSAANF.Return atom -> SSAANF.Return atom
                    | SSAANF.Jump (target, arguments) ->
                        SSAANF.Jump (mapped target, arguments)
                    | SSAANF.Branch (condition, yes, no) ->
                        SSAANF.Branch (condition, mapped yes, mapped no)
                label,
                { block with Label = label; Terminator = terminator })
            |> Map.ofList
        { current with Entry = mapped current.Entry; Blocks = blocks }
    let rec iterate remaining current =
        if remaining <= 0 then current
        else
            let next = rewriteOnce context options current
            if next = current then current
            else iterate (remaining - 1) next
    iterate 10 func
    |> devirtualizeCaptureFreeClosures
    |> compactLabels
