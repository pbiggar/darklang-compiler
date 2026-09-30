// SSATailCallDetection.fs - Preserve ownership-safe tail calls on SSA ANF blocks.

module SSATailCallDetection

open ANF

type private Facts = {
    Aliases: Map<TempId, TempId>
    Borrows: Map<TempId, Set<TempId>>
    Retained: Set<TempId>
    Released: Set<TempId>
}

let private emptyFacts = {
    Aliases = Map.empty
    Borrows = Map.empty
    Retained = Set.empty
    Released = Set.empty
}

let private advance (facts: Facts) (id, operation) : Facts =
    let root = TailCallDetection.canonicalTempId facts.Aliases
    let retained, released =
        match operation with
        | RefCountInc (Var value, _, _, _)
        | RefCountIncString (Var value)
        | RefCountIncBlob (Var value)
        | RefCountIncInt (Var value) -> Set.add (root value) facts.Retained, facts.Released
        | RefCountDec (Var value, _, _, _)
        | RefCountDecString (Var value)
        | RefCountDecBlob (Var value)
        | RefCountDecInt (Var value) ->
            Set.remove (root value) facts.Retained, Set.add (root value) facts.Released
        | _ -> facts.Retained, facts.Released
    {
        Aliases = TailCallDetection.extendAliasRoots facts.Aliases id operation
        Borrows = TailCallDetection.extendBorrowRoots facts.Aliases facts.Borrows id operation
        Retained = retained
        Released = released
    }

let private mergeFacts (left: Facts) (right: Facts) : Facts =
    // An alias or ownership event is certain only if every incoming path has
    // it. A borrow may depend on any predecessor, so retain every source.
    {
        Aliases =
            left.Aliases
            |> Map.filter (fun id root -> Map.tryFind id right.Aliases = Some root)
        Borrows =
            right.Borrows
            |> Map.fold (fun acc id sources ->
                let existing = Map.tryFind id acc |> Option.defaultValue Set.empty
                Map.add id (Set.union existing sources) acc) left.Borrows
        Retained = Set.intersect left.Retained right.Retained
        Released = Set.intersect left.Released right.Released
    }

let private addEdgeParameters
    (target: SSAANF.Block)
    (args: Atom list)
    (facts: Facts)
    : Facts =
    if List.length target.Parameters <> List.length args then
        Crash.crash "SSA tail-call detection: edge argument count does not match block parameters"
    List.zip target.Parameters args
    |> List.fold (fun current (parameter, argument) ->
        match argument with
        | Var source ->
            let root = TailCallDetection.canonicalTempId current.Aliases source
            let inherited =
                Map.tryFind source current.Borrows
                |> Option.orElse (Map.tryFind root current.Borrows)
                |> Option.defaultValue Set.empty
            {
                current with
                    Aliases = Map.add parameter.Id root current.Aliases
                    Borrows =
                        if Set.isEmpty inherited then current.Borrows
                        else Map.add parameter.Id inherited current.Borrows
            }
        | _ -> current) facts

let private incomingFacts (func: SSAANF.Function) : Map<SSAANF.Label, Facts> =
    // Propagate ownership-sensitive facts through SSA edges to a fixed point.
    // Loop headers can receive another predecessor after their first visit.
    let rec settle (known: Map<SSAANF.Label, Facts>) (pending: SSAANF.Label list) =
        match pending with
        | [] -> known
        | label :: rest ->
            match Map.tryFind label known, Map.tryFind label func.Blocks with
            | Some facts, Some block ->
                let after = List.fold advance facts block.Operations
                let edges =
                    match block.Terminator with
                    | SSAANF.Return _ -> []
                    | SSAANF.Jump (target, args) -> [target, args]
                    | SSAANF.Branch (_, ifTrue, ifFalse) -> [ifTrue, []; ifFalse, []]
                let known', pending' =
                    edges
                    |> List.fold (fun (current, queue) (targetLabel, args) ->
                        match Map.tryFind targetLabel func.Blocks with
                        | None -> current, queue
                        | Some target ->
                            let candidate = addEdgeParameters target args after
                            let next =
                                match Map.tryFind targetLabel current with
                                | None -> candidate
                                | Some previous -> mergeFacts previous candidate
                            if Map.tryFind targetLabel current = Some next then current, queue
                            else Map.add targetLabel next current, targetLabel :: queue)
                        (known, rest)
                settle known' pending'
            | _ -> settle known rest
    settle (Map.ofList [func.Entry, emptyFacts]) [func.Entry]

let detect
    (recursiveMembers: FunctionIdMap<AST.LoweredRecursiveMember>)
    (func: SSAANF.Function)
    : SSAANF.Function =
    if not (TailCallDetection.isEligibleFunctionName func.Name) then func
    else
        let factsByBlock = incomingFacts func
        let paramIds = func.TypedParams |> List.map (fun param -> param.Id) |> Set.ofList
        let entry =
            match Map.tryFind func.Entry func.Blocks with
            | Some block -> block
            | None -> Crash.crash "SSA tail-call detection: missing entry block"
        let entryExpr =
            List.foldBack (fun (id, op) rest -> Let (id, op, rest))
                entry.Operations (Return UnitLiteral)
        let ownedParams = TailCallDetection.leadingRetainedParams paramIds entryExpr
        let isCurrentMember target =
            match FunctionIdMap.tryFind func.Id recursiveMembers, FunctionIdMap.tryFind target recursiveMembers with
            | Some current, Some other ->
                current.Typed.Resolved.Parsed.Binding = other.Typed.Resolved.Parsed.Binding
            | None, None -> target = func.Id
            | _ -> false
        let transform label (block: SSAANF.Block) =
            match block.Terminator, Map.tryFind label factsByBlock with
            | SSAANF.Return value, Some facts ->
                let expr =
                    List.foldBack (fun (id, op) rest -> Let (id, op, rest))
                        block.Operations (Return value)
                let converted =
                    TailCallDetection.detectTailCalls
                        func.Id isCurrentMember func.TypedParams ownedParams facts.Released
                        true facts.Aliases facts.Borrows facts.Retained expr
                let rec unpack operations remaining =
                    match remaining with
                    | Let (id, operation, rest) -> unpack ((id, operation) :: operations) rest
                    | Return result -> { block with Operations = List.rev operations; Terminator = SSAANF.Return result }
                    | _ -> Crash.crash "SSA tail-call detection changed a block exit"
                unpack [] converted
            | _ -> block
        { func with Blocks = Map.map transform func.Blocks }
