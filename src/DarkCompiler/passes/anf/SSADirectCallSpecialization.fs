// SSADirectCallSpecialization.fs - Specialize direct calls on typed SSA blocks.

module SSADirectCallSpecialization

open ANF
open DirectCallFacts

module Facts = DirectCallFacts

type Specialization = {
    Functions: SSAANF.Function list
    CloneOrigins: Map<AST.FunctionId, AST.FunctionId>
}

let private operations (func: SSAANF.Function) =
    func.Blocks
    |> Map.toList
    |> List.collect (fun (_, block) -> block.Operations)

let private mapOperations transform (func: SSAANF.Function) =
    { func with
        Blocks =
            func.Blocks
            |> Map.map (fun _ block ->
                { block with Operations = List.map transform block.Operations }) }

let private exposeKnownIndirectTargets (func: SSAANF.Function) =
    mapOperations (fun (id, operation) -> id, Facts.exposeKnownIndirectCExpr operation) func

let private analyzeTerminator terminator analysis =
    match terminator with
    | SSAANF.Return atom -> Facts.analyzeAtom atom analysis
    | SSAANF.Jump (_, arguments) ->
        List.fold (fun state atom -> Facts.analyzeAtom atom state) analysis arguments
    | SSAANF.Branch (condition, _, _) -> Facts.analyzeAtom condition analysis

let private analyzeProgram functions =
    functions
    |> List.fold (fun analysis (func: SSAANF.Function) ->
        func.Blocks
        |> Map.fold (fun state _ block ->
            block.Operations
            |> List.fold (fun current (_, operation) ->
                Facts.analyzeCExpr operation current) state
            |> analyzeTerminator block.Terminator) analysis) Facts.emptyAnalysis

let private uniformLiteralAt index calls =
    let literals =
        calls
        |> List.map (fun args -> List.tryItem index args |> Option.bind Facts.scalarLiteralAtom)
    match literals with
    | Some first :: rest when rest |> List.forall (fun value -> value = Some first) ->
        Some (Facts.atomForScalarLiteral first)
    | _ -> None

let private buildRewriteMap
    (analysis: ProgramAnalysis)
    (functions: SSAANF.Function list) =
    functions
    |> List.choose (fun func ->
        match Map.tryFind func.Id analysis.DirectCalls with
        | None -> None
        | Some _ when Set.contains func.Id analysis.IndirectTargets -> None
        | Some calls ->
            let rewrites =
                func.TypedParams
                |> List.mapi (fun index parameter ->
                    match uniformLiteralAt index calls with
                    | Some literal when Facts.isScalarLiteralType parameter.Type ->
                        match Facts.scalarLiteralAtom literal with
                        | Some value when Facts.scalarLiteralMatchesType parameter.Type value ->
                            ReplaceParameterWith literal
                        | _ -> KeepParameter
                    | _ -> KeepParameter)
            if List.forall ((=) KeepParameter) rewrites then None
            else Some (func.Id, rewrites))
    |> Map.ofList

let private rewriteTerminator substitutions = function
    | SSAANF.Return atom -> SSAANF.Return (Facts.rewriteAtom substitutions atom)
    | SSAANF.Jump (target, arguments) ->
        SSAANF.Jump (target, List.map (Facts.rewriteAtom substitutions) arguments)
    | SSAANF.Branch (condition, yes, no) ->
        SSAANF.Branch (Facts.rewriteAtom substitutions condition, yes, no)

let private rewriteBody rewriteMap substitutions (func: SSAANF.Function) =
    { func with
        Blocks =
            func.Blocks
            |> Map.map (fun _ block ->
                { block with
                    Operations =
                        block.Operations
                        |> List.map (fun (id, operation) ->
                            id, Facts.rewriteCExpr rewriteMap substitutions operation)
                    Terminator = rewriteTerminator substitutions block.Terminator }) }

let private rewriteFunction rewriteMap (func: SSAANF.Function) =
    match Map.tryFind func.Id rewriteMap with
    | None -> rewriteBody rewriteMap Map.empty func
    | Some rewrites ->
        let rec pair parameters rewrites accumulated =
            match parameters, rewrites with
            | [], [] -> List.rev accumulated
            | parameter :: restParameters, rewrite :: restRewrites ->
                pair restParameters restRewrites ((parameter, rewrite) :: accumulated)
            | _ -> Crash.crash "SSA direct-call parameter rewrite count mismatch"
        let pairs = pair func.TypedParams rewrites []
        let parameters =
            pairs
            |> List.choose (fun (parameter, rewrite) ->
                match rewrite with
                | KeepParameter -> Some parameter
                | ReplaceParameterWith _ -> None)
        let substitutions =
            pairs
            |> List.choose (fun (parameter, rewrite) ->
                match rewrite with
                | ReplaceParameterWith literal -> Some (parameter.Id, literal)
                | KeepParameter -> None)
            |> Map.ofList
        { rewriteBody rewriteMap substitutions func with TypedParams = parameters }

// Definitions dominate their uses, so a CFG walk from entry sees every known
// construction before a valid use, including joins whose labels sort earlier.
let private knownValues (func: SSAANF.Function) =
    let rec walk visited known label =
        if Set.contains label visited then visited, known
        else
            match Map.tryFind label func.Blocks with
            | None -> Crash.crash "SSA direct-call facts: missing successor block"
            | Some block ->
                let visited = Set.add label visited
                let known =
                    block.Operations
                    |> List.fold (fun facts (id, operation) ->
                        match Facts.knownValueForCExpr facts operation with
                        | Some value -> Map.add id value facts
                        | None -> facts) known
                let successors =
                    match block.Terminator with
                    | SSAANF.Return _ -> []
                    | SSAANF.Jump (target, _) -> [target]
                    | SSAANF.Branch (_, yes, no) -> [yes; no]
                List.fold (fun (seen, facts) successor ->
                    walk seen facts successor) (visited, known) successors
    walk Set.empty Map.empty func.Entry |> snd

let private knownCallsInProgram functions =
    functions
    |> List.fold (fun calls func ->
        let known = knownValues func
        operations func
        |> List.fold (fun current (_, operation) ->
            match operation with
            | Call (name, arguments)
            | BorrowedCall (name, arguments)
            | TailCall (name, arguments) ->
                Facts.addKnownCall name arguments known current
            | _ -> current) calls) Map.empty

let private directCallsTo target (func: SSAANF.Function) =
    operations func
    |> List.choose (fun (_, operation) ->
        match operation with
        | Call (name, arguments)
        | BorrowedCall (name, arguments)
        | TailCall (name, arguments) when name = target -> Some arguments
        | _ -> None)

let private cloneableParameterIndices (func: SSAANF.Function) =
    let selfCalls = directCallsTo func.Id func
    func.TypedParams
    |> List.mapi (fun index parameter ->
        let eligible = Facts.isSpecializableValueType parameter.Type
        let passedThrough =
            selfCalls
            |> List.forall (fun arguments ->
                List.tryItem index arguments = Some (Var parameter.Id))
        if eligible && passedThrough then Some index else None)
    |> List.choose id
    |> Set.ofList

let private cloneGroups
    (analysis: ProgramAnalysis)
    knownCalls
    (functions: SSAANF.Function list) =
    functions
    |> List.choose (fun func ->
        match Map.tryFind func.Id knownCalls with
        | None -> None
        | Some _ when Set.contains func.Id analysis.IndirectTargets -> None
        | Some calls ->
            let eligibleIndices = cloneableParameterIndices func
            let recursiveCall = not (List.isEmpty (directCallsTo func.Id func))
            let valueBenefit value =
                match value with
                | LiteralValue _ -> 1
                | Int128Value _ | UInt128Value _ -> 2
                | TupleValue fields | RecordValue (_, fields) -> 1 + List.length fields
            let patterns =
                calls
                |> List.map (Facts.literalPatternAt eligibleIndices)
                |> List.map (List.filter (fun (index, value) ->
                    match List.tryItem index func.TypedParams with
                    | Some parameter -> Facts.knownValueMatchesType parameter.Type value
                    | None -> false))
                |> List.map (fun pattern ->
                    if recursiveCall then
                        pattern |> List.filter (fun (_, value) ->
                            match value with LiteralValue _ -> true | _ -> false)
                    else pattern)
                |> List.filter (not << List.isEmpty)
                |> List.countBy id
                |> List.sortBy (fun (pattern, occurrences) ->
                    let savedWork =
                        pattern
                        |> List.sumBy (snd >> valueBenefit)
                        |> (*) occurrences
                    -savedWork, pattern)
                |> List.map fst
                |> List.truncate Facts.maxLiteralClonesPerFunction
            if List.length patterns < 2 then None
            else Some (func.Id, func.Name, patterns))
    |> List.sortBy (fun (id, _, _) -> id)

let private routeFunction clonesByName (func: SSAANF.Function) =
    let known = knownValues func
    mapOperations
        (fun (id, operation) -> id, Facts.routeCExpr clonesByName known operation)
        func

let private atomUse used = function
    | Var id -> Set.add id used
    | _ -> used

let private terminatorUses used = function
    | SSAANF.Return atom -> atomUse used atom
    | SSAANF.Jump (_, arguments) -> List.fold atomUse used arguments
    | SSAANF.Branch (condition, _, _) -> atomUse used condition

let internal removeUnusedRematerializedValues (func: SSAANF.Function) =
    let definitions = operations func |> Map.ofList
    let removable =
        definitions
        |> Map.filter (fun _ operation ->
            Facts.isRematerializedValue operation
            || match operation with
               | ClosureAlloc _ | Atom _ | TypedAtom _ | IfValue _ -> true
               | _ -> false)
    let addUse counts id =
        let count = Map.tryFind id counts |> Option.defaultValue 0
        Map.add id (count + 1) counts
    let counts =
        func.Blocks
        |> Map.fold (fun counts _ block ->
            let counts =
                block.Operations
                |> List.fold (fun current (_, operation) ->
                    ANFEffects.cexprTempUses operation
                    |> Set.fold addUse current) counts
            terminatorUses Set.empty block.Terminator
            |> Set.fold addUse counts) Map.empty
    let initial =
        removable
        |> Map.keys
        |> Seq.filter (fun id -> Map.tryFind id counts |> Option.defaultValue 0 = 0)
        |> Set.ofSeq
    let rec eliminate counts removed pending =
        match pending |> Seq.tryHead with
        | None -> removed
        | Some id ->
            let pending = Set.remove id pending
            if Set.contains id removed
               || (Map.tryFind id counts |> Option.defaultValue 0) <> 0 then
                eliminate counts removed pending
            else
                let operation =
                    match Map.tryFind id removable with
                    | Some value -> value
                    | None -> Crash.crash "SSA direct-call cleanup lost a removable definition"
                let counts, pending =
                    ANFEffects.cexprTempUses operation
                    |> Set.fold (fun (counts, pending) usedId ->
                        let remaining =
                            (Map.tryFind usedId counts |> Option.defaultValue 0) - 1
                        let counts = Map.add usedId remaining counts
                        let pending =
                            if remaining = 0 && Map.containsKey usedId removable then
                                Set.add usedId pending
                            else pending
                        counts, pending) (counts, pending)
                eliminate counts (Set.add id removed) pending
    let removed = eliminate counts Set.empty initial
    { func with
        Blocks =
            func.Blocks
            |> Map.map (fun _ block ->
                { block with
                    Operations =
                        block.Operations
                        |> List.filter (fun (id, _) -> not (Set.contains id removed)) }) }

let private cloneFunction
    clonesByName
    (functionsById: Map<AST.FunctionId, SSAANF.Function>)
    (clone: LiteralClone) =
    let original: SSAANF.Function =
        match Map.tryFind clone.OriginalId functionsById with
        | Some func -> func
        | None -> Crash.crash $"Missing SSA direct-call clone source '{clone.OriginalId}'"
    let values = Map.ofList clone.Pattern
    let parameters =
        original.TypedParams
        |> List.mapi (fun index parameter ->
            if Map.containsKey index values then None else Some parameter)
        |> List.choose id
    let substitutions =
        original.TypedParams
        |> List.mapi (fun index parameter ->
            Map.tryFind index values
            |> Option.bind (function
                | LiteralValue literal ->
                    Some (parameter.Id, Facts.atomForScalarLiteral literal)
                | _ -> None))
        |> List.choose id
        |> Map.ofList
    let materializations =
        original.TypedParams
        |> List.mapi (fun index parameter ->
            Map.tryFind index values
            |> Option.bind (function
                | LiteralValue _ -> None
                | value -> Some (parameter.Id, Facts.cexprForKnownValue value)))
        |> List.choose id
    let rewritten = rewriteBody Map.empty substitutions original
    let blocks =
        rewritten.Blocks
        |> Map.change rewritten.Entry (function
            | Some entry ->
                Some { entry with Operations = materializations @ entry.Operations }
            | None -> Crash.crash "SSA direct-call clone has no entry block")
    { rewritten with
        Id = clone.CloneId
        Name = clone.CloneName
        TypedParams = parameters
        Blocks = blocks }
    |> routeFunction clonesByName

let specializeProgramWithFunctionNames functionNames functions =
    let exposed = List.map exposeKnownIndirectTargets functions
    let analysis = analyzeProgram exposed
    let rewriteMap = buildRewriteMap analysis exposed
    let rewritten = List.map (rewriteFunction rewriteMap) exposed
    let analysis = analyzeProgram rewritten
    let knownCalls = knownCallsInProgram rewritten
    let localIds = rewritten |> List.map (fun func -> func.Id) |> Set.ofList
    let idExists id = Set.contains id localIds || Map.containsKey id functionNames
    let clones =
        cloneGroups analysis knownCalls rewritten
        |> Facts.boundedCloneGroups
        |> Facts.buildLiteralClones idExists
    let clonesByName = clones |> List.groupBy (fun clone -> clone.OriginalId) |> Map.ofList
    let functionsById: Map<AST.FunctionId, SSAANF.Function> =
        rewritten |> List.map (fun func -> func.Id, func) |> Map.ofList
    let cloned = clones |> List.map (cloneFunction clonesByName functionsById)
    let routed = rewritten |> List.map (routeFunction clonesByName)
    {
        Functions =
            (cloned @ routed) |> List.map removeUnusedRematerializedValues
        CloneOrigins =
            clones |> List.map (fun clone -> clone.CloneId, clone.OriginalId) |> Map.ofList
    }

let reachableFrom (roots: Set<AST.FunctionId>) (functions: SSAANF.Function list) =
    let byId = functions |> List.map (fun func -> func.Id, func) |> Map.ofList
    let rec visit seen pending =
        match pending with
        | [] -> seen
        | id :: rest when Set.contains id seen -> visit seen rest
        | id :: rest ->
            let successors =
                match Map.tryFind id byId with
                | None -> []
                | Some func ->
                    let analysis = analyzeProgram [func]
                    (analysis.DirectCalls |> Map.keys |> Seq.toList)
                    @ (analysis.IndirectTargets |> Set.toList)
            visit (Set.add id seen) (successors @ rest)
    let reachable = visit Set.empty (Set.toList roots)
    functions |> List.filter (fun func -> Set.contains func.Id reachable)
