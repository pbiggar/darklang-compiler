// SSAHigherOrderSpecialization.fs - Specialize known callable arguments on typed SSA blocks.
//
// Callable facts are intersected at block parameters. Clones keep SSA value and
// block identities local to each function and replace closure calls with direct
// calls that receive captured values as ordinary parameters.

module SSAHigherOrderSpecialization

open ANF

type private Convention = ClosureValue | StaticFunction
type private Callable = {
    Target: AST.FunctionId
    Captures: Atom list
    Convention: Convention
}
type private KnownArgument = { Index: int; Callable: Callable }
type private Request = { Helper: AST.FunctionId; Arguments: KnownArgument list }
type private Shape = {
    Values: TypedParam list
    CaptureTypes: AST.SemanticType list
    ClosureId: TempId option
}
type private RewrittenCallable = {
    Target: AST.FunctionId
    Convention: Convention
    Captures: TypedParam list
}
type Specialization = {
    Functions: SSAANF.Function list
    CloneOrigins: Map<AST.FunctionId, AST.FunctionId>
}

let private maxPairs = 16
let private maxHelperNodes = 256
let private maxTargetNodes = 32

let private operations (func: SSAANF.Function) =
    func.Blocks |> Map.toList |> List.collect (fun (_, block) -> block.Operations)

let private tryAtom (known: Map<TempId, Callable>) (atom: Atom) : Callable option =
    match atom with
    | Var id -> Map.tryFind id known
    | FuncRef target -> Some { Target = target; Captures = []; Convention = StaticFunction }
    | _ -> None

let private instantiateReturn
    (definitions: Map<AST.FunctionId, SSAANF.Function>)
    (returns: Map<AST.FunctionId, Callable>) name (arguments: Atom list) =
    match Map.tryFind name definitions, Map.tryFind name returns with
    | Some func, Some callable when List.length func.TypedParams = List.length arguments ->
        let replacements =
            List.zip (func.TypedParams |> List.map (fun parameter -> parameter.Id)) arguments
            |> Map.ofList
        Some {
            callable with
                Captures =
                    callable.Captures
                    |> List.map (function
                        | Var id as atom -> Map.tryFind id replacements |> Option.defaultValue atom
                        | atom -> atom)
        }
    | _ -> None

let private tryOperation definitions returns (known: Map<TempId, Callable>) (operation: CExpr) : Callable option =
    match operation with
    | Atom atom | TypedAtom (atom, _) -> tryAtom known atom
    | IfValue (_, yes, no) ->
        match tryAtom known yes, tryAtom known no with
        | Some left, Some right when left = right -> Some left
        | _ -> None
    | ClosureAlloc (target, captures) ->
        Some { Target = target; Captures = captures; Convention = ClosureValue }
    | Call (name, arguments) | BorrowedCall (name, arguments) ->
        instantiateReturn definitions returns name arguments
    | _ -> None

let private evaluateBlock definitions returns known (block: SSAANF.Block) =
    block.Operations
    |> List.fold (fun facts (id, operation) ->
        match tryOperation definitions returns facts operation with
        | Some callable -> Map.add id callable facts
        | None -> Map.remove id facts) known

let private incomingFacts (func: SSAANF.Function) definitions returns facts =
    func.Blocks
    |> Map.toList
    |> List.collect (fun (label, block) ->
        match Map.tryFind label facts with
        | None -> []
        | Some known ->
            let after = evaluateBlock definitions returns known block
            let edge target arguments =
                match Map.tryFind target func.Blocks with
                | None -> Crash.crash "Higher-order specialization found a missing SSA block"
                | Some successor ->
                    let parameterFacts =
                        List.zip successor.Parameters arguments
                        |> List.choose (fun (parameter, atom) ->
                            tryAtom after atom |> Option.map (fun callable -> parameter.Id, callable))
                        |> Map.ofList
                    let inherited =
                        successor.Parameters
                        |> List.fold (fun facts parameter -> Map.remove parameter.Id facts) after
                    [target, Map.fold (fun facts id callable -> Map.add id callable facts) inherited parameterFacts]
            match block.Terminator with
            | SSAANF.Return _ -> []
            | SSAANF.Jump (target, arguments) -> edge target arguments
            | SSAANF.Branch (_, yes, no) -> edge yes [] @ edge no [])
    |> List.groupBy fst
    |> List.map (fun (label, inputs) ->
        let maps = List.map snd inputs
        let common =
            match maps with
            | [] -> Map.empty
            | first :: rest ->
                first
                |> Map.filter (fun id callable ->
                    rest |> List.forall (fun input -> Map.tryFind id input = Some callable))
        label, common)
    |> Map.ofList

let private blockFacts definitions returns (func: SSAANF.Function) =
    let rec solve remaining current =
        if remaining = 0 then current
        else
            let incoming = incomingFacts func definitions returns current
            let next =
                incoming |> Map.add func.Entry Map.empty
            if next = current then current else solve (remaining - 1) next
    solve (Map.count func.Blocks * 4 + 4) (Map.ofList [func.Entry, Map.empty])

let private knownAtOperations definitions returns func =
    let entries = blockFacts definitions returns func
    func.Blocks
    |> Map.toList
    |> List.choose (fun (label, block) ->
        Map.tryFind label entries
        |> Option.map (fun initial ->
            let _, sites =
                block.Operations
                |> List.fold (fun (known, sites) (id, operation) ->
                    let sites = Map.add id known sites
                    let known =
                        match tryOperation definitions returns known operation with
                        | Some callable -> Map.add id callable known
                        | None -> Map.remove id known
                    known, sites) (initial, Map.empty)
            label, sites))
    |> Map.ofList

let private returnFact definitions returns (func: SSAANF.Function) =
    let entries = blockFacts definitions returns func
    let returned =
        func.Blocks
        |> Map.toList
        |> List.choose (fun (label, block) ->
            match Map.tryFind label entries, block.Terminator with
            | Some known, SSAANF.Return atom ->
                let known = evaluateBlock definitions returns known block
                Some (tryAtom known atom)
            | _ -> None)
    let parameterIds = func.TypedParams |> List.map (fun parameter -> parameter.Id) |> Set.ofList
    match returned with
    | Some first :: rest
        when rest |> List.forall ((=) (Some first))
             && first.Captures |> List.forall (function
                 | Var id -> Set.contains id parameterIds
                 | _ -> true) -> Some first
    | _ -> None

let private buildReturns (definitions: Map<AST.FunctionId, SSAANF.Function>) =
    let rec solve remaining current =
        if remaining = 0 then current else
        let next =
            definitions
            |> Map.fold (fun facts name func ->
                match returnFact definitions facts func with
                | Some callable -> Map.add name callable facts
                | None -> facts) current
        if next = current then current else solve (remaining - 1) next
    solve (Map.count definitions + 1) Map.empty

let private shape (target: SSAANF.Function) (callable: Callable) =
    match callable.Convention with
    | StaticFunction -> Some { Values = target.TypedParams; CaptureTypes = []; ClosureId = None }
    | ClosureValue ->
        match target.TypedParams with
        | closure :: values ->
            match closure.Type with
            | AST.TTuple (AST.TInt64 :: captureTypes)
                when List.length captureTypes = List.length callable.Captures ->
                Some { Values = values; CaptureTypes = captureTypes; ClosureId = Some closure.Id }
            | _ -> None
        | [] -> None

let private atomUses id atom = ANFEffects.atomUsesTemp id atom
let private operationUses id operation = ANFEffects.cexprUsesTemp id operation
let private removeIndexes indexes items =
    items |> List.indexed |> List.choose (fun (index, item) ->
        if Set.contains index indexes then None else Some item)

let private terminatorUses id = function
    | SSAANF.Return atom -> atomUses id atom
    | SSAANF.Jump (_, arguments) -> List.exists (atomUses id) arguments
    | SSAANF.Branch (condition, _, _) -> atomUses id condition

let private targetUsesOnlyCaptures closureId captureCount (target: SSAANF.Function) =
    target.Blocks
    |> Map.forall (fun _ block ->
        block.Operations
        |> List.forall (fun (_, operation) ->
            match operation with
            | TupleGet (Var id, index) when id = closureId ->
                index >= 1 && index <= captureCount
            | _ -> not (operationUses closureId operation))
        && not (terminatorUses closureId block.Terminator))

let private helperUsesOnlyCalls (helper: SSAANF.Function) index parameterId =
    let allowed = function
        | ClosureCall (Var id, arguments) | ClosureTailCall (Var id, arguments)
            when id = parameterId && not (List.exists (atomUses parameterId) arguments) -> true
        | Call (name, arguments) | BorrowedCall (name, arguments) | TailCall (name, arguments)
            when name = helper.Id ->
            match List.tryItem index arguments with
            | Some (Var id) when id = parameterId ->
                arguments |> removeIndexes (Set.singleton index)
                |> List.exists (atomUses parameterId) |> not
            | _ -> false
        | operation -> not (operationUses parameterId operation)
    helper.Blocks
    |> Map.forall (fun _ block ->
        block.Operations |> List.forall (snd >> allowed)
        && not (terminatorUses parameterId block.Terminator))

let private closureCallArity parameterId (helper: SSAANF.Function) =
    operations helper
    |> List.tryPick (fun (_, operation) ->
        match operation with
        | ClosureCall (Var id, args) | ClosureTailCall (Var id, args) when id = parameterId ->
            Some (List.length args)
        | _ -> None)

let private validArgument
    (definitions: Map<AST.FunctionId, SSAANF.Function>)
    (helper: SSAANF.Function) (argument: KnownArgument) =
    match List.tryItem argument.Index helper.TypedParams,
          Map.tryFind argument.Callable.Target definitions with
    | Some parameter, Some target ->
        match shape target argument.Callable with
        | Some targetShape ->
            let targetOk =
                match targetShape.ClosureId with
                | Some id -> targetUsesOnlyCaptures id (List.length argument.Callable.Captures) target
                | None -> true
            Map.count target.Blocks + List.length (operations target) <= maxTargetNodes
            && targetOk
            && helperUsesOnlyCalls helper argument.Index parameter.Id
            && (closureCallArity parameter.Id helper
                |> Option.exists (fun arity -> List.length targetShape.Values = arity))
        | None -> false
    | _ -> false

let private knownArguments
    (definitions: Map<AST.FunctionId, SSAANF.Function>)
    (known: Map<TempId, Callable>) helperName (arguments: Atom list) =
    match Map.tryFind helperName definitions with
    | None -> []
    | Some helper ->
        arguments
        |> List.indexed
        |> List.choose (fun (index, atom) ->
            tryAtom known atom |> Option.map (fun callable -> { Index = index; Callable = callable }))
        |> List.filter (validArgument definitions helper)

let private directCall = function
    | Call (name, args) | BorrowedCall (name, args) | TailCall (name, args) -> Some (name, args)
    | _ -> None

let private key (request: Request) =
    request.Helper,
    (request.Arguments |> List.map (fun argument ->
        argument.Index, argument.Callable.Target, argument.Callable.Convention))

let private requestsInFunction definitions returns (func: SSAANF.Function) =
    let sites = knownAtOperations definitions returns func
    func.Blocks
    |> Map.toList
    |> List.collect (fun (label, block) ->
        block.Operations
        |> List.choose (fun (id, operation) ->
            match directCall operation with
            | None -> None
            | Some (helper, arguments) ->
                let known =
                    sites |> Map.tryFind label |> Option.bind (Map.tryFind id)
                    |> Option.defaultValue Map.empty
                match knownArguments definitions known helper arguments with
                | [] -> None
                | knownArguments -> Some { Helper = helper; Arguments = knownArguments }))

let private name (definitions: Map<AST.FunctionId, SSAANF.Function>) id =
    match Map.tryFind id definitions with
    | Some func -> func.Name
    | None -> Crash.crash "Higher-order specialization lost function display metadata"

let private targetName definitions id = $"{name definitions id}__captures"
let private helperName definitions request =
    let suffix =
        request.Arguments
        |> List.map (fun argument ->
            $"{AST.functionIdValue argument.Callable.Target}_{argument.Index}")
        |> String.concat "__"
    $"{name definitions request.Helper}__known_{suffix}"

let private newParameters types (func: SSAANF.Function) =
    let greatest =
        [ yield! func.FreshValueTypes |> Map.keys |> Seq.map (fun (TempId id) -> id)
          yield! func.TypedParams |> List.map (fun parameter -> let (TempId id) = parameter.Id in id) ]
        |> List.fold max 4000
    types
    |> List.mapi (fun index typ -> { Id = TempId (greatest + index + 1); Type = typ })

let private appendTypes parameters (func: SSAANF.Function) =
    { func with
        FreshValueTypes =
            parameters
            |> List.fold (fun types (parameter: TypedParam) ->
                Map.add parameter.Id parameter.Type types) func.FreshValueTypes }

let private mapOperations transform (func: SSAANF.Function) =
    { func with
        Blocks =
            func.Blocks |> Map.map (fun _ block ->
                { block with Operations = List.map transform block.Operations }) }

let private required definitions id =
    match Map.tryFind id definitions with
    | Some func -> func
    | None -> Crash.crash "Higher-order specialization lost a validated function"

let private generatedId generatedNames name =
    match Map.tryFind name generatedNames with
    | Some id -> id
    | None -> Crash.crash $"Higher-order specialization lost generated name '{name}'"

let private cloneTarget definitions generatedNames (callable: Callable) =
    let original = required definitions callable.Target
    let targetShape =
        match shape original callable with
        | Some value -> value
        | None -> Crash.crash "Higher-order specialization lost target shape"
    let closureId =
        match targetShape.ClosureId with
        | Some id -> id
        | None -> Crash.crash "Higher-order specialization expected a closure target"
    let captures = newParameters targetShape.CaptureTypes original
    let rewrite (id, operation) =
        let operation =
            match operation with
            | TupleGet (Var tupleId, index) when tupleId = closureId ->
                match List.tryItem (index - 1) captures with
                | Some parameter -> Atom (Var parameter.Id)
                | None -> Crash.crash "Higher-order specialization lost a capture"
            | _ -> operation
        id, operation
    let cloneName = targetName definitions callable.Target
    { mapOperations rewrite original with
        Id = generatedId generatedNames cloneName
        Name = cloneName
        TypedParams = captures @ targetShape.Values }
    |> appendTypes captures

let private cloneHelper definitions generatedNames (request: Request) =
    let original = required definitions request.Helper
    let indexed =
        request.Arguments
        |> List.map (fun argument ->
            let target = required definitions argument.Callable.Target
            let targetShape =
                match shape target argument.Callable with
                | Some value -> value
                | None -> Crash.crash "Higher-order specialization lost callable shape"
            argument, targetShape.CaptureTypes)
    let allTypes = indexed |> List.collect snd
    let captures = newParameters allTypes original
    let _, rewritten =
        indexed
        |> List.fold (fun (remaining, mapped) (argument, types) ->
            let indexedParameters = remaining |> List.indexed
            let own = indexedParameters |> List.choose (fun (index, parameter) ->
                if index < List.length types then Some parameter else None)
            let rest = indexedParameters |> List.choose (fun (index, parameter) ->
                if index >= List.length types then Some parameter else None)
            let parameter =
                match List.tryItem argument.Index original.TypedParams with
                | Some value -> value
                | None -> Crash.crash "Higher-order specialization lost helper parameter"
            let target =
                match argument.Callable.Convention with
                | StaticFunction -> argument.Callable.Target
                | ClosureValue ->
                    targetName definitions argument.Callable.Target
                    |> generatedId generatedNames
            let value = { Target = target; Convention = argument.Callable.Convention; Captures = own }
            rest, Map.add parameter.Id value mapped) (captures, Map.empty)
    let cloneName = helperName definitions request
    let cloneId = generatedId generatedNames cloneName
    let indexes = request.Arguments |> List.map (fun argument -> argument.Index) |> Set.ofList
    let appended = captures |> List.map (fun parameter -> Var parameter.Id)
    let rewrite (id, operation) =
        let operation =
            match operation with
            | ClosureCall (Var parameter, arguments)
            | ClosureTailCall (Var parameter, arguments) ->
                match Map.tryFind parameter rewritten with
                | None -> operation
                | Some callable ->
                    let captureArgs =
                        match callable.Convention with
                        | ClosureValue -> callable.Captures |> List.map (fun parameter -> Var parameter.Id)
                        | StaticFunction -> []
                    match operation with
                    | ClosureTailCall _ -> TailCall (callable.Target, captureArgs @ arguments)
                    | _ -> Call (callable.Target, captureArgs @ arguments)
            | Call (name, arguments) when name = original.Id ->
                Call (cloneId, removeIndexes indexes arguments @ appended)
            | BorrowedCall (name, arguments) when name = original.Id ->
                BorrowedCall (cloneId, removeIndexes indexes arguments @ appended)
            | TailCall (name, arguments) when name = original.Id ->
                TailCall (cloneId, removeIndexes indexes arguments @ appended)
            | _ -> operation
        id, operation
    { mapOperations rewrite original with
        Id = cloneId
        Name = cloneName
        TypedParams = removeIndexes indexes original.TypedParams @ captures }
    |> appendTypes captures

let private rewriteKnownCalls definitions returns generatedNames requests (func: SSAANF.Function) =
    let sites = knownAtOperations definitions returns func
    let specialized = requests |> List.map (fun request -> key request, generatedId generatedNames (helperName definitions request)) |> Map.ofList
    let rewritten =
        { func with
            Blocks =
                func.Blocks
                |> Map.map (fun label block ->
                    let operations =
                        block.Operations
                        |> List.map (fun (id, operation) ->
                            let known =
                                sites |> Map.tryFind label |> Option.bind (Map.tryFind id)
                                |> Option.defaultValue Map.empty
                            let rewritten =
                                match directCall operation with
                                | None -> operation
                                | Some (helper, arguments) ->
                                    let knownArgs = knownArguments definitions known helper arguments
                                    let request = { Helper = helper; Arguments = knownArgs }
                                    match Map.tryFind (key request) specialized with
                                    | None -> operation
                                    | Some target ->
                                        let indexes = knownArgs |> List.map (fun argument -> argument.Index) |> Set.ofList
                                        let args =
                                            removeIndexes indexes arguments
                                            @ (knownArgs |> List.collect (fun argument -> argument.Callable.Captures))
                                        match operation with
                                        | Call _ -> Call (target, args)
                                        | BorrowedCall _ -> BorrowedCall (target, args)
                                        | TailCall _ -> TailCall (target, args)
                                        | _ -> operation
                            id, rewritten)
                    { block with Operations = operations }) }
    // Closure allocations and aliases made dead by routing are removed by the
    // following SSA cleanup. Preserve the ownership-visible operation order.
    SSADirectCallSpecialization.removeUnusedRematerializedValues rewritten

let specializeProgramWithExternalFunctionsAndNames
    (reservedNames: Map<AST.FunctionId, string>)
    (externalFunctions: SSAANF.Function list)
    (functions: SSAANF.Function list) =
    let definitions =
        externalFunctions @ functions
        |> List.map (fun func -> func.Id, func)
        |> Map.ofList
    let returns = buildReturns definitions
    let requests =
        functions
        |> List.collect (requestsInFunction definitions returns)
        |> List.filter (fun request ->
            let helper = required definitions request.Helper
            Map.count helper.Blocks + List.length (operations helper) <= maxHelperNodes)
        |> List.distinctBy key |> List.sortBy key
        |> List.fold (fun (remaining, retained) request ->
            let cost = List.length request.Arguments
            if cost <= remaining then remaining - cost, request :: retained
            else remaining, retained) (maxPairs, [])
        |> snd |> List.rev
    let targetNames =
        requests |> List.collect (fun request -> request.Arguments)
        |> List.filter (fun argument -> argument.Callable.Convention = ClosureValue)
        |> List.map (fun argument -> targetName definitions argument.Callable.Target)
        |> Set.ofList
    let helperNames = requests |> List.map (helperName definitions) |> Set.ofList
    let exists name =
        let id = AST.functionIdForName name
        Map.containsKey id definitions || Map.containsKey id reservedNames
    let usable =
        requests
        |> List.filter (fun request ->
            let helper = helperName definitions request
            let targets =
                request.Arguments
                |> List.filter (fun argument -> argument.Callable.Convention = ClosureValue)
                |> List.map (fun argument -> targetName definitions argument.Callable.Target)
            not (exists helper)
            && not (Set.contains helper targetNames)
            && (targets |> List.forall (fun target ->
                not (exists target) && not (Set.contains target helperNames))))
    let generatedNames =
        Set.union targetNames helperNames
        |> Seq.map (fun name -> name, AST.functionIdForName name)
        |> Map.ofSeq
    let targetCallables =
        usable |> List.collect (fun request -> request.Arguments)
        |> List.map (fun argument -> argument.Callable)
        |> List.filter (fun callable -> callable.Convention = ClosureValue)
        |> List.distinctBy (fun callable -> callable.Target)
    let targets = targetCallables |> List.map (cloneTarget definitions generatedNames)
    let helpers = usable |> List.map (cloneHelper definitions generatedNames)
    let rewritten =
        functions |> List.map (rewriteKnownCalls definitions returns generatedNames usable)
    let origins =
        (targetCallables
         |> List.map (fun callable ->
             generatedId generatedNames (targetName definitions callable.Target), callable.Target))
        @ (usable
           |> List.map (fun request ->
               generatedId generatedNames (helperName definitions request), request.Helper))
        |> Map.ofList
    { Functions = targets @ rewritten @ helpers; CloneOrigins = origins }
