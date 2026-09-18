// ANF_HigherOrderSpecialization.fs - Specialize statically known callable arguments.
//
// Callable facts flow through aliases, branch values, joins, and function
// returns. One bounded helper clone jointly specializes all known functional
// arguments, passing closure captures directly or calling static references.

module ANF_HigherOrderSpecialization

open ANF

type private CallableConvention = ClosureValue | StaticFunction

type private KnownCallable = {
    TargetName: string
    Captures: Atom list
    Convention: CallableConvention
}

type private KnownArgument = { Index: int; Callable: KnownCallable }

type private SpecializationRequest = {
    HelperName: string
    KnownArguments: KnownArgument list
}

type private TargetShape = {
    ValueParameters: TypedParam list
    CaptureTypes: AST.Type list
    ClosureParameter: TempId option
}

type private RewrittenCallable = {
    TargetName: string
    Convention: CallableConvention
    CaptureParameters: TypedParam list
}

let private maxSpecializedPairs = 16
let private maxHelperNodes = 256
let private maxTargetNodes = 32

let private functionMap functions =
    functions |> List.map (fun func -> (func.Name, func)) |> Map.ofList

let private mergeFunctionMaps externalFunctions localFunctions =
    localFunctions
    |> List.fold (fun definitions func -> Map.add func.Name func definitions) (functionMap externalFunctions)

let private directCallNames = function
    | Call (name, args) | BorrowedCall (name, args) | TailCall (name, args) -> Some(name, args)
    | _ -> None

let private tryKnownAtom known = function
    | Var id -> Map.tryFind id known
    | FuncRef targetName ->
        Some { TargetName = targetName; Captures = []; Convention = StaticFunction }
    | _ -> None

let private rewriteAtomWithArguments parameters arguments atom =
    let replacements =
        List.zip (parameters |> List.map (fun parameter -> parameter.Id)) arguments
        |> Map.ofList
    match atom with
    | Var id -> Map.tryFind id replacements |> Option.defaultValue atom
    | _ -> atom

let private instantiateReturnedCallable definitions returnFacts name arguments =
    match Map.tryFind name definitions, Map.tryFind name returnFacts with
    | Some func, Some callable when List.length func.TypedParams = List.length arguments ->
        Some {
            callable with
                Captures = callable.Captures |> List.map (rewriteAtomWithArguments func.TypedParams arguments)
        }
    | _ -> None

let private tryKnownCExpr definitions returnFacts known = function
    | Atom atom | TypedAtom (atom, _) -> tryKnownAtom known atom
    | IfValue (_, thenValue, elseValue) ->
        match tryKnownAtom known thenValue, tryKnownAtom known elseValue with
        | Some left, Some right when left = right -> Some left
        | _ -> None
    | ClosureAlloc (targetName, captures) ->
        Some { TargetName = targetName; Captures = captures; Convention = ClosureValue }
    | Call (name, arguments) | BorrowedCall (name, arguments) ->
        instantiateReturnedCallable definitions returnFacts name arguments
    | _ -> None

let private knownAfterBinding definitions returnFacts known boundId cexpr =
    match tryKnownCExpr definitions returnFacts known cexpr with
    | Some callable -> Map.add boundId callable known
    | None -> Map.remove boundId known

let rec private collectJumpFacts definitions returnFacts target known expr =
    match expr with
    | Jump (jumpTarget, atom) when jumpTarget = target -> [tryKnownAtom known atom]
    | Jump _ | Return _ -> []
    | Let (boundId, cexpr, body) ->
        collectJumpFacts definitions returnFacts target
            (knownAfterBinding definitions returnFacts known boundId cexpr) body
    | Join (parameter, continuation, entry) ->
        collectJumpFacts definitions returnFacts target (Map.remove parameter.Id known) continuation
        @ collectJumpFacts definitions returnFacts target known entry
    | If (_, thenBranch, elseBranch) ->
        collectJumpFacts definitions returnFacts target known thenBranch
        @ collectJumpFacts definitions returnFacts target known elseBranch

let private tryJoinCallable definitions returnFacts parameter known entry =
    match collectJumpFacts definitions returnFacts parameter.Id known entry with
    | Some first :: rest when rest |> List.forall (fun candidate -> candidate = Some first) -> Some first
    | _ -> None

let private collectReturnedCallables definitions returnFacts expr =
    let rec collect known current =
        match current with
        | Jump _ -> Some []
        | Return atom -> tryKnownAtom known atom |> Option.map List.singleton
        | Let (boundId, cexpr, body) ->
            collect (knownAfterBinding definitions returnFacts known boundId cexpr) body
        | Join (parameter, continuation, entry) ->
            let continuationKnown =
                match tryJoinCallable definitions returnFacts parameter known entry with
                | Some callable -> Map.add parameter.Id callable known
                | None -> Map.remove parameter.Id known
            match collect continuationKnown continuation, collect known entry with
            | Some left, Some right -> Some(left @ right)
            | _ -> None
        | If (_, thenBranch, elseBranch) ->
            match collect known thenBranch, collect known elseBranch with
            | Some left, Some right -> Some(left @ right)
            | _ -> None
    match collect Map.empty expr with
    | Some (first :: rest) when rest |> List.forall ((=) first) -> Some first
    | _ -> None

let private buildReturnFacts definitions =
    let summaryUsesOnlyParameters (func: Function) (callable: KnownCallable) =
        let parameterIds = func.TypedParams |> List.map (fun parameter -> parameter.Id) |> Set.ofList
        callable.Captures
        |> List.forall (function
            | Var id -> Set.contains id parameterIds
            | _ -> true)
    let rec solve remaining current =
        if remaining = 0 then current else
        let next =
            definitions
            |> Map.fold (fun facts name func ->
                match collectReturnedCallables definitions facts func.Body with
                | Some callable when summaryUsesOnlyParameters func callable ->
                    Map.add name callable facts
                | _ -> facts) current
        if next = current then current else solve (remaining - 1) next
    solve (Map.count definitions + 1) Map.empty

let private countNodes expr =
    let rec count current = function
        | Jump _ | Return _ -> current + 1
        | Let (_, _, body) -> count (current + 1) body
        | Join (_, continuation, entry) -> current + 1 + count 0 continuation + count 0 entry
        | If (_, thenBranch, elseBranch) -> current + 1 + count 0 thenBranch + count 0 elseBranch
    count 0 expr

let private targetShape (target: Function) (callable: KnownCallable) =
    match callable.Convention with
    | StaticFunction ->
        Some { ValueParameters = target.TypedParams; CaptureTypes = []; ClosureParameter = None }
    | ClosureValue ->
        match target.TypedParams with
        | closureParameter :: valueParameters ->
            match closureParameter.Type with
            | AST.TTuple (AST.TInt64 :: captureTypes)
                when List.length captureTypes = List.length callable.Captures ->
                Some {
                    ValueParameters = valueParameters
                    CaptureTypes = captureTypes
                    ClosureParameter = Some closureParameter.Id
                }
            | _ -> None
        | [] -> None

let rec private targetBodyUsesOnlyCaptures closureId captureCount expr =
    let captureAccess = function
        | TupleGet (Var tupleId, index) when tupleId = closureId -> index >= 1 && index <= captureCount
        | cexpr -> not (ANFEffects.cexprUsesTemp closureId cexpr)
    match expr with
    | Jump (_, atom) | Return atom -> not (ANFEffects.atomUsesTemp closureId atom)
    | Let (_, cexpr, body) -> captureAccess cexpr && targetBodyUsesOnlyCaptures closureId captureCount body
    | Join (_, continuation, entry) ->
        targetBodyUsesOnlyCaptures closureId captureCount continuation
        && targetBodyUsesOnlyCaptures closureId captureCount entry
    | If (condition, thenBranch, elseBranch) ->
        not (ANFEffects.atomUsesTemp closureId condition)
        && targetBodyUsesOnlyCaptures closureId captureCount thenBranch
        && targetBodyUsesOnlyCaptures closureId captureCount elseBranch

let private closureCallArity functionParameterId expr =
    let rec find = function
        | Jump _ | Return _ -> None
        | Let (_, cexpr, body) ->
            match cexpr with
            | ClosureCall (Var id, args) | ClosureTailCall (Var id, args) when id = functionParameterId ->
                Some(List.length args)
            | _ -> find body
        | Join (_, continuation, entry) ->
            match find entry with Some arity -> Some arity | None -> find continuation
        | If (_, thenBranch, elseBranch) ->
            match find thenBranch with Some arity -> Some arity | None -> find elseBranch
    find expr

let private removeIndexes indexes items =
    items |> List.indexed |> List.choose (fun (index, item) ->
        if Set.contains index indexes then None else Some item)

let rec private helperUsesParameterOnlyForClosureOperations helperName parameterId argumentIndex expr =
    let allowed = function
        | ClosureCall (Var id, args) | ClosureTailCall (Var id, args)
            when id = parameterId && not (ANFEffects.atomsUseTemp parameterId args) -> true
        | Call (name, args) | BorrowedCall (name, args) | TailCall (name, args) when name = helperName ->
            match List.tryItem argumentIndex args with
            | Some (Var id) when id = parameterId ->
                args |> removeIndexes (Set.singleton argumentIndex) |> ANFEffects.atomsUseTemp parameterId |> not
            | _ -> false
        | cexpr -> not (ANFEffects.cexprUsesTemp parameterId cexpr)
    match expr with
    | Jump (_, atom) | Return atom -> not (ANFEffects.atomUsesTemp parameterId atom)
    | Let (_, cexpr, body) ->
        allowed cexpr && helperUsesParameterOnlyForClosureOperations helperName parameterId argumentIndex body
    | Join (_, continuation, entry) ->
        helperUsesParameterOnlyForClosureOperations helperName parameterId argumentIndex continuation
        && helperUsesParameterOnlyForClosureOperations helperName parameterId argumentIndex entry
    | If (condition, thenBranch, elseBranch) ->
        not (ANFEffects.atomUsesTemp parameterId condition)
        && helperUsesParameterOnlyForClosureOperations helperName parameterId argumentIndex thenBranch
        && helperUsesParameterOnlyForClosureOperations helperName parameterId argumentIndex elseBranch

let private validKnownArgument definitions helper (argument: KnownArgument) =
    match List.tryItem argument.Index helper.TypedParams, Map.tryFind argument.Callable.TargetName definitions with
    | Some helperParameter, Some target ->
        match targetShape target argument.Callable with
        | Some shape ->
            let targetBodyOk =
                match shape.ClosureParameter with
                | Some closureId ->
                    targetBodyUsesOnlyCaptures closureId (List.length argument.Callable.Captures) target.Body
                | None -> true
            countNodes target.Body <= maxTargetNodes
            && targetBodyOk
            && helperUsesParameterOnlyForClosureOperations helper.Name helperParameter.Id argument.Index helper.Body
            && (closureCallArity helperParameter.Id helper.Body
                |> Option.exists (fun arity -> List.length shape.ValueParameters = arity))
        | None -> false
    | _ -> false

let private knownArguments definitions known helperName arguments =
    match Map.tryFind helperName definitions with
    | Some helper ->
        arguments
        |> List.indexed
        |> List.choose (fun (index, atom) ->
            tryKnownAtom known atom |> Option.map (fun callable -> { Index = index; Callable = callable }))
        |> List.filter (validKnownArgument definitions helper)
    | None -> []

let private requestKey request =
    (request.HelperName,
     request.KnownArguments |> List.map (fun argument ->
         (argument.Index, argument.Callable.TargetName, argument.Callable.Convention)))

let rec private collectRequests definitions returnFacts expr known requests =
    match expr with
    | Jump _ | Return _ -> requests
    | Let (boundId, cexpr, body) ->
        let withCall =
            match directCallNames cexpr with
            | Some (helperName, arguments) ->
                match knownArguments definitions known helperName arguments with
                | [] -> requests
                | knownForCall -> { HelperName = helperName; KnownArguments = knownForCall } :: requests
            | None -> requests
        collectRequests definitions returnFacts body
            (knownAfterBinding definitions returnFacts known boundId cexpr) withCall
    | Join (parameter, continuation, entry) ->
        let continuationKnown =
            match tryJoinCallable definitions returnFacts parameter known entry with
            | Some callable -> Map.add parameter.Id callable known
            | None -> Map.remove parameter.Id known
        collectRequests definitions returnFacts entry known requests
        |> collectRequests definitions returnFacts continuation continuationKnown
    | If (_, thenBranch, elseBranch) ->
        collectRequests definitions returnFacts thenBranch known requests
        |> collectRequests definitions returnFacts elseBranch known

let private withinPairBudget requests =
    requests
    |> List.fold (fun (remaining, retained) request ->
        let cost = List.length request.KnownArguments
        if cost <= remaining then (remaining - cost, request :: retained) else (remaining, retained))
        (maxSpecializedPairs, [])
    |> snd |> List.rev

let rec private greatestTempId expr current =
    let atomId atom value = match atom with Var (TempId id) -> max id value | _ -> value
    match expr with
    | Jump (TempId target, atom) -> atomId atom (max target current)
    | Join (parameter, continuation, entry) ->
        let (TempId id) = parameter.Id
        greatestTempId entry (greatestTempId continuation (max id current))
    | Return atom -> atomId atom current
    | Let (TempId boundId, _, body) -> greatestTempId body (max current boundId)
    | If (_, thenBranch, elseBranch) -> greatestTempId elseBranch (greatestTempId thenBranch current)

let private freshVarGen functions main =
    let parameterValue parameter = let (TempId value) = parameter.Id in value
    functions
    |> List.fold (fun current func ->
        func.TypedParams |> List.fold (fun value parameter -> max value (parameterValue parameter)) current
        |> greatestTempId func.Body) 0
    |> greatestTempId main
    |> fun greatest -> VarGen (greatest + 1)

let private makeCaptureParameters captureTypes varGen =
    captureTypes
    |> List.fold (fun (parameters, current) typ ->
        let (id, next) = freshVar current
        ({ Id = id; Type = typ } :: parameters, next)) ([], varGen)
    |> fun (parameters, finalVarGen) -> (List.rev parameters, finalVarGen)

let rec private rewriteTargetBody closureId captureParameters expr =
    let rewriteCExpr cexpr =
        match cexpr with
        | TupleGet (Var tupleId, index) when tupleId = closureId ->
            captureParameters |> List.tryItem (index - 1)
            |> Option.map (fun parameter -> Atom (Var parameter.Id))
            |> Option.defaultValue cexpr
        | _ -> cexpr
    match expr with
    | Jump _ | Return _ -> expr
    | Let (boundId, cexpr, body) ->
        Let (boundId, rewriteCExpr cexpr, rewriteTargetBody closureId captureParameters body)
    | Join (parameter, continuation, entry) ->
        Join (parameter, rewriteTargetBody closureId captureParameters continuation,
              rewriteTargetBody closureId captureParameters entry)
    | If (condition, thenBranch, elseBranch) ->
        If (condition, rewriteTargetBody closureId captureParameters thenBranch,
            rewriteTargetBody closureId captureParameters elseBranch)

let private specializedTargetName targetName = $"{targetName}__captures"

let private specializedHelperName request =
    let suffix =
        request.KnownArguments
        |> List.map (fun argument -> $"{argument.Callable.TargetName}_{argument.Index}")
        |> String.concat "__"
    $"{request.HelperName}__known_{suffix}"

let rec private rewriteHelperBody
    helperName
    cloneName
    rewrittenCallables
    argumentIndexes
    appendedCaptureParameters
    expr =
    let appendedCaptures =
        appendedCaptureParameters |> List.map (fun parameter -> Var parameter.Id)
    let directTarget closureId arguments isTail original =
        match Map.tryFind closureId rewrittenCallables with
        | Some callable ->
            let captures =
                match callable.Convention with
                | ClosureValue -> callable.CaptureParameters |> List.map (fun parameter -> Var parameter.Id)
                | StaticFunction -> []
            if isTail then TailCall (callable.TargetName, captures @ arguments)
            else Call (callable.TargetName, captures @ arguments)
        | None -> original
    let rewriteCExpr cexpr =
        match cexpr with
        | ClosureCall (Var id, arguments) -> directTarget id arguments false cexpr
        | ClosureTailCall (Var id, arguments) -> directTarget id arguments true cexpr
        | Call (name, arguments) when name = helperName ->
            Call (cloneName, removeIndexes argumentIndexes arguments @ appendedCaptures)
        | BorrowedCall (name, arguments) when name = helperName ->
            BorrowedCall (cloneName, removeIndexes argumentIndexes arguments @ appendedCaptures)
        | TailCall (name, arguments) when name = helperName ->
            TailCall (cloneName, removeIndexes argumentIndexes arguments @ appendedCaptures)
        | _ -> cexpr
    match expr with
    | Jump _ | Return _ -> expr
    | Let (boundId, cexpr, body) ->
        Let (boundId, rewriteCExpr cexpr,
             rewriteHelperBody helperName cloneName rewrittenCallables argumentIndexes appendedCaptureParameters body)
    | Join (parameter, continuation, entry) ->
        Join (parameter,
              rewriteHelperBody helperName cloneName rewrittenCallables argumentIndexes appendedCaptureParameters continuation,
              rewriteHelperBody helperName cloneName rewrittenCallables argumentIndexes appendedCaptureParameters entry)
    | If (condition, thenBranch, elseBranch) ->
        If (condition,
            rewriteHelperBody helperName cloneName rewrittenCallables argumentIndexes appendedCaptureParameters thenBranch,
            rewriteHelperBody helperName cloneName rewrittenCallables argumentIndexes appendedCaptureParameters elseBranch)

let rec private exprUsesTemp tempId = function
    | Jump (_, atom) | Return atom -> ANFEffects.atomUsesTemp tempId atom
    | Let (_, cexpr, body) -> ANFEffects.cexprUsesTemp tempId cexpr || exprUsesTemp tempId body
    | Join (parameter, continuation, entry) ->
        exprUsesTemp tempId entry || (parameter.Id <> tempId && exprUsesTemp tempId continuation)
    | If (condition, thenBranch, elseBranch) ->
        ANFEffects.atomUsesTemp tempId condition
        || exprUsesTemp tempId thenBranch || exprUsesTemp tempId elseBranch

let rec private rewriteKnownCalls definitions returnFacts specializedNames known expr =
    let rewriteCExpr cexpr =
        match directCallNames cexpr with
        | Some (helperName, arguments) ->
            let knownForCall = knownArguments definitions known helperName arguments
            let key =
                (helperName, knownForCall |> List.map (fun argument ->
                    (argument.Index, argument.Callable.TargetName, argument.Callable.Convention)))
            match Map.tryFind key specializedNames with
            | Some newName ->
                let indexes = knownForCall |> List.map (fun argument -> argument.Index) |> Set.ofList
                let newArguments =
                    removeIndexes indexes arguments
                    @ (knownForCall |> List.collect (fun argument -> argument.Callable.Captures))
                match cexpr with
                | Call _ -> Call (newName, newArguments)
                | BorrowedCall _ -> BorrowedCall (newName, newArguments)
                | TailCall _ -> TailCall (newName, newArguments)
                | _ -> cexpr
            | None -> cexpr
        | None -> cexpr
    match expr with
    | Jump _ | Return _ -> expr
    | Let (boundId, cexpr, body) ->
        let knownAfter = knownAfterBinding definitions returnFacts known boundId cexpr
        let rewrittenBody = rewriteKnownCalls definitions returnFacts specializedNames knownAfter body
        let removable =
            match cexpr with
            | Atom _ | TypedAtom _ | IfValue _ | ClosureAlloc _ ->
                tryKnownCExpr definitions returnFacts known cexpr |> Option.isSome
            | _ -> false
        if removable && not (exprUsesTemp boundId rewrittenBody) then rewrittenBody
        else Let (boundId, rewriteCExpr cexpr, rewrittenBody)
    | Join (parameter, continuation, entry) ->
        let continuationKnown =
            match tryJoinCallable definitions returnFacts parameter known entry with
            | Some callable -> Map.add parameter.Id callable known
            | None -> Map.remove parameter.Id known
        Join (parameter,
              rewriteKnownCalls definitions returnFacts specializedNames continuationKnown continuation,
              rewriteKnownCalls definitions returnFacts specializedNames known entry)
    | If (condition, thenBranch, elseBranch) ->
        If (condition,
            rewriteKnownCalls definitions returnFacts specializedNames known thenBranch,
            rewriteKnownCalls definitions returnFacts specializedNames known elseBranch)

let private requiredFunction name definitions =
    match Map.tryFind name definitions with
    | Some func -> func
    | None -> Crash.crash $"Higher-order specialization lost validated function '{name}'"

let specializeProgramWithExternalFunctions externalFunctions (Program (functions, main)) =
    let definitions = mergeFunctionMaps externalFunctions functions
    let returnFacts = buildReturnFacts definitions
    let rawRequests =
        functions
        |> List.fold (fun requests func -> collectRequests definitions returnFacts func.Body Map.empty requests) []
        |> fun requests -> collectRequests definitions returnFacts main Map.empty requests
    let requests =
        rawRequests
        |> List.filter (fun request ->
            Map.tryFind request.HelperName definitions
            |> Option.exists (fun helper -> countNodes helper.Body <= maxHelperNodes))
        |> List.distinctBy requestKey |> List.sortBy requestKey |> withinPairBudget
    let existingNames = definitions |> Map.keys |> Set.ofSeq
    let targetCloneNames =
        requests |> List.collect (fun request -> request.KnownArguments)
        |> List.filter (fun argument -> argument.Callable.Convention = ClosureValue)
        |> List.map (fun argument -> specializedTargetName argument.Callable.TargetName) |> Set.ofList
    let helperCloneNames = requests |> List.map specializedHelperName |> Set.ofList
    let usableRequests =
        requests
        |> List.filter (fun request ->
            let helperName = specializedHelperName request
            let targets =
                request.KnownArguments
                |> List.filter (fun argument -> argument.Callable.Convention = ClosureValue)
                |> List.map (fun argument -> specializedTargetName argument.Callable.TargetName)
            not (Set.contains helperName existingNames)
            && not (Set.contains helperName targetCloneNames)
            && (targets |> List.forall (fun name ->
                not (Set.contains name existingNames) && not (Set.contains name helperCloneNames))))

    let allDefinitions = definitions |> Map.values |> Seq.toList
    let targetCallables =
        usableRequests |> List.collect (fun request -> request.KnownArguments)
        |> List.map (fun argument -> argument.Callable)
        |> List.filter (fun callable -> callable.Convention = ClosureValue)
        |> List.distinctBy (fun callable -> callable.TargetName)
    let (targetClones, varGenAfterTargets) =
        targetCallables
        |> List.fold (fun (clones, currentVarGen) callable ->
            let target = requiredFunction callable.TargetName definitions
            match targetShape target callable with
            | Some shape ->
                match shape.ClosureParameter with
                | Some closureId ->
                    let (captureParameters, nextVarGen) = makeCaptureParameters shape.CaptureTypes currentVarGen
                    let clone = {
                        target with
                            Name = specializedTargetName target.Name
                            TypedParams = captureParameters @ shape.ValueParameters
                            Body = rewriteTargetBody closureId captureParameters target.Body
                    }
                    (clone :: clones, nextVarGen)
                | None -> Crash.crash $"Expected closure target shape for '{target.Name}'"
            | None -> Crash.crash $"Higher-order specialization lost target shape '{target.Name}'")
            ([], freshVarGen allDefinitions main)

    let (helperClones, _) =
        usableRequests
        |> List.fold (fun (clones, currentVarGen) request ->
            let helper = requiredFunction request.HelperName definitions
            let (rewrittenCallables, captureParameters, nextVarGen) =
                request.KnownArguments
                |> List.fold (fun (rewritten, allCaptures, varGen) argument ->
                    let helperParameter =
                        match List.tryItem argument.Index helper.TypedParams with
                        | Some parameter -> parameter
                        | None -> Crash.crash $"Lost helper parameter {argument.Index}"
                    let target = requiredFunction argument.Callable.TargetName definitions
                    let shape =
                        match targetShape target argument.Callable with
                        | Some value -> value
                        | None -> Crash.crash $"Lost target shape '{target.Name}'"
                    let (captures, next) = makeCaptureParameters shape.CaptureTypes varGen
                    let targetName =
                        match argument.Callable.Convention with
                        | ClosureValue -> specializedTargetName argument.Callable.TargetName
                        | StaticFunction -> argument.Callable.TargetName
                    let rewrittenCallable = {
                        TargetName = targetName
                        Convention = argument.Callable.Convention
                        CaptureParameters = captures
                    }
                    (Map.add helperParameter.Id rewrittenCallable rewritten, allCaptures @ captures, next))
                    (Map.empty, [], currentVarGen)
            let indexes = request.KnownArguments |> List.map (fun argument -> argument.Index) |> Set.ofList
            let cloneName = specializedHelperName request
            let clone = {
                helper with
                    Name = cloneName
                    TypedParams = removeIndexes indexes helper.TypedParams @ captureParameters
                    Body =
                        rewriteHelperBody
                            helper.Name
                            cloneName
                            rewrittenCallables
                            indexes
                            captureParameters
                            helper.Body
            }
            (clone :: clones, nextVarGen)) ([], varGenAfterTargets)

    let specializedNames =
        usableRequests |> List.map (fun request -> (requestKey request, specializedHelperName request)) |> Map.ofList
    let rewrittenFunctions =
        functions |> List.map (fun func -> {
            func with Body = rewriteKnownCalls definitions returnFacts specializedNames Map.empty func.Body })
    let rewrittenMain = rewriteKnownCalls definitions returnFacts specializedNames Map.empty main
    Program (List.rev targetClones @ rewrittenFunctions @ List.rev helperClones, rewrittenMain)

let specializeProgram program = specializeProgramWithExternalFunctions [] program
