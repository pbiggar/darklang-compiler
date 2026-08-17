// 2.4.4_ANF_HigherOrderSpecialization.fs - Specialize known closure arguments.
//
// A direct call that receives a locally allocated closure has a static target
// and static capture atoms. For such a call, clone the higher-order helper and
// its closure target: captures become ordinary parameters and ClosureCall
// becomes a direct call. Unknown closure values continue to use the original
// generic helper, preserving the uniform closure ABI.

module ANF_HigherOrderSpecialization

open ANF

type private KnownClosure = {
    TargetName: string
    Captures: Atom list
}

type private SpecializationRequest = {
    HelperName: string
    HelperArgumentIndex: int
    Closure: KnownClosure
}

// Sixteen pairs bound cloned helper/target growth even when a program contains
// many independent known higher-order call sites.
let private maxSpecializedPairs = 16
let private maxHelperNodes = 256
let private maxTargetNodes = 32

let private functionMap (functions: Function list) : Map<string, Function> =
    functions |> List.map (fun func -> (func.Name, func)) |> Map.ofList

let private addKnownClosure
    (id: TempId)
    (closure: KnownClosure)
    (known: Map<TempId, KnownClosure>)
    : Map<TempId, KnownClosure> =
    Map.add id closure known

let private tryKnownArgument
    (args: Atom list)
    (index: int)
    (known: Map<TempId, KnownClosure>)
    : KnownClosure option =
    args
    |> List.tryItem index
    |> Option.bind (fun atom ->
        match atom with
        | Var id -> Map.tryFind id known
        | _ -> None)

let private directCallNames (cexpr: CExpr) : (string * Atom list) option =
    match cexpr with
    | Call (name, args)
    | BorrowedCall (name, args)
    | TailCall (name, args) -> Some(name, args)
    | _ -> None

let private appendRequestForKnownArguments
    (known: Map<TempId, KnownClosure>)
    (helperName: string)
    (args: Atom list)
    (requests: SpecializationRequest list)
    : SpecializationRequest list =
    args
    |> List.mapi (fun index _ -> index)
    |> List.fold (fun currentRequests index ->
        match tryKnownArgument args index known with
        | Some closure ->
            let request = {
                HelperName = helperName
                HelperArgumentIndex = index
                Closure = closure
            }
            currentRequests @ [request]
        | None -> currentRequests
    ) requests

let rec private collectRequests
    (functions: Map<string, Function>)
    (expr: AExpr)
    (known: Map<TempId, KnownClosure>)
    (requests: SpecializationRequest list)
    : SpecializationRequest list =
    let collectCExpr cexpr currentRequests =
        match directCallNames cexpr with
        | Some (helperName, args) when Map.containsKey helperName functions ->
            appendRequestForKnownArguments known helperName args currentRequests
        | _ -> currentRequests

    match expr with
    | Return _ -> requests
    | Let (boundId, cexpr, body) ->
        let withCall = collectCExpr cexpr requests
        let knownAfter =
            match cexpr with
            | ClosureAlloc (targetName, captures) ->
                addKnownClosure boundId { TargetName = targetName; Captures = captures } known
            | _ -> known
        collectRequests functions body knownAfter withCall
    | If (_, thenBranch, elseBranch) ->
        collectRequests functions thenBranch known requests
        |> collectRequests functions elseBranch known

let private countNodes (expr: AExpr) : int =
    let rec count current expr =
        match expr with
        | Return _ -> current + 1
        | Let (_, _, body) -> count (current + 1) body
        | If (_, thenBranch, elseBranch) ->
            current + 1 + count 0 thenBranch + count 0 elseBranch
    count 0 expr

let private targetShape
    (target: Function)
    (captures: Atom list)
    : (TypedParam list * AST.Type list) option =
    match target.TypedParams with
    | closureParam :: valueParams ->
        match closureParam.Type with
        | AST.TTuple (AST.TInt64 :: captureTypes) when List.length captureTypes = List.length captures ->
            Some(valueParams, captureTypes)
        | _ -> None
    | [] -> None

let rec private targetBodyUsesOnlyCaptures
    (closureId: TempId)
    (captureCount: int)
    (expr: AExpr)
    : bool =
    let captureAccess cexpr =
        match cexpr with
        | TupleGet (Var tupleId, index) when tupleId = closureId ->
            index >= 1 && index <= captureCount
        | _ -> not (ANF_Optimize.cexprUsesTemp closureId cexpr)

    match expr with
    | Return atom -> not (ANF_Optimize.atomUsesTemp closureId atom)
    | Let (_, cexpr, body) ->
        captureAccess cexpr && targetBodyUsesOnlyCaptures closureId captureCount body
    | If (condition, thenBranch, elseBranch) ->
        not (ANF_Optimize.atomUsesTemp closureId condition)
        && targetBodyUsesOnlyCaptures closureId captureCount thenBranch
        && targetBodyUsesOnlyCaptures closureId captureCount elseBranch

let private closureCallArity (functionParameterId: TempId) (expr: AExpr) : int option =
    let rec find current =
        match current with
        | Return _ -> None
        | Let (_, cexpr, body) ->
            match cexpr with
            | ClosureCall (Var id, args)
            | ClosureTailCall (Var id, args) when id = functionParameterId ->
                Some(List.length args)
            | _ -> find body
        | If (_, thenBranch, elseBranch) ->
            match find thenBranch with
            | Some arity -> Some arity
            | None -> find elseBranch
    find expr

let rec private helperUsesParameterOnlyForClosureOperations
    (helperName: string)
    (functionParameterId: TempId)
    (argumentIndex: int)
    (expr: AExpr)
    : bool =
    let allowed cexpr =
        match cexpr with
        | ClosureCall (Var id, args)
        | ClosureTailCall (Var id, args)
            when id = functionParameterId
                 && not (ANF_Optimize.atomsUseTemp functionParameterId args) -> true
        | Call (name, args)
        | BorrowedCall (name, args)
        | TailCall (name, args)
            when name = helperName
                 && not (ANF_Optimize.atomsUseTemp functionParameterId (List.removeAt argumentIndex args)) ->
            match List.tryItem argumentIndex args with
            | Some (Var id) -> id = functionParameterId
            | _ -> false
        | _ -> not (ANF_Optimize.cexprUsesTemp functionParameterId cexpr)

    match expr with
    | Return atom -> not (ANF_Optimize.atomUsesTemp functionParameterId atom)
    | Let (_, cexpr, body) ->
        allowed cexpr
        && helperUsesParameterOnlyForClosureOperations helperName functionParameterId argumentIndex body
    | If (condition, thenBranch, elseBranch) ->
        not (ANF_Optimize.atomUsesTemp functionParameterId condition)
        && helperUsesParameterOnlyForClosureOperations helperName functionParameterId argumentIndex thenBranch
        && helperUsesParameterOnlyForClosureOperations helperName functionParameterId argumentIndex elseBranch

let private validRequest
    (functions: Map<string, Function>)
    (request: SpecializationRequest)
    : bool =
    match Map.tryFind request.HelperName functions, Map.tryFind request.Closure.TargetName functions with
    | Some helper, Some target ->
        match List.tryItem request.HelperArgumentIndex helper.TypedParams with
        | Some helperParameter ->
            match targetShape target request.Closure.Captures with
            | Some(valueParams, _) ->
                let helperSizeOk = countNodes helper.Body <= maxHelperNodes
                let targetSizeOk = countNodes target.Body <= maxTargetNodes
                let targetBodyOk =
                    targetBodyUsesOnlyCaptures
                        (List.head target.TypedParams).Id
                        (List.length request.Closure.Captures)
                        target.Body
                let helperBodyOk =
                    helperUsesParameterOnlyForClosureOperations
                        helper.Name
                        helperParameter.Id
                        request.HelperArgumentIndex
                        helper.Body
                let arityOk =
                    closureCallArity helperParameter.Id helper.Body
                    |> Option.map (fun arity -> List.length valueParams = arity)
                    |> Option.defaultValue false
                helperSizeOk && targetSizeOk && targetBodyOk && helperBodyOk && arityOk
            | None -> false
        | None -> false
    | _ -> false

let private requestKey (request: SpecializationRequest) : string * string * int =
    (request.HelperName, request.Closure.TargetName, request.HelperArgumentIndex)

let rec private greatestTempId (expr: AExpr) (current: int) : int =
    match expr with
    | Return _ -> current
    | Let (TempId boundId, _, body) -> greatestTempId body (max current boundId)
    | If (_, thenBranch, elseBranch) ->
        greatestTempId elseBranch (greatestTempId thenBranch current)

let private freshVarGen (functions: Function list) (main: AExpr) : VarGen =
    let parameterIdValue (parameter: TypedParam) =
        let (TempId value) = parameter.Id
        value

    let greatest =
        functions
        |> List.fold (fun current func ->
            func.TypedParams
            |> List.fold (fun parameterCurrent parameter ->
                max parameterCurrent (parameterIdValue parameter)) current
            |> greatestTempId func.Body
        ) 0
        |> fun current -> greatestTempId main current
    VarGen (greatest + 1)

let private makeCaptureParameters
    (captureTypes: AST.Type list)
    (varGen: VarGen)
    : TypedParam list * VarGen =
    captureTypes
    |> List.fold (fun (parameters, currentVarGen) typ ->
        let (id, nextVarGen) = freshVar currentVarGen
        ({ Id = id; Type = typ } :: parameters, nextVarGen)
    ) ([], varGen)
    |> fun (reversedParameters, finalVarGen) -> (List.rev reversedParameters, finalVarGen)

let rec private rewriteTargetBody
    (closureId: TempId)
    (captureParameters: TypedParam list)
    (expr: AExpr)
    : AExpr =
    let rewriteCExpr cexpr =
        match cexpr with
        | TupleGet (Var tupleId, index) when tupleId = closureId ->
            captureParameters
            |> List.tryItem (index - 1)
            |> Option.map (fun parameter -> Atom (Var parameter.Id))
            |> Option.defaultValue cexpr
        | _ -> cexpr

    match expr with
    | Return _ -> expr
    | Let (boundId, cexpr, body) ->
        Let (boundId, rewriteCExpr cexpr, rewriteTargetBody closureId captureParameters body)
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            rewriteTargetBody closureId captureParameters thenBranch,
            rewriteTargetBody closureId captureParameters elseBranch
        )

let private removeAt (index: int) (items: Atom list) : Atom list =
    List.take index items @ List.skip (index + 1) items

let private removeTypedParameter (index: int) (parameters: TypedParam list) : TypedParam list =
    List.take index parameters @ List.skip (index + 1) parameters

let rec private rewriteHelperBody
    (helperName: string)
    (specializedHelperName: string)
    (specializedTargetName: string)
    (functionParameterId: TempId)
    (argumentIndex: int)
    (captureParameters: TypedParam list)
    (expr: AExpr)
    : AExpr =
    let captureAtoms = captureParameters |> List.map (fun parameter -> Var parameter.Id)

    let rewriteCExpr cexpr =
        match cexpr with
        | ClosureCall (Var id, args) when id = functionParameterId ->
            Call (specializedTargetName, captureAtoms @ args)
        | ClosureTailCall (Var id, args) when id = functionParameterId ->
            TailCall (specializedTargetName, captureAtoms @ args)
        | Call (name, args) when name = helperName ->
            Call (
                specializedHelperName,
                removeAt argumentIndex args @ captureAtoms
            )
        | BorrowedCall (name, args) when name = helperName ->
            BorrowedCall (
                specializedHelperName,
                removeAt argumentIndex args @ captureAtoms
            )
        | TailCall (name, args) when name = helperName ->
            TailCall (
                specializedHelperName,
                removeAt argumentIndex args @ captureAtoms
            )
        | _ -> cexpr

    match expr with
    | Return _ -> expr
    | Let (boundId, cexpr, body) ->
        Let (
            boundId,
            rewriteCExpr cexpr,
            rewriteHelperBody
                helperName
                specializedHelperName
                specializedTargetName
                functionParameterId
                argumentIndex
                captureParameters
                body
        )
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            rewriteHelperBody
                helperName
                specializedHelperName
                specializedTargetName
                functionParameterId
                argumentIndex
                captureParameters
                thenBranch,
            rewriteHelperBody
                helperName
                specializedHelperName
                specializedTargetName
                functionParameterId
                argumentIndex
                captureParameters
                elseBranch
        )

let rec private exprUsesTemp (tempId: TempId) (expr: AExpr) : bool =
    match expr with
    | Return atom -> ANF_Optimize.atomUsesTemp tempId atom
    | Let (_, cexpr, body) ->
        ANF_Optimize.cexprUsesTemp tempId cexpr || exprUsesTemp tempId body
    | If (condition, thenBranch, elseBranch) ->
        ANF_Optimize.atomUsesTemp tempId condition
        || exprUsesTemp tempId thenBranch
        || exprUsesTemp tempId elseBranch

let rec private rewriteKnownCalls
    (functions: Map<string, Function>)
    (specializedNames: Map<string * string * int, string>)
    (known: Map<TempId, KnownClosure>)
    (expr: AExpr)
    : AExpr =
    let rewriteCExpr cexpr =
        match directCallNames cexpr with
        | Some (helperName, args) when Map.containsKey helperName functions ->
            let specialized =
                args
                |> List.mapi (fun index atom -> (index, atom))
                |> List.choose (fun (index, atom) ->
                    match atom with
                    | Var id ->
                        Map.tryFind id known
                        |> Option.map (fun closure -> (index, closure))
                    | _ -> None)
                |> List.tryFind (fun (index, closure) ->
                    Map.containsKey (helperName, closure.TargetName, index) specializedNames)

            match specialized with
            | Some (index, closure) ->
                let newName = Map.find (helperName, closure.TargetName, index) specializedNames
                let newArgs = removeAt index args @ closure.Captures
                match cexpr with
                | Call _ -> Call (newName, newArgs)
                | BorrowedCall _ -> BorrowedCall (newName, newArgs)
                | TailCall _ -> TailCall (newName, newArgs)
                | _ -> cexpr
            | None -> cexpr
        | _ -> cexpr

    match expr with
    | Return _ -> expr
    | Let (boundId, cexpr, body) ->
        let knownAfter =
            match cexpr with
            | ClosureAlloc (targetName, captures) ->
                addKnownClosure boundId { TargetName = targetName; Captures = captures } known
            | _ -> known
        let rewrittenBody = rewriteKnownCalls functions specializedNames knownAfter body
        match cexpr with
        | ClosureAlloc _ when not (exprUsesTemp boundId rewrittenBody) ->
            rewrittenBody
        | _ ->
            Let (boundId, rewriteCExpr cexpr, rewrittenBody)
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            rewriteKnownCalls functions specializedNames known thenBranch,
            rewriteKnownCalls functions specializedNames known elseBranch
        )

let private specializedTargetName (targetName: string) : string =
    $"{targetName}__captures"

let private specializedHelperName (request: SpecializationRequest) : string =
    $"{request.HelperName}__known_{request.Closure.TargetName}_{request.HelperArgumentIndex}"

let specializeProgram (Program (functions, main)) : Program =
    let functionsByOriginalName = functionMap functions
    let rawRequests =
        functions
        |> List.fold (fun requests func ->
            collectRequests functionsByOriginalName func.Body Map.empty requests
        ) []
        |> fun requests -> collectRequests functionsByOriginalName main Map.empty requests
    let requests =
        rawRequests
        |> List.filter (fun request ->
            validRequest functionsByOriginalName request)
        |> List.distinctBy requestKey
        |> List.sortBy requestKey
        |> List.truncate maxSpecializedPairs

    let existingNames = functions |> List.map (fun func -> func.Name) |> Set.ofList
    let targetCloneNames =
        requests
        |> List.distinctBy (fun request -> request.Closure.TargetName)
        |> List.map (fun request -> specializedTargetName request.Closure.TargetName)
        |> Set.ofList
    let helperCloneNames =
        requests
        |> List.map specializedHelperName
        |> Set.ofList
    let usableRequests =
        requests
        |> List.filter (fun request ->
            not (Set.contains (specializedTargetName request.Closure.TargetName) existingNames)
            && not (Set.contains (specializedHelperName request) existingNames)
            && not (Set.contains (specializedTargetName request.Closure.TargetName) helperCloneNames)
            && not (Set.contains (specializedHelperName request) targetCloneNames))

    let startVarGen = freshVarGen functions main
    let targetRequests =
        usableRequests
        |> List.distinctBy (fun request -> request.Closure.TargetName)
        |> List.map (fun request ->
            (request, specializedTargetName request.Closure.TargetName))

    let (targetClones, varGenAfterTargets) =
        targetRequests
        |> List.fold (fun (clones, currentVarGen) (request, cloneName) ->
            let target = Map.find request.Closure.TargetName functionsByOriginalName
            match targetShape target request.Closure.Captures with
            | Some(valueParams, captureTypes) ->
                let (captureParameters, nextVarGen) = makeCaptureParameters captureTypes currentVarGen
                let closureParameter = List.head target.TypedParams
                let clone =
                    {
                        target with
                            Name = cloneName
                            TypedParams = captureParameters @ valueParams
                            Body = rewriteTargetBody closureParameter.Id captureParameters target.Body
                    }
                (clone :: clones, nextVarGen)
            | None -> (clones, currentVarGen)
        ) ([], startVarGen)

    let cloneNameForTarget =
        targetRequests
        |> List.map (fun (request, cloneName) -> (request.Closure.TargetName, cloneName))
        |> Map.ofList

    let (helperClones, _) =
        usableRequests
        |> List.fold (fun (clones, currentVarGen) request ->
            let helper = Map.find request.HelperName functionsByOriginalName
            let helperParameter =
                List.item request.HelperArgumentIndex helper.TypedParams
            let target = Map.find request.Closure.TargetName functionsByOriginalName
            match targetShape target request.Closure.Captures with
            | Some(_, captureTypes) ->
                let (captureParameters, nextVarGen) = makeCaptureParameters captureTypes currentVarGen
                let cloneName = specializedHelperName request
                let clone =
                    {
                        helper with
                            Name = cloneName
                            TypedParams =
                                removeTypedParameter request.HelperArgumentIndex helper.TypedParams
                                @ captureParameters
                            Body =
                                rewriteHelperBody
                                    helper.Name
                                    cloneName
                                    (Map.find request.Closure.TargetName cloneNameForTarget)
                                    helperParameter.Id
                                    request.HelperArgumentIndex
                                    captureParameters
                                    helper.Body
                    }
                (clone :: clones, nextVarGen)
            | None -> (clones, currentVarGen)
        ) ([], varGenAfterTargets)

    let specializedNames =
        usableRequests
        |> List.map (fun request ->
            let key = requestKey request
            (key, specializedHelperName request))
        |> Map.ofList

    let rewrittenFunctions =
        functions
        |> List.map (fun func ->
            { func with Body = rewriteKnownCalls functionsByOriginalName specializedNames Map.empty func.Body })

    let rewrittenMain = rewriteKnownCalls functionsByOriginalName specializedNames Map.empty main
    Program (
        List.rev targetClones @ rewrittenFunctions @ List.rev helperClones,
        rewrittenMain
    )
