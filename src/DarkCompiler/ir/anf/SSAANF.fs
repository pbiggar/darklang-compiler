// SSAANF.fs - Typed control-flow form for optimized ANF operations.
//
// Lexical joins become block parameters, and their jumps carry the parameter
// value on the edge. ANF may reuse one TempId in sibling branches; construction
// freshens later definitions so each SSA value has one defining site.

module SSAANF

type Label = Label of int

type Terminator =
    | Return of ANF.Atom
    | Jump of target:Label * arguments:ANF.Atom list
    | Branch of condition:ANF.Atom * ifTrue:Label * ifFalse:Label

type Block = {
    Label: Label
    Parameters: ANF.TypedParam list
    Operations: (ANF.TempId * ANF.CExpr) list
    Terminator: Terminator
}

type Function = {
    Id: AST.FunctionId
    Name: string
    TypedParams: ANF.TypedParam list
    ReturnType: AST.SemanticType
    ReturnOwnership: ANF.ReturnOwnership
    Entry: Label
    Blocks: Map<Label, Block>
    FreshValueTypes: Map<ANF.TempId, AST.SemanticType>
}

type private Renaming = {
    Seen: Set<ANF.TempId>
    Next: ANF.VarGen
    FreshValueTypes: Map<ANF.TempId, AST.SemanticType>
}

let private renamedAtom mapping atom = ANF_Inlining.renameAtom mapping atom

// LIR uses these virtual IDs for physical spill and ABI scratch registers.
let private isReservedBackendId (ANF.TempId id) =
    id = 1000 || id = 1001 || id = 1002 || id = 2000 || (id >= 3000 && id < 4000)

let private define
    (typeMap: ANF.TypeMap)
    (id: ANF.TempId)
    (knownType: AST.SemanticType option)
    (state: Renaming)
    : Result<ANF.TempId * Renaming, string> =
    if not (Set.contains id state.Seen) && not (isReservedBackendId id) then
        Ok (id, { state with Seen = Set.add id state.Seen })
    else
        let fresh, next = ANF.freshVar state.Next
        let valueType =
            match knownType with
            | Some typ -> Some typ
            | None -> Map.tryFind id typeMap
        match valueType with
        | None -> Error $"SSA ANF: missing type for repeated value {id}"
        | Some typ ->
            Ok (
                fresh,
                {
                    Seen = Set.add fresh state.Seen
                    Next = next
                    FreshValueTypes = Map.add fresh typ state.FreshValueTypes
                })

let rec private freshenDefinitions
    (typeMap: ANF.TypeMap)
    (mapping: Map<ANF.TempId, ANF.TempId>)
    (state: Renaming)
    (expr: ANF.AExpr)
    : Result<ANF.AExpr * Renaming, string> =
    match expr with
    | ANF.Let (id, operation, rest) ->
        let operation' = ANF_Inlining.renameCExpr mapping operation
        define typeMap id None state
        |> Result.bind (fun (defined, afterDefinition) ->
            freshenDefinitions typeMap (Map.add id defined mapping) afterDefinition rest
            |> Result.map (fun (rest', final) -> ANF.Let (defined, operation', rest'), final))
    | ANF.Return value ->
        Ok (ANF.Return (renamedAtom mapping value), state)
    | ANF.Jump (target, value) ->
        let target' = Map.tryFind target mapping |> Option.defaultValue target
        Ok (ANF.Jump (target', renamedAtom mapping value), state)
    | ANF.If (condition, ifTrue, ifFalse) ->
        freshenDefinitions typeMap mapping state ifTrue
        |> Result.bind (fun (trueBranch, afterTrue) ->
            freshenDefinitions typeMap mapping afterTrue ifFalse
            |> Result.map (fun (falseBranch, final) ->
                ANF.If (renamedAtom mapping condition, trueBranch, falseBranch), final))
    | ANF.Join (parameter, continuation, entry) ->
        define typeMap parameter.Id (Some parameter.Type) state
        |> Result.bind (fun (defined, afterDefinition) ->
            let scoped = Map.add parameter.Id defined mapping
            freshenDefinitions typeMap scoped afterDefinition continuation
            |> Result.bind (fun (continuation', afterContinuation) ->
                freshenDefinitions typeMap scoped afterContinuation entry
                |> Result.map (fun (entry', final) ->
                    ANF.Join ({ parameter with Id = defined }, continuation', entry'), final)))

type private Builder = {
    NextLabel: int
    Blocks: Map<Label, Block>
}

let private freshLabel (builder: Builder) : Label * Builder =
    (Label builder.NextLabel, { builder with NextLabel = builder.NextLabel + 1 })

let private finish
    (label: Label)
    (parameters: ANF.TypedParam list)
    (operationsRev: (ANF.TempId * ANF.CExpr) list)
    (terminator: Terminator)
    (builder: Builder)
    : Result<Builder, string> =
    if Map.containsKey label builder.Blocks then
        Error $"SSA ANF: block {label} is defined twice"
    else
        let block = {
            Label = label
            Parameters = parameters
            Operations = List.rev operationsRev
            Terminator = terminator
        }
        Ok { builder with Blocks = Map.add label block builder.Blocks }

let rec private convertExpr
    (joins: Map<ANF.TempId, Label>)
    (label: Label)
    (parameters: ANF.TypedParam list)
    (operationsRev: (ANF.TempId * ANF.CExpr) list)
    (expr: ANF.AExpr)
    (builder: Builder)
    : Result<Builder, string> =
    match expr with
    | ANF.Let (id, operation, rest) ->
        convertExpr joins label parameters ((id, operation) :: operationsRev) rest builder
    | ANF.Return value ->
        finish label parameters operationsRev (Return value) builder
    | ANF.Jump (target, value) ->
        match Map.tryFind target joins with
        | Some targetLabel ->
            finish label parameters operationsRev (Jump (targetLabel, [value])) builder
        | None -> Error $"SSA ANF: join target {target} is outside lexical scope"
    | ANF.If (condition, ifTrue, ifFalse) ->
        let trueLabel, afterTrue = freshLabel builder
        let falseLabel, afterFalse = freshLabel afterTrue
        finish label parameters operationsRev (Branch (condition, trueLabel, falseLabel)) afterFalse
        |> Result.bind (convertExpr joins trueLabel [] [] ifTrue)
        |> Result.bind (convertExpr joins falseLabel [] [] ifFalse)
    | ANF.Join (parameter, continuation, entry) ->
        let continuationLabel, afterLabel = freshLabel builder
        convertExpr
            (Map.add parameter.Id continuationLabel joins)
            label
            parameters
            operationsRev
            entry
            afterLabel
        |> Result.bind (convertExpr joins continuationLabel [parameter] [] continuation)

let convertFunction
    (maxSourceId: int)
    (typeMap: ANF.TypeMap)
    (func: ANF.Function)
    : Result<Function, string> =
    let entry = Label 0
    let initial = { NextLabel = 1; Blocks = Map.empty }
    let renaming = {
        Seen = func.TypedParams |> List.map (fun param -> param.Id) |> Set.ofList
        // Fresh source IDs must also avoid LIR's fixed virtual-register range.
        Next = ANF.VarGen (max 4000 (maxSourceId + 1))
        FreshValueTypes = Map.empty
    }
    let paramMapping, paramsRev, renaming =
        func.TypedParams
        |> List.fold (fun (mapping, paramsRev, state) param ->
            if isReservedBackendId param.Id then
                let fresh, next = ANF.freshVar state.Next
                (Map.add param.Id fresh mapping,
                 { param with Id = fresh } :: paramsRev,
                 { state with Next = next; Seen = Set.add fresh state.Seen })
            else
                (mapping, param :: paramsRev, state))
            (Map.empty, [], renaming)
    freshenDefinitions typeMap paramMapping renaming func.Body
    |> Result.bind (fun (body, afterRenaming) ->
        convertExpr Map.empty entry [] [] body initial
        |> Result.map (fun finished -> {
            Id = func.Id
            Name = func.Name
            TypedParams = List.rev paramsRev
            ReturnType = func.ReturnType
            ReturnOwnership = func.ReturnOwnership
            Entry = entry
            Blocks = finished.Blocks
            FreshValueTypes = afterRenaming.FreshValueTypes
        }))
