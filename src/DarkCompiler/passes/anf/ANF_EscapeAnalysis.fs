// ANF_EscapeAnalysis.fs - Eliminate scalar aggregates and reuse unique fixed blocks.
//
// This deliberately narrow first escape-analysis pass scalar-replaces local
// tuple, record, and boxed-sum allocations only when their complete lexical use
// set is projections, aliases, or a representation-only constructor source.
// Managed fields without a structural non-observable destruction proof and
// every unmodelled use retain the ordinary allocation.

module ANF_EscapeAnalysis

open ANF

type private ScalarAggregate = {
    Fields: Atom list
}

let private isScalarType (typ: AST.Type) : bool =
    match typ with
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TBool
    | AST.TFloat64
    | AST.TDateTime
    | AST.TUnit
    | AST.TRuntimeError -> true
    | _ -> false

/// Prove that releasing a displaced field cannot run a language-visible
/// finalizer. Nominal records use the complete registry and concrete type
/// arguments. Regular recursive records are admitted coinductively; type-growing
/// recursion, sums and closures still fail closed.
let private hasNonObservableDestruction
    (typeReg: TypeRegistries.TypeRegistry)
    (typ: AST.Type)
    : bool =
    let rec prove (expandingRecords: Map<string, AST.Type>) typ =
        isScalarType typ
        || match typ with
           | AST.TString | AST.TBlob | AST.TInt -> true
           | AST.TTuple elements -> List.forall (prove expandingRecords) elements
           | AST.TList element -> prove expandingRecords element
           | AST.TDict (key, value) ->
               prove expandingRecords key
               && prove expandingRecords value
           | AST.TRecord (name, typeArgs) ->
               let recordType = AST.TRecord (name, typeArgs)
               match Map.tryFind name expandingRecords with
               | Some expandingType -> expandingType = recordType
               | None ->
                   match Map.tryFind name typeReg with
                   | Some info when List.length info.TypeParams = List.length typeArgs ->
                       let subst = List.zip info.TypeParams typeArgs |> Map.ofList
                       let expandingRecords = Map.add name recordType expandingRecords
                       info.Fields
                       |> List.forall (fun (_, fieldType) ->
                           fieldType
                           |> TypeSubstitution.applySubstToType subst
                           |> prove expandingRecords)
                   | _ -> false
           | _ -> false
    prove Map.empty typ

let private atomIsScalar (scalarTemps: Set<TempId>) (atom: Atom) : bool =
    match atom with
    | UnitLiteral
    | IntLiteral _
    | BoolLiteral _
    | FloatLiteral _ -> true
    | Var id -> Set.contains id scalarTemps
    | StringLiteral _ | FuncRef _ -> false

let private atomsUseTracked (tracked: Set<TempId>) (atoms: Atom list) : bool =
    tracked
    |> Set.exists (fun id -> ANFEffects.atomsUseTemp id atoms)

let private cexprUsesTracked (tracked: Set<TempId>) (cexpr: CExpr) : bool =
    tracked
    |> Set.exists (fun id -> ANFEffects.cexprUsesTemp id cexpr)

let rec private exprUsesTracked (tracked: Set<TempId>) (expr: AExpr) : bool =
    match expr with
    | Return atom -> atomsUseTracked tracked [atom]
    | Jump (_, atom) -> atomsUseTracked tracked [atom]
    | Join (parameter, continuation, entry) ->
        exprUsesTracked (Set.remove parameter.Id tracked) continuation
        || exprUsesTracked tracked entry
    | Let (_, cexpr, body) ->
        cexprUsesTracked tracked cexpr || exprUsesTracked tracked body
    | If (condition, thenBranch, elseBranch) ->
        atomsUseTracked tracked [condition]
        || exprUsesTracked tracked thenBranch
        || exprUsesTracked tracked elseBranch

/// Rewrite the sole consuming clone of a uniquely local compatible record to
/// reuse its source block. RC elaboration retains replacement managed children
/// and releases displaced children before the stores. Running this after
/// scalar replacement preserves allocation-free immediate cases.
let rec private reuseUniqueRecordClone
    (sourceDescriptor: RecordDescriptor)
    (tracked: Set<TempId>)
    (expr: AExpr)
    : AExpr option =
    match expr with
    | Jump _ | Join _ | Return _ | If _ -> None
    | Let (boundId, RecordClone (descriptor, Var cloneSourceId, fields), body)
        when Set.contains cloneSourceId tracked && descriptor = sourceDescriptor ->
        if not (atomsUseTracked tracked fields)
           && not (exprUsesTracked tracked body) then
            Some (
                Let (
                    boundId,
                    RecordReuse (sourceDescriptor, descriptor, Var cloneSourceId, fields),
                    body
                )
            )
        else
            None
    | Let (boundId, Atom (Var sourceId), body) when Set.contains sourceId tracked ->
        reuseUniqueRecordClone sourceDescriptor (Set.add boundId tracked) body
        |> Option.map (fun rewritten -> Let (boundId, Atom (Var sourceId), rewritten))
    | Let (boundId, TypedAtom (Var sourceId, typ), body) when Set.contains sourceId tracked ->
        reuseUniqueRecordClone sourceDescriptor (Set.add boundId tracked) body
        |> Option.map (fun rewritten -> Let (boundId, TypedAtom (Var sourceId, typ), rewritten))
    | Let (boundId, cexpr, body) ->
        let isProjection =
            match cexpr with
            | RecordGet (_, Var recordId, _) -> Set.contains recordId tracked
            | _ -> false
        if isProjection || not (cexprUsesTracked tracked cexpr) then
            reuseUniqueRecordClone sourceDescriptor tracked body
            |> Option.map (fun rewritten -> Let (boundId, cexpr, rewritten))
        else
            None

/// Rewrite a later straight-line constructor of the same instantiated boxed
/// sum to reuse a uniquely local source block. The source descriptor remains
/// attached so RC elaboration releases the displaced variant payload type.
let rec private reuseUniqueSumConstructor
    (typeReg: TypeRegistries.TypeRegistry)
    (sourceDescriptor: RecordDescriptor)
    (sourceId: TempId)
    (tracked: Set<TempId>)
    (projections: Set<TempId>)
    (expr: AExpr)
    : AExpr option =
    match expr with
    | Jump _ | Join _ | Return _ | If _ -> None
    | Let (boundId, RecordAlloc (targetDescriptor, fields), body)
        when targetDescriptor.ValueType = sourceDescriptor.ValueType
             && List.length targetDescriptor.Fields = List.length sourceDescriptor.Fields
             && targetDescriptor.Fields
                |> List.forall (snd >> hasNonObservableDestruction typeReg) ->
        if not (atomsUseTracked tracked fields)
           && not (exprUsesTracked tracked body)
           && not (exprUsesTracked projections body) then
            Some (
                Let (
                    boundId,
                    RecordReuse (sourceDescriptor, targetDescriptor, Var sourceId, fields),
                    body
                )
            )
        else
            None
    | Let (boundId, Atom (Var sourceId), body) when Set.contains sourceId tracked ->
        reuseUniqueSumConstructor typeReg sourceDescriptor sourceId (Set.add boundId tracked) projections body
        |> Option.map (fun rewritten -> Let (boundId, Atom (Var sourceId), rewritten))
    | Let (boundId, TypedAtom (Var sourceId, typ), body) when Set.contains sourceId tracked ->
        reuseUniqueSumConstructor typeReg sourceDescriptor sourceId (Set.add boundId tracked) projections body
        |> Option.map (fun rewritten -> Let (boundId, TypedAtom (Var sourceId, typ), rewritten))
    | Let (boundId, Atom (Var projectionId), body) when Set.contains projectionId projections ->
        reuseUniqueSumConstructor typeReg sourceDescriptor sourceId tracked (Set.add boundId projections) body
        |> Option.map (fun rewritten -> Let (boundId, Atom (Var projectionId), rewritten))
    | Let (boundId, TypedAtom (Var projectionId, typ), body)
        when Set.contains projectionId projections ->
        reuseUniqueSumConstructor typeReg sourceDescriptor sourceId tracked (Set.add boundId projections) body
        |> Option.map (fun rewritten -> Let (boundId, TypedAtom (Var projectionId, typ), rewritten))
    | Let (boundId, (TupleGet (Var projectedSourceId, _) as cexpr), body)
    | Let (boundId, (RecordGet (_, Var projectedSourceId, _) as cexpr), body)
        when Set.contains projectedSourceId tracked ->
        reuseUniqueSumConstructor typeReg sourceDescriptor sourceId tracked (Set.add boundId projections) body
        |> Option.map (fun rewritten -> Let (boundId, cexpr, rewritten))
    | Let (boundId, cexpr, body) ->
        if not (cexprUsesTracked tracked cexpr) then
            reuseUniqueSumConstructor typeReg sourceDescriptor sourceId tracked projections body
            |> Option.map (fun rewritten -> Let (boundId, cexpr, rewritten))
        else
            None

let rec private reuseEligibleFixedBlocks
    (typeReg: TypeRegistries.TypeRegistry)
    (expr: AExpr)
    : AExpr =
    match expr with
    | Jump _ | Return _ -> expr
    | Join (parameter, continuation, entry) ->
        Join (
            parameter,
            reuseEligibleFixedBlocks typeReg continuation,
            reuseEligibleFixedBlocks typeReg entry
        )
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            reuseEligibleFixedBlocks typeReg thenBranch,
            reuseEligibleFixedBlocks typeReg elseBranch
        )
    | Let (boundId, cexpr, body) ->
        let body = reuseEligibleFixedBlocks typeReg body
        match cexpr with
        | RecordAlloc (descriptor, _)
        | RecordClone (descriptor, _, _)
        | RecordReuse (_, descriptor, _, _)
            when descriptor.Fields
                 |> List.forall (snd >> hasNonObservableDestruction typeReg) ->
            let candidate =
                match descriptor.ValueType with
                | AST.TSum _ ->
                    reuseUniqueSumConstructor
                        typeReg
                        descriptor
                        boundId
                        (Set.singleton boundId)
                        Set.empty
                        body
                | _ -> reuseUniqueRecordClone descriptor (Set.singleton boundId) body
            let rewritten = candidate |> Option.defaultValue body
            Let (boundId, cexpr, rewritten)
        | _ -> Let (boundId, cexpr, body)

/// Prove that an allocation and every alias derived from it stay inside the
/// local projection/clone boundary. Calls and storage are rejected by the
/// general use check without needing an optimistic effect classification.
let rec private hasOnlyLocalAggregateUses
    (tracked: Set<TempId>)
    (expr: AExpr)
    : bool =
    match expr with
    | Jump (_, atom) -> not (atomsUseTracked tracked [atom])
    | Join (parameter, continuation, entry) ->
        hasOnlyLocalAggregateUses (Set.remove parameter.Id tracked) continuation
        && hasOnlyLocalAggregateUses tracked entry
    | Return atom ->
        not (atomsUseTracked tracked [atom])
    | Let (boundId, cexpr, body) ->
        match cexpr with
        | Atom (Var sourceId)
        | TypedAtom (Var sourceId, _) when Set.contains sourceId tracked ->
            hasOnlyLocalAggregateUses (Set.add boundId tracked) body
        | TupleGet (Var sourceId, _)
        | RecordGet (_, Var sourceId, _) when Set.contains sourceId tracked ->
            hasOnlyLocalAggregateUses tracked body
        | RecordClone (_, Var sourceId, fields) when Set.contains sourceId tracked ->
            not (atomsUseTracked tracked fields)
            && hasOnlyLocalAggregateUses tracked body
        | _ ->
            not (cexprUsesTracked tracked cexpr)
            && hasOnlyLocalAggregateUses tracked body
    | If (condition, thenBranch, elseBranch) ->
        not (atomsUseTracked tracked [condition])
        && hasOnlyLocalAggregateUses tracked thenBranch
        && hasOnlyLocalAggregateUses tracked elseBranch

let private tryField (index: int) (aggregate: ScalarAggregate) : Atom =
    match List.tryItem index aggregate.Fields with
    | Some field -> field
    | None -> Crash.crash $"ANF_EscapeAnalysis: aggregate projection index {index} is out of bounds"

let private rewriteProjection
    (aggregates: Map<TempId, ScalarAggregate>)
    (cexpr: CExpr)
    : CExpr =
    match cexpr with
    | TupleGet (Var aggregateId, index)
    | RecordGet (_, Var aggregateId, index) ->
        match Map.tryFind aggregateId aggregates with
        | Some aggregate -> Atom (tryField index aggregate)
        | None -> cexpr
    | RecordClone (descriptor, Var aggregateId, fields) ->
        match Map.tryFind aggregateId aggregates with
        | Some _ -> RecordAlloc (descriptor, fields)
        | None -> cexpr
    | _ -> cexpr

let private cexprProducesScalar
    (returnTypes: Map<AST.FunctionId, AST.Type>)
    (scalarTemps: Set<TempId>)
    (cexpr: CExpr)
    : bool =
    let scalar = atomIsScalar scalarTemps
    match cexpr with
    | Atom atom -> scalar atom
    | TypedAtom (_, typ) -> isScalarType typ
    | Prim (_, left, right) -> scalar left && scalar right
    | UnaryPrim (_, atom) -> scalar atom
    | IfValue (_, thenValue, elseValue) -> scalar thenValue && scalar elseValue
    | Call (name, _)
    | BorrowedCall (name, _)
    | TailCall (name, _) ->
        Map.tryFind name returnTypes
        |> Option.map isScalarType
        |> Option.defaultValue false
    | RecordGet (descriptor, _, index) ->
        descriptor.Fields
        |> List.tryItem index
        |> Option.map (snd >> isScalarType)
        |> Option.defaultValue false
    | FloatToInt64 _
    | FloatToBits _
    | RandomInt64
    | DateTimeNow
    | Sleep _ -> true
    | _ -> false

let private tryScalarAggregate
    (scalarTemps: Set<TempId>)
    (cexpr: CExpr)
    : ScalarAggregate option =
    let scalarFields fields =
        if List.forall (atomIsScalar scalarTemps) fields then
            Some { Fields = fields }
        else
            None

    match cexpr with
    | TupleAlloc fields -> scalarFields fields
    | RecordAlloc (descriptor, fields)
    | RecordClone (descriptor, _, fields) ->
        let hasScalarLayout =
            List.length descriptor.Fields = List.length fields
            && descriptor.Fields |> List.forall (snd >> isScalarType)
        if hasScalarLayout then scalarFields fields else None
    | _ -> None

let rec private scalarReplaceExpr
    (returnTypes: Map<AST.FunctionId, AST.Type>)
    (scalarTemps: Set<TempId>)
    (aggregates: Map<TempId, ScalarAggregate>)
    (expr: AExpr)
    : AExpr =
    match expr with
    | Jump _ -> expr
    | Join (parameter, continuation, entry) ->
        let bodyScalars =
            if isScalarType parameter.Type then Set.add parameter.Id scalarTemps
            else Set.remove parameter.Id scalarTemps
        Join (parameter,
              scalarReplaceExpr returnTypes bodyScalars (Map.remove parameter.Id aggregates) continuation,
              scalarReplaceExpr returnTypes scalarTemps aggregates entry)
    | Return _ -> expr
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            scalarReplaceExpr returnTypes scalarTemps aggregates thenBranch,
            scalarReplaceExpr returnTypes scalarTemps aggregates elseBranch
        )
    | Let (boundId, Atom (Var sourceId), body) when Map.containsKey sourceId aggregates ->
        let aggregate = Map.find sourceId aggregates
        scalarReplaceExpr returnTypes scalarTemps (Map.add boundId aggregate aggregates) body
    | Let (boundId, TypedAtom (Var sourceId, _), body) when Map.containsKey sourceId aggregates ->
        let aggregate = Map.find sourceId aggregates
        scalarReplaceExpr returnTypes scalarTemps (Map.add boundId aggregate aggregates) body
    | Let (boundId, cexpr, body) ->
        let rewrittenCExpr = rewriteProjection aggregates cexpr
        match tryScalarAggregate scalarTemps rewrittenCExpr with
        | Some aggregate when hasOnlyLocalAggregateUses (Set.singleton boundId) body ->
            scalarReplaceExpr
                returnTypes
                scalarTemps
                (Map.add boundId aggregate aggregates)
                body
        | _ ->
            let scalarTemps' =
                if cexprProducesScalar returnTypes scalarTemps rewrittenCExpr then
                    Set.add boundId scalarTemps
                else
                    Set.remove boundId scalarTemps
            Let (
                boundId,
                rewrittenCExpr,
                scalarReplaceExpr returnTypes scalarTemps' aggregates body
            )

let private scalarReplaceFunction
    (typeReg: TypeRegistries.TypeRegistry)
    (returnTypes: Map<AST.FunctionId, AST.Type>)
    (func: Function)
    : Function =
    let scalarParams =
        func.TypedParams
        |> List.choose (fun param -> if isScalarType param.Type then Some param.Id else None)
        |> Set.ofList
    { func with
        Body =
            scalarReplaceExpr returnTypes scalarParams Map.empty func.Body
            |> reuseEligibleFixedBlocks typeReg }

let scalarReplaceProgram
    (typeReg: TypeRegistries.TypeRegistry)
    (Program (functions, mainExpr): Program)
    : Program =
    let returnTypes =
        functions
        |> List.map (fun func -> func.Id, func.ReturnType)
        |> Map.ofList
    Program (
        functions |> List.map (scalarReplaceFunction typeReg returnTypes),
        scalarReplaceExpr returnTypes Set.empty Map.empty mainExpr
        |> reuseEligibleFixedBlocks typeReg
    )
