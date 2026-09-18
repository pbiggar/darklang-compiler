// ExtractListRegions.fs - Recognize closed list computations and prove scalar scope eligibility.

module ExtractListRegions

open HIR

open ListRegion

type private ScalarLifetime = EnclosingLifetime | JoinEntryLifetime

type private Extraction = {
    Lists: Map<string, HIR.Value>
    Values: Map<string, HIR.Value>
    Operations: HIR.Operation<Operation<Transform>, FunctionalBlock> list
    NextId: int
    Lifetime: ScalarLifetime
}

let private listCall = function
    | AST.Call (name, args) -> Some (name, AST.NonEmptyList.toList args)
    | _ -> None

/// Prove scope destruction separately from evaluation effects. Calls use an
/// explicit contract; nominal payloads and unknown closures remain unproven.
let private inertExpression infer callIsInert =
    let inertType = DestructionAnalysis.hasInertDestruction
    let rec check types expr =
        let typedInert () =
            match infer types expr with
            | Ok typ -> inertType typ
            | Error _ -> false
        let recur = check types
        match expr with
        | AST.FuncRef _ -> true
        | AST.Closure (_, captures) -> List.forall recur captures
        | AST.Let (AST.LPVariable name, value, body) ->
            // Reject unsupported syntax before inference: declaration overlays
            // need not contain the pattern/layout metadata of their base context.
            if not (recur value) then false
            else
                match infer types value with
                | Ok typ when inertType typ -> check (Map.add name typ types) body
                | _ -> false
        | AST.Let ((AST.LPUnit | AST.LPWildcard), value, body)
        | AST.Sequence (value, body) -> recur value && recur body
        | AST.If (condition, yes, no) -> recur condition && recur yes && recur no
        | AST.BinOp (_, left, right) -> recur left && recur right
        | AST.UnaryOp (_, value) | AST.TupleAccess (value, _) -> recur value
        | AST.Call (name, args) ->
            callIsInert name && (AST.NonEmptyList.toList args |> List.forall recur) && typedInert ()
        | AST.TupleLiteral values | AST.ListLiteral values -> List.forall recur values
        | AST.Var _ -> typedInert ()
        | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _
        | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
        | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _
        | AST.UInt64Literal _ | AST.UInt128Literal _ | AST.BigIntLiteral _
        | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _
        | AST.FloatLiteral _ | AST.RuntimeError _ -> true
        | _ -> false
    check

/// Retain dependencies with each local proof so registry composition can revoke
/// transitive proofs when a definition changes. Recursive components need no
/// unrolling: the consumer rejects the backwards closure of unproven callees.
let scopeContracts infer (functions: AST.FunctionDef list) =
    let rec calls expr =
        let many expressions = expressions |> List.map calls |> Set.unionMany
        match expr with
        | AST.Call (name, args) -> Set.add name (many (AST.NonEmptyList.toList args))
        | AST.Closure (_, captures) | AST.TupleLiteral captures | AST.ListLiteral captures -> many captures
        | AST.Let (_, value, body) | AST.Sequence (value, body)
        | AST.BinOp (_, value, body) -> many [value; body]
        | AST.If (condition, yes, no) -> many [condition; yes; no]
        | AST.UnaryOp (_, value) | AST.TupleAccess (value, _) -> calls value
        | _ -> Set.empty
    functions |> List.map (fun func ->
        let parameters = AST.NonEmptyList.toList func.Params
        let types = Map.ofList parameters
        let localInert =
            DestructionAnalysis.hasInertDestruction func.ReturnType
            && List.forall (snd >> DestructionAnalysis.hasInertDestruction) parameters
            && inertExpression infer (fun _ -> true) types func.Body
        func.Name,
        ({ LocalDestruction = if localInert then DestructionAnalysis.InertScope else DestructionAnalysis.UnprovenScope
           Calls = calls func.Body }: DestructionAnalysis.FunctionScopeContract))
    |> Map.ofList

/// A failed recognition is semantic absence, not a compiler failure. The
/// original checked expression then uses the supported persistent List path.
let tryExtract
    (inertScopes: Set<string>)
    (parameterTypes: Map<string, AST.Type>)
    (infer: Map<string, AST.Type> -> AST.Expr -> Result<AST.Type, string>)
    (freeVariables: AST.Expr -> Set<string>)
    (expression: AST.Expr)
    : FunctionalRegion option =
    let inertExpression = inertExpression infer (fun name -> Set.contains name inertScopes)
    let types state = state.Values |> Map.map (fun _ value -> value.Type)
    let normalizedOperand state expr typ =
        let inputs =
            freeVariables expr
            |> Set.toList
            |> List.choose (fun name -> Map.tryFind name state.Values |> Option.map (fun value -> name, value))
            |> Map.ofList
        { Expression = expr; Type = typ; Inputs = inputs }

    let operand state accepts expr : Scalar option =
        let referencesList =
            freeVariables expr |> Set.exists (fun name -> Map.containsKey name state.Lists)
        let destructionIsInert =
            match state.Lifetime with
            | EnclosingLifetime -> true
            | JoinEntryLifetime -> inertExpression (types state) expr
        if referencesList || not destructionIsInert then None
        else
            match infer (types state) expr with
            | Ok typ when accepts typ -> Some (normalizedOperand state expr typ)
            | _ -> None

    let scalar state expr = operand state immediate expr

    let callback state expected expr : Scalar option =
        let scopeIsInert =
            match state.Lifetime, expr with
            | EnclosingLifetime, _ -> true
            | JoinEntryLifetime, AST.FuncRef name
            | JoinEntryLifetime, AST.Closure (name, _) -> Set.contains name inertScopes
            | JoinEntryLifetime, _ -> false
        // A closure may not hide a region alias or an effectful destructor.
        let capturesAreImmediate =
            match expr with
            | AST.Closure (_, captures) ->
                captures |> List.forall (function
                    | AST.FuncRef _ -> true // Static code addresses, including closure comparators.
                    | capture -> Option.isSome (scalar state capture))
            | AST.FuncRef _ -> true
            | _ -> false
        if not capturesAreImmediate || not scopeIsInert then None
        else
            match infer (types state) expr with
            | Ok typ when typ = expected -> Some (normalizedOperand state expr typ)
            | _ -> None

    let fresh typ state =
        { Id = HIR.ValueId state.NextId; Type = typ }, { state with NextId = state.NextId + 1 }

    let addList state operation =
        let value, next = fresh (AST.TList AST.TInt64) state
        value, { next with Operations = operation value :: state.Operations }

    let rec list state expr =
        match expr with
        | AST.Var name -> Map.tryFind name state.Lists |> Option.map (fun id -> id, state)
        | AST.ListLiteral elements when List.length elements <= maxCapacity ->
            let values = elements |> List.map (fun value -> scalar state value |> Option.filter (fun typed -> typed.Type = AST.TInt64))
            if values |> List.forall Option.isSome then
                Some (addList state (fun output -> Leaf (Construct (output, Literal (List.choose id values)))))
            else None
        | _ ->
            match listCall expr with
            | Some ("Stdlib.List.repeatUnsafe_i64", [count; value]) ->
                match operand state ((=) AST.TInt) count, operand state ((=) AST.TInt64) value with
                | Some count, Some value -> Some (addList state (fun output -> Leaf (Construct (output, Repeat (count, value)))))
                | _ -> None
            | Some ("Stdlib.List.map_i64_i64", [input; fn]) ->
                list state input
                |> Option.bind (fun (source, next) ->
                    callback state (AST.TFunction ([AST.TInt64], AST.TInt64)) fn
                    |> Option.map (fun fn -> addList next (fun id -> Leaf (Transform (id, source, Map fn)))))
            | Some ("Stdlib.List.reverse_i64", [input]) ->
                list state input
                |> Option.map (fun (source, next) -> addList next (fun id -> Leaf (Transform (id, source, Reverse))))
            | _ -> None

    let rec bindScalar state name expr =
        match expr with
        | AST.If (condition, yes, no) ->
            operand state ((=) AST.TBool) condition
            |> Option.bind (fun condition ->
                region name { state with Operations = []; Lifetime = JoinEntryLifetime } yes
                |> Option.bind (fun (FunctionalBlock yes as yesBlock, afterYes) ->
                    region name { state with Operations = []; NextId = afterYes; Lifetime = JoinEntryLifetime } no
                    |> Option.bind (fun (FunctionalBlock no as noBlock, afterNo) ->
                        if yes.Result.Type <> no.Result.Type then None
                        else
                            let result, next = fresh yes.Result.Type { state with NextId = afterNo }
                            Some { next with
                                      Values = Map.add name result state.Values
                                      Lists = Map.remove name state.Lists
                                      Operations = Branch (result, condition, yesBlock, noBlock) :: state.Operations })))
        | _ -> bindSimpleScalar state name expr

    and bindSimpleScalar state name expr =
        match listCall expr with
        | Some ("Stdlib.List.fold_i64_i64", [input; initial; fn]) ->
            list state input
            |> Option.bind (fun (source, next) ->
                match scalar state initial, callback state (AST.TFunction ([AST.TInt64; AST.TInt64], AST.TInt64)) fn with
                | Some initial, Some fn when initial.Type = AST.TInt64 ->
                    let result, afterResult = fresh AST.TInt64 next
                    Some { afterResult with
                             Values = Map.add name result next.Values
                             Lists = Map.remove name next.Lists
                             Operations = Leaf (Fold (result, source, initial, fn)) :: next.Operations }
                | _ -> None)
        | _ ->
            scalar state expr
            |> Option.map (fun value ->
                let result, next = fresh value.Type state
                { next with Values = Map.add name result state.Values
                            Lists = Map.remove name state.Lists
                            Operations = ScalarBinding (result, value) :: state.Operations })

    and region finalName state expr =
        match expr with
        | AST.Let (AST.LPVariable name, value, body) ->
            match list state value with
            | Some (id, next) ->
                region finalName { next with Lists = Map.add name id next.Lists
                                             Values = Map.add name id next.Values } body
            | None -> bindScalar state name value |> Option.bind (fun next -> region finalName next body)
        | _ ->
            bindScalar state finalName expr
            |> Option.bind (fun next ->
                Map.tryFind finalName next.Values
                |> Option.map (fun result ->
                    FunctionalBlock { Parameters = Map.empty
                                      Operations = List.rev next.Operations
                                      Result = result }, next.NextId))

    let rec collectNames expr =
        match expr with
        | AST.Let (AST.LPVariable name, value, body) -> Set.add name (Set.union (collectNames value) (collectNames body))
        | AST.If (condition, yes, no) -> Set.unionMany [collectNames condition; collectNames yes; collectNames no]
        | _ -> freeVariables expr
    let rec resultName names index =
        let name = $"__list_hir_result_{index}"
        if Set.contains name names then resultName names (index + 1) else name

    let isListOperation value =
        match listCall value with
        | Some ("Stdlib.List.map_i64_i64", _)
        | Some ("Stdlib.List.reverse_i64", _)
        | Some ("Stdlib.List.repeatUnsafe_i64", _)
        | Some ("Stdlib.List.fold_i64_i64", _) -> true
        | _ -> false
    let candidate =
        match expression with
        | AST.Let (_, AST.ListLiteral _, _) -> true
        | AST.Let (_, value, _) -> isListOperation value
        | _ -> isListOperation expression
    if candidate then
        let finalName = resultName (collectNames expression) 0
        let parameterNames = freeVariables expression |> Set.intersect (parameterTypes |> Map.keys |> Set.ofSeq)
        let parameters, nextId =
            parameterNames
            |> Set.toList
            |> List.mapFold (fun nextId name ->
                let typ = Map.find name parameterTypes
                (name, { Id = HIR.ValueId nextId; Type = typ }), nextId + 1) 0
            |> fun (values, nextId) -> Map.ofList values, nextId
        region finalName { Lists = Map.empty; Values = parameters; Operations = []; NextId = nextId; Lifetime = EnclosingLifetime } expression
        |> Option.bind (fun (FunctionalBlock block, _) ->
            let rec containsListOperation (FunctionalBlock block) =
                block.Operations
                |> List.exists (function
                    | Leaf _ -> true
                    | Branch (_, _, ifTrue, ifFalse) -> containsListOperation ifTrue || containsListOperation ifFalse
                    | Call _ -> false
                    | ScalarBinding _ -> false)
            let root = FunctionalBlock { block with Parameters = parameters }
            if not (containsListOperation root) then None
            else Some (FunctionalRegion root))
    else None
