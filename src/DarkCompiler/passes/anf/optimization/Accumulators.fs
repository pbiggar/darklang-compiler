// Accumulators.fs - Lower eligible recursion through scalar accumulators or constructor destinations.

module ANFAccumulatorOptimization

open ANF
open ANFExpressionOptimization
open TypeRegistries

type private SiblingAddition = {
    FirstCallId: TempId
    FirstArgs: Atom list
    SecondCallId: TempId
    SecondArgs: Atom list
    ResultId: TempId
}

/// A direct recursive call whose native-integer result is immediately
/// multiplied by a same-width parameter or literal. The factor restriction makes moving the wrapped
/// multiply ahead of the call observably safe without effect analysis.
type private WrappedMultiplication = {
    CallId: TempId
    CallArgs: Atom list
    Factor: Atom
    ResultId: TempId
}

type private WrappedSubtraction = {
    CallId: TempId
    CallArgs: Atom list
    Subtrahend: Atom
    ResultId: TempId
}

let private nativeIntegerTypeName (typ: AST.Type) : string option =
    match typ with
    | AST.TInt8 -> Some "Int8"
    | AST.TInt16 -> Some "Int16"
    | AST.TInt32 -> Some "Int32"
    | AST.TInt64 -> Some "Int64"
    | AST.TUInt8 -> Some "UInt8"
    | AST.TUInt16 -> Some "UInt16"
    | AST.TUInt32 -> Some "UInt32"
    | AST.TUInt64 -> Some "UInt64"
    | _ -> None

let private safeOperatorName
    (functionNames: FunctionNameRegistry)
    (typ: AST.Type)
    (operation: string)
    : AST.FunctionId option =
    nativeIntegerTypeName typ
    |> Option.bind (fun typeName ->
        let expected = $"Darklang.Stdlib.{typeName}.{operation}"
        functionNames
        |> Map.toSeq
        |> Seq.tryPick (fun (id, name) -> if name = expected then Some id else None))

let private integerLiteral (typ: AST.Type) (value: int) : Atom =
    match typ with
    | AST.TInt8 -> IntLiteral (Int8 (sbyte value))
    | AST.TInt16 -> IntLiteral (Int16 (int16 value))
    | AST.TInt32 -> IntLiteral (Int32 (int32 value))
    | AST.TInt64 -> IntLiteral (Int64 (int64 value))
    | AST.TUInt8 -> IntLiteral (UInt8 (byte value))
    | AST.TUInt16 -> IntLiteral (UInt16 (uint16 value))
    | AST.TUInt32 -> IntLiteral (UInt32 (uint32 value))
    | AST.TUInt64 -> IntLiteral (UInt64 (uint64 value))
    | _ -> Crash.crash $"Tail-recursion accumulator requested a non-native integer literal for {typ}"

let private isIntegerBinary
    (functionNames: FunctionNameRegistry)
    (typ: AST.Type)
    (primitive: BinOp)
    (safeName: string)
    (cexpr: CExpr)
    : (Atom * Atom) option =
    match cexpr with
    | Prim (op, left, right) when op = primitive -> Some (left, right)
    | Call (target, [left; right]) when safeOperatorName functionNames typ safeName = Some target -> Some (left, right)
    | _ -> None

/// A recursive list result immediately prepended with one already-evaluated
/// value. The public push wrapper is retained so the rewrite uses the same
/// typed skew-list constructor as the source expression.
type private WrappedListPrepend = {
    CallId: TempId
    CallArgs: Atom list
    PushName: AST.FunctionId
    Value: Atom
    ResultId: TempId
}

type private ConstructorLayerKind =
    | TupleLayer of Atom list
    | RecordLayer of RecordDescriptor * Atom list

type private ConstructorLayer = {
    ResultId: TempId
    Kind: ConstructorLayerKind
    HoleIndex: int
    ResultType: AST.Type
    ChildType: AST.Type
}

type private ConstructorContext = {
    CallId: TempId
    CallArgs: Atom list
    Prefix: (TempId * CExpr) list
    LayersInsideOut: ConstructorLayer list
}

let private tryLinearBindings (expr: AExpr) : ((TempId * CExpr) list * Atom) option =
    let rec collect reversedBindings remaining =
        match remaining with
        | Let (tempId, cexpr, body) ->
            collect ((tempId, cexpr) :: reversedBindings) body
        | Return atom ->
            Some (List.rev reversedBindings, atom)
        | Jump _ | Join _ | If _ ->
            None
    collect [] expr

/// Recognize a complete linear sibling-recursion arm. Requiring exactly two
/// self calls and a final addition keeps effect order and the rewrite boundary
/// explicit; the function-level gate rejects any recursion outside this shape.
let private trySiblingAddition (functionNames: FunctionNameRegistry) (funcName: AST.FunctionId) (returnType: AST.Type) (expr: AExpr) : SiblingAddition option =
    match tryLinearBindings expr with
    | Some (bindings, Var returnedId) ->
        match List.rev bindings with
        | (resultId, operation) :: _ when resultId = returnedId ->
            let selfCalls =
                bindings
                |> List.choose (fun (tempId, cexpr) ->
                    match cexpr with
                    | Call (target, args) when target = funcName -> Some (tempId, args)
                    | _ -> None)
            match isIntegerBinary functionNames returnType Add "add" operation, selfCalls with
            | Some (Var leftId, Var rightId), [(firstCallId, firstArgs); (secondCallId, secondArgs)]
                when (leftId = firstCallId && rightId = secondCallId)
                     || (leftId = secondCallId && rightId = firstCallId) ->
                    Some {
                        FirstCallId = firstCallId
                        FirstArgs = firstArgs
                        SecondCallId = secondCallId
                        SecondArgs = secondArgs
                        ResultId = resultId
                    }
            | _ -> None
        | _ -> None
    | _ -> None

let private isIntegerParameterOrLiteral (integerParams: Set<TempId>) (atom: Atom) : bool =
    match atom with
    | IntLiteral _ -> true
    | Var tempId -> Set.contains tempId integerParams
    | _ -> false

/// Recognize one direct self call wrapped by a final Int64 multiplication.
/// Every preceding binding must be pure and first-order, which rejects managed
/// allocations, effects, indirect calls, and unmodelled control-flow values.
let private tryWrappedMultiplication
    (functionNames: FunctionNameRegistry)
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (integerParams: Set<TempId>)
    (expr: AExpr)
    : WrappedMultiplication option =
    match tryLinearBindings expr with
    | Some (bindings, Var returnedId) ->
        let isAllowedBinding binding =
            match binding with
            | _, Atom _
            | _, TypedAtom _
            | _, Prim _
            | _, UnaryPrim _ -> true
            | _, Call (target, _) when target = funcName -> true
            | _, Call (target, _) when safeOperatorName functionNames returnType "multiply" = Some target -> true
            | _ -> false
        match List.rev bindings with
        | (resultId, operation) :: _ when resultId = returnedId ->
            let selfCalls =
                bindings
                |> List.choose (fun (tempId, cexpr) ->
                    match cexpr with
                    | Call (target, args) when target = funcName -> Some (tempId, args)
                    | _ -> None)
            let noOtherCalls = bindings |> List.forall isAllowedBinding
            match isIntegerBinary functionNames returnType Mul "multiply" operation, selfCalls with
            | Some (left, right), [(callId, callArgs)] when noOtherCalls ->
                match left, right with
                | Var resultCallId, factor when resultCallId = callId && isIntegerParameterOrLiteral integerParams factor ->
                    Some { CallId = callId; CallArgs = callArgs; Factor = factor; ResultId = resultId }
                | factor, Var resultCallId when resultCallId = callId && isIntegerParameterOrLiteral integerParams factor ->
                    Some { CallId = callId; CallArgs = callArgs; Factor = factor; ResultId = resultId }
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let private tryWrappedSubtraction
    (functionNames: FunctionNameRegistry)
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (integerParams: Set<TempId>)
    (expr: AExpr)
    : WrappedSubtraction option =
    match tryLinearBindings expr with
    | Some (bindings, Var returnedId) ->
        match List.rev bindings with
        | (resultId, operation) :: _ when resultId = returnedId ->
            let selfCalls =
                bindings
                |> List.choose (fun (tempId, cexpr) ->
                    match cexpr with
                    | Call (target, args) when target = funcName -> Some (tempId, args)
                    | _ -> None)
            let allowed =
                bindings
                |> List.forall (fun (_, cexpr) ->
                    match cexpr with
                    | Atom _ | TypedAtom _ | Prim _ | UnaryPrim _ -> true
                    | Call (target, _) when target = funcName -> true
                    | Call (target, _) when safeOperatorName functionNames returnType "subtract" = Some target -> true
                    | _ -> false)
            match isIntegerBinary functionNames returnType Sub "subtract" operation, selfCalls with
            | Some (Var resultCallId, subtrahend), [(callId, callArgs)]
                when resultCallId = callId
                     && allowed
                     && isIntegerParameterOrLiteral integerParams subtrahend ->
                Some {
                    CallId = callId
                    CallArgs = callArgs
                    Subtrahend = subtrahend
                    ResultId = resultId
                }
            | _ -> None
        | _ -> None
    | _ -> None

let private tryWrappedListPrepend
    (listPushIds: Set<AST.FunctionId>)
    (funcName: AST.FunctionId)
    (expr: AExpr)
    : WrappedListPrepend option =
    match tryLinearBindings expr with
    | Some (bindings, Var returnedId) ->
        match List.rev bindings with
        | (resultId, Call (pushName, [Var listId; value])) :: _
            when resultId = returnedId
                 && Set.contains pushName listPushIds ->
            let selfCalls =
                bindings
                |> List.choose (fun (tempId, cexpr) ->
                    match cexpr with
                    | Call (target, args) when target = funcName -> Some (tempId, args)
                    | _ -> None)
            let allowed =
                bindings
                |> List.forall (fun (tempId, cexpr) ->
                    match cexpr with
                    | Atom _
                    | TypedAtom _
                    | Prim _
                    | UnaryPrim _ -> true
                    | Call (target, _) when target = funcName -> true
                    | Call (target, [Var sourceId; _]) ->
                        tempId = resultId && target = pushName && sourceId = listId
                    | _ -> false)
            match selfCalls with
            | [(callId, callArgs)] when callId = listId && allowed ->
                Some {
                    CallId = callId
                    CallArgs = callArgs
                    PushName = pushName
                    Value = value
                    ResultId = resultId
                }
            | _ -> None
        | _ -> None
    | _ -> None

let private tryConstructorContext
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (expr: AExpr)
    : ConstructorContext option =
    let allocationFields cexpr =
        match cexpr with
        | TupleAlloc fields -> Some (TupleLayer fields, fields)
        | RecordAlloc (descriptor, fields) -> Some (RecordLayer (descriptor, fields), fields)
        | _ -> None

    let childType kind holeIndex expectedResultType isDirectCall =
        match kind with
        | RecordLayer (descriptor, _) ->
            descriptor.Fields |> List.tryItem holeIndex |> Option.map snd
        | TupleLayer fields ->
            match expectedResultType, isDirectCall, holeIndex, List.length fields with
            | AST.TSum _, true, 1, 2 -> Some returnType
            | _ -> None

    match tryLinearBindings expr with
    | Some (bindings, Var returnedId) ->
        let indexed = List.indexed bindings
        let selfCalls =
            indexed
            |> List.choose (fun (index, (tempId, cexpr)) ->
                match cexpr with
                | Call (target, args) when target = funcName -> Some (index, tempId, args)
                | _ -> None)
        match selfCalls with
        | [(callIndex, callId, callArgs)] ->
            let suffix = bindings |> List.skip (callIndex + 1)
            let rec collect previousId remaining layers =
                match remaining with
                | [] ->
                    match layers with
                    | (outerId, _, _) :: _ when outerId = returnedId -> Some (List.rev layers)
                    | _ -> None
                | (resultId, cexpr) :: rest ->
                    allocationFields cexpr
                    |> Option.bind (fun (kind, fields) ->
                        let holes =
                            fields
                            |> List.indexed
                            |> List.choose (fun (index, atom) ->
                                match atom with
                                | Var id when id = previousId -> Some index
                                | _ -> None)
                        match holes with
                        | [holeIndex] ->
                            collect resultId rest ((resultId, kind, holeIndex) :: layers)
                        | _ -> None)
            collect callId suffix []
            |> Option.bind (fun rawLayers ->
                let rec assign expectedResult outerToInner assigned =
                    match outerToInner with
                    | [] -> Some assigned
                    | (resultId, kind, holeIndex) :: rest ->
                        let isDirectCall = List.isEmpty rest
                        childType kind holeIndex expectedResult isDirectCall
                        |> Option.bind (fun expectedChild ->
                            let layer = {
                                ResultId = resultId
                                Kind = kind
                                HoleIndex = holeIndex
                                ResultType = expectedResult
                                ChildType = expectedChild
                            }
                            assign expectedChild rest (layer :: assigned))
                assign returnType (List.rev rawLayers) []
                |> Option.map (fun layersInsideOut -> {
                    CallId = callId
                    CallArgs = callArgs
                    Prefix = bindings |> List.take callIndex
                    LayersInsideOut = layersInsideOut
                }))
        | _ -> None
    | _ -> None

let private selfCallCount (funcName: AST.FunctionId) (expr: AExpr) : int =
    let rec count expr =
        match expr with
        | Jump _ | Return _ -> 0
        | Let (_, cexpr, body) ->
            let current =
                match cexpr with
                | Call (target, _) when target = funcName -> 1
                | _ -> 0
            current + count body
        | Join (_, continuation, entry) -> count continuation + count entry
        | If (_, thenBranch, elseBranch) ->
            count thenBranch + count elseBranch
    count expr

let private siblingAdditionCount (functionNames: FunctionNameRegistry) (funcName: AST.FunctionId) (returnType: AST.Type) (expr: AExpr) : int =
    let rec count expr =
        match trySiblingAddition functionNames funcName returnType expr with
        | Some _ -> 1
        | None ->
            match expr with
            | Jump _ | Return _ -> 0
            | Let (_, _, body) -> count body
            | Join (_, continuation, entry) -> count continuation + count entry
            | If (_, thenBranch, elseBranch) -> count thenBranch + count elseBranch
    count expr

let private wrappedMultiplicationCount
    (functionNames: FunctionNameRegistry)
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (integerParams: Set<TempId>)
    (expr: AExpr)
    : int =
    let rec count current =
        match tryWrappedMultiplication functionNames funcName returnType integerParams current with
        | Some _ -> 1
        | None ->
            match current with
            | Jump _ | Return _ -> 0
            | Let (_, _, body) -> count body
            | Join (_, continuation, entry) -> count continuation + count entry
            | If (_, thenBranch, elseBranch) -> count thenBranch + count elseBranch
    count expr

let private wrappedSubtractionCount
    (functionNames: FunctionNameRegistry)
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (integerParams: Set<TempId>)
    (expr: AExpr)
    : int =
    let rec count current =
        match tryWrappedSubtraction functionNames funcName returnType integerParams current with
        | Some _ -> 1
        | None ->
            match current with
            | Jump _ | Return _ -> 0
            | Let (_, _, body) -> count body
            | Join (_, continuation, entry) -> count continuation + count entry
            | If (_, thenBranch, elseBranch) -> count thenBranch + count elseBranch
    count expr

let private wrappedListPrependCount
    (listPushIds: Set<AST.FunctionId>)
    (funcName: AST.FunctionId)
    (expr: AExpr)
    : int =
    let rec count current =
        match tryWrappedListPrepend listPushIds funcName current with
        | Some _ -> 1
        | None ->
            match current with
            | Jump _ | Return _ -> 0
            | Let (_, _, body) -> count body
            | Join (_, continuation, entry) -> count continuation + count entry
            | If (_, thenBranch, elseBranch) -> count thenBranch + count elseBranch
    count expr

let private listPrependCallCount
    (listPushIds: Set<AST.FunctionId>)
    (expr: AExpr)
    : int =
    let rec count current =
        match current with
        | Jump _ | Return _ -> 0
        | Let (_, cexpr, body) ->
            let here =
                match cexpr with
                | Call (target, _) when Set.contains target listPushIds -> 1
                | _ -> 0
            here + count body
        | Join (_, continuation, entry) -> count continuation + count entry
        | If (_, thenBranch, elseBranch) -> count thenBranch + count elseBranch
    count expr

let private constructorContextCount
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (expr: AExpr)
    : int =
    let rec count current =
        match tryConstructorContext funcName returnType current with
        | Some _ -> 1
        | None ->
            match current with
            | Jump _ | Return _ -> 0
            | Let (_, _, body) -> count body
            | Join (_, continuation, entry) -> count continuation + count entry
            | If (_, thenBranch, elseBranch) -> count thenBranch + count elseBranch
    count expr

let private rebuildBindings (bindings: (TempId * CExpr) list) (body: AExpr) : AExpr =
    List.foldBack (fun (tempId, cexpr) acc -> Let (tempId, cexpr, acc)) bindings body

let private transformSiblingAddition
    (helperName: AST.FunctionId)
    (accumulatorId: TempId)
    (zero: Atom)
    (varGen: VarGen)
    (sibling: SiblingAddition)
    (bindings: (TempId * CExpr) list)
    : AExpr * VarGen =
    let (nextAccumulatorId, varGen') = freshVar varGen
    let rec rewrite remaining =
        match remaining with
        | [] -> Return (Var sibling.SecondCallId)
        | (tempId, _) :: rest when tempId = sibling.ResultId ->
            rewrite rest
        | (tempId, Call (_, _)) :: rest when tempId = sibling.FirstCallId ->
            Let (
                tempId,
                Call (helperName, sibling.FirstArgs @ [zero]),
                rewrite rest
            )
        | (tempId, Call (_, _)) :: rest when tempId = sibling.SecondCallId ->
            Let (
                nextAccumulatorId,
                Prim (Add, Var accumulatorId, Var sibling.FirstCallId),
                Let (
                    tempId,
                    Call (helperName, sibling.SecondArgs @ [Var nextAccumulatorId]),
                    rewrite rest
                )
            )
        | binding :: rest ->
            rebuildBindings [binding] (rewrite rest)
    (rewrite bindings, varGen')

let rec private transformAccumulatorBody
    (functionNames: FunctionNameRegistry)
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (helperName: AST.FunctionId)
    (accumulatorId: TempId)
    (zero: Atom)
    (varGen: VarGen)
    (expr: AExpr)
    : AExpr * VarGen =
    match trySiblingAddition functionNames funcName returnType expr, tryLinearBindings expr with
    | Some sibling, Some (bindings, _) ->
        transformSiblingAddition helperName accumulatorId zero varGen sibling bindings
    | _ ->
        match expr with
        | Jump _ -> (expr, varGen)
        | Join (parameter, continuation, entry) ->
            let body, next = transformAccumulatorBody functionNames funcName returnType helperName accumulatorId zero varGen continuation
            let entry', final = transformAccumulatorBody functionNames funcName returnType helperName accumulatorId zero next entry
            (Join (parameter, body, entry'), final)
        | Return atom ->
            let (resultId, varGen') = freshVar varGen
            (Let (resultId, Prim (Add, Var accumulatorId, atom), Return (Var resultId)), varGen')
        | Let (tempId, cexpr, body) ->
            let (body', varGen') =
                transformAccumulatorBody functionNames funcName returnType helperName accumulatorId zero varGen body
            (Let (tempId, cexpr, body'), varGen')
        | If (cond, thenBranch, elseBranch) ->
            let (thenBranch', varGenAfterThen) =
                transformAccumulatorBody functionNames funcName returnType helperName accumulatorId zero varGen thenBranch
            let (elseBranch', varGenAfterElse) =
                transformAccumulatorBody functionNames funcName returnType helperName accumulatorId zero varGenAfterThen elseBranch
            (If (cond, thenBranch', elseBranch'), varGenAfterElse)

let private transformWrappedMultiplication
    (helperName: AST.FunctionId)
    (accumulatorId: TempId)
    (varGen: VarGen)
    (wrapped: WrappedMultiplication)
    (bindings: (TempId * CExpr) list)
    : AExpr * VarGen =
    let (nextAccumulatorId, varGen') = freshVar varGen
    let rec rewrite remaining =
        match remaining with
        | [] -> Return (Var wrapped.CallId)
        | (tempId, _) :: rest when tempId = wrapped.ResultId -> rewrite rest
        | (tempId, Call (_, _)) :: rest when tempId = wrapped.CallId ->
            Let (
                nextAccumulatorId,
                Prim (Mul, Var accumulatorId, wrapped.Factor),
                Let (tempId, Call (helperName, wrapped.CallArgs @ [Var nextAccumulatorId]), rewrite rest)
            )
        | binding :: rest -> rebuildBindings [binding] (rewrite rest)
    (rewrite bindings, varGen')

let rec private transformMultiplicationAccumulatorBody
    (functionNames: FunctionNameRegistry)
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (integerParams: Set<TempId>)
    (helperName: AST.FunctionId)
    (accumulatorId: TempId)
    (varGen: VarGen)
    (expr: AExpr)
    : AExpr * VarGen =
    match tryWrappedMultiplication functionNames funcName returnType integerParams expr, tryLinearBindings expr with
    | Some wrapped, Some (bindings, _) ->
        transformWrappedMultiplication helperName accumulatorId varGen wrapped bindings
    | _ ->
        match expr with
        | Jump _ -> (expr, varGen)
        | Join (parameter, continuation, entry) ->
            let body, next = transformMultiplicationAccumulatorBody functionNames funcName returnType integerParams helperName accumulatorId varGen continuation
            let entry', final = transformMultiplicationAccumulatorBody functionNames funcName returnType integerParams helperName accumulatorId next entry
            (Join (parameter, body, entry'), final)
        | Return atom ->
            let (resultId, varGen') = freshVar varGen
            (Let (resultId, Prim (Mul, Var accumulatorId, atom), Return (Var resultId)), varGen')
        | Let (tempId, cexpr, body) ->
            let (body', varGen') =
                transformMultiplicationAccumulatorBody functionNames funcName returnType integerParams helperName accumulatorId varGen body
            (Let (tempId, cexpr, body'), varGen')
        | If (cond, thenBranch, elseBranch) ->
            let (thenBranch', varGenAfterThen) =
                transformMultiplicationAccumulatorBody functionNames funcName returnType integerParams helperName accumulatorId varGen thenBranch
            let (elseBranch', varGenAfterElse) =
                transformMultiplicationAccumulatorBody functionNames funcName returnType integerParams helperName accumulatorId varGenAfterThen elseBranch
            (If (cond, thenBranch', elseBranch'), varGenAfterElse)

let private transformWrappedSubtraction
    (helperName: AST.FunctionId)
    (accumulatorId: TempId)
    (varGen: VarGen)
    (wrapped: WrappedSubtraction)
    (bindings: (TempId * CExpr) list)
    : AExpr * VarGen =
    let nextAccumulatorId, varGen' = freshVar varGen
    let rec rewrite remaining =
        match remaining with
        | [] -> Return (Var wrapped.CallId)
        | (tempId, _) :: rest when tempId = wrapped.ResultId -> rewrite rest
        | (tempId, Call (_, _)) :: rest when tempId = wrapped.CallId ->
            Let (
                nextAccumulatorId,
                Prim (Sub, Var accumulatorId, wrapped.Subtrahend),
                Let (
                    tempId,
                    Call (helperName, wrapped.CallArgs @ [Var nextAccumulatorId]),
                    rewrite rest
                )
            )
        | binding :: rest -> rebuildBindings [binding] (rewrite rest)
    (rewrite bindings, varGen')

let rec private transformSubtractionAccumulatorBody
    (functionNames: FunctionNameRegistry)
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (integerParams: Set<TempId>)
    (helperName: AST.FunctionId)
    (accumulatorId: TempId)
    (varGen: VarGen)
    (expr: AExpr)
    : AExpr * VarGen =
    match tryWrappedSubtraction functionNames funcName returnType integerParams expr, tryLinearBindings expr with
    | Some wrapped, Some (bindings, _) ->
        transformWrappedSubtraction helperName accumulatorId varGen wrapped bindings
    | _ ->
        match expr with
        | Jump _ -> (expr, varGen)
        | Join (parameter, continuation, entry) ->
            let body, next =
                transformSubtractionAccumulatorBody
                    functionNames funcName returnType integerParams helperName accumulatorId varGen continuation
            let entry', final =
                transformSubtractionAccumulatorBody
                    functionNames funcName returnType integerParams helperName accumulatorId next entry
            (Join (parameter, body, entry'), final)
        | Return atom ->
            let resultId, varGen' = freshVar varGen
            (Let (resultId, Prim (Add, Var accumulatorId, atom), Return (Var resultId)), varGen')
        | Let (tempId, cexpr, body) ->
            let body', varGen' =
                transformSubtractionAccumulatorBody
                    functionNames funcName returnType integerParams helperName accumulatorId varGen body
            (Let (tempId, cexpr, body'), varGen')
        | If (cond, thenBranch, elseBranch) ->
            let thenBranch', afterThen =
                transformSubtractionAccumulatorBody
                    functionNames funcName returnType integerParams helperName accumulatorId varGen thenBranch
            let elseBranch', afterElse =
                transformSubtractionAccumulatorBody
                    functionNames funcName returnType integerParams helperName accumulatorId afterThen elseBranch
            (If (cond, thenBranch', elseBranch'), afterElse)

let private transformWrappedListPrepend
    (helperName: AST.FunctionId)
    (accumulatorId: TempId)
    (suffixCellId: TempId)
    (varGen: VarGen)
    (wrapped: WrappedListPrepend)
    (bindings: (TempId * CExpr) list)
    : AExpr * VarGen =
    let (nextAccumulatorId, varGen') = freshVar varGen
    let rec rewrite remaining =
        match remaining with
        | [] -> Return (Var wrapped.CallId)
        | (tempId, _) :: rest when tempId = wrapped.ResultId -> rewrite rest
        | (tempId, Call (_, _)) :: rest when tempId = wrapped.CallId ->
            Let (
                nextAccumulatorId,
                Call (wrapped.PushName, [Var accumulatorId; wrapped.Value]),
                Let (
                    tempId,
                    Call (helperName, wrapped.CallArgs @ [Var nextAccumulatorId; Var suffixCellId]),
                    rewrite rest
                )
            )
        | binding :: rest -> rebuildBindings [binding] (rewrite rest)
    (rewrite bindings, varGen')

let rec private transformListAccumulatorBody
    (listPushIds: Set<AST.FunctionId>)
    (funcName: AST.FunctionId)
    (helperName: AST.FunctionId)
    (listType: AST.Type)
    (accumulatorId: TempId)
    (suffixCellId: TempId)
    (varGen: VarGen)
    (expr: AExpr)
    : AExpr * VarGen =
    match tryWrappedListPrepend listPushIds funcName expr, tryLinearBindings expr with
    | Some wrapped, Some (bindings, _) ->
        transformWrappedListPrepend helperName accumulatorId suffixCellId varGen wrapped bindings
    | _ ->
        match expr with
        | Jump _ -> (expr, varGen)
        | Join (parameter, continuation, entry) ->
            let body, next =
                transformListAccumulatorBody listPushIds funcName helperName listType accumulatorId suffixCellId varGen continuation
            let entry', final =
                transformListAccumulatorBody listPushIds funcName helperName listType accumulatorId suffixCellId next entry
            (Join (parameter, body, entry'), final)
        | Return atom ->
            let (storeId, varGen') = freshVar varGen
            (Let (
                storeId,
                RawSlotInit (Var suffixCellId, IntLiteral (Int64 0L), atom, listType),
                Return (Var accumulatorId)
             ), varGen')
        | Let (tempId, cexpr, body) ->
            let body', varGen' =
                transformListAccumulatorBody listPushIds funcName helperName listType accumulatorId suffixCellId varGen body
            (Let (tempId, cexpr, body'), varGen')
        | If (cond, thenBranch, elseBranch) ->
            let thenBranch', varGenAfterThen =
                transformListAccumulatorBody listPushIds funcName helperName listType accumulatorId suffixCellId varGen thenBranch
            let elseBranch', varGenAfterElse =
                transformListAccumulatorBody listPushIds funcName helperName listType accumulatorId suffixCellId varGenAfterThen elseBranch
            (If (cond, thenBranch', elseBranch'), varGenAfterElse)

let private layerWithPlaceholder
    (layer: ConstructorLayer)
    (placeholder: Atom)
    : CExpr =
    let replace fields =
        fields
        |> List.mapi (fun index atom -> if index = layer.HoleIndex then placeholder else atom)
    match layer.Kind with
    | TupleLayer fields -> TupleAlloc (replace fields)
    | RecordLayer (descriptor, fields) -> RecordAlloc (descriptor, replace fields)

let private buildConstructorLayers
    (incomingDestination: (Atom * Atom) option)
    (existingRoot: TempId option)
    (layersInsideOut: ConstructorLayer list)
    (varGen: VarGen)
    : (TempId * Atom * Atom * (TempId * CExpr) list * VarGen) option =
    let outerToInner = List.rev layersInsideOut
    let rec build remaining destination rootId reversedBindings vg =
        match remaining with
        | [] ->
            match destination, rootId with
            | Some (rawId, offset), Some root ->
                Some (root, rawId, offset, List.rev reversedBindings, vg)
            | _ -> None
        | layer :: rest ->
            let placeholderId, vgAfterPlaceholder = freshVar vg
            let rawId, vgAfterRaw = freshVar vgAfterPlaceholder
            let offset = IntLiteral (Int64 (int64 (layer.HoleIndex * 8)))
            let allocation = layerWithPlaceholder layer (Var placeholderId)
            let baseBindings =
                [ (placeholderId, TypedAtom (IntLiteral (Int64 0L), layer.ChildType))
                  (layer.ResultId, allocation)
                  (rawId, FixedBlockToRawPtr (Var layer.ResultId)) ]
            let storeBindings, vgAfterStore =
                match destination with
                | None -> ([], vgAfterRaw)
                | Some (destinationPtr, destinationOffset) ->
                    let storeId, next = freshVar vgAfterRaw
                    ([ (storeId,
                        RawSlotInit (
                            destinationPtr,
                            destinationOffset,
                            Var layer.ResultId,
                            layer.ResultType)) ], next)
            let nextRoot =
                match destination with
                | None -> Some layer.ResultId
                | Some _ -> rootId
            build
                rest
                (Some (Var rawId, offset))
                nextRoot
                (List.rev (baseBindings @ storeBindings) @ reversedBindings)
                vgAfterStore
    match outerToInner with
    | [] -> None
    | _ -> build outerToInner incomingDestination existingRoot [] varGen

let private transformConstructorContext
    (helperName: AST.FunctionId)
    (destinationId: TempId option)
    (destinationOffsetId: TempId option)
    (rootId: TempId option)
    (context: ConstructorContext)
    (varGen: VarGen)
    : AExpr * VarGen =
    let incomingDestination =
        match destinationId, destinationOffsetId with
        | Some destination, Some offset -> Some (Var destination, Var offset)
        | None, None -> None
        | _ -> Crash.crash "Constructor TRMC destination parameters are incomplete"
    match buildConstructorLayers incomingDestination rootId context.LayersInsideOut varGen with
    | None -> Crash.crash "Constructor TRMC context has no constructor layers"
    | Some (constructedRootId, leafRawId, leafOffset, constructorBindings, vgAfterConstructors) ->
        let leafOffsetId, vgAfterOffset = freshVar vgAfterConstructors
        let callResultId, vgAfterCall = freshVar vgAfterOffset
        let returnedRoot = rootId |> Option.defaultValue constructedRootId
        let body =
            Let (
                leafOffsetId,
                Atom leafOffset,
                Let (
                    callResultId,
                    Call (
                        helperName,
                        context.CallArgs
                        @ [ leafRawId
                            Var leafOffsetId
                            Var returnedRoot ]
                    ),
                    Return (Var callResultId)
                )
            )
        (rebuildBindings context.Prefix (rebuildBindings constructorBindings body), vgAfterCall)

let rec private transformConstructorBody
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (helperName: AST.FunctionId)
    (destinationId: TempId)
    (destinationOffsetId: TempId)
    (rootId: TempId)
    (varGen: VarGen)
    (expr: AExpr)
    : AExpr * VarGen =
    match tryConstructorContext funcName returnType expr with
    | Some context ->
        transformConstructorContext
            helperName
            (Some destinationId)
            (Some destinationOffsetId)
            (Some rootId)
            context
            varGen
    | None ->
        match expr with
        | Jump _ -> (expr, varGen)
        | Return atom ->
            let storeId, next = freshVar varGen
            (Let (
                storeId,
                RawSlotInit (Var destinationId, Var destinationOffsetId, atom, returnType),
                Return (Var rootId)
             ), next)
        | Let (tempId, cexpr, body) ->
            let body', next =
                transformConstructorBody
                    funcName returnType helperName destinationId destinationOffsetId rootId varGen body
            (Let (tempId, cexpr, body'), next)
        | Join (parameter, continuation, entry) ->
            let continuation', next =
                transformConstructorBody
                    funcName returnType helperName destinationId destinationOffsetId rootId varGen continuation
            let entry', final =
                transformConstructorBody
                    funcName returnType helperName destinationId destinationOffsetId rootId next entry
            (Join (parameter, continuation', entry'), final)
        | If (cond, thenBranch, elseBranch) ->
            let thenBranch', next =
                transformConstructorBody
                    funcName returnType helperName destinationId destinationOffsetId rootId varGen thenBranch
            let elseBranch', final =
                transformConstructorBody
                    funcName returnType helperName destinationId destinationOffsetId rootId next elseBranch
            (If (cond, thenBranch', elseBranch'), final)

let private freshHelperName (usedNames: Set<string>) (funcName: string) : string =
    let rec choose suffix =
        let candidate =
            if suffix = 0 then $"{funcName}$trmo"
            else $"{funcName}$trmo{suffix}"
        if Set.contains candidate usedNames then choose (suffix + 1) else candidate
    choose 0

let private plannedHelpers (functionNames: FunctionNameRegistry) =
    let initialNames = functionNames |> Map.values |> Set.ofSeq
    let helperNames, _ =
        functionNames
        |> Map.toList
        |> List.sortBy snd
        |> List.mapFold (fun usedNames (functionId, functionName) ->
            let helperName = freshHelperName usedNames functionName
            ((functionId, helperName), Set.add helperName usedNames)) initialNames
    let helperIds =
        AST.allocateFunctionIds
            (functionNames |> Map.keys)
            (helperNames |> List.map snd)
    helperNames
    |> List.map (fun (functionId, helperName) ->
        functionId, (helperName, Map.find helperName helperIds))
    |> Map.ofList

let rec private transformConstructorWrapperBody
    (funcName: AST.FunctionId)
    (returnType: AST.Type)
    (helperName: AST.FunctionId)
    (varGen: VarGen)
    (expr: AExpr)
    : AExpr * VarGen =
    match tryConstructorContext funcName returnType expr with
    | Some context ->
        transformConstructorContext helperName None None None context varGen
    | None ->
        match expr with
        | Jump _ | Return _ -> (expr, varGen)
        | Let (tempId, cexpr, body) ->
            let body', next = transformConstructorWrapperBody funcName returnType helperName varGen body
            (Let (tempId, cexpr, body'), next)
        | Join (parameter, continuation, entry) ->
            let continuation', next =
                transformConstructorWrapperBody funcName returnType helperName varGen continuation
            let entry', final =
                transformConstructorWrapperBody funcName returnType helperName next entry
            (Join (parameter, continuation', entry'), final)
        | If (cond, thenBranch, elseBranch) ->
            let thenBranch', next =
                transformConstructorWrapperBody funcName returnType helperName varGen thenBranch
            let elseBranch', final =
                transformConstructorWrapperBody funcName returnType helperName next elseBranch
            (If (cond, thenBranch', elseBranch'), final)

/// Lower linear recursion beneath tuple-backed sum constructors and record
/// constructors with destination passing. Constructor fields other than the
/// recursive path must already be atoms, so no effects move across the call.
let internal transformTailRecursionModuloFixedConstructors
    (functionNames: FunctionNameRegistry)
    (eligibleFunctions: Set<AST.FunctionId>)
    (program: Program)
    : Program =
    let (Program (functions, mainExpr)) = program
    let helpers = plannedHelpers functionNames
    let initialNames =
        Set.union
            (functions |> List.map (fun func -> func.Name) |> Set.ofList)
            (functionNames |> Map.values |> Set.ofSeq)
    let initialVarGen = freshVarGenForProgram program
    let functionsReversed, _, _ =
        functions
        |> List.fold
            (fun (rewritten, usedNames, varGen) func ->
                let contexts = constructorContextCount func.Id func.ReturnType func.Body
                let recursiveCalls = selfCallCount func.Id func.Body
                let managedReturn =
                    match func.ReturnType with
                    | AST.TRecord _ | AST.TSum _ -> true
                    | _ -> false
                if not (Set.contains func.Id eligibleFunctions)
                   || not managedReturn
                   || contexts = 0
                   || contexts <> recursiveCalls then
                    (func :: rewritten, Set.add func.Name usedNames, varGen)
                else
                    let helperName, helperId = Map.find func.Id helpers
                    let destinationId, afterDestination = freshVar varGen
                    let destinationOffsetId, afterOffset = freshVar afterDestination
                    let rootId, afterRoot = freshVar afterOffset
                    let helperBody, afterHelper =
                        transformConstructorBody
                            func.Id
                            func.ReturnType
                            helperId
                            destinationId
                            destinationOffsetId
                            rootId
                            afterRoot
                            func.Body
                    let wrapperBody, afterWrapper =
                        transformConstructorWrapperBody
                            func.Id func.ReturnType helperId afterHelper func.Body
                    let helper = {
                        func with
                            Id = helperId
                            Name = helperName
                            TypedParams =
                                func.TypedParams
                                @ [ { Id = destinationId; Type = AST.TRawPtr }
                                    { Id = destinationOffsetId; Type = AST.TInt64 }
                                    { Id = rootId; Type = func.ReturnType } ]
                            Body = helperBody
                    }
                    let wrapper = { func with Body = wrapperBody }
                    (helper :: wrapper :: rewritten,
                     Set.add helperName usedNames,
                     afterWrapper))
            ([], initialNames, initialVarGen)
    Program (List.rev functionsReversed, mainExpr)

let internal transformTailRecursionModuloAddition
    (functionNames: FunctionNameRegistry)
    (eligibleFunctions: Set<AST.FunctionId>)
    (program: Program)
    : Program =
    let (Program (functions, mainExpr)) = program
    let helpers = plannedHelpers functionNames
    let initialNames =
        Set.union
            (functions |> List.map (fun func -> func.Name) |> Set.ofList)
            (functionNames |> Map.values |> Set.ofSeq)
    let initialVarGen = freshVarGenForProgram program
    let (functionsReversed, _, _) =
        functions
        |> List.fold
            (fun (rewritten, usedNames, varGen) func ->
                let pairs = siblingAdditionCount functionNames func.Id func.ReturnType func.Body
                let recursiveCalls = selfCallCount func.Id func.Body
                let eligible =
                    Set.contains func.Id eligibleFunctions
                    && (nativeIntegerTypeName func.ReturnType |> Option.isSome)
                    && pairs > 0
                    && recursiveCalls = pairs * 2
                if not eligible then
                    (func :: rewritten, Set.add func.Name usedNames, varGen)
                else
                    let helperName, helperId = Map.find func.Id helpers
                    let (accumulatorId, varGenAfterAccumulator) = freshVar varGen
                    let (helperBody, varGenAfterHelper) =
                        transformAccumulatorBody
                            functionNames
                            func.Id
                            func.ReturnType
                            helperId
                            accumulatorId
                            (integerLiteral func.ReturnType 0)
                            varGenAfterAccumulator
                            func.Body
                    let (wrapperResultId, varGenAfterWrapper) = freshVar varGenAfterHelper
                    let helper = {
                        func with
                            Id = helperId
                            Name = helperName
                            TypedParams =
                                func.TypedParams @ [{ Id = accumulatorId; Type = func.ReturnType }]
                            Body = helperBody
                    }
                    let wrapper = {
                        func with
                            Body =
                                Let (
                                    wrapperResultId,
                                    Call (
                                        helperId,
                                        (func.TypedParams |> List.map (fun param -> Var param.Id))
                                        @ [integerLiteral func.ReturnType 0]
                                    ),
                                    Return (Var wrapperResultId)
                                )
                    }
                    (helper :: wrapper :: rewritten, Set.add helperName usedNames, varGenAfterWrapper))
            ([], initialNames, initialVarGen)
    Program (List.rev functionsReversed, mainExpr)

/// Turn direct recursive native-integer multiplication with a pure
/// parameter/literal factor into an accumulator helper. Modular machine-word
/// multiplication is associative at every supported width.
let internal transformTailRecursionModuloMultiplication
    (functionNames: FunctionNameRegistry)
    (eligibleFunctions: Set<AST.FunctionId>)
    (program: Program)
    : Program =
    let (Program (functions, mainExpr)) = program
    let helpers = plannedHelpers functionNames
    let initialNames =
        Set.union
            (functions |> List.map (fun func -> func.Name) |> Set.ofList)
            (functionNames |> Map.values |> Set.ofSeq)
    let initialVarGen = freshVarGenForProgram program
    let (functionsReversed, _, _) =
        functions
        |> List.fold
            (fun (rewritten, usedNames, varGen) func ->
                let integerParams =
                    func.TypedParams
                    |> List.choose (fun param -> if param.Type = func.ReturnType then Some param.Id else None)
                    |> Set.ofList
                let wrappedCalls =
                    wrappedMultiplicationCount functionNames func.Id func.ReturnType integerParams func.Body
                let recursiveCalls = selfCallCount func.Id func.Body
                let eligible =
                    Set.contains func.Id eligibleFunctions
                    && (nativeIntegerTypeName func.ReturnType |> Option.isSome)
                    && wrappedCalls > 0
                    && recursiveCalls = wrappedCalls
                if not eligible then
                    (func :: rewritten, Set.add func.Name usedNames, varGen)
                else
                    let helperName, helperId = Map.find func.Id helpers
                    let (accumulatorId, varGenAfterAccumulator) = freshVar varGen
                    let (helperBody, varGenAfterHelper) =
                        transformMultiplicationAccumulatorBody
                            functionNames func.Id func.ReturnType integerParams helperId accumulatorId varGenAfterAccumulator func.Body
                    let (wrapperResultId, varGenAfterWrapper) = freshVar varGenAfterHelper
                    let helper = {
                        func with
                            Id = helperId
                            Name = helperName
                            TypedParams = func.TypedParams @ [{ Id = accumulatorId; Type = func.ReturnType }]
                            Body = helperBody
                    }
                    let wrapper = {
                        func with
                            Body =
                                Let (
                                    wrapperResultId,
                                    Call (helperId, (func.TypedParams |> List.map (fun param -> Var param.Id)) @ [integerLiteral func.ReturnType 1]),
                                    Return (Var wrapperResultId)
                                )
                    }
                    (helper :: wrapper :: rewritten, Set.add helperName usedNames, varGenAfterWrapper))
            ([], initialNames, initialVarGen)
    Program (List.rev functionsReversed, mainExpr)

let internal transformTailRecursionModuloSubtraction
    (functionNames: FunctionNameRegistry)
    (eligibleFunctions: Set<AST.FunctionId>)
    (program: Program)
    : Program =
    let (Program (functions, mainExpr)) = program
    let helpers = plannedHelpers functionNames
    let initialNames =
        Set.union
            (functions |> List.map (fun func -> func.Name) |> Set.ofList)
            (functionNames |> Map.values |> Set.ofSeq)
    let initialVarGen = freshVarGenForProgram program
    let functionsReversed, _, _ =
        functions
        |> List.fold
            (fun (rewritten, usedNames, varGen) func ->
                let integerParams =
                    func.TypedParams
                    |> List.choose (fun param -> if param.Type = func.ReturnType then Some param.Id else None)
                    |> Set.ofList
                let wrappedCalls =
                    wrappedSubtractionCount functionNames func.Id func.ReturnType integerParams func.Body
                let recursiveCalls = selfCallCount func.Id func.Body
                let eligible =
                    Set.contains func.Id eligibleFunctions
                    && (nativeIntegerTypeName func.ReturnType |> Option.isSome)
                    && wrappedCalls > 0
                    && recursiveCalls = wrappedCalls
                if not eligible then
                    (func :: rewritten, Set.add func.Name usedNames, varGen)
                else
                    let helperName, helperId = Map.find func.Id helpers
                    let accumulatorId, afterAccumulator = freshVar varGen
                    let helperBody, afterHelper =
                        transformSubtractionAccumulatorBody
                            functionNames func.Id func.ReturnType integerParams helperId accumulatorId afterAccumulator func.Body
                    let wrapperResultId, afterWrapper = freshVar afterHelper
                    let helper = {
                        func with
                            Id = helperId
                            Name = helperName
                            TypedParams = func.TypedParams @ [{ Id = accumulatorId; Type = func.ReturnType }]
                            Body = helperBody
                    }
                    let wrapper = {
                        func with
                            Body =
                                Let (
                                    wrapperResultId,
                                    Call (
                                        helperId,
                                        (func.TypedParams |> List.map (fun param -> Var param.Id))
                                        @ [integerLiteral func.ReturnType 0]
                                    ),
                                    Return (Var wrapperResultId)
                                )
                    }
                    (helper :: wrapper :: rewritten, Set.add helperName usedNames, afterWrapper))
            ([], initialNames, initialVarGen)
    Program (List.rev functionsReversed, mainExpr)

/// Turn recursive `List.push (self ...) value` construction into a reverse
/// accumulator loop and finish with the existing linear `__reverseInto`
/// kernel. Every recursive call must have the same constructor boundary.
let internal transformTailRecursionModuloListConstructors
    (functionNames: FunctionNameRegistry)
    (eligibleFunctions: Set<AST.FunctionId>)
    (externalFunctions: Map<string, Function>)
    (program: Program)
    : Program =
    let (Program (functions, mainExpr)) = program
    let helpers = plannedHelpers functionNames
    let initialNames =
        Set.union
            (functions |> List.map (fun func -> func.Name) |> Set.ofList)
            (functionNames |> Map.values |> Set.ofSeq)
    let initialVarGen = freshVarGenForProgram program
    let externalNamesById =
        externalFunctions
        |> Map.toSeq
        |> Seq.map (fun (_, func) -> func.Id, func.Name)
        |> Map.ofSeq
    let listPushIds =
        externalNamesById
        |> Map.filter (fun _ name -> name.StartsWith("Darklang.Stdlib.List.push_"))
        |> Map.keys
        |> Set.ofSeq
    let (functionsReversed, _, _) =
        functions
        |> List.fold
            (fun (rewritten, usedNames, varGen) func ->
                let wrappedCalls = wrappedListPrependCount listPushIds func.Id func.Body
                let prependCalls = listPrependCallCount listPushIds func.Body
                let recursiveCalls = selfCallCount func.Id func.Body
                let pushName =
                    let rec find expr =
                        match tryWrappedListPrepend listPushIds func.Id expr with
                        | Some wrapped -> Some wrapped.PushName
                        | None ->
                            match expr with
                            | Jump _ | Return _ -> None
                            | Let (_, _, body) -> find body
                            | Join (_, continuation, entry) -> find continuation |> Option.orElseWith (fun () -> find entry)
                            | If (_, thenBranch, elseBranch) -> find thenBranch |> Option.orElseWith (fun () -> find elseBranch)
                    find func.Body
                let finishTarget =
                    pushName
                    |> Option.bind (fun id -> Map.tryFind id externalNamesById)
                    |> Option.map (fun name ->
                        name.Replace(
                            "Darklang.Stdlib.List.push_",
                            "Darklang.Stdlib.List.__reverseInto_"
                        ))
                let eligible =
                    match func.ReturnType, finishTarget with
                    | AST.TList _, Some target ->
                        Set.contains func.Id eligibleFunctions
                        && wrappedCalls > 0
                        && recursiveCalls = wrappedCalls
                        && prependCalls = wrappedCalls
                        && Map.containsKey target externalFunctions
                    | _ -> false
                if not eligible then
                    (func :: rewritten, Set.add func.Name usedNames, varGen)
                else
                    let targetName = finishTarget |> Option.defaultWith (fun () -> Crash.crash "Eligible list TRMC function lost its finish target")
                    let target =
                        Map.tryFind targetName externalFunctions
                        |> Option.map (fun func -> func.Id)
                        |> Option.defaultWith (fun () -> Crash.crash "Eligible list TRMC finish target is absent")
                    let helperName, helperId = Map.find func.Id helpers
                    let accumulatorId, varGenAfterAccumulator = freshVar varGen
                    let suffixCellId, varGenAfterSuffixCell = freshVar varGenAfterAccumulator
                    let helperBody, varGenAfterHelper =
                        transformListAccumulatorBody
                            listPushIds
                            func.Id
                            helperId
                            func.ReturnType
                            accumulatorId
                            suffixCellId
                            varGenAfterSuffixCell
                            func.Body
                    let emptyId, varGenAfterEmpty = freshVar varGenAfterHelper
                    let initialSuffixCellId, varGenAfterInitialSuffixCell = freshVar varGenAfterEmpty
                    let reversedId, varGenAfterReversed = freshVar varGenAfterInitialSuffixCell
                    let suffixId, varGenAfterSuffix = freshVar varGenAfterReversed
                    let wrapperResultId, varGenAfterWrapper = freshVar varGenAfterSuffix
                    let helper = {
                        func with
                            Id = helperId
                            Name = helperName
                            TypedParams =
                                func.TypedParams
                                @ [ { Id = accumulatorId; Type = func.ReturnType }
                                    { Id = suffixCellId; Type = AST.TTuple [func.ReturnType] } ]
                            Body = helperBody
                    }
                    let wrapper = {
                        func with
                            Body =
                                Let (
                                    emptyId,
                                    TypedAtom (IntLiteral (Int64 0L), func.ReturnType),
                                    Let (
                                        initialSuffixCellId,
                                        TupleAlloc [Var emptyId],
                                        Let (
                                            reversedId,
                                            Call (
                                                helperId,
                                                (func.TypedParams |> List.map (fun param -> Var param.Id))
                                                @ [Var emptyId; Var initialSuffixCellId]
                                            ),
                                            Let (
                                                suffixId,
                                                TupleGet (Var initialSuffixCellId, 0),
                                                Let (
                                                    wrapperResultId,
                                                    Call (
                                                        target,
                                                        [Var reversedId; Var suffixId]
                                                    ),
                                                    Return (Var wrapperResultId)
                                                )
                                            )
                                        )
                                    )
                                )
                    }
                    (helper :: wrapper :: rewritten, Set.add helperName usedNames, varGenAfterWrapper))
            ([], initialNames, initialVarGen)
    Program (List.rev functionsReversed, mainExpr)
