// Cleanup.fs - Plan retain/release placement and preserve cleanup across tail calls.

module RcCleanup

open MemoryModel
open MemoryPlanning
open ANF
open LiftExpressions
open LiftFunctions
open ANFContinuations
open LoweringExpressions
open RcTypeFacts
open RcReturnAnalysis
open RcShapePlanning

type internal ReturnDec =
    TempId * AST.Type * RcShape * RcKind option * RcMetadata option

let internal createReturnDec
    (ctx: TypeContext)
    (tempId: TempId)
    (typ: AST.Type)
    (shape: RcShape)
    (kindOverride: RcKind option)
    : ReturnDec =
    let metadata =
        match rcShapeReleaseOperation shape with
        | Some (FixedSizeRoot _) ->
            Some (rcMetadataForTypeAndShape ctx typ shape)
        | Some DynamicStringBuffer
        | Some DynamicBlobBuffer
        | None ->
            None
    (tempId, typ, shape, kindOverride, metadata)

let internal retainExprForShape
    (ctx: TypeContext)
    (tempId: TempId)
    (typ: AST.Type)
    (shape: RcShape)
    : CExpr =
    match rcShapeRetainOperation shape with
    | Some DynamicStringBuffer ->
        RefCountIncString (Var tempId)
    | Some DynamicBlobBuffer ->
        RefCountIncBlob (Var tempId)
    | Some (FixedSizeRoot (size, kind)) ->
        RefCountInc (
            Var tempId,
            size,
            kind,
            Some (rcMetadataForTypeAndShape ctx typ shape))
    | None ->
        Crash.crash $"retainExprForShape: type '{typ}' does not have an RC retain operation"

let private releaseExprForShape
    (tempId: TempId)
    (typ: AST.Type)
    (shape: RcShape)
    (kindOverride: RcKind option)
    (metadata: RcMetadata option)
    : CExpr =
    match rcShapeReleaseOperation shape with
    | Some DynamicStringBuffer ->
        RefCountDecString (Var tempId)
    | Some DynamicBlobBuffer ->
        RefCountDecBlob (Var tempId)
    | Some (FixedSizeRoot (size, defaultKind)) ->
        let kind = kindOverride |> Option.defaultValue defaultKind
        let metadata =
            match metadata with
            | Some metadata -> metadata
            | None ->
                Crash.crash
                    $"releaseExprForShape: fixed-size type '{typ}' is missing RC metadata"
        RefCountDec (
            Var tempId,
            size,
            kind,
            Some metadata)
    | None ->
        Crash.crash $"releaseExprForShape: type '{typ}' does not have an RC release operation"

let internal functionParamReturnTransfersOwnedAccumulator
    (ctx: TypeContext)
    (funcName: string)
    (paramIndex: int)
    (paramType: AST.Type)
    : bool =
    let isMapHelper =
        funcName = "Darklang.Stdlib.List.__mapHelper"
        || funcName.StartsWith("Darklang.Stdlib.List.__mapHelper_")
    let returnsClosureList =
        match tryGetFuncReturnTypeFromReg ctx funcName with
        | Some (AST.TList (AST.TFunction _)) -> true
        | _ -> false

    match isMapHelper, paramIndex, paramType with
    | true, 0, AST.TList _ when returnsClosureList -> true
    | true, 2, AST.TList _ -> true
    | _ -> false

/// Recognize a managed parameter used as the sole returned accumulator of a
/// direct self-recursive loop. The caller restricts this proof to RC-managed
/// shapes. The function keeps its own reference to the parameter so each
/// backedge can release the previous value before adopting its freshly-owned
/// replacement.
let internal isInternalOwnedTailAccumulator
    (func: Function)
    (paramIndex: int)
    (param: TypedParam)
    : bool =
    let rec canonicalAlias (aliases: Map<TempId, TempId>) (tempId: TempId) : TempId =
        match Map.tryFind tempId aliases with
        | Some sourceId when sourceId <> tempId -> canonicalAlias aliases sourceId
        | _ -> tempId

    let rec analyze
        (aliases: Map<TempId, TempId>)
        (expr: AExpr)
        : bool * bool =
        match expr with
        | Join _ | Jump _ -> (false, false)
        | Return (Var tempId) ->
            (canonicalAlias aliases tempId = param.Id, false)
        | Return _ ->
            (false, false)
        | Let (callTemp, Call (targetFunc, args), Return (Var returnTemp))
            when targetFunc = func.Name && callTemp = returnTemp ->
            match List.tryItem paramIndex args with
            | Some (Var replacement) when canonicalAlias aliases replacement <> param.Id ->
                (true, true)
            | _ ->
                (false, false)
        | Let (tempId, Atom (Var sourceId), body)
        | Let (tempId, TypedAtom (Var sourceId, _), body) ->
            analyze (Map.add tempId (canonicalAlias aliases sourceId) aliases) body
        | Let (_, Call (targetFunc, _), _) when targetFunc = func.Name ->
            (false, false)
        | Let (_, _, body) ->
            analyze aliases body
        | If (_, thenBranch, elseBranch) ->
            let (thenValid, thenRecurses) = analyze aliases thenBranch
            let (elseValid, elseRecurses) = analyze aliases elseBranch
            (thenValid && elseValid, thenRecurses || elseRecurses)

    let supportedAccumulator =
        match param.Type with
        | AST.TRecord _ -> true
        | _ -> func.Name.Contains("$trmo")

    match func.ReturnType with
    | returnType when supportedAccumulator && returnType = param.Type ->
        let (valid, recurses) = analyze Map.empty func.Body
        valid && recurses
    | _ ->
        false

/// Insert RefCountInc for returned parameters at a Return node
let insertParamIncsAtReturn
    (ctx: TypeContext)
    (paramIncs: (TempId * AST.Type * RcShape) list)
    (returned: Set<TempId>)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let active =
        paramIncs
        |> List.filter (fun (tempId, _, _) -> Set.contains tempId returned)
    List.foldBack
        (fun (tempId, typ, shape) (accExpr, accVarGen, accTypes) ->
            let (dummyId, varGen') = freshVar accVarGen
            let incExpr = retainExprForShape ctx tempId typ shape
            let accExpr' = Let (dummyId, incExpr, accExpr)
            (accExpr', varGen', Map.add dummyId AST.TUnit accTypes))
        active
        (expr, varGen, types)

/// Insert RefCountDec operations before a Return using the current dec stack
let insertReturnDecs
    (returnDecs: ReturnDec list)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let decsInOrder = List.rev returnDecs
    List.fold
        (fun (accExpr, accVarGen, accTypes) (tempId, typ, shape, kindOverride, metadata) ->
            let (dummyId, varGen') = freshVar accVarGen
            let decExpr = releaseExprForShape tempId typ shape kindOverride metadata
            let accExpr' = Let (dummyId, decExpr, accExpr)
            (accExpr', varGen', Map.add dummyId AST.TUnit accTypes))
        (expr, varGen, types)
        decsInOrder

/// Stored state for rebuilding a Let while unwinding an expression spine
type LetFrame = {
    TempId: TempId
    CExpr: CExpr
    TupleIncTargets: (TempId * AST.Type * RcShape) list
    /// The pass owns exactly one pending release for this value, and its next
    /// use transfers that ownership into a closed returned aggregate suffix.
    TransferableOwnership: ReturnDec option
    ReturnInc: (AST.Type * RcShape) option
    BranchDec: ReturnDec option
}

/// Apply a single Let frame around an expression (uses current varGen/types)
let applyLetFrame
    (ctx: TypeContext)
    (frame: LetFrame)
    (expr: AExpr, varGen: VarGen, types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let (incBindingsRev, varGen1) =
        frame.TupleIncTargets
        |> List.fold (fun (acc, vg) (tid, typ, shape) ->
            let (dummyId, vg') = freshVar vg
            ((dummyId, retainExprForShape ctx tid typ shape) :: acc, vg')) ([], varGen)
    let incBindings = List.rev incBindingsRev

    let typesWithIncs =
        incBindings
        |> List.fold (fun m (tid, _) -> Map.add tid AST.TUnit m) types

    let (returnIncBinding, varGen2, typesWithReturnInc) =
        match frame.ReturnInc with
        | Some (typ, shape) ->
            let (incId, vg) = freshVar varGen1
            let incExpr = retainExprForShape ctx frame.TempId typ shape
            ([(incId, incExpr)], vg, Map.add incId AST.TUnit typesWithIncs)
        | None ->
            ([], varGen1, typesWithIncs)

    let bodyWithReturnInc = wrapBindings returnIncBinding expr
    let letExpr = Let (frame.TempId, frame.CExpr, bodyWithReturnInc)
    let exprWithIncs = wrapBindings incBindings letExpr
    (exprWithIncs, varGen2, typesWithReturnInc)

/// Apply a stack of Let frames (innermost-first)
let applyLetFrames
    (ctx: TypeContext)
    (frames: LetFrame list)
    (expr: AExpr, varGen: VarGen, types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let folder
        ((accExpr, accVarGen, accTypes): AExpr * VarGen * Map<TempId, AST.Type>)
        (frame: LetFrame)
        : AExpr * VarGen * Map<TempId, AST.Type> =
        applyLetFrame ctx frame (accExpr, accVarGen, accTypes)
    List.fold folder (expr, varGen, types) frames

let private tailCallArgTempIds (cexpr: CExpr) : Set<TempId> =
    let fromAtom (atom: Atom) : Set<TempId> =
        match atom with
        | Var tid -> Set.singleton tid
        | _ -> Set.empty
    match cexpr with
    | TailCall (_, args) ->
        args |> List.fold (fun acc atom -> Set.union acc (fromAtom atom)) Set.empty
    | IndirectTailCall (func, args) ->
        (fromAtom func, args)
        ||> List.fold (fun acc atom -> Set.union acc (fromAtom atom))
    | ClosureTailCall (closure, args) ->
        (fromAtom closure, args)
        ||> List.fold (fun acc atom -> Set.union acc (fromAtom atom))
    | _ ->
        Set.empty

let private isSelfTailCallTarget (currentFuncName: string) (targetFunc: string) : bool =
    targetFunc = currentFuncName
    || targetFunc.StartsWith($"{currentFuncName}_")

let rec internal isTempUsedAsSelfTailCallArg
    (currentFuncName: string)
    (targetTemp: TempId)
    (expr: ReturnAnnotatedExpr)
    : bool =
    let rec loop (aliases: Set<TempId>) (expr: ReturnAnnotatedExpr) : bool =
        let argsContainAlias (args: Atom list) : bool =
            args
            |> List.exists (function
                | Var tempId -> Set.contains tempId aliases
                | _ -> false)

        match expr with
        | RJump _ -> false
        | RJoin (_, continuation, entry, _) -> loop aliases continuation || loop aliases entry
        | RReturn _ ->
            false
        | RLet (_, Call (targetFunc, args), _, _)
            when isSelfTailCallTarget currentFuncName targetFunc && argsContainAlias args ->
            true
        | RLet (_, TailCall (targetFunc, args), _, _)
            when isSelfTailCallTarget currentFuncName targetFunc && argsContainAlias args ->
            true
        | RLet (aliasTemp, Atom (Var sourceTemp), body, _)
        | RLet (aliasTemp, TypedAtom (Var sourceTemp, _), body, _)
            when Set.contains sourceTemp aliases ->
            loop (Set.add aliasTemp aliases) body
        | RLet (_, _, body, _) ->
            loop aliases body
        | RIf (_, thenBranch, elseBranch, _) ->
            loop aliases thenBranch || loop aliases elseBranch

    loop (Set.singleton targetTemp) expr

let rec private collectMovableTailDecPrefix
    (tailArgTemps: Set<TempId>)
    (expr: AExpr)
    : (TempId * CExpr) list * AExpr =
    match expr with
    | Let (tmpId, RefCountDec (Var tid, size, kind, sourceType), rest) when not (Set.contains tid tailArgTemps) ->
        let (bindings, remaining) = collectMovableTailDecPrefix tailArgTemps rest
        ((tmpId, RefCountDec (Var tid, size, kind, sourceType)) :: bindings, remaining)
    | Let (tmpId, RefCountDecString atom, rest) ->
        let overlaps =
            match atom with
            | Var tid -> Set.contains tid tailArgTemps
            | _ -> false
        if overlaps then
            ([], expr)
        else
            let (bindings, remaining) = collectMovableTailDecPrefix tailArgTemps rest
            ((tmpId, RefCountDecString atom) :: bindings, remaining)
    | Let (tmpId, RefCountDecBlob atom, rest) ->
        let overlaps =
            match atom with
            | Var tid -> Set.contains tid tailArgTemps
            | _ -> false
        if overlaps then
            ([], expr)
        else
            let (bindings, remaining) = collectMovableTailDecPrefix tailArgTemps rest
            ((tmpId, RefCountDecBlob atom) :: bindings, remaining)
    | _ ->
        ([], expr)

let rec internal moveDecsBeforeNonSelfTailCalls (currentFuncName: string) (expr: AExpr) : AExpr =
    match expr with
    | Jump _ -> expr
    | Join (parameter, continuation, entry) ->
        Join (parameter, moveDecsBeforeNonSelfTailCalls currentFuncName continuation, moveDecsBeforeNonSelfTailCalls currentFuncName entry)
    | Return _ ->
        expr
    | If (cond, thenBranch, elseBranch) ->
        If (
            cond,
            moveDecsBeforeNonSelfTailCalls currentFuncName thenBranch,
            moveDecsBeforeNonSelfTailCalls currentFuncName elseBranch
        )
    | Let (tempId, cexpr, body) ->
        let body' = moveDecsBeforeNonSelfTailCalls currentFuncName body
        match cexpr with
        | TailCall (targetFunc, _) when targetFunc <> currentFuncName ->
            let tailArgTemps = tailCallArgTempIds cexpr
            let (movableDecs, remainingBody) = collectMovableTailDecPrefix tailArgTemps body'
            let tailLet = Let (tempId, cexpr, remainingBody)
            wrapBindings movableDecs tailLet
        | _ ->
            Let (tempId, cexpr, body')

let rec internal insertOwnedAccumulatorDecsBeforeSelfTailCalls
    (ctx: TypeContext)
    (currentFuncName: string)
    (ownedParamDecs: ReturnDec list)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let decsForSelfTailCall (args: Atom list) : ReturnDec list =
        let argTemps =
            args
            |> List.fold (fun acc atom ->
                match atom with
                | Var tempId -> Set.add tempId acc
                | _ -> acc) Set.empty

        ownedParamDecs
        |> List.filter (fun (tempId, _, _, _, _) -> not (Set.contains tempId argTemps))

    let wrapOwnedAccumulatorDecs
        (decs: ReturnDec list)
        (tailExpr: AExpr)
        (varGen: VarGen)
        (types: Map<TempId, AST.Type>)
        : AExpr * VarGen * Map<TempId, AST.Type> =
        decs
        |> List.fold
            (fun (accExpr, accVarGen, accTypes) (tempId, typ, shape, kindOverride, metadata) ->
                let (dummyId, varGen') = freshVar accVarGen
                let decExpr =
                    releaseExprForShape tempId typ shape kindOverride metadata
                (Let (dummyId, decExpr, accExpr), varGen', Map.add dummyId AST.TUnit accTypes))
            (tailExpr, varGen, types)

    match expr with
    | Jump _ -> (expr, varGen, types)
    | Join (parameter, continuation, entry) ->
        let body, next, bodyTypes = insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs continuation varGen types
        let entry', final, finalTypes = insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs entry next bodyTypes
        (Join (parameter, body, entry'), final, finalTypes)
    | Return _ ->
        (expr, varGen, types)
    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGen1, types1) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs thenBranch varGen types
        let (elseBranch', varGen2, types2) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs elseBranch varGen1 types1
        (If (cond, thenBranch', elseBranch'), varGen2, types2)
    | Let (tempId, Call (targetFunc, args), body) when isSelfTailCallTarget currentFuncName targetFunc ->
        let (body', varGen1, types1) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs body varGen types
        let callExpr = Let (tempId, Call (targetFunc, args), body')
        wrapOwnedAccumulatorDecs (decsForSelfTailCall args) callExpr varGen1 types1
    | Let (tempId, TailCall (targetFunc, args), body) when isSelfTailCallTarget currentFuncName targetFunc ->
        let (body', varGen1, types1) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs body varGen types
        let tailExpr = Let (tempId, TailCall (targetFunc, args), body')
        wrapOwnedAccumulatorDecs (decsForSelfTailCall args) tailExpr varGen1 types1
    | Let (tempId, cexpr, body) ->
        let (body', varGen1, types1) =
            insertOwnedAccumulatorDecsBeforeSelfTailCalls ctx currentFuncName ownedParamDecs body varGen types
        (Let (tempId, cexpr, body'), varGen1, types1)

let private isClosureMapHelperTarget (targetFunc: string) : bool =
    targetFunc = "Darklang.Stdlib.List.__mapHelper"
    || targetFunc.StartsWith("Darklang.Stdlib.List.__mapHelper_")

/// Find the two rare post-RC cleanups with one allocation-free body scan.
let rec internal requiredFunctionCleanups
    (currentFuncName: string)
    (expr: AExpr)
    : bool * bool =
    match expr with
    | Jump _ | Return _ -> (false, false)
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        let (thenNeedsMapRetain, thenNeedsTailDecMove) =
            requiredFunctionCleanups currentFuncName thenBranch
        let (elseNeedsMapRetain, elseNeedsTailDecMove) =
            requiredFunctionCleanups currentFuncName elseBranch
        (thenNeedsMapRetain || elseNeedsMapRetain,
         thenNeedsTailDecMove || elseNeedsTailDecMove)
    | Let (_, cexpr, body) ->
        let (bodyNeedsMapRetain, bodyNeedsTailDecMove) =
            requiredFunctionCleanups currentFuncName body
        let currentNeedsMapRetain =
            match cexpr with
            | Call (targetFunc, _)
            | TailCall (targetFunc, _) -> isClosureMapHelperTarget targetFunc
            | _ -> false
        let currentNeedsTailDecMove =
            match cexpr with
            | TailCall (targetFunc, _) -> targetFunc <> currentFuncName
            | _ -> false
        (currentNeedsMapRetain || bodyNeedsMapRetain,
         currentNeedsTailDecMove || bodyNeedsTailDecMove)

let rec internal insertClosureMapSourceRetainsBeforeHelperCalls
    (ctx: TypeContext)
    (currentFuncName: string)
    (expr: AExpr)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : AExpr * VarGen * Map<TempId, AST.Type> =
    let currentIsMapHelper =
        currentFuncName = "Darklang.Stdlib.List.__mapHelper"
        || currentFuncName.StartsWith("Darklang.Stdlib.List.__mapHelper_")

    let targetReturnsClosureList (targetFunc: string) : bool =
        match tryGetFuncReturnTypeFromReg ctx targetFunc with
        | Some (AST.TList (AST.TFunction _)) -> true
        | _ -> false

    let wrapSourceRetain
        (targetFunc: string)
        (args: Atom list)
        (callExpr: AExpr)
        (varGen: VarGen)
        (types: Map<TempId, AST.Type>)
        : AExpr * VarGen * Map<TempId, AST.Type> =
        match currentIsMapHelper, targetReturnsClosureList targetFunc, args with
        | false, true, Var sourceTemp :: _ ->
            match tryGetType (withTempTypes ctx types) sourceTemp with
            | Some sourceType ->
                let shape = rcShapeForType ctx sourceType
                if rcShapeNeedsBorrowedRetain shape then
                    let (dummyId, varGen') = freshVar varGen
                    let incExpr =
                        retainExprForShape ctx sourceTemp sourceType shape
                    (Let (dummyId, incExpr, callExpr), varGen', Map.add dummyId AST.TUnit types)
                else
                    (callExpr, varGen, types)
            | _ ->
                (callExpr, varGen, types)
        | _ ->
            (callExpr, varGen, types)

    match expr with
    | Jump _ -> (expr, varGen, types)
    | Join (parameter, continuation, entry) ->
        let body, next, bodyTypes = insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName continuation varGen types
        let entry', final, finalTypes = insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName entry next bodyTypes
        (Join (parameter, body, entry'), final, finalTypes)
    | Return _ ->
        (expr, varGen, types)
    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGen1, types1) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName thenBranch varGen types
        let (elseBranch', varGen2, types2) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName elseBranch varGen1 types1
        (If (cond, thenBranch', elseBranch'), varGen2, types2)
    | Let (tempId, Call (targetFunc, args), body) when isClosureMapHelperTarget targetFunc ->
        let (body', varGen1, types1) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName body varGen types
        let callExpr = Let (tempId, Call (targetFunc, args), body')
        wrapSourceRetain targetFunc args callExpr varGen1 types1
    | Let (tempId, TailCall (targetFunc, args), body) when isClosureMapHelperTarget targetFunc ->
        let (body', varGen1, types1) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName body varGen types
        let callExpr = Let (tempId, TailCall (targetFunc, args), body')
        wrapSourceRetain targetFunc args callExpr varGen1 types1
    | Let (tempId, cexpr, body) ->
        let (body', varGen1, types1) =
            insertClosureMapSourceRetainsBeforeHelperCalls ctx currentFuncName body varGen types
        (Let (tempId, cexpr, body'), varGen1, types1)

/// Insert reference counting operations using return analysis and a dec stack
/// Returns (transformed expr, varGen, types defined in this subtree)
