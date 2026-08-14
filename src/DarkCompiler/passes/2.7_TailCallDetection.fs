// 2.7_TailCallDetection.fs - Tail Call Detection Pass
//
// Detects tail calls in ANF and transforms them to tail call variants:
// - Call → TailCall
// - IndirectCall → IndirectTailCall
// - ClosureCall → ClosureTailCall
//
// A call is in tail position if:
// - It's in a Let binding where the body eventually returns the same variable
// - Both branches of an If are in tail position if the If itself is
//
// This runs AFTER RefCountInsertion, so RefCountDec operations may be inserted
// between the call and the return. We look through any RefCountDec operations
// to find the final Return. This is crucial because without TCO, functions like
// __reverseHelper would use regular calls instead of tail calls, causing the
// intermediate cons cells to be freed prematurely (leading to corrupted results
// when the free list reuses those cells for subsequent allocations).
//
// CURRENT STATUS: TCO is ENABLED. The DCE bug that caused 197 test failures
// has been fixed (DeadCodeElimination.fs was not recognizing TailCall as a
// function call, causing stdlib functions called via tail call to be removed).
//
// See docs/features/tail-call-optimization.md for detailed documentation.

module TailCallDetection

open ANF

/// Check if a CExpr is a RefCountDec operation
let isRefCountDec (cexpr: CExpr) : bool =
    match cexpr with
    | RefCountDec _ -> true
    | RefCountDecString _ -> true
    | RefCountDecBlob _ -> true
    | _ -> false

/// Check if an expression eventually returns a specific TempId
/// Looks through any RefCountDec operations to find the final Return
let rec isReturnOf (tempId: TempId) (expr: AExpr) : bool =
    match expr with
    | Return (Var tid) when tid = tempId -> true
    | Let (_, cexpr, body) when isRefCountDec cexpr ->
        // RefCountDec followed by more expressions - look through it
        isReturnOf tempId body
    | _ -> false

/// Transform a Call to TailCall if it's in tail position
let convertToTailCall (cexpr: CExpr) : CExpr =
    match cexpr with
    | Call (funcName, args) -> TailCall (funcName, args)
    | BorrowedCall (funcName, args) -> TailCall (funcName, args)
    | IndirectCall (func, args) -> IndirectTailCall (func, args)
    | ClosureCall (closure, args) -> ClosureTailCall (closure, args)
    | _ -> cexpr

let private wrapBindings (bindings: (TempId * CExpr) list) (body: AExpr) : AExpr =
    List.foldBack (fun (tempId, cexpr) acc -> Let (tempId, cexpr, acc)) bindings body

let private canonicalTempId (aliasRoots: Map<TempId, TempId>) (tempId: TempId) : TempId =
    // extendAliasRoots canonicalizes the source before insertion, so every map
    // value is already a root rather than another link in an alias chain.
    match Map.tryFind tempId aliasRoots with
    | Some root -> root
    | None -> tempId

let private extendAliasRoots
    (aliasRoots: Map<TempId, TempId>)
    (tempId: TempId)
    (cexpr: CExpr)
    : Map<TempId, TempId> =
    let sourceAlias =
        match cexpr with
        | Atom (Var tid) -> Some tid
        | TypedAtom (Var tid, _) -> Some tid
        | _ -> None

    match sourceAlias with
    | Some sourceTid ->
        Map.add tempId (canonicalTempId aliasRoots sourceTid) aliasRoots
    | None ->
        aliasRoots

let private tailCallArgTempIds
    (aliasRoots: Map<TempId, TempId>)
    (cexpr: CExpr)
    : Set<TempId> =
    let addAtom (temps: Set<TempId>) (atom: Atom) : Set<TempId> =
        match atom with
        | Var tid -> Set.add (canonicalTempId aliasRoots tid) temps
        | _ -> temps
    match cexpr with
    | TailCall (_, args) ->
        args |> List.fold addAtom Set.empty
    | IndirectTailCall (func, args) ->
        args |> List.fold addAtom (addAtom Set.empty func)
    | ClosureTailCall (closure, args) ->
        args |> List.fold addAtom (addAtom Set.empty closure)
    | _ ->
        Set.empty

let private atomOverlapsTailArgs
    (aliasRoots: Map<TempId, TempId>)
    (tailArgTemps: Set<TempId>)
    (atom: Atom)
    : bool =
    match atom with
    | Var tid -> Set.contains (canonicalTempId aliasRoots tid) tailArgTemps
    | _ -> false

let rec private collectMovableDecPrefix
    (aliasRoots: Map<TempId, TempId>)
    (tailArgTemps: Set<TempId>)
    (expr: AExpr)
    : (TempId * CExpr) list * AExpr =
    match expr with
    | Let (tmpId, RefCountDec (Var tid, size, kind, sourceType), rest)
        when not (Set.contains (canonicalTempId aliasRoots tid) tailArgTemps) ->
        let (bindings, remaining) = collectMovableDecPrefix aliasRoots tailArgTemps rest
        ((tmpId, RefCountDec (Var tid, size, kind, sourceType)) :: bindings, remaining)
    | Let (tmpId, RefCountDecString atom, rest)
        when not (atomOverlapsTailArgs aliasRoots tailArgTemps atom) ->
        let (bindings, remaining) = collectMovableDecPrefix aliasRoots tailArgTemps rest
        ((tmpId, RefCountDecString atom) :: bindings, remaining)
    | Let (_, RefCountDecString _, _) ->
        ([], expr)
    | Let (tmpId, RefCountDecBlob atom, rest)
        when not (atomOverlapsTailArgs aliasRoots tailArgTemps atom) ->
        let (bindings, remaining) = collectMovableDecPrefix aliasRoots tailArgTemps rest
        ((tmpId, RefCountDecBlob atom) :: bindings, remaining)
    | Let (_, RefCountDecBlob _, _) ->
        ([], expr)
    | _ ->
        ([], expr)

let private isDirectReturnOf (tempId: TempId) (expr: AExpr) : bool =
    match expr with
    | Return (Var tid) when tid = tempId -> true
    | _ -> false

let rec private leadingRetainedParams
    (paramIds: Set<TempId>)
    (expr: AExpr)
    : Set<TempId> =
    match expr with
    | Let (_, RefCountInc (Var tempId, _, _, _), body) when Set.contains tempId paramIds ->
        Set.add tempId (leadingRetainedParams paramIds body)
    | Let (_, RefCountIncString (Var tempId), body) when Set.contains tempId paramIds ->
        Set.add tempId (leadingRetainedParams paramIds body)
    | Let (_, RefCountIncBlob (Var tempId), body) when Set.contains tempId paramIds ->
        Set.add tempId (leadingRetainedParams paramIds body)
    | _ ->
        Set.empty

/// Transfer the owned replacement record into the next loop iteration. RC
/// insertion marks this shape by retaining the initial parameter and releasing
/// the previous parameter before the call. The post-call release belongs to the
/// ordinary recursive frame; a loop instead adopts that argument's ownership.
let private tryTransferOwnedSelfTailArgument
    (aliasRoots: Map<TempId, TempId>)
    (ownedParams: Set<TempId>)
    (releasedTemps: Set<TempId>)
    (typedParams: TypedParam list)
    (callTempId: TempId)
    (tailCall: CExpr)
    (remainingBody: AExpr)
    : AExpr option =
    match tailCall, remainingBody with
    | TailCall (_, args), Let (_, RefCountDec (Var cleanupTemp, _, _, _), Return (Var returnTemp))
        when returnTemp = callTempId ->
        let cleanupRoot = canonicalTempId aliasRoots cleanupTemp
        let matchingOwnedParams =
            List.zip typedParams args
            |> List.choose (fun (param, arg) ->
                match arg with
                | Var argTemp
                    when canonicalTempId aliasRoots argTemp = cleanupRoot
                         && Set.contains param.Id ownedParams
                         && Set.contains (canonicalTempId aliasRoots param.Id) releasedTemps ->
                    Some param.Id
                | _ ->
                    None)
        match matchingOwnedParams with
        | [_] -> Some (Return (Var callTempId))
        | _ -> None
    | _ ->
        None

/// Check if a CExpr is a call (direct, indirect, or closure)
let isCallExpr (cexpr: CExpr) : bool =
    match cexpr with
    | Call _ | BorrowedCall _ | IndirectCall _ | ClosureCall _ -> true
    | _ -> false

/// Detect and transform tail calls in an expression.
/// The 'inTailPosition' parameter indicates if the current expression
/// is in tail position (its result is directly returned).
let rec detectTailCalls
    (currentFuncName: string)
    (typedParams: TypedParam list)
    (ownedParams: Set<TempId>)
    (releasedTemps: Set<TempId>)
    (inTailPosition: bool)
    (aliasRoots: Map<TempId, TempId>)
    (expr: AExpr)
    : AExpr =
    match expr with
    | Return atom ->
        // Return is always a base case - just return it
        Return atom

    | Let (tempId, cexpr, body) ->
        // Check if this is a tail call pattern:
        // Let (t, Call(...), Return (Var t))
        if inTailPosition && isCallExpr cexpr && isReturnOf tempId body then
            // This is a tail call! Convert the call to tail call variant
            let tailCall = convertToTailCall cexpr
            let tailArgTemps = tailCallArgTempIds aliasRoots tailCall
            let (movableDecs, remainingBody) = collectMovableDecPrefix aliasRoots tailArgTemps body
            let transferredBody =
                match tailCall with
                | TailCall (targetFunc, _) when targetFunc = currentFuncName ->
                    tryTransferOwnedSelfTailArgument
                        aliasRoots
                        ownedParams
                        releasedTemps
                        typedParams
                        tempId
                        tailCall
                        remainingBody
                | _ ->
                    None
            match transferredBody with
            | Some bodyAfterTransfer ->
                wrapBindings movableDecs (Let (tempId, tailCall, bodyAfterTransfer))
            | None when isDirectReturnOf tempId remainingBody ->
                wrapBindings movableDecs (Let (tempId, tailCall, remainingBody))
            | None ->
                // Cleanup remains after the call (typically overlap with a tail argument),
                // so keep a normal call to preserve the post-call unwind work.
                let aliasRoots' = extendAliasRoots aliasRoots tempId cexpr
                let body' =
                    detectTailCalls
                        currentFuncName typedParams ownedParams releasedTemps inTailPosition aliasRoots' body
                Let (tempId, cexpr, body')
        else
            // Not a tail call - recurse into body
            // Body is in tail position if current expression is
            let aliasRoots' = extendAliasRoots aliasRoots tempId cexpr
            let releasedTemps' =
                match cexpr with
                | RefCountDec (Var releasedTemp, _, _, _)
                | RefCountDecString (Var releasedTemp)
                | RefCountDecBlob (Var releasedTemp) ->
                    Set.add (canonicalTempId aliasRoots releasedTemp) releasedTemps
                | _ ->
                    releasedTemps
            let body' =
                detectTailCalls
                    currentFuncName typedParams ownedParams releasedTemps' inTailPosition aliasRoots' body
            Let (tempId, cexpr, body')

    | If (cond, thenBranch, elseBranch) ->
        // If expression: both branches are in tail position if If is
        let thenBranch' =
            detectTailCalls currentFuncName typedParams ownedParams releasedTemps inTailPosition aliasRoots thenBranch
        let elseBranch' =
            detectTailCalls currentFuncName typedParams ownedParams releasedTemps inTailPosition aliasRoots elseBranch
        If (cond, thenBranch', elseBranch')

/// Detect tail calls in a function
let detectTailCallsInFunction (func: Function) : Function =
    // Function body is always in tail position
    let paramIds = func.TypedParams |> List.map (fun param -> param.Id) |> Set.ofList
    let ownedParams = leadingRetainedParams paramIds func.Body
    let body' = detectTailCalls func.Name func.TypedParams ownedParams Set.empty true Map.empty func.Body
    { func with Body = body' }

/// Detect tail calls in a program
let detectTailCallsInProgram (program: ANF.Program) : ANF.Program =
    // TCO is ENABLED - the DCE bug that caused 197 test failures has been fixed
    // (DeadCodeElimination.fs was not recognizing TailCall as a function call)
    let (ANF.Program (functions, main)) = program
    ANF.Program (functions |> List.map detectTailCallsInFunction, main)
