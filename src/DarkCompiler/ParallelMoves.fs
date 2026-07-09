// ParallelMoves.fs - Parallel move resolution algorithm
//
// This module implements the parallel move resolution algorithm used for:
// - TailArgMoves in code generation
// - Phi resolution in SSA-based register allocation
//
// The algorithm correctly sequences parallel moves to avoid clobbering values.
// It handles:
// - Simple moves (no conflict)
// - Chain moves (must reorder)
// - Cycles (need temp register)
// - Self-moves (eliminated as no-ops)

module ParallelMoves

/// Result of parallel move resolution - actions to perform in order
type MoveAction<'Reg, 'Src> =
    | SaveToTemp of 'Reg           // Save register to temp (before cycle)
    | Move of dest:'Reg * src:'Src // Regular move
    | MoveFromTemp of 'Reg         // Move from temp to dest (end of cycle)

/// Resolve parallel moves into a sequence of actions
///
/// Parameters:
/// - moves: List of (dest, src) pairs representing parallel moves
/// - getSrcReg: Function to extract source register from src if it's a register (None for immediates, stack slots, etc.)
///
/// Returns: List of actions to perform in order to correctly implement the parallel moves
let resolve<'Reg, 'Src when 'Reg : equality and 'Reg : comparison and 'Src : equality>
    (moves: ('Reg * 'Src) list)
    (getSrcReg: 'Src -> 'Reg option)
    : MoveAction<'Reg, 'Src> list =

    // Filter out self-loops (X1 <- X1) since they're no-ops
    let isSelfLoop (destReg: 'Reg, srcOp: 'Src) =
        match getSrcReg srcOp with
        | Some srcReg -> srcReg = destReg
        | None -> false

    let nonSelfLoops = moves |> List.filter (not << isSelfLoop)

    let prependActions
        (actionsRev: MoveAction<'Reg, 'Src> list)
        (actions: MoveAction<'Reg, 'Src> list)
        : MoveAction<'Reg, 'Src> list =
        actions |> List.fold (fun acc action -> action :: acc) actionsRev

    let prependMoves
        (actionsRev: MoveAction<'Reg, 'Src> list)
        (moves': ('Reg * 'Src) list)
        : MoveAction<'Reg, 'Src> list =
        moves'
        |> List.fold (fun acc (dest, src) -> Move (dest, src) :: acc) actionsRev

    // Phase 1: Emit non-register-source moves whose destination is NOT used as
    // a source. A move like X0 <- Imm cannot be emitted early if X0 is a source
    // for another move.
    let allRegSrcRegs =
        nonSelfLoops
        |> List.choose (fun (_, srcOp) -> getSrcReg srcOp)
        |> Set.ofList

    let (nonRegMoves, regMoves) =
        nonSelfLoops |> List.partition (fun (_, srcOp) -> getSrcReg srcOp = None)

    let (safeNonRegMoves, unsafeNonRegMoves) =
        nonRegMoves |> List.partition (fun (dest, _) -> not (Set.contains dest allRegSrcRegs))

    // Phase 2: Iteratively emit moves where dest is not a source for remaining moves.
    let rec collectSafeMoves
        (remaining: ('Reg * 'Src) list)
        (actionsRev: MoveAction<'Reg, 'Src> list)
        : MoveAction<'Reg, 'Src> list * ('Reg * 'Src) list =
        let remainingSrcs =
            remaining
            |> List.choose (fun (_, srcOp) -> getSrcReg srcOp)
            |> Set.ofList

        let (safe, unsafe) =
            remaining |> List.partition (fun (destReg, _) -> not (Set.contains destReg remainingSrcs))

        match safe with
        | [] -> (actionsRev, unsafe)
        | _ -> collectSafeMoves unsafe (prependMoves actionsRev safe)

    // Phase 3: Handle cycles using temp register. At this point, all remaining
    // moves form cycles. For each cycle:
    // 1. Save the FIRST destination to temp (it gets clobbered first but read later)
    // 2. Emit all moves in DEPENDENCY ORDER (so we read from registers before they're overwritten)
    // 3. Any move that reads the saved register uses temp instead
    //
    // Example cycle: X0 <- X1, X1 <- X2, X2 <- X0
    // 1. Save X0 to temp (X0 is written first but X2 <- X0 reads it later)
    // 2. Emit in order: X0 <- X1, X1 <- X2, X2 <- temp
    let rec collectCycleMoves
        (remaining: ('Reg * 'Src) list)
        (actionsRev: MoveAction<'Reg, 'Src> list)
        : MoveAction<'Reg, 'Src> list =
        match remaining with
        | [] -> actionsRev
        | (firstDest, _) :: _ ->
            let savedReg = firstDest

            let rec buildOrderedChain
                (currentDest: 'Reg)
                (movesLeft: ('Reg * 'Src) list)
                (chain: ('Reg * 'Src) list)
                : ('Reg * 'Src) list =
                match movesLeft |> List.tryFind (fun (dest, _) -> dest = currentDest) with
                | Some ((_, src) as move) ->
                    let movesLeft' = movesLeft |> List.filter (fun candidate -> candidate <> move)
                    let chain' = move :: chain

                    match getSrcReg src with
                    | Some srcReg when srcReg <> savedReg -> buildOrderedChain srcReg movesLeft' chain'
                    | Some _
                    | None -> chain'
                | None -> chain

            let orderedMoves = buildOrderedChain savedReg remaining [] |> List.rev

            let cycleActions =
                orderedMoves
                |> List.map (fun (dest, src) ->
                    match getSrcReg src with
                    | Some srcReg when srcReg = savedReg -> MoveFromTemp dest
                    | Some _
                    | None -> Move (dest, src))

            let remaining' =
                remaining |> List.filter (fun move -> not (List.contains move orderedMoves))

            collectCycleMoves remaining' (prependActions actionsRev (SaveToTemp savedReg :: cycleActions))

    let phase1ActionsRev = prependMoves [] safeNonRegMoves
    let phase1Remaining = unsafeNonRegMoves @ regMoves
    let (phase2ActionsRev, phase2Remaining) = collectSafeMoves phase1Remaining phase1ActionsRev

    collectCycleMoves phase2Remaining phase2ActionsRev |> List.rev
