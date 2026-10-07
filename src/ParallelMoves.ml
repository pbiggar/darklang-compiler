(*
   This module implements the parallel move resolution algorithm used for:
   - TailArgMoves in code generation
   - Phi resolution in SSA-based register allocation
   The algorithm correctly sequences parallel moves to avoid clobbering values.
   It handles:
   - Simple moves (no conflict)
   - Chain moves (must reorder)
   - Cycles (need temp register)
   - Self-moves (eliminated as no-ops)
*)
(* ParallelMoves.ml - Parallel move resolution algorithm. *)
[@@@warning "-4"]
(*
   Result of parallel move resolution - actions to perform in order
   Save register to temp (before cycle)
   Regular move
   Move from temp to dest (end of cycle)
*)
type ('reg, 'src) moveAction = SaveToTemp of 'reg | Move of 'reg * 'src | MoveFromTemp of 'reg
(*
   Resolve parallel moves into a sequence of actions
   Parameters:
   - moves: List of (dest, src) pairs representing parallel moves
   - getSrcReg: Function to extract source register from src if it's a register (None for immediates, stack slots, etc.)
   Returns: List of actions to perform in order to correctly implement the parallel moves
   Filter out self-loops (X1 <- X1) since they're no-ops
   Phase 1: Emit non-register-source moves whose destination is NOT used as
   a source. A move like X0 <- Imm cannot be emitted early if X0 is a source
   for another move.
   Phase 2: Iteratively emit moves where dest is not a source for remaining moves.
   Phase 3: Handle cycles using temp register. At this point, all remaining
   moves form cycles. For each cycle:
   1. Save the FIRST destination to temp (it gets clobbered first but read later)
   2. Emit all moves in DEPENDENCY ORDER (so we read from registers before they're overwritten)
   3. Any move that reads the saved register uses temp instead
   Example cycle: X0 <- X1, X1 <- X2, X2 <- X0
   1. Save X0 to temp (X0 is written first but X2 <- X0 reads it later)
   2. Emit in order: X0 <- X1, X1 <- X2, X2 <- temp
*)
let resolve moves getSrcReg =
 let nonSelfLoops = List.filter (fun (dest, source) -> match getSrcReg source with Some reg -> reg <> dest | None -> true) moves in
 let prependActions reversed actions = List.fold_left (fun reversed action -> action :: reversed) reversed actions in
 let prependMoves reversed moves = List.fold_left (fun reversed (dest, source) -> Move (dest, source) :: reversed) reversed moves in
 let sources moves = List.filter_map (fun (_, source) -> getSrcReg source) moves in
 let allSources = sources nonSelfLoops in
 let nonRegMoves, regMoves = List.partition (fun (_, source) -> getSrcReg source = None) nonSelfLoops in
 let safeNonRegMoves, unsafeNonRegMoves = List.partition (fun (dest, _) -> not (List.mem dest allSources)) nonRegMoves in
 let rec collectSafe remaining reversed =
  let sources = sources remaining in
  let safe, unsafe = List.partition (fun (dest, _) -> not (List.mem dest sources)) remaining in
  match safe with [] -> reversed, unsafe | _ -> collectSafe unsafe (prependMoves reversed safe) in
 let rec collectCycles remaining reversed = match remaining with
  | [] -> reversed
  | (saved, _) :: _ ->
   let rec chain current remaining reversed = match List.find_opt (fun (dest, _) -> dest = current) remaining with
    | None -> reversed
    | Some ((_, source) as move) -> let remaining = List.filter ((<>) move) remaining in let reversed = move :: reversed in
      (match getSrcReg source with Some reg when reg <> saved -> chain reg remaining reversed | Some _ | None -> reversed) in
   let ordered = List.rev (chain saved remaining []) in
   let actions = List.map (fun (dest, source) -> match getSrcReg source with Some reg when reg = saved -> MoveFromTemp dest | Some _ | None -> Move (dest, source)) ordered in
   let remaining = List.filter (fun move -> not (List.mem move ordered)) remaining in
   collectCycles remaining (prependActions reversed (SaveToTemp saved :: actions)) in
 let reversed = prependMoves [] safeNonRegMoves in
 let reversed, remaining = collectSafe (unsafeNonRegMoves @ regMoves) reversed in
 List.rev (collectCycles remaining reversed)
