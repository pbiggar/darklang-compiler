(* RegisterLiveness.ml - Solve CFG liveness and prepare caller-save requirements. *)
[@@@warning "-4"]

open AllocationModel
open RegisterFacts
module IntSet = Set.Make (Int)

(*
   Get successor labels for a terminator
*)
let getSuccessors = function
  | LIR.Ret -> []
  | LIR.Branch (_, yes, no)
  | LIR.BranchZero (_, yes, no)
  | LIR.BranchBitZero (_, _, yes, no)
  | LIR.BranchBitNonZero (_, _, yes, no)
  | LIR.CondBranch (_, yes, no) ->
      [ yes; no ]
  | LIR.Jump label -> [ label ]

let addPhiUse domain predIdx vregId uses =
  let rec insert = function
    | [] ->
        let bits = Bitset.empty domain.wordCount in
        vregBitsAddInPlace domain vregId bits;
        [ (predIdx, bits) ]
    | (idx, bits) :: rest as remaining ->
        if idx = predIdx then (
          vregBitsAddInPlace domain vregId bits;
          remaining)
        else (idx, bits) :: insert rest
  in
  insert uses

let collectPhiUsesByPred domain blockIndex classifiedBlocks =
  let result = Array.init (Array.length classifiedBlocks) (fun _ -> []) in
  Array.iteri
    (fun blockIdx blockFacts ->
      let uses = ref [] in
      Array.iter
        (fun facts ->
          List.iter
            (fun (id, predLabel) ->
              match tryBlockIndex blockIndex predLabel with
              | Some predIdx -> uses := addPhiUse domain predIdx id !uses
              | None -> ())
            facts.intPhiUses)
        blockFacts.instrFacts;
      result.(blockIdx) <- !uses)
    classifiedBlocks;
  result

let collectFPhiUsesByPred domain blockIndex classifiedBlocks =
  let result = Array.init (Array.length classifiedBlocks) (fun _ -> []) in
  Array.iteri
    (fun blockIdx blockFacts ->
      let uses = ref [] in
      Array.iter
        (fun facts ->
          List.iter
            (fun (id, predLabel) ->
              match tryBlockIndex blockIndex predLabel with
              | Some predIdx -> uses := addPhiUse domain predIdx id !uses
              | None -> ())
            facts.floatPhiUses)
        blockFacts.instrFacts;
      result.(blockIdx) <- !uses)
    classifiedBlocks;
  result

(*
   Compute GEN and KILL sets for a basic block
   GEN = variables used before being defined
   KILL = variables defined
   Process instructions in forward order
   Add to GEN if used and not already killed (defined earlier in block)
   Add to KILL if defined
   Also add terminator uses to GEN
*)
let computeGenKillFromFacts domain blockFacts =
  let gen = Bitset.empty domain.wordCount in
  let kill = Bitset.empty domain.wordCount in
  Array.iter
    (fun facts ->
      List.iter
        (fun u ->
          if not (vregBitsContains domain kill u) then
            vregBitsAddInPlace domain u gen)
        facts.intUses;
      match facts.intDef with
      | Some d -> vregBitsAddInPlace domain d kill
      | None -> ())
    blockFacts.instrFacts;
  List.iter
    (fun u ->
      if not (vregBitsContains domain kill u) then
        vregBitsAddInPlace domain u gen)
    blockFacts.terminatorUses;
  (gen, kill)

let computeGenKill domain block =
  computeGenKillFromFacts domain (classifyBlocks [| block |]).(0)

(*
   Compute liveness using backward dataflow analysis
   Handles SSA phi nodes: phi sources are live at predecessor exits, not at phi's block entry
   Collect all integer VReg IDs referenced in the CFG (uses, defs, phi sources/dests, terminators)
*)
let collectVRegIdsFromFacts classifiedBlocks =
  Array.fold_left
    (fun acc blockFacts ->
      let acc =
        List.fold_left (fun acc id -> id :: acc) acc blockFacts.terminatorUses
      in
      Array.fold_left
        (fun acc facts ->
          let acc =
            List.fold_left (fun acc id -> id :: acc) acc facts.intUses
          in
          let acc =
            List.fold_left (fun acc (id, _) -> id :: acc) acc facts.intPhiUses
          in
          match facts.intDef with Some id -> id :: acc | None -> acc)
        acc blockFacts.instrFacts)
    [] classifiedBlocks

(*
   Compute float GEN and KILL sets for a basic block
   GEN = float variables used before being defined
   KILL = float variables defined
*)
let computeFloatGenKillFromFacts domain blockFacts =
  let gen = Bitset.empty domain.wordCount in
  let kill = Bitset.empty domain.wordCount in
  Array.iter
    (fun facts ->
      List.iter
        (fun u ->
          if not (vregBitsContains domain kill u) then
            vregBitsAddInPlace domain u gen)
        facts.floatUses;
      match facts.floatDef with
      | Some d -> vregBitsAddInPlace domain d kill
      | None -> ())
    blockFacts.instrFacts;
  (gen, kill)

let computeFloatGenKill domain block =
  computeFloatGenKillFromFacts domain (classifyBlocks [| block |]).(0)

(*
   Compute float liveness using backward dataflow analysis
   Handles SSA FPhi nodes: phi sources are live at predecessor exits, not at phi's block entry
   Collect all float VReg IDs referenced in the CFG (uses, defs, FPhi sources/dests)
*)
let collectFVRegIdsFromFacts classifiedBlocks =
  Array.fold_left
    (fun acc blockFacts ->
      Array.fold_left
        (fun acc facts ->
          let acc =
            List.fold_left (fun acc id -> id :: acc) acc facts.floatUses
          in
          let acc =
            List.fold_left (fun acc (id, _) -> id :: acc) acc facts.floatPhiUses
          in
          match facts.floatDef with Some id -> id :: acc | None -> acc)
        acc blockFacts.instrFacts)
    [] classifiedBlocks

(*
   Compute integer and float liveness in one backward CFG fixed point.
   The two domains remain distinct, so allocation receives the exact same live
   sets as independent solvers, while CFG successors and edge lookups are shared.
   Liveness flows from successors to predecessors. Visiting a CFG in
   postorder therefore settles acyclic regions in one sweep; the fixed
   point below only has to revisit loop backedges. Successor and predecessor
   indices are retained so the solver only revisits blocks affected by a
   changed successor instead of rescanning the entire CFG.
*)
let computeCombinedLivenessBitsFromFacts blockIndex classifiedBlocks intExtraIds
    floatExtraIds =
  let intDomain =
    buildVRegDomain (collectVRegIdsFromFacts classifiedBlocks @ intExtraIds)
  in
  let floatDomain =
    buildVRegDomain (collectFVRegIdsFromFacts classifiedBlocks @ floatExtraIds)
  in
  let emptyIntBits = Bitset.empty intDomain.wordCount in
  let emptyFloatBits = Bitset.empty floatDomain.wordCount in
  let n = Array.length classifiedBlocks in
  let intGenKillBits =
    Array.init n (fun idx ->
        computeGenKillFromFacts intDomain classifiedBlocks.(idx))
  in
  let floatGenKillBits =
    Array.init n (fun idx ->
        computeFloatGenKillFromFacts floatDomain classifiedBlocks.(idx))
  in
  let intPhiUsesBits =
    collectPhiUsesByPred intDomain blockIndex classifiedBlocks
  in
  let floatPhiUsesBits =
    collectFPhiUsesByPred floatDomain blockIndex classifiedBlocks
  in
  let intLiveness =
    Array.init n (fun _ -> { liveIn = emptyIntBits; liveOut = emptyIntBits })
  in
  let floatLiveness =
    Array.init n (fun _ ->
        { liveIn = emptyFloatBits; liveOut = emptyFloatBits })
  in
  let successorIndices =
    Array.init n (fun idx ->
        getSuccessors classifiedBlocks.(idx).block.LIR.terminator
        |> List.filter_map (tryBlockIndex blockIndex)
        |> Array.of_list)
  in
  let predecessorIndicesRev = Array.init n (fun _ -> ref []) in
  Array.iteri
    (fun predIdx successors ->
      Array.iter
        (fun succIdx ->
          predecessorIndicesRev.(succIdx)
          := predIdx :: !(predecessorIndicesRev.(succIdx)))
        successors)
    successorIndices;
  let predecessorIndices =
    Array.map (fun indices -> List.rev !indices) predecessorIndicesRev
  in
  let backwardDataflowOrder =
    let roots = blockIndex.entryIndex :: List.init n Fun.id in
    let rec visit work visited postorderRev =
      match work with
      | [] -> List.rev postorderRev
      | (blockIdx, expanded) :: remaining ->
          if expanded then visit remaining visited (blockIdx :: postorderRev)
          else if IntSet.mem blockIdx visited then
            visit remaining visited postorderRev
          else
            let successors =
              Array.to_list successorIndices.(blockIdx)
              |> List.map (fun idx -> (idx, false))
            in
            visit
              (successors @ ((blockIdx, true) :: remaining))
              (IntSet.add blockIdx visited)
              postorderRev
    in
    visit (List.map (fun idx -> (idx, false)) roots) IntSet.empty []
  in
  let phiUsesForEdge phiUses emptyBits succIdx predIdx =
    match List.find_opt (fun (idx, _) -> idx = predIdx) phiUses.(succIdx) with
    | Some (_, bits) -> bits
    | None -> emptyBits
  in
  let work = Queue.create () in
  let queued = Array.make n false in
  List.iter
    (fun idx ->
      Queue.add idx work;
      queued.(idx) <- true)
    backwardDataflowOrder;
  while not (Queue.is_empty work) do
    let blockIdx = Queue.take work in
    queued.(blockIdx) <- false;
    let intLiveOutAccumulator = ref NoUnionBits in
    let floatLiveOutAccumulator = ref NoUnionBits in
    Array.iter
      (fun succIdx ->
        intLiveOutAccumulator :=
          bitsetAccumulateUnion !intLiveOutAccumulator
            intLiveness.(succIdx).liveIn;
        intLiveOutAccumulator :=
          bitsetAccumulateUnion !intLiveOutAccumulator
            (phiUsesForEdge intPhiUsesBits emptyIntBits succIdx blockIdx);
        floatLiveOutAccumulator :=
          bitsetAccumulateUnion !floatLiveOutAccumulator
            floatLiveness.(succIdx).liveIn;
        floatLiveOutAccumulator :=
          bitsetAccumulateUnion !floatLiveOutAccumulator
            (phiUsesForEdge floatPhiUsesBits emptyFloatBits succIdx blockIdx))
      successorIndices.(blockIdx);
    let intGen, intKill = intGenKillBits.(blockIdx) in
    let oldIntLiveness = intLiveness.(blockIdx) in
    let newIntLiveOut = bitsetFinishUnion emptyIntBits !intLiveOutAccumulator in
    let newIntLiveIn = Bitset.clone newIntLiveOut in
    Bitset.diffInPlace newIntLiveIn intKill;
    Bitset.unionInPlace newIntLiveIn intGen;
    let intLiveInChanged =
      not (Bitset.equal newIntLiveIn oldIntLiveness.liveIn)
    in
    if
      intLiveInChanged
      || not (Bitset.equal newIntLiveOut oldIntLiveness.liveOut)
    then
      intLiveness.(blockIdx) <-
        { liveIn = newIntLiveIn; liveOut = newIntLiveOut };
    let floatGen, floatKill = floatGenKillBits.(blockIdx) in
    let oldFloatLiveness = floatLiveness.(blockIdx) in
    let newFloatLiveOut =
      bitsetFinishUnion emptyFloatBits !floatLiveOutAccumulator
    in
    let newFloatLiveIn = Bitset.clone newFloatLiveOut in
    Bitset.diffInPlace newFloatLiveIn floatKill;
    Bitset.unionInPlace newFloatLiveIn floatGen;
    let floatLiveInChanged =
      not (Bitset.equal newFloatLiveIn oldFloatLiveness.liveIn)
    in
    if
      floatLiveInChanged
      || not (Bitset.equal newFloatLiveOut oldFloatLiveness.liveOut)
    then
      floatLiveness.(blockIdx) <-
        { liveIn = newFloatLiveIn; liveOut = newFloatLiveOut };
    if intLiveInChanged || floatLiveInChanged then
      List.iter
        (fun predIdx ->
          if not queued.(predIdx) then (
            Queue.add predIdx work;
            queued.(predIdx) <- true))
        predecessorIndices.(blockIdx)
  done;
  (intDomain, intLiveness, floatDomain, floatLiveness)

let computeLivenessBitsFromFacts blockIndex classifiedBlocks extraIds =
  let domain, liveness, _, _ =
    computeCombinedLivenessBitsFromFacts blockIndex classifiedBlocks extraIds []
  in
  (domain, liveness)

let computeFloatLivenessBitsFromFacts blockIndex classifiedBlocks extraIds =
  let _, _, domain, liveness =
    computeCombinedLivenessBitsFromFacts blockIndex classifiedBlocks [] extraIds
  in
  (domain, liveness)

let computeLivenessBitsRaw blockIndex blocks extraIds =
  computeLivenessBitsFromFacts blockIndex (classifyBlocks blocks) extraIds

(*
   Compute liveness using bitsets for the dataflow fixed point.
*)
let computeLivenessBits cfg =
  let blockIndex, blocks = buildBlockIndex cfg in
  let domain, liveness = computeLivenessBitsRaw blockIndex blocks [] in
  (domain, blockIndex, liveness)

let computeFloatLivenessBitsRaw blockIndex blocks extraIds =
  computeFloatLivenessBitsFromFacts blockIndex (classifyBlocks blocks) extraIds

(*
   Compute float liveness using bitsets for the dataflow fixed point
*)
let computeFloatLivenessBits cfg =
  let blockIndex, blocks = buildBlockIndex cfg in
  let domain, liveness = computeFloatLivenessBitsRaw blockIndex blocks [] in
  (domain, blockIndex, liveness)

(*
   Capture continuation liveness for each SaveRegs/RestoreRegs pair.
*)
let computeSaveRegsPreparation intDomain floatDomain (block : LIR.basicBlock)
    instrFacts intLiveOut floatLiveOut =
  let intLive = Bitset.clone intLiveOut in
  let floatLive = Bitset.clone floatLiveOut in
  List.iter
    (fun id -> vregBitsAddInPlace intDomain id intLive)
    (getTerminatorUsedVRegs block.LIR.terminator);
  let rec walkBackwards instrIdx pendingRestores snapshots =
    if instrIdx < 0 then
      if pendingRestores = [] then snapshots
      else
        Crash.crash "Unmatched RestoreRegs while computing caller-save liveness"
    else
      let facts = instrFacts.(instrIdx) in
      let pendingRestores, snapshots =
        match facts.instr with
        | LIR.RestoreRegs ([], []) ->
            ( (Bitset.clone intLive, Bitset.clone floatLive) :: pendingRestores,
              snapshots )
        | LIR.SaveRegs ([], []) -> (
            match pendingRestores with
            | snapshot :: rest -> (rest, snapshot :: snapshots)
            | [] ->
                Crash.crash
                  "Unmatched SaveRegs while computing caller-save liveness")
        | _ -> (pendingRestores, snapshots)
      in
      (match facts.intDef with
      | Some id -> vregBitsRemoveInPlace intDomain id intLive
      | None -> ());
      List.iter
        (fun id -> vregBitsAddInPlace intDomain id intLive)
        facts.intUses;
      (match facts.floatDef with
      | Some id -> vregBitsRemoveInPlace floatDomain id floatLive
      | None -> ());
      List.iter
        (fun id -> vregBitsAddInPlace floatDomain id floatLive)
        facts.floatUses;
      walkBackwards (instrIdx - 1) pendingRestores snapshots
  in
  walkBackwards (Array.length instrFacts - 1) [] []

let isEmptySaveRegs = function LIR.SaveRegs ([], []) -> true | _ -> false
