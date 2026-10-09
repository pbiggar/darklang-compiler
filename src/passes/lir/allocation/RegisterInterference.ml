(* RegisterInterference.ml - Build register-interference graphs from solved liveness. *)
open AllocationModel
open RegisterFacts
open RegisterLiveness

(*
   Chordal Graph Coloring Register Allocation
*)
let buildInterferenceGraphBitsetFastWithLivenessInternal blockIndex
    classifiedBlocks domain liveness entryDefs =
  let n = Array.length domain.ids in
  let wordCount = domain.wordCount in
  let adjacency = Array.init n (fun _ -> Bitset.empty wordCount) in
  let present = Bitset.empty wordCount in
  let markPresentIdx idx =
    if idx >= 0 && idx < n then Bitset.addIndexInPlace idx present
  in
  let markPresentValue value =
    match tryIndexOf domain value with
    | Some idx -> markPresentIdx idx
    | None -> ()
  in
  let addEdgesToLive defIdx live =
    Bitset.unionInPlace adjacency.(defIdx) live;
    Bitset.removeIndexInPlace defIdx adjacency.(defIdx);
    Bitset.iterIndices live (fun idx ->
        if idx <> defIdx then Bitset.addIndexInPlace defIdx adjacency.(idx))
  in
  Array.iteri
    (fun blockIdx blockFacts ->
      let blockLiveness = liveness.(blockIdx) in
      let live = Bitset.clone blockLiveness.liveOut in
      List.iter
        (fun v -> vregBitsAddInPlace domain v live)
        blockFacts.terminatorUses;
      Bitset.iterIndices live markPresentIdx;
      for instrIdx = Array.length blockFacts.instrFacts - 1 downto 0 do
        let facts = blockFacts.instrFacts.(instrIdx) in
        List.iter markPresentValue facts.intUses;
        (match facts.intDef with
        | Some d ->
            markPresentValue d;
            (match tryIndexOf domain d with
            | Some defIdx -> addEdgesToLive defIdx live
            | None -> ());
            vregBitsRemoveInPlace domain d live
        | None -> ());
        List.iter (fun u -> vregBitsAddInPlace domain u live) facts.intUses
      done;
      if blockIdx = blockIndex.entryIndex then
        Bitset.iterIndices entryDefs (fun defIdx ->
            if Bitset.containsIndex defIdx live then (
              markPresentIdx defIdx;
              addEdgesToLive defIdx live)))
    classifiedBlocks;
  { domain; vertices = present; neighbors = adjacency }

(*
   Build interference graph from CFG using bitset liveness
*)
let buildInterferenceGraphBitsetWithLiveness blockIndex classifiedBlocks domain
    liveness entryDefs =
  buildInterferenceGraphBitsetFastWithLivenessInternal blockIndex
    classifiedBlocks domain liveness entryDefs

(*
   Build interference graph from CFG using bitset liveness
*)
let buildInterferenceGraphBitsetFast cfg entryDefs =
  let blockIndex, blocks = buildBlockIndex cfg in
  let classifiedBlocks = classifyBlocks blocks in
  let domain, liveness =
    computeLivenessBitsFromFacts blockIndex classifiedBlocks entryDefs
  in
  let entryBits = vregBitsFromList domain entryDefs in
  buildInterferenceGraphBitsetWithLiveness blockIndex classifiedBlocks domain
    liveness entryBits

(*
   Build interference graph from CFG using bitset liveness
*)
let buildInterferenceGraphBitset cfg entryDefs =
  buildInterferenceGraphBitsetFast cfg entryDefs

(*
   Build float interference graph from CFG using bitset liveness
*)
let buildFloatInterferenceGraphBitsetWithLiveness blockIndex classifiedBlocks
    domain liveness entryDefs =
  let ids = domain.ids in
  let n = Array.length ids in
  let wordCount = domain.wordCount in
  let adjacency = Array.init n (fun _ -> Bitset.empty wordCount) in
  let present = Bitset.empty wordCount in
  let markPresentIdx idx =
    if idx >= 0 && idx < n then Bitset.addIndexInPlace idx present
  in
  let markPresentValue value =
    match tryIndexOf domain value with
    | Some idx -> markPresentIdx idx
    | None -> ()
  in
  let addEdgesToLive defIdx live =
    Bitset.unionInPlace adjacency.(defIdx) live;
    Bitset.removeIndexInPlace defIdx adjacency.(defIdx);
    Bitset.iterIndices live (fun idx ->
        if idx <> defIdx then Bitset.addIndexInPlace defIdx adjacency.(idx))
  in
  Array.iteri
    (fun blockIdx blockFacts ->
      let blockLiveness = liveness.(blockIdx) in
      let live = Bitset.clone blockLiveness.liveOut in
      Bitset.iterIndices live markPresentIdx;
      for instrIdx = Array.length blockFacts.instrFacts - 1 downto 0 do
        let facts = blockFacts.instrFacts.(instrIdx) in
        List.iter markPresentValue facts.floatUses;
        (match facts.floatDef with
        | Some d ->
            markPresentValue d;
            (match tryIndexOf domain d with
            | Some defIdx -> addEdgesToLive defIdx live
            | None -> ());
            vregBitsRemoveInPlace domain d live
        | None -> ());
        List.iter (fun u -> vregBitsAddInPlace domain u live) facts.floatUses
      done;
      if blockIdx = blockIndex.entryIndex then
        Bitset.iterIndices entryDefs (fun defIdx ->
            if Bitset.containsIndex defIdx live then (
              markPresentIdx defIdx;
              addEdgesToLive defIdx live)))
    classifiedBlocks;
  { domain; vertices = present; neighbors = adjacency }
