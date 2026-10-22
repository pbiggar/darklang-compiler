// Coalescing.fs - Collect move preferences and coalesce compatible graph vertices.

module RegisterCoalescing

open AllocationModel

let private normalizePair (a: int) (b: int) : int * int =
    if a < b then (a, b) else (b, a)

let internal dedupePairs (pairs: (int * int) list) : (int * int) list =
    let sorted = pairs |> List.map (fun (a, b) -> normalizePair a b) |> List.sort
    let rec loop (last: (int * int) option) (acc: (int * int) list) (remaining: (int * int) list) =
        match remaining with
        | [] -> List.rev acc
        | head :: tail ->
            match last with
            | Some prev when prev = head -> loop last acc tail
            | _ -> loop (Some head) (head :: acc) tail
    loop None [] sorted

/// Collect move-related coalescing pairs from CFG.
/// Returns undirected pairs of virtual registers that are directly moved between.
let collectMovePairs (blocks: LIR.BasicBlock array) : (int * int) list =
    blocks
    |> Array.fold (fun acc block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIR.Mov (LIR.Virtual destId, LIR.Reg (LIR.Virtual srcId)) ->
                (destId, srcId) :: acc
            | _ -> acc) acc) []
    |> dedupePairs

/// Collect phi-related coalescing pairs from CFG.
/// Returns undirected pairs of virtual registers that flow into the same phi destination.
let collectPhiPairs (blocks: LIR.BasicBlock array) : (int * int) list =
    blocks
    |> Array.fold (fun acc block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIR.Phi (LIR.Virtual destId, sources, _) ->
                sources
                |> List.fold (fun acc (src, _) ->
                    match src with
                    | LIR.Reg (LIR.Virtual srcId) when srcId <> destId ->
                        (destId, srcId) :: acc
                    | _ -> acc) acc
            | _ -> acc) acc) []
    |> dedupePairs

/// Collect float phi-related coalescing pairs from CFG blocks.
/// Returns undirected pairs of non-identical virtual float registers that flow
/// into the same FPhi destination.
let collectFPhiPairs (blocks: LIR.BasicBlock array) : (int * int) list =
    blocks
    |> Array.fold (fun acc block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIR.FPhi (LIR.FVirtual destId, sources) ->
                sources
                |> List.fold (fun acc (src, _) ->
                    match src with
                    | LIR.FVirtual srcId when srcId <> destId ->
                        (destId, srcId) :: acc
                    | _ -> acc) acc
            | _ -> acc) acc) []
    |> dedupePairs

/// Collect virtual float moves that define FPhi sources. Coalescing the whole
/// incoming copy chain avoids trading a resolved phi move for its feeder move.
let collectFPhiSourceMovePairs (blocks: LIR.BasicBlock array) : (int * int) list =
    let phiSources =
        blocks
        |> Array.fold (fun acc block ->
            block.Instrs
            |> List.fold (fun acc instr ->
                match instr with
                | LIR.FPhi (_, sources) ->
                    sources
                    |> List.fold (fun acc (src, _) ->
                        match src with
                        | LIR.FVirtual srcId -> Set.add srcId acc
                        | LIR.FPhysical _ | LIR.FParallelMoveTemp -> acc) acc
                | _ -> acc) acc) Set.empty
    blocks
    |> Array.fold (fun acc block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            match instr with
            | LIR.FMov (LIR.FVirtual destId, LIR.FVirtual srcId)
                when Set.contains destId phiSources ->
                (destId, srcId) :: acc
            | _ -> acc) acc) []
    |> dedupePairs

/// Collect phi coalescing preferences from CFG.
/// Returns undirected pairs (vregId, vregId) representing preferred coalescing.
let collectPhiPreferences (blocks: LIR.BasicBlock array) : (int * int) list =
    collectPhiPairs blocks

/// Maximum Cardinality Search - computes Perfect Elimination Ordering for chordal graphs
/// Returns vertices in PEO order (first vertex is most "central")
/// Uses a bucket queue for linear-time selection in terms of vertices + edges.
let private maximumCardinalitySearchCore
    (graph: InterferenceGraph)
    : int list * McsProfile =
    let domain = graph.Domain
    let n = domain.Ids.Length
    let vertexCount = Bitset.count graph.Vertices
    if vertexCount = 0 then
        let profile = { VertexCount = 0; SelectionChecks = 0; WeightUpdates = 0; BucketSkips = 0 }
        ([], profile)
    else
        let inGraph = Array.create n false
        Bitset.iterIndices graph.Vertices (fun idx -> inGraph.[idx] <- true)

        // Track weights and ordered status
        let weights = Array.zeroCreate<int> n
        let ordered = Array.create n false

        // Bucket queue state (weight -> list of vertices)
        let bucketHeads = Array.create vertexCount -1
        let next = Array.create n -1
        let prev = Array.create n -1

        // Initialize all vertices in bucket 0
        for idx in 0 .. n - 1 do
            if inGraph.[idx] then
                let head = bucketHeads.[0]
                next.[idx] <- head
                prev.[idx] <- -1
                if head <> -1 then prev.[head] <- idx
                bucketHeads.[0] <- idx

        let removeFromBucket (idx: int) (weight: int) : unit =
            let p = prev.[idx]
            let nidx = next.[idx]
            if p <> -1 then
                next.[p] <- nidx
            else
                bucketHeads.[weight] <- nidx
            if nidx <> -1 then
                prev.[nidx] <- p
            next.[idx] <- -1
            prev.[idx] <- -1

        let addToBucket (idx: int) (weight: int) : unit =
            let head = bucketHeads.[weight]
            next.[idx] <- head
            prev.[idx] <- -1
            if head <> -1 then prev.[head] <- idx
            bucketHeads.[weight] <- idx

        let mutable currentMax = 0
        let mutable ordering = []
        let mutable selectionChecks = 0
        let mutable weightUpdates = 0
        let mutable bucketSkips = 0

        for _ in 0 .. vertexCount - 1 do
            while currentMax >= 0 && bucketHeads.[currentMax] = -1 do
                currentMax <- currentMax - 1
                bucketSkips <- bucketSkips + 1
            if currentMax < 0 then
                Crash.crash "MCS bucket queue empty before selecting all vertices"

            let idx = bucketHeads.[currentMax]
            selectionChecks <- selectionChecks + 1
            removeFromBucket idx currentMax
            ordered.[idx] <- true
            ordering <- domain.Ids.[idx] :: ordering

            Bitset.iterIndices graph.Neighbors.[idx] (fun nidx ->
                if inGraph.[nidx] && not ordered.[nidx] then
                    let oldWeight = weights.[nidx]
                    removeFromBucket nidx oldWeight
                    let newWeight = oldWeight + 1
                    if newWeight >= vertexCount then
                        Crash.crash $"MCS weight overflow: {newWeight} >= {vertexCount}"
                    weights.[nidx] <- newWeight
                    addToBucket nidx newWeight
                    if newWeight > currentMax then currentMax <- newWeight
                    weightUpdates <- weightUpdates + 1)

        let profile = {
            VertexCount = vertexCount
            SelectionChecks = selectionChecks
            WeightUpdates = weightUpdates
            BucketSkips = bucketSkips
        }
        (List.rev ordering, profile)

let maximumCardinalitySearchWithProfile (graph: InterferenceGraph) : int list * McsProfile =
    maximumCardinalitySearchCore graph

let maximumCardinalitySearch (graph: InterferenceGraph) : int list =
    let (ordering, _profile) = maximumCardinalitySearchWithProfile graph
    ordering

type internal CoalescedGraph = {
    Graph: InterferenceGraph
    RepOfIndex: int array
    RepMembers: BitSet array
    Preferences: BitSet array
    Precolored: int option array
}

let internal coalesceGraphFast
    (graph: InterferenceGraph)
    (precoloredPairs: (int * int) list)
    (movePairs: (int * int) list)
    (preferencePairs: (int * int) list)
    : CoalescedGraph =
    let domain = graph.Domain
    let n = domain.Ids.Length
    let wordCount = domain.WordCount
    if Bitset.isEmpty graph.Vertices then
        { Graph = graph
          RepOfIndex = Array.init n id
          RepMembers = Array.init n (fun _ -> Bitset.empty wordCount)
          Preferences = Array.init n (fun _ -> Bitset.empty wordCount)
          Precolored = Array.create n None }
    else
        let inGraph = Array.create n false
        Bitset.iterIndices graph.Vertices (fun idx -> inGraph.[idx] <- true)

        let parent = Array.init n id
        let sizes = Array.create n 0
        let members = Array.init n (fun _ -> Bitset.empty wordCount)
        let neighbors = Array.init n (fun _ -> Bitset.empty wordCount)
        let precolor = Array.create n None
        let repId = Array.create n 0

        for idx in 0 .. n - 1 do
            if inGraph.[idx] then
                sizes.[idx] <- 1
                let bits = Bitset.empty wordCount
                Bitset.addIndexInPlace idx bits
                members.[idx] <- bits
                neighbors.[idx] <- Bitset.clone graph.Neighbors.[idx]
                repId.[idx] <- domain.Ids.[idx]
            else
                repId.[idx] <- domain.Ids.[idx]

        for (vregId, color) in precoloredPairs do
            match tryIndexOf domain vregId with
            | Some idx when inGraph.[idx] -> precolor.[idx] <- Some color
            | _ -> ()

        let rec find idx =
            let p = parent.[idx]
            if p = idx then
                idx
            else
                let root = find p
                parent.[idx] <- root
                root

        let canMerge rootA rootB =
            if rootA = rootB then
                false
            else
                match precolor.[rootA], precolor.[rootB] with
                | Some c1, Some c2 when c1 <> c2 -> false
                | _ ->
                    if Bitset.intersects neighbors.[rootA] members.[rootB] then
                        false
                    else
                        not (Bitset.intersects neighbors.[rootB] members.[rootA])

        let union rootA rootB =
            let ra, rb =
                if sizes.[rootA] < sizes.[rootB] then
                    (rootB, rootA)
                else
                    (rootA, rootB)
            parent.[rb] <- ra
            sizes.[ra] <- sizes.[ra] + sizes.[rb]
            Bitset.unionInPlace members.[ra] members.[rb]
            Bitset.unionInPlace neighbors.[ra] neighbors.[rb]
            Bitset.diffInPlace neighbors.[ra] members.[ra]
            match precolor.[ra], precolor.[rb] with
            | None, Some color -> precolor.[ra] <- Some color
            | _ -> ()
            if repId.[rb] < repId.[ra] then
                repId.[ra] <- repId.[rb]

        for (u, v) in movePairs do
            match tryIndexOf domain u, tryIndexOf domain v with
            | Some idxU, Some idxV when inGraph.[idxU] && inGraph.[idxV] ->
                let rootU = find idxU
                let rootV = find idxV
                if canMerge rootU rootV then
                    union rootU rootV
            | _ -> ()

        let rootOfIdx = Array.init n (fun idx -> if inGraph.[idx] then find idx else idx)

        let repIndexOfRoot = Array.create n -1
        for idx in 0 .. n - 1 do
            if inGraph.[idx] && parent.[idx] = idx then
                let repValue = repId.[idx]
                match tryIndexOf domain repValue with
                | Some repIdx -> repIndexOfRoot.[idx] <- repIdx
                | None -> Crash.crash $"coalesceGraphFast: Missing rep index for {repValue}"

        let repOfIndex = Array.create n -1
        for idx in 0 .. n - 1 do
            if inGraph.[idx] then
                let root = rootOfIdx.[idx]
                let repIdx = repIndexOfRoot.[root]
                if repIdx < 0 then
                    Crash.crash $"coalesceGraphFast: Missing rep for {domain.Ids.[idx]}"
                repOfIndex.[idx] <- repIdx

        let repMembers = Array.init n (fun _ -> Bitset.empty wordCount)
        let repVertices = Bitset.empty wordCount
        for idx in 0 .. n - 1 do
            if inGraph.[idx] then
                let repIdx = repOfIndex.[idx]
                Bitset.addIndexInPlace idx repMembers.[repIdx]
                Bitset.addIndexInPlace repIdx repVertices

        let repPrecolored = Array.create n None
        for idx in 0 .. n - 1 do
            if inGraph.[idx] && parent.[idx] = idx then
                let repIdx = repIndexOfRoot.[idx]
                match precolor.[idx] with
                | Some color -> repPrecolored.[repIdx] <- Some color
                | None -> ()

        let repPreferences = Array.init n (fun _ -> Bitset.empty wordCount)
        for (u, v) in preferencePairs do
            match tryIndexOf domain u, tryIndexOf domain v with
            | Some idxU, Some idxV when inGraph.[idxU] && inGraph.[idxV] ->
                let repU = repOfIndex.[idxU]
                let repV = repOfIndex.[idxV]
                if repU <> repV && repU >= 0 && repV >= 0 then
                    Bitset.addIndexInPlace repV repPreferences.[repU]
                    Bitset.addIndexInPlace repU repPreferences.[repV]
            | _ -> ()

        let repNeighbors = Array.init n (fun _ -> Bitset.empty wordCount)
        for idx in 0 .. n - 1 do
            if inGraph.[idx] && parent.[idx] = idx then
                let repIdx = repIndexOfRoot.[idx]
                Bitset.iterIndices neighbors.[idx] (fun nidx ->
                    if inGraph.[nidx] then
                        let rootN = rootOfIdx.[nidx]
                        if rootN <> idx then
                            let repN = repIndexOfRoot.[rootN]
                            if repIdx <> repN && repIdx >= 0 && repN >= 0 then
                                Bitset.addIndexInPlace repN repNeighbors.[repIdx]
                                Bitset.addIndexInPlace repIdx repNeighbors.[repN])

        let repGraph = { Domain = domain; Vertices = repVertices; Neighbors = repNeighbors }

        { Graph = repGraph
          RepOfIndex = repOfIndex
          RepMembers = repMembers
          Preferences = repPreferences
          Precolored = repPrecolored }

let internal expandColoring (result: ColoringResult) (repMembers: BitSet array) : ColoringResult =
    let domain = result.Domain
    let n = domain.Ids.Length
    let expandedColors = Array.create n None
    let expandedSpills = Bitset.empty domain.WordCount

    for repIdx in 0 .. n - 1 do
        match result.Colors.[repIdx] with
        | Some color ->
            Bitset.iterIndices repMembers.[repIdx] (fun memberIdx ->
                expandedColors.[memberIdx] <- Some color)
        | None -> ()

    Bitset.iterIndices result.Spills (fun repIdx ->
        Bitset.unionInPlace expandedSpills repMembers.[repIdx])

    { Domain = domain
      Colors = expandedColors
      Spills = expandedSpills
      ChromaticNumber = result.ChromaticNumber }
