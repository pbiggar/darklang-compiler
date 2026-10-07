(* FloatAllocation.ml - Schedule, allocate, spill, and materialize floating-point values. *)
[@@@warning "-4"]

open AllocationModel
open RegisterFacts
open RegisterLiveness
open RegisterInterference
open RegisterCoalescing
open RegisterColoring
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let add a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let mul a b = Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let neg a = Int32.to_int (Int32.neg (Int32.of_int a))

let floatCallerSavedRegs =
  [ LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7 ]

let floatCalleeSavedRegs =
  [ LIR.D8; LIR.D9; LIR.D10; LIR.D11; LIR.D12; LIR.D13; LIR.D14; LIR.D15 ]

let allocatableFloatRegs = floatCallerSavedRegs @ floatCalleeSavedRegs

let allocatableFloatRegsFor = function
  | Platform.ARM64 -> allocatableFloatRegs
  | Platform.X86_64 ->
      List.filteri (fun index _ -> index < 14) allocatableFloatRegs

let floatCallerSavedRegsFor = function
  | Platform.X86_64 -> allocatableFloatRegsFor Platform.X86_64
  | Platform.ARM64 -> floatCallerSavedRegs

type fAllocation =
  | FPhysReg of LIR.physFPReg
  | FStackSlot of int
  | FRematerialized of float

type fAllocationResult = {
  domain : vRegDomain;
  allocations : fAllocation option array;
  stackSize : int;
  usedCalleeSavedF : LIR.physFPReg list;
  spillScratchLeft : LIR.fReg;
  spillScratchRight : LIR.fReg;
  spillScratchThird : LIR.fReg;
}

let physFPRegToInt = function
  | LIR.D0 -> 0
  | LIR.D1 -> 1
  | LIR.D2 -> 2
  | LIR.D3 -> 3
  | LIR.D4 -> 4
  | LIR.D5 -> 5
  | LIR.D6 -> 6
  | LIR.D7 -> 7
  | LIR.D8 -> 8
  | LIR.D9 -> 9
  | LIR.D10 -> 10
  | LIR.D11 -> 11
  | LIR.D12 -> 12
  | LIR.D13 -> 13
  | LIR.D14 -> 14
  | LIR.D15 -> 15

let tryFloatAllocation (allocation : fAllocationResult) fvregId =
  match tryIndexOf allocation.domain fvregId with
  | Some idx -> allocation.allocations.(idx)
  | None -> None

let alignTo16 size = if size = 0 then 0 else mul (add size 15 / 16) 16

let rematerializableFloatLoads blocks =
  Array.fold_left
    (fun values (block : LIR.basicBlock) ->
      List.fold_left
        (fun values -> function
          | LIR.FLoad (LIR.FVirtual id, value) -> IntMap.add id value values
          | _ -> values)
        values block.LIR.instrs)
    IntMap.empty blocks

let allocateSpillSlots (graph : interferenceGraph) spills rematerializable =
  let spillIndices =
    List.init (Array.length graph.domain.ids) Fun.id
    |> List.filter (fun idx ->
        Bitset.containsIndex idx spills
        && not (IntMap.mem graph.domain.ids.(idx) rematerializable))
  in
  let firstAvailableColor used =
    let rec find candidate =
      if IntSet.mem candidate used then find (add candidate 1) else candidate
    in
    find 0
  in
  let assignments =
    List.fold_left
      (fun assigned idx ->
        let used =
          IntMap.fold
            (fun neighborIdx color colors ->
              if Bitset.containsIndex neighborIdx graph.neighbors.(idx) then
                IntSet.add color colors
              else colors)
            assigned IntSet.empty
        in
        IntMap.add idx (firstAvailableColor used) assigned)
      IntMap.empty spillIndices
  in
  let slotCount =
    IntMap.fold (fun _ color count -> max count (add color 1)) assignments 0
  in
  (assignments, slotCount)

let floatColoringToAllocation graph (colorResult : coloringResult) registers
    initialStackSize rematerializable =
  let spillSlots, spillSlotCount =
    allocateSpillSlots graph colorResult.spills rematerializable
  in
  let allocationAt idx =
    match colorResult.colors.(idx) with
    | Some color when color < List.length registers ->
        if color < 0 then
          invalid_arg
            "The index was outside the range of elements in the list. \
             (Parameter 'index')";
        Some (FPhysReg (List.nth registers color))
    | _ when Bitset.containsIndex idx colorResult.spills -> (
        match IntMap.find_opt colorResult.domain.ids.(idx) rematerializable with
        | Some value -> Some (FRematerialized value)
        | None -> (
            match IntMap.find_opt idx spillSlots with
            | Some slotColor ->
                Some
                  (FStackSlot
                     (neg (add initialStackSize (mul (add slotColor 1) 8))))
            | None ->
                Crash.crash
                  ("Missing Float spill slot for domain index "
                 ^ string_of_int idx)))
    | _ -> None
  in
  let allocations =
    Array.init (Array.length colorResult.domain.ids) allocationAt
  in
  let usedCalleeSaved =
    Array.to_list allocations
    |> List.filter_map (function
      | Some (FPhysReg reg) when List.mem reg floatCalleeSavedRegs -> Some reg
      | _ -> None)
    |> List.sort_uniq Stdlib.compare
  in
  let spillScratchLeft, spillScratchRight, spillScratchThird =
    if List.mem LIR.D15 registers then
      (LIR.FVirtual (-1000), LIR.FVirtual (-1001), LIR.FVirtual (-1002))
    else (LIR.FPhysical LIR.D14, LIR.FPhysical LIR.D15, LIR.FVirtual (-1002))
  in
  {
    domain = colorResult.domain;
    allocations;
    stackSize = alignTo16 (add initialStackSize (mul spillSlotCount 8));
    usedCalleeSavedF = usedCalleeSaved;
    spillScratchLeft;
    spillScratchRight;
    spillScratchThird;
  }

(*
   Move pure Float literal loads immediately before their first local use. Loads
   used only on CFG edges retain their original dominating position.
*)
let scheduleFloatLoadsInBlock (block : LIR.basicBlock) =
  let indexed = List.mapi (fun idx instr -> (idx, instr)) block.LIR.instrs in
  let loads =
    List.filter_map
      (fun (idx, instr) ->
        match instr with
        | LIR.FLoad (LIR.FVirtual id, _) -> Some (id, (idx, instr))
        | _ -> None)
      indexed
    |> IntMap.of_list
  in
  let firstUses =
    List.fold_left
      (fun uses (idx, instr) ->
        List.fold_left
          (fun result id ->
            if IntMap.mem id result then result else IntMap.add id idx result)
          uses (getUsedFVRegs instr))
      IntMap.empty indexed
  in
  let moves =
    IntMap.fold
      (fun id (loadIdx, instr) scheduled ->
        match IntMap.find_opt id firstUses with
        | Some useIdx when useIdx > loadIdx ->
            IntMap.add loadIdx (useIdx, instr) scheduled
        | _ -> scheduled)
      loads IntMap.empty
  in
  let loadsAtUse =
    IntMap.fold
      (fun _ (useIdx, instr) byUse ->
        let existing =
          Option.value ~default:[] (IntMap.find_opt useIdx byUse)
        in
        IntMap.add useIdx (existing @ [ instr ]) byUse)
      moves IntMap.empty
  in
  let scheduledInstrs =
    List.concat_map
      (fun (idx, instr) ->
        let before =
          Option.value ~default:[] (IntMap.find_opt idx loadsAtUse)
        in
        if IntMap.mem idx moves then before else before @ [ instr ])
      indexed
  in
  { block with LIR.instrs = scheduledInstrs }

let scheduleFloatLoadsInCFG (cfg : LIR.cfg) =
  {
    cfg with
    LIR.blocks = LIR.LabelMap.map scheduleFloatLoadsInBlock cfg.LIR.blocks;
  }

let chordalFloatAllocationWithLiveness registers initialStackSize blockIndex
    blocks classifiedBlocks additionalVRegs paramPrecolors domain livenessBits =
  let graph =
    buildFloatInterferenceGraphBitsetWithLiveness blockIndex classifiedBlocks
      domain livenessBits additionalVRegs
  in
  let graphWithParams =
    { graph with vertices = Bitset.union graph.vertices additionalVRegs }
  in
  if Bitset.isEmpty graphWithParams.vertices then
    {
      domain;
      allocations = Array.make (Array.length domain.ids) None;
      stackSize = initialStackSize;
      usedCalleeSavedF = [];
      spillScratchLeft = LIR.FVirtual (-1000);
      spillScratchRight = LIR.FVirtual (-1001);
      spillScratchThird = LIR.FVirtual (-1002);
    }
  else
    let phiPairs = collectFPhiPairs blocks in
    let movePairs =
      dedupePairs (collectFPhiSourceMovePairs blocks @ phiPairs)
    in
    let phiIds =
      List.fold_left
        (fun ids (destId, sourceId) ->
          IntSet.add sourceId (IntSet.add destId ids))
        IntSet.empty phiPairs
    in
    let phiParamPrecolors =
      List.filter (fun (vregId, _) -> IntSet.mem vregId phiIds) paramPrecolors
    in
    let colorResult =
      chordalGraphColor graphWithParams phiParamPrecolors
        (List.length registers) phiPairs movePairs
    in
    floatColoringToAllocation graphWithParams colorResult registers
      initialStackSize
      (rematerializableFloatLoads blocks)

let chordalFloatAllocation cfg additionalVRegs =
  let scheduledCFG = scheduleFloatLoadsInCFG cfg in
  let blockIndex, blocks = buildBlockIndex scheduledCFG in
  let classifiedBlocks = classifyBlocks blocks in
  let domain, livenessBits =
    computeFloatLivenessBitsFromFacts blockIndex classifiedBlocks
      additionalVRegs
  in
  chordalFloatAllocationWithLiveness allocatableFloatRegs 0 blockIndex blocks
    classifiedBlocks
    (vregBitsFromList domain additionalVRegs)
    [] domain livenessBits

let isFixedFReg = function
  | LIR.FVirtual -1
  | LIR.FVirtual -1000
  | LIR.FVirtual -1001
  | LIR.FVirtual -1002
  | LIR.FVirtual -2000 ->
      true
  | _ -> false

let applyFloatAllocationToFReg allocation = function
  | LIR.FPhysical _ as freg -> freg
  | freg when isFixedFReg freg -> freg
  | LIR.FVirtual id -> (
      match tryFloatAllocation allocation id with
      | Some (FPhysReg reg) -> LIR.FPhysical reg
      | Some (FStackSlot _) ->
          Crash.crash
            ("Spilled Float vreg " ^ string_of_int id
           ^ " requires instruction repair")
      | Some (FRematerialized _) ->
          Crash.crash
            ("Rematerialized Float vreg " ^ string_of_int id
           ^ " requires instruction repair")
      | None ->
          Crash.crash
            ("Float register allocation bug: FVirtual " ^ string_of_int id
           ^ " not found in allocation"))

let materializeUse allocation scratch = function
  | LIR.FPhysical _ as freg -> ([], freg)
  | freg when isFixedFReg freg -> ([], freg)
  | LIR.FVirtual id -> (
      match tryFloatAllocation allocation id with
      | Some (FPhysReg reg) -> ([], LIR.FPhysical reg)
      | Some (FStackSlot slot) -> ([ LIR.FSpillLoad (scratch, slot) ], scratch)
      | Some (FRematerialized value) -> ([ LIR.FLoad (scratch, value) ], scratch)
      | None ->
          Crash.crash
            ("Float register allocation bug: FVirtual " ^ string_of_int id
           ^ " not found in allocation"))

let destination allocation scratch = function
  | LIR.FPhysical _ as freg -> (freg, Fun.id)
  | freg when isFixedFReg freg -> (freg, Fun.id)
  | LIR.FVirtual id -> (
      match tryFloatAllocation allocation id with
      | Some (FPhysReg reg) -> (LIR.FPhysical reg, Fun.id)
      | Some (FStackSlot slot) ->
          (scratch, fun instrs -> instrs @ [ LIR.FSpillStore (slot, scratch) ])
      | Some (FRematerialized _) ->
          Crash.crash
            ("Only Float literal loads may define rematerialized vreg "
           ^ string_of_int id)
      | None ->
          Crash.crash
            ("Float register allocation bug: FVirtual " ^ string_of_int id
           ^ " not found in allocation"))

let physFPRegAsGPReg = function
  | LIR.D0 -> LIR.X0
  | LIR.D1 -> LIR.X1
  | LIR.D2 -> LIR.X2
  | LIR.D3 -> LIR.X3
  | LIR.D4 -> LIR.X4
  | LIR.D5 -> LIR.X5
  | LIR.D6 -> LIR.X6
  | LIR.D7 -> LIR.X7
  | LIR.D8 -> LIR.X8
  | LIR.D9 -> LIR.X9
  | LIR.D10 -> LIR.X10
  | LIR.D11 -> LIR.X11
  | LIR.D12 -> LIR.X12
  | LIR.D13 -> LIR.X13
  | LIR.D14 -> LIR.X14
  | LIR.D15 -> LIR.X15

let applyFloatArgMoves allocation moves =
  let located =
    List.map
      (fun (dest, src) ->
        let source =
          match src with
          | LIR.FPhysical reg -> FPhysReg reg
          | LIR.FVirtual id -> (
              match tryFloatAllocation allocation id with
              | Some value -> value
              | None ->
                  Crash.crash
                    ("Float argument source " ^ string_of_int id
                   ^ " has no allocation"))
        in
        (dest, source))
      moves
  in
  let sourceRegister = function
    | FPhysReg reg -> Some reg
    | FStackSlot _ | FRematerialized _ -> None
  in
  ParallelMoves.resolve located sourceRegister
  |> List.concat_map (function
    | ParallelMoves.SaveToTemp reg ->
        [ LIR.FMov (allocation.spillScratchRight, LIR.FPhysical reg) ]
    | ParallelMoves.Move (dest, FPhysReg src) ->
        [ LIR.FMov (LIR.FPhysical dest, LIR.FPhysical src) ]
    | ParallelMoves.Move (dest, FStackSlot slot) ->
        [ LIR.FSpillLoad (LIR.FPhysical dest, slot) ]
    | ParallelMoves.Move (dest, FRematerialized value) ->
        [ LIR.FLoad (LIR.FPhysical dest, value) ]
    | ParallelMoves.MoveFromTemp dest ->
        [ LIR.FMov (LIR.FPhysical dest, allocation.spillScratchRight) ])

let applyFloatAllocationToInstrs allocation instr =
  let unary dest src makeInstr =
    let loads, allocatedSrc =
      materializeUse allocation allocation.spillScratchLeft src
    in
    let allocatedDest, finish =
      destination allocation allocation.spillScratchLeft dest
    in
    finish (loads @ [ makeInstr allocatedDest allocatedSrc ])
  in
  let binary dest left right makeInstr =
    let leftLoads, allocatedLeft =
      materializeUse allocation allocation.spillScratchLeft left
    in
    let rightLoads, allocatedRight =
      materializeUse allocation allocation.spillScratchRight right
    in
    let allocatedDest, finish =
      destination allocation allocation.spillScratchLeft dest
    in
    finish
      (leftLoads @ rightLoads
      @ [ makeInstr allocatedDest allocatedLeft allocatedRight ])
  in
  let ternary dest left right third makeInstr =
    let leftLoads, allocatedLeft =
      materializeUse allocation allocation.spillScratchLeft left
    in
    let rightLoads, allocatedRight =
      materializeUse allocation allocation.spillScratchRight right
    in
    let thirdLoads, allocatedThird =
      materializeUse allocation allocation.spillScratchThird third
    in
    let allocatedDest, finish =
      destination allocation allocation.spillScratchLeft dest
    in
    finish
      (leftLoads @ rightLoads @ thirdLoads
      @ [ makeInstr allocatedDest allocatedLeft allocatedRight allocatedThird ]
      )
  in
  let useOne src makeInstr =
    let loads, allocatedSrc =
      materializeUse allocation allocation.spillScratchLeft src
    in
    loads @ [ makeInstr allocatedSrc ]
  in
  match instr with
  | LIR.FMov (dest, src) -> unary dest src (fun d s -> LIR.FMov (d, s))
  | LIR.FLoad (dest, value) when isFixedFReg dest -> [ LIR.FLoad (dest, value) ]
  | LIR.FLoad (LIR.FVirtual id, value) -> (
      match tryFloatAllocation allocation id with
      | Some (FPhysReg reg) -> [ LIR.FLoad (LIR.FPhysical reg, value) ]
      | Some (FStackSlot slot) ->
          [
            LIR.FLoad (allocation.spillScratchLeft, value);
            LIR.FSpillStore (slot, allocation.spillScratchLeft);
          ]
      | Some (FRematerialized _) -> []
      | None ->
          Crash.crash
            ("Float literal destination " ^ string_of_int id
           ^ " has no allocation"))
  | LIR.FLoad (dest, value) -> [ LIR.FLoad (dest, value) ]
  | LIR.FSpillLoad _ | LIR.FSpillStore _ -> [ instr ]
  | LIR.FAdd (dest, left, right) ->
      binary dest left right (fun d l r -> LIR.FAdd (d, l, r))
  | LIR.FSub (dest, left, right) ->
      binary dest left right (fun d l r -> LIR.FSub (d, l, r))
  | LIR.FMul (dest, left, right) ->
      binary dest left right (fun d l r -> LIR.FMul (d, l, r))
  | LIR.FMadd (dest, left, right, addend) ->
      ternary dest left right addend (fun d l r a -> LIR.FMadd (d, l, r, a))
  | LIR.FDiv (dest, left, right) ->
      binary dest left right (fun d l r -> LIR.FDiv (d, l, r))
  | LIR.FNeg (dest, src) -> unary dest src (fun d s -> LIR.FNeg (d, s))
  | LIR.FAbs (dest, src) -> unary dest src (fun d s -> LIR.FAbs (d, s))
  | LIR.FSqrt (dest, src) -> unary dest src (fun d s -> LIR.FSqrt (d, s))
  | LIR.FCmp (left, right) ->
      let leftLoads, allocatedLeft =
        materializeUse allocation allocation.spillScratchLeft left
      in
      let rightLoads, allocatedRight =
        materializeUse allocation allocation.spillScratchRight right
      in
      leftLoads @ rightLoads @ [ LIR.FCmp (allocatedLeft, allocatedRight) ]
  | LIR.Int64ToFloat (dest, src) ->
      let allocatedDest, finish =
        destination allocation allocation.spillScratchLeft dest
      in
      finish [ LIR.Int64ToFloat (allocatedDest, src) ]
  | LIR.GpToFp (dest, src) ->
      let allocatedDest, finish =
        destination allocation allocation.spillScratchLeft dest
      in
      finish [ LIR.GpToFp (allocatedDest, src) ]
  | LIR.FloatToInt64 (dest, src) ->
      useOne src (fun s -> LIR.FloatToInt64 (dest, s))
  | LIR.FloatToBits (dest, src) ->
      useOne src (fun s -> LIR.FloatToBits (dest, s))
  | LIR.FpToGp (dest, src) -> useOne src (fun s -> LIR.FpToGp (dest, s))
  | LIR.PrintFloat src -> useOne src (fun s -> LIR.PrintFloat s)
  | LIR.PrintFloatNoNewline src ->
      useOne src (fun s -> LIR.PrintFloatNoNewline s)
  | LIR.FloatToString (dest, src) ->
      useOne src (fun s -> LIR.FloatToString (dest, s))
  | LIR.Sleep (effectId, src) -> useOne src (fun s -> LIR.Sleep (effectId, s))
  | LIR.FArgMoves moves -> applyFloatArgMoves allocation moves
  | LIR.FPhi (dest, sources) ->
      let allocatedDest = applyFloatAllocationToFReg allocation dest in
      let allocatedSources =
        List.map
          (fun (src, label) ->
            (applyFloatAllocationToFReg allocation src, label))
          sources
      in
      [ LIR.FPhi (allocatedDest, allocatedSources) ]
  | LIR.HeapStore (addr, offset, LIR.Reg (LIR.Virtual id), Some AST.TFloat64)
    -> (
      let loads, allocated =
        materializeUse allocation allocation.spillScratchLeft (LIR.FVirtual id)
      in
      match allocated with
      | LIR.FPhysical reg ->
          loads
          @ [
              LIR.HeapStore
                ( addr,
                  offset,
                  LIR.Reg (LIR.Physical (physFPRegAsGPReg reg)),
                  Some AST.TFloat64 );
            ]
      | LIR.FVirtual scratchId ->
          loads
          @ [
              LIR.HeapStore
                ( addr,
                  offset,
                  LIR.Reg (LIR.Virtual scratchId),
                  Some AST.TFloat64 );
            ])
  | _ -> [ instr ]

let applyFloatAllocationToBlock allocation (block : LIR.basicBlock) =
  {
    block with
    LIR.instrs =
      List.concat_map (applyFloatAllocationToInstrs allocation) block.LIR.instrs;
  }

let applyFloatAllocationToBlocks allocation blocks =
  Array.map (applyFloatAllocationToBlock allocation) blocks

let applyFloatAllocationToCFG allocation (cfg : LIR.cfg) =
  let blockIndex, blocks = buildBlockIndex cfg in
  let updatedBlocks = applyFloatAllocationToBlocks allocation blocks in
  { cfg with LIR.blocks = blocksToMap blockIndex updatedBlocks }
