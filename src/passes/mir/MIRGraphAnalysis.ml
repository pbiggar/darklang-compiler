(* MIRGraphAnalysis.ml - Analyze MIR control-flow edges, dominance and register uses. *)
[@@@warning "-4"]

open MIR
module F = MIROptimizationFacts
module LM = LabelMap
module LS = LabelSet
module VS = VRegSet

type predecessors = label list LM.t
type dominators = label LM.t

let structuralLabel (Label value) =
  StructuralFormat.format
    (StructuralFormat.Union ("Label", [ StructuralFormat.Text value ]))

(*
   Get successor labels of a block
*)
let getSuccessors (block : basicBlock) =
  match block.terminator with
  | Ret _ -> []
  | Jump label -> [ label ]
  | Branch (_, yes, no) -> [ yes; no ]

(*
   Build predecessors map from CFG
   Add edges from terminator
*)
let buildPredecessors (cfg : cfg) =
  let edge from target predecessors =
    LM.add target
      (from :: Option.value ~default:[] (LM.find_opt target predecessors))
      predecessors
  in
  LM.fold
    (fun label block predecessors ->
      match block.terminator with
      | Ret _ -> predecessors
      | Jump target -> edge label target predecessors
      | Branch (_, yes, no) -> edge label no (edge label yes predecessors))
    cfg.blocks LM.empty

(*
   Cooper-Harvey-Kennedy converges quickly when blocks are visited in
   reverse postorder. Keep the DFS stack explicit because generated test
   functions can contain thousands of blocks.
   Mapping entry to itself gives intersect a sentinel root. Remove it from
   the public result after the fixed point settles.
*)
let computeDominators (cfg : cfg) predecessors =
  let entry = cfg.entry in
  let rec order work visited reversed =
    match work with
    | [] -> reversed
    | (label, expanded) :: rest ->
        if expanded then order rest visited (label :: reversed)
        else if LS.mem label visited then order rest visited reversed
        else
          let successors =
            match LM.find_opt label cfg.blocks with
            | None -> []
            | Some block -> getSuccessors block
          in
          order
            (List.map (fun label -> (label, false)) successors
            @ ((label, true) :: rest))
            (LS.add label visited) reversed
  in
  let reversePostorder =
    order [ (entry, false) ] LS.empty []
    |> List.filter (fun label -> LM.mem label cfg.blocks)
  in
  let positions =
    LM.of_list (List.mapi (fun index label -> (label, index)) reversePostorder)
  in
  let position label =
    match LM.find_opt label positions with
    | Some position -> position
    | None ->
        Crash.crash
          ("SSA: Missing reverse-postorder position for "
         ^ structuralLabel label)
  in
  let parent dominators label =
    match LM.find_opt label dominators with
    | Some parent -> parent
    | None ->
        Crash.crash
          ("SSA: Missing immediate dominator for " ^ structuralLabel label)
  in
  let rec intersect dominators left right =
    if left = right then left
    else if position left > position right then
      intersect dominators (parent dominators left) right
    else intersect dominators left (parent dominators right)
  in
  let rec converge labels dominators =
    let changed, updated =
      List.fold_left
        (fun (changed, current) label ->
          let processed =
            Option.value ~default:[] (LM.find_opt label predecessors)
            |> List.filter (fun label -> LM.mem label current)
          in
          match processed with
          | [] -> (changed, current)
          | first :: rest -> (
              let immediate = List.fold_left (intersect current) first rest in
              match LM.find_opt label current with
              | Some old when old = immediate -> (changed, current)
              | _ -> (true, LM.add label immediate current)))
        (false, dominators) labels
    in
    if changed then converge labels updated else updated
  in
  match reversePostorder with
  | [] -> LM.empty
  | _ :: labels -> LM.remove entry (converge labels (LM.singleton entry entry))

let getBlockDefs (block : basicBlock) =
  List.fold_left
    (fun defs instruction ->
      match F.getInstrDest instruction with
      | None -> defs
      | Some dest -> VS.add dest defs)
    VS.empty block.instrs

(*
   Extract VRegs used in an operand
*)
let getOperandUses = function Register reg -> VS.singleton reg | _ -> VS.empty

(*
   Get all variables used (read) in a basic block
   Returns set of VRegs that are read in the block
   No operand uses
   Also include uses in terminator
*)
let getBlockUses (block : basicBlock) =
  let uses =
    List.fold_left
      (fun uses instruction ->
        match instruction with
        | RuntimeErrorString value -> VS.union uses (getOperandUses value)
        | _ -> VS.union uses (F.getInstrUses instruction))
      VS.empty block.instrs
  in
  VS.union uses (F.getTerminatorUses block.terminator)

let buildDomTree dominators =
  LM.fold
    (fun label parent tree ->
      LM.add parent
        (label :: Option.value ~default:[] (LM.find_opt parent tree))
        tree)
    dominators LM.empty
