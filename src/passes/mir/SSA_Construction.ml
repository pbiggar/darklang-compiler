(*
   SSA_Construction.fs - SSA Construction Pass
   Converts MIR to SSA (Static Single Assignment) form by:
   1. Computing dominators and dominance frontiers
   2. Inserting phi nodes at join points
   3. Renaming variables so each definition has a unique name
   After SSA construction, every virtual register is defined exactly once.
   This enables powerful optimizations like GVN, SCCP, and easy DCE.
*)
(* SSA_Construction.fs - Convert typed MIR control flow to static single assignment. *)
[@@@warning "-4"]
open MIR
module F = MIROptimizationFacts
module LM = LabelMap
module LS = LabelSet
module VM = VRegMap
module VS = VRegSet
module IS = IntSet
(*
   Predecessors map: for each label, which labels can jump to it
*)
type predecessors = label list LM.t
(*
   Compute immediate dominators in reverse postorder.
   Returns map from label to its immediate dominator
*)
type dominators = label LM.t
(*
   Dominance frontier: blocks where dominance ends
   DF(n) = blocks that n dominates a predecessor of, but not the block itself
*)
type dominanceFrontier = LS.t LM.t
type ssaConstructionTiming = {phase : string; elapsedMs : float}
let add left right = Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))
let timePhase enabled phase reversed action = if not enabled then action (), reversed else let started = (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in let result = action () in result, {phase; elapsedMs = (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. started} :: reversed
let labelName (Label value) = value
let structuralLabel (Label value) = StructuralFormat.format (StructuralFormat.Union ("Label", [StructuralFormat.Text value]))
let structuralReg (VReg value) = "VReg " ^ string_of_int value
let requiredBlock context blocks label = match LM.find_opt label blocks with Some block -> block | None -> Crash.crash ("SSA: Missing CFG block " ^ labelName label ^ " while " ^ context)
(*
   Get successor labels of a block
*)
let getSuccessors (block : basicBlock) = match block.terminator with Ret _ -> [] | Jump label -> [label] | Branch (_, yes, no) -> [yes; no]
(*
   Build predecessors map from CFG
   Add edges from terminator
*)
let buildPredecessors (cfg : cfg) =
 let edge from target predecessors = LM.add target (from :: Option.value ~default:[] (LM.find_opt target predecessors)) predecessors in
 LM.fold (fun label block predecessors -> match block.terminator with Ret _ -> predecessors | Jump target -> edge label target predecessors | Branch (_, yes, no) -> edge label no (edge label yes predecessors)) cfg.blocks LM.empty
(*
   Cooper-Harvey-Kennedy converges quickly when blocks are visited in
   reverse postorder. Keep the DFS stack explicit because generated test
   functions can contain thousands of blocks.
   Mapping entry to itself gives intersect a sentinel root. Remove it from
   the public result after the fixed point settles.
*)
let computeDominators (cfg : cfg) predecessors =
 let entry = cfg.entry in
 let rec order work visited reversed = match work with [] -> reversed | (label, expanded) :: rest -> if expanded then order rest visited (label :: reversed) else if LS.mem label visited then order rest visited reversed else let successors = match LM.find_opt label cfg.blocks with None -> [] | Some block -> getSuccessors block in order (List.map (fun label -> label, false) successors @ ((label, true) :: rest)) (LS.add label visited) reversed in
 let reversePostorder = order [entry, false] LS.empty [] |> List.filter (fun label -> LM.mem label cfg.blocks) in
 let positions = LM.of_list (List.mapi (fun index label -> label, index) reversePostorder) in
 let position label = match LM.find_opt label positions with Some position -> position | None -> Crash.crash ("SSA: Missing reverse-postorder position for " ^ structuralLabel label) in
 let parent dominators label = match LM.find_opt label dominators with Some parent -> parent | None -> Crash.crash ("SSA: Missing immediate dominator for " ^ structuralLabel label) in
 let rec intersect dominators left right = if left = right then left else if position left > position right then intersect dominators (parent dominators left) right else intersect dominators left (parent dominators right) in
 let rec converge labels dominators =
  let changed, updated = List.fold_left (fun (changed, current) label -> let processed = Option.value ~default:[] (LM.find_opt label predecessors) |> List.filter (fun label -> LM.mem label current) in match processed with [] -> changed, current | first :: rest -> let immediate = List.fold_left (intersect current) first rest in match LM.find_opt label current with Some old when old = immediate -> changed, current | _ -> true, LM.add label immediate current) (false, dominators) labels in if changed then converge labels updated else updated in
 match reversePostorder with [] -> LM.empty | _ :: labels -> LM.remove entry (converge labels (LM.singleton entry entry))
type labelIndex = {labels : label array; indexOf : int LM.t}
let buildLabelIndex (cfg : cfg) = let labels = Array.of_list (List.map fst (LM.bindings cfg.blocks)) in {labels; indexOf = LM.of_list (Array.to_list (Array.mapi (fun index label -> label, index) labels))}
(*
   For each block b, for each predecessor p of b:
   Walk up the dominator tree from p until we reach idom(b)
   All blocks on this path have b in their dominance frontier
*)
let computeDominanceFrontier cfg predecessors dominators =
 let index = buildLabelIndex cfg in let count = Array.length index.labels in
 let parents = Array.map (fun label -> Option.bind (LM.find_opt label dominators) (fun parent -> LM.find_opt parent index.indexOf)) index.labels in
 let frontiers = Array.init count (fun _ -> Bitset.empty (Bitset.wordCount count)) in
 Array.iteri (fun block label -> List.iter (fun predecessor -> match LM.find_opt predecessor index.indexOf with None -> () | Some predecessor -> let rec walk current = match parents.(block) with Some parent when current = parent -> () | _ -> Bitset.addIndexInPlace block frontiers.(current); match parents.(current) with Some parent when parent <> current -> walk parent | _ -> () in walk predecessor) (Option.value ~default:[] (LM.find_opt label predecessors))) index.labels;
 LM.of_list (Array.to_list (Array.mapi (fun block label -> label, LS.of_list (List.map (Array.get index.labels) (Bitset.indicesToList frontiers.(block)))) index.labels))
(*
   Get all variable definitions in a basic block
   Returns set of VRegs that are defined (written to) in the block
   Tail calls have no destination
   Indirect tail calls have no destination
   Closure tail calls have no destination
   No destination register
*)
let getBlockDefs (block : basicBlock) = List.fold_left (fun defs instruction -> match F.getInstrDest instruction with None -> defs | Some dest -> VS.add dest defs) VS.empty block.instrs
(*
   Get all variables defined anywhere in the CFG
*)
let getAllDefs (cfg : cfg) = LM.fold (fun label block defs -> VS.fold (fun reg defs -> VM.add reg (LS.add label (Option.value ~default:LS.empty (VM.find_opt reg defs))) defs) (getBlockDefs block) defs) cfg.blocks VM.empty
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
let getBlockUses (block : basicBlock) = let uses = List.fold_left (fun uses instruction -> match instruction with RuntimeErrorString value -> VS.union uses (getOperandUses value) | _ -> VS.union uses (F.getInstrUses instruction)) VS.empty block.instrs in VS.union uses (F.getTerminatorUses block.terminator)
type vRegIndex = {vRegs : vReg array; regIndex : int VM.t; wordCount : int}
let buildVRegIndex registers = let vRegs = Array.of_list (VS.elements registers) in {vRegs; regIndex = VM.of_list (Array.to_list (Array.mapi (fun index reg -> reg, index) vRegs)); wordCount = Bitset.wordCount (Array.length vRegs)}
(*
   Compute liveness information for the CFG
   Returns (liveIn, liveOut) maps from Label to Set<VReg>
   A variable is live-in at a block if it may be used before being defined
   A variable is live-out at a block if it's live-in at any successor
   Liveness flows from successors to predecessors. Postorder solves an
   acyclic region in one pass, while the surrounding fixed point retains
   exact behavior for loop backedges. The explicit work stack keeps CFG
   ordering stack-safe for large generated functions.
   Within each round, predecessors see successor values computed earlier in
   the same postorder traversal instead of waiting for another global round.
*)
let computeLivenessForVRegs tracked includeOut (cfg : cfg) =
 let labels = buildLabelIndex cfg in let count = Array.length labels.labels in
 let usesAndDefs = Array.map (fun label -> let block = requiredBlock "precomputing liveness" cfg.blocks label in getBlockUses block, getBlockDefs block) labels.labels in
 let registers = buildVRegIndex (match tracked with Some values -> values | None -> Array.fold_left (fun all (uses, defs) -> VS.union (VS.union all uses) defs) VS.empty usesAndDefs) in
 let bits values = let bits = Bitset.empty registers.wordCount in VS.iter (fun reg -> match VM.find_opt reg registers.regIndex with Some index -> Bitset.addIndexInPlace index bits | None -> ()) values; bits in
 let blockUses = Array.map (fun (uses, _) -> bits uses) usesAndDefs and blockDefs = Array.map (fun (_, defs) -> bits defs) usesAndDefs in
 let successors = Array.map (fun label -> let block = requiredBlock "computing liveness for block" cfg.blocks label in List.map (fun successor -> let _ = requiredBlock "computing liveness successor" cfg.blocks successor in match LM.find_opt successor labels.indexOf with Some index -> index | None -> Crash.crash ("SSA: Missing label index for " ^ labelName successor ^ " while computing liveness successor")) (getSuccessors block)) labels.labels in
 let entry = match LM.find_opt cfg.entry labels.indexOf with Some entry -> entry | None -> Crash.crash "SSA: Missing entry label index while ordering liveness" in
 let rec order work visited reversed = match work with [] -> List.rev reversed | (block, expanded) :: rest -> if expanded then order rest visited (block :: reversed) else if IS.mem block visited then order rest visited reversed else order (List.map (fun index -> index, false) successors.(block) @ ((block, true) :: rest)) (IS.add block visited) reversed in
 let ordering = order (List.map (fun block -> block, false) (entry :: List.init count Fun.id)) IS.empty [] in
 let empty = Bitset.empty registers.wordCount in let liveIn = Array.init count (fun _ -> empty) in
 let liveOut index = Array.init registers.wordCount (fun word -> List.fold_left (fun value successor -> Int64.logor value liveIn.(successor).(word)) 0L successors.(index)) in
 let live index = let out = liveOut index in Array.init registers.wordCount (fun word -> Int64.logor blockUses.(index).(word) (Int64.logand out.(word) (Int64.lognot blockDefs.(index).(word)))) in
 let rec converge () = let changed = ref false in List.iter (fun index -> let next = live index in if not (Bitset.equal liveIn.(index) next) then (liveIn.(index) <- next; changed := true)) ordering; if !changed then converge () in converge ();
 let toMap values = LM.of_list (Array.to_list (Array.mapi (fun index label -> label, VS.of_list (List.map (Array.get registers.vRegs) (Bitset.indicesToList values.(index)))) labels.labels)) in
 toMap (Array.copy liveIn), (if includeOut then toMap (Array.init count liveOut) else LM.empty)
let computeLiveness cfg = computeLivenessForVRegs None true cfg
type phiTypeEvidence = KnownPhiType of AST.semanticType | ConflictingPhiTypes
(*
   Insert phi nodes at dominance frontiers
   For each variable v defined in block b:
   For each block d in DF(b):
   Insert phi node for v in d (if not already present AND v is live-in at d)
   This also counts as a definition, so recursively process
   Create a map from parameter VReg to its type
   IfValue lowering defines both incoming arms with typed moves. Preserve
   that type on the SSA phi so floating-point joins stay in FP registers.
   Get all definitions from instructions in the CFG
   Add function parameters as definitions at the entry block
   This is critical for self-recursive functions: params are defined at entry (from args)
   AND re-defined in recursive blocks (before jumping back). SSA needs both definition
   sites to insert phi nodes at the loop header (the entry block for such functions).
   Worklist algorithm: for each variable, propagate phi insertion
   Get dominance frontier of this block
   For each block in the frontier, insert phi if not already there AND variable is live
   Already has phi for this var
   Only insert phi if variable is live-in at this block
   Variable not live here, skip phi
   Insert phi node
   Create phi with placeholder sources (will be renamed later)
   Add to block (at the beginning)
   Add to worklist (phi is a definition, may need more phis)
   Process all variables
*)
let insertPhiNodes (cfg : cfg) frontier predecessors liveIn parameters types =
 let parameterTypes = VM.of_list (List.combine parameters types) in
 let localTypes = LM.fold (fun _ block types -> List.fold_left (fun types instruction -> match instruction with Mov (dest, _, Some typ) -> (match VM.find_opt dest types with None -> VM.add dest (KnownPhiType typ) types | Some (KnownPhiType old) when old = typ -> types | Some _ -> VM.add dest ConflictingPhiTypes types) | _ -> types) types block.instrs) cfg.blocks VM.empty in
 let definitions = List.fold_left (fun defs reg -> VM.add reg (LS.add cfg.entry (Option.value ~default:LS.empty (VM.find_opt reg defs))) defs) (getAllDefs cfg) parameters in
 let rec insert reg work phis cfg = match LS.min_elt_opt work with None -> cfg | Some block ->
  let work, phis, cfg = LS.fold (fun target (work, phis, cfg) -> if LS.mem target phis || not (VS.mem reg (Option.value ~default:VS.empty (LM.find_opt target liveIn))) then work, phis, cfg else
    let sources = List.map (fun predecessor -> Register reg, predecessor) (Option.value ~default:[] (LM.find_opt target predecessors)) in
    let typ = match VM.find_opt reg parameterTypes with Some typ -> Some typ | None -> (match VM.find_opt reg localTypes with Some (KnownPhiType typ) -> Some typ | Some ConflictingPhiTypes | None -> None) in
    let block = requiredBlock "inserting phi node" cfg.blocks target in let cfg = {cfg with blocks = LM.add target {block with instrs = Phi (reg, sources, typ) :: block.instrs} cfg.blocks} in LS.add target work, LS.add target phis, cfg) (Option.value ~default:LS.empty (LM.find_opt block frontier)) (LS.remove block work, phis, cfg) in insert reg work phis cfg in
 VM.fold (fun reg sites cfg -> insert reg sites LS.empty cfg) definitions cfg
(*
   Rename variables to SSA form
   Each definition gets a fresh version number
   Uses dominator tree traversal to maintain scoping
   Version stacks for original VRegs, mutated during the dominator walk.
   Original VRegs in push order, used to restore the exact scope depth.
   Next available version number
   Original floatRegs set (VReg IDs that are floats)
   Updated floatRegs set (includes SSA renamed VRegs)
*)
type renamingState = {versionStacks : (vReg, int Stack.t) Hashtbl.t; pushedVersions : vReg Stack.t; mutable nextVersion : int; originalFloatRegs : IS.t; floatRegs : IS.t ref}
(*
   Create initial renaming state, starting VReg numbers above any existing VRegs
   Preserve the existing numbering scheme of starting 10000 above CFG
   definitions, while also staying above parameter registers that are not
   materialized as MIR definitions.
*)
let createInitialRenamingState (cfg : cfg) floats extra =
 let definitions = LM.fold (fun _ block regs -> VS.union regs (getBlockDefs block)) cfg.blocks VS.empty in
 let cfgMax = VS.fold (fun (VReg id) largest -> max largest id) definitions 0 in
 let extraMax = List.fold_left (fun largest (VReg id) -> max largest id) 0 extra in
 {versionStacks = Hashtbl.create 16; pushedVersions = Stack.create (); nextVersion = max (add cfgMax 10000) (add extraMax 1); originalFloatRegs = floats; floatRegs = ref floats}
(*
   Create new version for a definition
   If the original VReg was a float, the new SSA version is also a float
*)
let newVersion state ((VReg original) as reg) = let version = state.nextVersion in let stack = match Hashtbl.find_opt state.versionStacks reg with Some stack -> stack | None -> let stack = Stack.create () in Hashtbl.add state.versionStacks reg stack; stack in Stack.push version stack; Stack.push reg state.pushedVersions; if IS.mem original state.originalFloatRegs then state.floatRegs := IS.add version !(state.floatRegs); state.nextVersion <- add version 1; version, VReg version, state
(*
   Get the renamed VReg for a use
*)
let getRenamedReg state reg = match Hashtbl.find_opt state.versionStacks reg with Some stack when not (Stack.is_empty stack) -> VReg (Stack.top stack) | Some _ -> reg | None -> reg
(*
   Rename operand
*)
let renameOperand state = function Register reg -> Register (getRenamedReg state reg) | value -> value
(*
   Rename instruction (uses and defs)
   No dest
   addr is used, not defined
   Phi sources are renamed when processing predecessors
   Here we just rename the destination
   No registers to rename
*)
let renameInstr state (instruction : instr) = match instruction with
| Mov (dest, src, vt) ->
let src' = renameOperand state src in
let (_, newDest, state') = newVersion state dest in
(Mov (newDest, src', vt), state')
| BinOp (dest, op, left, right, opType) ->
let left' = renameOperand state left in
let right' = renameOperand state right in
let (_, newDest, state') = newVersion state dest in
(BinOp (newDest, op, left', right', opType), state')
| UnaryOp (dest, op, src) ->
let src' = renameOperand state src in
let (_, newDest, state') = newVersion state dest in
(UnaryOp (newDest, op, src'), state')
| Call (dest, funcName, args, argTypes, returnType) ->
let args' = List.map (renameOperand state) args in
let (_, newDest, state') = newVersion state dest in
(Call (newDest, funcName, args', argTypes, returnType), state')
| TailCall (funcName, args, argTypes, returnType) ->
let args' = List.map (renameOperand state) args in
(TailCall (funcName, args', argTypes, returnType), state)
| IndirectCall (dest, func, args, argTypes, returnType) ->
let func' = renameOperand state func in
let args' = List.map (renameOperand state) args in
let (_, newDest, state') = newVersion state dest in
(IndirectCall (newDest, func', args', argTypes, returnType), state')
| IndirectTailCall (func, args, argTypes, returnType) ->
let func' = renameOperand state func in
let args' = List.map (renameOperand state) args in
(IndirectTailCall (func', args', argTypes, returnType), state)
| ClosureAlloc (dest, funcName, captures) ->
let captures' = List.map (renameOperand state) captures in
let (_, newDest, state') = newVersion state dest in
(ClosureAlloc (newDest, funcName, captures'), state')
| ClosureCall (dest, closure, args, argTypes, returnType) ->
let closure' = renameOperand state closure in
let args' = List.map (renameOperand state) args in
let (_, newDest, state') = newVersion state dest in
(ClosureCall (newDest, closure', args', argTypes, returnType), state')
| ClosureTailCall (closure, args, argTypes) ->
let closure' = renameOperand state closure in
let args' = List.map (renameOperand state) args in
(ClosureTailCall (closure', args', argTypes), state)
| HeapAlloc (dest, size) ->
let (_, newDest, state') = newVersion state dest in
(HeapAlloc (newDest, size), state')
| HeapStore (addr, offset, src, vt) ->
let src' = renameOperand state src in
let addr' = getRenamedReg state addr in
(HeapStore (addr', offset, src', vt), state)
| HeapLoad (dest, addr, offset, vt) ->
let addr' = getRenamedReg state addr in
let (_, newDest, state') = newVersion state dest in
(HeapLoad (newDest, addr', offset, vt), state')
| StringConcat (dest, first, second, remaining) ->
let first' = renameOperand state first in
let second' = renameOperand state second in
let remaining' = List.map (renameOperand state) remaining in
let (_, newDest, state') = newVersion state dest in
(StringConcat (newDest, first', second', remaining'), state')
| CanonicalBufferEq (dest, kind, left, right) ->
let left' = renameOperand state left in
let right' = renameOperand state right in
let (_, newDest, state') = newVersion state dest in
(CanonicalBufferEq (newDest, kind, left', right'), state')
| RefCountInc (addr, size, kind, sourceType) ->
let addr' = getRenamedReg state addr in
(RefCountInc (addr', size, kind, sourceType), state)
| RefCountDec (addr, size, kind, sourceType) ->
let addr' = getRenamedReg state addr in
(RefCountDec (addr', size, kind, sourceType), state)
| Print (src, vt) ->
let src' = renameOperand state src in
(Print (src', vt), state)
| StdoutWrite (effectId, src, appendNewline) ->
(StdoutWrite (effectId, renameOperand state src, appendNewline), state)
| StdinReadLine dest ->
let (_, newDest, state') = newVersion state dest in
(StdinReadLine newDest, state')
| FileReadBlob (dest, path) ->
let path' = renameOperand state path in
let (_, newDest, state') = newVersion state dest in
(FileReadBlob (newDest, path'), state')
| FileExists (dest, path) ->
let path' = renameOperand state path in
let (_, newDest, state') = newVersion state dest in
(FileExists (newDest, path'), state')
| FileWriteBlob (dest, path, content) ->
let path' = renameOperand state path in
let content' = renameOperand state content in
let (_, newDest, state') = newVersion state dest in
(FileWriteBlob (newDest, path', content'), state')
| FileAppendText (dest, path, content) ->
let path' = renameOperand state path in
let content' = renameOperand state content in
let (_, newDest, state') = newVersion state dest in
(FileAppendText (newDest, path', content'), state')
| FileDelete (dest, path) ->
let path' = renameOperand state path in
let (_, newDest, state') = newVersion state dest in
(FileDelete (newDest, path'), state')
| FileCreateDirectory (dest, path) ->
let path' = renameOperand state path in
let (_, newDest, state') = newVersion state dest in
(FileCreateDirectory (newDest, path'), state')
| FileSetExecutable (dest, path) ->
let path' = renameOperand state path in
let (_, newDest, state') = newVersion state dest in
(FileSetExecutable (newDest, path'), state')
| FileWriteFromPtr (dest, path, ptr, length) ->
let path' = renameOperand state path in
let ptr' = renameOperand state ptr in
let length' = renameOperand state length in
let (_, newDest, state') = newVersion state dest in
(FileWriteFromPtr (newDest, path', ptr', length'), state')
| Phi (dest, sources, valueType) ->
let (_, newDest, state') = newVersion state dest in
(Phi (newDest, sources, valueType), state')
| RawAlloc (dest, numBytes) ->
let numBytes' = renameOperand state numBytes in
let (_, newDest, state') = newVersion state dest in
(RawAlloc (newDest, numBytes'), state')
| MappedAlloc (dest, numBytes) ->
let numBytes' = renameOperand state numBytes in
let (_, newDest, state') = newVersion state dest in
(MappedAlloc (newDest, numBytes'), state')
| RawFree ptr ->
let ptr' = renameOperand state ptr in
(RawFree ptr', state)
| MappedFree ptr ->
let ptr' = renameOperand state ptr in
(MappedFree ptr', state)
| RawGet (dest, ptr, byteOffset, valueType) ->
let ptr' = renameOperand state ptr in
let byteOffset' = renameOperand state byteOffset in
let (_, newDest, state') = newVersion state dest in
(RawGet (newDest, ptr', byteOffset', valueType), state')
| RawGetByte (dest, ptr, byteOffset) ->
let ptr' = renameOperand state ptr in
let byteOffset' = renameOperand state byteOffset in
let (_, newDest, state') = newVersion state dest in
(RawGetByte (newDest, ptr', byteOffset'), state')
| StringToRawPtr (dest, value) ->
let value' = renameOperand state value in
let (_, newDest, state') = newVersion state dest in
(StringToRawPtr (newDest, value'), state')
| RawPtrToString (dest, ptr) ->
let ptr' = renameOperand state ptr in
let (_, newDest, state') = newVersion state dest in
(RawPtrToString (newDest, ptr'), state')
| BlobToRawPtr (dest, value) ->
let value' = renameOperand state value in
let (_, newDest, state') = newVersion state dest in
(BlobToRawPtr (newDest, value'), state')
| RawPtrToBlob (dest, ptr) ->
let ptr' = renameOperand state ptr in
let (_, newDest, state') = newVersion state dest in
(RawPtrToBlob (newDest, ptr'), state')
| DictToRawPtr (dest, dict) ->
let dict' = renameOperand state dict in
let (_, newDest, state') = newVersion state dest in
(DictToRawPtr (newDest, dict'), state')
| RawPtrToDict (dest, ptr, tag) ->
let ptr' = renameOperand state ptr in
let tag' = renameOperand state tag in
let (_, newDest, state') = newVersion state dest in
(RawPtrToDict (newDest, ptr', tag'), state')
| ListToRawPtr (dest, list) ->
let list' = renameOperand state list in
let (_, newDest, state') = newVersion state dest in
(ListToRawPtr (newDest, list'), state')
| RawPtrToList (dest, ptr, tag) ->
let ptr' = renameOperand state ptr in
let tag' = renameOperand state tag in
let (_, newDest, state') = newVersion state dest in
(RawPtrToList (newDest, ptr', tag'), state')
| RawWriteWord (ptr, byteOffset, value) ->
let ptr' = renameOperand state ptr in
let byteOffset' = renameOperand state byteOffset in
let value' = renameOperand state value in
(RawWriteWord (ptr', byteOffset', value'), state)
| RawWriteByte (ptr, byteOffset, value) ->
let ptr' = renameOperand state ptr in
let byteOffset' = renameOperand state byteOffset in
let value' = renameOperand state value in
(RawWriteByte (ptr', byteOffset', value'), state)
| RawSlotInit (ptr, byteOffset, value, valueType) ->
let ptr' = renameOperand state ptr in
let byteOffset' = renameOperand state byteOffset in
let value' = renameOperand state value in
(RawSlotInit (ptr', byteOffset', value', valueType), state)
| FloatSqrt (dest, src) ->
let src' = renameOperand state src in
let (_, newDest, state') = newVersion state dest in
(FloatSqrt (newDest, src'), state')
| FloatAbs (dest, src) ->
let src' = renameOperand state src in
let (_, newDest, state') = newVersion state dest in
(FloatAbs (newDest, src'), state')
| FloatNeg (dest, src) ->
let src' = renameOperand state src in
let (_, newDest, state') = newVersion state dest in
(FloatNeg (newDest, src'), state')
| Int64ToFloat (dest, src) ->
let src' = renameOperand state src in
let (_, newDest, state') = newVersion state dest in
(Int64ToFloat (newDest, src'), state')
| FloatToInt64 (dest, src) ->
let src' = renameOperand state src in
let (_, newDest, state') = newVersion state dest in
(FloatToInt64 (newDest, src'), state')
| FloatToBits (dest, src) ->
let src' = renameOperand state src in
let (_, newDest, state') = newVersion state dest in
(FloatToBits (newDest, src'), state')
| RefCountIncString str ->
let str' = renameOperand state str in
(RefCountIncString str', state)
| RefCountDecString str ->
let str' = renameOperand state str in
(RefCountDecString str', state)
| RefCountIncBlob bytes ->
let bytes' = renameOperand state bytes in
(RefCountIncBlob bytes', state)
| RefCountDecBlob bytes ->
let bytes' = renameOperand state bytes in
(RefCountDecBlob bytes', state)
| RefCountIncInt value ->
let value' = renameOperand state value in
(RefCountIncInt value', state)
| RefCountDecInt value ->
let value' = renameOperand state value in
(RefCountDecInt value', state)
| RandomInt64 dest ->
let (_, newDest, state') = newVersion state dest in
(RandomInt64 newDest, state')
| DateTimeNow dest ->
let (_, newDest, state') = newVersion state dest in
(DateTimeNow newDest, state')
| Sleep (effectId, dest, delayMs) ->
let delayMs' = renameOperand state delayMs in
let (_, newDest, state') = newVersion state dest in
(Sleep (effectId, newDest, delayMs'), state')
| CliNative (dest, operation, args) ->
let args' = List.map (renameOperand state) args in
let (_, newDest, state') = newVersion state dest in
(CliNative (newDest, operation, args'), state')
| FloatToString (dest, value) ->
let value' = renameOperand state value in
let (_, newDest, state') = newVersion state dest in
(FloatToString (newDest, value'), state')
| RuntimeError message ->
(RuntimeError message, state)
| RuntimeErrorString message ->
(RuntimeErrorString (renameOperand state message), state)
| CoverageHit exprId ->
(CoverageHit exprId, state)
(*
   Rename terminator
*)
let renameTerminator state (terminator : terminator) = match terminator with Ret value -> Ret (renameOperand state value) | Branch (value, yes, no) -> Branch (renameOperand state value, yes, no) | Jump label -> Jump label
(*
   Rename a basic block
   Rename all instructions
   Rename terminator
*)
let renameBlock state (block : basicBlock) = let instrs, state = List.fold_left (fun (instrs, state) instruction -> let instruction, state = renameInstr state instruction in instruction :: instrs, state) ([], state) block.instrs in {block with instrs = List.rev instrs; terminator = renameTerminator state block.terminator}, state
module PhiUpdateMap = Map.Make (struct type t = label * label * vReg let compare (Label left, Label from, VReg reg) (Label right, Label source, VReg other) = let result = StringOrder.compare left right in if result <> 0 then result else let result = StringOrder.compare from source in if result <> 0 then result else Int.compare reg other end)
type phiSourceUpdates = operand PhiUpdateMap.t
(*
   Apply deferred predecessor-specific source versions without disturbing the
   instruction or predecessor order established during phi insertion.
*)
let applyPhiSourceUpdates updates (block : basicBlock) =
 let rec leading = function Phi (dest, sources, typ) :: rest -> let sources = List.map (fun (source, from) -> match source with Register reg -> Option.value ~default:source (PhiUpdateMap.find_opt (block.label, from, reg) updates), from | _ -> source, from) sources in Phi (dest, sources, typ) :: leading rest | rest -> rest in {block with instrs = leading block.instrs}
(*
   Record the renamed values supplied by one predecessor. Applying these
   records after the dominator walk avoids rebuilding successor blocks and the
   persistent CFG map once per incoming edge.
   Get successor labels from terminator
   Each source is keyed by successor, predecessor, and original register.
   Phi insertion creates register sources; non-register sources are retained
   unchanged by applyPhiSourceUpdates.
*)
let collectPhiSourceUpdatesForSuccessors (cfg : cfg) current state =
 let block = requiredBlock "updating successor phi sources" cfg.blocks current in
 List.concat_map (fun successor -> let block = requiredBlock "collecting successor phi sources" cfg.blocks successor in
  let rec leading instructions updates = match instructions with Phi (_, sources, _) :: rest -> let updates = List.fold_left (fun updates (source, from) -> match source with Register reg when from = current -> ((successor, current, reg), renameOperand state source) :: updates | _ -> updates) updates sources in leading rest updates | _ -> List.rev updates in leading block.instrs []) (getSuccessors block)
(*
   Build dominator tree children
*)
let buildDomTree dominators = LM.fold (fun label parent tree -> LM.add parent (label :: Option.value ~default:[] (LM.find_opt parent tree)) tree) dominators LM.empty
(*
   Restore the version stacks to a dominator scope boundary.
*)
let popVersionsToDepth state depth =
 while Stack.length state.pushedVersions > depth do
  let reg = Stack.pop state.pushedVersions in
  match Hashtbl.find_opt state.versionStacks reg with Some stack when not (Stack.is_empty stack) -> ignore (Stack.pop stack) | _ -> Crash.crash ("SSA: Missing version stack while restoring " ^ structuralReg reg)
 done
(*
   Rename CFG using dominator tree traversal
   Rename CFG to SSA form
   Returns (renamed CFG, updated floatRegs set with SSA versions)
   DFS traversal of dominator tree
   Renamed blocks and phi updates are accumulated as lists and materialized
   once after traversal, while state carries NextVersion across siblings.
   Rename this block
   Child visits restore their own pushes before returning, leaving this
   block's versions visible to every dominated sibling.
   Start from entry with initial state based on CFG's existing VRegs
*)
let renameCFG (cfg : cfg) dominators floats parameters =
 let tree = buildDomTree dominators in
 let rec visit label state blocks updates =
  let block = requiredBlock "renaming CFG block" cfg.blocks label in let depth = Stack.length state.pushedVersions in
  let block, state = renameBlock state block in let blocks = (label, block) :: blocks in
  let updates = List.fold_left (fun updates update -> update :: updates) updates (collectPhiSourceUpdatesForSuccessors cfg label state) in
  let blocks, updates, state = List.fold_left (fun (blocks, updates, state) child -> visit child state blocks updates) (blocks, updates, state) (Option.value ~default:[] (LM.find_opt label tree)) in
  popVersionsToDepth state depth; blocks, updates, state in
 let initial = createInitialRenamingState cfg floats parameters in let blocks, updates, state = visit cfg.entry initial [] [] in
 let renamed = LM.of_list blocks and updates = PhiUpdateMap.of_list updates in
 let blocks = LM.mapi (fun label original -> applyPhiSourceUpdates updates (Option.value ~default:original (LM.find_opt label renamed))) cfg.blocks in {cfg with blocks}, !(state.floatRegs)
(*
   Convert a function to SSA form
   Phi placement only asks whether candidate variables are live. Liveness
   is independent per variable, so unrelated single-definition temporaries
   need not widen every block bitset.
   Insert phi nodes (only for live variables)
   Pass function params so they're treated as defined at entry (for self-recursive functions)
   Rename variables and update floatRegs with SSA versions
*)
let convertFunctionToSSAInternal timed (func : functionDef) =
 let cfg = func.cfg in
 let predecessors, timings = timePhase timed "SSA: Predecessors" [] (fun () -> buildPredecessors cfg) in
 let dominators, timings = timePhase timed "SSA: Dominators" timings (fun () -> computeDominators cfg predecessors) in
 let frontier, timings = timePhase timed "SSA: Dominance Frontier" timings (fun () -> computeDominanceFrontier cfg predecessors dominators) in
 let parameters = List.map (fun (param : typedMIRParam) -> param.reg) func.typedParams and types = List.map (fun (param : typedMIRParam) -> param.typ) func.typedParams in
 let definitions = List.fold_left (fun defs reg -> VM.add reg (LS.add cfg.entry (Option.value ~default:LS.empty (VM.find_opt reg defs))) defs) (getAllDefs cfg) parameters in
 let candidates = VM.fold (fun reg sites candidates -> if LS.exists (fun site -> Option.fold ~none:false ~some:(fun frontier -> not (LS.is_empty frontier)) (LM.find_opt site frontier)) sites then VS.add reg candidates else candidates) definitions VS.empty in
 let (liveIn, _), timings = timePhase timed "SSA: Liveness" timings (fun () -> if VS.is_empty candidates then LM.empty, LM.empty else computeLivenessForVRegs (Some candidates) false cfg) in
 let withPhis, timings = timePhase timed "SSA: Phi Insertion" timings (fun () -> insertPhiNodes cfg frontier predecessors liveIn parameters types) in
 let (cfg, floatRegs), timings = timePhase timed "SSA: Renaming" timings (fun () -> renameCFG withPhis dominators func.floatRegs parameters) in
 {func with cfg; floatRegs}, List.rev timings
(*
   Convert a function to SSA form.
*)
let convertFunctionToSSA func = fst (convertFunctionToSSAInternal false func)
(*
   Convert one function to SSA form and retain its nested phase timings.
*)
let convertFunctionToSSAWithTiming func = convertFunctionToSSAInternal true func
(*
   Convert a program to SSA form
*)
let convertToSSA (Program (functions, variants, records)) = Program (List.map convertFunctionToSSA functions, variants, records)
(*
   Convert a program to SSA form and collect aggregate phase timings.
*)
let convertToSSAWithTiming (Program (functions, variants, records)) =
 let functions, timings = List.fold_left (fun (converted, collected) func -> let func, timings = convertFunctionToSSAInternal true func in func :: converted, List.fold_left (fun collected timing -> timing :: collected) collected timings) ([], []) functions in Program (List.rev functions, variants, records), List.rev timings
