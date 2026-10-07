(* ARM64CalleeClobbers.ml - Conservatively summarize ARM64 register writes across direct calls. *)
[@@@warning "-4"]
type writes = {ints:int64;floats:int64}
let intBit reg =
 let index=match reg with
 | LIR.X0 -> 0 | LIR.X1 -> 1 | LIR.X2 -> 2 | LIR.X3 -> 3 | LIR.X4 -> 4 | LIR.X5 -> 5 | LIR.X6 -> 6 | LIR.X7 -> 7
 | LIR.X8 -> 8 | LIR.X9 -> 9 | LIR.X10 -> 10 | LIR.X11 -> 11 | LIR.X12 -> 12 | LIR.X13 -> 13 | LIR.X14 -> 14 | LIR.X15 -> 15
 | LIR.X16 -> 16 | LIR.X17 -> 17 | LIR.X19 -> 18 | LIR.X20 -> 19 | LIR.X21 -> 20 | LIR.X22 -> 21 | LIR.X23 -> 22 | LIR.X24 -> 23
 | LIR.X25 -> 24 | LIR.X26 -> 25 | LIR.X27 -> 26 | LIR.X29 -> 27 | LIR.X30 -> 28 | LIR.SP -> 29 in
 Int64.shift_left 1L index
let floatBit reg = Int64.shift_left 1L (FloatAllocation.physFPRegToInt reg)
let ofInts regs = List.fold_left (fun mask reg -> Int64.logor mask (intBit reg)) 0L regs
let ofFloats regs = List.fold_left (fun mask reg -> Int64.logor mask (floatBit reg)) 0L regs
let containsInt reg writes = Int64.logand writes.ints (intBit reg) <> 0L
let containsFloat reg writes = Int64.logand writes.floats (floatBit reg) <> 0L
let empty = {ints=0L;floats=0L}
let all = {ints=65535L;floats=255L}
let union left right = {ints=Int64.logor left.ints right.ints;floats=Int64.logor left.floats right.floats}
let writeInt = function LIR.Physical reg -> {empty with ints=intBit reg} | LIR.Virtual _ -> all
(*
   Reserved D16 return shuttle.
*)
let writeFloat = function LIR.FPhysical reg -> {empty with floats=floatBit reg} | LIR.FVirtual (-1) -> empty | LIR.FVirtual _ -> all
(*
   The modeled operations lower to writes of their stated destinations only.
   Every other opcode retains the complete ABI clobber set, including backend
   expansions with hidden scratch registers or calls to runtime helpers.
   A literal outside FMOV's immediate range is addressed via X9.
   ARM64 emits the call result in X0; a separate LIR Mov writes its local
   destination. Counting Call.dest here would report a write that BL omits.
   Parallel move cycles use reserved X16, outside the saved set.
*)
let instructionWrites calleeWrites instr = match instr with
| LIR.Mov (dest,(LIR.Imm _ | LIR.Reg _ | LIR.StringSymbol _ | LIR.FuncAddr _)) | LIR.LoadFuncAddr (dest,_) -> writeInt dest
| LIR.Mov (dest,LIR.StackSlot offset) -> let baseWrites=writeInt dest in if offset >= -256 && offset <= 255 then baseWrites else union baseWrites {empty with ints=intBit LIR.X10}
| LIR.Store (offset,_) -> if offset >= -256 && offset <= 255 then empty else {empty with ints=intBit LIR.X10}
| LIR.Add (dest,_,(LIR.Reg _ | LIR.Imm _)) | LIR.Sub (dest,_,(LIR.Reg _ | LIR.Imm _)) -> (match instr with LIR.Add (_,_,LIR.Imm n) | LIR.Sub (_,_,LIR.Imm n) when n<0L || n>=4096L -> union (writeInt dest) {empty with ints=intBit LIR.X9} | _ -> writeInt dest)
| LIR.Mul (dest,_,_) | LIR.Sdiv (dest,_,_) | LIR.Udiv (dest,_,_) | LIR.Msub (dest,_,_,_) | LIR.Madd (dest,_,_,_) | LIR.Cset (dest,_) | LIR.Select (dest,_,_,_) | LIR.And (dest,_,_) | LIR.Orr (dest,_,_) | LIR.Eor (dest,_,_) | LIR.Lsl (dest,_,_) | LIR.Lsr (dest,_,_) | LIR.Asr (dest,_,_) | LIR.Lsl_imm (dest,_,_) | LIR.Lsr_imm (dest,_,_) | LIR.Asr_imm (dest,_,_) | LIR.Neg (dest,_) | LIR.Mvn (dest,_) | LIR.Sxtb (dest,_) | LIR.Sxth (dest,_) | LIR.Sxtw (dest,_) | LIR.Uxtb (dest,_) | LIR.Uxth (dest,_) | LIR.Uxtw (dest,_) -> writeInt dest
| LIR.Cmp (_,LIR.Reg _) | LIR.FCmp _ -> empty
| LIR.Cmp (_,LIR.Imm value) -> if value>=0L && value<4096L then empty else {empty with ints=intBit LIR.X9}
| LIR.FLoad (dest,_) -> union (writeFloat dest) {empty with ints=intBit LIR.X9}
| LIR.FSpillLoad (dest,_) -> union (writeFloat dest) {empty with ints=intBit LIR.X10}
| LIR.FSpillStore _ -> {empty with ints=intBit LIR.X10}
| LIR.FMov (dest,_) | LIR.FAdd (dest,_,_) | LIR.FSub (dest,_,_) | LIR.FMul (dest,_,_) | LIR.FMadd (dest,_,_,_) | LIR.FDiv (dest,_,_) | LIR.FNeg (dest,_) | LIR.FAbs (dest,_) | LIR.FSqrt (dest,_) | LIR.Int64ToFloat (dest,_) | LIR.GpToFp (dest,_) -> writeFloat dest
| LIR.FloatToInt64 (dest,_) | LIR.FloatToBits (dest,_) | LIR.FpToGp (dest,_) | LIR.HeapLoad (dest,_,_) -> writeInt dest
| LIR.Call (_,callee,_) | LIR.TailCall (callee,_) -> Option.value (calleeWrites callee) ~default:all
| LIR.ArgMoves moves | LIR.TailArgMoves moves -> if List.exists (fun (_,operand) -> match operand with LIR.Imm _ | LIR.Reg _ | LIR.FuncAddr _ -> false | _ -> true) moves then all else {empty with ints=ofInts (List.map fst moves)}
| LIR.FArgMoves moves -> {empty with floats=ofFloats (List.map fst moves)}
| LIR.SaveRegs _ -> empty
| LIR.RestoreRegs (ints,floats) -> {ints=ofInts ints;floats=ofFloats floats}
| _ -> all
let summarizeFunction calleeWrites (func:LIR.functionDef) = LIR.LabelMap.fold (fun _ (block:LIR.basicBlock) writes -> List.fold_left (fun writes instr -> union writes (instructionWrites calleeWrites instr)) writes block.LIR.instrs) func.LIR.cfg.LIR.blocks empty
let summariesWithKnown known functions =
 let rec converge local =
  let lookup id = match FunctionIdMap.tryFind id local with Some _ as found -> found | None -> FunctionIdMap.tryFind id known in
  let next=List.fold_left (fun acc (func:LIR.functionDef) -> let writes=summarizeFunction lookup func in let old=Option.value (FunctionIdMap.tryFind func.LIR.id acc) ~default:empty in FunctionIdMap.add func.LIR.id (union old writes) acc) local functions in
  if FunctionIdMap.toList next=FunctionIdMap.toList local then local else converge next in
 converge (FunctionIdMap.ofList (List.map (fun (func:LIR.functionDef) -> func.LIR.id,empty) functions))
let summaries functions = summariesWithKnown FunctionIdMap.empty functions
(*
   Saves precede argument setup, so its writes matter too.
*)
let envelopeWrites callees beforeRestore =
 let calls=List.filter_map (function LIR.Call (_,id,_) -> Some id | _ -> None) beforeRestore in
 let safeEnvelope=List.for_all (function LIR.Call _ | LIR.ArgMoves _ | LIR.FArgMoves _ | LIR.FMov (LIR.FVirtual (-1),LIR.FPhysical LIR.D0) -> true | _ -> false) beforeRestore in
 match calls with [_] when safeEnvelope -> Some (List.fold_left (fun writes instr -> union writes (instructionWrites (fun id -> FunctionIdMap.tryFind id callees) instr)) empty beforeRestore) | _ -> None
let rec beforeRestore = function [] -> [],[] | LIR.RestoreRegs _ :: _ as tail -> [],tail | instr::rest -> let before,after=beforeRestore rest in instr::before,after
(*
   Match the liveness snapshots produced for empty call-save placeholders.
   Unknown envelopes carry the full ABI set through allocation.
*)
let callWritesForSaves callees (block:LIR.basicBlock) =
 let rec collect = function
 | LIR.SaveRegs ([],[])::rest -> let before,after=beforeRestore rest in let writes=match after with LIR.RestoreRegs ([],[])::_ -> Option.value (envelopeWrites callees before) ~default:all | _ -> all in writes::collect rest
 | _::rest -> collect rest | [] -> [] in collect block.LIR.instrs
let pruneBlock callees (block:LIR.basicBlock) =
 let rec rewrite = function
 | LIR.SaveRegs (ints,floats)::rest ->
  let before,after=beforeRestore rest in
  (match after with
  | LIR.RestoreRegs (restoreInts,restoreFloats)::tail when ints=restoreInts && floats=restoreFloats ->
   (match envelopeWrites callees before with
   | Some writes -> let keptInts=List.filter (fun reg -> containsInt reg writes) ints in let keptFloats=List.filter (fun reg -> containsFloat reg writes) floats in LIR.SaveRegs (keptInts,keptFloats)::before @ (LIR.RestoreRegs (keptInts,keptFloats)::rewrite tail)
   | None -> LIR.SaveRegs (ints,floats)::before @ (LIR.RestoreRegs (restoreInts,restoreFloats)::rewrite tail))
  | _ -> LIR.SaveRegs (ints,floats)::rewrite rest)
 | instr::rest -> instr::rewrite rest | [] -> [] in
 let rewritten=rewrite block.LIR.instrs in if rewritten=block.LIR.instrs then block else {block with LIR.instrs=rewritten}
module IdSet = Set.Make(struct type t=AST.functionId let compare left right=Int64.unsigned_compare (AST.functionIdValue left) (AST.functionIdValue right) end)
(*
   Reused stdlib variants can appear in the final binary without
   a unique saved summary. Analyze just those bodies, using the
   finalized summaries for every other callee.
*)
let refineWithCache cache knownWrites functions =
 let callees=match knownWrites with None -> summaries functions | Some known -> let missing=List.filter (fun (func:LIR.functionDef) -> not (FunctionIdMap.containsKey func.LIR.id known)) functions in FunctionIdMap.fold (fun writes id value -> FunctionIdMap.add id value writes) known (summariesWithKnown known missing) in
 List.map (fun (func:LIR.functionDef) ->
  let hasSaves=LIR.LabelMap.exists (fun _ (block:LIR.basicBlock) -> List.exists (function LIR.SaveRegs _ -> true | _ -> false) block.LIR.instrs) func.LIR.cfg.LIR.blocks in
  if not hasSaves then func else
  let directIds=LIR.LabelMap.bindings func.LIR.cfg.LIR.blocks |> List.concat_map (fun (_,block) -> List.filter_map (function LIR.Call (_,id,_) -> Some id | _ -> None) block.LIR.instrs) |> IdSet.of_list in
  let relevant=IdSet.fold (fun id writes -> match FunctionIdMap.tryFind id callees with Some value -> FunctionIdMap.add id value writes | None -> writes) directIds FunctionIdMap.empty in
  let generate () = let blocks=LIR.LabelMap.map (pruneBlock relevant) func.LIR.cfg.LIR.blocks in if LIR.LabelMap.equal (=) blocks func.LIR.cfg.LIR.blocks then func else {func with LIR.cfg={func.LIR.cfg with LIR.blocks=blocks}} in
  match cache with Some reuse -> reuse func relevant generate | None -> generate ()) functions
let refine functions = refineWithCache None None functions
