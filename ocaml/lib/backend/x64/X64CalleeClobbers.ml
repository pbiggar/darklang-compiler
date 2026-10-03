(* CalleeClobbers.fs - Conservatively summarize x64 caller-register writes. *)
[@@@warning "-4"]
open ARM64CalleeClobbers
type writes = ARM64CalleeClobbers.writes
let empty : writes = {ints=0L;floats=0L}
let all : writes = {ints=65535L;floats=16383L}
let union left right : writes = {ints=Int64.logor left.ints right.ints;floats=Int64.logor left.floats right.floats}
let intWrite = function LIR.Physical reg -> {empty with ints=intBit reg} | LIR.Virtual _ -> all
let floatWrite = function LIR.FPhysical reg -> {empty with floats=floatBit reg} | LIR.FVirtual (-1) -> empty | LIR.FVirtual _ -> all
(*
   Only x64 instructions whose backend expansion has no hidden scratch write
   receive a narrow summary. Every other opcode retains the full ABI set.
*)
let instructionWrites calleeWrites = function
| LIR.Mov (dest,(LIR.Imm _ | LIR.Reg _ | LIR.FuncAddr _)) -> intWrite dest
| LIR.FMov (dest,_) -> floatWrite dest
| LIR.Call (dest,callee,_) -> union (Option.value (calleeWrites callee) ~default:all) (intWrite dest)
| LIR.TailCall (callee,_) -> Option.value (calleeWrites callee) ~default:all
| LIR.SaveRegs _ -> empty
| LIR.RestoreRegs (ints,floats) -> {ints=ofInts ints;floats=ofFloats floats}
| _ -> all
(*
   Return shuttles may be emitted after symbolic LIR and use these ABI
   result registers even for an otherwise empty function.
*)
let summarizeFunction calleeWrites (func:LIR.functionDef) =
 let writes=LIR.LabelMap.fold (fun _ (block:LIR.basicBlock) writes -> List.fold_left (fun writes instr -> union writes (instructionWrites calleeWrites instr)) writes block.LIR.instrs) func.LIR.cfg.LIR.blocks empty in
 union writes {ints=intBit LIR.X0;floats=floatBit LIR.D0}
let summariesWithKnown known functions =
 let rec converge local =
  let lookup id=match FunctionIdMap.tryFind id local with Some _ as found -> found | None -> FunctionIdMap.tryFind id known in
  let next=List.fold_left (fun acc (func:LIR.functionDef) -> let writes=summarizeFunction lookup func in let old=Option.value (FunctionIdMap.tryFind func.LIR.id acc) ~default:empty in FunctionIdMap.add func.LIR.id (union old writes) acc) local functions in
  if FunctionIdMap.toList next=FunctionIdMap.toList local then local else converge next in
 converge (FunctionIdMap.ofList (List.map (fun (func:LIR.functionDef) -> func.LIR.id,empty) functions))
(*
   Calls with argument setup or an unrecognized save envelope retain the full
   ABI clobber set. Narrow envelopes currently cover zero-argument calls.
*)
let callWritesForSaves callees (block:LIR.basicBlock) =
 let rec beforeRestore = function [] -> [],[] | LIR.RestoreRegs _ :: _ as tail -> [],tail | instr::rest -> let before,after=beforeRestore rest in instr::before,after in
 let rec collect = function
 | LIR.SaveRegs ([],[])::rest -> let before,after=beforeRestore rest in let writes=match before,after with [LIR.Call (_,callee,[])],LIR.RestoreRegs ([],[])::_ -> Option.value (FunctionIdMap.tryFind callee callees) ~default:all | _ -> all in writes::collect rest
 | _::rest -> collect rest | [] -> [] in collect block.LIR.instrs
(*
   Preserve the original stack parity at the call site. The
   backend emits each saved GP or FP register as eight bytes.
*)
let pruneFunction callees (func:LIR.functionDef) =
 let pruneBlock (block:LIR.basicBlock) =
  let rec rewrite = function
  | LIR.SaveRegs (ints,floats)::LIR.Call (dest,callee,[])::LIR.RestoreRegs (restoreInts,restoreFloats)::rest when ints=restoreInts && floats=restoreFloats ->
   let writes=Option.value (FunctionIdMap.tryFind callee callees) ~default:all in
   let keptInts=List.filter (fun reg -> containsInt reg writes) ints in
   let keptFloats=List.filter (fun reg -> containsFloat reg writes) floats in
   let removed=List.length ints+List.length floats-List.length keptInts-List.length keptFloats in
   let keptInts,keptFloats=if removed mod 2=0 then keptInts,keptFloats else match List.find_opt (fun reg -> not (List.mem reg keptInts)) ints with
    | Some extra -> List.filter (fun reg -> reg=extra || List.mem reg keptInts) ints,keptFloats
    | None -> let extra=match List.find_opt (fun reg -> not (List.mem reg keptFloats)) floats with Some extra -> extra | None -> Crash.crash "x64 call-save parity has no omitted register" in keptInts,List.filter (fun reg -> reg=extra || List.mem reg keptFloats) floats in
   LIR.SaveRegs (keptInts,keptFloats)::LIR.Call (dest,callee,[])::LIR.RestoreRegs (keptInts,keptFloats)::rewrite rest
  | instr::rest -> instr::rewrite rest | [] -> [] in
  {block with LIR.instrs=rewrite block.LIR.instrs} in
 let blocks=LIR.LabelMap.map pruneBlock func.LIR.cfg.LIR.blocks in
 if LIR.LabelMap.equal (=) blocks func.LIR.cfg.LIR.blocks then func else {func with LIR.cfg={func.LIR.cfg with LIR.blocks=blocks}}
