(* MIR_Optimize.ml - Schedule MIR simplification and optimization to a fixed point. *)
[@@@warning "-4"]
open MIR
module F = MIROptimizationFacts
module T = MIRLoopTopology
module Functions = SpecializationIdentity.FunctionSet
let timestamp () = Int64.of_float ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) *. 1000000.)
(*
   Run all optimizations in a single pass (returns whether anything changed).
*)
let optimizeCFGOnceWithEffectFreeCalls functions (options : F.optimizeOptions) recordTicks existingTopology cfg =
 let measure name operation = match recordTicks with None -> operation () | Some record -> let started = timestamp () in let result = operation () in record name (Int64.sub (timestamp ()) started); result in
 let cfg0, changed0 = if options.F.enableSCCP then measure "MIR Sparse Conditional Simplification" (fun () -> MIRSparseConditionalConstants.applySparseConditionalSimplification cfg) else cfg, false in
 let topologyForCse = if changed0 then None else existingTopology in
 let cfg1, changed1, cseTopology = if options.F.enableCSE then measure "MIR Common Subexpression Elimination" (fun () -> let cfg, changed, topology = MIRCommonExpressions.applyCSEWithEffectFreeCallsAndTopology topologyForCse functions cfg0 in cfg, changed, Some topology) else cfg0, false, topologyForCse in
 let _cfg2, changed2, cfg3, changed3, loopTopology = if options.F.enableLICM then
  let topology = measure "MIR Loop Topology" (fun () -> match cseTopology with Some topology -> T.tryBuildLoopTopologyWithDominators cfg1 topology | None -> T.tryBuildLoopTopology cfg1) in
  match topology with None -> cfg1, false, cfg1, false, None | Some topology ->
   let cfg2, changed2 = measure "MIR Affine Strength Reduction" (fun () -> MIRInduction.applyAffineInductionStrengthReductionWithTopology topology cfg1) in
   let cfg3, changed3, topology = measure "MIR Loop Invariant Code Motion" (fun () -> MIRLoopInvariantMotion.applyLoopInvariantCodeMotionWithEffectFreeCalls functions topology cfg2) in
   cfg2, changed2, cfg3, changed3, Some topology
 else cfg1, false, cfg1, false, None in
 let cfg4, changed4 = match loopTopology with Some topology -> measure "MIR Counted Loop Unrolling" (fun () -> MIRUnrolling.applyCountedLoopUnrollingWithTopology topology cfg3) | None -> cfg3, false in
 let cfg5, changed5 = if options.F.enableDCE then measure "MIR Dead Code Elimination" (fun () -> MIRDeadCode.eliminateDeadCodeWithTickTrace recordTicks cfg4) else cfg4, false in
 let cfg6, changed6 = if options.F.enableSCCP then measure "MIR Simplify Return Phi Joins" (fun () -> MIRControlFlow.simplifyRetPhiJoins cfg5) else cfg5, false in
 let cfg7, changed7 = if options.F.enableSCCP then measure "MIR Simplify Empty Blocks" (fun () -> MIRControlFlow.simplifyEmptyBlocks cfg6) else cfg6, false in
 let cfg8, changed8 = if options.F.enableSCCP then measure "MIR Merge Linear Blocks" (fun () -> MIRControlFlow.mergeLinearBlocks cfg7) else cfg7, false in
 let changed = changed0 || changed1 || changed2 || changed3 || changed4 || changed5 || changed6 || changed7 || changed8 in
 let topologyChanged = changed0 || changed3 || changed4 || changed6 || changed7 || changed8 in
 cfg8, changed, (if topologyChanged then None else cseTopology)
let optimizeCFGOnce options cfg = let cfg, changed, _ = optimizeCFGOnceWithEffectFreeCalls Functions.empty options None None cfg in cfg, changed
(*
   Run all optimizations until fixed point
*)
let optimizeCFGWithEffectFreeCalls functions options recordTicks cfg =
 let rec loop current remaining topology = if remaining <= 0 then current else let next, changed, topology = optimizeCFGOnceWithEffectFreeCalls functions options recordTicks topology current in if changed then loop next (remaining - 1) topology else next in loop cfg 10 None
let optimizeCFGWithOptions options cfg = optimizeCFGWithEffectFreeCalls Functions.empty options None cfg
let optimizeCFG cfg = optimizeCFGWithOptions F.defaultOptimizeOptions cfg
let explicitFloatRegisters (cfg : cfg) = LabelMap.fold (fun _ block registers -> List.fold_left (fun registers instr -> let dest = match instr with Mov (dest, _, Some AST.TFloat64) | BinOp (dest, _, _, _, AST.TFloat64) | Phi (dest, _, Some AST.TFloat64) | FloatSqrt (dest, _) | FloatAbs (dest, _) | FloatNeg (dest, _) | Int64ToFloat (dest, _) -> Some dest | _ -> None in match dest with Some (VReg id) -> IntSet.add id registers | None -> registers) registers block.instrs) cfg.blocks IntSet.empty
let withOptimizedCFG (func : functionDef) cfg = {func with cfg; floatRegs = IntSet.union func.floatRegs (explicitFloatRegisters cfg)}
(*
   Optimize a function
*)
let optimizeFunctionWithOptions options (func : functionDef) = withOptimizedCFG func (optimizeCFGWithOptions options func.cfg)
let optimizeFunctionWithEffectFreeCalls functions options (func : functionDef) = withOptimizedCFG func (optimizeCFGWithEffectFreeCalls functions options None func.cfg)
let optimizeFunctionWithEffectFreeCallsAndTickTrace recorder functions options (func : functionDef) = withOptimizedCFG func (optimizeCFGWithEffectFreeCalls functions options recorder func.cfg)
let optimizeFunction (func : functionDef) = withOptimizedCFG func (optimizeCFG func.cfg)
let sameReturnOperand left right = match left, right with FloatSymbol a, FloatSymbol b -> Int64.bits_of_float a = Int64.bits_of_float b | _ -> left = right
let constantReturnOperand (func : functionDef) =
 let tailCall = LabelMap.exists (fun _ block -> List.exists (function TailCall _ | IndirectTailCall _ | ClosureTailCall _ -> true | _ -> false) block.instrs) func.cfg.blocks in
 let definitions = LabelMap.fold (fun _ block constants -> List.fold_left (fun constants -> function Mov (dest, ((Int64Const _ | BoolConst _ | FloatSymbol _ | StringSymbol _ | FuncAddr _) as value), _) -> VRegMap.add dest value constants | _ -> constants) constants block.instrs) func.cfg.blocks VRegMap.empty in
 let resolve = function ((Int64Const _ | BoolConst _ | FloatSymbol _ | StringSymbol _ | FuncAddr _) as operand) -> Some operand | Register reg -> VRegMap.find_opt reg definitions in
 let returns = LabelMap.bindings func.cfg.blocks |> List.filter_map (fun (_, block) -> match block.terminator with Ret operand -> Some (resolve operand) | Jump _ | Branch _ -> None) in
 match tailCall, returns with true, _ -> None | false, Some first :: rest when List.for_all (function Some value -> sameReturnOperand first value | None -> false) rest -> Some first | _ -> None
let constantCallResults functions = List.filter_map (fun (func : functionDef) -> Option.map (fun value -> func.id, value) (constantReturnOperand func)) functions |> FunctionIdMap.ofList
let propagateConstantCallResults optimizeAgain (options : F.optimizeOptions) functions = if not options.F.enableSCCP then functions else
 let results = constantCallResults functions in if FunctionIdMap.isEmpty results then functions else
 List.map (fun (func : functionDef) -> let cfg, changed = MIRSparseConditionalConstants.applySparseConditionalConstantPropagationWithCallResults (fun id -> FunctionIdMap.tryFind id results) func.cfg in if changed then optimizeAgain (withOptimizedCFG func cfg) else func) functions
(*
   Optimize a program
*)
let optimizeProgramWithOptions (options : F.optimizeOptions) (Program (functions, variants, records)) =
 let effects = if options.F.enableLICM || options.F.enableCSE then F.analyzeEffectFreeFunctions functions else Functions.empty in
 let optimize = optimizeFunctionWithEffectFreeCalls effects options in
 let functions = List.map optimize functions |> propagateConstantCallResults optimize options in Program (functions, variants, records)
(*
   Optimize a program and report aggregate timings for the fixed-point
   subpasses. Timings are accumulated as timestamp ticks so tracing does not
   allocate a Stopwatch for every function iteration.
*)
let optimizeProgramWithOptionsAndTrace recorder (options : F.optimizeOptions) program = match recorder with None -> optimizeProgramWithOptions options program | Some record ->
 let Program (functions, variants, records) = program in
 let ticks = Hashtbl.create 16 and order = ref [] in
 let addTicks name value = match Hashtbl.find_opt ticks name with Some old -> Hashtbl.replace ticks name (Int64.add old value) | None -> order := name :: !order; Hashtbl.add ticks name value in
 let started = timestamp () in
 let effects = if options.F.enableLICM || options.F.enableCSE then F.analyzeEffectFreeFunctions functions else Functions.empty in
 addTicks "MIR Effect Analysis" (Int64.sub (timestamp ()) started);
 let optimize (func : functionDef) = withOptimizedCFG func (optimizeCFGWithEffectFreeCalls effects options (Some addTicks) func.cfg) in
 let functions = List.map optimize functions |> propagateConstantCallResults optimize options in
 List.iter (fun name -> record name (Int64.to_float (Hashtbl.find ticks name) *. 1000. /. 1000000000.)) (List.rev !order);
 Program (functions, variants, records)
let optimizeProgram program = optimizeProgramWithOptions F.defaultOptimizeOptions program
