(* Complete SSA construction observations for definitions, edges, types and numbering. *)
[@@@warning "-4"]
open Dark_compiler
module J = ProductionMIR
module S = SSA_Construction
module M = MIR
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let verification value = match value with Ok () -> SemanticJson.union "FSharpResult" "Ok" [`Null] | Error message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let set values = `Assoc ["set", list J.vReg (M.VRegSet.elements values)]
let labelMap encode values = `Assoc ["map", list (fun (label, value) -> tuple [J.label label; encode value]) (M.LabelMap.bindings values)]
let regMap encode values = `Assoc ["map", list (fun (reg, value) -> tuple [J.vReg reg; encode value]) (M.VRegMap.bindings values)]
let labels values = `Assoc ["set", list J.label (M.LabelSet.elements values)]
let id value = M.VReg value
let v value = M.Register (id value)
let label text = M.Label text
let block name instructions terminator : M.basicBlock = {M.label = label name; instrs = instructions; terminator}
let graph entry blocks : M.cfg = {M.entry = label entry; blocks = M.LabelMap.of_list (List.map (fun (block : M.basicBlock) -> block.M.label, block) blocks)}
let observe source =
 let types = [AST.TInt64; AST.TFloat64; AST.TString] in
 let graphs typ =
  let move value = M.Mov (id 1, value, Some typ) in
  let left = block "left" [move (v 2)] (M.Jump (label "join")) and right = block "right" [move (M.Int64Const 0L)] (M.Jump (label "join")) and join = block "join" [] (M.Ret (v 1)) in
  let entry = block source [] (M.Branch (v 2, label "left", label "right")) in
  let header = block "header" [] (M.Branch (v 2, label "body", label "exit")) and body = block "body" [move (v 1)] (M.Jump (label "header")) and exit = block "exit" [] (M.Ret (v 1)) in
  [graph source [block source [M.Mov (id 1, v 1, Some typ)] (M.Ret (v 1))];
   graph source [entry; left; right; join];
   graph source [entry; left; {right with M.instrs = [M.Mov (id 1, v 2, Some AST.TBool)]}; join];
   graph source [entry; left; right; join; block "unreachable" [move (v 1)] (M.Ret (v 1))];
   graph source [block source [move (v 2)] (M.Jump (label "header")); header; body; exit];
   graph source [block source [move (v 2)] (M.Branch (v 2, label "join", label "join")); join];
   graph source [block source [M.RuntimeErrorString (v 1)] (M.Ret (v 2))];
   graph source [block source [M.Mov (id 2147474000, v 2, Some typ); M.Mov (id 1, M.Register (id 2147474000), Some typ)] (M.Ret (v 1))];
   graph source [block source [] (M.Jump (label "missing"))]; graph source [];
   graph source [block source [M.Phi (id 1, [v 2, label source; v 1, label "unreachable"], Some typ)] (M.Ret (v 1)); block "unreachable" [] (M.Jump (label source))];
   graph source [block source [M.Mov (id 3, v 2, Some typ)] (M.Jump (label "middle")); block "middle" [M.Phi (id 4, [v 3, label source], Some typ)] (M.Jump (label "exit")); block "exit" [M.Phi (id 5, [v 4, label "middle"], Some typ)] (M.Ret (v 5))];
   graph source [entry; {left with M.instrs = []}; {right with M.instrs = []}; block "join" [M.Phi (id 3, [v 2, label "left"; M.Int64Const 0L, label "right"], Some typ); M.Mov (id 4, v 3, Some typ)] (M.Ret (v 4))];
   graph source [block source [] (M.Jump (label "empty")); block "empty" [] (M.Jump (label "join")); block "join" [M.Phi (id 3, [v 2, label "empty"], Some typ)] (M.Ret (v 3))];
   graph source [block source [] (M.Jump (label "a")); block "a" [] (M.Jump (label "b")); block "b" [] (M.Jump (label "a"))];
   graph source [entry; {left with M.instrs = [M.Mov (id 3, v 2, Some typ)]}; {right with M.instrs = []}; block "join" [] (M.Ret (v 3))];
   graph source [block source [M.Mov (id 2, v 2, Some typ)] (M.Ret (v 2))];
   graph source [block source [M.Mov (id 3, v 4, Some typ); M.Mov (id 4, v 2, Some typ)] (M.Ret (v 3))];
   graph source [entry; {left with M.instrs = []}; {right with M.instrs = []}; block "join" [M.Phi (id 3, [v 2, label "left"; v 2, label "left"], Some typ)] (M.Ret (v 3))];
   graph source [entry; {left with M.instrs = []}; {right with M.instrs = []}; block "join" [M.Phi (id 3, [v 2, label "left"; v 2, label "right"], Some typ); M.RuntimeErrorString (v 3)] (M.Ret (v 3))]] in
 list (fun typ -> list (fun cfg ->
  let parameters = [id 2] in let floats = if typ = AST.TFloat64 then M.IntSet.of_list [1;2] else M.IntSet.empty in
  let func : M.functionDef = {M.id = AST.functionId 200L; name = source; typedParams = [{M.reg = id 2; typ}]; returnType = typ; cfg; floatRegs = floats} in
  let predecessors = S.buildPredecessors cfg in let dominators = S.computeDominators cfg predecessors in let frontier = S.computeDominanceFrontier cfg predecessors dominators in
  tuple [labelMap (list J.label) predecessors; labelMap J.label dominators; labelMap labels frontier;
   list (fun (_, block) -> tuple [set (S.getBlockDefs block); set (S.getBlockUses block); list J.label (S.getSuccessors block)]) (M.LabelMap.bindings cfg.M.blocks);
   regMap labels (S.getAllDefs cfg);
   attempt (fun (input, output) -> tuple [labelMap set input; labelMap set output]) (fun () -> S.computeLiveness cfg);
   attempt J.cfg (fun () -> let input, _ = S.computeLiveness cfg in S.insertPhiNodes cfg frontier predecessors input parameters [typ]);
   attempt J.functionDef (fun () -> S.convertFunctionToSSA func);
   attempt (fun (cfg, floats) -> tuple [J.cfg cfg; `Assoc ["set", list SemanticJson.int32 (M.IntSet.elements floats)]]) (fun () -> S.renameCFG cfg dominators floats parameters);
   labelMap (list J.label) (S.buildDomTree dominators);
   attempt verification (fun () -> MIR_SSA_Verify.verifyFunction func);
   attempt verification (fun () -> MIR_SSA_Verify.verifyFunction (S.convertFunctionToSSA func));
   `Bool (MIRLoopTopology.cfgHasReachableCycle cfg);
   attempt (labelMap labels) (fun () -> MIRLoopTopology.findNaturalLoops cfg);
   list (fun transform -> attempt (fun (cfg, changed) -> tuple [J.cfg cfg; `Bool changed]) (fun () -> transform cfg)) [MIRControlFlow.mergeLinearBlocks; MIRControlFlow.simplifyEmptyBlocks; MIRControlFlow.simplifyRetPhiJoins];
   attempt (fun (func, timings) -> tuple [J.functionDef func; list (fun timing -> tuple [SemanticJson.string timing.S.phase; `Bool (timing.S.elapsedMs >= 0.)]) timings]) (fun () -> S.convertFunctionToSSAWithTiming func)]) (graphs typ)) types
