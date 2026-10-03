(* Complete induction, unrolling, and LICM CFG observations. *)
[@@@warning "-4"]
open Dark_compiler
module M = MIR
module J = ProductionMIR
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let reg n = M.VReg n
let v n = M.Register (reg n)
let label text = M.Label text
let block name instrs terminator : M.basicBlock = {M.label = label name; instrs; terminator}
let graph entry blocks : M.cfg = {M.entry = label entry; blocks = M.LabelMap.of_list (List.map (fun (block : M.basicBlock) -> block.M.label, block) blocks)}
let cfgChange (cfg, changed) = tuple [J.cfg cfg; `Bool changed]
let topology (value : MIRLoopTopology.loopTopology) = SemanticJson.record "LoopTopology" ["Loops", `Assoc ["map", list (fun (label, values) -> tuple [J.label label; `Assoc ["set", list J.label (M.LabelSet.elements values)]]) (M.LabelMap.bindings value.MIRLoopTopology.loops)]; "Predecessors", `Assoc ["map", list (fun (label, values) -> tuple [J.label label; list J.label values]) (M.LabelMap.bindings value.MIRLoopTopology.predecessors)]]
let observe source =
 let types = [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TFloat64;AST.TBool;AST.TChar;AST.TDateTime;AST.TString;AST.TUnit;AST.TInt128;AST.TUInt128;AST.TTuple [AST.TInt64]] in
 let make typ scale offset variant =
  let scaleInstr = match scale with 0 -> M.BinOp (reg 6, M.Mul, v 1, v 4, typ) | 1 -> M.BinOp (reg 6, M.Mul, v 4, v 1, typ) | _ -> M.BinOp (reg 6, M.Shl, v 1, M.Int64Const 1L, typ) in
  let affine = match offset with 0 -> M.BinOp (reg 7, M.Add, v 6, v 5, typ) | 1 -> M.BinOp (reg 7, M.Add, v 5, v 6, typ) | _ -> M.BinOp (reg 7, M.Sub, v 6, v 5, typ) in
  let outside = if variant = 2 then [M.Int64Const 0L, label "left"; M.Int64Const 2L, label "right"] else [M.Int64Const 0L, label source] in
  let phis = [M.Phi (reg 1, outside @ [v 10, label "latch"], Some typ); M.Phi (reg 3, outside @ [v 11, label "latch"], Some typ)] in
  let invariantPhi = if variant = 1 then [M.Phi (reg 30, [v 4, label source; v 30, label "latch"], Some typ)] else [] in
  let bound = if variant = 4 then v 1 else if variant = 5 then M.Int64Const 4L else v 2 in
  let header = block "header" (phis @ invariantPhi @ [M.BinOp (reg 12, M.Gte, v 1, bound, AST.TInt64)]) (M.Branch (v 12, label "exit", label "latch")) in
  let extra = match variant with
   | 1 -> [M.BinOp (reg 40, M.Add, v 4, M.Int64Const 1L, typ); M.BinOp (reg 41, M.Mul, v 40, M.Int64Const 2L, typ); M.FloatAbs (reg 42, v 30)]
   | 2 | 3 -> [M.BinOp (reg 40, M.Add, v 4, M.Int64Const 1L, typ)]
   | 6 -> [M.UnaryOp (reg 40, M.Neg, v 6)]
   | 8 -> [M.Call (reg 40, AST.functionId 200L, [v 4], [typ], typ)]
   | 9 -> [M.FloatNeg (reg 40, v 4); M.FloatToBits (reg 41, v 40)]
   | _ -> [] in
  let step = if variant = 7 then 2L else 1L in
  let latch = block "latch" ([scaleInstr; affine; M.BinOp (reg 11, M.Add, v 3, v 7, typ); M.BinOp (reg 10, M.Add, v 1, M.Int64Const step, typ)] @ extra) (M.Jump (label "header")) in
  let exit = block "exit" [M.Mov (reg 13, v 3, Some typ)] (M.Ret (v 13)) in
  let entry = if variant = 2 then [block source [] (M.Branch (v 2, label "left", label "right")); block "left" [] (M.Jump (label "header")); block "right" [] (M.Jump (label "header"))] else [block source [] (if variant = 3 then M.Branch (v 2, label "header", label "exit") else M.Jump (label "header"))] in
  let collision = if variant = 5 then [block "latch_unroll_second" [] (M.Ret (v 2)); block "exit_unroll_remainder" [M.Mov (reg 2147483646, v 4, Some typ)] (M.Ret (v 2)); block "header_preheader" [] (M.Ret (v 2))] else [] in
  graph source (entry @ [header; latch; exit] @ collision) in
 list (fun typ -> list (fun scale -> list (fun offset -> list (fun variant -> let cfg = make typ scale offset variant in
  tuple [J.cfg cfg;
   SemanticJson.int32 (MIRInduction.nextRegisterId cfg);
   list (fun transform -> attempt cfgChange (fun () -> transform cfg)) [MIRInduction.applyAffineInductionStrengthReduction; MIRUnrolling.applyCountedLoopUnrolling; MIRLoopInvariantMotion.applyLoopInvariantCodeMotion];
   list (fun functions -> attempt (fun result -> match result with None -> SemanticJson.union "FSharpOption" "None" [] | Some (cfg, changed, facts) -> SemanticJson.union "FSharpOption" "Some" [tuple [J.cfg cfg; `Bool changed; topology facts]]) (fun () -> Option.map (fun facts -> MIRLoopInvariantMotion.applyLoopInvariantCodeMotionWithEffectFreeCalls functions facts cfg) (MIRLoopTopology.tryBuildLoopTopology cfg))) [SpecializationIdentity.FunctionSet.empty; SpecializationIdentity.FunctionSet.singleton (AST.functionId 200L)]]) [0;1;2;3;4;5;6;7;8;9]) [0;1;2]) [0;1;2]) types
