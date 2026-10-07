(*
   Verifies MIR optimizer transformations that depend on fixpoint iteration or
   require direct construction of CFG shapes not preserved by earlier passes.
*)
(* MIROptimizeTests.ml - Unit tests for MIR optimizer fixpoint behavior *)
[@@@warning "-4-42"]
open Dark_compiler
open MIR
open MIRLoopInvariantMotion
open MIRControlFlow
open MIRConstants
open MIRCommonExpressions
open MIRUnrolling
open MIRSparseConditionalConstants
open MIR_Optimize
open MIRPrinter
type testResult = (unit, string) result
let fid = TestIds.functionIdForName
let singleOptimizedFunction name = function [func] -> Ok func | [] -> Error (name ^ ": optimizer returned no functions") | _ -> Error (name ^ ": optimizer returned multiple functions")
let program functions = Program (functions, StringOrder.Map.empty, StringOrder.Map.empty)
let actual func = formatMIR (program [func])
let optimizedBlockForLabel name label (func : functionDef) = match LabelMap.find_opt label func.cfg.blocks with Some block -> Ok block | None -> Error (name ^ ": optimizer removed expected block " ^ StructuralFormat.format (StructuralFormat.Union ("Label", [StructuralFormat.Text (let Label text = label in text)])) ^ ".\nActual:\n" ^ actual func)
let directCallCount name (func : functionDef) = LabelMap.bindings func.cfg.blocks |> List.concat_map (fun (_, block) -> block.instrs) |> List.filter (function Call (_, called, _, _, _) -> called = fid name | _ -> false) |> List.length
let v n = Register (VReg n)
let r n = VReg n
let label name = Label name
let basicBlock label instrs terminator : basicBlock = {label; instrs; terminator}
let graph entry blocks : cfg = {entry; blocks = LabelMap.of_list (List.map (fun (block : basicBlock) -> block.label, block) blocks)}
let functionDef name params returnType cfg : functionDef = {id = fid name; name; typedParams = List.map (fun (reg, typ) -> {reg; typ}) params; returnType; cfg; floatRegs = IntSet.empty}
let scalarIdentityFunction name = let entry = label (name ^ "_entry") in functionDef name [r 0, AST.TInt64] AST.TInt64 (graph entry [basicBlock entry [] (Ret (v 0))])
let firstError results = match List.find_map (function Error error -> Some error | Ok () -> None) results with Some error -> Error error | None -> Ok ()
let check condition message = if condition then Ok () else Error message
let equalCFG (a : cfg) (b : cfg) = a.entry = b.entry && LabelMap.equal (=) a.blocks b.blocks
let find label (cfg : cfg) = LabelMap.find_opt label cfg.blocks
let testCseReusesEffectFreeDirectScalarCalls () =
 let entry = label "caller_entry" in let cfg = graph entry [basicBlock entry [Call (r 1, fid "pure", [v 0], [AST.TInt64], AST.TInt64); Call (r 2, fid "pure", [v 0], [AST.TInt64], AST.TInt64); BinOp (r 3, Add, v 1, v 2, AST.TInt64)] (Ret (v 3))] in
 let caller = {(scalarIdentityFunction "caller") with cfg} in let Program (functions, _, _) = optimizeProgram (program [scalarIdentityFunction "pure"; caller]) in
 match List.find_opt (fun (func : functionDef) -> func.name = "caller") functions with Some func -> let count = directCallCount "pure" func in check (count = 1) ("Expected one effect-free direct call after CSE, found " ^ string_of_int count ^ ".") | None -> Error "Expected optimized caller function"
let testCseReusesDominatingEffectFreeDirectScalarCalls () =
 let entry = label "caller_entry" and child = label "caller_child" in
 let cfg = graph entry [basicBlock entry [Call (r 1, fid "pure", [v 0], [AST.TInt64], AST.TInt64)] (Jump child);basicBlock child [Call (r 2, fid "pure", [v 0], [AST.TInt64], AST.TInt64)] (Ret (v 2))] in
 let cfg, changed = applyCSEWithEffectFreeCalls (SpecializationIdentity.FunctionSet.singleton (fid "pure")) cfg in
 let count = LabelMap.bindings cfg.blocks |> List.concat_map (fun (_, block) -> block.instrs) |> List.filter (function Call (_, name, _, _, _) when name = fid "pure" -> true | _ -> false) |> List.length in
 check (changed && count = 1) ("Expected one dominating effect-free call after CSE, found " ^ string_of_int count ^ ".")
let testCseDirectCallsRespectBarriersAndScalarTypes () =
 let entry = label "entry" in let call typ dest = Call (dest, fid "pure", [v 0], [typ], typ) in
 let verify name middle typ = let cfg = graph entry [basicBlock entry [call typ (r 1);middle;call typ (r 2)] (Ret (v 2))] in
  let cfg, changed = applyCSEWithEffectFreeCalls (SpecializationIdentity.FunctionSet.singleton (fid "pure")) cfg in
  let count = match find entry cfg with Some block -> List.filter (function Call (_, fn, _, _, _) when fn = fid "pure" -> true | _ -> false) block.instrs |> List.length | None -> 0 in check (not changed && count = 2) ("Expected " ^ name ^ " to prevent direct-call CSE.") in
 firstError [verify "an unproven call" (Call (r 3, fid "observe", [], [], AST.TUnit)) AST.TInt64;verify "a heap allocation" (HeapAlloc (r 3, 16)) AST.TInt64;verify "a reference-count decrement" (RefCountDec (r 3, 8, GenericHeap, None)) AST.TInt64;verify "a managed result type" (Mov (r 3, Int64Const 0L, None)) AST.TString]
let testCseDoesNotReuseThrowingDirectCalls () =
 let entry = label "entry" in
 let throwing = {(scalarIdentityFunction "throwing") with cfg = graph entry [basicBlock entry [RuntimeError "boom"] (Ret (Int64Const 0L))]} in
 let caller = {(scalarIdentityFunction "throwing_caller") with cfg = graph entry [basicBlock entry [Call (r 1, fid "throwing", [v 0], [AST.TInt64], AST.TInt64);Call (r 2, fid "throwing", [v 0], [AST.TInt64], AST.TInt64)] (Ret (v 2))]} in
 let Program (functions, _, _) = optimizeProgram (program [throwing; caller]) in
 match List.find_opt (fun (func : functionDef) -> func.name = "throwing_caller") functions with Some func -> let count = directCallCount "throwing" func in check (count = 2) ("Expected throwing calls to remain, found " ^ string_of_int count ^ ".") | None -> Error "Expected optimized throwing caller function"
let testCseAfterCopyPropFixpoint () =
 let entry = label "entry" in
 let block = basicBlock entry [Mov (r 2, v 0, Some AST.TInt64);BinOp (r 3, Add, v 2, v 1, AST.TInt64);BinOp (r 4, Add, v 0, v 1, AST.TInt64);BinOp (r 5, Add, v 3, v 4, AST.TInt64)] (Ret (v 5)) in
 let func = functionDef "fixpoint_cse" [r 0, AST.TInt64;r 1, AST.TInt64] AST.TInt64 (graph entry [block]) in
 let Program (functions, _, _) = optimizeProgram (program [func]) in
 match singleOptimizedFunction "testCseAfterCopyPropFixpoint" functions with Error error -> Error error | Ok func ->
 match optimizedBlockForLabel "testCseAfterCopyPropFixpoint" entry func with Error error -> Error error | Ok optimized ->
 let expected = {block with instrs = [BinOp (r 3, Add, v 0, v 1, AST.TInt64);BinOp (r 5, Add, v 3, v 3, AST.TInt64)]} in check (optimized = expected) ("MIR optimization did not reach fixpoint.\nActual:\n" ^ actual func)
let testCseReusesDominatingExpressions () =
 let entry = label "entry" and bridge = label "bridge" and child = label "child" in
 let entryBlock = basicBlock entry [BinOp (r 2, Add, v 0, v 1, AST.TInt64);UnaryOp (r 3, Neg, v 0)] (Jump bridge) in
 let childBlock = basicBlock child [BinOp (r 4, Add, v 0, v 1, AST.TInt64);UnaryOp (r 5, Neg, v 0)] (Ret (v 4)) in
 let cfg, changed = applyCSE (graph entry [entryBlock;basicBlock bridge [] (Jump child);childBlock]) in
 let expected = {childBlock with instrs = [Mov (r 4, v 2, None);Mov (r 5, v 3, None)]} in
 check (changed && find child cfg = Some expected) ("Expected binary and unary expressions from the dominating entry block to be reused.\nActual:\n" ^ actual (functionDef "dominating_cse" [r 0, AST.TInt64;r 1, AST.TInt64] AST.TInt64 cfg))
let testUnaryPartialRedundancyEliminationCompletesMissingPath () =
 let entry = label "unary_pre_entry" and left = label "unary_pre_left" and right = label "unary_pre_right" and join = label "unary_pre_join" in
 let expression dest = UnaryOp (dest, BitNot, v 0) in
 let cfg, changed = applyCSE (graph entry [basicBlock entry [] (Branch (v 1, left, right));basicBlock left [expression (r 2)] (Jump join);basicBlock right [] (Jump join);basicBlock join [expression (r 3)] (Ret (v 3))]) in
 check (changed && (match find right cfg, find join cfg with Some rightBlock, Some joinBlock -> rightBlock.instrs = [expression (r 4)] && joinBlock.instrs = [Phi (r 3, [v 4, right;v 2, left], None)] | _ -> false)) "Expected unary PRE to insert BitNot on the missing path and merge both values with a phi"
let testPartialRedundancyEliminationCompletesMissingPath () =
 let entry = label "entry" and left = label "left" and right = label "right" and join = label "join" in let expression dest = BinOp (dest, Add, v 0, v 1, AST.TInt64) in
 let cfg, changed = applyCSE (graph entry [basicBlock entry [] (Branch (v 2, left, right));basicBlock left [expression (r 3)] (Jump join);basicBlock right [] (Jump join);basicBlock join [expression (r 4)] (Ret (v 4))]) in
 check (changed && (match find right cfg, find join cfg with Some rightBlock, Some joinBlock -> rightBlock.instrs = [expression (r 5)] && joinBlock.instrs = [Phi (r 4, [v 5, right;v 3, left], Some AST.TInt64)] | _ -> false)) ("Expected PRE to insert the missing expression and replace the join computation with a phi.\nActual:\n" ^ actual (functionDef "pre" [r 0, AST.TInt64;r 1, AST.TInt64;r 2, AST.TBool] AST.TInt64 cfg))
let testCseReusesDominatingScalarHeapLoad () =
 let entry = label "entry" and bridge = label "bridge" and child = label "child" in let typ = AST.TInt64 in
 let entryBlock = basicBlock entry [HeapLoad (r 1, r 0, 8, Some typ)] (Jump bridge) in let childBlock = basicBlock child [HeapLoad (r 2, r 0, 8, Some typ)] (Ret (v 2)) in
 let cfg, changed = applyCSE (graph entry [entryBlock;basicBlock bridge [] (Jump child);childBlock]) in
 check (changed && find child cfg = Some {childBlock with instrs = [Mov (r 2, v 1, Some typ)]}) ("Expected an exact scalar heap load from the dominating entry block to be reused.\nActual:\n" ^ actual (functionDef "dominating_scalar_heap_load_cse" [r 0, AST.TTuple [typ;typ]] typ cfg))
let testCseDoesNotReuseDominatingScalarHeapLoadAcrossBarriers () =
 let entry = label "entry" and child = label "child" in let typ = AST.TInt64 in
 let barriers = ["call", Call (r 3, fid "observe", [], [], AST.TUnit);"heap allocation", HeapAlloc (r 3, 16);"heap store", HeapStore (r 0, 8, Int64Const 99L, Some typ);"raw memory read", RawGet (r 3, v 0, Int64Const 0L, Some typ);"raw memory write", RawWriteWord (v 0, Int64Const 0L, Int64Const 99L);"reference count", RefCountInc (r 0, 16, GenericHeap, None);"non-scalar load", HeapLoad (r 3, r 0, 16, Some AST.TString)] in
 let rec checkBarriers = function [] -> Ok () | (name, barrier) :: rest -> let cfg = graph entry [basicBlock entry [HeapLoad (r 1, r 0, 8, Some typ);barrier] (Jump child);basicBlock child [HeapLoad (r 2, r 0, 8, Some typ)] (Ret (v 2))] in let optimized, changed = applyCSE cfg in if not changed && equalCFG optimized cfg then checkBarriers rest else Error ("Expected " ^ name ^ " to invalidate a dominated scalar heap load") in checkBarriers barriers
let testCsePreservesExpressionsAcrossSiblingBlocks () =
 let entry = label "entry" and left = label "left" and right = label "right" in
 let cfg = graph entry [basicBlock entry [] (Branch (v 2, left, right));basicBlock left [BinOp (r 3, Add, v 0, v 1, AST.TInt64);UnaryOp (r 4, Neg, v 0)] (Ret (v 3));basicBlock right [BinOp (r 5, Add, v 0, v 1, AST.TInt64);UnaryOp (r 6, Neg, v 0)] (Ret (v 5))] in let optimized, changed = applyCSE cfg in
 check (not changed && equalCFG optimized cfg) ("Expected duplicate expressions in non-dominating sibling blocks to remain independent.\nActual:\n" ^ actual (functionDef "sibling_cse" [r 0, AST.TInt64;r 1, AST.TInt64;r 2, AST.TBool] AST.TInt64 optimized))
let testCseDoesNotReuseExpressionsAcrossRefCountDecrement () =
 let entry = label "entry" and child = label "child" in
 let cfg = graph entry [basicBlock entry [BinOp (r 3, Add, v 1, v 2, AST.TInt64);RefCountDecString (v 0)] (Jump child);basicBlock child [BinOp (r 4, Add, v 1, v 2, AST.TInt64)] (Ret (v 4))] in let optimized, changed = applyCSE cfg in
 check (not changed && equalCFG optimized cfg) ("Expected the reference-count decrement to invalidate available expressions.\nActual:\n" ^ actual (functionDef "refcount_barrier_cse" [r 0, AST.TString;r 1, AST.TInt64;r 2, AST.TInt64] AST.TInt64 optimized))
let testCseDoesNotExtendExpressionsAcrossCalls () =
 let entry = label "entry" and child = label "child" in
 let cfg = graph entry [basicBlock entry [BinOp (r 2, Add, v 0, v 1, AST.TInt64);Call (r 3, fid "observe", [], [], AST.TUnit)] (Jump child);basicBlock child [BinOp (r 4, Add, v 0, v 1, AST.TInt64)] (Ret (v 4))] in let optimized, changed = applyCSE cfg in
 check (not changed && equalCFG optimized cfg) ("Expected the call to prevent extension of expression availability.\nActual:\n" ^ actual (functionDef "call_barrier_cse" [r 0, AST.TInt64;r 1, AST.TInt64] AST.TInt64 optimized))
let testCseDoesNotExportNonScalarBinaryTypes () =
 let entry = label "entry" and child = label "child" in let cfg = graph entry [basicBlock entry [BinOp (r 2, Add, v 0, v 1, AST.TUnit)] (Jump child);basicBlock child [BinOp (r 3, Add, v 0, v 1, AST.TUnit)] (Ret (v 3))] in let optimized, changed = applyCSE cfg in check (not changed && equalCFG optimized cfg) "Expected a binary expression with the non-scalar TUnit type to remain block-local"
let rec checkCases operation = function [] -> Ok () | item :: rest -> match operation item with Error error -> Error error | Ok () -> checkCases operation rest
let pureScalarInstructionCases = ["FloatSqrt", (fun dest src -> FloatSqrt (dest, src));"FloatAbs", (fun dest src -> FloatAbs (dest, src));"FloatNeg", (fun dest src -> FloatNeg (dest, src));"Int64ToFloat", (fun dest src -> Int64ToFloat (dest, src));"FloatToInt64", (fun dest src -> FloatToInt64 (dest, src));"FloatToBits", (fun dest src -> FloatToBits (dest, src))]
let testCseKeepsScalarHeapLoadsAvailableAcrossPureScalarInstructions () =
 let entry = label "entry" in checkCases (fun (name, instr) -> let block = basicBlock entry [HeapLoad (r 1, r 0, 8, Some AST.TFloat64);instr (r 2) (v 1);HeapLoad (r 3, r 0, 8, Some AST.TFloat64)] (Ret (v 3)) in let cfg, changed = applyCSE (graph entry [block]) in let expected = {block with instrs = [HeapLoad (r 1, r 0, 8, Some AST.TFloat64);instr (r 2) (v 1);Mov (r 3, v 1, Some AST.TFloat64)]} in check (changed && find entry cfg = Some expected) ("Expected " ^ name ^ " to preserve exact scalar heap-load availability")) pureScalarInstructionCases
let testCseDoesNotExportScalarHeapLoadsAcrossPureScalarInstructions () =
 let entry = label "entry" and child = label "child" in checkCases (fun (name, instr) -> let cfg = graph entry [basicBlock entry [HeapLoad (r 1, r 0, 8, Some AST.TFloat64);instr (r 2) (v 1)] (Jump child);basicBlock child [HeapLoad (r 3, r 0, 8, Some AST.TFloat64)] (Ret (v 3))] in let optimized, changed = applyCSE cfg in check (not changed && equalCFG optimized cfg) ("Expected " ^ name ^ " to stop scalar heap-load availability at the block boundary")) pureScalarInstructionCases
let testCseDoesNotKeepDirectCallsAvailableAcrossPureScalarInstructions () =
 let entry = label "entry" in checkCases (fun (name, instr) -> let cfg = graph entry [basicBlock entry [Call (r 1, fid "pure", [], [], AST.TFloat64);instr (r 2) (v 1);Call (r 3, fid "pure", [], [], AST.TFloat64)] (Ret (v 3))] in let optimized, changed = applyCSEWithEffectFreeCalls (SpecializationIdentity.FunctionSet.singleton (fid "pure")) cfg in check (not changed && equalCFG optimized cfg) ("Expected " ^ name ^ " to retain the conservative direct-call CSE boundary")) pureScalarInstructionCases
let testDceRemovesSelfReferentialDeadPhi () =
 let entry = label "entry" and loop = label "loop" and exit = label "exit" in
 let cfg = graph entry [basicBlock entry [] (Jump loop);basicBlock loop [Phi (r 1, [v 0, entry;v 1, loop], Some AST.TBool)] (Branch (v 0, exit, loop));basicBlock exit [] (Ret (v 0))] in
 let Program (functions, _, _) = optimizeProgram (program [functionDef "dead_phi_cycle" [r 0, AST.TBool] AST.TBool cfg]) in
 match singleOptimizedFunction "testDceRemovesSelfReferentialDeadPhi" functions with Error error -> Error error | Ok func ->
 match optimizedBlockForLabel "testDceRemovesSelfReferentialDeadPhi" loop func with Error error -> Error error | Ok block -> check (not (List.exists (function Phi _ -> true | _ -> false) block.instrs)) ("Expected dead self-referential phi to be removed by DCE.\nActual:\n" ^ actual func)
let testCfgSimplifyRemovesRetPhiJoin () =
 let entry = label "entry" and yes = label "then" and no = label "else" and join = label "join" in
 let cfg = graph entry [basicBlock entry [] (Branch (v 0, yes, no));basicBlock yes [] (Jump join);basicBlock no [] (Jump join);basicBlock join [Phi (r 1, [Int64Const 1L, yes;Int64Const 2L, no], Some AST.TInt64)] (Ret (v 1))] in
 let Program (functions, _, _) = optimizeProgram (program [functionDef "ret_phi_join" [r 0, AST.TBool] AST.TInt64 cfg]) in
 match singleOptimizedFunction "testCfgSimplifyRemovesRetPhiJoin" functions with Error error -> Error error | Ok func ->
 let returns label value = match find label func.cfg with Some block -> block.terminator = Ret (Int64Const value) | None -> false in check (not (LabelMap.mem join func.cfg.blocks) && returns yes 1L && returns no 2L) ("Expected ret-phi join simplification.\nActual:\n" ^ actual func)
let testCfgSimplifyCollapsesCopyWrappedRetPhiChain () =
 let left = label "left" and right = label "right" and inner = label "inner_join" and outerElse = label "outer_else" and outer = label "outer_join" in
 let cfg, changed = simplifyRetPhiJoins (graph left [basicBlock left [] (Jump inner);basicBlock right [] (Jump inner);basicBlock inner [Phi (r 2, [Int64Const 1L, left;Int64Const 2L, right], Some AST.TInt64);Mov (r 4, v 2, Some AST.TInt64)] (Jump outer);basicBlock outerElse [] (Jump outer);basicBlock outer [Phi (r 3, [v 4, inner;Int64Const 3L, outerElse], Some AST.TInt64)] (Ret (v 3))]) in
 let returns label expected = match find label cfg with Some block -> block.terminator = Ret (Int64Const expected) | None -> false in
 check (changed && not (LabelMap.mem inner cfg.blocks) && not (LabelMap.mem outer cfg.blocks) && returns left 1L && returns right 2L && returns outerElse 3L) "Expected local copies not to force another whole optimizer iteration"
let testEmptyBlockRemovalRewritesPhiSourceToPredecessor () =
 let entry = label "entry" and empty = label "empty" and join = label "join" in
 let cfg, changed = simplifyEmptyBlocks (graph entry [basicBlock entry [Mov (r 0, Int64Const 41L, Some AST.TInt64)] (Jump empty);basicBlock empty [] (Jump join);basicBlock join [Phi (r 1, [v 0, empty], Some AST.TInt64);BinOp (r 2, Add, v 1, Int64Const 1L, AST.TInt64)] (Ret (v 2))]) in
 match find join cfg with Some block -> check (changed && (match block.instrs with Phi (_, [(Register (VReg 0), source)], _) :: _ -> source = entry | _ -> false)) ("Expected phi source to be rewritten from removed empty block to entry predecessor.\nActual:\n" ^ actual (functionDef "empty_phi" [] AST.TInt64 cfg)) | None -> Error "Expected join block to remain after empty block removal"
let testLinearBlockMergePreservesPhiSources () =
 let entry = label "entry" and body = label "body" and alternate = label "alternate" and join = label "join" in
 let entryBlock = basicBlock entry [BinOp (r 2, Add, v 0, v 1, AST.TInt64)] (Jump body) in
 let bodyBlock = basicBlock body [Phi (r 3, [v 2, entry], Some AST.TInt64);BinOp (r 4, Add, v 3, Int64Const 1L, AST.TInt64)] (Jump join) in
 let joinBlock = basicBlock join [Phi (r 6, [v 4, body;v 5, alternate], Some AST.TInt64)] (Ret (v 6)) in
 let cfg, changed = mergeLinearBlocks (graph entry [entryBlock;bodyBlock;basicBlock alternate [Mov (r 5, Int64Const 0L, Some AST.TInt64)] (Jump join);joinBlock]) in
 let expectedEntry = {entryBlock with instrs = entryBlock.instrs @ [Mov (r 3, v 2, Some AST.TInt64);BinOp (r 4, Add, v 3, Int64Const 1L, AST.TInt64)];terminator = Jump join} in
 let expectedJoin = {joinBlock with instrs = [Phi (r 6, [v 4, entry;v 5, alternate], Some AST.TInt64)]} in
 check (changed && not (LabelMap.mem body cfg.blocks) && find entry cfg = Some expectedEntry && find join cfg = Some expectedJoin) ("Expected linear block merge to preserve phi values and source labels.\nActual:\n" ^ actual (functionDef "linear_phi" [] AST.TInt64 cfg))
let testLinearBlockMergeExposesLocalCSE () =
 let entry = label "entry" and body = label "body" in let first = BinOp (r 2, Add, v 0, v 1, AST.TInt64) in
 let entryBlock = basicBlock entry [first] (Jump body) in
 let cfg = optimizeCFG (graph entry [entryBlock;basicBlock body [BinOp (r 3, Add, v 0, v 1, AST.TInt64);BinOp (r 4, Add, v 2, v 3, AST.TInt64)] (Ret (v 4))]) in
 let expected = {entryBlock with instrs = [first;BinOp (r 4, Add, v 2, v 2, AST.TInt64)];terminator = Ret (v 4)} in
 check (LabelMap.equal (=) cfg.blocks (LabelMap.singleton entry expected)) ("Expected linear block merge to expose duplicate expressions to local CSE.\nActual:\n" ^ actual (functionDef "linear_cse" [] AST.TInt64 cfg))
let testSameTargetBranchBecomesJumpAndDropsCondition () =
 let entry = label "entry" and target = label "target" in
 let cfg = optimizeCFG (graph entry [basicBlock entry [BinOp (r 2, Eq, v 0, v 1, AST.TBool)] (Branch (v 2, target, target));basicBlock target [] (Ret (Int64Const 1L))]) in
 check ((match find entry cfg with Some block -> block.instrs = [] && block.terminator = Ret (Int64Const 1L) && LabelMap.cardinal cfg.blocks = 1 | None -> false)) ("Expected same-target branch to become a jump and its dead condition to be removed.\nActual:\n" ^ actual (functionDef "same_target_branch" [r 0, AST.TBool;r 1, AST.TBool] AST.TInt64 cfg))
let testSccpPropagatesPhiConstantAndRemovesUnreachableEdge () =
 let entry = label "entry" and left = label "left" and right = label "right" and join = label "join" and live = label "live_result" and dead = label "dead_result" in
 let cfg, changed = applySparseConditionalConstantPropagation (graph entry [basicBlock entry [] (Branch (v 0, left, right));basicBlock left [Mov (r 1, Int64Const 7L, Some AST.TInt64)] (Jump join);basicBlock right [Mov (r 2, Int64Const 7L, Some AST.TInt64)] (Jump join);basicBlock join [Phi (r 3, [v 1, left;v 2, right], Some AST.TInt64);BinOp (r 4, Eq, v 3, Int64Const 7L, AST.TInt64)] (Branch (v 4, live, dead));basicBlock live [] (Ret (Int64Const 42L));basicBlock dead [RuntimeError "unreachable"] (Ret (Int64Const 0L))]) in
 check ((match find join cfg with Some block -> changed && block.terminator = Jump live && LabelMap.mem live cfg.blocks && not (LabelMap.mem dead cfg.blocks) | None -> false)) ("Expected SCCP to fold the phi-derived branch and prune its false edge.\nActual:\n" ^ actual (functionDef "sccp_phi_constant" [r 0, AST.TBool] AST.TInt64 cfg))
let testSccpPhiIgnoresNonExecutableIncomingEdge () =
 let entry = label "entry" and live = label "live" and dead = label "dead" and join = label "join" in
 let cfg, changed = applySparseConditionalConstantPropagation (graph entry [basicBlock entry [Mov (r 0, BoolConst true, Some AST.TBool)] (Branch (v 0, live, dead));basicBlock live [Mov (r 1, Int64Const 5L, Some AST.TInt64)] (Jump join);basicBlock dead [Mov (r 2, Int64Const 99L, Some AST.TInt64)] (Jump join);basicBlock join [Phi (r 3, [v 1, live;v 2, dead], Some AST.TInt64)] (Ret (v 3))]) in
 check ((match find join cfg with Some block -> changed && block.instrs = [Mov (r 3, Int64Const 5L, Some AST.TInt64)] && block.terminator = Ret (v 3) && not (LabelMap.mem dead cfg.blocks) | None -> false)) "Expected SCCP phi evaluation to ignore the non-executable incoming edge"
let structuralCFG cfg = StructuralFormat.format (MIRTestFormatting.cfg cfg)
let testSccpCombinesCopyFoldingAndDeadEdgePruning () =
 let entry = label "entry" and live = label "live" and dead = label "dead" and join = label "join" in
 let cfg, changed = applySparseConditionalSimplification (graph entry [basicBlock entry [Mov (r 1, Int64Const 20L, Some AST.TInt64);Mov (r 2, v 1, Some AST.TInt64);BinOp (r 3, Add, v 2, Int64Const 22L, AST.TInt64);BinOp (r 4, Eq, v 3, Int64Const 42L, AST.TInt64);BinOp (r 6, Add, v 2, v 0, AST.TInt64)] (Branch (v 4, live, dead));basicBlock live [] (Jump join);basicBlock dead [] (Jump join);basicBlock join [Phi (r 5, [v 6, live;Int64Const 99L, dead], Some AST.TInt64)] (Ret (v 5))]) in
 check ((match find entry cfg, find join cfg with Some entryBlock, Some joinBlock -> changed && entryBlock.terminator = Jump live && not (LabelMap.mem dead cfg.blocks) && List.mem (Mov (r 3, Int64Const 42L, Some AST.TInt64)) entryBlock.instrs && List.mem (Mov (r 4, BoolConst true, Some AST.TBool)) entryBlock.instrs && List.mem (BinOp (r 6, Add, Int64Const 20L, v 0, AST.TInt64)) entryBlock.instrs && joinBlock.instrs = [Phi (r 5, [v 6, live], Some AST.TInt64)] | _ -> false)) ("Expected SCCP to fold through a copy, remove a dead branch, and trim its phi input: " ^ structuralCFG cfg)
let testSccpPropagatesNegatedBooleanThroughCopy () =
 let entry = label "entry" and inverted = label "inverted" and normal = label "normal" and invertedLive = label "inverted_live" and invertedDead = label "inverted_dead" and normalLive = label "normal_live" and normalDead = label "normal_dead" in
 let cfg, changed = applySparseConditionalSimplification (graph entry [basicBlock entry [UnaryOp (r 1, Not, v 0);Mov (r 2, v 1, Some AST.TBool)] (Branch (v 2, inverted, normal));basicBlock inverted [] (Branch (v 0, invertedDead, invertedLive));basicBlock normal [] (Branch (v 0, normalLive, normalDead));basicBlock invertedLive [] (Ret (Int64Const 1L));basicBlock invertedDead [] (Ret (Int64Const 99L));basicBlock normalLive [] (Ret (Int64Const 2L));basicBlock normalDead [] (Ret (Int64Const 98L))]) in
 check ((match find inverted cfg, find normal cfg with Some a, Some b -> changed && a.terminator = Jump invertedLive && b.terminator = Jump normalLive && not (LabelMap.mem invertedDead cfg.blocks) && not (LabelMap.mem normalDead cfg.blocks) | _ -> false)) ("Expected both outcomes of a copied negation to remove contradictory branches: " ^ structuralCFG cfg)
let testSccpLoopBackedgeWidensInductionValue () =
 let entry = label "entry" and header = label "header" and body = label "body" and exit = label "exit" in
 let cfg, _ = applySparseConditionalConstantPropagation (graph entry [basicBlock entry [] (Jump header);basicBlock header [Phi (r 0, [Int64Const 0L, entry;v 2, body], Some AST.TInt64);BinOp (r 1, Lt, v 0, Int64Const 3L, AST.TInt64)] (Branch (v 1, body, exit));basicBlock body [BinOp (r 2, Add, v 0, Int64Const 1L, AST.TInt64)] (Jump header);basicBlock exit [] (Ret (v 0))]) in
 check ((match find header cfg with Some block -> block.terminator = Branch (v 1, body, exit) && LabelMap.mem body cfg.blocks && LabelMap.mem exit cfg.blocks | None -> false)) "Expected the executable loop backedge to widen the induction value and retain both exits"
let testSccpTracksFloatAndStringConstantsWithoutBypass () =
 let entry = label "entry" and live = label "live" and dead = label "dead" in
 let cfg, changed = applySparseConditionalConstantPropagation (graph entry [basicBlock entry [Mov (r 0, FloatSymbol 1.5, Some AST.TFloat64);Mov (r 1, StringSymbol "same", Some AST.TString);BinOp (r 2, Eq, v 0, FloatSymbol 1.5, AST.TFloat64);CanonicalBufferEq (r 3, MemoryModel.Utf8String, v 1, StringSymbol "same");BinOp (r 4, And, v 2, v 3, AST.TBool)] (Branch (v 4, live, dead));basicBlock live [] (Ret (StringSymbol "same"));basicBlock dead [RuntimeError "unreachable"] (Ret (StringSymbol "dead"))]) in
 check ((match find entry cfg with Some block -> changed && block.terminator = Jump live && not (LabelMap.mem dead cfg.blocks) | None -> false)) "Expected SCCP to analyze Float and String constants without bypassing the CFG"
let testSccpStabilizesNanConstants () =
 let entry = label "entry" and live = label "live" and dead = label "dead" in
 let cfg, changed = applySparseConditionalConstantPropagation (graph entry [basicBlock entry [Mov (r 0, FloatSymbol (Int64.float_of_bits 0xfff8000000000000L), Some AST.TFloat64);FloatNeg (r 1, v 0);BinOp (r 2, Neq, v 1, v 1, AST.TFloat64)] (Branch (v 2, live, dead));basicBlock live [] (Ret (BoolConst true));basicBlock dead [RuntimeError "unreachable"] (Ret (BoolConst false))]) in
 check ((match find entry cfg with Some block -> changed && block.terminator = Jump live && not (LabelMap.mem dead cfg.blocks) | None -> false)) "Expected SCCP to stabilize NaN constants and prune their false comparison edge"
let testSccpTracksAggregateConstructorFields () =
 let entry = label "entry" and left = label "left" and right = label "right" and join = label "join" and live = label "live" and dead = label "dead" in
 let cfg, changed = applySparseConditionalConstantPropagation (graph entry [basicBlock entry [] (Branch (v 0, left, right));basicBlock left [HeapAlloc (r 1, 16);HeapStore (r 1, 0, Int64Const 1L, None)] (Jump join);basicBlock right [HeapAlloc (r 2, 16);HeapStore (r 2, 0, Int64Const 1L, None)] (Jump join);basicBlock join [Phi (r 3, [v 1, left;v 2, right], Some (AST.TSum ("Choice", [])));HeapLoad (r 4, r 3, 0, None);BinOp (r 5, Eq, v 4, Int64Const 1L, AST.TInt64)] (Branch (v 5, live, dead));basicBlock live [] (Ret (Int64Const 1L));basicBlock dead [RuntimeError "unreachable"] (Ret (Int64Const 0L))]) in
 check ((match find join cfg with Some block -> changed && block.terminator = Jump live && not (LabelMap.mem dead cfg.blocks) | None -> false)) "Expected SCCP to merge constructor aggregates and propagate their common tag"
let testSccpUsesCallResultRange () =
 let entry = label "entry" and live = label "live" and dead = label "dead" in let call = Call (r 0, fid "smallResult", [], [], AST.TUInt8) in
 let cfg, changed = applySparseConditionalConstantPropagation (graph entry [basicBlock entry [call;BinOp (r 1, Lte, v 0, Int64Const 255L, AST.TUInt8)] (Branch (v 1, live, dead));basicBlock live [] (Ret (Int64Const 1L));basicBlock dead [RuntimeError "unreachable"] (Ret (Int64Const 0L))]) in
 check ((match find entry cfg with Some block -> changed && List.mem call block.instrs && block.terminator = Jump live && not (LabelMap.mem dead cfg.blocks) | None -> false)) "Expected SCCP to retain the call effect while using its typed result range"
let testSccpPropagatesConstantCallResult () =
 let calleeEntry = label "callee_entry" and entry = label "caller_entry" and live = label "caller_live" and dead = label "caller_dead" in
 let callee = functionDef "constantText" [] AST.TString (graph calleeEntry [basicBlock calleeEntry [] (Ret (StringSymbol "known"))]) in let call = Call (r 0, callee.id, [], [], AST.TString) in
 let caller = functionDef "constantCaller" [] AST.TInt64 (graph entry [basicBlock entry [call;CanonicalBufferEq (r 1, MemoryModel.Utf8String, v 0, StringSymbol "known")] (Branch (v 1, live, dead));basicBlock live [] (Ret (Int64Const 1L));basicBlock dead [RuntimeError "unreachable"] (Ret (Int64Const 0L))]) in
 let Program (functions, _, _) = optimizeProgram (program [callee;caller]) in match List.find_opt (fun (func : functionDef) -> func.id = caller.id) functions with None -> Error "Expected optimized program to retain the constant-call caller" | Some func -> check ((match find entry func.cfg with Some block -> List.mem call block.instrs && (block.terminator = Jump live || block.terminator = Ret (Int64Const 1L)) && not (LabelMap.mem dead func.cfg.blocks) | None -> false)) "Expected SCCP to retain the constant-returning call while pruning its false result edge"
let testSccpDoesNotApplyIntegerFoldsToFloatOperations () =
 let entry = label "entry" in let instruction = BinOp (r 1, Mul, Int64Const 0L, v 0, AST.TFloat64) in let cfg, changed = applySparseConditionalConstantPropagation (graph entry [basicBlock entry [instruction] (Ret (v 1))]) in check ((match find entry cfg with Some block -> not changed && block.instrs = [instruction] | None -> false)) "Expected SCCP to leave non-integer binary operations overdefined"
let testPartialRedundancyEliminationSupportsFloatAndNarrowValues () =
 checkCases (fun typ -> let entry = label "typed_pre_entry" and left = label "typed_pre_left" and right = label "typed_pre_right" and join = label "typed_pre_join" in
 let expression dest = BinOp (dest, Add, v 0, v 1, typ) in
 let cfg, changed = applyCSE (graph entry [basicBlock entry [] (Branch (v 2, left, right));basicBlock left [expression (r 3)] (Jump join);basicBlock right [] (Jump join);basicBlock join [expression (r 4)] (Ret (v 4))]) in
 check ((match find right cfg, find join cfg with Some rightBlock, Some joinBlock -> changed && rightBlock.instrs = [expression (r 5)] && joinBlock.instrs = [Phi (r 4, [v 5, right;v 3, left], Some typ)] | _ -> false)) ("Expected PRE to complete a partially redundant " ^ StructuralFormat.semanticType typ ^ " addition")) [AST.TFloat64;AST.TInt8]
let operandText operand = StructuralFormat.format (MIRTestFormatting.operand operand)
let testSelfComparisonFoldingRequiresConcreteSafeType () =
 let operand = v 0 in let cases = ["generic equality", Eq, AST.TVar "a";"float equality", Eq, AST.TFloat64;"string equality", Eq, AST.TString;"generic less-than", Lt, AST.TVar "a";"float less-than", Lt, AST.TFloat64;"generic greater-or-equal", Gte, AST.TVar "a";"float greater-or-equal", Gte, AST.TFloat64] in
 let folded = List.filter_map (fun (name, op, typ) -> Option.map (fun result -> name ^ " folded to " ^ operandText result) (tryFoldBinOp op operand operand typ)) cases in match folded with [] -> Ok () | first :: _ -> Error ("Expected self-comparison with non-concrete-safe type to stay unfolded, but " ^ first)
let testSelfComparisonFoldingRequiresSameRegister () =
 let cases = ["equality", Eq;"inequality", Neq;"less-than", Lt;"greater-than", Gt;"less-or-equal", Lte;"greater-or-equal", Gte] in
 let folded = List.filter_map (fun (name, op) -> Option.map (fun result -> name ^ " folded to " ^ operandText result) (tryFoldBinOp op (v 1) (v 0) AST.TInt64)) cases in match folded with [] -> Ok () | first :: _ -> Error ("Expected comparison of distinct registers to stay unfolded, but " ^ first)
let testLicmCanonicalizesMultipleLoopEntries () =
 let entry = label "entry" and left = label "left" and right = label "right" and header = label "header" and latch = label "latch" and exit = label "exit" and preheader = label "header_preheader" in
 let invariant = BinOp (r 3, Mul, v 0, v 1, AST.TInt64) in
 let cfg, changed = applyLoopInvariantCodeMotion (graph entry [basicBlock entry [] (Branch (v 0, left, right));basicBlock left [] (Jump header);basicBlock right [] (Jump header);basicBlock header [Phi (r 2, [v 1, left;v 1, right;v 4, latch], Some AST.TInt64);invariant] (Branch (v 5, latch, exit));basicBlock latch [] (Jump header);basicBlock exit [] (Ret (v 3))]) in
 match find preheader cfg, find header cfg with Some preBlock, Some headBlock ->
  let entries = List.for_all (fun label -> match find label cfg with Some block -> block.terminator = Jump preheader | None -> false) [left;right] in
  let phis = match headBlock.instrs with Phi (_, [(Register preValue, source);(Register (VReg 4), latchSource)], _) :: rest -> source = preheader && latchSource = latch && preBlock.instrs = [Phi (preValue, [v 1, left;v 1, right], Some AST.TInt64);invariant] && not (List.mem invariant rest) | _ -> false in
  check (changed && entries && phis) "Expected multiple loop entries to merge through a deterministic preheader and hoist the invariant multiply"
 | _ -> Error "Expected a dedicated preheader and preserved loop header"
let testLicmRetainsExistingLoopPreheader () =
 let entry = label "entry" and preheader = label "preheader" and header = label "header" and latch = label "latch" and exit = label "exit" in let invariant = BinOp (r 2, Mul, v 0, v 1, AST.TInt64) in
 let cfg, changed = applyLoopInvariantCodeMotion (graph entry [basicBlock entry [] (Jump preheader);basicBlock preheader [] (Jump header);basicBlock header [invariant] (Branch (v 3, latch, exit));basicBlock latch [] (Jump header);basicBlock exit [] (Ret (v 2))]) in
 check ((match find preheader cfg, find header cfg with Some preBlock, Some headBlock -> changed && preBlock.instrs = [invariant] && headBlock.instrs = [] | _ -> false)) "Expected the existing preheader to be retained and used for LICM"
let testLicmCanonicalizesNestedLoopEntry () =
 let entry = label "entry" and left = label "left" and right = label "right" and outer = label "outer_header" and inner = label "inner_header" and innerLatch = label "inner_latch" and outerLatch = label "outer_latch" and outerExit = label "outer_exit" and preheader = label "inner_header_preheader" in
 let invariant = BinOp (r 3, Mul, v 0, v 1, AST.TInt64) in
 let cfg, changed = applyLoopInvariantCodeMotion (graph entry [basicBlock entry [] (Branch (v 5, left, right));basicBlock left [] (Jump outer);basicBlock right [] (Jump outer);basicBlock outer [] (Branch (v 2, inner, outerExit));basicBlock inner [invariant] (Branch (v 4, innerLatch, outerLatch));basicBlock innerLatch [] (Jump inner);basicBlock outerLatch [] (Jump outer);basicBlock outerExit [] (Ret (v 3))]) in
 check ((match find preheader cfg, find outer cfg, find inner cfg with Some preBlock, Some outerBlock, Some innerBlock -> changed && outerBlock.terminator = Branch (v 2, preheader, outerExit) && preBlock.instrs = [invariant] && innerBlock.instrs = [] | _ -> false)) "Expected nested-loop entry canonicalization to preserve the outer loop and hoist the inner invariant"
let testLicmHoistsFloatUnaryAndConversionFamilies () =
 checkCases (fun (name, make) -> let entry = label (name ^ "_entry") and preheader = label (name ^ "_preheader") and header = label (name ^ "_header") and latch = label (name ^ "_latch") and exit = label (name ^ "_exit") in
 let invariant = make (r 2) (v 0) in
 let cfg, changed = applyLoopInvariantCodeMotion (graph entry [basicBlock entry [] (Jump preheader);basicBlock preheader [] (Jump header);basicBlock header [invariant] (Branch (v 1, latch, exit));basicBlock latch [] (Jump header);basicBlock exit [] (Ret (v 2))]) in
 check ((match find preheader cfg, find header cfg with Some preBlock, Some headBlock -> changed && preBlock.instrs = [invariant] && headBlock.instrs = [] | _ -> false)) ("Expected LICM to hoist invariant " ^ name ^ " work into the preheader")) pureScalarInstructionCases
let testCountedLoopUnrollingSupportsNarrowSignedAndUnsignedValues () =
 checkCases (fun typ -> let preheader = label "narrow_preheader" and header = label "narrow_header" and latch = label "narrow_latch" and exit = label "narrow_exit" in
 let cfg, changed = applyCountedLoopUnrolling (graph preheader [basicBlock preheader [] (Jump header);basicBlock header [Phi (r 0, [Int64Const 0L, preheader;v 3, latch], Some AST.TInt64);Phi (r 1, [Int64Const 1L, preheader;v 4, latch], Some typ);BinOp (r 2, Gte, v 0, Int64Const 4L, AST.TInt64)] (Branch (v 2, exit, latch));basicBlock latch [BinOp (r 4, Add, v 1, Int64Const 1L, typ);BinOp (r 3, Add, v 0, Int64Const 1L, AST.TInt64)] (Jump header);basicBlock exit [] (Ret (v 1))]) in
 let second = LabelMap.exists (fun (Label name) block -> String.starts_with ~prefix:"narrow_latch_unroll_second" name && List.exists (function BinOp (_, Add, _, _, valueType) when valueType = typ -> true | _ -> false) block.instrs) cfg.blocks in
 check (changed && second) ("Expected counted-loop unrolling to clone " ^ StructuralFormat.semanticType typ ^ " scalar work")) [AST.TInt8;AST.TUInt8]
let tests = [
 "MIR CSE reuses effect-free direct scalar calls", testCseReusesEffectFreeDirectScalarCalls;
 "MIR CSE reuses dominating effect-free direct scalar calls", testCseReusesDominatingEffectFreeDirectScalarCalls;
 "MIR CSE direct calls respect barriers and scalar types", testCseDirectCallsRespectBarriersAndScalarTypes;
 "MIR CSE does not reuse throwing direct calls", testCseDoesNotReuseThrowingDirectCalls;
 "MIR optimize fixed point CSE after copy prop", testCseAfterCopyPropFixpoint;
 "MIR CSE reuses dominating binary and unary expressions", testCseReusesDominatingExpressions;
 "MIR PRE completes an expression missing on one incoming path", testPartialRedundancyEliminationCompletesMissingPath;
 "MIR unary PRE completes an expression missing on one incoming path", testUnaryPartialRedundancyEliminationCompletesMissingPath;
 "MIR PRE supports Float and narrow scalar values", testPartialRedundancyEliminationSupportsFloatAndNarrowValues;
 "MIR CSE reuses dominating scalar heap loads", testCseReusesDominatingScalarHeapLoad;
 "MIR CSE scalar heap load barriers", testCseDoesNotReuseDominatingScalarHeapLoadAcrossBarriers;
 "MIR CSE preserves binary and unary expressions across siblings", testCsePreservesExpressionsAcrossSiblingBlocks;
 "MIR CSE invalidates expressions at reference-count decrements", testCseDoesNotReuseExpressionsAcrossRefCountDecrement;
 "MIR CSE does not extend expressions across calls", testCseDoesNotExtendExpressionsAcrossCalls;
 "MIR CSE does not export non-scalar binary types", testCseDoesNotExportNonScalarBinaryTypes;
 "MIR CSE keeps scalar heap loads available across pure scalar instructions", testCseKeepsScalarHeapLoadsAvailableAcrossPureScalarInstructions;
 "MIR CSE does not export scalar heap loads across pure scalar instructions", testCseDoesNotExportScalarHeapLoadsAcrossPureScalarInstructions;
 "MIR CSE does not keep direct calls available across pure scalar instructions", testCseDoesNotKeepDirectCallsAvailableAcrossPureScalarInstructions;
 "MIR optimize removes dead self-referential phi", testDceRemovesSelfReferentialDeadPhi;
 "MIR optimize removes ret-phi join blocks", testCfgSimplifyRemovesRetPhiJoin;
 "MIR optimize collapses copy-wrapped ret-phi chains", testCfgSimplifyCollapsesCopyWrappedRetPhiChain;
 "MIR empty block removal rewrites phi source to predecessor", testEmptyBlockRemovalRewritesPhiSourceToPredecessor;
 "MIR linear block merge preserves phi sources", testLinearBlockMergePreservesPhiSources;
 "MIR linear block merge exposes local CSE", testLinearBlockMergeExposesLocalCSE;
 "MIR same-target branch becomes jump and drops condition", testSameTargetBranchBecomesJumpAndDropsCondition;
 "MIR SCCP propagates phi constants and removes unreachable edges", testSccpPropagatesPhiConstantAndRemovesUnreachableEdge;
 "MIR SCCP ignores non-executable phi inputs", testSccpPhiIgnoresNonExecutableIncomingEdge;
 "MIR SCCP combines copy folding and dead-edge pruning", testSccpCombinesCopyFoldingAndDeadEdgePruning;
 "MIR SCCP propagates copied negation facts", testSccpPropagatesNegatedBooleanThroughCopy;
 "MIR SCCP widens loop values after executable backedges", testSccpLoopBackedgeWidensInductionValue;
 "MIR SCCP tracks Float and String constants without bypass", testSccpTracksFloatAndStringConstantsWithoutBypass;
 "MIR SCCP stabilizes NaN constants", testSccpStabilizesNanConstants;
 "MIR SCCP tracks aggregate constructor fields", testSccpTracksAggregateConstructorFields;
 "MIR SCCP uses call-result ranges", testSccpUsesCallResultRange;
 "MIR SCCP propagates constant call results", testSccpPropagatesConstantCallResult;
 "MIR SCCP does not apply integer folds to float operations", testSccpDoesNotApplyIntegerFoldsToFloatOperations;
 "MIR self-comparison folding requires concrete safe type", testSelfComparisonFoldingRequiresConcreteSafeType;
 "MIR self-comparison folding requires same register", testSelfComparisonFoldingRequiresSameRegister;
 "MIR LICM canonicalizes multiple loop entries", testLicmCanonicalizesMultipleLoopEntries;
 "MIR LICM retains existing loop preheader", testLicmRetainsExistingLoopPreheader;
 "MIR LICM canonicalizes nested loop entry", testLicmCanonicalizesNestedLoopEntry;
 "MIR LICM hoists Float unary and conversion families", testLicmHoistsFloatUnaryAndConversionFamilies;
 "MIR counted-loop unrolling supports narrow signed and unsigned values", testCountedLoopUnrollingSupportsNarrowSignedAndUnsignedValues
]
