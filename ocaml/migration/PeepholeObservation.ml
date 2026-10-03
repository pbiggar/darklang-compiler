(* Full low-level simplification, cleanup, CFG and diamond observations. *)
[@@@warning "-4"]
open Dark_compiler
module L=LIR
module P=LIR_Peephole
let tuple values=`Assoc ["tuple",`List values]
let list fn values=`List (List.map fn values)
let option fn = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [fn value]
let pair fn (value,changed)=tuple [fn value;`Bool changed]
let instrs=list ProductionLIR.instr
let branchPair (is,term)=tuple [instrs is;ProductionLIR.terminator term]
let attempt fn action=try SemanticJson.union "FSharpResult" "Ok" [fn (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
(* Typed projections of all unchanged lir-peepholes.liropt inputs and expectations.
   The complete DSL parser and runner remain separate inventory components. *)
let dslFixtures = [
 "mul_add_left_fuses_to_madd", [L.Mul (L.Virtual 1, L.Virtual 2, L.Virtual 3);L.Add (L.Virtual 4, L.Virtual 1, L.Reg (L.Virtual 5))], [L.Madd (L.Virtual 4, L.Virtual 2, L.Virtual 3, L.Virtual 5)];
 "mul_add_right_fuses_to_madd", [L.Mul (L.Virtual 1, L.Virtual 2, L.Virtual 3);L.Add (L.Virtual 4, L.Virtual 5, L.Reg (L.Virtual 1))], [L.Madd (L.Virtual 4, L.Virtual 2, L.Virtual 3, L.Virtual 5)];
 "live_multiply_result_prevents_madd", [L.Mul (L.Virtual 1, L.Virtual 2, L.Virtual 3);L.Add (L.Virtual 4, L.Virtual 1, L.Reg (L.Virtual 5));L.PrintInt64 (L.Virtual 1)], [L.Mul (L.Virtual 1, L.Virtual 2, L.Virtual 3);L.Add (L.Virtual 4, L.Virtual 1, L.Reg (L.Virtual 5));L.PrintInt64 (L.Virtual 1)];
 "add_zero_retargets_destination", [L.Add (L.Virtual 2, L.Virtual 1, L.Imm 0L)], [L.Mov (L.Virtual 2, L.Reg (L.Virtual 1))];
 "add_zero_same_destination_disappears", [L.Add (L.Virtual 1, L.Virtual 1, L.Imm 0L)], [];
 "subtract_zero_retargets_destination", [L.Sub (L.Virtual 2, L.Virtual 1, L.Imm 0L)], [L.Mov (L.Virtual 2, L.Reg (L.Virtual 1))];
 "subtract_zero_same_destination_disappears", [L.Sub (L.Virtual 1, L.Virtual 1, L.Imm 0L)], [];
 "multiply_by_power_plus_one_right_strength_reduces", [L.Mov (L.Virtual 1, L.Imm 3L);L.Mul (L.Virtual 4, L.Virtual 2, L.Virtual 1)], [L.Lsl_imm (L.Virtual 1, L.Virtual 2, 1);L.Add (L.Virtual 4, L.Virtual 2, L.Reg (L.Virtual 1))];
 "multiply_by_power_plus_one_left_strength_reduces", [L.Mov (L.Virtual 1, L.Imm 3L);L.Mul (L.Virtual 4, L.Virtual 1, L.Virtual 2)], [L.Lsl_imm (L.Virtual 1, L.Virtual 2, 1);L.Add (L.Virtual 4, L.Virtual 2, L.Reg (L.Virtual 1))];
 "multiply_by_power_minus_one_right_strength_reduces", [L.Mov (L.Virtual 1, L.Imm 7L);L.Mul (L.Virtual 4, L.Virtual 2, L.Virtual 1)], [L.Lsl_imm (L.Virtual 1, L.Virtual 2, 3);L.Sub (L.Virtual 4, L.Virtual 1, L.Reg (L.Virtual 2))];
 "multiply_by_power_minus_one_left_strength_reduces", [L.Mov (L.Virtual 1, L.Imm 7L);L.Mul (L.Virtual 4, L.Virtual 1, L.Virtual 2)], [L.Lsl_imm (L.Virtual 1, L.Virtual 2, 3);L.Sub (L.Virtual 4, L.Virtual 1, L.Reg (L.Virtual 2))];
]
let observe source=
 let label=L.Label source in let yes=L.Label "yes" in let no=L.Label "no" in
 let block label instrs terminator : L.basicBlock={L.label;instrs;terminator} in
 let cfg label blocks : L.cfg={L.entry=label;blocks=L.LabelMap.of_list (List.map (fun (b:L.basicBlock) -> b.L.label,b) blocks)} in
 let func (block:L.basicBlock) : L.functionDef={L.id=AST.functionId 0L;name=source;typedParams=[];cfg=cfg label [block];stackSize=32;usedCalleeSaved=[L.X19];codegenFacts=None} in
 let roles mask=Array.init 4 (fun n -> L.Virtual ((mask lsr (n*2)) land 3)) in
 let floats physical roles=Array.map (function L.Virtual id -> if physical then L.FPhysical (List.nth FloatAllocation.allocatableFloatRegs id) else L.FVirtual id | L.Physical _ -> assert false) roles in
 let sequences roles fregs suffix=
  let r n=roles.(n) in let f n=fregs.(n) in
  [[L.FAdd (f 0,f 1,f 2);suffix;L.FMov (f 3,f 0)];[L.FMul (f 0,f 1,f 2);L.FAdd (f 3,f 0,f 2);suffix];[L.Mov (r 0,L.Imm 3L);L.Mul (r 1,r 2,r 0);suffix];[L.Mul (r 0,r 1,r 2);L.Add (r 3,r 0,L.Reg (r 2));suffix];[L.Sub (r 0,r 1,L.Imm 1L);suffix;L.Mov (r 1,L.Reg (r 0))];[L.FMov (f 0,f 1);suffix;L.FMov (f 1,f 0)]] in
 let observeSequence is=
  let block=block label is (L.Branch (L.Virtual 0,yes,no)) in let func=func block in
  tuple [instrs (P.optimizeInstrs is);instrs (P.removeSelfMovesFromInstrs is);instrs (P.sinkSeparatedAllocatedFAdds is);instrs (P.retargetSeparatedDeadFAdds is);instrs (P.removeRedundantFloatingCopyBackMoves is);option instrs (P.sinkImmediateCounterUpdate is);instrs (P.tryMulByConstant is);instrs (P.tryFuseMulAdd is);instrs (P.tryFuseMulSub is);pair instrs (P.tryFuseFloatMultiplyAdd is);pair ProductionLIR.basicBlock (P.optimizeBlock block);ProductionLIR.functionDef (P.removePostAllocationMovesFromFunction func);ProductionLIR.functionDef (P.optimizeAllocatedCounterUpdates func)] in
 let singles=list (fun instr -> tuple [option ProductionLIR.instr (P.optimizeInstr instr);`Bool (P.isCallInstr instr);`Bool (P.isPureLoopInstr instr);list (fun reg -> `Bool (P.isRegUsedInInstrs reg [instr])) [L.Virtual 0;L.Virtual 1;L.Virtual 2;L.Virtual 3]]) (AllocationFixtures.instructions source (roles 228) (floats false (roles 228)) (L.Reg (L.Virtual 3)) AST.TFloat64) in
 let sequenceCases=list (fun mask -> list (fun physical -> let regs=roles mask in let fregs=floats physical regs in list (fun suffix -> list observeSequence (sequences regs fregs suffix)) (AllocationFixtures.instructions source regs fregs (L.Reg regs.(3)) AST.TFloat64)) [false;true]) [0;228;27;85;170] in
 let arithmeticCases=list (fun mask -> list (fun physical -> let regs=roles mask in list observeSequence (sequences regs (floats physical regs) (L.PrintFloat (if physical then L.FPhysical L.D0 else L.FVirtual 0)))) [false;true]) (List.init 256 Fun.id) in
 let terms=[L.Ret;L.Jump yes;L.Branch (L.Virtual 0,yes,no);L.BranchZero (L.Virtual 0,yes,no);L.BranchBitZero (L.Virtual 0,3,yes,no);L.BranchBitNonZero (L.Virtual 0,3,yes,no)] @ List.map (fun c -> L.CondBranch (c,yes,no)) [L.EQ;L.NE;L.LT;L.GT;L.LE;L.GE;L.ULT;L.UGT;L.ULE;L.UGE] in
 let branchCases=list (fun mask -> let regs=roles mask in let r n=regs.(n) in
  let patterns=[[];[L.Cmp (r 1,L.Imm 0L);L.Cset (r 0,L.LT)];[L.Mov (r 0,L.Imm 1L);L.Sub (r 0,r 0,L.Reg (r 1))];[L.And_imm (r 0,r 1,8L)];[L.PrintInt64 (r 0);L.And_imm (r 0,r 1,8L)];[L.Cmp (r 1,L.Imm 1L)]] in
  list (fun is -> list (fun term -> list (fun count -> let counts=P.RegMap.singleton (L.Virtual 0) count in tuple [option branchPair (P.tryFuseCondBranch counts is term);option branchPair (P.tryFuseBooleanNotBranch counts is term);option branchPair (P.tryFuseAndBitBranch is term);option branchPair (P.tryFuseCmpZeroBranch is term);branchPair (P.applyAndBitBranchFusion is term)]) [0;1;2;3]) terms) patterns) [0;228;27;85;170] in
 let labels=[|L.Label "a";L.Label "b";L.Label "c"|] in
 let successors=[[];[0];[1];[2];[0;1];[0;2];[1;2]] in
 let graphCases=list (fun entry -> list (fun style -> list (fun code ->
  let blocks=List.init 3 (fun n ->
   let successors=List.nth successors ((code / (if n=0 then 1 else if n=1 then 7 else 49)) mod 7) in
   let term=match successors with [] -> L.Ret | [target] -> L.Jump labels.(target) | [a;b] -> L.Branch (L.Virtual 0,labels.(a),labels.(b)) | _ -> assert false in
   let is=match style with 0 -> [] | 1 -> [L.Mov (L.Virtual 0,L.Imm 7L);L.FLoad (L.FVirtual 0,-0.)] | 2 -> [L.Mov (L.Virtual 0,L.Imm 7L);L.Mov (L.Virtual 0,L.Imm 9L);L.FLoad (L.FVirtual 0,-0.)] | 3 -> [L.Call (L.Virtual 0,AST.functionId 3L,[])] | _ -> [L.PrintInt64 (L.Virtual 0)] in block labels.(n) is term) in
  let cfg=cfg labels.(entry) blocks in
  tuple [attempt ProductionLIR.cfg (fun () -> P.optimizeCFG cfg);pair ProductionLIR.cfg (P.formSelectDiamonds cfg)]) (List.init 343 Fun.id)) [0;1;2;3;4]) [0;1;2] in
 let types=None::List.map Option.some [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TBool;AST.TUnit;AST.TChar;AST.TDateTime;AST.TInternalRawPtr;AST.TFloat64;AST.TString;AST.TList AST.TInt64] in
 let diamonds=list (fun typ -> list (fun term -> list (fun shape -> list (fun arm ->
  let join=L.Label "join" in
  let phi=L.Phi (L.Virtual 3,[L.Reg (L.Virtual 1),yes;L.Reg (L.Virtual 2),no],typ) in
  let joinInstrs=match shape with 0 -> [phi] | 1 -> [phi;L.Phi (L.Virtual 4,[L.Reg (L.Virtual 2),yes;L.Reg (L.Virtual 1),no],typ)] | 2 -> [L.Phi (L.Virtual 3,[L.Imm 1L,yes;L.Reg (L.Virtual 2),no],typ)] | 3 -> [L.FPhi (L.FVirtual 3,[L.FVirtual 1,yes;L.FVirtual 2,no])] | 4 -> [L.PrintInt64 (L.Virtual 0);phi] | _ -> [phi;L.PrintInt64 (L.Virtual 3)] in
  let cfg=cfg label [block label [L.Cmp (L.Virtual 0,L.Imm 0L)] term;block yes (if arm=1 then [L.Mov (L.Virtual 0,L.Imm 1L)] else []) (L.Jump join);block no [] (if arm=2 then L.Ret else L.Jump join);block join joinInstrs L.Ret] in
  let cfg=if arm=3 then {cfg with L.blocks=L.LabelMap.add (L.Label "other") (block (L.Label "other") [] (L.Jump yes)) cfg.L.blocks} else cfg in
  tuple [pair ProductionLIR.cfg (P.formSelectDiamonds cfg);attempt ProductionLIR.cfg (fun () -> P.optimizeCFG cfg)]) [0;1;2;3]) [0;1;2;3;4;5]) (List.filter (function L.Branch _ | L.BranchZero _ | L.CondBranch _ -> true | _ -> false) terms)) types in
 let large=list (fun count -> let blocks=List.init count (fun n -> let label=L.Label (string_of_int n) in block label [L.Mov (L.Virtual n,L.Imm (Int64.of_int n))] (if n=count-1 then L.Jump (L.Label "1") else L.Jump (L.Label (string_of_int (n+1))))) in attempt ProductionLIR.cfg (fun () -> P.optimizeCFG (cfg (L.Label "0") blocks))) [2;65;129] in
 let malformed=list (fun bad -> attempt ProductionLIR.cfg (fun () -> P.optimizeCFG bad)) [{L.entry=label;blocks=L.LabelMap.empty};cfg label [block label [] (L.Jump yes)]] in
 let numeric=list (fun n -> tuple [SemanticJson.int32 n;option (fun (shift,pattern) -> tuple [SemanticJson.int32 shift;SemanticJson.union "MulConstantPattern" (match pattern with P.PowerOfTwoPlusOne -> "PowerOfTwoPlusOne" | P.PowerOfTwoMinusOne -> "PowerOfTwoMinusOne") []]) (P.tryMulConstantPattern (Int64.of_int n));`Bool (P.isPowerOf2 (Int64.of_int n))]) (List.init 257 (fun n -> n-128)) in
 let numeric64=list (fun n -> tuple [`Assoc ["kind",`String "int64";"value",`String (Int64.to_string n)];`Bool (P.isPowerOf2 n);option SemanticJson.int32 (if P.isPowerOf2 n then Some (P.bitPosition n) else None)]) ([Int64.min_int;Int64.max_int;(-1L);0L] @ List.init 63 (fun n -> Int64.shift_left 1L n)) in
 let dsl=list (fun (name,input,expected) -> let program is=let entry=L.Label "entry" in let b=block entry is L.Ret in L.Program ([{(func b) with L.id=AST.functionId 0L;name="_start";cfg=cfg entry [b];stackSize=0;usedCalleeSaved=[]}],StringOrder.Map.empty,StringOrder.Map.empty) in let actual=P.optimizeProgram (program input) in tuple [SemanticJson.string name;ProductionLIR.program (program input);ProductionLIR.program (program expected);ProductionLIR.program actual;`Bool (actual=program expected)]) dslFixtures in
 tuple [singles;sequenceCases;arithmeticCases;branchCases;graphCases;diamonds;large;malformed;numeric;numeric64;dsl]
