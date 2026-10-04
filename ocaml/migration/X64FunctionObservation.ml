(* Full x64 instruction dispatch, terminators, blocks and functions. *)
open Dark_compiler
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let result=function Ok xs->SemanticJson.union "FSharpResult" "Ok" [`List (List.map MachineISAObservation.x64Instr xs)]|Error e->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string e]
let call f=try tuple [`Bool false;result (f ())] with Failure e|Invalid_argument e->tuple [`Bool true;SemanticJson.string e]
let observe source=
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let fps=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15] in
 let types=[AST.TInt64;AST.TFloat64;AST.TString;AST.TBlob;AST.TInt;AST.TBool;AST.TChar;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TTuple [AST.TString;AST.TList AST.TInt64];AST.TList AST.TString;AST.TDict (AST.TString,AST.TList AST.TString);AST.TFunction ([AST.TInt64],AST.TString);AST.TStream AST.TString;AST.TRecord ("R",[]);AST.TSum ("S",[])] in
 let reg=LIR.Physical LIR.X19 and freg=LIR.FPhysical LIR.D0 in
 let fixtures=List.concat_map (fun reg -> LIRFixtures.instructionsWithRegisters source reg freg (LIR.Imm 1L) AST.TInt64) (List.map (fun phys -> LIR.Physical phys) physical@[LIR.Virtual (-1);LIR.Virtual 0]) @ List.concat_map (fun fp -> LIRFixtures.instructionsWithRegisters source reg fp (LIR.Reg reg) AST.TFloat64) (List.map (fun fp -> LIR.FPhysical fp) fps@[LIR.FVirtual (-2000);LIR.FVirtual (-1000);LIR.FVirtual (-1);LIR.FVirtual 0]) @ List.concat_map (fun operand -> LIRFixtures.instructionsWithRegisters source reg freg operand AST.TString) [LIR.Imm Int64.min_int;LIR.Imm Int64.max_int;LIR.Imm 0L;LIR.StringSymbol source;LIR.StringSymbol "";LIR.Reg reg;LIR.StackSlot (-32769);LIR.StackSlot 0;LIR.StackSlot 32768] @ List.concat_map (fun typ -> LIRFixtures.instructionsWithRegisters source reg freg (LIR.Reg reg) typ) types in

 let names=FunctionIdMap.ofList [AST.functionId 0L,source;AST.functionId 1L,"fn";AST.functionId (-1L),"largest"] in
 let records=StringOrder.Map.singleton "R" ["field",AST.TString] in
 let sums=StringOrder.Map.singleton "S" {MemoryModel.typeParams=[];payloads=[0,None;1,Some AST.TString];unaryPayloadTags=MemoryModel.IntSet.singleton 1} in
 let ctx leak={X64CodeGenTypes.functionName=source;stackSize=32;usedCalleeSaved=[LIR.X19;LIR.X20];enableLeakCheck=leak;recordRegistry=records;sumShapeRegistry=sums;functionNames=names} in
 let comparisons=[None;Some X64InstructionContext.IntegerComparison;Some X64InstructionContext.FloatComparison] in
 let dispatch=list (fun enabled->list (fun comparison->list (fun instruction->call (fun ()->X64Instructions.translateInstr comparison (ctx enabled) instruction)) fixtures) comparisons) [false;true] in
 let label name=LIR.Label name in
 let block name instructions terminator={LIR.label=label name;instrs=instructions;terminator} in
 let cfg entry blocks={LIR.entry=label entry;blocks=LIR.LabelMap.of_seq (List.to_seq (List.map (fun (block:LIR.basicBlock) -> block.LIR.label,block) blocks))} in
 let func name cfg stack saved={LIR.id=AST.functionId 0L;name;typedParams=[];cfg;stackSize=stack;usedCalleeSaved=saved;codegenFacts=None} in
 let regs=[LIR.Physical LIR.X0;LIR.Physical LIR.X19;LIR.Physical LIR.X30;LIR.Physical LIR.SP;LIR.Virtual (-1);LIR.Virtual 0] in
 let conditions=[LIR.EQ;LIR.NE;LIR.LT;LIR.GT;LIR.LE;LIR.GE;LIR.ULT;LIR.UGT;LIR.ULE;LIR.UGE] in
 let terminators=[LIR.Ret;LIR.Jump (label "a");LIR.Jump (label "missing")]@List.concat_map (fun reg -> [LIR.Branch (reg,label "a",label "b");LIR.BranchZero (reg,label "a",label "b")]@List.concat_map (fun bit -> [LIR.BranchBitZero (reg,bit,label "a",label "b");LIR.BranchBitNonZero (reg,bit,label "a",label "b")]) [-1;0;31;32;63;64;255]) regs@List.map (fun condition -> LIR.CondBranch (condition,label "a",label "b")) conditions in
 let instructions=LIRFixtures.instructionsWithRegisters source (LIR.Physical LIR.X19) (LIR.FPhysical LIR.D0) (LIR.Imm 1L) AST.TInt64 in
 let blocks=List.map (fun instruction -> block source [instruction] LIR.Ret) instructions@[block source [] LIR.Ret;block source [LIR.Mov (LIR.Virtual 0,LIR.Imm 1L);LIR.RefCountInc (LIR.Physical LIR.X19,8,LIR.GenericHeap,Some {MemoryModel.releasePlanCacheKey=None;releasePlan=None;sourceType=Some AST.TString});LIR.Exit] LIR.Ret] in
 let graphs=List.map (fun terminator -> cfg source [block source [] terminator;block "a" [] LIR.Ret;block "b" [] LIR.Ret]) terminators@[cfg "missing" [];cfg source [];cfg source [block source [] (LIR.Jump (label "missing"))];cfg source [block source [] (LIR.Jump (label "a"));block "a" [] (LIR.Jump (label source))];cfg source [block source [] LIR.Ret;block "unreachable" [LIR.HeapAlloc (LIR.Physical LIR.X19,8)] LIR.Ret]] in

 let terms=list (fun comparison->list (fun term->list (fun next->call (fun ()->X64Blocks.translateTerminator comparison "epilogue" next term)) [None;Some "a";Some "b";Some "epilogue"]) terminators) comparisons in
 let conditioned=List.concat_map (fun condition->List.map (fun instructions->block source instructions (LIR.CondBranch (condition,label "a",label "b"))) [[];[LIR.Cmp (reg,LIR.Imm 0L)];[LIR.FCmp (freg,freg)];[LIR.FCmp (freg,freg);LIR.Mov (reg,LIR.Imm 0L)];[LIR.FCmp (freg,freg);LIR.Cmp (reg,LIR.Imm 0L)];[LIR.Cmp (reg,LIR.Imm 0L);LIR.FCmp (freg,freg)]]) conditions in
 let lowered=list (fun enabled->
  let c=ctx enabled in
  let blockCases=list (fun currentBlock->list (fun next->call (fun ()->X64Blocks.translateBlock c "epilogue" next currentBlock)) [None;Some (block "a" [] LIR.Ret);Some (block "b" [] LIR.Ret)]) (blocks@conditioned) in
  let functions=list (fun graph->call (fun ()->X64Functions.translateFunction enabled records sums names (func source graph 32 [LIR.X19;LIR.X20]))) graphs in
  let individual=list (fun block->call (fun ()->X64Functions.translateFunction enabled records sums names (func source (cfg source [block]) 0 []))) blocks in
  let frames=list (fun name->list (fun stack->list (fun saved->call (fun ()->X64Functions.translateFunction enabled records sums names (func name (cfg source [block source [] LIR.Ret]) stack saved))) [[];[LIR.X19;LIR.X20];[LIR.X0;LIR.SP]]) [-1;0;8;16;32;32768]) [source;"_start";"Darklang.Stdlib.List.fn"] in
  tuple [blockCases;functions;individual;frames]) [false;true] in
 tuple [dispatch;terms;lowered]
