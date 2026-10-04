(* Full block/function lowering and profiling metadata; validate variable elapsed time. *)
open Dark_compiler
module C=ARM64CodeGenTypes
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code xs=list MachineISAObservation.symInstr xs
let result=function Ok xs -> SemanticJson.union "FSharpResult" "Ok" [code xs] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let call f=try tuple [`Bool false;result (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let observe source=
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
 let observing ctx action=
  let trace=ref [] in
  let record name opcode detail count ticks=trace:=(name,opcode,detail,count,Int64.compare ticks 0L>=0):: !trace in
  let result=call (fun () -> action {ctx with C.recordLirOpExpansion=Some record}) in
  tuple [result;list (fun (name,opcode,detail,count,elapsedValid) -> tuple [SemanticJson.string name;SemanticJson.string opcode;SemanticJson.string detail;SemanticJson.int32 count;`Bool elapsedValid]) (List.rev !trace)]
 in
 let terms=list (fun terminator -> list (fun next -> call (fun () -> ARM64Blocks.convertTerminator "epilogue" next terminator)) [None;Some "a";Some "b";Some "epilogue"]) terminators in
 let contexts=list (fun target -> list (fun leak -> list (fun coverage ->
  let ctx={ (ARMPrintingObservation.context source target leak) with C.options={C.defaultOptions with C.enableLeakCheck=leak;enableCoverage=coverage;coverageExprCount=3};functionNames=FunctionIdMap.ofList [AST.functionId 0L,source;AST.functionId 1L,"fn";AST.functionId (-1L),"largest"]} in
  let loweredBlocks=list (fun currentBlock -> list (fun next -> observing ctx (fun ctx -> ARM64Blocks.convertBlock ctx "epilogue" next currentBlock)) [None;Some (block "a" [] LIR.Ret);Some (block "b" [] LIR.Ret)]) blocks in
  let loweredGraphs=list (fun graph -> observing ctx (fun ctx -> ARM64Blocks.convertCFG ctx "epilogue" graph)) graphs in
  let trap=HeapAllocation.preparedHeapOverflowTrapBody target in
  let allFunctions=list (fun block -> let f=func source (cfg source [block]) 0 [] in tuple [observing ctx (fun ctx -> ARM64Functions.convertFunction trap ctx f);observing ctx (fun ctx -> ARM64Functions.convertFunction trap ctx (LIR.attachFunctionCodegenFacts f))]) blocks in
  let frames=list (fun name -> list (fun stack -> list (fun saved -> let f=func name (cfg source [block source [] LIR.Ret]) stack saved in list (fun facts -> observing ctx (fun ctx -> ARM64Functions.convertFunction trap ctx {f with LIR.codegenFacts=facts})) [None;Some (LIR.analyzeFunctionCodegenFacts f);Some {(LIR.analyzeFunctionCodegenFacts f) with LIR.arm64UsedCalleeSavedF=[LIR.D8;LIR.D15]}]) [[];[LIR.X19;LIR.X20];[LIR.X0;LIR.SP]]) [-1;0;8;16;32;32768]) [source;"_start";"Darklang.Stdlib.List.fn"] in
  tuple [loweredBlocks;loweredGraphs;allFunctions;frames]) [false;true]) [false;true]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64] in
 tuple [terms;contexts]
