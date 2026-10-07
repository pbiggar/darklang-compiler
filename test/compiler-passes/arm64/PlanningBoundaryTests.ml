(*
   PlanningBoundaryTests.fs - Verify code-generation planning preconditions and cost attribution.
*)
[@@@warning "-4-42"]
open Dark_compiler
open Fixtures
let testRejectsUnpreparedCodegenFacts ()=
 let unprepared=makeSimpleProgramWithVariants [LIR.Mov (LIR.Physical LIR.X0,LIR.Imm 1L)] StringOrder.Map.empty in
 let commonFactsOnly=LIR.attachCodegenFacts unprepared in
 match Backend_Arm64_CodeGen.generateARM64 target unprepared,Backend_Arm64_CodeGen.generateARM64 target commonFactsOnly with
 |Error missingFacts,Error missingPlan when Text.contains missingFacts "has no codegen facts" && Text.contains missingPlan "has no ARM64 helper plan"->Ok ()
 |_->Error "Expected unprepared ARM64 LIR to be rejected"
let testLirOpExpansionRecorderAttributesGeneratedInstructions ()=
 let observations=Queue.create () in
 let ctx:ARM64CodeGenTypes.codeGenContext={ARM64CodeGenTypes.target;options=ARM64CodeGenTypes.defaultOptions;sumShapeRegistry=StringOrder.Map.empty;recordRegistry=StringOrder.Map.empty;rawSlotInitRetainTargets=None;closurePayloadSizes=StringOrder.Map.empty;closureCaptureTypes=StringOrder.Map.empty;functionNames=FunctionIdMap.empty;functionName="lir_op_profile";instructionSite="";stackSize=0;usedCalleeSaved=[];usedCalleeSavedF=[];heapOverflowLabel="__heap_oom_lir_op_profile";recordLirOpExpansion=Some (fun functionName opcode detail count ticks->Queue.add (functionName,opcode,detail,count,ticks) observations)} in
 let label=LIR.Label "lir_op_profile_entry" in let block:LIR.basicBlock={LIR.label;instrs=[LIR.PrintHeapString (LIR.Physical LIR.X0)];terminator=LIR.Ret} in
 match ARM64Blocks.convertBlock ctx "_epilogue_lir_op_profile" None block with
 |Error error->Error error|Ok _->match List.of_seq (Queue.to_seq observations) with
 |[("lir_op_profile","PrintHeapString","",16,ticks)] when ticks>=0L->Ok ()
 |_->Error "Expected one attributed 16-instruction PrintHeapString expansion"
