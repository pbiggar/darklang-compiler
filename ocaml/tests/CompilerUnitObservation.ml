(* Compare original compiler unit registration and complete result diagnostics. *)
open Dark_compiler
module J=Semantic_observation.SemanticJson
let tuple=J.tuple
let result=function Ok ()->J.union "FSharpResult" "Ok" [`Null]|Error error->J.union "FSharpResult" "Error" [J.string error]
let tests values=`List (List.map (fun (name,run)->let outcome=run () in tuple [J.string name;result outcome]) values)
let stdlib=lazy (Result.bind (Platform.detectHostTarget ()) StdlibCompilation.buildStdlib)
let arm64ContractTests=[
 "LIR ARM64 codegen reports missing entry block",ControlFlowTests.testReportsMissingEntryBlock;
 "ARM64 compact record fields start at offset zero",ReleasePlanningTests.testCompactRecordFieldsStartAtOffsetZero;
 "Generated ARM64 entry uses allocator transfers only",ControlFlowTests.testGeneratedEntryUsesAllocatorTransfersOnly;
 "ARM64 UInt64 runtime zero branches target digit handlers",ControlFlowTests.testPrintUInt64RuntimeZeroBranches;
 "ARM64 UInt64 runtime preserves trailing newline",ControlFlowTests.testPrintUInt64RuntimePreservesNewline;
 "ARM64 branch false edge falls through",ControlFlowTests.testBranchFalseEdgeFallsThrough;
 "ARM64 common-return transfer cost",ControlFlowTests.testSharedReturnTransferCost;
 "ARM64 dynamic buffer RC instruction cost",ControlFlowTests.testDynamicBufferRcInstructionCost;
 "ARM64 primitive list payload preservation cost",ControlFlowTests.testPrimitiveListPayloadPreservationCost;
 "Small generic release plan remains inline",ReleasePlanningTests.testSmallGenericReleasePlanRemainsInline;
 "Expensive generic release is prepared as a call",ReleasePlanningTests.testExpensiveGenericReleaseIsPreparedAsCall;
 "Generic release helper preserves cached instructions",ReleasePlanningTests.testGenericReleaseHelperPreservesCachedInstructions;
 "Outlined generic release uses allocator liveness",ReleasePlanningTests.testOutlinedGenericReleaseUsesAllocatorLiveness;
 "Generic release helpers preserve ownership policy",ReleasePlanningTests.testGenericReleaseHelpersPreserveOwnershipPolicy;
 "Dict list value planned helper releases collision payloads",DestructionTests.testDictListValuePlannedHelperReleasesCollisionPayloads;
 "Dict tuple value planned helper releases collision payloads",DestructionTests.testDictTupleValuePlannedHelperReleasesCollisionPayloads;
 "Dict string key tuple value planned helper releases collision payloads",DestructionTests.testDictStringKeyTupleValuePlannedHelperReleasesCollisionPayloads;
 "Generic fixed-block nested bytes field uses release plan",DestructionTests.testGenericFixedBlockNestedBytesFieldUsesReleasePlan;
 "Planned list generic leaf release reloads block pointer",DestructionTests.testPlannedListGenericLeafReleaseReloadsBlockPointer;
 "Planned list nested generic release preserves block pointer",DestructionTests.testPlannedListNestedGenericReleasePreservesBlockPointer;
 "Planned list tuple payload uses planned helper",DestructionTests.testPlannedListTuplePayloadUsesPlannedHelper;
 "Planned list record payload uses planned helper",DestructionTests.testPlannedListRecordPayloadUsesPlannedHelper;
 "Planned list record nested string-dict uses planned list helper",DestructionTests.testPlannedListRecordNestedStringDictUsesPlannedListHelper;
 "Planned list tuple5 payload uses planned helper",DestructionTests.testPlannedListTuple5PayloadUsesPlannedHelper;
 "Planned list record5 payload uses planned helper",DestructionTests.testPlannedListRecord5PayloadUsesPlannedHelper;
 "Generic fixed-block nested immediate field releases child root",DestructionTests.testGenericFixedBlockNestedImmediateFieldReleasesChildRoot;
 "Generic fixed-block nested mixed boxed-sum bytes payload uses variant dispatch",DestructionTests.testGenericFixedBlockNestedMixedBoxedSumBytesPayloadUsesVariantDispatch;
 "Generic mixed boxed-sum payload dispatch skips remaining cases",DestructionTests.testGenericMixedBoxedSumPayloadDispatchSkipsRemainingCases;
 "Recursive-sum release skips variants without managed fields",DestructionTests.testRecursiveSumReleaseSkipsVariantWithoutManagedFields;
 "Closure capture nested fixed-block bytes field uses release plan",DestructionTests.testClosureCaptureNestedFixedBlockBytesFieldUsesReleasePlan;
 "Closure capture boxed-sum bytes payload uses release plan",DestructionTests.testClosureCaptureBoxedSumBytesPayloadUsesReleasePlan;
]
let fixed=lazy (
 let prepared=match Lazy.force stdlib with Error error->J.union "FSharpResult" "Error" [J.string error]|Ok stdlib->J.union "FSharpResult" "Ok" [tuple [tests (ProgramStructureTests.tests stdlib);tests (ValueSearchCatalogTests.tests stdlib);tests (JsonPlanningTests.tests stdlib);tests (StdlibOptimizationTests.tests stdlib);tests (CompilationSessionTests.tests Platform.LinuxX86_64 stdlib)]] in
 [tests arm64ContractTests;tests ["ARM64 codegen rejects unprepared facts",PlanningBoundaryTests.testRejectsUnpreparedCodegenFacts;"ARM64 codegen attributes LIR opcode expansion",PlanningBoundaryTests.testLirOpExpansionRecorderAttributesGeneratedInstructions];tests StdlibSourceTests.tests;tests ScriptHelperTests.tests;tests ASTToANFTests.tests;tests ListHIRTests.tests;tests SyntaxDSLTests.tests;tests ChordalGraphTests.tests;`List (List.map (fun (name,outcome)->tuple [J.string name;result outcome]) (ChordalGraphTests.runAllTests ()));tests SSALivenessTests.tests;result (SSALivenessTests.runAll ());tests TypeCheckingTests.tests;result (TypeCheckingTests.runAll ());prepared;tests RuntimeDataLayoutTests.tests;tests X86_64ResolveTests.tests;tests LambdaLiftingTests.tests;tests MonomorphizationTests.tests;tests IRPrinterTests.tests;result (IRPrinterTests.runAll ());tests IRSymbolTests.tests;result (IRSymbolTests.runAll ());tests DeadCodeEliminationTests.tests;result (DeadCodeEliminationTests.runAll ())])
let observe source=tuple (J.string source::(Lazy.force fixed @ [`List (List.map (fun instr->J.string (HostStructuralFormat.format (LIRTestFormatting.instr instr))) (Semantic_observation.LIRFixtures.instructions source))]))
