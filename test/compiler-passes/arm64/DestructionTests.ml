(*
   DestructionTests.ml - Verify recursive payload destruction and helper register preservation.
   Recursive-nominal release dispatch shape is not observable in an executable
   E2E test. A variant without managed fields must not consume a tag case in
   the generated helper, while the recursive variant must remain dispatched.
*)
[@@@warning "-4-42"]
open Dark_compiler
open Fixtures
module L=LIR
module S=Symbolic
module M=StringOrder.Map
let (let*)=Result.bind
let require condition error=if condition then Ok () else Error error
let releaseInstr size kind metadata=L.RefCountDec (L.Physical L.X0,size,kind,Some metadata)
let release size kind typ=releaseInstr size kind (rcMetadata typ)
let generate instr variants=makeSimpleProgramWithVariants [instr] variants |> generatePreparedARM64 target
let hasLabel fragment instrs=List.exists (function S.Label label->Text.contains label fragment|_->false) instrs
let collision typ label error=let* instrs=generate (release 0 L.DictHeap typ) M.empty in require (hasLabel label instrs) error
let testDictListValuePlannedHelperReleasesCollisionPayloads ()=collision (AST.TDict (AST.TInt64,AST.TList AST.TInt64)) "collision_root_payload_loop" "Dict<int, list<int>> planned helper did not emit a collision root payload release loop"
let testDictTupleValuePlannedHelperReleasesCollisionPayloads ()=collision (AST.TDict (AST.TInt64,AST.TTuple [AST.TString;AST.TList AST.TInt64])) "collision_generic_payload_loop" "Dict<int, tuple<string, list<int>>> planned helper did not emit a collision generic payload release loop"
let testDictStringKeyTupleValuePlannedHelperReleasesCollisionPayloads ()=collision (AST.TDict (AST.TString,AST.TTuple [AST.TString;AST.TList AST.TInt64])) "collision_generic_payload_loop" "Dict<string, tuple<string, list<int>>> planned helper did not emit a collision generic payload release loop"
let testGenericFixedBlockNestedBytesFieldUsesReleasePlan ()=
 let* instrs=generate (release 8 L.GenericHeap (AST.TTuple [AST.TTuple [AST.TBlob]])) M.empty in
 let* ()=require (List.mem (S.LDR (S.X12,S.X11,0)) instrs) "Generic fixed-block nested bytes field release did not consume the nested release plan" in
 require (List.mem (S.STP_pre (S.X10,S.X11,S.SP,-48)) instrs) "Generic fixed-block nested release did not preserve X11 while using it as child base"
let testPlannedListGenericLeafReleaseReloadsBlockPointer ()=
 let* instrs=generate (release 0 L.TaggedList (AST.TList (AST.TTuple [AST.TString;AST.TInt64]))) M.empty in
 require (List.mem (S.LDR (S.X8,S.X3,0)) instrs) "ARM64 planned list generic release did not reload the leaf pointer before freeing it"
let testPlannedListNestedGenericReleasePreservesBlockPointer ()=
 let* instrs=generate (release 0 L.TaggedList (AST.TList (AST.TTuple [AST.TTuple [AST.TString;AST.TInt64];AST.TInt64]))) M.empty in
 require (List.mem (S.STP_pre (S.X12,S.X30,S.SP,-16)) instrs) "ARM64 planned list nested generic release did not preserve the block pointer across nested field releases"
let plannedTuple typ error=let* instrs=generate (release 0 L.TaggedList (AST.TList typ)) M.empty in require (ReleasePlanningTests.emitsPlannedListHelperLabel instrs) error
let testPlannedListTuplePayloadUsesPlannedHelper ()=plannedTuple (AST.TTuple [AST.TString;AST.TList AST.TInt64;AST.TDict (AST.TInt64,AST.TInt64)]) "ARM64 tuple list payload did not emit a planned list helper"
let recordList name fields=let records=M.singleton name fields in makeSimpleProgramWithRecords [releaseInstr 0 L.TaggedList (rcMetadataWithRecords records (AST.TList (AST.TRecord (name,[]))))] records |> generatePreparedARM64 target
let testPlannedListRecordPayloadUsesPlannedHelper ()=
 let* instrs=recordList "ARM64PlannedListRecordPayload" ["name",AST.TString;"items",AST.TList AST.TInt64] in require (ReleasePlanningTests.emitsPlannedListHelperLabel instrs) "ARM64 record list payload did not emit a planned list helper"
let testPlannedListRecordNestedStringDictUsesPlannedListHelper ()=
 let* instrs=recordList "ARM64PlannedListRecordNestedStringDict" ["items",AST.TList (AST.TDict (AST.TString,AST.TString))] in
 if List.mem (S.BL "__dark_list_refcount_dec_dict_helper") instrs then Error "Nested List<Dict<String, String>> called the unplanned legacy list/dict helper" else
 let count=List.filter_map (function S.Label label when Text.startsWith label "__dark_list_refcount_dec_plan_"->Some label|_->None) instrs |> StringOrder.Set.of_list |> StringOrder.Set.cardinal in
 if count<2 then Error (Printf.sprintf "Expected outer-record and inner-dict planned list helpers, found %d" count) else Ok ()
let testPlannedListTuple5PayloadUsesPlannedHelper ()=plannedTuple (AST.TTuple [AST.TString;AST.TBlob;AST.TList AST.TInt64;AST.TDict (AST.TInt64,AST.TList AST.TInt64);AST.TFunction ([AST.TInt64],AST.TInt64)]) "ARM64 tuple5 list payload did not emit a planned list helper"
let testPlannedListRecord5PayloadUsesPlannedHelper ()=
 let* instrs=recordList "ARM64PlannedListRecord5Payload" ["name",AST.TString;"blob",AST.TBlob;"items",AST.TList AST.TInt64;"lookup",AST.TDict (AST.TInt64,AST.TList AST.TInt64);"fn",AST.TFunction ([AST.TInt64],AST.TInt64)] in
 require (ReleasePlanningTests.emitsPlannedListHelperLabel instrs) "ARM64 record5 list payload did not emit a planned list helper"
let testGenericFixedBlockNestedImmediateFieldReleasesChildRoot ()=
 let* instrs=generate (release 8 L.GenericHeap (AST.TTuple [AST.TTuple [AST.TInt64]])) M.empty in require (List.mem (S.LDR (S.X12,S.X0,0)) instrs) "Generic fixed-block nested immediate field release did not release the child root"
let variant name tag payload fieldCount:L.variantInfo={L.name;tag;payload;fieldCount}
let variants name cases=M.singleton name {L.typeParams=[];variants=cases}
let sumMetadata registry typ=
 let sums=M.map (fun (v:L.typeVariants)->{MemoryModel.typeParams=v.L.typeParams;unaryPayloadTags=List.filter_map (fun (v:L.variantInfo)->if v.L.fieldCount=1 then Some v.L.tag else None) v.L.variants |> MemoryModel.IntSet.of_list;payloads=List.sort (fun (a:L.variantInfo) (b:L.variantInfo)->Int.compare a.L.tag b.L.tag) v.L.variants |> List.map (fun (v:L.variantInfo)->v.L.tag,v.L.payload)}) registry in rcMetadataWithSumShapes sums typ
let testGenericFixedBlockNestedMixedBoxedSumBytesPayloadUsesVariantDispatch ()=
 let name="Arm64NestedFixedBlockSumBytes" in let typ=AST.TSum (name,[]) in let parent=AST.TTuple [typ] in
 let registry=variants name [variant "Arm64NestedFixedBlockNoPayload" 0 None 0;variant "Arm64NestedFixedBlockSumListPayload" 1 (Some (AST.TList AST.TBlob)) 1;variant "Arm64NestedFixedBlockSumBlobPayload" 2 (Some AST.TBlob) 1] in
 let* instrs=generate (releaseInstr 8 L.GenericHeap (sumMetadata registry parent)) registry in require (List.mem (S.LDR (S.X10,S.X11,0)) instrs) "Generic fixed-block nested mixed boxed-sum payload release did not dispatch on the child variant tag"
let testGenericMixedBoxedSumPayloadDispatchSkipsRemainingCases ()=
 let name="Arm64MixedSumPayloadDispatch" in let typ=AST.TSum (name,[]) in
 let registry=variants name [variant "Arm64MixedSumBytesPayload" 0 (Some AST.TBlob) 1;variant "Arm64MixedSumListPayload" 1 (Some (AST.TList AST.TInt64)) 1] in
 let* instrs=generate (releaseInstr 16 L.GenericHeap (sumMetadata registry typ)) registry in
 let rec branch seen=function []->false|S.CMP_imm (S.X10,0)::rest->branch true rest|S.CMP_imm (S.X10,1)::_ when seen->false|S.B _::_ when seen->true|_::rest->branch seen rest in
 require (branch false instrs) "Generic mixed boxed-sum payload release did not branch past remaining variant cases after a match"
let testRecursiveSumReleaseSkipsVariantWithoutManagedFields ()=
 let name="Arm64RecursiveReleaseTree" in let typ=AST.TSum (name,[]) in
 let registry=variants name [variant "Arm64RecursiveReleaseLeaf" 0 (Some AST.TInt64) 1;variant "Arm64RecursiveReleaseNode" 1 (Some (AST.TTuple [typ;typ])) 2] in
 let* instrs=generate (releaseInstr 16 L.GenericHeap (sumMetadata registry typ)) registry in
 let rec takeUntilRet=function S.RET::_->[]|i::rest->i::takeUntilRet rest|[]->[] in
 let rec helper=function S.Label label::rest when Text.startsWith label "__dark_recursive_nominal_rc_dec_"->Some (takeUntilRet rest)|_::rest->helper rest|[]->None in
 match helper instrs with None->Error "Recursive-nominal release helper was not generated"|Some body->
 if List.exists (function S.CBNZ (S.X1,label) when Text.contains label "_variant_0_next"->true|_->false) body then Error "Recursive-nominal release helper dispatched a variant without managed fields"
 else if not (List.mem (S.CMP_imm (S.X1,1)) body) then Error "Recursive-nominal release helper omitted the recursive variant" else Ok ()
let closure name captureType registry=
 let captured=ControlFlowTests.makeEmptyFunction name [{L.reg=L.Physical L.X0;typ=AST.TTuple [AST.TInt64;captureType]}] in
 let program=makeSimpleProgramWithVariants [L.ClosureAlloc (L.Physical L.X1,TestIds.functionIdForName name,[L.Reg (L.Physical L.X2)]);L.RefCountDec (L.Physical L.X1,16,L.ClosureHeap,Some (rcMetadata (AST.TFunction ([AST.TInt64],AST.TInt64))))] registry in
 let program=match program with L.Program ([func],v,r)->L.Program ([func;captured],v,r)|other->other in generatePreparedARM64 target program
let testClosureCaptureNestedFixedBlockBytesFieldUsesReleasePlan ()=
 let* instrs=closure "arm64_nested_tuple_capture_fn" (AST.TTuple [AST.TTuple [AST.TBlob]]) M.empty in require (List.mem (S.LDR (S.X12,S.X11,0)) instrs) "Closure capture nested fixed-block bytes field release did not consume the nested release plan"
let testClosureCaptureBoxedSumBytesPayloadUsesReleasePlan ()=
 let name="Arm64ClosureCaptureSumBytes" in let registry=variants name [variant "Arm64ClosureCaptureSumBytesPayload" 0 (Some AST.TBlob) 1;variant "Arm64ClosureCaptureSumIntegerPayload" 1 (Some AST.TInt64) 1] in
 let* instrs=closure "arm64_sum_bytes_capture_fn" (AST.TSum (name,[])) registry in require (List.mem (S.LDR (S.X12,S.X8,8)) instrs) "Closure capture boxed-sum bytes payload release did not consume the variant release plan"
