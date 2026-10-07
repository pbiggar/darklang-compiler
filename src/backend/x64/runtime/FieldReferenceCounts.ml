(* FieldReferenceCounts.ml - Generate shape-directed field and fixed-block destruction. *)
[@@@warning "-4"]
open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open! MemoryModel
module F=StructuralFormat
let number n=F.Scalar (string_of_int n)
let kindValue kind=F.Union ((match kind with MemoryModel.GenericHeap->"GenericHeap"|StreamHeap->"StreamHeap"|TaggedList->"TaggedList"|DictHeap->"DictHeap"|ClosureHeap->"ClosureHeap"),[])
let operationValue = function
 | MemoryModel.FixedSizeRoot (size,kind)->F.Union ("FixedSizeRoot",[number size;kindValue kind])
 | DynamicStringBuffer->F.Union ("DynamicStringBuffer",[])
 | DynamicBlobBuffer->F.Union ("DynamicBlobBuffer",[])
 | DynamicIntBuffer->F.Union ("DynamicIntBuffer",[])
let rec planValue = function
 | MemoryModel.NoReleasePlan->F.Union ("NoReleasePlan",[])
 | DynamicBufferRelease operation->F.Union ("DynamicBufferRelease",[operationValue operation])
 | RecursiveRelease typ->F.Union ("RecursiveRelease",[F.semanticValue typ])
 | RootRelease (size,kind,payload)->F.Union ("RootRelease",[number size;kindValue kind;payloadValue payload])
and fieldValue (MemoryModel.FieldRelease (offset,plan))=F.Union ("FieldRelease",[number offset;planValue plan])
and fieldsValue fields=F.Sequence (List.map fieldValue fields)
and payloadValue = function
 | MemoryModel.NoPayloadRelease->F.Union ("NoPayloadRelease",[])
 | FixedBlockPayloadRelease (size,fields)->F.Union ("FixedBlockPayloadRelease",[number size;fieldsValue fields])
 | BoxedSumPayloadRelease (size,fields,variants)->F.Union ("BoxedSumPayloadRelease",[number size;fieldsValue fields;F.Sequence (List.map (fun (variant:MemoryModel.rcBoxedSumVariantRelease)->F.Record ["Tag",number variant.MemoryModel.tag;"FieldReleases",fieldsValue variant.MemoryModel.fieldReleases]) variants)])
 | TaggedListPayloadRelease plan->F.Union ("TaggedListPayloadRelease",[planValue plan])
 | DictPayloadRelease (key,value)->F.Union ("DictPayloadRelease",[planValue key;planValue value])
 | ClosurePayloadRelease fields->F.Union ("ClosurePayloadRelease",[fieldsValue fields])
let planText value=F.format (planValue value)

module X=X86_64
let savedRegs=[X.RAX;X.RDI;X.RSI;X.RDX;X.RCX;X.R8;X.R9;X.R10;scratch]
let pushes regs=List.map (fun reg->X.PUSH reg) regs
let pops regs=List.map (fun reg->X.POP reg) (List.rev regs)
let rec genFieldReleases recursiveHelperLabel preserveRegisters ctx fieldReleases =
 List.concat_map (fun (MemoryModel.FieldRelease (fieldOffset,fieldReleasePlan)) ->
  match fieldReleasePlan with
  | MemoryModel.DynamicBufferRelease operation -> genDynamicBufferFieldRelease ctx (operation=MemoryModel.DynamicIntBuffer) fieldOffset
  | MemoryModel.RootRelease (_,MemoryModel.DictHeap,_) -> genDictFieldRelease fieldOffset fieldReleasePlan
  | MemoryModel.RootRelease (_,MemoryModel.ClosureHeap,_) -> genClosureFieldRelease fieldOffset
  | MemoryModel.RootRelease (_,MemoryModel.StreamHeap,_) ->
    [X.PUSH X.RDX;X.MOV_load (X.RAX,X.RDX,Int32.of_int fieldOffset);X.CALL streamRefCountDecHelperLabel;X.POP X.RDX]
  | MemoryModel.RootRelease (_,MemoryModel.TaggedList,_) -> genListFieldRelease fieldOffset fieldReleasePlan
  | MemoryModel.RootRelease (childPayloadSize,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease _)
  | MemoryModel.RootRelease (childPayloadSize,MemoryModel.GenericHeap,MemoryModel.BoxedSumPayloadRelease _) ->
    genFixedBlockFieldRelease recursiveHelperLabel preserveRegisters ctx fieldOffset childPayloadSize fieldReleasePlan
  | MemoryModel.RecursiveRelease sourceType ->
    [X.PUSH X.RDX;X.MOV_load (X.RAX,X.RDX,Int32.of_int fieldOffset);X.CALL (recursiveHelperLabel sourceType);X.POP X.RDX]
  | _ -> []) fieldReleases
and genBoxedSumVariantFieldReleases recursiveHelperLabel preserveRegisters ctx variants =
 let releaseVariant (variant:MemoryModel.rcBoxedSumVariantRelease)=
  let releaseInstrs=genFieldReleases recursiveHelperLabel preserveRegisters ctx variant.MemoryModel.fieldReleases in
  if releaseInstrs=[] then None else Some (variant.MemoryModel.tag,releaseInstrs) in
 let cases=List.filter_map releaseVariant variants in
 if cases=[] then [] else
  let doneLabel=freshLabel "rc_dec_sum_done" in
  let caseInstructions=List.mapi (fun index (tag,releaseInstrs)->
   let nextCaseLabel=freshLabel ("rc_dec_sum_case_"^string_of_int index^"_next") in
   [X.CMP_imm (X.R10,Int32.of_int tag);X.Jcc (X.NE,nextCaseLabel)]@releaseInstrs@[X.JMP doneLabel;X.Label nextCaseLabel]) cases |> List.concat in
  [X.MOV_load (X.R10,X.RDX,0l)]@caseInstructions@[X.Label doneLabel]
and genFixedBlockFieldReleases recursiveHelperLabel preserveRegisters ctx releasePlan =
 match releasePlan with
 | Some (MemoryModel.RootRelease (_,_,MemoryModel.FixedBlockPayloadRelease (_,plannedFieldReleases)))
 | Some (MemoryModel.RootRelease (_,_,MemoryModel.BoxedSumPayloadRelease (_,plannedFieldReleases,[]))) ->
   genFieldReleases recursiveHelperLabel preserveRegisters ctx plannedFieldReleases
 | Some (MemoryModel.RootRelease (_,_,MemoryModel.BoxedSumPayloadRelease (_,_,variants))) ->
   genBoxedSumVariantFieldReleases recursiveHelperLabel preserveRegisters ctx variants
 | _ -> []
and genFixedBlockFieldRelease recursiveHelperLabel preserveRegisters ctx fieldOffset childPayloadSize fieldReleasePlan =
 let saveParent=if preserveRegisters then [] else [X.PUSH X.RDX] in
 let restoreParent=if preserveRegisters then [] else [X.POP X.RDX] in
 let release=genRefCountDecGenericWithPlanUsing recursiveHelperLabel preserveRegisters ctx X.R8 childPayloadSize (Some fieldReleasePlan) in
 saveParent@[X.MOV_load (X.R8,X.RDX,Int32.of_int fieldOffset)]@release@restoreParent
(* Generic RefCountDec: decrement refcount at [addr + payloadSize].
   If zero, release known fields, free block to free list, and update leak accounting.
   Public lowering preserves scratch registers; recursive workers preserve only
   parent roots at nested fixed-block boundaries to keep deep release bounded. *)
and genRefCountDecGenericWithPlanUsing recursiveHelperLabel preserveRegisters ctx addrReg payloadSize releasePlan =
 let skipLabel=freshLabel "rc_dec_skip" in
 let noFreeLabel=freshLabel "rc_dec_nofree" in
 let leakDec=genLeakCounterDec ctx in
 let fieldReleases=genFixedBlockFieldReleases recursiveHelperLabel preserveRegisters ctx releasePlan in
 let saves=if preserveRegisters then pushes savedRegs else [] in
 let restores=if preserveRegisters then pops savedRegs else [] in
 let size=Int32.of_int payloadSize in
 [X.TEST_reg (addrReg,addrReg);X.Jcc (X.EQ,skipLabel)]@saves@
 [X.MOV_reg (X.RDX,addrReg);X.MOV_load (X.RCX,X.RDX,size);X.SUB_imm (X.RCX,1l);
  X.MOV_store (X.RDX,size,X.RCX);X.TEST_reg (X.RCX,X.RCX);X.Jcc (X.NE,noFreeLabel)]
 @fieldReleases@(if payloadSize>=0 && payloadSize<freeListSize then
  [X.MOV_load (X.RCX,freeListBase,size);X.MOV_store (X.RDX,0l,X.RCX);X.MOV_store (freeListBase,size,X.RDX)] else [])
 @leakDec@[X.Label noFreeLabel]@restores@[X.Label skipLabel]

let genRefCountDecGenericWithPlan ctx addrReg payloadSize releasePlan =
 genRefCountDecGenericWithPlanUsing recursiveNominalRefCountDecHelperLabel true ctx addrReg payloadSize releasePlan
let genRefCountDecGeneric ctx addrReg payloadSize metadata =
 genRefCountDecGenericWithPlan ctx addrReg payloadSize (rcMetadataReleasePlan metadata)
(* Stream roots have the generic fixed-block layout, but their close callback
   must run before the two owned callback closures are released. The lifecycle
   word makes this finalizer share close's idempotence boundary. *)
let genRefCountDecStream ctx addrReg metadata =
 let skipLabel=freshLabel "stream_rc_dec_skip" in
 let noFreeLabel=freshLabel "stream_rc_dec_nofree" in
 let alreadyClosedLabel=freshLabel "stream_rc_dec_closed" in
 let fieldReleases=genFixedBlockFieldReleases recursiveNominalRefCountDecHelperLabel true ctx (rcMetadataReleasePlan metadata) in
 let saves=pushes savedRegs in let restores=pops savedRegs in
 let leakDec=genLeakCounterDec ctx in
 [X.TEST_reg (addrReg,addrReg);X.Jcc (X.EQ,skipLabel)]@saves@
 [X.MOV_reg (X.RDX,addrReg);X.MOV_load (X.RCX,X.RDX,24l);X.SUB_imm (X.RCX,1l);
  X.MOV_store (X.RDX,24l,X.RCX);X.TEST_reg (X.RCX,X.RCX);X.Jcc (X.NE,noFreeLabel);
  X.MOV_load (X.RCX,X.RDX,0l);X.CMP_imm (X.RCX,5l);X.Jcc (X.EQ,alreadyClosedLabel);
  X.MOV_imm32 (X.RCX,5l);X.MOV_store (X.RDX,0l,X.RCX);X.PUSH X.RDX;
  X.MOV_load (X.RAX,X.RDX,16l);X.MOV_load (X.R10,X.RAX,0l);X.MOV_imm32 (X.RDI,0l);
  X.CALL_reg X.R10;X.POP X.RDX;X.Label alreadyClosedLabel]@fieldReleases@
 [X.MOV_load (X.RCX,freeListBase,24l);X.MOV_store (X.RDX,0l,X.RCX);X.MOV_store (freeListBase,24l,X.RDX)]
 @leakDec@[X.Label noFreeLabel]@restores@[X.Label skipLabel]
let generateStreamRefCountDecHelper ctx =
 let sourceType=AST.TStream (AST.TVar "a") in
 let releasePlan=MemoryPlanning.rcReleasePlanOfTypeWithSums ctx.recordRegistry ctx.sumShapeRegistry sourceType in
 let metadata={MemoryModel.releasePlanCacheKey=ReleasePlanFingerprint.rcReleasePlanCacheKey sourceType releasePlan;MemoryModel.releasePlan=Some releasePlan;MemoryModel.sourceType=Some sourceType} in
 let release=genRefCountDecStream ctx X.RAX (Some metadata) in
 [X.Label streamRefCountDecHelperLabel]@release@[X.RET]
let generateRecursiveNominalRefCountDecHelper enableLeakCheck recordRegistry sumShapeRegistry sourceType =
 let releasePlan=MemoryPlanning.rcReleasePlanOfTypeWithSums recordRegistry sumShapeRegistry sourceType in
 let helperCtx={functionName="__dark_recursive_sum_rc_dec";stackSize=0;usedCalleeSaved=[];enableLeakCheck;recordRegistry;sumShapeRegistry;functionNames=FunctionIdMap.empty} in
 let helperLabel=recursiveNominalRefCountDecHelperLabel sourceType in
 let workerLabel typ=recursiveNominalRefCountDecHelperLabel typ^"_worker" in
 let saves=pushes savedRegs in let restores=pops savedRegs in
 match releasePlan with
 | MemoryModel.RootRelease (payloadSize,MemoryModel.GenericHeap,_) ->
   let release=genRefCountDecGenericWithPlanUsing workerLabel false helperCtx X.RAX payloadSize (Some releasePlan) in
   [X.Label helperLabel]@saves@[X.CALL (workerLabel sourceType)]@restores@[X.RET;X.Label (workerLabel sourceType)]@release@[X.RET]
 | MemoryModel.RootRelease (_,MemoryModel.TaggedList,_) ->
   [X.Label helperLabel]@saves@[X.CALL (listDecHelperForReleasePlan releasePlan)]@restores@[X.RET]
 | MemoryModel.RootRelease (_,MemoryModel.DictHeap,_) ->
   [X.Label helperLabel]@saves@[X.CALL (dictDecHelperForReleasePlan releasePlan)]@restores@[X.RET]
 | MemoryModel.RootRelease (_,MemoryModel.ClosureHeap,_) ->
   [X.Label helperLabel]@saves@[X.CALL closureRefCountDecHelperLabel]@restores@[X.RET]
 | MemoryModel.RootRelease (_,MemoryModel.StreamHeap,_) ->
   [X.Label helperLabel]@saves@[X.CALL streamRefCountDecHelperLabel]@restores@[X.RET]
 | _ -> Crash.crash ("x64 recursive nominal RC helper requires a managed root release plan, got "^planText releasePlan)
let recursiveReleaseTypesInFunctions functions =
 let instructions=List.concat_map (fun (func:LIR.functionDef)->List.concat_map (fun (_, (block:LIR.basicBlock))->block.LIR.instrs) (LIR.LabelMap.bindings func.LIR.cfg.LIR.blocks)) functions in
 List.fold_left (fun recursiveTypes instr->match instr with
 | LIR.RefCountDec (_,_,_,Some metadata)->
   let types=Option.fold ~none:MemoryPlanning.SemanticTypeSet.empty ~some:MemoryPlanning.recursiveReleaseTypes metadata.MemoryModel.releasePlan in
   MemoryPlanning.SemanticTypeSet.union recursiveTypes types
 | _ -> recursiveTypes) MemoryPlanning.SemanticTypeSet.empty instructions
(* Generic RefCountInc: increment refcount at [addr + payloadSize]. *)
let genRefCountIncGeneric addrReg payloadSize =
 let skipLabel=freshLabel "rc_inc_skip" in
 let size=Int32.of_int payloadSize in
 [X.TEST_reg (addrReg,addrReg);X.Jcc (X.EQ,skipLabel);X.PUSH X.RDX;X.PUSH X.R10;
  X.MOV_reg (X.R10,addrReg);X.MOV_load (X.RDX,X.R10,size);X.ADD_imm (X.RDX,1l);
  X.MOV_store (X.R10,size,X.RDX);X.POP X.R10;X.POP X.RDX;X.Label skipLabel]
