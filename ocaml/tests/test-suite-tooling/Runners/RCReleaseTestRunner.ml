(*
   RCReleaseTestRunner.fs - Executes semantic managed-graph release fixtures.
   Builds canonical LIR heap graphs and requires final release to leave no leaks.
   The fixture constructs a boxed root. The extra payload
   case keeps String sums boxed even when nullable two-case
   String sums use the payload pointer directly.
*)
[@@@warning "-4-42"]
open Dark_compiler
open RCReleaseFormat
module L=LIR
module M=StringOrder.Map
let (let*)=Result.bind
type typedShape={shape:managedShape;typ:AST.semanticType;path:string;children:typedShape list}
type buildState={availableRegisters:L.physReg list;instructions:L.instr list;functionIds:AST.functionId M.t}
let closureId name ids=match M.find_opt name ids with Some id->id|None->Crash.crash ("Release fixture closure '"^name^"' was not allocated")
let fixtureRegisters=[L.X2;L.X3;L.X4;L.X5;L.X6;L.X7;L.X8;L.X9;L.X10;L.X11;L.X12;L.X13;L.X14;L.X15;L.X16;L.X17;L.X19;L.X20;L.X21;L.X22;L.X23]
let indexedChildren path shapes=List.mapi (fun index shape->path^"_"^string_of_int index,shape) shapes
let rec describeShape path shape=
 let describeChildren shapes=indexedChildren path shapes |> List.map (fun (path,shape)->describeShape path shape) in
 let result typ children={shape;typ;path;children} in
 match shape with
 |Int64Value->result AST.TInt64 []|EnumValue->result (AST.TSum ("RCFixtureEnum_"^path,[])) []
 |DynamicString|LiteralString->result AST.TString []|DynamicBlob->result AST.TBlob []
 |ListValue element->let child=describeShape (path^"_item") element in result (AST.TList child.typ) [child]
 |DictValue (key,value)->let key=describeShape (path^"_key") key in let value=describeShape (path^"_value") value in result (AST.TDict (key.typ,value.typ)) [key;value]
 |TupleValue fields->let children=describeChildren fields in result (AST.TTuple (List.map (fun child->child.typ) children)) children
 |RecordValue fields->result (AST.TRecord ("RCFixtureRecord_"^path,[])) (describeChildren fields)
 |SumValue payload->result (AST.TSum ("RCFixtureSum_"^path,[])) [describeShape (path^"_payload") payload]
 |ClosureValue captures->result (AST.TFunction ([AST.TInt64],AST.TInt64)) (describeChildren captures)
let rec collectRecords typed=
 let nested=List.map collectRecords typed.children |> List.fold_left (fun records children->M.fold (fun name fields current->M.add name fields current) children records) M.empty in
 match typed.shape with RecordValue _->let name=match typed.typ with AST.TRecord (name,_)->name|_->Crash.crash "Described record fixture had a non-record type" in M.add name (List.mapi (fun index child->"field"^string_of_int index,child.typ) typed.children) nested|_->nested
let rec collectVariants typed=
 let nested=List.map collectVariants typed.children |> List.fold_left (fun variants children->M.fold (fun name cases current->M.add name cases current) children variants) M.empty in
 match typed.shape with
 |EnumValue->let name=match typed.typ with AST.TSum (name,_)->name|_->Crash.crash "Described enum fixture had a non-sum type" in M.add name {L.typeParams=[];variants=[{L.name=name^"_case";tag=0;payload=None;fieldCount=0}]} nested
 |SumValue _->let name=match typed.typ with AST.TSum (name,_)->name|_->Crash.crash "Described sum fixture had a non-sum type" in (match typed.children with [payload]->M.add name {L.typeParams=[];variants=[{L.name=name^"_payload";tag=0;payload=Some payload.typ;fieldCount=1};{L.name=name^"_empty";tag=1;payload=None;fieldCount=0};{L.name=name^"_other";tag=2;payload=Some AST.TInt64;fieldCount=1}]} nested|_->Crash.crash "Described sum fixture did not have one payload")
 |_->nested
let sumShapes variants=M.map (fun (v:L.typeVariants)->{MemoryModel.typeParams=v.L.typeParams;payloads=List.map (fun (variant:L.variantInfo)->variant.L.tag,variant.L.payload) v.L.variants;unaryPayloadTags=List.filter_map (fun (variant:L.variantInfo)->if variant.L.fieldCount=1 then Some variant.L.tag else None) v.L.variants |> MemoryModel.IntSet.of_list}) variants
let append instructions state={state with instructions=state.instructions@instructions}
let acquireRegister context state=match state.availableRegisters with register::rest->Ok (register,{state with availableRegisters=rest})|[]->Error ("Reference-release fixture exhausted registers while building "^context)
let releaseRegister register state={state with availableRegisters=register::state.availableRegisters}
let physical register=L.Physical register
let rec buildInto typed target state=
 let buildAndStoreField offset child current=
  let* childRegister,afterAcquire=acquireRegister child.path current in let* afterBuild=buildInto child childRegister afterAcquire in
  Ok (afterBuild |> append [L.HeapStore (physical target,offset,L.Reg (physical childRegister),Some child.typ)] |> releaseRegister childRegister) in
 let buildFields children startOffset current=List.mapi (fun index child->startOffset+index*8,child) children |> List.fold_left (fun result (offset,child)->let* current=result in buildAndStoreField offset child current) (Ok current) in
 let tagPointer current=let* tagRegister,afterAcquire=acquireRegister (typed.path^" tag") current in Ok (afterAcquire |> append [L.Mov (physical tagRegister,L.Imm 2L);L.Orr (physical target,physical target,physical tagRegister)] |> releaseRegister tagRegister) in
 match typed.shape with
 |Int64Value->Ok (append [L.Mov (physical target,L.Imm 42L)] state)
 |EnumValue->Ok (append [L.Mov (physical target,L.Imm 1L)] state)
 |LiteralString->Ok (append [L.Mov (physical target,L.StringSymbol "literal")] state)
 |DynamicString|DynamicBlob->Ok (append [L.StringConcat (physical target,L.StringSymbol "left",L.StringSymbol "right",[])] state)
 |TupleValue _|RecordValue _->buildFields typed.children 0 (append [L.HeapAlloc (physical target,List.length typed.children*8)] state)
 |SumValue _->(match typed.children with [payload]->buildAndStoreField 8 payload (append [L.HeapAlloc (physical target,16);L.HeapStore (physical target,0,L.Imm 0L,None)] state)|_->Error "Sum fixture must contain exactly one payload")
 |ListValue _->(match typed.children with [element]->let* built=buildAndStoreField 0 element (append [L.HeapAlloc (physical target,8)] state) in tagPointer built|_->Error "List fixture must contain exactly one representative element")
 |DictValue _->(match typed.children with [key;value]->let* built=buildAndStoreField 0 key (append [L.HeapAlloc (physical target,16)] state) in let* built=buildAndStoreField 8 value built in tagPointer built|_->Error "Dict fixture must contain one representative key and value")
 |ClosureValue _->
  let rec buildCaptures built current=function []->Ok (List.rev built,current)|capture::rest->let* register,acquired=acquireRegister capture.path current in let* builtState=buildInto capture register acquired in buildCaptures (register::built) builtState rest in
  let* registers,built=buildCaptures [] state typed.children in let operands=List.map (fun reg->L.Reg (physical reg)) registers in let name="rc_fixture_closure_"^typed.path in
  let withClosure=append [L.ClosureAlloc (physical target,closureId name built.functionIds,operands)] built in Ok (List.fold_left (fun current register->releaseRegister register current) withClosure registers)
let rec collectClosureFunctions ids typed=
 let nested=List.concat_map (collectClosureFunctions ids) typed.children in match typed.shape with ClosureValue _->
 let name="rc_fixture_closure_"^typed.path in let label=L.Label (name^"_entry") in let captureTuple=AST.TTuple (AST.TInt64::List.map (fun child->child.typ) typed.children) in
 let func:L.functionDef={L.id=closureId name ids;name;typedParams=[{L.reg=physical L.X0;typ=captureTuple}];cfg={L.entry=label;blocks=L.LabelMap.singleton label {L.label;instrs=[];terminator=L.Ret}};stackSize=0;usedCalleeSaved=[];codegenFacts=None} in func::nested|_->nested
let rootReleaseInstruction typed rootRegister metadata=match typed.shape with
 |DynamicString|LiteralString|DynamicBlob->Ok (L.RefCountDecString (L.Reg (physical rootRegister)))
 |ListValue _->Ok (L.RefCountDec (physical rootRegister,0,L.TaggedList,Some metadata))
 |DictValue _->Ok (L.RefCountDec (physical rootRegister,0,L.DictHeap,Some metadata))
 |ClosureValue captures->Ok (L.RefCountDec (physical rootRegister,(List.length captures+1)*8,L.ClosureHeap,Some metadata))
 |TupleValue fields|RecordValue fields->Ok (L.RefCountDec (physical rootRegister,List.length fields*8,L.GenericHeap,Some metadata))
 |SumValue _->Ok (L.RefCountDec (physical rootRegister,16,L.GenericHeap,Some metadata))
 |Int64Value|EnumValue->Error "ROOT must be a managed value"
let buildProgram test=
 let typed=describeShape "root" test.root in
 let rec closureNames typed=let nested=List.concat_map closureNames typed.children in match typed.shape with ClosureValue _->("rc_fixture_closure_"^typed.path)::nested|_->nested in
 let ids=AST.allocateFunctionIds (List.to_seq [TestIds.functionIdForName "_start";TestIds.functionIdForName "__dark_compiler_program_entry"]) (List.to_seq (closureNames typed)) in
 let rootRegister,preserved=match test.placement with CanonicalRoot->L.X19,[]|ExplicitRoot (register,values)->register,values in
 let unavailable=rootRegister::List.map (fun value->value.register) preserved in
 let initial={availableRegisters=List.filter (fun register->not (List.mem register unavailable)) fixtureRegisters;instructions=[];functionIds=ids} in
 let records=collectRecords typed in let variants=collectVariants typed in let shapes=sumShapes variants in let plan=MemoryPlanning.rcReleasePlanOfTypeWithSums records shapes typed.typ in
 let metadata:MemoryModel.rcMetadata={MemoryModel.releasePlanCacheKey=ReleasePlanFingerprint.rcReleasePlanCacheKey typed.typ plan;releasePlan=Some plan;sourceType=Some typed.typ} in
 let* built=buildInto typed rootRegister initial in let* release=rootReleaseInstruction typed rootRegister metadata in
 let setup=List.map (fun value->L.Mov (physical value.register,L.Imm value.value)) preserved in
 let checks=match preserved with []->[]|[value]->[L.PrintInt64 (physical value.register)]|first::rest->let accumulator=rootRegister in L.Mov (physical accumulator,L.Reg (physical first.register))::(List.concat_map (fun value->[L.Add (physical accumulator,physical accumulator,L.Reg (physical value.register))]) rest)@[L.PrintInt64 (physical accumulator)] in
 let instructions=built.instructions@setup@[release]@checks in let entry=L.Label "entry" in
 let main:L.functionDef={L.id=TestIds.functionIdForName "_start";name="_start";typedParams=[];cfg={L.entry;blocks=L.LabelMap.singleton entry {L.label=entry;instrs=instructions;terminator=L.Ret}};stackSize=0;usedCalleeSaved=[];codegenFacts=None} in
 Ok (L.Program (main::collectClosureFunctions ids typed,variants,records),preserved)
let checkedAdd left right=let result=Int64.add left right in if Int64.compare (Int64.logand (Int64.logxor left result) (Int64.logxor right result)) 0L<0 then raise (Failure "Arithmetic operation resulted in an overflow.");result
let runRCReleaseTest target test=
 let* program,preserved=buildProgram test in let* exitCode,stdout,stderr=LIRExecutionTestRunner.executeProgram target program LIRExecutionFormat.LeakCheckEnabled in
 let expected=if preserved=[] then "" else List.fold_left (fun sum value->checkedAdd sum value.value) 0L preserved |> Int64.to_string in
 if exitCode<>0 then Error (Printf.sprintf "Expected release fixture to exit 0, got %d: %s" exitCode (HostText.trim stderr))
 else if HostText.trim stdout<>expected then Error ("Preserved registers produced '"^HostText.trim stdout^"', expected '"^expected^"'")
 else if HostText.trim stderr<>"" then Error ("Release fixture leaked memory: "^HostText.trim stderr) else Ok ()
let loadRCReleaseTests path=if not (TestFileIO.exists path) then Error ("Reference-release fixture not found: "^path) else try RCReleaseFormat.parseRCReleaseFileContent path (HostFile.readText path) with exn->Error ("Failed to read reference-release fixture "^path^": "^HostFile.errorMessage path exn)
let tests target files=Array.to_list files |> List.sort StringOrder.compare |> List.concat_map (fun path->match loadRCReleaseTests path with Error msg->["parse "^Filename.basename path,(fun ()->Error msg)]|Ok cases->List.map (fun test->test.name,(fun ()->runRCReleaseTest target test)) cases)
