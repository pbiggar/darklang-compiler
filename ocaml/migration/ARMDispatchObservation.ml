(* Exhaustive LIR dispatch and outlined generic helpers with complete cache keys. *)
open Dark_compiler
open! MemoryModel
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code xs=list MachineISAObservation.symInstr xs
let call encode f=try tuple [`Bool false;encode (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let result=function Ok xs->SemanticJson.union "FSharpResult" "Ok" [code xs] | Error error->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let observe source=
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let fps=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15] in
 let types=[AST.TInt64;AST.TFloat64;AST.TString;AST.TBlob;AST.TInt;AST.TBool;AST.TChar;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TTuple [AST.TString;AST.TList AST.TInt64];AST.TList AST.TString;AST.TDict (AST.TString,AST.TList AST.TString);AST.TFunction ([AST.TInt64],AST.TString);AST.TStream AST.TString;AST.TRecord ("R",[]);AST.TSum ("S",[])] in
 let reg=LIR.Physical LIR.X19 and freg=LIR.FPhysical LIR.D0 in
 let fixtures=List.concat_map (fun reg -> LIRFixtures.instructionsWithRegisters source reg freg (LIR.Imm 1L) AST.TInt64) (List.map (fun phys -> LIR.Physical phys) physical@[LIR.Virtual (-1);LIR.Virtual 0]) @ List.concat_map (fun fp -> LIRFixtures.instructionsWithRegisters source reg fp (LIR.Reg reg) AST.TFloat64) (List.map (fun fp -> LIR.FPhysical fp) fps@[LIR.FVirtual (-2000);LIR.FVirtual (-1000);LIR.FVirtual (-1);LIR.FVirtual 0]) @ List.concat_map (fun operand -> LIRFixtures.instructionsWithRegisters source reg freg operand AST.TString) [LIR.Imm Int64.min_int;LIR.Imm Int64.max_int;LIR.Imm 0L;LIR.StringSymbol source;LIR.StringSymbol "";LIR.Reg reg;LIR.StackSlot (-32769);LIR.StackSlot 0;LIR.StackSlot 32768] @ List.concat_map (fun typ -> LIRFixtures.instructionsWithRegisters source reg freg (LIR.Reg reg) typ) types in
 let dynamic=DynamicBufferRelease DynamicStringBuffer in
 let fixed=RootRelease (8,GenericHeap,FixedBlockPayloadRelease (8,[FieldRelease (0,dynamic)])) in
 let plans=[NoReleasePlan;dynamic;RecursiveRelease (AST.TRecord ("R",[]));fixed;RootRelease (8,GenericHeap,FixedBlockPayloadRelease (8,[FieldRelease (0,fixed)]));RootRelease (16,GenericHeap,BoxedSumPayloadRelease (16,[FieldRelease (8,dynamic)],[{tag=1;fieldReleases=[FieldRelease (8,dynamic)]}]));RootRelease (16,GenericHeap,BoxedSumPayloadRelease (16,[],[{tag=0;fieldReleases=[]};{tag=1;fieldReleases=[FieldRelease (8,dynamic)]}]))] in
 let contexts=list (fun target -> list (fun enabled ->
  let ctx={ (ARMPrintingObservation.context source target enabled) with ARM64CodeGenTypes.functionNames=FunctionIdMap.ofList [AST.functionId 0L,source;AST.functionId (-1L),"largest"];recordRegistry=StringOrder.Map.singleton "R" ["field",AST.TString];sumShapeRegistry=StringOrder.Map.singleton "S" {MemoryModel.typeParams=[];payloads=[0,None;1,Some AST.TString];unaryPayloadTags=MemoryModel.IntSet.singleton 1}} in
  let dispatch=list (fun instruction -> call result (fun () -> ARM64Instructions.convertInstr ctx instruction)) fixtures in
  let generic=list (fun plan -> list (fun size -> list (fun owns -> let spec={LIR.releasePlanMemoKeys=LIR.RcReleasePlanMemoKeySet.empty;payloadSize=size;releasePlan=plan;ownsSinglePayloadSum=owns} in call code (fun () -> GenericReferenceCounts.generatePlannedGenericRefCountDecHelper source spec ctx)) [false;true]) [-2147483648;-32769;-1;0;8;16;248;256;32768;65535;65536;2147483647]) plans in
  tuple [dispatch;generic]) [false;true]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64] in
 let cache=list (fun label -> list (fun id -> let key=GenericReferenceCounts.plannedGenericRefCountDecHelperCacheKey (AST.functionId id) label in tuple [ProductionLIR.functionDef key;`Bool (GenericReferenceCounts.isPlannedGenericRefCountDecHelperCacheKey key);`Bool (GenericReferenceCounts.isPlannedGenericRefCountDecHelperCacheKey (LIR.attachFunctionCodegenFacts key))]) [0L;1L;Int64.min_int;-1L]) [source;"";ARM64CodeGenTypes.plannedGenericRefCountDecHelperLabelPrefix;ARM64CodeGenTypes.plannedGenericRefCountDecHelperLabelPrefix^"hé😀";"\000"^ARM64CodeGenTypes.plannedGenericRefCountDecHelperLabelPrefix] in
 tuple [contexts;cache]
