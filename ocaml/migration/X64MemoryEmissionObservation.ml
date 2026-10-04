(* Complete x64 memory emission, aliases and slot retention. *)
open Dark_compiler
module E=X64EmitMemory
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false;(match f () with Ok xs->SemanticJson.union "FSharpResult" "Ok" [`List (List.map MachineISAObservation.x64Instr xs)] | Error e->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string e])] with Failure e | Invalid_argument e->tuple [`Bool true;SemanticJson.string e]
let observe source=
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let gps=List.map (fun p->LIR.Physical p) physical@[LIR.Virtual (-1);LIR.Virtual 0;LIR.Virtual 2147483647] in
 let selected=List.map (fun p->LIR.Physical p) [LIR.X0;LIR.X3;LIR.X6;LIR.X7;LIR.X8;LIR.X19;LIR.SP]@[LIR.Virtual (-1)] in
 let sizes=[-2147483648;-65536;-32769;-1;0;1;7;8;9;16;248;255;256;4095;4096;32767;32768;65535;65536;2147483647] in
 let operands=List.map (fun reg->LIR.Reg reg) gps@List.map (fun n->LIR.StackSlot n) sizes@List.map (fun n->LIR.Imm n) [Int64.min_int;-1L;0L;4096L;Int64.max_int]@[LIR.FloatImm 0.;LIR.FloatImm (-0.);LIR.FloatImm nan;LIR.FloatImm infinity;LIR.FloatImm 0.1;LIR.FloatSymbol (-0.);LIR.FloatSymbol (Int64.float_of_bits 0xfff8000000000000L);LIR.FloatSymbol (Int64.float_of_bits 0x7ff8000000000001L);LIR.FloatSymbol (Int64.float_of_bits 0x7ff0000000000001L);LIR.StringSymbol source;LIR.StringSymbol "hé😀";LIR.StringSymbol (HostText.ofUtf16Units [|0xd800;97;0xdc00|]);LIR.FuncAddr (AST.functionId 0L);LIR.FuncAddr (AST.functionId 1L);LIR.FuncAddr (AST.functionId (-1L))] in
 let types=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TChar;AST.TDateTime;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TVar source;AST.TInferenceVar (source,source);AST.TList AST.TString;AST.TStream AST.TString;AST.TDict (AST.TString,AST.TInt64);AST.TFunction ([AST.TInt64],AST.TBool);AST.TRecord ("R",[]);AST.TRecord ("missing",[]);AST.TRecord ("Rec",[]);AST.TSum ("S",[]);AST.TSum ("missing",[]);AST.TTuple [];AST.TTuple [AST.TInt64;AST.TString;AST.TList AST.TString]] in
 let ctx={X64CodeGenTypes.functionName=source;stackSize=32;usedCalleeSaved=[LIR.X19];enableLeakCheck=false;recordRegistry=StringOrder.Map.of_list ["R",["a",AST.TInt64;"b",AST.TString];"Rec",["next",AST.TRecord ("Rec",[])]];sumShapeRegistry=StringOrder.Map.of_list ["S",{MemoryModel.typeParams=[];payloads=[0,None;1,Some AST.TString];unaryPayloadTags=MemoryModel.IntSet.singleton 1}];functionNames=FunctionIdMap.ofList [AST.functionId 0L,source;AST.functionId (-1L),"largest"]} in
 let allocations=list (fun enabled->let ctx={ctx with X64CodeGenTypes.enableLeakCheck=enabled} in
  let heaps=list (fun dest->list (fun size->call (fun ()->E.emitHeapAlloc ctx dest size)) sizes) gps in
  let raws=list (fun dest->list (fun size->call (fun ()->E.emitRawAlloc ctx dest size)) gps) gps in
  let frees=list (fun reg->call (fun ()->E.emitRawFree ctx reg)) gps in
  let mapped=list (fun dest->list (fun size->call (fun ()->E.emitMappedAlloc ctx dest size)) gps) gps in
  let unmapped=list (fun reg->call (fun ()->E.emitMappedFree ctx reg)) gps in
  tuple [heaps;raws;frees;mapped;unmapped]) [false;true] in
 let heap=list (fun addr->
  let stores=list (fun operand->call (fun ()->E.emitHeapStore ctx addr 0 operand)) operands in
  let offsets=list (fun offset->
   let stores=list (fun operand->call (fun ()->E.emitHeapStore ctx addr offset operand)) [LIR.Reg addr;LIR.Reg (LIR.Physical LIR.X8);LIR.Imm Int64.min_int;LIR.StringSymbol source;LIR.StackSlot 0] in
   let loads=list (fun dest->call (fun ()->E.emitHeapLoad ctx dest addr offset)) gps in tuple [stores;loads]) sizes in
  tuple [stores;offsets]) selected in
 let raw=list (fun a->list (fun b->list (fun c->list call [(fun ()->E.emitRawGet ctx a b c);(fun ()->E.emitRawGetByte ctx a b c);(fun ()->E.emitRawWriteWord ctx a b c);(fun ()->E.emitRawWriteByte ctx a b c)]) [a;b;LIR.Physical LIR.X3;LIR.Physical LIR.X8;LIR.Virtual (-1)]) gps) gps in
 let slots=list (fun ptr->list (fun offset->list (fun value->list (fun typ->call (fun ()->E.emitRawSlotInit ctx ptr offset value typ)) types) selected) [ptr;LIR.Physical LIR.X3;LIR.Physical LIR.X8]) selected in
 tuple [allocations;heap;raw;slots]
