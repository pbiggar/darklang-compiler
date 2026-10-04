(* Complete canonical-buffer comparison and binary/many string concatenation. *)
open Dark_compiler
module E=X64EmitBuffers
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false;(match f () with Ok xs->SemanticJson.union "FSharpResult" "Ok" [`List (List.map MachineISAObservation.x64Instr xs)] | Error e->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string e])] with Failure e | Invalid_argument e->tuple [`Bool true;SemanticJson.string e]
let observe source=
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let gps=List.map (fun p->LIR.Physical p) physical@[LIR.Virtual (-1);LIR.Virtual 0;LIR.Virtual 2147483647] in
 let selected=List.map (fun p->LIR.Physical p) [LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X8;LIR.X19] in
 let operands=List.map (fun reg->LIR.Reg reg) gps@List.map (fun n->LIR.StackSlot n) [-2147483648;-1;0;8;2147483647]@[LIR.Imm Int64.min_int;LIR.Imm 0L;LIR.Imm Int64.max_int;LIR.FloatImm (-0.);LIR.FloatImm nan;LIR.FloatSymbol 0.1;LIR.StringSymbol source;LIR.StringSymbol "hé😀";LIR.StringSymbol (HostText.ofUtf16Units [|0xd800;97;0xdc00|]);LIR.FuncAddr (AST.functionId (-1L))] in
 let kinds=[MemoryModel.Utf8String;MemoryModel.NullableUtf8String;MemoryModel.GraphemeCluster;MemoryModel.NullableGraphemeCluster] in
 let context enabled={X64CodeGenTypes.functionName=source;stackSize=32;usedCalleeSaved=[LIR.X19;LIR.X20];enableLeakCheck=enabled;recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.empty} in
 let ctx=context false in
 let equality=list (fun dest->list (fun left->list (fun right->list (fun kind->call (fun ()->E.emitCanonicalBufferEq ctx kind dest left right)) kinds) [left;LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X8);LIR.StringSymbol source;LIR.Imm 0L]) operands) selected in
 let concat=list (fun dest->list (fun left->list (fun right->call (fun ()->E.emitStringConcat ctx dest left right [])) [left;LIR.Reg (LIR.Physical LIR.X8);LIR.StringSymbol source]) operands) gps in
 let many=list (fun enabled->let ctx=context enabled in list (fun dest->list (fun operand->list (fun rest->call (fun ()->E.emitStringConcat ctx dest operand (LIR.StringSymbol source) rest)) [[LIR.StringSymbol "hé😀"];[operand;LIR.Reg dest];[LIR.StackSlot (-1);LIR.StringSymbol ""];List.init 12 (fun _->LIR.StringSymbol source)]) operands) [LIR.Physical LIR.X0;LIR.Physical LIR.X8;LIR.Physical LIR.X19;LIR.Virtual (-1)]) [false;true] in
 let enabled=list (fun kind->call (fun ()->E.emitCanonicalBufferEq (context true) kind (LIR.Physical LIR.X0) (LIR.StringSymbol source) (LIR.StringSymbol "hé😀"))) kinds in
 tuple [equality;concat;many;enabled]
