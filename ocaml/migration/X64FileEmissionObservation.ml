(* Full file-operation emission, ordered labels, syscall setup and errors. *)
open Dark_compiler
module E=X64EmitFiles
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false;(match f () with Ok xs->SemanticJson.union "FSharpResult" "Ok" [`List (List.map MachineISAObservation.x64Instr xs)] | Error e->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string e])] with Failure e | Invalid_argument e->tuple [`Bool true;SemanticJson.string e]
let observe source=
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let gps=List.map (fun p->LIR.Physical p) physical@[LIR.Virtual (-1);LIR.Virtual 0;LIR.Virtual 2147483647] in
 let operands=List.map (fun reg->LIR.Reg reg) gps@List.map (fun n->LIR.StackSlot n) [-2147483648;-1;0;8;2147483647]@[LIR.Imm Int64.min_int;LIR.Imm 0L;LIR.Imm Int64.max_int;LIR.FloatImm (-0.);LIR.FloatImm nan;LIR.FloatSymbol 0.1;LIR.StringSymbol source;LIR.StringSymbol "hé😀";LIR.StringSymbol (HostText.ofUtf16Units [|0xd800;97;0xdc00|]);LIR.FuncAddr (AST.functionId (-1L))] in
 let ctx enabled={X64CodeGenTypes.functionName=source;stackSize=32;usedCalleeSaved=[LIR.X19;LIR.X20];enableLeakCheck=enabled;recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.empty} in
 let actions ctx dest path=[(fun ()->E.emitFileReadBlob ctx dest path);(fun ()->E.emitFileExists ctx dest path);(fun ()->E.emitFileDelete ctx dest path);(fun ()->E.emitFileCreateDirectory ctx dest path)] in
 let unary=list (fun dest->list (fun path->list call (actions (ctx false) dest path)) operands) gps in
 let selected=List.map (fun p->LIR.Physical p) [LIR.X0;LIR.X5;LIR.X6;LIR.X8;LIR.X19]@[LIR.Virtual (-1)] in
 let writes=list (fun dest->list (fun path->list (fun content->list (fun append->let instr=if append then LIR.FileAppendText (dest,path,content) else LIR.FileWriteBlob (dest,path,content) in call (fun ()->E.emitFileWriteBlob (ctx false) instr dest path content)) [false;true]) [path;LIR.Reg (LIR.Physical LIR.X5);LIR.StringSymbol source]) operands) selected in
 let accounting=list (fun dest->list (fun path->let other=LIR.StringSymbol "hé😀" in let baseCases=list call (actions (ctx true) dest path) in let write=call (fun ()->E.emitFileWriteBlob (ctx true) (LIR.Mov (dest,other)) dest path other) in tuple [baseCases;write]) [LIR.Reg dest;LIR.StringSymbol source;LIR.StackSlot (-1);LIR.Imm 0L]) gps in
 let trivial=list (fun dest->list call [(fun ()->E.emitFileSetExecutable (ctx false) dest);(fun ()->E.emitFileWriteFromPtr (ctx false) dest)]) gps in
 tuple [unary;writes;accounting;trivial]
