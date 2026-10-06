(* Complete x64 scalar printing, heap startup and source behavior boundaries. *)
open Dark_compiler
module P=X64EmitPrinting
module R=InstrumentedX64Printing
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code=list MachineISAObservation.x64Instr
let result=function Ok xs->SemanticJson.union "FSharpResult" "Ok" [code xs]|Error error->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let call f=try tuple [`Bool false;result (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let observe source=
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let regs=List.map (fun reg -> LIR.Physical reg) physical@[LIR.Virtual (-1);LIR.Virtual 0] in
 let ctx={X64CodeGenTypes.functionName=source;stackSize=32;usedCalleeSaved=[];enableLeakCheck=false;recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.empty} in
 let registerCases=list (fun reg -> list call [(fun () -> P.emitPrintInt64 ctx reg);(fun () -> P.emitPrintUInt64 ctx reg);(fun () -> P.emitPrintInt64NoNewline ctx reg);(fun () -> P.emitPrintUInt64NoNewline ctx reg);(fun () -> P.emitPrintBool ctx reg);(fun () -> P.emitPrintBoolNoNewline ctx reg);(fun () -> P.emitPrintHeapString ctx reg);(fun () -> P.emitPrintHeapStringNoNewline ctx reg);(fun () -> P.emitPrintList ctx reg AST.TString);(fun () -> P.emitPrintSum ctx reg [source,0,Some AST.TString] false);(fun () -> P.emitPrintRecord ctx reg source ["field",AST.TString]);(fun () -> P.emitPrintBlob ctx reg)]) regs in
 let fpCases=list (fun freg -> list call [(fun () -> P.emitPrintFloat ctx freg);(fun () -> P.emitPrintFloatNoNewline ctx freg)]) (List.map (fun reg -> LIR.FPhysical reg) [LIR.D0;LIR.D1;LIR.D14;LIR.D15]@[LIR.FVirtual (-1);LIR.FVirtual 0]) in
 let texts=list (fun text -> call (fun () -> P.emitPrintString ctx text)) [source;"";"hé😀";String.make 7 'a';String.make 8 'a';String.make 9 'a';HostText.ofScalars [|0xfffd;0x61;0xfffd|]] in
 let chars=list (fun length -> call (fun () -> P.emitPrintChars ctx (List.init length (fun index -> Char.chr ((index*73+255) land 255))))) (List.init 34 Fun.id@[63;64;65]) in
 let runtime=list (fun reg -> list call [(fun () -> Ok (R.genPrintInt64 reg true));(fun () -> Ok (R.genPrintInt64 reg false));(fun () -> Ok (R.genPrintUInt64 reg true));(fun () -> Ok (R.genPrintUInt64 reg false));(fun () -> Ok (R.genPrintInt64AndExit reg));(fun () -> Ok (R.genPrintBoolAndExit reg))]) (Array.to_list X64EncodingFixtures.regValues) in
 let heap=call (fun () -> Ok (R.genHeapInit ())) in
 tuple [registerCases;fpCases;texts;chars;runtime;heap]
