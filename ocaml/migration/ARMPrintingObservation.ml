(* Complete output helpers, large literal buffers and leak-accounting streams. *)
[@@@warning "-4"]
open Dark_compiler
module J=MachineISAObservation
module C=ARM64CodeGenTypes
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let word value=`Assoc ["kind",`String "uint32";"value",`String (Printf.sprintf "%lu" value)]
let attempt encode f=try tuple [`Bool false;encode (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let words xs=list word (List.mapi (fun idx instr -> ARM64_Encoding.encodeWithLabels instr (idx*4) StringOrder.Map.empty StringOrder.Map.empty StringOrder.Map.empty StringOrder.Map.empty) xs)
let code xs=tuple [list J.armInstr xs;attempt Fun.id (fun () -> words xs)]
let context source target enabled : C.codeGenContext={C.target=target;options={C.defaultOptions with C.enableLeakCheck=enabled};sumShapeRegistry=StringOrder.Map.empty;recordRegistry=StringOrder.Map.empty;rawSlotInitRetainTargets=None;closurePayloadSizes=StringOrder.Map.empty;closureCaptureTypes=StringOrder.Map.empty;functionNames=FunctionIdMap.empty;functionName=source;instructionSite=source;stackSize=0;usedCalleeSaved=[];usedCalleeSavedF=[];heapOverflowLabel=source;recordLirOpExpansion=None}
let observe source=
 list (fun target ->
  let simple=list (fun f -> code (f target)) [PrintAndExit.generatePrintInt64;PrintAndExit.generatePrintBool;PrintAndExit.generatePrintFloat;PrintAndExit.generateExit;PrintValues.generatePrintInt64NoExit;PrintValues.generatePrintUInt64NoExit;PrintValues.generatePrintInt64ToStderrNoExit;PrintValues.generatePrintBoolNoExit;PrintValues.generatePrintInt64NoNewline;PrintValues.generatePrintUInt64NoNewline;PrintValues.generatePrintBoolNoNewline;PrintValues.generatePrintFloatNoNewline;PrintValues.generatePrintStringNoNewline;PrintValues.generatePrintBlob;PrintValues.generateWriteSyscall] in
  let strings=list (fun len -> attempt code (fun () -> PrintAndExit.generatePrintString target len)) [-2147483648;-1;0;1;4095;4096;65535;65536;2147483647] in
  let byteLists=[[];[0];[255];List.init 256 Fun.id;List.init 256 (fun n -> 255-n)]@List.map (fun len -> List.init len (fun n -> n mod 256)) [7;8;15;16;17;31;32;255;256;4095;4096;65536] in
  let chars=list (fun bytes -> tuple [code (PrintValues.generatePrintChars target bytes);code (PrintValues.generatePrintCharsToStderr target bytes)]) byteLists in
  let leaks=list (fun enabled ->
   let ctx=context source target enabled in
   let symbolic xs=tuple [list J.symInstr xs;attempt (fun codes -> list word (Array.to_list codes)) (fun () -> ARM64_Encoding.encodeSymbolicWithPools xs LiteralPool.emptyStringPool LiteralPool.emptyFloatPool (ARM64.targetOS target) enabled)] in
   tuple [symbolic (LeakAccounting.generateLeakCounterInc ctx);symbolic (LeakAccounting.generateLeakCounterDec ctx);list (fun role -> let reg=match J.armInstructions source role 0 with ARM64.MOVZ (reg,_,_)::_ -> reg | _ -> assert false in symbolic (LeakAccounting.generateLeakCounterIncIfResultError ctx reg)) (List.init 32 Fun.id);symbolic (LeakAccounting.generateLeakCheckReport ctx)]) [false;true] in
  tuple [simple;strings;chars;leaks]) [ARM64.targetConfigFor Platform.MacOSARM64;ARM64.targetConfigFor Platform.LinuxARM64]
