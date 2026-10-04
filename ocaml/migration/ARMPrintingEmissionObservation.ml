(* Full scalar/aggregate printers and ordered list-display release callbacks. *)
open Dark_compiler
module E=ARM64EmitPrinting
module C=ARM64CodeGenTypes
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false; (match f () with Ok xs -> SemanticJson.union "FSharpResult" "Ok" [`List (List.map J.symInstr xs)] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error])] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP]
let fpPhysical=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15]
let observe source=
 let gps=List.map (fun p -> LIR.Physical p) physical@List.map (fun n -> LIR.Virtual n) [-1;0;2147483647] in
 let fps=List.map (fun p -> LIR.FPhysical p) fpPhysical@List.map (fun n -> LIR.FVirtual n) [-2147483648;-2001;-2000;-1003;-1002;-1001;-1000;-2;-1;0;7;8;9999;10000;2147483647] in
 let types=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TChar;AST.TDateTime;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TVar source;AST.TInferenceVar (source,source);AST.TList AST.TString;AST.TList AST.TInt64;AST.TList AST.TBool;AST.TList AST.TUnit;AST.TStream AST.TString;AST.TDict (AST.TString,AST.TInt64);AST.TFunction ([AST.TInt64],AST.TBool);AST.TRecord (source,[]);AST.TSum (source,[]);AST.TTuple [];AST.TTuple [AST.TInt64;AST.TString]] in
 let variants=[[];[source,0,None];[source,-1,None;"",65536,None];["none",0,None;source,1,Some AST.TString];[source,65535,Some AST.TString;"none",-65536,None];["first",1,Some AST.TString;source,1,Some AST.TString];["none",0,None;"none2",1,None;source,2,Some AST.TString]]@List.concat_map (fun typ -> [[source,0,Some typ];[source,-2147483648,Some typ;"other",2147483647,None];[source,1,Some typ;"other",2,Some typ]]) types in
 let observingSum ctx reg cases transparent failing=
  let recorded=ref [] and count=ref 0 in
  let convert supplied instr=recorded:=(supplied.C.functionName,supplied.C.instructionSite,instr):: !recorded;incr count;if failing then Error "release-check" else Ok [Symbolic.MOVZ (Symbolic.X26,!count,0);Symbolic.Label ("release-"^string_of_int !count)] in
  let result=call (fun () -> E.emitPrintSum ctx convert reg cases transparent) in
  tuple [result;list (fun (name,site,instr) -> tuple [SemanticJson.string name;SemanticJson.string site;ProductionLIR.instr instr]) (List.rev !recorded)]
 in
 list (fun target -> let ctx=ARMPrintingObservation.context source target false in tuple [
  list (fun reg -> list (fun f -> call (fun () -> f ctx reg)) [E.emitPrintBool;E.emitPrintBlob;E.emitPrintInt64NoNewline;E.emitPrintUInt64NoNewline;E.emitPrintBoolNoNewline;E.emitPrintHeapStringNoNewline;E.emitPrintInt64;E.emitPrintUInt64;E.emitPrintHeapString]) gps;
  list (fun freg -> tuple [call (fun () -> E.emitPrintFloatNoNewline ctx freg);call (fun () -> E.emitPrintFloat ctx freg)]) fps;
  list (fun chars -> call (fun () -> E.emitPrintChars ctx chars)) [[];[0];List.init 256 Fun.id;List.init 257 (fun n -> n land 255)];
  list (fun text -> call (fun () -> E.emitPrintString ctx text)) [source;"";"hé😀";"a\000b";HostText.ofUtf16Units [|0xd800;97;0xdc00|]];
  list (fun reg -> list (fun typ -> call (fun () -> E.emitPrintList ctx reg typ)) types) gps;
  list (fun reg -> list (fun cases -> list (fun transparent -> list (fun failing -> observingSum ctx reg cases transparent failing) [false;true]) [false;true]) variants) [LIR.Physical LIR.X0;LIR.Physical LIR.X19;LIR.SP |> (fun p -> LIR.Physical p);LIR.Virtual (-1)];
  list (fun reg -> list (fun fields -> call (fun () -> E.emitPrintRecord ctx reg source fields)) ([]::List.map (fun typ -> [source,typ]) types@[["a",AST.TInt64;"b",AST.TUInt64;"c",AST.TBool;"d",AST.TFloat64;"e",AST.TString;"f",AST.TChar;"g",AST.TInt128;"h",AST.TUInt128]])) gps;
  observingSum ctx (LIR.Physical LIR.X19) ["a",0,Some (AST.TList AST.TInt64);"b",1,Some (AST.TList AST.TString)] false false
 ]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64]
