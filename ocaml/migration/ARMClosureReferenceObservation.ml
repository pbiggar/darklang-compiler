(* Complete closure/stream ownership helpers, capture plans and ordered callback observations. *)
open Dark_compiler
module E=ARM64ClosureReferenceCounts
module C=ARM64CodeGenTypes
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code xs=list J.symInstr xs
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let plan=SemanticANF.memoryModel_rcReleasePlan
let call encode f=try tuple [`Bool false;encode (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let observe source=
 let records=StringOrder.Map.of_list ["R",["a",AST.TString;"b",AST.TList AST.TBlob;"c",AST.TDict (AST.TInt64,AST.TInt64)];"Rec",["next",AST.TRecord ("Rec",[])];"Child",["x",AST.TTuple [AST.TString;AST.TList AST.TString]];"Large",List.init 35 (fun index -> string_of_int index,AST.TString)] in
 let info payloads unary={MemoryModel.typeParams=[];MemoryModel.payloads=payloads;MemoryModel.unaryPayloadTags=MemoryModel.IntSet.of_list unary} in
 let sums=StringOrder.Map.of_list ["None",info [] [];"Nullable",info [0,None;1,Some AST.TString] [1];"S",info [0,None;1,Some (AST.TTuple [AST.TString;AST.TList AST.TString]);65536,Some (AST.TRecord ("Child",[]))] [];"RecSum",info [0,None;1,Some (AST.TTuple [AST.TString;AST.TSum ("RecSum",[])] )] []] in
 let primitives=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TChar;AST.TDateTime;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TVar source;AST.TInferenceVar (source,source);AST.TFunction ([AST.TString],AST.TBool);AST.TRecord ("missing",[]);AST.TRecord ("R",[]);AST.TRecord ("Rec",[]);AST.TRecord ("Child",[]);AST.TRecord ("Large",[]);AST.TSum ("missing",[]);AST.TSum ("None",[]);AST.TSum ("Nullable",[]);AST.TSum ("S",[]);AST.TSum ("RecSum",[]);AST.TTuple [];AST.TTuple [AST.TString;AST.TList AST.TString]] in
 let types=primitives@List.concat_map (fun typ -> [AST.TList typ;AST.TList (AST.TList typ);AST.TStream typ;AST.TDict (AST.TInt64,typ);AST.TDict (AST.TString,typ);AST.TTuple [typ;AST.TString];AST.TTuple [AST.TTuple [typ;AST.TBlob];AST.TTuple [AST.TList typ;AST.TDict (AST.TString,typ)]]]) primitives in
 let captures=[]::types::(List.init 4095 (fun _ -> AST.TUnit)@[AST.TString])::List.map (fun typ -> [typ]) types in
 let observing f=
  let trace=ref [] and count=ref 0 in
  let select plan=trace:=plan:: !trace;incr count;"dict-"^string_of_int !count in
  let result=call code (fun () -> f select) in
  tuple [result;list plan (List.rev !trace)]
 in
 let contexts=list (fun target -> list (fun enabled ->
  let ctx={ (ARMPrintingObservation.context source target enabled) with C.recordRegistry=records;C.sumShapeRegistry=sums } in
  let helpers=tuple [call code (fun () -> E.generateClosureRefCountIncHelper ctx);call code (fun () -> E.generateStreamRefCountDecHelper ctx);observing (fun select -> E.generateClosureRefCountDecHelper select ctx);list (fun typ -> observing (fun select -> E.generateRecursiveNominalRefCountDecHelper select ctx typ)) types] in
  let sizeCases=list (fun size -> let ctx={ctx with C.closurePayloadSizes=StringOrder.Map.of_list ["😀",size;"\xee\x80\x80",8;source,24;"a",size]} in tuple [call code (fun () -> E.generateClosureRefCountIncHelper ctx);observing (fun select -> E.generateClosureRefCountDecHelper select ctx)]) [-2147483648;-65536;-32769;-32768;-1;0;8;16;248;255;256;32767;32768;65535;65536;2147483647] in
  let captureCases=list (fun captureTypes -> let ctx={ctx with C.closureCaptureTypes=StringOrder.Map.of_list [source,captureTypes;"😀",[];"\xee\x80\x80",[AST.TInt;AST.TRecord ("missing",[])]];C.closurePayloadSizes=StringOrder.Map.of_list [source,(List.length captureTypes+1)*8]} in observing (fun select -> E.generateClosureRefCountDecHelper select ctx)) captures in
  tuple [helpers;sizeCases;captureCases]) [false;true]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64] in
 let typed=list (fun typ -> call (option plan) (fun () -> E.tryRcReleasePlanOfType records sums typ)) types in
 let metadata=[None;Some {MemoryModel.releasePlanCacheKey=None;releasePlan=None;sourceType=None}]@List.filter_map (fun typ -> try match E.tryRcReleasePlanOfType records sums typ with None -> None | Some releasePlan -> Some (Some {MemoryModel.releasePlanCacheKey=Some source;releasePlan=Some releasePlan;sourceType=Some typ}) with Failure _ | Invalid_argument _ -> None) types in
 tuple [contexts;typed;list (fun meta -> tuple [call (option plan) (fun () -> E.rcMetadataReleasePlan meta);call plan (fun () -> E.requiredRcMetadataReleasePlan source meta)]) metadata]
