(* Exact x64 label resolution, deferred patching and mutation observations. *)
[@@@warning "-4"]
open Dark_compiler
module R=X86_64_Resolve
module X=X86_64
module E=X64EncodingObservation
let tuple values=`Assoc ["tuple",`List values]
let list f xs=`List (List.map f xs)
let i=SemanticJson.int32
let str=SemanticJson.string
let map f values=`Assoc ["map",list (fun (key,value) -> tuple [str key;f value]) (StringOrder.Map.bindings values)]
let result f=function Ok value -> SemanticJson.union "FSharpResult" "Ok" [f value] | Error msg -> SemanticJson.union "FSharpResult" "Error" [str msg]
let attempt f action=try result f (Ok (action ())) with Failure msg | Invalid_argument msg -> result f (Error msg)
let fixup (f:R.fixup)=SemanticJson.record "Fixup" ["PatchOffset",i f.R.patchOffset;"NextInstrOffset",i f.R.nextInstrOffset;"TargetLabel",str f.R.targetLabel]
let resolved (r:R.resolveResult)=SemanticJson.record "ResolveResult" ["MachineCode",E.bytes r.R.machineCode;"LabelPositions",map i r.R.labelPositions;"DeferredFixups",list fixup r.R.deferredFixups]
let observe source =
 let labels=[source;"target";"_leak_count";X.stringLiteralLabel source;"missing"] in
 let graphs label=[[];[X.RET];[X.Label label;X.CALL label;X.JMP label;X.Jcc (X.EQ,label);X.LEA_rip (X.R12,label);X.RET];
 [X.CALL label;X.MOV_imm32 (X.RAX,42l);X.Label label;X.RET];[X.JMP "target";X.Label label;X.Jcc (X.NP,"target");X.Label "target";X.LEA_rip (X.R8,label);X.CALL "unknown";X.CALL "unknown";X.RET];
 [X.Label label;X.Label label;X.LEA_index (X.RAX,X.RBX,X.RSP,3,0l)];[X.CALL label;X.CALL "missing";X.JMP label;X.LEA_rip (X.RSI,X.stringLiteralLabel source)];
 [X.LEA_rip (X.RDI,X.stringLiteralLabel "");X.LEA_rip (X.RAX,X.stringLiteralLabel source);X.LEA_rip (X.R8,X.stringLiteralLabel "é");X.LEA_rip (X.R8,X.stringLiteralLabel source)];[X.LEA_index (X.RAX,X.RBX,X.RSP,3,0l)];[X.Label label;X.LEA_index (X.RAX,X.RBP,X.RCX,3,0l)]] in
 let graphCases=list (fun label -> list (fun instrs -> tuple [list MachineISAObservation.x64Instr instrs;ARMEncodingObservation.stringPool (R.collectStringPool instrs);
 attempt (fun value -> match value with Error _ -> result resolved value | Ok resolvedValue ->
 let pristine=Bytes.copy resolvedValue.R.machineCode in
 let before=resolved resolvedValue in
 let patches=list (fun (dataLabels,codeOffset) -> let input={resolvedValue with R.machineCode=Bytes.copy pristine} in let outcome=R.patchDataLabels input dataLabels codeOffset in tuple [map i dataLabels;i codeOffset;result resolved outcome;E.bytes input.R.machineCode])
 [StringOrder.Map.empty,0;StringOrder.Map.of_list [label,120;"target",65535;"unknown",128;"missing",0;X.stringLiteralLabel source,256;X.stringLiteralLabel "",264;X.stringLiteralLabel "é",280],120;R.dataLabelOffsets 120 (Bytes.length pristine) (R.collectStringPool instrs),120;StringOrder.Map.singleton label (Int32.to_int Int32.min_int),(Int32.to_int Int32.max_int)] in
 tuple [before;patches]) (fun () -> R.resolveAndEncode instrs)]) (graphs label)) labels in
 let pools=[LiteralPool.emptyStringPool;LiteralPool.createStringPool (List.to_seq [source]);LiteralPool.createStringPool (List.to_seq ["";"1234567";"12345678";"123456789";"é";source]);R.collectStringPool [X.LEA_rip (X.RAX,X.stringLiteralLabel source);X.LEA_rip (X.RDI,X.stringLiteralLabel source)]] in
 let layouts=list (fun pool -> tuple [ARMEncodingObservation.stringPool pool;list (fun codeOffset -> list (fun codeSize -> let offsets=R.dataLabelOffsets codeOffset codeSize pool in tuple [map i offsets;list (fun label -> result i (R.requireLabelPosition label offsets)) labels]) [0;1;7;8;65535;Int32.to_int Int32.max_int]) [0;120;4096;Int32.to_int Int32.min_int;Int32.to_int Int32.max_int]]) pools in
 let patches=list (fun offset -> list (fun value -> let bytes=Bytes.init 24 (fun n -> Char.chr (n+64)) in R.patchRel32 bytes offset value;tuple [i offset;i value;E.bytes bytes]) [Int32.to_int Int32.min_int;-65536;-129;-128;-1;0;1;127;128;65536;Int32.to_int Int32.max_int]) [0;1;8;20] in
 tuple [graphCases;layouts;patches]
