(* Complete x64 process helper instructions, resolution, pools and ELF bytes. *)
open Dark_compiler
module P=X64Process
module X=X86_64
module R=X86_64_Resolve
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error e->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string e]
let observe _source=
 list (fun enabled->
  let actions=[(fun ()->P.generateCliArgvHelper ());(fun ()->P.generateCliEnvironmentPackedHelper enabled);(fun ()->P.generateCliDirectoryCurrentHelper enabled);(fun ()->P.generateCliSetEnvHelper enabled);(fun ()->P.generateCliUnsetEnvHelper enabled);(fun ()->P.generateCliDirectoryListHelper enabled);(fun ()->P.generateCliGetEnvHelper enabled);(fun ()->P.generateLinuxCliSpawnProcessHelper ());(fun ()->P.generateLinuxCliProcessLifecycleHelpers enabled);(fun ()->P.generateLinuxCliRunProcessHelper enabled);(fun ()->P.generateLinuxCliExecuteHelper enabled)] in
  let groups=List.map (fun f->f ()) actions in
  let code xs=
   let pool=R.collectStringPool xs in
   let encoded=R.resolveAndEncode (xs@[X.Label X64Operands.oomHandlerLabel;X.RET;X.Label "_leak_count";X.RET]) in
   let image=result (fun resolved->
    let patched=R.patchDataLabels resolved (R.dataLabelOffsets 120 (Bytes.length resolved.R.machineCode) pool) 120 in
    result (fun r->tuple [X64EncodingObservation.bytes r.R.machineCode;X64EncodingObservation.bytes (Binary_Generation_ELF_X86_64.createExecutableWithPools r.R.machineCode pool LiteralPool.emptyFloatPool false 0)]) patched) encoded in
   tuple [list MachineISAObservation.x64Instr xs;ARMEncodingObservation.stringPool pool;image] in
  tuple [list code groups;code (List.concat groups)]) [false;true]
