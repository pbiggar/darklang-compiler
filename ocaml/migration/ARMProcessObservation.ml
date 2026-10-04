(* Full process helpers, literal pools, words and executable images. *)
open Dark_compiler
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let word value=`Assoc ["kind",`String "uint32";"value",`String (Printf.sprintf "%lu" value)]
let observe source=
 list (fun target -> list (fun enabled ->
  let ctx=ARMPrintingObservation.context source target enabled in
  let labels=[source;"argv";"hé😀";"nul\000label";HostText.ofUtf16Units [|0xd800;97;0xdc00|]] in
  let groups=[[ProcessLifecycle.generateHeapInit target];List.map (ProcessLifecycle.generateCliArgvHelper ctx) labels;[ExecuteProcess.generateLinuxCliExecuteHelper ()];[RunProcess.generateLinuxCliRunProcessHelper ()];[ProcessLifecycle.generateLinuxCliSpawnProcessHelper ()];[ProcessLifecycle.generateLinuxCliProcessLifecycleHelpers ctx]] in
  let code xs=
   let sp,fp=ARM64_Resolve.collectPools xs in
   let words=ARM64_Encoding.encodeSymbolicWithPools xs sp fp (ARM64.targetOS target) enabled in
   let image=match ARM64.targetOS target with Platform.Linux -> Backend_Arm64_Binary_Generation_ELF.createExecutableWithPools words sp fp enabled | Platform.MacOS -> ControlledMachO.createExecutableWithPools words sp fp enabled in
   tuple [list J.symInstr xs;ARMEncodingObservation.stringPool sp;ARMEncodingObservation.floatPool fp;list word (Array.to_list words);X64EncodingObservation.bytes image] in
  tuple [list (list code) groups;code (List.concat (List.concat groups))]) [false;true]) [ARM64.targetConfigFor Platform.MacOSARM64;ARM64.targetConfigFor Platform.LinuxARM64]
