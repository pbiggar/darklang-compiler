(* Backend_Arm64_CodeGen.mli - Assemble planned function and runtime-helper instruction chunks. *)
type functionCodegenCache =
  LIR.functionDef ->
  (unit -> (Symbolic.instr list, string) result) ->
  (Symbolic.instr list, string) result

type metadataGroup = {
  contextIdentity : Obj.t;
  functions : LIR.functionDef list;
}

type functionGroup = {
  contextIdentity : Obj.t;
  reusableAcrossCompilations : bool;
  functions : LIR.functionDef list;
}

type metadataGroupCache =
  Obj.t ->
  LIR.functionDef list ->
  (unit -> ARM64CodeGenTypes.arm64ProgramMetadata) ->
  ARM64CodeGenTypes.arm64ProgramMetadata

type helperCacheKey = {
  closurePayloadSizesFromParams : (string * int) list;
  closurePayloadSizesFromAllocs : (AST.functionId * int) list;
  closureCaptureTypes : (string * AST.semanticType list) list;
  recursiveReleaseTypes : AST.semanticType list;
  cliArgvHelperLabels : string list;
  needsCliExecuteHelper : bool;
  needsCliRunProcessHelper : bool;
  needsCliProcessLifecycleHelpers : bool;
  needsRuntimeErrorHelper : bool;
  listDecHelperLabels : string list;
  plannedListDecHelpers : (string * int) list;
  plannedGenericDecHelperLabels : string list;
  plannedDictDecHelperLabels : string list;
  dictDecHelperLabels : string list;
  needsListRcIncHelper : bool;
  needsDictRcIncHelper : bool;
  needsClosureRcIncHelper : bool;
  needsClosureRcDecHelper : bool;
  needsStreamRcDecHelper : bool;
}

type helperCodegenCache =
  helperCacheKey -> (unit -> Symbolic.instr list) -> Symbolic.instr list

type generatedChunk = {
  instructionParts : Symbolic.instr list list;
  reusableAcrossCompilations : bool;
}

type functionGroupCodegenCache =
  Obj.t ->
  LIR.functionDef list ->
  (unit -> (generatedChunk list, string) result) ->
  (generatedChunk list, string) result

type generatedProgram

val generatedProgramChunks : generatedProgram -> generatedChunk list
val generatedProgramInstructions : generatedProgram -> Symbolic.instr list

val generateARM64WithOptionsAndCaches :
  ARM64.targetConfig ->
  ARM64CodeGenTypes.codeGenOptions ->
  MemoryModel.rcSumShapeRegistry option ->
  ARM64CalleeClobbers.writes FunctionIdMap.t option ->
  functionCodegenCache option ->
  (LIR.functionDef ->
  ARM64CalleeClobbers.writes FunctionIdMap.t ->
  (unit -> LIR.functionDef) ->
  LIR.functionDef)
  option ->
  functionGroupCodegenCache option ->
  functionGroup list ->
  metadataGroupCache option ->
  helperCodegenCache option ->
  metadataGroup list ->
  ARM64CodeGenTypes.lirOpExpansionRecorder option ->
  (string -> float -> unit) option ->
  LIR.program ->
  (generatedProgram, string) result

val generateARM64WithOptionsAndCache :
  ARM64.targetConfig ->
  ARM64CodeGenTypes.codeGenOptions ->
  functionCodegenCache option ->
  (string -> float -> unit) option ->
  LIR.program ->
  (generatedProgram, string) result

val generateARM64WithOptions :
  ARM64.targetConfig ->
  ARM64CodeGenTypes.codeGenOptions ->
  LIR.program ->
  (generatedProgram, string) result

val generateARM64 :
  ARM64.targetConfig -> LIR.program -> (generatedProgram, string) result
