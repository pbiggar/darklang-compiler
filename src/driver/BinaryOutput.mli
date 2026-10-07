(* BinaryOutput.mli - Select a validated target backend and assemble executable output. *)
val finalizeArm64GenericHelperIds :
  LIR.program ->
  Backend_Arm64_CodeGen.functionGroup list ->
  Backend_Arm64_CodeGen.metadataGroup list ->
  LIR.program
  * Backend_Arm64_CodeGen.functionGroup list
  * Backend_Arm64_CodeGen.metadataGroup list

val generateBinary :
  Platform.target ->
  int ->
  CompilerOptions.compilerOptions ->
  (unit -> float) ->
  CompilerOptions.passTimingRecorder option ->
  string ->
  string ->
  bool ->
  bool ->
  CompilationSession.compilationSession option ->
  Obj.t ->
  Backend_Arm64_CodeGen.functionGroup list ->
  Backend_Arm64_CodeGen.metadataGroup list ->
  MemoryModel.rcSumShapeRegistry ->
  ARM64CalleeClobbers.writes FunctionIdMap.t ->
  LIR.program ->
  (bytes, string) result
