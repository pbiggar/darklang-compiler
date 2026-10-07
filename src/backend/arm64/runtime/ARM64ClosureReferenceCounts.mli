val generateClosureRefCountIncHelper :
  ARM64CodeGenTypes.codeGenContext -> Symbolic.instr list

val tryRcReleasePlanOfType :
  LIR.recordRegistry ->
  MemoryModel.rcSumShapeRegistry ->
  AST.semanticType ->
  MemoryModel.rcReleasePlan option

val rcMetadataReleasePlan :
  MemoryModel.rcMetadata option -> MemoryModel.rcReleasePlan option

val requiredRcMetadataReleasePlan :
  string -> MemoryModel.rcMetadata option -> MemoryModel.rcReleasePlan

val generateRecursiveNominalRefCountDecHelper :
  (MemoryModel.rcReleasePlan -> string) ->
  ARM64CodeGenTypes.codeGenContext ->
  AST.semanticType ->
  Symbolic.instr list

val generateClosureRefCountDecHelper :
  (MemoryModel.rcReleasePlan -> string) ->
  ARM64CodeGenTypes.codeGenContext ->
  Symbolic.instr list

val generateStreamRefCountDecHelper :
  ARM64CodeGenTypes.codeGenContext -> Symbolic.instr list
