(* ANF_Optimize.mli - Preserve ANF rewrite checks while production optimization uses SSA. *)
val optimizeProgramWithOptionsAndExternalFunctionsWithTrace :
  (string -> float -> unit) option ->
  ANFConstants.optimizeContext ->
  ANFConstants.optimizeOptions ->
  SpecializationIdentity.FunctionSet.t ->
  ANF.functionDef StringOrder.Map.t ->
  ANF.program ->
  ANF.program

val optimizeProgramWithOptionsAndExternalFunctions :
  ANFConstants.optimizeContext ->
  ANFConstants.optimizeOptions ->
  SpecializationIdentity.FunctionSet.t ->
  ANF.functionDef StringOrder.Map.t ->
  ANF.program ->
  ANF.program

val optimizeProgramWithOptions :
  ANFConstants.optimizeContext ->
  ANFConstants.optimizeOptions ->
  ANF.program ->
  ANF.program

val optimizeProgram : ANFConstants.optimizeContext -> ANF.program -> ANF.program

val optimizeConstFolding :
  ANFConstants.optimizeContext -> ANF.program -> ANF.program

val optimizeCopyProp :
  ANFConstants.optimizeContext -> ANF.program -> ANF.program

val optimizeDCE : ANFConstants.optimizeContext -> ANF.program -> ANF.program
