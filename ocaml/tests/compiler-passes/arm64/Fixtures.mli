(*
   Fixtures.fs - Build typed ARM64 code-generation test fixtures.
*)
open Dark_compiler
type testResult=(unit,string) result
val target : ARM64.targetConfig
val generatePreparedARM64 : ARM64.targetConfig -> LIR.program -> (Symbolic.instr list,string) result
val rcMetadata : AST.semanticType -> MemoryModel.rcMetadata
val rcMetadataWithSumShapes : MemoryModel.rcSumShapeRegistry -> AST.semanticType -> MemoryModel.rcMetadata
val rcMetadataWithRecords : LIR.recordRegistry -> AST.semanticType -> MemoryModel.rcMetadata
val makeSimpleProgramWithVariants : LIR.instr list -> LIR.variantRegistry -> LIR.program
val makeSimpleProgramWithRecords : LIR.instr list -> LIR.recordRegistry -> LIR.program
