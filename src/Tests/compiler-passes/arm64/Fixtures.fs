// Fixtures.fs - Build typed ARM64 code-generation test fixtures.

module ARM64CodeGenFixtures

type TestResult = Result<unit, string>

let internal target = ARM64.targetConfigFor Platform.LinuxARM64

let private generatePreparedARM64WithOptions target options program =
    program
    |> ARM64PrepareFunctions.prepareARM64Program
    |> CodeGen.generateARM64WithOptions target options
    |> Result.map CodeGen.generatedProgramInstructions

let internal generatePreparedARM64 target program =
    generatePreparedARM64WithOptions target ARM64CodeGenTypes.defaultOptions program

let internal rcMetadata (typ: AST.Type) : MemoryModel.RcMetadata =
    let releasePlan = MemoryPlanning.rcReleasePlanOfTypeWithSums Map.empty Map.empty typ
    { MemoryModel.ReleasePlanCacheKey = ReleasePlanFingerprint.rcReleasePlanCacheKey typ releasePlan
      MemoryModel.ReleasePlan = Some releasePlan
      MemoryModel.SourceType = Some typ }

let internal rcMetadataWithSumShapes (sumShapes: MemoryModel.RcSumShapeRegistry) (typ: AST.Type) : MemoryModel.RcMetadata =
    let releasePlan = MemoryPlanning.rcReleasePlanOfTypeWithSums Map.empty sumShapes typ
    { MemoryModel.ReleasePlanCacheKey = ReleasePlanFingerprint.rcReleasePlanCacheKey typ releasePlan
      MemoryModel.ReleasePlan = Some releasePlan
      MemoryModel.SourceType = Some typ }

let internal rcMetadataWithRecords (records: LIR.RecordRegistry) (typ: AST.Type) : MemoryModel.RcMetadata =
    let releasePlan = MemoryPlanning.rcReleasePlanOfTypeWithSums records Map.empty typ
    { MemoryModel.ReleasePlanCacheKey = ReleasePlanFingerprint.rcReleasePlanCacheKey typ releasePlan
      MemoryModel.ReleasePlan = Some releasePlan
      MemoryModel.SourceType = Some typ }

let internal makeSimpleProgramWithVariants
    (instrs: LIR.Instr list)
    (variants: LIR.VariantRegistry)
    : LIR.Program =
    let label = LIR.Label "_start_entry"
    let block : LIR.BasicBlock = {
        Label = label
        Instrs = instrs
        Terminator = LIR.Ret
    }
    let func : LIR.Function = {
        Id = AST.functionIdForName "_start"
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, block)]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }
    LIR.Program ([func], variants, Map.empty)

let internal makeSimpleProgramWithRecords
    (instrs: LIR.Instr list)
    (records: LIR.RecordRegistry)
    : LIR.Program =
    let label = LIR.Label "_start_entry"
    let block : LIR.BasicBlock = {
        Label = label
        Instrs = instrs
        Terminator = LIR.Ret
    }
    let func : LIR.Function = {
        Id = AST.functionIdForName "_start"
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, block)]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }
    LIR.Program ([func], Map.empty, records)
