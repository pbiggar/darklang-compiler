[@@@warning "-42"]

(*
   Fixtures.ml - Build typed ARM64 code-generation test fixtures.
*)
open Dark_compiler
module L = LIR
module M = StringOrder.Map

type testResult = (unit, string) result

let target = ARM64.targetConfigFor Platform.LinuxARM64

let generatePreparedARM64WithOptions target options program =
  program |> ARM64PrepareFunctions.prepareARM64Program
  |> Backend_Arm64_CodeGen.generateARM64WithOptions target options
  |> Result.map Backend_Arm64_CodeGen.generatedProgramInstructions

let generatePreparedARM64 target program =
  generatePreparedARM64WithOptions target ARM64CodeGenTypes.defaultOptions
    program

let metadata records sums typ =
  let releasePlan =
    MemoryPlanning.rcReleasePlanOfTypeWithSums records sums typ
  in
  {
    MemoryModel.releasePlanCacheKey =
      ReleasePlanFingerprint.rcReleasePlanCacheKey typ releasePlan;
    releasePlan = Some releasePlan;
    sourceType = Some typ;
  }

let rcMetadata typ = metadata M.empty M.empty typ
let rcMetadataWithSumShapes sums typ = metadata M.empty sums typ
let rcMetadataWithRecords records typ = metadata records M.empty typ

let makeProgram instrs variants records =
  let label = L.Label "_start_entry" in
  let block : L.basicBlock = { L.label; instrs; terminator = L.Ret } in
  let func : L.functionDef =
    {
      L.id = TestIds.functionIdForName "_start";
      name = "_start";
      typedParams = [];
      cfg = { L.entry = label; blocks = L.LabelMap.singleton label block };
      stackSize = 0;
      usedCalleeSaved = [];
      codegenFacts = None;
    }
  in
  L.Program ([ func ], variants, records)

let makeSimpleProgramWithVariants instrs variants =
  makeProgram instrs variants M.empty

let makeSimpleProgramWithRecords instrs records =
  makeProgram instrs M.empty records
