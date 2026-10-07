(* Complete typed LIR values in original test failure diagnostics. *)
open Dark_compiler

val physReg : LIR.physReg -> StructuralValue.value
val physFPReg : LIR.physFPReg -> StructuralValue.value
val reg : LIR.reg -> StructuralValue.value
val fReg : LIR.fReg -> StructuralValue.value
val typedLIRParam : LIR.typedLIRParam -> StructuralValue.value
val operand : LIR.operand -> StructuralValue.value
val condition : LIR.condition -> StructuralValue.value
val rcKind : LIR.rcKind -> StructuralValue.value
val cliOperation : LIR.cliOperation -> StructuralValue.value
val label : LIR.label -> StructuralValue.value
val instr : LIR.instr -> StructuralValue.value
val terminator : LIR.terminator -> StructuralValue.value
val basicBlock : LIR.basicBlock -> StructuralValue.value
val cfg : LIR.cfg -> StructuralValue.value
val rcReleasePlanMemoKey : LIR.rcReleasePlanMemoKey -> StructuralValue.value

val arm64ReleasePlanSummary :
  LIR.arm64ReleasePlanSummary -> StructuralValue.value

val arm64PlannedGenericDecHelper :
  LIR.arm64PlannedGenericDecHelper -> StructuralValue.value

val arm64RcHelperRequirements :
  LIR.arm64RcHelperRequirements -> StructuralValue.value

val arm64SlotInitRootRetainTarget :
  LIR.arm64SlotInitRootRetainTarget -> StructuralValue.value

val functionCodegenFacts : LIR.functionCodegenFacts -> StructuralValue.value
val functionDef : LIR.functionDef -> StructuralValue.value
val recordRegistry : LIR.recordRegistry -> StructuralValue.value
val variantInfo : LIR.variantInfo -> StructuralValue.value
val typeVariants : LIR.typeVariants -> StructuralValue.value
val variantRegistry : LIR.variantRegistry -> StructuralValue.value
val program : LIR.program -> StructuralValue.value
