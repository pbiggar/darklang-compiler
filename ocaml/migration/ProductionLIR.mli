(* Complete typed symbolic LIR encoders. *)
open Dark_compiler
val physReg : LIR.physReg -> Yojson.Basic.t
val physFPReg : LIR.physFPReg -> Yojson.Basic.t
val reg : LIR.reg -> Yojson.Basic.t
val fReg : LIR.fReg -> Yojson.Basic.t
val typedLIRParam : LIR.typedLIRParam -> Yojson.Basic.t
val operand : LIR.operand -> Yojson.Basic.t
val condition : LIR.condition -> Yojson.Basic.t
val rcKind : LIR.rcKind -> Yojson.Basic.t
val cliOperation : LIR.cliOperation -> Yojson.Basic.t
val label : LIR.label -> Yojson.Basic.t
val instr : LIR.instr -> Yojson.Basic.t
val terminator : LIR.terminator -> Yojson.Basic.t
val basicBlock : LIR.basicBlock -> Yojson.Basic.t
val cfg : LIR.cfg -> Yojson.Basic.t
val rcReleasePlanMemoKey : LIR.rcReleasePlanMemoKey -> Yojson.Basic.t
val arm64ReleasePlanSummary : LIR.arm64ReleasePlanSummary -> Yojson.Basic.t
val arm64PlannedGenericDecHelper : LIR.arm64PlannedGenericDecHelper -> Yojson.Basic.t
val arm64RcHelperRequirements : LIR.arm64RcHelperRequirements -> Yojson.Basic.t
val arm64SlotInitRootRetainTarget : LIR.arm64SlotInitRootRetainTarget -> Yojson.Basic.t
val functionCodegenFacts : LIR.functionCodegenFacts -> Yojson.Basic.t
val functionDef : LIR.functionDef -> Yojson.Basic.t
val recordRegistry : LIR.recordRegistry -> Yojson.Basic.t
val variantInfo : LIR.variantInfo -> Yojson.Basic.t
val typeVariants : LIR.typeVariants -> Yojson.Basic.t
val variantRegistry : LIR.variantRegistry -> Yojson.Basic.t
val program : LIR.program -> Yojson.Basic.t
