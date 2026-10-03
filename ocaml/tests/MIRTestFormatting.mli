open Dark_compiler
(* Typed formatting of complete MIR values in original test diagnostics. *)
val vReg : MIR.vReg -> StructuralValue.value
val typedMIRParam : MIR.typedMIRParam -> StructuralValue.value
val operand : MIR.operand -> StructuralValue.value
val binOp : MIR.binOp -> StructuralValue.value
val unaryOp : MIR.unaryOp -> StructuralValue.value
val rcKind : MIR.rcKind -> StructuralValue.value
val cliOperation : MIR.cliOperation -> StructuralValue.value
val label : MIR.label -> StructuralValue.value
val instr : MIR.instr -> StructuralValue.value
val terminator : MIR.terminator -> StructuralValue.value
val basicBlock : MIR.basicBlock -> StructuralValue.value
val cfg : MIR.cfg -> StructuralValue.value
val functionDef : MIR.functionDef -> StructuralValue.value
val variantInfo : MIR.variantInfo -> StructuralValue.value
val typeVariants : MIR.typeVariants -> StructuralValue.value
val recordField : MIR.recordField -> StructuralValue.value
val program : MIR.program -> StructuralValue.value
val regGen : MIR.regGen -> StructuralValue.value
val labelGen : MIR.labelGen -> StructuralValue.value
val variantRegistry : MIR.variantRegistry -> StructuralValue.value
val recordRegistry : MIR.recordRegistry -> StructuralValue.value
val functionId : AST.functionId -> StructuralValue.value
