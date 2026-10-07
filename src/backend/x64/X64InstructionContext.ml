(*
   X64InstructionContext.ml - Shared operand context for x64 instruction-family lowering.
   Adjust a stack slot offset to account for callee-saved registers pushed after RBP.
   LIR stack slots are byte offsets from FP (e.g., -8, -16), but callee-saved pushes
   occupy [RBP-8] through [RBP-N*8], so spill slots must be shifted past them.
*)
let adjustStackOffset (ctx:X64CodeGenTypes.funcCtx) offset =
 Int32.to_int (Int32.sub (Int32.of_int offset) (Int32.mul (Int32.of_int (List.length ctx.X64CodeGenTypes.usedCalleeSaved)) 8l))
(*
   The comparison whose flags condition consumers read within a basic block.
   UCOMISD sets CF/ZF differently from CMP, which sets SF/OF/ZF.
   Translate a single LIR instruction to x86-64 instructions
*)
type comparisonContext = IntegerComparison | FloatComparison
