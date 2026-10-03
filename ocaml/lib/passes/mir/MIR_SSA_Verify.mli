(* MIR_SSA_Verify.fs - Check SSA MIR definitions, dominance and phi edges. *)
val verifyFunction : MIR.functionDef -> (unit, string) result
