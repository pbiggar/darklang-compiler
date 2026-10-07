(* MIR_SSA_Verify.mli - Check SSA MIR definitions, dominance and phi edges. *)
val verifyFunction : MIR.functionDef -> (unit, string) result
