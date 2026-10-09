(*
   PlanningBoundaryTests.mli - Verify code-generation planning preconditions and cost attribution.
*)
val testRejectsUnpreparedCodegenFacts : unit -> (unit, string) result

val testLirOpExpansionRecorderAttributesGeneratedInstructions :
  unit -> (unit, string) result
