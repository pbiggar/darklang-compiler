(*
   ARM64EncodingTestRunner.mli - Test runner for ARM64 encoding tests
   Loads ARM64 encoding test files (.arm64enc), runs the ARM64 encoder,
   and compares the output with expected machine code hex values.
   Load ARM64 encoding test from file
   Check if all values in a list are different
   Format encoding mismatches for display
   Run ARM64 encoding test
   Encode each instruction
   Check each encoding matches expected
   Check if all values are different (if required)
*)
val loadARM64EncodingTest : string -> (ARM64EncodingFormat.arm64EncodingTest,string) result
val hasAllDifferent : int32 list -> bool
val formatMismatches : (int*Dark_compiler.ARM64.instr*int32*int32) list -> string
val runARM64EncodingTest : ARM64EncodingFormat.arm64EncodingTest -> TestOutcome.t
