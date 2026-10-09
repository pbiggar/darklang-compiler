type testResult = (unit, string) result

val testEncodeReg : unit -> testResult
val testMOVKShiftEncoding : unit -> testResult
val testMOVZMOVKSequence : unit -> testResult
val testCombinedInstructionEncoding : unit -> testResult
val testUnsignedMemoryOffsetsRejectInvalidValues : unit -> testResult
val testSignedPairOffsetsRejectInvalidValues : unit -> testResult
val testArithmeticImmediatesRejectOutOfRangeValues : unit -> testResult
val testMoveWideShiftsRejectInvalidValues : unit -> testResult
val testFMOVImmediateEncoding : unit -> testResult
val testBICRegisterEncoding : unit -> testResult
val testPreparedChunksPreserveWholeProgramEncoding : unit -> testResult
val testRotatedLogicalImmediateEncoding : unit -> testResult
val testBytePopcountSequenceEncoding : unit -> testResult
val testInvalidAssertDifferentValueIsRejected : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
