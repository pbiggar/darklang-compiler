type testResult=(unit,string) result
val testLiteralFirstUseLayout : unit -> testResult
val testUint32ToBytes : unit -> testResult
val testUint64ToBytes : unit -> testResult
val testPadString : unit -> testResult
val testPadStringTruncate : unit -> testResult
val testSerializeMachHeaderSize : unit -> testResult
val testSerializeMachHeaderMagic : unit -> testResult
val testSerializeSection64Size : unit -> testResult
val testCreateExecutableNonEmpty : unit -> testResult
val testCreateExecutableMagic : unit -> testResult
val testCreateExecutableContainsCode : unit -> testResult
val testMachOConstSectionOffsetPointsToAlignedData : unit -> testResult
val testElfWriteToFileReturnsErrorForInvalidPath : unit -> testResult
val testCreateExecutableWithCoverageIncludesCoverageSection : unit -> testResult
val testSerializeMachOReportsInvalidCodeOffset : unit -> testResult
val testSerializeMachOReportsTextSegmentTooSmall : unit -> testResult
val testCompleteEncodingPipeline : unit -> testResult
val testExecuteLinuxElf : unit -> testResult
val testWriteToFileReturnsErrorForInvalidPath : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
