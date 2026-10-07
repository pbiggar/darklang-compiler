(*
   ASTToANFTests.mli - Unit tests for AST to ANF conversion behavior
   Covers targeted AST-to-ANF regression cases that are easier to express
   directly at the pass boundary than through end-to-end language tests.
*)
type testResult=(unit,string) result
val testMissingVariantPayloadTypeErrors : unit -> testResult
val testNeedsLambdaLoweringIgnoresShadowedFunc : unit -> testResult
val testNeedsLambdaLoweringDetectsFuncValue : unit -> testResult
val testNeedsLambdaLoweringDetectsLambda : unit -> testResult
val testMangledTypePreservesFreshenedTypeVariables : unit -> testResult
val testMangledFunctionTypePreservesSyntheticTypeVariables : unit -> testResult
val testSyntheticNullaryCallLowersToZeroArgs : unit -> testResult
val testSyntheticUnitParamLowersFunctionToZeroParams : unit -> testResult
val testErasedListHeadPatternLowersToBorrowedCall : unit -> testResult
val testTypedListHeadPatternRemainsOwnedCall : unit -> testResult
val testTypedParamAllocationPreservesOrder : unit -> testResult
val testOverlayFunctionIdsContainOnlyLocalDefinitions : unit -> testResult
val tests : (string*(unit -> testResult)) list
