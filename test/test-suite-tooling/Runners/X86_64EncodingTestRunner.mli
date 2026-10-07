(*
   X86_64EncodingTestRunner.fs - Executes x64 encoding and label-resolution fixtures.
   Reports final byte streams and deferred fixup labels with stable diagnostics.
*)
val runX64EncodingTest : X86_64EncodingFormat.x64EncodingTest -> TestOutcome.t
val loadX64EncodingTests : string -> (X86_64EncodingFormat.x64EncodingTest list,string) result
val tests : string array -> (string*(unit -> (unit,string) result)) list
