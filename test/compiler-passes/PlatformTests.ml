(*
   PlatformTests.ml - Tests for validated compiler target classification.
   These tests keep supported OS/architecture pairs explicit without depending
   on the host running the test suite.
*)
(* PlatformTests.ml - Retain explicit supported target classifications. *)
open Dark_compiler
type testResult = (unit, string) result
let expectTarget os arch expected =
  match Platform.targetFor os arch with
  | Ok actual when actual = expected -> Ok ()
  | Ok _ -> Error "Expected target differs from actual target"
  | Error error -> Error ("Expected a supported target, got error: " ^ error)
let testTargetForRepresentsSupportedPairs () =
  Result.bind
    (expectTarget Platform.MacOS Platform.ARM64 (Platform.ARM64Backend Platform.MacOSARM64))
    (fun () -> Result.bind
      (expectTarget Platform.Linux Platform.ARM64 (Platform.ARM64Backend Platform.LinuxARM64))
      (fun () -> expectTarget Platform.Linux Platform.X86_64 Platform.LinuxX86_64))
let testTargetForRejectsMacOSX86_64 () =
  match Platform.targetFor Platform.MacOS Platform.X86_64 with
  | Error _ -> Ok ()
  | Ok _ -> Error "Expected macOS x86_64 to be rejected"
let tests = [
  "targetFor represents supported pairs", testTargetForRepresentsSupportedPairs;
  "targetFor rejects macOS x86_64", testTargetForRejectsMacOSX86_64;
]
