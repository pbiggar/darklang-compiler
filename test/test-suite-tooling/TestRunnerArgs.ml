(*
   TestRunnerArgs.ml - Helper functions for test runner CLI arguments
   Provides parsing helpers shared by the test runner and its unit tests.
*)
(* TestRunnerArgs.ml - Parse unchanged runner flags and Unicode name filters. *)
open Dark_compiler
type testTarget = Host | Explicit of Platform.target
let parsePrefixedArg prefix args =
  Array.find_opt (String.starts_with ~prefix) args
  |> Option.map (fun arg -> String.sub arg (String.length prefix) (String.length arg - String.length prefix))
(*
   Parse command line for --filter=PATTERN option
*)
let parseFilterArg args = parsePrefixedArg "--filter=" args
(*
   Select the architecture whose backend and E2E behavior this invocation
   validates. Host remains the default so cross-target work is always opt-in.
*)
let parseTargetArg args =
  let values = Array.to_list args |> List.filter_map (fun arg ->
    if String.starts_with ~prefix:"--target=" arg then
      Some (String.sub arg 9 (String.length arg - 9)) else None) in
  match values with
  | [] | ["host"] -> Ok Host
  | ["linux-x86_64"] -> Ok (Explicit Platform.LinuxX86_64)
  | [value] -> Error ("Unsupported test target '" ^ value ^ "' (expected 'host' or 'linux-x86_64')")
  | _ -> Error "--target may be specified only once"
let has flag args = Array.exists ((=) flag) args
(*
   Check if --coverage flag is present (show inline coverage after tests)
*)
let hasCoverageArg = has "--coverage"
(*
   Check if --verification flag is present (enable verification/stress tests)
*)
let hasVerificationArg = has "--verification"
(*
   Check if --verbose flag is present (print failing tests immediately)
*)
let hasVerboseArg args = has "--verbose" args || has "-v" args
(*
   Check if --parser-pretty-roundtrip is present (legacy compatibility no-op)
*)
let hasParserPrettyRoundtripArg = has "--parser-pretty-roundtrip"
(*
   Check if --roundtrip-all-dark is present (include all upstream .dark files in corpus roundtrip)
*)
let hasRoundtripAllDarkArg = has "--roundtrip-all-dark"
(*
   Check if --all-test-timings is present (print timing for every test)
*)
let hasAllTestTimingsArg = has "--all-test-timings"
(*
   Check if quiet mode is present (compact success/failure output)
*)
let hasQuietArg = has "--quiet"
(*
   Check if AI mode is present (compact output with test-count progress)
*)
let hasAiArg = has "--ai"
let parsePath flag args =
  match parsePrefixedArg (flag ^ "=") args with
  | None -> Ok None
  | Some path when Text.trim path = "" -> Error (flag ^ " requires a non-empty path")
  | Some path -> Ok (Some path)
(*
   Parse --timings-json=PATH option
*)
let parseTimingsJsonArg = parsePath "--timings-json"
(*
   Parse --codegen-profile-json=PATH option
*)
let parseCodegenProfileJsonArg = parsePath "--codegen-profile-json"
(* The frozen runner's public batching limit; retain its value until the
   translated E2E runner owns the shared limit. *)
(*
   By default, place every compatible contiguous E2E check in the same process.
   A finite runner bound still protects explicit command-line input and future
   corpus growth from producing an arbitrarily large compiler input.
*)
let defaultE2EBatchSize = 8192
(*
   Parse --e2e-batch-size=N. One preserves singular execution for comparison;
   larger values batch compatible value-equality tests.
*)
let parseE2EBatchSizeArg args =
  match parsePrefixedArg "--e2e-batch-size=" args with
  | None -> Ok defaultE2EBatchSize
  | Some value -> match Text.tryParseInt32 value with
      | Some size when size >= 1l && size <= 8192l -> Ok (Int32.to_int size)
      | _ -> Error "--e2e-batch-size requires an integer from 1 through 8192"
(*
   Check if a test name matches the filter (case-insensitive substring match)
*)
let matchesFilter filter testName =
  match filter with
  | None -> true
  | Some pattern -> Text.contains (Text.lowerInvariant testName) (Text.lowerInvariant pattern)
(*
   Check if --help flag is present
*)
let hasHelpArg args = has "--help" args || has "-h" args
