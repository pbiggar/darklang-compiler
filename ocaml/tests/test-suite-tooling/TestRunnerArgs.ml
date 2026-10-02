(* TestRunnerArgs.ml - Parse unchanged runner flags and Unicode name filters. *)
open Dark_compiler
type testTarget = Host | Explicit of Platform.target
let parsePrefixedArg prefix args =
  Array.find_opt (String.starts_with ~prefix) args
  |> Option.map (fun arg -> String.sub arg (String.length prefix) (String.length arg - String.length prefix))
let parseFilterArg args = parsePrefixedArg "--filter=" args
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
let hasCoverageArg = has "--coverage"
let hasVerificationArg = has "--verification"
let hasVerboseArg args = has "--verbose" args || has "-v" args
let hasParserPrettyRoundtripArg = has "--parser-pretty-roundtrip"
let hasRoundtripAllDarkArg = has "--roundtrip-all-dark"
let hasAllTestTimingsArg = has "--all-test-timings"
let hasQuietArg = has "--quiet"
let hasAiArg = has "--ai"
let parsePath flag args =
  match parsePrefixedArg (flag ^ "=") args with
  | None -> Ok None
  | Some path when HostText.trim path = "" -> Error (flag ^ " requires a non-empty path")
  | Some path -> Ok (Some path)
let parseTimingsJsonArg = parsePath "--timings-json"
let parseCodegenProfileJsonArg = parsePath "--codegen-profile-json"
(* The frozen runner's public batching limit; retain its value until the
   translated E2E runner owns the shared limit. *)
let defaultE2EBatchSize = 8192
let parseE2EBatchSizeArg args =
  match parsePrefixedArg "--e2e-batch-size=" args with
  | None -> Ok defaultE2EBatchSize
  | Some value -> match HostText.tryParseInt32 value with
      | Some size when size >= 1l && size <= 8192l -> Ok (Int32.to_int size)
      | _ -> Error "--e2e-batch-size requires an integer from 1 through 8192"
let matchesFilter filter testName =
  match filter with
  | None -> true
  | Some pattern -> HostText.contains (HostText.lowerInvariant testName) (HostText.lowerInvariant pattern)
let hasHelpArg args = has "--help" args || has "-h" args
