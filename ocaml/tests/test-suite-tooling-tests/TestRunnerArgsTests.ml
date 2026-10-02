(* TestRunnerArgsTests.ml - Retain every original runner argument test. *)
open TestRunnerArgs
type testResult = (unit, string) result
let expectEqual expected actual = if actual = expected then Ok () else Error "Expected value differs from actual value"
let testTimingsJsonParsesPath () =
  Result.bind (parseTimingsJsonArg [|"--timings-json=/tmp/timings.json"|]) (expectEqual (Some "/tmp/timings.json"))
let testTimingsJsonRejectsEmptyPath () =
  match parseTimingsJsonArg [|"--timings-json="|] with
  | Error "--timings-json requires a non-empty path" -> Ok ()
  | Error _ | Ok _ -> Error "Expected invalid timings JSON path"
let testCodegenProfileJsonParsesPath () =
  Result.bind (parseCodegenProfileJsonArg [|"--codegen-profile-json=/tmp/codegen.json"|]) (expectEqual (Some "/tmp/codegen.json"))
let testE2EBatchSizeParsesBoundedSize () =
  Result.bind (parseE2EBatchSizeArg [|"--e2e-batch-size=8192"|]) (expectEqual 8192)
let testE2EBatchSizeDefaultsToBatching () =
  Result.bind (parseE2EBatchSizeArg [||]) (expectEqual defaultE2EBatchSize)
let testE2EBatchSizeRejectsInvalidValues () =
  let results = List.map (fun value -> parseE2EBatchSizeArg [|"--e2e-batch-size=" ^ value|]) ["0"; "8193"; "many"] in
  if List.for_all Result.is_error results then Ok () else Error "Expected invalid E2E batch sizes to fail"
let testTargetDefaultsToHost () = Result.bind (parseTargetArg [||]) (expectEqual Host)
let testTargetParsesLinuxX86_64 () =
  Result.bind (parseTargetArg [|"--target=linux-x86_64"|]) (expectEqual (Explicit Dark_compiler.Platform.LinuxX86_64))
let testTargetRejectsUnsupportedAndDuplicateValues () =
  let results = List.map parseTargetArg [[|"--target=linux-arm64"|]; [|"--target=host"; "--target=linux-x86_64"|]] in
  if List.for_all Result.is_error results then Ok () else Error "Expected invalid test target selections to fail"
let tests = [
  "timings JSON parses path", testTimingsJsonParsesPath;
  "timings JSON rejects empty path", testTimingsJsonRejectsEmptyPath;
  "codegen profile JSON parses path", testCodegenProfileJsonParsesPath;
  "E2E batch size parses a bounded size", testE2EBatchSizeParsesBoundedSize;
  "E2E batch size defaults to batching", testE2EBatchSizeDefaultsToBatching;
  "E2E batch size rejects invalid values", testE2EBatchSizeRejectsInvalidValues;
  "test target defaults to host", testTargetDefaultsToHost;
  "test target parses Linux x86_64", testTargetParsesLinuxX86_64;
  "test target rejects unsupported and duplicate values", testTargetRejectsUnsupportedAndDuplicateValues;
]
