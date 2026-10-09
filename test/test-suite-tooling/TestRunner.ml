(*
   TestRunner.ml - Test runner entrypoint and suite orchestration.

   Defines Dark compiler test suites and their execution order.
   Print help message
   Check for --help flag
   Check for --filter=PATTERN argument
   Check for --coverage flag (show inline coverage after tests)
   Check for --verification flag (enable verification/stress tests)
   Parser/pretty corpus roundtrip always runs by default.
   Keep --parser-pretty-roundtrip accepted as a compatibility no-op.
   Include every upstream .dark file in corpus roundtrip mode.
   Print timing for every test in the final timing section.
   Check for --verbose flag (print failing tests immediately)
   Optionally write machine-readable timing data to JSON.
   Use the source tree for test data to avoid copying files into the build output.
   A filter may name an expression rather than its source file.
   Load the enabled upstream inventory so matchesE2EFilter below can
   select those expression names instead of incorrectly reporting 0/0.
   Keep known unsupported cases discoverable while running every other upstream
   test by default. Paths are suffixes so this remains independent of worktree.
   Run ANF→MIR tests
   Run MIR→LIR tests
   Run LIR→ARM64 tests
   Run ARM64 encoding tests
   Run Type Checking tests
   Run Optimization Tests (ANF, MIR, LIR)
   Order unit test suites so non-stdlib tests run first.
   Compute stdlib coverage only if --coverage flag is set
   Print slowest tests
   Print first 10 failing tests summary
   Format: "1. E2E: file.e2e:L43: expression" with file:line in cyan
   "L43: expression" (skip "E2E: ")
*)
[@@@warning "-4-30-42"]

open Dark_compiler
module M = StringOrder.Map
module C = TestRunnerColors
module F = TestFramework
module E = E2EFormat
module R = E2ETestRunner
module O = CompilerOptions

let println = Output.println
let get = function Ok x -> x | Error error -> Crash.crash error
let now () = Mtime_clock.elapsed_ns ()
let elapsed start = Int64.sub (now ()) start

let arrayFilter predicate values =
  Array.to_list values |> List.filter predicate |> Array.of_list

let queueList queue = Queue.to_seq queue |> List.of_seq
let textLength text = Array.length (Text.scalars text)

let substring text start count =
  Text.ofScalars (Array.sub (Text.scalars text) start count)

let truncateName maximum retained text =
  if textLength text > maximum then substring text 0 retained ^ "..." else text

let padRight text width =
  text ^ String.make (max 0 (width - textLength text)) ' '

let padLeft text width =
  String.make (max 0 (width - textLength text)) ' ' ^ text

let replace text pattern replacement =
  let length = String.length pattern in
  let buffer = Buffer.create (String.length text) in
  let rec loop offset =
    if offset >= String.length text then Buffer.contents buffer
    else if
      offset + length <= String.length text
      && String.sub text offset length = pattern
    then (
      Buffer.add_string buffer replacement;
      loop (offset + length))
    else (
      Buffer.add_char buffer text.[offset];
      loop (offset + 1))
  in
  if length = 0 then Crash.crash "Cannot replace an empty diagnostic pattern"
  else loop 0

let aiFailureLimit = 5
let aiMessageCharacterLimit = 1200
let aiDetailsCharacterLimit = 2400
let utf8Replacement = Utf8.utf8

let truncateDiagnostic maximum text =
  let text = utf8Replacement text in
  let maximum = max 0 maximum in
  let length = textLength text in
  if length <= maximum then text
  else if maximum = 0 then ""
  else
    let suffix =
      Printf.sprintf "... [truncated %d characters]" (length - maximum)
    in
    if textLength suffix >= maximum then substring suffix 0 maximum
    else substring text 0 (maximum - textLength suffix) ^ suffix

let truncateDiagnosticDetails maximum values =
  let rec loop remaining acc = function
    | [] -> List.rev acc
    | _ when remaining <= 0 -> List.rev acc
    | value :: rest ->
        let rendered = truncateDiagnostic remaining value in
        loop (remaining - textLength rendered) (rendered :: acc) rest
  in
  loop maximum [] values

let printHelp () =
  List.iter println
    [
      "Usage: Tests [OPTIONS]";
      "";
      "Options:";
      "  --filter=PATTERN   Run only tests matching PATTERN (case-insensitive \
       substring)";
      "  --coverage         Show stdlib coverage percentage after running tests";
      "  --verification     Enable verification/stress tests";
      "  --parser-pretty-roundtrip  Legacy no-op (parser/pretty corpus \
       roundtrip runs by default)";
      "  --roundtrip-all-dark  Include all upstream .dark files in \
       parser/pretty corpus roundtrip";
      "  --all-test-timings  Print timing for every test in final timing \
       summary";
      "  --e2e-batch-size=N  Compile up to N compatible E2E checks together \
       (default/max 8192; 1 disables)";
      "  --target=TARGET     Validate host (default) or linux-x86_64";
      "  --timings-json=PATH  Write machine-readable timing data to PATH";
      "  --codegen-profile-json=PATH  Write opt-in per-function ARM64 codegen \
       metrics";
      "  --quiet            Quiet mode: print 'success' or list failed tests";
      "  --ai               AI mode: compact output with a dot every 250 \
       completed tests";
      "  --verbose, -v      Print failing tests as soon as they occur";
      "  --help, -h         Show this help message";
      "";
      "Examples:";
      "  Tests                      Run all tests";
      "  Tests --filter=tuple       Run tests with 'tuple' in the name";
      "  Tests --filter=string      Run tests with 'string' in the name";
      "  Tests --coverage           Run tests and show coverage percentage";
      "  Tests --verification       Run verification/stress tests";
      "  Tests --parser-pretty-roundtrip  Legacy no-op (corpus roundtrip \
       already enabled)";
      "  Tests --roundtrip-all-dark  Roundtrip all upstream .dark files and \
       stop on first error";
      "  Tests --all-test-timings  Show timing for every test at the end";
      "  Tests --target=linux-x86_64  Run x64 backend and E2E tests (QEMU when \
       cross-target)";
      "  Tests --timings-json=/tmp/timings.json  Write timing data as JSON";
    ]

type testRunResult = {
  exitCode : int;
  state : F.testRunState;
  totalTime : int64;
  unaccountedBreakdown : F.unaccountedTimeBreakdown;
}

let aiProgressTestInterval = 250

let emptyRunResult exitCode =
  {
    exitCode;
    state = F.createState ();
    totalTime = 0L;
    unaccountedBreakdown = { F.unaccounted = 0L; runtime = 0L; overhead = 0L };
  }

type timingJsonSummary = {
  passed : int;
  failed : int;
  total : int;
  total_ms : float;
  unaccounted_ms : float;
  runtime_unaccounted_ms : float;
  overhead_unaccounted_ms : float;
  e2e_batch_size : int;
  e2e_logical_tests : int;
  e2e_batch_eligible_tests : int;
  e2e_physical_executions : int;
  e2e_batch_executions : int;
  e2e_batched_logical_tests : int;
  e2e_largest_batch : int;
}

type timingJsonTest = {
  name : string;
  total_ms : float;
  compile_ms : float option;
  runtime_ms : float option;
}

type timingJsonPass = { name : string; elapsed_ms : float; invocations : int }

type timingJsonPayload = {
  summary : timingJsonSummary;
  tests : timingJsonTest array;
  passes : timingJsonPass array;
}

type codegenProfileFunction = {
  name : string;
  category : string;
  generations : int;
  elapsed_ms : float;
  lir_instructions : int;
  symbolic_instructions : int;
}

type codegenProfileCategory = {
  name : string;
  elapsed_ms : float;
  percentage_of_codegen : float;
  functions : int;
  generations : int;
}

type codegenProfilePhase = {
  name : string;
  elapsed_ms : float;
  percentage_of_codegen : float;
}

type codegenProfileLirOp = {
  name : string;
  occurrences : int;
  elapsed_ms : float;
  symbolic_instructions_before_peephole : int;
  average_symbolic_instructions_before_peephole : float;
}

type codegenProfileLirOpFunction = {
  function_name : string;
  category : string;
  opcode : string;
  detail : string;
  occurrences : int;
  elapsed_ms : float;
  symbolic_instructions_before_peephole : int;
}

type codegenProfileSummary = {
  codegen_ms : float;
  attributed_function_ms : float;
  program_overhead_ms : float;
  cache_hits : int;
  cache_misses : int;
  release_plan_summary_cache_hits : int;
  release_plan_summary_cache_misses : int;
  json_plan_cache_hits : int;
  json_plan_cache_misses : int;
  anf_dependency_cache_hits : int;
  anf_dependency_cache_misses : int;
  compiled_dependency_cache_hits : int;
  compiled_dependency_cache_misses : int;
  mir_optimization_cache_hits : int;
  mir_optimization_cache_misses : int;
  allocated_lir_function_cache_hits : int;
  allocated_lir_function_cache_misses : int;
  stdlib_reachability_cache_hits : int;
  stdlib_reachability_cache_misses : int;
  metadata_group_cache_hits : int;
  metadata_group_cache_misses : int;
  helper_cache_hits : int;
  helper_cache_misses : int;
  start_codegen_cache_hits : int;
}

type codegenProfilePayload = {
  schema_version : int;
  summary : codegenProfileSummary;
  phases : codegenProfilePhase array;
  categories : codegenProfileCategory array;
  functions : codegenProfileFunction array;
  lir_ops : codegenProfileLirOp array;
  lir_op_functions : codegenProfileLirOpFunction array;
}

let roundedMilliseconds value =
  let scaled = value *. 1000. in
  let lower = Float.floor scaled in
  let fraction = scaled -. lower in
  (if fraction > 0.5 || (fraction = 0.5 && mod_float lower 2. <> 0.) then
     lower +. 1.
   else lower)
  /. 1000.

let milliseconds elapsed = roundedMilliseconds (Int64.to_float elapsed /. 1e6)
let optionalMilliseconds value = Option.map milliseconds value

let jsonString value =
  let buffer = Buffer.create (String.length value + 2) in
  Buffer.add_char buffer '"';
  Array.iter
    (fun code ->
      match code with
      | 8 -> Buffer.add_string buffer "\\b"
      | 9 -> Buffer.add_string buffer "\\t"
      | 10 -> Buffer.add_string buffer "\\n"
      | 12 -> Buffer.add_string buffer "\\f"
      | 13 -> Buffer.add_string buffer "\\r"
      | 92 -> Buffer.add_string buffer "\\\\"
      | code
        when code < 32 || code > 126
             || List.mem code [ 34; 38; 39; 43; 60; 62; 96 ] ->
          Buffer.add_string buffer (Printf.sprintf "\\u%04X" code)
      | code -> Buffer.add_char buffer (Char.chr code))
    (Text.scalars value);
  Buffer.add_char buffer '"';
  Buffer.contents buffer

type json =
  | JString of string
  | JInt of int
  | JFloat of float
  | JArray of json list
  | JObject of (string * json) list

let rec serializeJson indent depth value =
  let pad n = String.make (n * 2) ' ' in
  let sequence left right values =
    if values = [] then left ^ right
    else if not indent then left ^ String.concat "," values ^ right
    else
      left ^ "\n"
      ^ String.concat ",\n" (List.map (fun s -> pad (depth + 1) ^ s) values)
      ^ "\n" ^ pad depth ^ right
  in
  match value with
  | JString s -> jsonString s
  | JInt n -> string_of_int n
  | JFloat f -> FloatFormat.roundTrip f
  | JArray xs ->
      sequence "[" "]" (List.map (serializeJson indent (depth + 1)) xs)
  | JObject xs ->
      sequence "{" "}"
        (List.map
           (fun (k, v) ->
             jsonString k
             ^ (if indent then ": " else ":")
             ^ serializeJson indent (depth + 1) v)
           xs)

let rec createDirectory path =
  if path <> "" && path <> "." && not (Sys.file_exists path) then (
    createDirectory (Filename.dirname path);
    Unix.mkdir path 0o777)

let writeJson indent path value =
  createDirectory (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel (serializeJson indent 0 value))

let json_timingJsonSummary (value : timingJsonSummary) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("passed", JInt value.passed);
         Some ("failed", JInt value.failed);
         Some ("total", JInt value.total);
         Some ("total_ms", JFloat value.total_ms);
         Some ("unaccounted_ms", JFloat value.unaccounted_ms);
         Some ("runtime_unaccounted_ms", JFloat value.runtime_unaccounted_ms);
         Some ("overhead_unaccounted_ms", JFloat value.overhead_unaccounted_ms);
         Some ("e2e_batch_size", JInt value.e2e_batch_size);
         Some ("e2e_logical_tests", JInt value.e2e_logical_tests);
         Some ("e2e_batch_eligible_tests", JInt value.e2e_batch_eligible_tests);
         Some ("e2e_physical_executions", JInt value.e2e_physical_executions);
         Some ("e2e_batch_executions", JInt value.e2e_batch_executions);
         Some ("e2e_batched_logical_tests", JInt value.e2e_batched_logical_tests);
         Some ("e2e_largest_batch", JInt value.e2e_largest_batch);
       ])

let json_timingJsonTest (value : timingJsonTest) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("name", JString value.name);
         Some ("total_ms", JFloat value.total_ms);
         Option.map (fun n -> ("compile_ms", JFloat n)) value.compile_ms;
         Option.map (fun n -> ("runtime_ms", JFloat n)) value.runtime_ms;
       ])

let json_timingJsonPass (value : timingJsonPass) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("name", JString value.name);
         Some ("elapsed_ms", JFloat value.elapsed_ms);
         Some ("invocations", JInt value.invocations);
       ])

let json_timingJsonPayload (value : timingJsonPayload) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("summary", json_timingJsonSummary value.summary);
         Some
           ( "tests",
             JArray (Array.to_list value.tests |> List.map json_timingJsonTest)
           );
         Some
           ( "passes",
             JArray (Array.to_list value.passes |> List.map json_timingJsonPass)
           );
       ])

let json_codegenProfileFunction (value : codegenProfileFunction) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("name", JString value.name);
         Some ("category", JString value.category);
         Some ("generations", JInt value.generations);
         Some ("elapsed_ms", JFloat value.elapsed_ms);
         Some ("lir_instructions", JInt value.lir_instructions);
         Some ("symbolic_instructions", JInt value.symbolic_instructions);
       ])

let json_codegenProfileCategory (value : codegenProfileCategory) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("name", JString value.name);
         Some ("elapsed_ms", JFloat value.elapsed_ms);
         Some ("percentage_of_codegen", JFloat value.percentage_of_codegen);
         Some ("functions", JInt value.functions);
         Some ("generations", JInt value.generations);
       ])

let json_codegenProfilePhase (value : codegenProfilePhase) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("name", JString value.name);
         Some ("elapsed_ms", JFloat value.elapsed_ms);
         Some ("percentage_of_codegen", JFloat value.percentage_of_codegen);
       ])

let json_codegenProfileLirOp (value : codegenProfileLirOp) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("name", JString value.name);
         Some ("occurrences", JInt value.occurrences);
         Some ("elapsed_ms", JFloat value.elapsed_ms);
         Some
           ( "symbolic_instructions_before_peephole",
             JInt value.symbolic_instructions_before_peephole );
         Some
           ( "average_symbolic_instructions_before_peephole",
             JFloat value.average_symbolic_instructions_before_peephole );
       ])

let json_codegenProfileLirOpFunction (value : codegenProfileLirOpFunction) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("function_name", JString value.function_name);
         Some ("category", JString value.category);
         Some ("opcode", JString value.opcode);
         Some ("detail", JString value.detail);
         Some ("occurrences", JInt value.occurrences);
         Some ("elapsed_ms", JFloat value.elapsed_ms);
         Some
           ( "symbolic_instructions_before_peephole",
             JInt value.symbolic_instructions_before_peephole );
       ])

let json_codegenProfileSummary (value : codegenProfileSummary) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("codegen_ms", JFloat value.codegen_ms);
         Some ("attributed_function_ms", JFloat value.attributed_function_ms);
         Some ("program_overhead_ms", JFloat value.program_overhead_ms);
         Some ("cache_hits", JInt value.cache_hits);
         Some ("cache_misses", JInt value.cache_misses);
         Some
           ( "release_plan_summary_cache_hits",
             JInt value.release_plan_summary_cache_hits );
         Some
           ( "release_plan_summary_cache_misses",
             JInt value.release_plan_summary_cache_misses );
         Some ("json_plan_cache_hits", JInt value.json_plan_cache_hits);
         Some ("json_plan_cache_misses", JInt value.json_plan_cache_misses);
         Some ("anf_dependency_cache_hits", JInt value.anf_dependency_cache_hits);
         Some
           ( "anf_dependency_cache_misses",
             JInt value.anf_dependency_cache_misses );
         Some
           ( "compiled_dependency_cache_hits",
             JInt value.compiled_dependency_cache_hits );
         Some
           ( "compiled_dependency_cache_misses",
             JInt value.compiled_dependency_cache_misses );
         Some
           ( "mir_optimization_cache_hits",
             JInt value.mir_optimization_cache_hits );
         Some
           ( "mir_optimization_cache_misses",
             JInt value.mir_optimization_cache_misses );
         Some
           ( "allocated_lir_function_cache_hits",
             JInt value.allocated_lir_function_cache_hits );
         Some
           ( "allocated_lir_function_cache_misses",
             JInt value.allocated_lir_function_cache_misses );
         Some
           ( "stdlib_reachability_cache_hits",
             JInt value.stdlib_reachability_cache_hits );
         Some
           ( "stdlib_reachability_cache_misses",
             JInt value.stdlib_reachability_cache_misses );
         Some ("metadata_group_cache_hits", JInt value.metadata_group_cache_hits);
         Some
           ( "metadata_group_cache_misses",
             JInt value.metadata_group_cache_misses );
         Some ("helper_cache_hits", JInt value.helper_cache_hits);
         Some ("helper_cache_misses", JInt value.helper_cache_misses);
         Some ("start_codegen_cache_hits", JInt value.start_codegen_cache_hits);
       ])

let json_codegenProfilePayload (value : codegenProfilePayload) =
  JObject
    (List.filter_map Fun.id
       [
         Some ("schema_version", JInt value.schema_version);
         Some ("summary", json_codegenProfileSummary value.summary);
         Some
           ( "phases",
             JArray
               (Array.to_list value.phases |> List.map json_codegenProfilePhase)
           );
         Some
           ( "categories",
             JArray
               (Array.to_list value.categories
               |> List.map json_codegenProfileCategory) );
         Some
           ( "functions",
             JArray
               (Array.to_list value.functions
               |> List.map json_codegenProfileFunction) );
         Some
           ( "lir_ops",
             JArray
               (Array.to_list value.lir_ops |> List.map json_codegenProfileLirOp)
           );
         Some
           ( "lir_op_functions",
             JArray
               (Array.to_list value.lir_op_functions
               |> List.map json_codegenProfileLirOpFunction) );
       ])

type e2eStats = {
  mutable logicalTests : int;
  mutable eligibleTests : int;
  mutable executions : int;
  mutable batchExecutions : int;
  mutable batchedTests : int;
  mutable largestBatch : int;
}

type executionUnit =
  | Single of int * E.e2eTest * R.preparedE2EBatchTest option
  | Batch of (int * R.preparedE2EBatchTest) list

let runTestsWithProgressReporter completedTestReporter args =
  if TestRunnerArgs.hasHelpArg args then (
    printHelp ();
    emptyRunResult 0)
  else
    let totalStart = now () in
    let filter = TestRunnerArgs.parseFilterArg args in
    let showCoverage = TestRunnerArgs.hasCoverageArg args
    and verificationEnabled = TestRunnerArgs.hasVerificationArg args in
    let _parserPrettyRoundtripFlagPresent =
      TestRunnerArgs.hasParserPrettyRoundtripArg args
    in
    let roundtripAllDark = TestRunnerArgs.hasRoundtripAllDarkArg args
    and showAllTestTimings = TestRunnerArgs.hasAllTestTimingsArg args
    and verbose = TestRunnerArgs.hasVerboseArg args in
    let timingsJsonPath = get (TestRunnerArgs.parseTimingsJsonArg args)
    and codegenProfileJsonPath =
      get (TestRunnerArgs.parseCodegenProfileJsonArg args)
    and e2eBatchSize = get (TestRunnerArgs.parseE2EBatchSizeArg args) in
    let target =
      match get (TestRunnerArgs.parseTargetArg args) with
      | TestRunnerArgs.Explicit target -> target
      | TestRunnerArgs.Host -> (
          match Platform.detectHostTarget () with
          | Ok target -> target
          | Error err -> Crash.crash ("Host target detection failed: " ^ err))
    in
    println (C.bold ^ C.cyan ^ "🧪 Running DSL-based Tests" ^ C.reset);
    Option.iter
      (fun pattern -> println (C.gray ^ "  Filter: " ^ pattern ^ C.reset))
      filter;
    println
      (C.gray ^ "  Parser/pretty corpus roundtrip: enabled (default)" ^ C.reset);
    println
      (C.gray
      ^ (if roundtripAllDark then
           "  Roundtrip upstream .dark coverage: all files (mode enabled)"
         else "  Roundtrip upstream .dark coverage: default subset")
      ^ C.reset);
    if showAllTestTimings then
      println (C.gray ^ "  Per-test timings: all tests (mode enabled)" ^ C.reset);
    println
      (C.gray ^ Printf.sprintf "  E2E batch size: %d" e2eBatchSize ^ C.reset);
    println
      (C.gray ^ "  Development target: "
      ^ (match target with
        | Platform.LinuxX86_64 -> "LinuxX86_64"
        | Platform.ARM64Backend Platform.LinuxARM64 -> "ARM64Backend LinuxARM64"
        | Platform.ARM64Backend Platform.MacOSARM64 -> "ARM64Backend MacOSARM64")
      ^ C.reset);
    Option.iter
      (fun path -> println (C.gray ^ "  Timing JSON output: " ^ path ^ C.reset))
      timingsJsonPath;
    Option.iter
      (fun path ->
        println (C.gray ^ "  Codegen profile JSON output: " ^ path ^ C.reset))
      codegenProfileJsonPath;
    println "";
    let getTestFiles dir suffix =
      RepositoryTestFiles.filesUnder ("test/fixtures/" ^ dir) ("." ^ suffix)
    in
    let allUpstreamDarkPaths = getTestFiles "e2e/upstream" "dark" in
    Array.sort StringOrder.compare allUpstreamDarkPaths;
    let filterUpstreamDarkPaths paths =
      match filter with
      | None -> paths
      | Some pattern ->
          let pattern = Text.lowerInvariant (Text.trim pattern) in
          arrayFilter
            (fun path -> Text.contains (Text.lowerInvariant path) pattern)
            paths
    in
    let includeUpstreamDarkPathsForE2E =
      match filter with
      | None -> allUpstreamDarkPaths
      | Some _ ->
          let paths = filterUpstreamDarkPaths allUpstreamDarkPaths in
          if Array.length paths = 0 then allUpstreamDarkPaths else paths
    in
    let includeUpstreamDarkPathsForRoundtrip =
      if roundtripAllDark then filterUpstreamDarkPaths allUpstreamDarkPaths
      else includeUpstreamDarkPathsForE2E
    in
    let e2eFiles = getTestFiles "e2e" "e2e" in
    let e2eTestFiles = Array.append e2eFiles includeUpstreamDarkPathsForE2E in
    let _roundtripCorpusTestFiles =
      Array.append e2eFiles includeUpstreamDarkPathsForRoundtrip
    in
    let verificationTestFiles = getTestFiles "verification" "e2e" in
    let optTestFiles =
      Array.concat
        [
          getTestFiles "optimization" "opt";
          getTestFiles "optimization" "liropt";
          getTestFiles "optimization" "arm64opt";
          getTestFiles "optimization" "lir2x64";
        ]
    in
    let typecheckTestFiles = getTestFiles "typecheck" "typecheck"
    and anf2mirTestFiles = getTestFiles "passes/anf2mir" "anf2mir"
    and mir2lirTestFiles = getTestFiles "passes/mir2lir" "mir2lir" in
    let isArm =
      match target with
      | Platform.ARM64Backend _ -> true
      | Platform.LinuxX86_64 -> false
    in
    let lir2arm64TestFiles =
      if isArm then getTestFiles "passes/lir2arm64" "lir2arm64" else [||]
    in
    let arm64encTestFiles =
      if isArm then getTestFiles "passes/arm64enc" "arm64enc" else [||]
    in
    let x64encTestFiles =
      if isArm then [||] else getTestFiles "passes/x64enc" "x64enc"
    in
    let graphColorTestFiles = getTestFiles "algorithms/graph-color" "graphcolor"
    and parallelMoveTestFiles =
      getTestFiles "algorithms/parallel-moves" "parallelmoves"
    and irFormatSnapshotTestFiles = getTestFiles "formatting/ir" "irformat"
    and memoryLayoutTestFiles = getTestFiles "runtime-layout" "memlayout"
    and lirExecutionTestFiles = getTestFiles "backend/x64" "lirexec"
    and rcReleaseTestFiles =
      getTestFiles "backend/reference-release" "rcrelease"
    and formattingRoundtripTestFiles =
      getTestFiles "formatting-roundtrip" "roundtrip"
    and syntaxTestFiles = getTestFiles "syntax" "syntax" in
    let unitStdlibSuites = [ "Stdlib Compile Tests"; "Preamble Build Tests" ] in
    let buildUnitTests stdlib =
      [|
        { F.name = "Parser Tests"; tests = ParserTests.tests };
        { F.name = "Platform Tests"; tests = PlatformTests.tests };
        { F.name = "Program CLI Tests"; tests = ProgramCliTests.tests };
        {
          F.name = "ValueSearch Catalog Tests";
          tests = ValueSearchCatalogTests.tests stdlib;
        };
        {
          F.name = "Program Structure Tests";
          tests = ProgramStructureTests.tests stdlib;
        };
        {
          F.name = "Stdlib Optimization Tests";
          tests = StdlibOptimizationTests.tests stdlib;
        };
        {
          F.name = "Compilation Session Tests";
          tests = CompilationSessionTests.tests target stdlib;
        };
        {
          F.name = "JSON Planning Tests";
          tests = JsonPlanningTests.tests stdlib;
        };
        { F.name = "List HIR Tests"; tests = ListHIRTests.tests };
        {
          F.name = "HIR Verification Tests";
          tests = HIRVerificationTests.tests;
        };
        {
          F.name = "HIR Construction Tests";
          tests = HIRConstructionTests.tests;
        };
        {
          F.name = "Owned Function Groups";
          tests = OwnedFunctionGroupTests.tests;
        };
        {
          F.name = "Region Ownership Contracts";
          tests = RegionContractTests.tests;
        };
        {
          F.name = "Ownership Uniqueness Inference";
          tests = OwnershipUniquenessInferenceTests.tests;
        };
        {
          F.name = "Recursive Ownership Inference";
          tests = RecursiveOwnershipInferenceTests.tests;
        };
        {
          F.name = "Owned Function Group Inference";
          tests = OwnedFunctionGroupInferenceTests.tests;
        };
        {
          F.name = "Ownership Variant Selection";
          tests = OwnershipVariantSelectionTests.tests;
        };
        {
          F.name = "Ownership Variant Materialization";
          tests = OwnershipVariantMaterializationTests.tests;
        };
        {
          F.name = "Whole Function Ownership";
          tests = WholeFunctionOwnershipTests.tests;
        };
        {
          F.name = "Owned HIR Verification";
          tests = OwnedHIRVerificationTests.tests;
        };
        {
          F.name = "Ownership Call Facts";
          tests = OwnershipCallFactsTests.tests;
        };
        {
          F.name = "Ownership Variant Scheduling";
          tests = OwnershipVariantSchedulingTests.tests;
        };
        {
          F.name = "Runtime Data Layout Tests";
          tests = RuntimeDataLayoutTests.tests;
        };
        { F.name = "IR Symbol Tests"; tests = IRSymbolTests.tests };
        { F.name = "IR Printer Tests"; tests = IRPrinterTests.tests };
        { F.name = "ANF to MIR Tests"; tests = ANFToMIRTests.tests };
        { F.name = "MIR Optimize Tests"; tests = MIROptimizeTests.tests };
        { F.name = "LIR Peephole Tests"; tests = LIRPeepholeTests.tests };
        { F.name = "LIR Layout Tests"; tests = LIRLayoutTests.tests };
        {
          F.name = "Dead Code Elimination Tests";
          tests = DeadCodeEliminationTests.tests;
        };
        { F.name = "ANF Optimize Tests"; tests = ANFOptimizeTests.tests };
        { F.name = "Script Helper Tests"; tests = ScriptHelperTests.tests };
        { F.name = "Process Capture Tests"; tests = TestProcessTests.tests };
        { F.name = "Stdlib Source Tests"; tests = StdlibSourceTests.tests };
        { F.name = "Test Runner Args Tests"; tests = TestRunnerArgsTests.tests };
        {
          F.name = "Type Checking Test Runner Tests";
          tests = TypeCheckingTestRunnerTests.tests;
        };
        { F.name = "Pass Test Runner Tests"; tests = PassTestRunnerTests.tests };
        {
          F.name = "Optimization Format Tests";
          tests = OptimizationFormatTests.tests;
        };
        {
          F.name = "Type Checking Format Tests";
          tests = TypeCheckingFormatTests.tests;
        };
        { F.name = "Progress Bar Tests"; tests = ProgressBarTests.tests };
        { F.name = "Encoding DSL Tests"; tests = EncodingDSLTests.tests };
        { F.name = "Graph Color DSL Tests"; tests = GraphColorDSLTests.tests };
        {
          F.name = "Parallel Move DSL Tests";
          tests = ParallelMoveDSLTests.tests;
        };
        {
          F.name = "IR Format Snapshot DSL Tests";
          tests = IRFormatSnapshotDSLTests.tests;
        };
        {
          F.name = "IR Format Snapshot Fixture Tests";
          tests = IRFormatSnapshotTestRunner.tests irFormatSnapshotTestFiles;
        };
        {
          F.name = "Memory Layout Fixture Tests";
          tests = MemoryLayoutTestRunner.tests stdlib memoryLayoutTestFiles;
        };
        {
          F.name = "LIR Execution DSL Tests";
          tests = LIRExecutionDSLTests.tests;
        };
        {
          F.name = "Reference Release DSL Tests";
          tests = RCReleaseDSLTests.tests target;
        };
        {
          F.name = "LIR Execution Fixture Tests";
          tests = LIRExecutionTestRunner.tests lirExecutionTestFiles;
        };
        {
          F.name = "Reference Release Fixture Tests";
          tests = RCReleaseTestRunner.tests target rcReleaseTestFiles;
        };
        { F.name = "ARM64 Encoding Tests"; tests = ARM64EncodingTests.tests };
        { F.name = "ARM64 Binary Tests"; tests = ARM64BinaryTests.tests };
        { F.name = "ARM64 CodeGen Tests"; tests = ARM64CodeGenTests.tests };
        {
          F.name = "x64 Encoding Fixture Tests";
          tests = X86_64EncodingTestRunner.tests x64encTestFiles;
        };
        {
          F.name = "Native Unicode and Mach-O Repairs";
          tests =
            NativeRegressionTests.textTests @ NativeRegressionTests.machoTests;
        };
        { F.name = "x64 Binary Tests"; tests = X86_64BinaryTests.tests };
        { F.name = "x64 Resolve Tests"; tests = X86_64ResolveTests.tests };
        { F.name = "x64 CodeGen Tests"; tests = X86_64CodeGenTests.tests };
        { F.name = "Type Checking Tests"; tests = TypeCheckingTests.tests };
        {
          F.name = "Parallel Move Fixture Tests";
          tests = ParallelMoveTestRunner.tests parallelMoveTestFiles;
        };
        { F.name = "Bitset Tests"; tests = BitsetTests.tests };
        {
          F.name = "SSA Construction Tests";
          tests = SSAConstructionTests.tests;
        };
        { F.name = "SSA Liveness Tests"; tests = SSALivenessTests.tests };
        { F.name = "Phi Resolution Tests"; tests = PhiResolutionTests.tests };
        {
          F.name = "Graph Coloring Fixture Tests";
          tests = GraphColorTestRunner.tests graphColorTestFiles;
        };
        {
          F.name = "Chordal Graph Integration Tests";
          tests = ChordalGraphTests.tests;
        };
        { F.name = "AST to ANF Tests"; tests = ASTToANFTests.tests };
        {
          F.name = "RefCount Insertion Tests";
          tests = RefCountInsertionTests.tests;
        };
        {
          F.name = "TailCall Detection Tests";
          tests = TailCallDetectionTests.tests;
        };
        { F.name = "SSA Inlining Tests"; tests = SSAInliningTests.tests };
        {
          F.name = "SSA Optimization Tests";
          tests = SSAOptimizationTests.tests;
        };
        {
          F.name = "Monomorphization Tests";
          tests = MonomorphizationTests.tests;
        };
        { F.name = "Lambda Lifting Tests"; tests = LambdaLiftingTests.tests };
        {
          F.name = "Formatting Roundtrip Tests";
          tests = FormattingRoundtripTests.tests formattingRoundtripTestFiles;
        };
        { F.name = "Syntax DSL Tests"; tests = SyntaxDSLTests.tests };
        {
          F.name = "Syntax Fixture Tests";
          tests = SyntaxTestRunner.tests syntaxTestFiles;
        };
        { F.name = "E2E Format Tests"; tests = E2EFormatTests.tests };
      |]
    in

    let symbols : F.outputSymbols =
      { F.pass = "✓"; fail = "✗"; sectionPrefix = "└─" }
    in
    let runState = F.createStateWithProgressReporter completedTestReporter in
    let codegenMetrics = ref []
    and codegenLirOpMetrics = ref []
    and cacheCounts = ref M.empty in
    let count name = Option.value ~default:0 (M.find_opt name !cacheCounts) in
    let addCount name n =
      cacheCounts := M.add name (count name + n) !cacheCounts
    in
    let stats =
      {
        logicalTests = 0;
        eligibleTests = 0;
        executions = 0;
        batchExecutions = 0;
        batchedTests = 0;
        largestBatch = 0;
      }
    in
    let recordTiming = F.recordTiming runState
    and recordResults = F.recordResults runState
    and recordPassTiming = F.recordPassTiming runState in
    let passTimingTotal () = runState.F.overheadPassTimingTotal in
    let mergeRunState (target : F.testRunState) (source : F.testRunState) =
      let previous = target.F.passed + target.F.failed in
      target.F.overheadPassTimingTotal <-
        Int64.add target.F.overheadPassTimingTotal
          source.F.overheadPassTimingTotal;
      target.F.passed <- target.F.passed + source.F.passed;
      target.F.failed <- target.F.failed + source.F.failed;
      Queue.iter
        (fun value -> Queue.add value target.F.failedTests)
        source.F.failedTests;
      Queue.iter
        (fun value -> Queue.add value target.F.timings)
        source.F.timings;
      Queue.iter
        (fun name ->
          match M.find_opt name source.F.passTimings with
          | None -> ()
          | Some elapsed ->
              if not (M.mem name target.F.passTimings) then
                Queue.add name target.F.passTimingOrder;
              target.F.passTimings <-
                M.add name
                  (Int64.add
                     (Option.value ~default:0L
                        (M.find_opt name target.F.passTimings))
                     elapsed)
                  target.F.passTimings)
        source.F.passTimingOrder;
      target.F.passTimingCounts <-
        M.fold
          (fun name n acc ->
            M.add name (n + Option.value ~default:0 (M.find_opt name acc)) acc)
          source.F.passTimingCounts target.F.passTimingCounts;
      Option.iter
        (fun report ->
          for
            completed = previous + 1
            to previous + source.F.passed + source.F.failed
          do
            report completed
          done)
        target.F.completedTestReporter
    in
    let recordNonPassTiming name elapsed =
      if elapsed > 0L then recordPassTiming { O.pass = name; elapsed }
    in
    let recordPhaseOverhead name elapsed before after =
      let delta = Int64.sub after before in
      if delta < 0L then
        Crash.crash
          (Printf.sprintf
             "recordPhaseOverhead: pass timing delta (%s) is negative for %s"
             (StructuralFormat.format
                (StructuralFormat.Scalar (F.formatTime delta)))
             name);
      recordNonPassTiming name (Int64.sub elapsed delta)
    in
    let runSuiteWithExecutionTiming name run =
      let before = passTimingTotal () and start = now () in
      run ();
      recordPhaseOverhead name (elapsed start) before (passTimingTotal ())
    in
    let before = passTimingTotal () and start = now () in
    let stdlib =
      match
        StdlibCompilation.buildStdlibWithTrace target (Some recordPassTiming)
      with
      | Ok value -> value
      | Error err -> Crash.crash ("Stdlib did not build with error: " ^ err)
    in
    let stdlibElapsed = elapsed start in
    recordPhaseOverhead "Stdlib Build Overhead" stdlibElapsed before
      (passTimingTotal ());
    let allUnitTests = Array.append (buildUnitTests stdlib) [||] in
    let unitSuiteSupportsTarget name =
      let arm64 =
        [
          "ARM64 Encoding Tests";
          "ARM64 Binary Tests";
          "ARM64 CodeGen Tests";
          "Parallel Move Fixture Tests";
        ]
      in
      let x64 =
        [
          "LIR Execution Fixture Tests";
          "x64 Encoding Fixture Tests";
          "x64 Binary Tests";
          "x64 Resolve Tests";
          "x64 CodeGen Tests";
        ]
      in
      not (List.mem name (if isArm then x64 else arm64))
    in
    (* Exclusions match the exhaustive 2026-10-08 compatibility test audit. *)
    let disabledUpstreamFiles =
      [
        "test/fixtures/e2e/upstream/cli/app-service-safety.dark";
        "test/fixtures/e2e/upstream/cli/command-completions.dark";
        "test/fixtures/e2e/upstream/cli/deprecation-kinds.dark";
        "test/fixtures/e2e/upstream/cli/include-parsing.dark";
        "test/fixtures/e2e/upstream/cli/outliner.dark";
        "test/fixtures/e2e/upstream/cli/permissions-display.dark";
        "test/fixtures/e2e/upstream/cli/permissions-grammar.dark";
        "test/fixtures/e2e/upstream/cli/tailscale.dark";
        "test/fixtures/e2e/upstream/cli/workbench-repl.dark";
        "test/fixtures/e2e/upstream/cloud/db.dark";
        "test/fixtures/e2e/upstream/language/builtin-introspection.dark";
        "test/fixtures/e2e/upstream/language/effect-ceiling.dark";
        "test/fixtures/e2e/upstream/language/error-type-names.dark";
        "test/fixtures/e2e/upstream/language/runtime-to-programtypes.dark";
        "test/fixtures/e2e/upstream/scm/commit-hash.dark";
        "test/fixtures/e2e/upstream/scm/constraint-kinds.dark";
        "test/fixtures/e2e/upstream/scm/lww.dark";
        "test/fixtures/e2e/upstream/scm/matter-routes.dark";
        "test/fixtures/e2e/upstream/scm/propagation-policy.dark";
        "test/fixtures/e2e/upstream/scm/removal-conflicts.dark";
        "test/fixtures/e2e/upstream/scm/sync-seen-everything.dark";
        "test/fixtures/e2e/upstream/scm/sync-wire.dark";
        "test/fixtures/e2e/upstream/stachu/darklangParser.dark";
        "test/fixtures/e2e/upstream/stachu/parser.dark";
        "test/fixtures/e2e/upstream/stachu/tinyLang.dark";
        "test/fixtures/e2e/upstream/stdlib/earg.dark";
        "test/fixtures/e2e/upstream/stdlib/eself.dark";
        "test/fixtures/e2e/upstream/stdlib/json.dark";
        "test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark";
        "test/fixtures/e2e/upstream/stdlib/language-tools/pickLocation.dark";
        "test/fixtures/e2e/upstream/stdlib/language-tools/semanticTokenization.dark";
        "test/fixtures/e2e/upstream/stdlib/prettyPrinter.dark";
        "test/fixtures/e2e/upstream/stdlib/sqlite.dark";
        "test/fixtures/e2e/upstream/stdlib/string.dark";
      ]
    in
    let disabledUpstreamLines =
      [
        ("test/fixtures/e2e/upstream/language/apply/eapply.dark", [ 118 ]);
        ("test/fixtures/e2e/upstream/language/apply/einfix.dark", [ 57; 58; 59 ]);
        ("test/fixtures/e2e/upstream/language/basic/eand.dark", [ 5; 13 ]);
        ( "test/fixtures/e2e/upstream/language/basic/elet.dark",
          [ 1; 2; 19; 20; 45; 54; 60; 66; 71; 82 ] );
        ("test/fixtures/e2e/upstream/language/basic/eor.dark", [ 18; 19 ]);
        ("test/fixtures/e2e/upstream/language/collections/dlist.dark", [ 9; 10 ]);
        ( "test/fixtures/e2e/upstream/language/collections/dtuple.dark",
          [ 9; 11 ] );
        ( "test/fixtures/e2e/upstream/language/custom-data/enums.dark",
          [ 7; 9; 11; 74; 106 ] );
        ( "test/fixtures/e2e/upstream/language/custom-data/record-field-acess.dark",
          [ 13 ] );
        ("test/fixtures/e2e/upstream/language/custom-data/records.dark", [ 8 ]);
        ( "test/fixtures/e2e/upstream/language/custom-data/values.dark",
          [
            5;
            9;
            13;
            17;
            21;
            25;
            29;
            33;
            37;
            41;
            45;
            49;
            53;
            57;
            61;
            65;
            76;
            77;
            78;
            80;
            81;
            83;
            84;
            86;
            87;
            89;
            90;
            92;
            93;
            95;
            96;
            98;
            99;
            101;
            102;
            104;
            105;
            107;
            108;
            110;
            111;
            113;
            114;
            116;
            117;
            119;
            120;
            122;
            123;
            125;
            126;
            128;
            129;
          ] );
        ( "test/fixtures/e2e/upstream/language/derror.dark",
          [ 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 25; 32 ] );
        ( "test/fixtures/e2e/upstream/language/elambda.dark",
          [ 13; 18; 21; 33; 35 ] );
        ("test/fixtures/e2e/upstream/language/error-syntax.dark", [ 1 ]);
        ( "test/fixtures/e2e/upstream/language/flow-control/eif.dark",
          [ 3; 4; 5; 6; 10; 23; 26 ] );
        ( "test/fixtures/e2e/upstream/language/flow-control/ematch.dark",
          [ 656; 660; 663; 667; 672; 677; 681 ] );
        ("test/fixtures/e2e/upstream/language/flow-control/epipe.dark", [ 11 ]);
        ("test/fixtures/e2e/upstream/language/nested-fns.dark", [ 55; 60 ]);
        ( "test/fixtures/e2e/upstream/scm/branch-identity.dark",
          [ 9; 20; 23; 27; 31; 32 ] );
        ( "test/fixtures/e2e/upstream/scm/conflicts.dark",
          [
            51;
            58;
            64;
            70;
            79;
            86;
            92;
            99;
            105;
            111;
            129;
            136;
            144;
            155;
            161;
            169;
            175;
            184;
            194;
            204;
            220;
            224;
            228;
            232;
            233;
            234;
            236;
            242;
            243;
            246;
            251;
            257;
            258;
            259;
            263;
            265;
            266;
          ] );
        ( "test/fixtures/e2e/upstream/stdlib/base64.dark",
          [
            7;
            9;
            10;
            11;
            12;
            20;
            21;
            22;
            23;
            24;
            25;
            26;
            27;
            28;
            29;
            33;
            34;
            35;
            36;
            39;
            40;
            43;
            44;
            45;
            46;
            47;
          ] );
        ("test/fixtures/e2e/upstream/stdlib/dict.dark", [ 76; 93 ]);
        ( "test/fixtures/e2e/upstream/stdlib/float.dark",
          [
            47;
            55;
            58;
            59;
            64;
            65;
            73;
            76;
            81;
            89;
            107;
            110;
            111;
            125;
            127;
            134;
            136;
            160;
            173;
            176;
            244;
            255;
          ] );
        ( "test/fixtures/e2e/upstream/stdlib/html.dark",
          [ 42; 44; 66; 69; 72; 75; 83 ] );
        ( "test/fixtures/e2e/upstream/stdlib/http.dark",
          [
            21;
            27;
            33;
            48;
            51;
            57;
            63;
            102;
            108;
            110;
            111;
            114;
            117;
            122;
            123;
            124;
            128;
            129;
            130;
            131;
            133;
            137;
            144;
            149;
            155;
            161;
          ] );
        ( "test/fixtures/e2e/upstream/stdlib/httpclient.dark",
          [ 71; 108; 111; 131; 132; 133; 134 ] );
        ("test/fixtures/e2e/upstream/stdlib/httpserver.dark", [ 29; 33; 37 ]);
        ("test/fixtures/e2e/upstream/stdlib/ints/int64.dark", [ 368 ]);
        ("test/fixtures/e2e/upstream/stdlib/ints/int8.dark", [ 47 ]);
        ( "test/fixtures/e2e/upstream/stdlib/list.dark",
          [ 61; 65; 71; 75; 81; 157; 184; 238; 346 ] );
        ("test/fixtures/e2e/upstream/stdlib/math.dark", [ 27; 30 ]);
        ("test/fixtures/e2e/upstream/stdlib/nomodule.dark", [ 308 ]);
      ]
    in
    let normalizePath path =
      String.map (fun c -> if c = '\\' then '/' else c) path
    in
    let pathMatchesSourceFile source suffix =
      Text.endsWith (normalizePath source) suffix
    in
    let isDisabledUpstreamFile source =
      List.exists (pathMatchesSourceFile source) disabledUpstreamFiles
    in
    let disabledLinesForSourceFile source =
      List.find_map
        (fun (suffix, lines) ->
          if pathMatchesSourceFile source suffix then Some lines else None)
        disabledUpstreamLines
    in
    let tryParseTestLineNumber name =
      if Text.startsWith name "L" then
        match String.index_opt name ':' with
        | Some colon when colon > 1 ->
            Option.map Int32.to_int
              (Text.tryParseInt32 (String.sub name 1 (colon - 1)))
        | _ -> None
      else None
    in
    let upstreamSkipReason (test : E.e2eTest) =
      if
        Text.contains test.E.source "Builtin.testRuntimeError"
        || Text.contains test.E.preamble "Builtin.testRuntimeError"
      then
        "unsupported interpreter test infrastructure: Builtin.testRuntimeError"
      else "pending upstream support"
    in
    let applyUpstreamEnablementGate (test : E.e2eTest) =
      if Option.is_some test.E.skipReason then test
      else if isDisabledUpstreamFile test.E.sourceFile then
        { test with E.skipReason = Some (upstreamSkipReason test) }
      else
        match
          ( disabledLinesForSourceFile test.E.sourceFile,
            tryParseTestLineNumber test.E.name )
        with
        | Some lines, Some line when List.mem line lines ->
            { test with E.skipReason = Some (upstreamSkipReason test) }
        | _ -> test
    in
    let loadE2ETests files =
      let tests, errors =
        Array.fold_left
          (fun (tests, errors) file ->
            match E.parseE2ETestFile file with
            | Ok parsed ->
                ( List.rev_append
                    (List.map applyUpstreamEnablementGate parsed)
                    tests,
                  errors )
            | Error msg -> (tests, (file, msg) :: errors))
          ([], []) files
      in
      (Array.of_list (List.rev tests), List.rev errors)
    in
    let matchesE2EFilter (test : E.e2eTest) =
      TestRunnerArgs.matchesFilter filter test.E.name
      || TestRunnerArgs.matchesFilter filter test.E.sourceFile
    in
    let reportParseErrors suiteName errors =
      List.iter
        (fun (path, msg) ->
          let name = Filename.basename path in
          println ("  " ^ C.red ^ "✗ ERROR parsing " ^ name ^ C.reset);
          println ("    " ^ msg);
          recordResults 0 1
            [
              {
                F.file = path;
                name = suiteName ^ ": " ^ name;
                message = msg;
                details = [];
              };
            ])
        errors
    in

    let runE2ESuite baseStdlib suiteName progressLabel testsArray =
      let numTests = Array.length testsArray in
      if numTests > 0 then begin
        let session =
          new CompilationSession.compilationSession
            ~collectCodegenMetrics:(Option.is_some codegenProfileJsonPath)
            ()
        in
        Fun.protect
          ~finally:(fun () -> session#dispose)
          (fun () ->
            let before = passTimingTotal () and start = now () in
            let contexts =
              R.buildSuiteContexts baseStdlib testsArray (Some recordPassTiming)
            in
            recordPhaseOverhead
              (suiteName ^ " Suite Context Overhead")
              (elapsed start) before (passTimingTotal ());
            let results = Array.make numTests None in
            let progress = ProgressBar.create progressLabel numTests in
            ProgressBar.update progress;
            let unpack = function
              | R.CompileFailed (code, error, compile) ->
                  (code, "", error, compile, 0L)
              | R.Ran (code, out, err, compile, runtime) ->
                  (code, out, err, compile, runtime)
            in
            let fromResult = function
              | Ok run -> run
              | Error failure -> failure.R.run
            in
            let preambleFailure message =
              Error { R.run = R.CompileFailed (1, message, 0L); message }
            in
            let collectExitCodeDetails (test : E.e2eTest) run =
              let code, _, _, _, _ = unpack run in
              if code <> test.E.expectedExitCode then
                [
                  Printf.sprintf "Expected exit code: %d, Actual: %d"
                    test.E.expectedExitCode code;
                ]
              else []
            in
            let printE2EFailure (test : E.e2eTest) (failure : R.e2eFailure) =
              let _, stdout, stderr, compile, runtime = unpack failure.R.run in
              let cleanName = replace test.E.name "Darklang.Stdlib." "" in
              println
                ("  "
                ^ truncateName 60 57 cleanName
                ^ "... " ^ C.red ^ "✗ FAIL" ^ C.reset ^ " " ^ C.gray
                ^ "(compile: " ^ F.formatTime compile ^ ", run: "
                ^ F.formatTime runtime ^ ")" ^ C.reset);
              println ("    " ^ failure.R.message);
              let details = collectExitCodeDetails test failure.R.run in
              List.iter (fun detail -> println ("    " ^ detail)) details;
              List.iter
                (fun (channel, expected, actual) ->
                  match expected with
                  | Some text when Text.trim actual <> Text.trim text ->
                      println
                        ("    Expected " ^ channel ^ ": "
                       ^ replace text "\n" "\\n");
                      println
                        ("    Actual " ^ channel ^ ": "
                       ^ replace actual "\n" "\\n")
                  | _ -> ())
                [
                  ("stdout", test.E.expectedStdout, stdout);
                  ("stderr", test.E.expectedStderr, stderr);
                ];
              details
            in
            let preparedTests = Array.map R.tryPrepareBatchTest testsArray in
            let rec collectBatch first next remaining acc =
              if next >= numTests || remaining = 0 then List.rev acc
              else
                match preparedTests.(next) with
                | Some prepared when R.canBatchTogether first prepared ->
                    collectBatch first (next + 1) (remaining - 1)
                      ((next, prepared) :: acc)
                | _ -> List.rev acc
            in
            let rec build index acc =
              if index >= numTests then List.rev acc
              else
                match preparedTests.(index) with
                | None ->
                    build (index + 1)
                      (Single (index, testsArray.(index), None) :: acc)
                | Some first when e2eBatchSize = 1 ->
                    build (index + 1)
                      (Single (index, first.R.test, Some first) :: acc)
                | Some first -> (
                    let batch =
                      collectBatch first (index + 1) (e2eBatchSize - 1)
                        [ (index, first) ]
                    in
                    match batch with
                    | [ _ ] ->
                        build (index + 1)
                          (Single (index, first.R.test, Some first) :: acc)
                    | _ -> build (index + List.length batch) (Batch batch :: acc)
                    )
            in
            let executionUnits = build 0 [] in
            stats.logicalTests <- stats.logicalTests + numTests;
            stats.eligibleTests <-
              stats.eligibleTests
              + Array.fold_left
                  (fun n x -> if Option.is_some x then n + 1 else n)
                  0 preparedTests;
            let recordPhysicalTiming run delta =
              let _, _, _, compile, runtime = unpack run in
              recordNonPassTiming "Compile Overhead" (Int64.sub compile delta);
              recordNonPassTiming F.testRuntimeTimingName runtime
            in
            let recordLogicalResult index (test : E.e2eTest) result =
              let _, _, _, compile, runtime = unpack (fromResult result) in
              results.(index) <- Some (test, result);
              recordTiming
                {
                  F.name = suiteName ^ ": " ^ test.E.name;
                  totalTime = Int64.add compile runtime;
                  compileTime = Some compile;
                  runtimeTime = Some runtime;
                };
              ProgressBar.increment progress (Result.is_ok result);
              match result with
              | Error failure when verbose ->
                  ProgressBar.finish progress;
                  ignore (printE2EFailure test failure);
                  ProgressBar.update progress
              | _ -> ()
            in
            let tryPreambleContext (test : E.e2eTest) =
              match contexts with
              | Error err -> Error ("Preamble build failed: " ^ err)
              | Ok current -> (
                  match
                    R.PreambleContextMap.find_opt
                      (R.preambleContextKeyForTest test)
                      current.R.preambleContexts
                  with
                  | Some context -> Ok context
                  | None ->
                      Error
                        ("Missing built preamble context for "
                       ^ test.E.sourceFile))
            in
            List.iter
              (function
                | Single (index, test, prepared) ->
                    let result, delta =
                      match tryPreambleContext test with
                      | Error message -> (preambleFailure message, 0L)
                      | Ok (stdlib, ctx) ->
                          stats.executions <- stats.executions + 1;
                          let before = passTimingTotal () in
                          let result =
                            match prepared with
                            | None ->
                                R.runE2ETestWithPreambleContext stdlib ctx
                                  (Some session) test (Some recordPassTiming)
                            | Some prepared ->
                                R.runPreparedE2ETestWithPreambleContext stdlib
                                  ctx (Some session) prepared
                                  (Some recordPassTiming)
                          in
                          (result, Int64.sub (passTimingTotal ()) before)
                    in
                    recordPhysicalTiming (fromResult result) delta;
                    recordLogicalResult index test result
                | Batch indexedBatch -> (
                    match indexedBatch with
                    | [] -> Crash.crash "Missing first prepared batch test"
                    | (_, first) :: _ -> (
                        let firstTest = first.R.test in
                        let batchCount = List.length indexedBatch in
                        match tryPreambleContext firstTest with
                        | Error message ->
                            let result = preambleFailure message in
                            recordPhysicalTiming (fromResult result) 0L;
                            List.iter
                              (fun (index, prepared) ->
                                recordLogicalResult index prepared.R.test result)
                              indexedBatch
                        | Ok (stdlib, ctx) ->
                            stats.executions <- stats.executions + 1;
                            stats.batchExecutions <- stats.batchExecutions + 1;
                            stats.batchedTests <-
                              stats.batchedTests + batchCount;
                            stats.largestBatch <-
                              max stats.largestBatch batchCount;
                            if verbose then (
                              ProgressBar.finish progress;
                              println
                                (Printf.sprintf "  batch %d: %s:%s" batchCount
                                   firstTest.E.sourceFile firstTest.E.name);
                              ProgressBar.update progress);
                            let start = now ()
                            and before = passTimingTotal () in
                            let execution =
                              R.runE2ETestBatchWithPreambleContext stdlib ctx
                                (Some session)
                                (List.map snd indexedBatch)
                                (Some recordPassTiming)
                            in
                            let after = passTimingTotal ()
                            and duration = elapsed start in
                            if verbose then (
                              ProgressBar.finish progress;
                              println
                                ("  batch completed in " ^ F.formatTime duration);
                              ProgressBar.update progress);
                            recordPhysicalTiming execution.R.aggregateRun
                              (Int64.sub after before);
                            List.iter2
                              (fun (index, _) (test, result) ->
                                recordLogicalResult index test result)
                              indexedBatch execution.R.results)))
              executionUnits;
            ProgressBar.finish progress;
            let passed, failed, failures =
              Array.fold_left
                (fun (passed, failed, failures) -> function
                  | None -> (passed, failed, failures)
                  | Some (_, Ok _) -> (passed + 1, failed, failures)
                  | Some (test, Error failure) ->
                      let details =
                        if verbose then
                          collectExitCodeDetails test failure.R.run
                        else printE2EFailure test failure
                      in
                      ( passed,
                        failed + 1,
                        {
                          F.file = test.E.sourceFile;
                          name = suiteName ^ ": " ^ test.E.name;
                          message = failure.R.message;
                          details;
                        }
                        :: failures ))
                (0, 0, []) results
            in
            recordResults passed failed (List.rev failures);
            println
              ("  " ^ C.green
              ^ Printf.sprintf "✓ %d passed" passed
              ^ C.reset
              ^
              if failed = 0 then ""
              else ", " ^ C.red ^ Printf.sprintf "✗ %d failed" failed ^ C.reset
              );
            if Option.is_some codegenProfileJsonPath then begin
              codegenMetrics := !codegenMetrics @ session#arm64CodegenMetrics;
              codegenLirOpMetrics :=
                !codegenLirOpMetrics @ session#arm64LirOpMetrics;
              List.iter
                (fun (name, n) -> addCount name n)
                [
                  ("cache_hits", session#arm64CodegenHitCount);
                  ("cache_misses", session#arm64CodegenMissCount);
                  ( "release_plan_summary_cache_hits",
                    session#arm64ReleasePlanSummaryHitCount );
                  ( "release_plan_summary_cache_misses",
                    session#arm64ReleasePlanSummaryMissCount );
                  ("json_plan_cache_hits", session#jsonPlanHitCount);
                  ("json_plan_cache_misses", session#jsonPlanMissCount);
                  ("anf_dependency_cache_hits", session#anfDependencyHitCount);
                  ("anf_dependency_cache_misses", session#anfDependencyMissCount);
                  ( "compiled_dependency_cache_hits",
                    session#compiledDependencyHitCount );
                  ( "compiled_dependency_cache_misses",
                    session#compiledDependencyMissCount );
                  ( "mir_optimization_cache_hits",
                    session#mirOptimizationHitCount );
                  ( "mir_optimization_cache_misses",
                    session#mirOptimizationMissCount );
                  ( "allocated_lir_function_cache_hits",
                    session#allocatedLirFunctionHitCount );
                  ( "allocated_lir_function_cache_misses",
                    session#allocatedLirFunctionMissCount );
                  ( "stdlib_reachability_cache_hits",
                    session#stdlibReachabilityHitCount );
                  ( "stdlib_reachability_cache_misses",
                    session#stdlibReachabilityMissCount );
                  ( "metadata_group_cache_hits",
                    session#arm64MetadataGroupHitCount );
                  ( "metadata_group_cache_misses",
                    session#arm64MetadataGroupMissCount );
                  ("helper_cache_hits", session#arm64HelperHitCount);
                  ("helper_cache_misses", session#arm64HelperMissCount);
                  ("start_codegen_cache_hits", session#arm64StartCodegenHitCount);
                ]
            end)
      end
    in

    let runPassTestFile load run path = Result.map run (load path) in
    let handlePassTestSuccess label progress path name duration
        (result : TestOutcome.t) : F.fileSuiteSummary =
      if result.TestOutcome.success then (
        ProgressBar.increment progress true;
        { F.passed = 1; failed = 0; failedTests = [] })
      else begin
        ProgressBar.increment progress false;
        ProgressBar.finish progress;
        println
          ("  " ^ name ^ "... " ^ C.red ^ "✗ FAIL" ^ C.reset ^ " " ^ C.gray
         ^ "(" ^ F.formatTime duration ^ ")" ^ C.reset);
        println ("    " ^ result.TestOutcome.message);
        let details =
          F.addExpectedActualDetails result.TestOutcome.expected
            result.TestOutcome.actual
        in
        let failure : F.failedTestInfo =
          {
            F.file = path;
            name = label ^ ": " ^ name;
            message = result.TestOutcome.message;
            details;
          }
        in
        ProgressBar.update progress;
        { F.passed = 0; failed = 1; failedTests = [ failure ] }
      end
    in
    let handlePassTestError label progress path name duration msg :
        F.fileSuiteSummary =
      ProgressBar.increment progress false;
      ProgressBar.finish progress;
      println
        ("  " ^ name ^ "... " ^ C.red ^ "✗ ERROR" ^ C.reset ^ " " ^ C.gray ^ "("
       ^ F.formatTime duration ^ ")" ^ C.reset);
      println ("    Failed to load test: " ^ msg);
      let failure : F.failedTestInfo =
        {
          F.file = path;
          name = label ^ ": " ^ name;
          message = "Failed to load test: " ^ msg;
          details = [];
        }
      in
      ProgressBar.update progress;
      { F.passed = 0; failed = 1; failedTests = [ failure ] }
    in
    let runPassSuite timing title progressLabel label files load run =
      let files =
        arrayFilter
          (fun path ->
            TestRunnerArgs.matchesFilter filter (Filename.basename path))
          files
      in
      runSuiteWithExecutionTiming timing (fun () ->
          F.runFileSuite runState symbols title progressLabel files
            Filename.basename
            (fun name -> label ^ ": " ^ name)
            (runPassTestFile load run)
            (handlePassTestSuccess label)
            (handlePassTestError label))
    in
    runPassSuite "ANF to MIR Test Suite Execution" "📦 ANF→MIR Tests" "ANF→MIR"
      "ANF→MIR" anf2mirTestFiles PassTestRunner.loadANF2MIRTest
      (fun (input, expected) -> PassTestRunner.runANF2MIRTest input expected);
    runPassSuite "MIR to LIR Test Suite Execution" "🔄 MIR→LIR Tests" "MIR→LIR"
      "MIR→LIR" mir2lirTestFiles PassTestRunner.loadMIR2LIRTest
      (fun (input, expected) -> PassTestRunner.runMIR2LIRTest input expected);
    runPassSuite "LIR to ARM64 Test Suite Execution" "🎯 LIR→ARM64 Tests"
      "LIR→ARM64" "LIR→ARM64" lir2arm64TestFiles
      PassTestRunner.loadLIR2ARM64Test (fun (input, expected) ->
        PassTestRunner.runLIR2ARM64Test input expected);
    runPassSuite "ARM64 Encoding Test Suite Execution" "⚙️  ARM64 Encoding Tests"
      "ARM64 Enc" "ARM64 Encoding" arm64encTestFiles
      ARM64EncodingTestRunner.loadARM64EncodingTest
      ARM64EncodingTestRunner.runARM64EncodingTest;
    let stem path = Filename.remove_extension (Filename.basename path) in
    let typecheckTests =
      arrayFilter
        (fun path -> TestRunnerArgs.matchesFilter filter (stem path))
        typecheckTestFiles
    in
    let handleTypecheckSuccess progress path fileName _
        (results : TypeCheckingTestRunner.typeCheckingTestResult list) :
        F.fileSuiteSummary =
      let passed =
        List.filter
          (fun result -> result.TypeCheckingTestRunner.success)
          results
        |> List.length
      in
      let failed = List.length results - passed in
      if failed = 0 then (
        ProgressBar.increment progress true;
        { F.passed; failed = 0; failedTests = [] })
      else
        let failures =
          List.filter_map
            (fun result ->
              if result.TypeCheckingTestRunner.success then None
              else begin
                ProgressBar.increment progress false;
                ProgressBar.finish progress;
                let desc =
                  match result.TypeCheckingTestRunner.expectedType with
                  | Some typ -> CheckingDiagnostics.typeToString typ
                  | None -> "error"
                in
                println
                  ("  " ^ desc ^ " (" ^ fileName ^ ")... " ^ C.red ^ "✗ FAIL"
                 ^ C.reset);
                println ("    " ^ result.TypeCheckingTestRunner.message);
                ProgressBar.update progress;
                Some
                  {
                    F.file = path;
                    name = "Type Checking: " ^ desc ^ " (" ^ fileName ^ ")";
                    message = result.TypeCheckingTestRunner.message;
                    details = [];
                  }
              end)
            results
        in
        { F.passed; failed; failedTests = failures }
    in
    let handleFileParseError label progress path _ _ msg : F.fileSuiteSummary =
      ProgressBar.increment progress false;
      ProgressBar.finish progress;
      println
        ("  " ^ C.red ^ "✗ ERROR parsing " ^ Filename.basename path ^ C.reset);
      println ("    " ^ msg);
      ProgressBar.update progress;
      {
        F.passed = 0;
        failed = 1;
        failedTests =
          [
            {
              F.file = path;
              name = label ^ ": " ^ Filename.basename path;
              message = msg;
              details = [];
            };
          ];
      }
    in
    runSuiteWithExecutionTiming "Type Checking Test Suite Execution" (fun () ->
        F.runFileSuite runState symbols "📋 Type Checking Tests" "TypeCheck"
          typecheckTests stem
          (fun name -> "TypeCheck: " ^ name)
          TypeCheckingTestRunner.runTypeCheckingTestFile handleTypecheckSuccess
          (handleFileParseError "Type Checking"));
    if Array.length optTestFiles > 0 then begin
      let runOptimizationFile path =
        let lower = Text.lowerInvariant (stem path) in
        let stage =
          match Filename.extension path with
          | ".liropt" -> OptimizationFormat.DirectLIR
          | ".arm64opt" -> OptimizationFormat.DirectARM64
          | ".lir2x64" -> OptimizationFormat.DirectLIR2X64
          | _ ->
              if Text.contains lower "anf" then OptimizationFormat.ANF
              else if Text.contains lower "mir" then OptimizationFormat.MIR
              else if Text.contains lower "lir" then OptimizationFormat.LIR
              else OptimizationFormat.ANF
        in
        OptimizationTestRunner.runTestFile stdlib (Some recordPassTiming)
          (fun test ->
            TestRunnerArgs.matchesFilter filter test.OptimizationFormat.name)
          stage path
      in
      let handleOptimizationSuccess progress path _ _ results :
          F.fileSuiteSummary =
        let filtered =
          List.filter
            (fun (test, _) ->
              TestRunnerArgs.matchesFilter filter test.OptimizationFormat.name)
            results
        in
        let passed =
          List.filter (fun (_, result) -> result.TestOutcome.success) filtered
          |> List.length
        in
        let failed = List.length filtered - passed in
        if failed = 0 then (
          ProgressBar.increment progress true;
          { F.passed; failed = 0; failedTests = [] })
        else begin
          ProgressBar.increment progress false;
          ProgressBar.finish progress;
          let failures =
            List.filter_map
              (fun (test, result) ->
                if result.TestOutcome.success then None
                else begin
                  println
                    ("  " ^ test.OptimizationFormat.name ^ "... " ^ C.red
                   ^ "✗ FAIL" ^ C.reset);
                  println ("    " ^ result.TestOutcome.message);
                  let details =
                    F.addExpectedActualDetails result.TestOutcome.expected
                      result.TestOutcome.actual
                  in
                  Some
                    {
                      F.file = path;
                      name = "Optimization: " ^ test.OptimizationFormat.name;
                      message = result.TestOutcome.message;
                      details;
                    }
                end)
              filtered
          in
          ProgressBar.update progress;
          { F.passed; failed; failedTests = failures }
        end
      in
      runSuiteWithExecutionTiming "Optimization Test Suite Execution" (fun () ->
          F.runFileSuite runState symbols "⚡ Optimization Tests" "Optimization"
            optTestFiles stem
            (fun name -> "Optimization: " ^ name)
            runOptimizationFile handleOptimizationSuccess
            (handleFileParseError "Optimization"))
    end;
    let unitTests =
      Array.to_list allUnitTests
      |> List.filter (fun (suite : F.unitTestSuite) ->
          unitSuiteSupportsTarget suite.F.name)
      |> List.filter_map (fun (suite : F.unitTestSuite) ->
          if TestRunnerArgs.matchesFilter filter suite.F.name then Some suite
          else
            let tests =
              List.filter
                (fun (name, _) -> TestRunnerArgs.matchesFilter filter name)
                suite.F.tests
            in
            if tests = [] then None else Some { suite with F.tests })
    in
    let withStdlib, withoutStdlib =
      List.partition
        (fun (suite : F.unitTestSuite) ->
          List.mem suite.F.name unitStdlibSuites)
        unitTests
    in
    let unitTestsOrdered = Array.of_list (withoutStdlib @ withStdlib) in
    let runE2EAndVerification baseStdlib =
      let runSection suite title parseTiming suiteTiming showStdlib files =
        if Array.length files > 0 then begin
          let start = now () in
          println (C.cyan ^ title ^ C.reset);
          let parseStart = now () in
          let allTests, parseErrors = loadE2ETests files in
          recordNonPassTiming parseTiming (elapsed parseStart);
          reportParseErrors suite parseErrors;
          if Array.length allTests > 0 then begin
            let filtered = arrayFilter matchesE2EFilter allTests in
            let skipped =
              Array.fold_left
                (fun n test ->
                  if Option.is_some test.E.skipReason then n + 1 else n)
                0 filtered
            in
            let tests =
              arrayFilter
                (fun test -> Option.is_none test.E.skipReason)
                filtered
            in
            if skipped > 0 then
              println
                (C.gray
                ^ Printf.sprintf
                    "  Skipping %d test(s) marked with skip=\"...\"" skipped
                ^ C.reset);
            if Array.length tests > 0 then begin
              if showStdlib then
                println
                  ("  " ^ C.gray ^ "(Stdlib compiled in "
                 ^ F.formatTime stdlibElapsed ^ ")" ^ C.reset);
              runE2ESuite baseStdlib suite suite tests
            end
          end;
          let duration = elapsed start in
          recordNonPassTiming suiteTiming duration;
          println
            ("  " ^ C.gray ^ "└─ Completed in " ^ F.formatTime duration
           ^ C.reset);
          println ""
        end
      in
      runSection "E2E" "🚀 E2E Tests" "E2E Test Parse" "E2E Suite Execution" true
        e2eTestFiles;
      if verificationEnabled then
        runSection "Verification" "🔬 Verification Tests"
          "Verification Test Parse" "Verification Suite Execution" false
          verificationTestFiles
    in
    println (C.gray ^ "  Unit and E2E suites: running in parallel" ^ C.reset);
    let unitResult = ref None in
    let unitThread =
      Thread.create
        (fun () ->
          unitResult :=
            Some
              (try
                 let state = F.createState () and start = now () in
                 F.runUnitTestSuites state symbols "🔧 Unit Tests" "Unit"
                   unitTestsOrdered;
                 F.recordPassTiming state
                   {
                     O.pass = "Unit Test Suite Execution";
                     elapsed = elapsed start;
                   };
                 Ok state
               with ex -> Error ex))
        ()
    in
    runE2EAndVerification stdlib;
    Thread.join unitThread;
    let unitState =
      match !unitResult with
      | Some (Ok state) -> state
      | Some (Error ex) -> raise ex
      | None -> Crash.crash "Missing completed unit-suite task result"
    in
    mergeRunState runState unitState;
    let coveragePercent =
      if (not showCoverage) || Array.length e2eTestFiles = 0 then None
      else
        let all =
          CompilerReachability.getAllStdlibFunctionNamesFromStdlib stdlib
        in
        let covered = ref StringOrder.Set.empty in
        Array.iter
          (fun path ->
            match E.parseE2ETestFile path with
            | Error _ -> ()
            | Ok tests ->
                List.iter
                  (fun (test : E.e2eTest) ->
                    if Option.is_none test.E.skipReason then
                      match
                        CompilerReachability
                        .getReachableStdlibFunctionsFromStdlib stdlib
                          test.E.source
                      with
                      | Error _ -> ()
                      | Ok reachable ->
                          StringOrder.Set.iter
                            (fun name ->
                              if StringOrder.Set.mem name all then
                                covered := StringOrder.Set.add name !covered)
                            reachable)
                  tests)
          e2eTestFiles;
        let total = StringOrder.Set.cardinal all in
        if total > 0 then
          Some
            (float_of_int (StringOrder.Set.cardinal !covered)
            /. float_of_int total *. 100.)
        else None
    in
    let totalTime = elapsed totalStart in
    let unaccountedBreakdown =
      F.calculateUnaccountedTimeBreakdown totalTime runState.F.passTimings
        (Queue.to_seq runState.F.timings)
    in
    let knownOrder = queueList runState.F.passTimingOrder in
    let knownNames = StringOrder.Set.of_list knownOrder in
    let extras =
      M.bindings runState.F.passTimings
      |> List.map fst
      |> List.filter (fun name -> not (StringOrder.Set.mem name knownNames))
    in
    let orderedPassTimingNames = knownOrder @ extras in

    let writeTimingsJson path =
      let tests =
        queueList runState.F.timings
        |> List.stable_sort (fun (a : F.testTiming) (b : F.testTiming) ->
            Int64.compare b.F.totalTime a.F.totalTime)
        |> List.map (fun (t : F.testTiming) ->
            ({
               name = t.F.name;
               total_ms = milliseconds t.F.totalTime;
               compile_ms = optionalMilliseconds t.F.compileTime;
               runtime_ms = optionalMilliseconds t.F.runtimeTime;
             }
              : timingJsonTest))
        |> Array.of_list
      in
      let passes =
        List.filter_map
          (fun name ->
            Option.map
              (fun duration ->
                ({
                   name;
                   elapsed_ms = milliseconds duration;
                   invocations =
                     Option.value ~default:0
                       (M.find_opt name runState.F.passTimingCounts);
                 }
                  : timingJsonPass))
              (M.find_opt name runState.F.passTimings))
          orderedPassTimingNames
        |> Array.of_list
      in
      let summary : timingJsonSummary =
        {
          passed = runState.F.passed;
          failed = runState.F.failed;
          total = runState.F.passed + runState.F.failed;
          total_ms = milliseconds totalTime;
          unaccounted_ms = milliseconds unaccountedBreakdown.F.unaccounted;
          runtime_unaccounted_ms = milliseconds unaccountedBreakdown.F.runtime;
          overhead_unaccounted_ms = milliseconds unaccountedBreakdown.F.overhead;
          e2e_batch_size = e2eBatchSize;
          e2e_logical_tests = stats.logicalTests;
          e2e_batch_eligible_tests = stats.eligibleTests;
          e2e_physical_executions = stats.executions;
          e2e_batch_executions = stats.batchExecutions;
          e2e_batched_logical_tests = stats.batchedTests;
          e2e_largest_batch = stats.largestBatch;
        }
      in
      writeJson false path (json_timingJsonPayload { summary; tests; passes })
    in
    Option.iter
      (fun path ->
        writeTimingsJson path;
        println ("  " ^ C.gray ^ "⏱  Wrote timing JSON: " ^ path ^ C.reset))
      timingsJsonPath;
    let writeCodegenProfileJson path =
      let categoryForFunction name =
        if
          String.starts_with ~prefix:"Darklang.Stdlib.Json." name
          || String.starts_with ~prefix:"Darklang.Stdlib.AltJson." name
        then "shared_json_runtime"
        else if String.starts_with ~prefix:"__dark_json_" name then
          "generated_json_codec"
        else if
          String.starts_with ~prefix:"__dark_eq_" name
          && (Text.contains name "Json" || Text.contains name "TypeReferenc")
        then "generated_json_equality"
        else "other"
      in
      let groupBy key values =
        let add groups value =
          let selected = key value in
          let rec update = function
            | [] -> [ (selected, [ value ]) ]
            | (k, xs) :: rest when k = selected -> (k, xs @ [ value ]) :: rest
            | first :: rest -> first :: update rest
          in
          update groups
        in
        List.fold_left add [] values
      in
      let sumInt select values =
        List.fold_left (fun total value -> total + select value) 0 values
      in
      let sumFloat select values =
        List.fold_left (fun total value -> total +. select value) 0. values
      in
      let functions =
        groupBy
          (fun (m : O.codegenFunctionMetric) -> m.O.functionName)
          !codegenMetrics
        |> List.map (fun (name, metrics) ->
            ({
               name;
               category = categoryForFunction name;
               generations = List.length metrics;
               elapsed_ms =
                 roundedMilliseconds
                   (sumFloat
                      (fun (m : O.codegenFunctionMetric) ->
                        Int64.to_float m.O.elapsed /. 1e6)
                      metrics);
               lir_instructions =
                 sumInt
                   (fun (m : O.codegenFunctionMetric) ->
                     m.O.lirInstructionCount)
                   metrics;
               symbolic_instructions =
                 sumInt
                   (fun (m : O.codegenFunctionMetric) ->
                     m.O.symbolicInstructionCount)
                   metrics;
             }
              : codegenProfileFunction))
        |> List.stable_sort
             (fun (a : codegenProfileFunction) (b : codegenProfileFunction) ->
               Float.compare b.elapsed_ms a.elapsed_ms)
        |> Array.of_list
      in
      let codegenMs =
        milliseconds
          (Option.value ~default:0L
             (M.find_opt "Code Generation" runState.F.passTimings))
      in
      let attributedMs =
        sumFloat
          (fun (entry : codegenProfileFunction) -> entry.elapsed_ms)
          (Array.to_list functions)
      in
      let percentage ms =
        if codegenMs <= 0. then 0.
        else roundedMilliseconds (ms *. 100. /. codegenMs)
      in
      let phases =
        [
          "ARM64 Codegen Metadata";
          "ARM64 Codegen Functions";
          "ARM64 Codegen Helpers";
          "ARM64 Codegen Assembly";
          "ARM64 Codegen Peephole";
        ]
        |> List.filter_map (fun name ->
            Option.map
              (fun duration ->
                let ms = milliseconds duration in
                ({
                   name;
                   elapsed_ms = ms;
                   percentage_of_codegen = percentage ms;
                 }
                  : codegenProfilePhase))
              (M.find_opt name runState.F.passTimings))
        |> Array.of_list
      in
      let categories =
        groupBy
          (fun (entry : codegenProfileFunction) -> entry.category)
          (Array.to_list functions)
        |> List.map (fun (name, entries) ->
            let ms =
              sumFloat
                (fun (entry : codegenProfileFunction) -> entry.elapsed_ms)
                entries
            in
            ({
               name;
               elapsed_ms = roundedMilliseconds ms;
               percentage_of_codegen = percentage ms;
               functions = List.length entries;
               generations =
                 sumInt
                   (fun (entry : codegenProfileFunction) -> entry.generations)
                   entries;
             }
              : codegenProfileCategory))
        |> List.stable_sort
             (fun (a : codegenProfileCategory) (b : codegenProfileCategory) ->
               Float.compare b.elapsed_ms a.elapsed_ms)
        |> Array.of_list
      in
      let lir_ops =
        groupBy
          (fun (m : O.codegenLirOpMetric) -> m.O.opcode)
          !codegenLirOpMetrics
        |> List.map (fun (name, metrics) ->
            let occurrences =
              sumInt (fun (m : O.codegenLirOpMetric) -> m.O.occurrences) metrics
            in
            let symbolic =
              sumInt
                (fun (m : O.codegenLirOpMetric) -> m.O.symbolicInstructionCount)
                metrics
            in
            ({
               name;
               occurrences;
               elapsed_ms =
                 roundedMilliseconds
                   (sumFloat
                      (fun (m : O.codegenLirOpMetric) ->
                        Int64.to_float m.O.elapsed /. 1e6)
                      metrics);
               symbolic_instructions_before_peephole = symbolic;
               average_symbolic_instructions_before_peephole =
                 (if occurrences = 0 then 0.
                  else
                    roundedMilliseconds
                      (float_of_int symbolic /. float_of_int occurrences));
             }
              : codegenProfileLirOp))
        |> List.stable_sort
             (fun (a : codegenProfileLirOp) (b : codegenProfileLirOp) ->
               Int.compare b.symbolic_instructions_before_peephole
                 a.symbolic_instructions_before_peephole)
        |> Array.of_list
      in
      let lir_op_functions =
        groupBy
          (fun (m : O.codegenLirOpMetric) ->
            (m.O.functionName, m.O.opcode, m.O.detail))
          !codegenLirOpMetrics
        |> List.map (fun ((function_name, opcode, detail), metrics) ->
            ({
               function_name;
               category = categoryForFunction function_name;
               opcode;
               detail;
               occurrences =
                 sumInt
                   (fun (m : O.codegenLirOpMetric) -> m.O.occurrences)
                   metrics;
               elapsed_ms =
                 roundedMilliseconds
                   (sumFloat
                      (fun (m : O.codegenLirOpMetric) ->
                        Int64.to_float m.O.elapsed /. 1e6)
                      metrics);
               symbolic_instructions_before_peephole =
                 sumInt
                   (fun (m : O.codegenLirOpMetric) ->
                     m.O.symbolicInstructionCount)
                   metrics;
             }
              : codegenProfileLirOpFunction))
        |> List.stable_sort
             (fun
               (a : codegenProfileLirOpFunction)
               (b : codegenProfileLirOpFunction)
             ->
               Int.compare b.symbolic_instructions_before_peephole
                 a.symbolic_instructions_before_peephole)
        |> Array.of_list
      in
      let summary : codegenProfileSummary =
        {
          codegen_ms = codegenMs;
          attributed_function_ms = roundedMilliseconds attributedMs;
          program_overhead_ms =
            roundedMilliseconds (max 0. (codegenMs -. attributedMs));
          cache_hits = count "cache_hits";
          cache_misses = count "cache_misses";
          release_plan_summary_cache_hits =
            count "release_plan_summary_cache_hits";
          release_plan_summary_cache_misses =
            count "release_plan_summary_cache_misses";
          json_plan_cache_hits = count "json_plan_cache_hits";
          json_plan_cache_misses = count "json_plan_cache_misses";
          anf_dependency_cache_hits = count "anf_dependency_cache_hits";
          anf_dependency_cache_misses = count "anf_dependency_cache_misses";
          compiled_dependency_cache_hits =
            count "compiled_dependency_cache_hits";
          compiled_dependency_cache_misses =
            count "compiled_dependency_cache_misses";
          mir_optimization_cache_hits = count "mir_optimization_cache_hits";
          mir_optimization_cache_misses = count "mir_optimization_cache_misses";
          allocated_lir_function_cache_hits =
            count "allocated_lir_function_cache_hits";
          allocated_lir_function_cache_misses =
            count "allocated_lir_function_cache_misses";
          stdlib_reachability_cache_hits =
            count "stdlib_reachability_cache_hits";
          stdlib_reachability_cache_misses =
            count "stdlib_reachability_cache_misses";
          metadata_group_cache_hits = count "metadata_group_cache_hits";
          metadata_group_cache_misses = count "metadata_group_cache_misses";
          helper_cache_hits = count "helper_cache_hits";
          helper_cache_misses = count "helper_cache_misses";
          start_codegen_cache_hits = count "start_codegen_cache_hits";
        }
      in
      writeJson true path
        (json_codegenProfilePayload
           {
             schema_version = 10;
             summary;
             phases;
             categories;
             functions;
             lir_ops;
             lir_op_functions;
           })
    in
    Option.iter
      (fun path ->
        writeCodegenProfileJson path;
        println
          ("  " ^ C.gray ^ "⏱  Wrote codegen profile JSON: " ^ path ^ C.reset))
      codegenProfileJsonPath;

    let divider color =
      println
        (C.bold ^ color ^ "═══════════════════════════════════════" ^ C.reset)
    in
    if Queue.length runState.F.timings > 0 then begin
      let title =
        if showAllTestTimings then "⏱ All Test Timings" else "🐢 Slowest Tests"
      in
      let timings =
        queueList runState.F.timings
        |> List.stable_sort (fun (a : F.testTiming) (b : F.testTiming) ->
            Int64.compare b.F.totalTime a.F.totalTime)
      in
      let timings =
        if showAllTestTimings then timings
        else List.filteri (fun i _ -> i < 5) timings
      in
      divider C.gray;
      println (C.bold ^ C.gray ^ title ^ C.reset);
      divider C.gray;
      List.iteri
        (fun i (timing : F.testTiming) ->
          let text =
            match (timing.F.compileTime, timing.F.runtimeTime) with
            | Some compile, Some runtime ->
                "compile: " ^ F.formatTime compile ^ "  run: "
                ^ F.formatTime runtime ^ "  total: "
                ^ F.formatTime timing.F.totalTime
            | _ -> "total: " ^ F.formatTime timing.F.totalTime
          in
          println
            ("  " ^ C.gray
            ^ string_of_int (i + 1)
            ^ ". "
            ^ padRight (truncateName 45 42 timing.F.name) 45
            ^ " " ^ text ^ C.reset))
        timings;
      println ""
    end;
    divider C.gray;
    println (C.bold ^ C.gray ^ "⏱ Suite Timings" ^ C.reset);
    divider C.gray;
    let columns =
      F.buildPassTimingColumns runState.F.passTimings
        (queueList runState.F.passTimingOrder)
        unaccountedBreakdown.F.unaccounted
    in
    let formatColumn (sections : F.passTimingSection list) =
      let entries =
        List.concat_map (fun section -> section.F.entries) sections
      in
      let formatSeconds duration =
        if Int64.to_float duration /. 1e6 < 100. then ">0.1s"
        else Printf.sprintf "%.1fs" (Int64.to_float duration /. 1e9)
      in
      let numberText (entry : F.passTimingEntry) =
        Option.value ~default:"" entry.F.number
      in
      let numberWidth =
        List.fold_left
          (fun width entry -> max width (textLength (numberText entry)))
          0 entries
      in
      let numberPadWidth = if numberWidth > 0 then numberWidth + 2 else 0 in
      let labelFor (entry : F.passTimingEntry) =
        let number = padRight (numberText entry) numberPadWidth in
        if numberPadWidth > 0 then number ^ entry.F.name else entry.F.name
      in
      let labelWidth =
        List.fold_left
          (fun width entry -> max width (textLength (labelFor entry)))
          0 entries
      in
      let timeWidth =
        List.fold_left
          (fun width entry ->
            max width (textLength (formatSeconds entry.F.elapsed)))
          0 entries
      in
      let formatEntry entry =
        let time = padLeft (formatSeconds entry.F.elapsed) timeWidth in
        let seconds = Int64.to_float entry.F.elapsed /. 1e9 in
        let colored =
          if seconds > 3. then C.red ^ time ^ C.gray
          else if seconds > 2. then C.yellow ^ time ^ C.gray
          else if seconds > 1. then C.white ^ time ^ C.gray
          else time
        in
        "  " ^ padRight (labelFor entry) labelWidth ^ "  " ^ colored
      in
      List.mapi
        (fun i section ->
          let lines =
            section.F.title
            ::
            (if section.F.entries = [] then [ "  (none)" ]
             else List.map formatEntry section.F.entries)
          in
          if i < List.length sections - 1 then lines @ [ "" ] else lines)
        sections
      |> List.concat
    in
    let leftLines = Array.of_list (formatColumn columns.F.ordered)
    and rightLines = Array.of_list (formatColumn columns.F.byTime) in
    let visibleLength text =
      List.fold_left
        (fun text code -> replace text code "")
        text
        [ C.red; C.yellow; C.white; C.gray; C.bold; C.reset ]
      |> textLength
    in
    let leftWidth =
      Array.fold_left
        (fun width text -> max width (visibleLength text))
        0 leftLines
    in
    for i = 0 to max (Array.length leftLines) (Array.length rightLines) - 1 do
      let left = if i < Array.length leftLines then leftLines.(i) else "" in
      let right = if i < Array.length rightLines then rightLines.(i) else "" in
      let padded =
        left ^ String.make (max 0 (leftWidth - visibleLength left)) ' '
      in
      println ("  " ^ C.gray ^ padded ^ "  " ^ right ^ C.reset)
    done;
    println "";
    divider C.cyan;
    println (C.bold ^ C.cyan ^ "📊 Test Results" ^ C.reset);
    divider C.cyan;
    if runState.F.failed = 0 then
      println
        ("  " ^ C.green
        ^ Printf.sprintf "✓ All tests passed: %d/%d" runState.F.passed
            (runState.F.passed + runState.F.failed)
        ^ C.reset)
    else begin
      println
        ("  " ^ C.green
        ^ Printf.sprintf "✓ Passed: %d" runState.F.passed
        ^ C.reset);
      println
        ("  " ^ C.red
        ^ Printf.sprintf "✗ Failed: %d" runState.F.failed
        ^ C.reset)
    end;
    Option.iter
      (fun pct ->
        println
          ("  " ^ C.gray
          ^ Printf.sprintf "📊 Stdlib coverage: %.1f%%" pct
          ^ C.reset))
      coveragePercent;
    println
      ("  " ^ C.gray ^ "⏱  Unaccounted time: "
      ^ F.formatTime unaccountedBreakdown.F.unaccounted
      ^ " (runtime: "
      ^ F.formatTime unaccountedBreakdown.F.runtime
      ^ ", overhead: "
      ^ F.formatTime unaccountedBreakdown.F.overhead
      ^ ")" ^ C.reset);
    println
      ("  " ^ C.gray ^ "⏱  Total time: " ^ F.formatTime totalTime ^ C.reset);
    divider C.cyan;
    if Queue.length runState.F.failedTests > 0 then begin
      let failures = Array.of_list (queueList runState.F.failedTests) in
      let displayed = min 10 (Array.length failures) in
      let more = Array.length failures - displayed in
      println "";
      divider C.red;
      println
        (C.bold ^ C.red
        ^ (if more > 0 then
             Printf.sprintf "❌ First %d Failing Tests (of %d total)" displayed
               (Array.length failures)
           else Printf.sprintf "❌ Failing Tests (%d)" (Array.length failures))
        ^ C.reset);
      divider C.red;
      println "";
      for i = 0 to displayed - 1 do
        let test = failures.(i) in
        let fileName =
          if test.F.file = "" then "" else Filename.basename test.F.file
        in
        let displayName =
          if fileName <> "" && Text.startsWith test.F.name "E2E: L" then
            "E2E: " ^ C.cyan ^ fileName ^ ":"
            ^ substring test.F.name 5 (textLength test.F.name - 5)
            ^ C.reset
          else if fileName <> "" then
            C.cyan ^ fileName ^ ": " ^ C.reset ^ C.red ^ test.F.name
          else test.F.name
        in
        println (C.red ^ string_of_int (i + 1) ^ ". " ^ displayName ^ C.reset);
        println ("   " ^ C.gray ^ test.F.message ^ C.reset);
        List.iter
          (fun detail -> println ("   " ^ C.gray ^ detail ^ C.reset))
          test.F.details;
        println ""
      done;
      if more > 0 then (
        println
          (C.gray
          ^ Printf.sprintf "... and %d more failing test(s)" more
          ^ C.reset);
        println "")
    end;
    {
      exitCode = (if runState.F.failed = 0 then 0 else 1);
      state = runState;
      totalTime;
      unaccountedBreakdown;
    }

let runTests args = runTestsWithProgressReporter None args

let captureOutput run =
  let result, _, _ = TestCapture.run run in
  result

let formatFailureDisplayName (test : F.failedTestInfo) =
  let file = if test.F.file = "" then "" else Filename.basename test.F.file in
  if file <> "" && Text.startsWith test.F.name "E2E: L" then
    "E2E: " ^ file ^ ":" ^ substring test.F.name 5 (textLength test.F.name - 5)
  else if file <> "" then file ^ ": " ^ test.F.name
  else test.F.name

let printFailures (state : F.testRunState) limit bounded =
  let failures = Array.of_list (queueList state.F.failedTests) in
  let display = min limit (Array.length failures) in
  for i = 0 to display - 1 do
    let test = failures.(i) in
    println (Printf.sprintf "%d. %s" (i + 1) (formatFailureDisplayName test));
    println
      ("   "
      ^
      if bounded then truncateDiagnostic aiMessageCharacterLimit test.F.message
      else test.F.message);
    let details =
      if bounded then
        truncateDiagnosticDetails aiDetailsCharacterLimit test.F.details
      else test.F.details
    in
    List.iter (fun detail -> println ("   " ^ detail)) details
  done;
  let more = Array.length failures - display in
  if more > 0 then
    println (Printf.sprintf "... and %d more failing test(s)" more)

let printStructuredFailures state limit = printFailures state limit false

let printBoundedStructuredFailures state =
  printFailures state aiFailureLimit true

let printQuietResult result =
  let _breakdown = result.unaccountedBreakdown in
  if result.exitCode = 0 then (
    println "success";
    0)
  else begin
    println "failed tests:";
    if Queue.length result.state.F.failedTests = 0 then
      println
        (Printf.sprintf "(test runner exited with code %d)" result.exitCode)
    else printStructuredFailures result.state 20;
    1
  end

let runAiMode args =
  let directory = Filename.concat (Sys.getcwd ()) "TestResults/ai" in
  createDirectory directory;
  let time = Unix.gettimeofday () in
  let parts = Unix.gmtime time in
  let timestamp =
    Printf.sprintf "%04d%02d%02dT%02d%02d%02d%03dZ"
      (parts.Unix.tm_year + 1900)
      (parts.Unix.tm_mon + 1) parts.Unix.tm_mday parts.Unix.tm_hour
      parts.Unix.tm_min parts.Unix.tm_sec
      (int_of_float ((time -. Float.floor time) *. 1000.))
  in
  let path = Filename.concat directory ("test-run-" ^ timestamp ^ ".log") in
  flush stdout;
  flush stderr;
  let savedOut = Unix.dup ~cloexec:true Unix.stdout
  and savedErr = Unix.dup ~cloexec:true Unix.stderr in
  let writeOriginal text =
    let bytes = Bytes.of_string (utf8Replacement text) in
    let rec send offset =
      if offset < Bytes.length bytes then
        let count =
          Unix.write savedOut bytes offset (Bytes.length bytes - offset)
        in
        if count = 0 then Crash.crash "AI progress stream closed"
        else send (offset + count)
    in
    send 0
  in
  let descriptor =
    Unix.openfile path
      [ Unix.O_WRONLY; Unix.O_CLOEXEC; Unix.O_CREAT; Unix.O_TRUNC ]
      0o666
  in
  let result =
    Fun.protect
      ~finally:(fun () ->
        flush stdout;
        flush stderr;
        Unix.dup2 savedOut Unix.stdout;
        Unix.dup2 savedErr Unix.stderr;
        Unix.close descriptor;
        Unix.close savedOut;
        Unix.close savedErr)
      (fun () ->
        Unix.dup2 descriptor Unix.stdout;
        Unix.dup2 descriptor Unix.stderr;
        writeOriginal "running tests (AI mode)\n";
        let report completed =
          if completed mod aiProgressTestInterval = 0 then writeOriginal "."
        in
        runTestsWithProgressReporter (Some report) args)
  in
  let total = result.state.F.passed + result.state.F.failed in
  if total >= aiProgressTestInterval then println "";
  let seconds =
    Printf.sprintf "%.1f" (Int64.to_float result.totalTime /. 1e9)
  in
  if result.exitCode = 0 then (
    Sys.remove path;
    println
      (Printf.sprintf "success: %d/%d passed in %ss" result.state.F.passed total
         seconds))
  else begin
    println "failed";
    println
      (Printf.sprintf "summary: %d passed, %d failed, %ss" result.state.F.passed
         result.state.F.failed seconds);
    printBoundedStructuredFailures result.state;
    println ("full output: " ^ RepositoryTestFiles.relativePath path)
  end;
  result.exitCode

let main args =
  if TestRunnerArgs.hasHelpArg args then (
    printHelp ();
    0)
  else if TestRunnerArgs.hasQuietArg args then
    printQuietResult (captureOutput (fun () -> runTests args))
  else if TestRunnerArgs.hasAiArg args then runAiMode args
  else (runTests args).exitCode
