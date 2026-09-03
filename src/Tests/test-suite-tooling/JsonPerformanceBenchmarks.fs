// JsonPerformanceBenchmarks.fs - focused typed JSON compile/runtime measurements.

module JsonPerformanceBenchmarks

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json

type private BenchmarkCase = {
    Name: string
    Payload: string
    Iterations: int64
    Source: int64 -> string
    Expected: int64 -> string
}

type BenchmarkResult = {
    name: string
    payload_bytes: int
    iterations: int64
    compile_ms: float
    binary_bytes: int
    runtime_samples_ms: float array
    median_runtime_ms: float
    nanoseconds_per_decode: float
    throughput_mib_per_second: float
    leak_check_passed: bool
    leak_check_stderr: string
}

type BenchmarkPayload = {
    schema_version: int
    compiler_commit: string
    target: string
    samples_per_case: int
    benchmarks: BenchmarkResult array
}

let private invariantRound (value: float) : float = Math.Round(value, 3)

let private median (values: float array) : float =
    let sorted = Array.sort values
    let middle = sorted.Length / 2
    if sorted.Length % 2 = 1 then sorted[middle]
    else (sorted[middle - 1] + sorted[middle]) / 2.0

let private gitCommit () : string =
    try
        let startInfo = ProcessStartInfo("git", "rev-parse HEAD")
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.UseShellExecute <- false
        use child = Process.Start(startInfo)
        let output = child.StandardOutput.ReadToEnd().Trim()
        child.WaitForExit()
        if child.ExitCode = 0 then output else "unknown"
    with _ ->
        "unknown"

let private darkStringLiteral (value: string) : string =
    JsonSerializer.Serialize(value)

let private scalarCase : BenchmarkCase =
    let payload = "123456789"
    {
        Name = "scalar"
        Payload = payload
        Iterations = 10000L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                $"        match Stdlib.Json.parse<Int64>({darkStringLiteral payload}) with"
                "        | Ok(value) -> benchmark(remaining - 1L, checksum + value)"
                "        | Error(_) -> -1L"
                $"benchmark({iterations}L, 0L)"
            ]
        Expected = fun iterations -> string (iterations * 123456789L)
    }

let private flatRecordCase : BenchmarkCase =
    let fields = [0 .. 3] |> List.map (fun index -> $"field{index:D2}")
    let fieldValue = String.replicate 240 "a"
    let payload =
        fields
        |> List.map (fun name -> $"\"{name}\":\"{fieldValue}\"")
        |> String.concat ","
        |> fun body -> "{" + body + "}"
    let typeFields = fields |> List.map (fun name -> $"{name}: String") |> String.concat ", "
    {
        Name = "flat_record_1k"
        Payload = payload
        Iterations = 20L
        Source = fun iterations ->
            String.concat "\n" [
                $"type JsonBenchFlatRecord = {{ {typeFields} }}"
                "let benchmark(remaining: Int64, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                $"        match Stdlib.Json.parse<JsonBenchFlatRecord>({darkStringLiteral payload}) with"
                "        | Ok(value) ->"
                "            benchmark(remaining - 1L, checksum + Stdlib.String.__byteLength(value.field00))"
                "        | Error(_) -> -1L"
                $"benchmark({iterations}L, 0L)"
            ]
        Expected = fun iterations -> string (iterations * int64 fieldValue.Length)
    }

let private collectionCase : BenchmarkCase =
    let values = [0 .. 255]
    let payload = values |> List.map string |> String.concat "," |> fun body -> "[" + body + "]"
    {
        Name = "collection_1k"
        Payload = payload
        Iterations = 100L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                $"        match Stdlib.Json.parse<List<Int64>>({darkStringLiteral payload}) with"
                "        | Ok(values) ->"
                "            match values with"
                "            | first :: _ -> benchmark(remaining - 1L, checksum + first + 1L)"
                "            | [] -> -2L"
                "        | Error(_) -> -1L"
                $"benchmark({iterations}L, 0L)"
            ]
        Expected = fun iterations -> string iterations
    }

let private nestedCase : BenchmarkCase =
    let leaf = "{\"JsonBenchLeaf\":[\"" + String.replicate 32 "n" + "\"]}"
    let rec grow (value: string) =
        if Encoding.UTF8.GetByteCount(value) >= 64 * 1024 then value
        else grow ("{\"JsonBenchBranch\":[" + value + "," + value + "]}")
    let payload = "{\"root\":" + grow leaf + "}"
    {
        Name = "nested_record_sum_64k"
        Payload = payload
        Iterations = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "type JsonBenchNested = JsonBenchLeaf of String | JsonBenchBranch of JsonBenchNested * JsonBenchNested"
                "type JsonBenchEnvelope = { root: JsonBenchNested }"
                "let benchmark(remaining: Int64, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                $"        match Stdlib.Json.parse<JsonBenchEnvelope>({darkStringLiteral payload}) with"
                "        | Ok(value) ->"
                "            match value.root with"
                "            | JsonBenchBranch(_, _) -> benchmark(remaining - 1L, checksum + 1L)"
                "            | JsonBenchLeaf(_) -> benchmark(remaining - 1L, checksum + 1L)"
                "        | Error(_) -> -1L"
                $"benchmark({iterations}L, 0L)"
            ]
        Expected = fun iterations -> string iterations
    }

let private cases = [| scalarCase; flatRecordCase; collectionCase; nestedCase |]

let private compile
    (stdlib: CompilerLibrary.StdlibResult)
    (session: CompilerLibrary.CompilationSession)
    (enableLeakCheck: bool)
    (name: string)
    (source: string)
    : Result<CompilerLibrary.CompileReport, string> =
    let request : CompilerLibrary.CompileRequest = {
        Context = CompilerLibrary.StdlibOnly stdlib
        Mode = CompilerLibrary.TestExpression
        Sources =
            AST.NonEmptyList.singleton {
                CompilerLibrary.SourceUnit.Name = $"JsonPerformanceBenchmarks/{name}.dark"
                Purpose = NameSyntax.SourceUnitPurpose.Executable
                Source = source
            }
        AllowInternal = true
        Verbosity = 0
        Options = { CompilerLibrary.defaultOptions with EnableLeakCheck = enableLeakCheck }
        PackageValues = CompilerLibrary.emptyPackageValueCatalog
        PassTimingRecorder = None
        Session = Some session
    }
    let report = CompilerLibrary.compile request
    match report.Result with
    | Ok _ -> Ok report
    | Error error -> Error error

let private executeAndValidate
    (target: Platform.Target)
    (expected: string)
    (binary: byte array)
    : Result<CompilerLibrary.ExecutionOutput, string> =
    let result = CompilerLibrary.executeCaptured target 0 CompilerLibrary.Closed binary
    if result.ExitCode <> 0 then
        Error $"execution exited {result.ExitCode}: {result.Stderr.Trim()}"
    elif result.Stdout.Trim() <> expected then
        Error $"expected output {expected}, got {result.Stdout.Trim()}"
    else
        Ok result

let private measureCase
    (stdlib: CompilerLibrary.StdlibResult)
    (session: CompilerLibrary.CompilationSession)
    (sampleCount: int)
    (benchmark: BenchmarkCase)
    : Result<BenchmarkResult, string> =
    compile stdlib session false benchmark.Name (benchmark.Source benchmark.Iterations)
    |> Result.bind (fun report ->
        let binary = report.Result |> Result.defaultValue [||]
        let expected = benchmark.Expected benchmark.Iterations
        executeAndValidate report.Target expected binary
        |> Result.bind (fun _ ->
            [|1 .. sampleCount|]
            |> Array.fold
                (fun state _ ->
                    state
                    |> Result.bind (fun samples ->
                        executeAndValidate report.Target expected binary
                        |> Result.map (fun execution -> execution.RuntimeTime.TotalMilliseconds :: samples)))
                (Ok [])
            |> Result.bind (fun reversedSamples ->
                use leakSession = new CompilerLibrary.CompilationSession()
                compile stdlib leakSession true benchmark.Name (benchmark.Source 1L)
                |> Result.bind (fun leakReport ->
                    let leakBinary = leakReport.Result |> Result.defaultValue [||]
                    executeAndValidate leakReport.Target (benchmark.Expected 1L) leakBinary
                    |> Result.map (fun leakExecution ->
                        let samples = reversedSamples |> List.rev |> List.toArray
                        let medianMs = median samples
                        let bytesProcessed = float (Encoding.UTF8.GetByteCount(benchmark.Payload)) * float benchmark.Iterations
                        {
                            name = benchmark.Name
                            payload_bytes = Encoding.UTF8.GetByteCount(benchmark.Payload)
                            iterations = benchmark.Iterations
                            compile_ms = invariantRound report.CompileTime.TotalMilliseconds
                            binary_bytes = binary.Length
                            runtime_samples_ms = samples |> Array.map invariantRound
                            median_runtime_ms = invariantRound medianMs
                            nanoseconds_per_decode = invariantRound (medianMs * 1000000.0 / float benchmark.Iterations)
                            throughput_mib_per_second = invariantRound (bytesProcessed / (medianMs / 1000.0) / 1048576.0)
                            leak_check_passed = not (leakExecution.Stderr.Contains("leaks:"))
                            leak_check_stderr = leakExecution.Stderr.Trim()
                        })))))

let run (outputPath: string) : int =
    let sampleCount = 7
    match Platform.detectHostTarget () with
    | Error error ->
        Console.Error.WriteLine($"JSON_BENCHMARK_ERROR {error}")
        1
    | Ok target ->
        match CompilerLibrary.buildStdlib target with
        | Error error ->
            Console.Error.WriteLine($"JSON_BENCHMARK_ERROR {error}")
            1
        | Ok stdlib ->
            use session = new CompilerLibrary.CompilationSession()
            let results =
                cases
                |> Array.fold
                    (fun state benchmark ->
                        state
                        |> Result.bind (fun measured ->
                            Console.Error.WriteLine($"JSON_BENCHMARK measuring {benchmark.Name}")
                            measureCase stdlib session sampleCount benchmark
                            |> Result.map (fun result -> result :: measured)))
                    (Ok [])
            match results with
            | Error error ->
                Console.Error.WriteLine($"JSON_BENCHMARK_ERROR {error}")
                1
            | Ok reversedResults ->
                let payload = {
                    schema_version = 1
                    compiler_commit = gitCommit ()
                    target = string target
                    samples_per_case = sampleCount
                    benchmarks = reversedResults |> List.rev |> List.toArray
                }
                let directory = Path.GetDirectoryName(outputPath)
                if not (String.IsNullOrWhiteSpace(directory)) then
                    Directory.CreateDirectory(directory) |> ignore
                let options = JsonSerializerOptions(WriteIndented = true)
                File.WriteAllText(outputPath, JsonSerializer.Serialize(payload, options))
                Console.Error.WriteLine($"JSON_BENCHMARK wrote {outputPath}")
                if payload.benchmarks |> Array.forall (fun result -> result.leak_check_passed) then 0 else 1
