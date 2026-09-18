// Integer128PerformanceBenchmarks.fs - focused 128-bit integer and UUID measurements.

module Integer128PerformanceBenchmarks

open System
open System.Diagnostics
open System.IO
open System.Numerics
open System.Text.Json

type private BenchmarkCase = {
    Name: string
    Iterations: int64
    OperationsPerIteration: int64
    Source: int64 -> string
    Expected: int64 -> string
}

type BenchmarkResult = {
    name: string
    iterations: int64
    operations_per_iteration: int64
    compile_ms: float
    binary_bytes: int
    runtime_samples_ms: float array
    median_runtime_ms: float
    nanoseconds_per_iteration: float
    nanoseconds_per_operation: float
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

let private modulus = BigInteger.One <<< 128
let private signedBoundary = BigInteger.One <<< 127

let private wrapUnsigned (value: BigInteger) : BigInteger =
    let remainder = value % modulus
    if remainder.Sign < 0 then remainder + modulus else remainder

let private wrapSigned (value: BigInteger) : BigInteger =
    let unsigned = wrapUnsigned value
    if unsigned >= signedBoundary then unsigned - modulus else unsigned

let private invariantRound (value: float) : float = Math.Round(value, 3)

let private median (values: float array) : float =
    let sorted = Array.sort values
    let middle = sorted.Length / 2
    if sorted.Length % 2 = 1 then sorted[middle]
    else (sorted[middle - 1] + sorted[middle]) / 2.0

let private gitCommit () : string =
    let startInfo =
        ProcessStartInfo(
            "git",
            "rev-parse HEAD",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false)
    use child = Process.Start(startInfo)
    let output = child.StandardOutput.ReadToEnd().Trim()
    child.WaitForExit()
    if child.ExitCode = 0 then output else "unknown"

let private int128Arithmetic : BenchmarkCase =
    let start = BigInteger.Parse("170141183460469231731687303715884100727")
    {
        Name = "int128_arithmetic"
        Iterations = 10000L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, value: Int128) : Int128 ="
                "    if remaining <= 0L then value"
                "    else benchmark(remaining - 1L, Stdlib.Int128.subtract(Stdlib.Int128.add(value, 3Q), 2Q))"
                $"benchmark({iterations}L, 170141183460469231731687303715884100727Q)"
            ]
        Expected = fun iterations -> string (wrapSigned (start + BigInteger iterations))
    }

let private uint128Arithmetic : BenchmarkCase =
    let start = BigInteger.Parse("340282366920938463463374607431768206455")
    {
        Name = "uint128_arithmetic"
        Iterations = 10000L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, value: UInt128) : UInt128 ="
                "    if remaining <= 0L then value"
                "    else benchmark(remaining - 1L, Stdlib.UInt128.subtract(Stdlib.UInt128.add(value, 3Z), 2Z))"
                $"benchmark({iterations}L, 340282366920938463463374607431768206455Z)"
            ]
        Expected = fun iterations -> string (wrapUnsigned (start + BigInteger iterations))
    }

let private int128ComparisonAndBitwise : BenchmarkCase =
    {
        Name = "int128_comparison_bitwise"
        Iterations = 10000L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, value: Int128, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                "        let next = Stdlib.Int128.bitwiseXor(value, 1Q) in"
                "        benchmark(remaining - 1L, next, checksum + (if Stdlib.Int128.greaterThan(next, value) then 1L else 2L))"
                $"benchmark({iterations}L, 0Q, 0L)"
            ]
        Expected = fun iterations -> string ((iterations / 2L) * 3L + (iterations % 2L))
    }

let private uint128ComparisonAndBitwise : BenchmarkCase =
    {
        Name = "uint128_comparison_bitwise"
        Iterations = 100L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, value: UInt128, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                "        let next = Stdlib.UInt128.bitwiseXor(value, 340282366920938463463374607431768211455Z) in"
                "        benchmark(remaining - 1L, next, checksum + (if Stdlib.UInt128.greaterThan(next, value) then 1L else 2L))"
                $"benchmark({iterations}L, 0Z, 0L)"
            ]
        Expected = fun iterations -> string ((iterations / 2L) * 3L + (iterations % 2L))
    }

let private int128DecimalConversion : BenchmarkCase =
    let text = "-170141183460469231731687303715884105728"
    {
        Name = "int128_decimal_conversion"
        Iterations = 2000L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                $"        match Stdlib.Int128.parse(\"{text}\") with"
                "        | Ok(value) -> benchmark(remaining - 1L, checksum + Stdlib.String.__byteLength(Stdlib.Int128.toString(value)))"
                "        | Error(_) -> -1L"
                $"benchmark({iterations}L, 0L)"
            ]
        Expected = fun iterations -> string (iterations * int64 text.Length)
    }

let private uint128DecimalConversion : BenchmarkCase =
    let text = "340282366920938463463374607431768211455"
    {
        Name = "uint128_decimal_conversion"
        Iterations = 2000L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                $"        match Stdlib.UInt128.parse(\"{text}\") with"
                "        | Ok(value) -> benchmark(remaining - 1L, checksum + Stdlib.String.__byteLength(Stdlib.UInt128.toString(value)))"
                "        | Error(_) -> -1L"
                $"benchmark({iterations}L, 0L)"
            ]
        Expected = fun iterations -> string (iterations * int64 text.Length)
    }

let private uuidParseAndFormat : BenchmarkCase =
    {
        Name = "uuid_parse_format"
        Iterations = 100L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                "        match Stdlib.Uuid.parse(\"3700adbc-7a46-4ff4-81d3-45afb03f6e2d\") with"
                "        | Ok(value) -> benchmark(remaining - 1L, checksum + Stdlib.String.__byteLength(Stdlib.Uuid.toString(value)))"
                "        | Error(_) -> -1L"
                $"benchmark({iterations}L, 0L)"
            ]
        Expected = fun iterations -> string (iterations * 36L)
    }

let private uuidEquality : BenchmarkCase =
    {
        Name = "uuid_equality"
        Iterations = 20000L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, left: Uuid, equal: Uuid, different: Uuid, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else benchmark(remaining - 1L, left, equal, different, checksum + (if left == equal && left != different then 1L else 1000L))"
                "let left = Builtin.unwrap(Stdlib.Uuid.parse(\"3700adbc-7a46-4ff4-81d3-45afb03f6e2d\")) in"
                "let equal = Builtin.unwrap(Stdlib.Uuid.parse(\"3700ADBC-7A46-4FF4-81D3-45AFB03F6E2D\")) in"
                "let different = Builtin.unwrap(Stdlib.Uuid.parse(\"3700adbc-7a46-4ff4-81d3-45afb03f6e2e\")) in"
                $"benchmark({iterations}L, left, equal, different, 0L)"
            ]
        Expected = string
    }

let private uuidGeneration : BenchmarkCase =
    {
        Name = "uuid_generation"
        Iterations = 256L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let benchmark(remaining: Int64, checksum: Int64) : Int64 ="
                "    if remaining <= 0L then checksum"
                "    else"
                "        let text = Stdlib.Uuid.toString(Stdlib.Uuid.generate()) in"
                "        let valid = Stdlib.String.__byteAtUnchecked(text, 14L) == 52L && Stdlib.String.__byteLength(text) == 36L in"
                "        benchmark(remaining - 1L, checksum + (if valid then 1L else 1000L))"
                $"benchmark({iterations}L, 0L)"
            ]
        Expected = string
    }

let private uint128Collection : BenchmarkCase =
    {
        Name = "uint128_collection_copy"
        Iterations = 2000L
        OperationsPerIteration = 2L
        Source = fun iterations ->
            String.concat "\n" [
                "let build(remaining: Int64, value: UInt128, values: List<UInt128>) : List<UInt128> ="
                "    if remaining <= 0L then values"
                "    else build(remaining - 1L, Stdlib.UInt128.add(value, 1Z), Stdlib.List.push<UInt128>(values, value))"
                "let consume(values: List<UInt128>, checksum: Int64) : Int64 ="
                "    match values with"
                "    | [] -> checksum"
                "    | value :: tail -> consume(tail, checksum + (if value == 0Z then 1L else 2L))"
                $"let values = build({iterations}L, 0Z, []) in consume(values, 0L)"
            ]
        Expected = fun iterations -> string (if iterations = 0L then 0L else iterations * 2L - 1L)
    }

let private cases =
    [| int128Arithmetic
       uint128Arithmetic
       int128ComparisonAndBitwise
       uint128ComparisonAndBitwise
       int128DecimalConversion
       uint128DecimalConversion
       uuidParseAndFormat
       uuidEquality
       uuidGeneration
       uint128Collection |]

let private compile
    (stdlib: CompilerLibrary.StdlibResult)
    (session: CompilerLibrary.CompilationSession)
    (enableLeakCheck: bool)
    (name: string)
    (source: string)
    : Result<CompilerLibrary.CompileReport * byte array, string> =
    let request : CompilerLibrary.CompileRequest = {
        Context = CompilerLibrary.StdlibOnly stdlib
        Mode = CompilerLibrary.TestExpression
        Sources =
            AST.NonEmptyList.singleton {
                CompilerLibrary.SourceUnit.Name = $"Integer128PerformanceBenchmarks/{name}.dark"
                Purpose = NameSyntax.SourceUnitPurpose.Executable
                Source = source
            }
        AllowInternal = true
        Verbosity = 0
        Options = { CompilerLibrary.defaultOptions with EnableLeakCheck = enableLeakCheck }
        PackageValues = CompilerLibrary.emptyPackageValueCatalog
        PackageManager = None
        PassTimingRecorder = None
        Session = Some session
    }
    let report = CompilerLibrary.compile request
    match report.Result with
    | Ok binary -> Ok (report, binary)
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
    |> Result.bind (fun (report, binary) ->
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
                |> Result.bind (fun (leakReport, leakBinary) ->
                    executeAndValidate leakReport.Target (benchmark.Expected 1L) leakBinary
                    |> Result.map (fun leakExecution ->
                        let samples = reversedSamples |> List.rev |> List.toArray
                        let medianMs = median samples
                        let operationCount = float benchmark.Iterations * float benchmark.OperationsPerIteration
                        {
                            name = benchmark.Name
                            iterations = benchmark.Iterations
                            operations_per_iteration = benchmark.OperationsPerIteration
                            compile_ms = invariantRound report.CompileTime.TotalMilliseconds
                            binary_bytes = binary.Length
                            runtime_samples_ms = samples |> Array.map invariantRound
                            median_runtime_ms = invariantRound medianMs
                            nanoseconds_per_iteration = invariantRound (medianMs * 1000000.0 / float benchmark.Iterations)
                            nanoseconds_per_operation = invariantRound (medianMs * 1000000.0 / operationCount)
                            leak_check_passed = not (leakExecution.Stderr.Contains("leaks:"))
                            leak_check_stderr = leakExecution.Stderr.Trim()
                        })))))

let run (outputPath: string) : int =
    let sampleCount = 7
    match Platform.detectHostTarget () with
    | Error error ->
        Console.Error.WriteLine($"INTEGER128_BENCHMARK_ERROR {error}")
        1
    | Ok target ->
        match CompilerLibrary.buildStdlib target with
        | Error error ->
            Console.Error.WriteLine($"INTEGER128_BENCHMARK_ERROR {error}")
            1
        | Ok stdlib ->
            use session = new CompilerLibrary.CompilationSession()
            let results =
                cases
                |> Array.fold
                    (fun state benchmark ->
                        state
                        |> Result.bind (fun measured ->
                            Console.Error.WriteLine($"INTEGER128_BENCHMARK measuring {benchmark.Name}")
                            measureCase stdlib session sampleCount benchmark
                            |> Result.map (fun result -> result :: measured)))
                    (Ok [])
            match results with
            | Error error ->
                Console.Error.WriteLine($"INTEGER128_BENCHMARK_ERROR {error}")
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
                Console.Error.WriteLine($"INTEGER128_BENCHMARK wrote {outputPath}")
                if payload.benchmarks |> Array.forall (fun result -> result.leak_check_passed) then 0 else 1
