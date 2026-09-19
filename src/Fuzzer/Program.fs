// Program.fs - Differential fuzzing of compiler-generated Dark programs against the Darklang interpreter.

module Fuzzer.Program

open System
open System.Diagnostics
open System.IO
open AST
open CompilationContexts
open CompilerOptions

type Action =
    | Fuzz
    | Replay of sourcePath:string
    | Minimize of sourcePath:string

type Config = {
    Seed: int
    MaxDepth: int
    TimeoutMs: int
    ArtifactDirectory: string
    Action: Action
}

type ProcessOutcome =
    | Completed of exitCode:int * stdout:string * stderr:string
    | TimedOut
    | StartFailed of message:string

type CaseOutcome =
    | Passed
    | InterpreterFailed of ProcessOutcome
    | CompilerRejected of message:string
    | NativeFailed of exitCode:int * stdout:string * stderr:string
    | ResultMismatch of interpreter:string * native:string

let defaultConfig : Config = {
    Seed = Environment.TickCount
    MaxDepth = 6
    TimeoutMs = 2000
    ArtifactDirectory = "fuzz-results"
    Action = Fuzz
}

let usage =
    "Usage: dotnet run --project src/Fuzzer/Fuzzer.fsproj -- "
    + "[--seed N] [--max-depth N] [--timeout-ms N] [--artifacts PATH] "
    + "[--replay FILE | --minimize FILE]"

let private parsePositiveInt (flag: string) (value: string) : Result<int, string> =
    match Int32.TryParse value with
    | true, parsed when parsed > 0 -> Ok parsed
    | _ -> Error $"{flag} requires a positive integer, got '{value}'"

let private parseInt (flag: string) (value: string) : Result<int, string> =
    match Int32.TryParse value with
    | true, parsed -> Ok parsed
    | _ -> Error $"{flag} requires an integer, got '{value}'"

let rec parseArgs (config: Config) (args: string list) : Result<Config option, string> =
    match args with
    | [] -> Ok (Some config)
    | ["--help"] | ["-h"] -> Ok None
    | "--seed" :: value :: rest ->
        parseInt "--seed" value
        |> Result.bind (fun parsed -> parseArgs { config with Seed = parsed } rest)
    | "--max-depth" :: value :: rest ->
        parsePositiveInt "--max-depth" value
        |> Result.bind (fun parsed -> parseArgs { config with MaxDepth = parsed } rest)
    | "--timeout-ms" :: value :: rest ->
        parsePositiveInt "--timeout-ms" value
        |> Result.bind (fun parsed -> parseArgs { config with TimeoutMs = parsed } rest)
    | "--artifacts" :: value :: rest when value <> "" ->
        parseArgs { config with ArtifactDirectory = value } rest
    | "--replay" :: value :: rest when value <> "" ->
        match config.Action with
        | Fuzz -> parseArgs { config with Action = Replay value } rest
        | Replay _ | Minimize _ -> Error "Specify only one of --replay and --minimize"
    | "--minimize" :: value :: rest when value <> "" ->
        match config.Action with
        | Fuzz -> parseArgs { config with Action = Minimize value } rest
        | Replay _ | Minimize _ -> Error "Specify only one of --replay and --minimize"
    | flag :: _ -> Error $"Unknown or incomplete argument '{flag}'"

let private supportedTypes = [TInt64; TBool; TString]
let private observableTypes = [TInt64; TBool]

let private sameType (left: Type) (right: Type) : bool = left = right

let private variablesOfType (typ: Type) (environment: (string * Type) list) : string list =
    environment
    |> List.choose (fun (name, variableType) ->
        if sameType typ variableType then Some name else None)

let private choose (random: Random) (items: 'a list) : 'a option =
    match items with
    | [] -> None
    | _ -> List.tryItem (random.Next(List.length items)) items

let private generateLiteral (random: Random) (typ: Type) : Expr =
    match typ with
    | TInt64 -> Int64Literal (random.NextInt64(-100L, 101L))
    | TBool -> BoolLiteral (random.Next(2) = 0)
    | TString ->
        [""; "a"; "dark"; "line\nbreak"; "quote\"slash\\"; "héllo"]
        |> choose random
        |> Option.defaultValue ""
        |> StringLiteral
    | _ -> Crash.crash $"Unsupported fuzzer literal type: {typ}"

let private generateLeaf
    (random: Random)
    (typ: Type)
    (environment: (string * Type) list)
    : Expr =
    match choose random (variablesOfType typ environment) with
    | Some name when random.Next(3) <> 0 -> Var name
    | _ -> generateLiteral random typ

let rec private generateExpr
    (random: Random)
    (depth: int)
    (nextVariable: int)
    (environment: (string * Type) list)
    (typ: Type)
    : Expr * int =
    if depth <= 0 then
        generateLeaf random typ environment, nextVariable
    else
        let generateChild childType currentVariable =
            generateExpr random (depth - 1) currentVariable environment childType

        let generateIf () =
            let condition, afterCondition = generateChild TBool nextVariable
            let thenBranch, afterThen = generateChild typ afterCondition
            let elseBranch, afterElse = generateChild typ afterThen
            If (condition, thenBranch, elseBranch), afterElse

        let generateLet () =
            let bindingType =
                choose random supportedTypes |> Option.defaultValue TInt64
            let binding, afterBinding = generateChild bindingType nextVariable
            let name = $"fuzz{afterBinding}"
            let body, afterBody =
                generateExpr
                    random
                    (depth - 1)
                    (afterBinding + 1)
                    ((name, bindingType) :: environment)
                    typ
            Let (LPVariable name, binding, body), afterBody

        let generateTypedOperation () =
            match typ with
            | TInt64 ->
                let left, afterLeft = generateChild TInt64 nextVariable
                let right, afterRight = generateChild TInt64 afterLeft
                let op =
                    [Add; Sub; Mul]
                    |> choose random
                    |> Option.defaultValue Add
                BinOp (op, left, right), afterRight
            | TBool ->
                if random.Next(2) = 0 then
                    let left, afterLeft = generateChild TBool nextVariable
                    let right, afterRight = generateChild TBool afterLeft
                    let op = if random.Next(2) = 0 then And else Or
                    BinOp (op, left, right), afterRight
                else
                    let operandType =
                        choose random supportedTypes |> Option.defaultValue TInt64
                    let left, afterLeft = generateChild operandType nextVariable
                    let right, afterRight = generateChild operandType afterLeft
                    let op =
                        match operandType with
                        | TInt64 ->
                            [Eq; Neq; Lt; Gt; Lte; Gte]
                            |> choose random
                            |> Option.defaultValue Eq
                        | TBool | TString -> if random.Next(2) = 0 then Eq else Neq
                        | _ -> Crash.crash $"Unsupported comparison operand type: {operandType}"
                    BinOp (op, left, right), afterRight
            | TString ->
                let left, afterLeft = generateChild TString nextVariable
                let right, afterRight = generateChild TString afterLeft
                BinOp (StringConcat, left, right), afterRight
            | _ -> Crash.crash $"Unsupported fuzzer expression type: {typ}"

        match random.Next(5) with
        | 0 -> generateLeaf random typ environment, nextVariable
        | 1 -> generateIf ()
        | 2 -> generateLet ()
        | _ -> generateTypedOperation ()

let generateProgram (random: Random) (maxDepth: int) : Program =
    let resultType =
        choose random observableTypes |> Option.defaultValue TInt64
    let expression, _ = generateExpr random maxDepth 0 [] resultType
    Program [Expression ([], expression)]

let private normalizeOutput (output: string) : string =
    output.TrimEnd('\r', '\n')

let private runProcess
    (fileName: string)
    (arguments: string list)
    (timeoutMs: int)
    : ProcessOutcome =
    try
        let startInfo = ProcessStartInfo(fileName)
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        arguments |> List.iter startInfo.ArgumentList.Add

        use child = new Process()
        child.StartInfo <- startInfo
        if not (child.Start()) then
            StartFailed $"Failed to start '{fileName}'"
        else
            let stdout = child.StandardOutput.ReadToEndAsync()
            let stderr = child.StandardError.ReadToEndAsync()
            if child.WaitForExit timeoutMs then
                Completed (child.ExitCode, stdout.Result, stderr.Result)
            else
                child.Kill(true)
                child.WaitForExit()
                TimedOut
    with ex ->
        StartFailed ex.Message

let private interpreterResult (config: Config) (source: string) : ProcessOutcome =
    runProcess "darklang-interpreter" ["eval"; source] config.TimeoutMs

let private runNative
    (config: Config)
    (target: Platform.Target)
    (binary: byte array)
    : ProcessOutcome =
    let path = Path.Combine(Path.GetTempPath(), $"dark-fuzzer-{Guid.NewGuid():N}")
    try
        try
            File.WriteAllBytes(path, binary)
            let permissions = File.GetUnixFileMode(path)
            File.SetUnixFileMode(path, permissions ||| UnixFileMode.UserExecute)

            let signing =
                if Platform.requiresCodeSigning (Platform.osFor target) then
                    runProcess "codesign" ["-s"; "-"; path] config.TimeoutMs
                else
                    Completed (0, "", "")

            match signing with
            | Completed (0, _, _) -> runProcess path [] config.TimeoutMs
            | Completed (exitCode, stdout, stderr) -> Completed (exitCode, stdout, stderr)
            | failure -> failure
        with ex ->
            StartFailed ex.Message
    finally
        try
            File.Delete(path)
        with _ ->
            ()

let private compilerRequest
    (stdlib: StdlibResult)
    (sourceName: string)
    (source: string)
    : CompileRequest =
    {
        Context = StdlibOnly stdlib
        Mode = TestExpression
        Sources =
            NonEmptyList.singleton {
                Name = sourceName
                Purpose = NameSyntax.SourceUnitPurpose.Executable
                Source = source
            }
        AllowInternal = false
        Verbosity = 0
        Options = { defaultOptions with EnableLeakCheck = true }
        PackageValues = emptyPackageValueCatalog
        PassTimingRecorder = None
        Session = None
    }

let private checkCase
    (config: Config)
    (stdlib: StdlibResult)
    (caseIndex: int64)
    (source: string)
    : CaseOutcome =
    match interpreterResult config source with
    | (TimedOut | StartFailed _) as failure -> InterpreterFailed failure
    | Completed (exitCode, stdout, stderr) when exitCode <> 0 ->
        InterpreterFailed (Completed (exitCode, stdout, stderr))
    | Completed (_, interpreterStdout, _) ->
        let report =
            compilerRequest stdlib $"fuzz-{config.Seed}-{caseIndex}.dark" source
            |> CompilerLibrary.compile
        match report.Result with
        | Error message -> CompilerRejected message
        | Ok binary ->
            match runNative config report.Target binary with
            | TimedOut -> NativeFailed (-1, "", "native execution timed out")
            | StartFailed message -> NativeFailed (-1, "", message)
            | Completed (exitCode, stdout, stderr) when exitCode <> 0 || stderr <> "" ->
                NativeFailed (exitCode, stdout, stderr)
            | Completed (_, nativeStdout, _) ->
                let interpreterOutput = normalizeOutput interpreterStdout
                let nativeOutput = normalizeOutput nativeStdout
                if interpreterOutput = nativeOutput then Passed
                else ResultMismatch (interpreterOutput, nativeOutput)

/// Conservatively recognize the exact typed subset emitted by this generator.
/// The interpreter still decides whether every minimized candidate is valid;
/// this only prevents a shrink from changing the top-level observation type.
let rec private inferGeneratedType
    (environment: (string * Type) list)
    (expr: Expr)
    : Type option =
    let variableType name =
        environment
        |> List.tryPick (fun (variableName, typ) ->
            if variableName = name then Some typ else None)

    let sameOperandTypes left right =
        match inferGeneratedType environment left, inferGeneratedType environment right with
        | Some leftType, Some rightType when leftType = rightType -> Some leftType
        | _ -> None

    match expr with
    | Int64Literal _ -> Some TInt64
    | BoolLiteral _ -> Some TBool
    | StringLiteral _ -> Some TString
    | Var name -> variableType name
    | BinOp (op, left, right) ->
        match op with
        | Add | Sub | Mul ->
            match sameOperandTypes left right with
            | Some TInt64 -> Some TInt64
            | _ -> None
        | StringConcat ->
            match sameOperandTypes left right with
            | Some TString -> Some TString
            | _ -> None
        | And | Or ->
            match sameOperandTypes left right with
            | Some TBool -> Some TBool
            | _ -> None
        | Eq | Neq | Lt | Gt | Lte | Gte ->
            sameOperandTypes left right |> Option.map (fun _ -> TBool)
        | Div | Mod | Pow | Shl | Shr | BitAnd | BitOr | BitXor -> None
    | Let (LPVariable name, binding, body) ->
        inferGeneratedType environment binding
        |> Option.bind (fun bindingType ->
            inferGeneratedType ((name, bindingType) :: environment) body)
    | If (condition, thenBranch, elseBranch) ->
        match
            inferGeneratedType environment condition,
            inferGeneratedType environment thenBranch,
            inferGeneratedType environment elseBranch
        with
        | Some TBool, Some thenType, Some elseType when thenType = elseType -> Some thenType
        | _ -> None
    | _ -> None

let rec private expressionSize (expr: Expr) : int =
    match expr with
    | BinOp (_, left, right) -> 1 + expressionSize left + expressionSize right
    | Let (_, binding, body) -> 1 + expressionSize binding + expressionSize body
    | If (condition, thenBranch, elseBranch) ->
        1 + expressionSize condition + expressionSize thenBranch + expressionSize elseBranch
    | _ -> 1

/// Enumerate deterministic local rewrites over the compiler AST. The oracle,
/// not this function, decides whether a rewrite preserves the reported defect.
let rec private oneStepSimplifications (expr: Expr) : Expr list =
    let literalSimplifications =
        match expr with
        | Int64Literal value when value <> 0L ->
            let towardSign = if value < 0L then -1L else 1L
            [Int64Literal 0L; Int64Literal towardSign]
        | BoolLiteral false -> [BoolLiteral true]
        | StringLiteral value when value <> "" -> [StringLiteral ""]
        | _ -> []

    let structuralSimplifications =
        match expr with
        | BinOp (op, left, right) ->
            [left; right]
            @ (oneStepSimplifications left |> List.map (fun candidate -> BinOp (op, candidate, right)))
            @ (oneStepSimplifications right |> List.map (fun candidate -> BinOp (op, left, candidate)))
        | Let (pattern, binding, body) ->
            [binding; body]
            @ (oneStepSimplifications binding |> List.map (fun candidate -> Let (pattern, candidate, body)))
            @ (oneStepSimplifications body |> List.map (fun candidate -> Let (pattern, binding, candidate)))
        | If (condition, thenBranch, elseBranch) ->
            [condition; thenBranch; elseBranch]
            @ (oneStepSimplifications condition
               |> List.map (fun candidate -> If (candidate, thenBranch, elseBranch)))
            @ (oneStepSimplifications thenBranch
               |> List.map (fun candidate -> If (condition, candidate, elseBranch)))
            @ (oneStepSimplifications elseBranch
               |> List.map (fun candidate -> If (condition, thenBranch, candidate)))
        | _ -> []

    literalSimplifications @ structuralSimplifications
    |> List.filter (fun candidate -> candidate <> expr)
    |> List.distinct

let private sameFailure (expected: CaseOutcome) (candidate: CaseOutcome) : bool =
    match expected, candidate with
    | CompilerRejected expectedMessage, CompilerRejected candidateMessage ->
        expectedMessage = candidateMessage
    | NativeFailed (expectedExit, _, expectedError), NativeFailed (candidateExit, _, candidateError) ->
        expectedExit = candidateExit && expectedError = candidateError
    | ResultMismatch _, ResultMismatch _ -> true
    | _ -> false

let private minimize
    (config: Config)
    (stdlib: StdlibResult)
    (source: string)
    : Result<string * CaseOutcome * int * int, string> =
    match Parser.parseString false source with
    | Error message -> Error $"Cannot parse minimizer input: {message}"
    | Ok (Program [Expression (_, originalExpr)]) ->
        match inferGeneratedType [] originalExpr with
        | None -> Error "Minimizer input is outside the generated expression subset"
        | Some originalType ->
            let originalOutcome = checkCase config stdlib -1L source
            match originalOutcome with
            | Passed -> Error "The input does not reproduce a discrepancy"
            | InterpreterFailed _ -> Error "The interpreter must accept a program before it can be minimized"
            | CompilerRejected _ | NativeFailed _ | ResultMismatch _ ->
                let rec tryCandidates attempts candidates =
                    match candidates with
                    | [] -> None, attempts
                    | (candidateExpr, candidateSource) :: rest ->
                        let outcome = checkCase config stdlib -1L candidateSource
                        let nextAttempts = attempts + 1
                        if sameFailure originalOutcome outcome then
                            Some (candidateExpr, candidateSource, outcome), nextAttempts
                        else
                            tryCandidates nextAttempts rest

                let rec reduce
                    (attempts: int)
                    (reductions: int)
                    (currentExpr: Expr)
                    (currentSource: string)
                    (currentOutcome: CaseOutcome)
                    =
                    let currentMetric = expressionSize currentExpr, currentSource.Length
                    let candidates =
                        oneStepSimplifications currentExpr
                        |> List.choose (fun candidateExpr ->
                            match inferGeneratedType [] candidateExpr with
                            | Some candidateType when candidateType = originalType ->
                                let candidateSource =
                                    Program [Expression ([], candidateExpr)]
                                    |> ASTPrettyPrinter.formatProgram
                                let candidateMetric = expressionSize candidateExpr, candidateSource.Length
                                if candidateMetric < currentMetric then
                                    Some (candidateExpr, candidateSource)
                                else
                                    None
                            | _ -> None)
                        |> List.distinctBy snd

                    match tryCandidates attempts candidates with
                    | Some (smallerExpr, smallerSource, smallerOutcome), nextAttempts ->
                        printfn $"reduced: {currentSource.Length} -> {smallerSource.Length} bytes"
                        reduce nextAttempts (reductions + 1) smallerExpr smallerSource smallerOutcome
                    | None, nextAttempts ->
                        Ok (currentSource, currentOutcome, nextAttempts, reductions)

                reduce 0 0 originalExpr source originalOutcome
    | Ok _ -> Error "Minimizer input must contain exactly one top-level expression"

let private describeProcessOutcome (outcome: ProcessOutcome) : string =
    match outcome with
    | TimedOut -> "interpreter timed out"
    | StartFailed message -> $"interpreter failed to start: {message}"
    | Completed (exitCode, stdout, stderr) ->
        $"interpreter exited {exitCode}\nstdout:\n{stdout}\nstderr:\n{stderr}"

let private describeCaseOutcome (outcome: CaseOutcome) : string =
    match outcome with
    | Passed -> "passed"
    | InterpreterFailed interpreter -> describeProcessOutcome interpreter
    | CompilerRejected message -> $"compiler rejected the generated program:\n{message}"
    | NativeFailed (exitCode, stdout, stderr) ->
        $"native program exited {exitCode}\nstdout:\n{stdout}\nstderr:\n{stderr}"
    | ResultMismatch (interpreter, native) ->
        $"result mismatch\ninterpreter:\n{interpreter}\nnative:\n{native}"

let private saveFinding
    (config: Config)
    (caseIndex: int64)
    (source: string)
    (outcome: CaseOutcome)
    : string =
    let prefix = Path.Combine(config.ArtifactDirectory, $"seed-{config.Seed}-case-{caseIndex}")
    let sourcePath = prefix + ".dark"
    File.WriteAllText(sourcePath, source + Environment.NewLine)
    File.WriteAllText(
        prefix + ".txt",
        $"seed: {config.Seed}\ncase: {caseIndex}\nmax-depth: {config.MaxDepth}\n"
        + $"{describeCaseOutcome outcome}\n")
    prefix

let private run (config: Config) : int =
    Directory.CreateDirectory(config.ArtifactDirectory) |> ignore
    let currentSourcePath = Path.Combine(config.ArtifactDirectory, "current.dark")

    match Platform.detectHostTarget () with
    | Error message ->
        eprintfn $"Target detection failed: {message}"
        1
    | Ok target ->
        match StdlibCompilation.buildStdlib target with
        | Error message ->
            eprintfn $"Standard library compilation failed: {message}"
            1
        | Ok stdlib ->
            match config.Action with
            | Replay path when not (File.Exists path) ->
                eprintfn $"Replay source does not exist: {path}"
                1
            | Replay path ->
                let source = File.ReadAllText path
                match checkCase config stdlib 0L source with
                | Passed ->
                    printfn "Replay passed; interpreter and compiler agree."
                    0
                | failure ->
                    eprintfn $"Replay reproduced: {describeCaseOutcome failure}"
                    1
            | Minimize path when not (File.Exists path) ->
                eprintfn $"Minimizer source does not exist: {path}"
                1
            | Minimize path ->
                let source = File.ReadAllText path
                match minimize config stdlib source with
                | Error message ->
                    eprintfn $"Minimization failed: {message}"
                    1
                | Ok (minimizedSource, outcome, attempts, reductions) ->
                    let outputPath = Path.ChangeExtension(path, ".min.dark")
                    File.WriteAllText(outputPath, minimizedSource + Environment.NewLine)
                    printfn $"Minimized in {attempts} oracle attempts and {reductions} reductions."
                    printfn $"Result: {outputPath}"
                    printfn $"Preserved failure: {describeCaseOutcome outcome}"
                    0
            | Fuzz ->
                printfn $"seed: {config.Seed}"
                let random = Random(config.Seed)
                let rec loop caseIndex =
                    let source =
                        generateProgram random config.MaxDepth
                        |> ASTPrettyPrinter.formatProgram
                    File.WriteAllText(currentSourcePath, source + Environment.NewLine)

                    match checkCase config stdlib caseIndex source with
                    | Passed ->
                        if (caseIndex + 1L) % 100L = 0L then
                            printfn $"checked {caseIndex + 1L}"
                        loop (caseIndex + 1L)
                    | failure ->
                        let artifactPrefix = saveFinding config caseIndex source failure
                        eprintfn $"Discrepancy found: {describeCaseOutcome failure}"
                        eprintfn $"Artifacts: {artifactPrefix}.dark and {artifactPrefix}.txt"
                        1
                loop 0L

[<EntryPoint>]
let main argv =
    match parseArgs defaultConfig (Array.toList argv) with
    | Error message ->
        eprintfn $"{message}\n{usage}"
        2
    | Ok None ->
        printfn "%s" usage
        0
    | Ok (Some config) -> run config
