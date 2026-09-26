// Program.fs - Differential fuzzing of compiler-generated Dark programs against the Darklang interpreter.

module Fuzzer.Program

open System
open System.Diagnostics
open System.IO
open System.Numerics
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

// Values with source literals. Opaque and bottom types have no general literal.
let private scalarTypes =
    [ TInt8; TInt16; TInt32; TInt64; TInt128; TInt
      TUInt8; TUInt16; TUInt32; TUInt64; TUInt128
      TBool; TFloat64; TString; TChar; TUnit ]

let private supportedTypes =
    scalarTypes
    @ (scalarTypes |> List.map TList)
    @ (scalarTypes |> List.map (fun typ -> TDict (TString, typ)))
    @ [TTuple [TInt64; TBool]; TRecord ("FuzzBox", []);
       TBlob; TDateTime; TStream TInt64]
let private observableTypes = [TInt64; TBool]

let private sameType (left: SemanticType) (right: SemanticType) : bool = left = right

let private variablesOfType (typ: SemanticType) (environment: (string * SemanticType) list) : string list =
    environment
    |> List.choose (fun (name, variableType) ->
        if sameType typ variableType then Some name else None)

let private choose (random: Random) (items: 'a list) : 'a option =
    match items with
    | [] -> None
    | _ -> List.tryItem (random.Next(List.length items)) items

let rec private generateLiteral (random: Random) (typ: SemanticType) : Expr =
    match typ with
    | TInt8 -> Int8Literal (sbyte (random.Next(-100, 101)))
    | TInt16 -> Int16Literal (int16 (random.Next(-100, 101)))
    | TInt32 -> Int32Literal (random.Next(-100, 101))
    | TInt64 -> Int64Literal (random.NextInt64(-100L, 101L))
    | TInt128 -> Int128Literal (Int128.op_Implicit (random.NextInt64(-100L, 101L)))
    | TInt -> BigIntLiteral (bigint (random.Next(-100, 101)))
    | TUInt8 -> UInt8Literal (byte (random.Next(0, 201)))
    | TUInt16 -> UInt16Literal (uint16 (random.Next(0, 201)))
    | TUInt32 -> UInt32Literal (uint32 (random.Next(0, 201)))
    | TUInt64 -> UInt64Literal (uint64 (random.Next(0, 201)))
    | TUInt128 -> UInt128Literal (UInt128.op_Implicit (uint64 (random.Next(0, 201))))
    | TBool -> BoolLiteral (random.Next(2) = 0)
    | TFloat64 -> FloatLiteral (float (random.Next(-100, 101)) / 4.0)
    | TChar -> CharLiteral (["a"; "é"; "🚀"] |> choose random |> Option.defaultValue "a")
    | TUnit -> UnitLiteral
    | TString ->
        [""; "a"; "dark"; "line\nbreak"; "quote\"slash\\"; "héllo"]
        |> choose random
        |> Option.defaultValue ""
        |> StringLiteral
    | TTuple [TInt64; TBool] ->
        TupleLiteral [generateLiteral random TInt64; generateLiteral random TBool]
    | TList elementType ->
        ListLiteral (List.init (random.Next(1, 4)) (fun _ -> generateLiteral random elementType))
    | TDict (TString, valueType) ->
        DictLiteral (TString, valueType,
            [StringLiteral "a", generateLiteral random valueType
             StringLiteral "b", generateLiteral random valueType])
    | TRecord ("FuzzBox", []) ->
        RecordLiteral (unresolvedRecordReference "FuzzBox" [],
            [unresolvedRecordFieldReference "value", generateLiteral random TInt64
             unresolvedRecordFieldReference "flag", generateLiteral random TBool])
    | TBlob ->
        Apply (Var "Stdlib.Blob.fromString", [],
            NonEmptyList.singleton (generateLiteral random TString))
    | TDateTime ->
        Apply (Var "Stdlib.DateTime.fromMilliseconds", [],
            NonEmptyList.singleton (generateLiteral random TInt))
    | TStream TInt64 ->
        Apply (Var "Stdlib.Stream.fromList", [],
            NonEmptyList.singleton (generateLiteral random (TList TInt64)))
    | _ -> Crash.crash $"Unsupported fuzzer literal type: {typ}"

let private generateLeaf
    (random: Random)
    (typ: SemanticType)
    (environment: (string * SemanticType) list)
    : Expr =
    match choose random (variablesOfType typ environment) with
    | Some name when random.Next(3) <> 0 -> Var name
    | _ -> generateLiteral random typ

let rec private generateExpr
    (random: Random)
    (depth: int)
    (nextVariable: int)
    (environment: (string * SemanticType) list)
    (typ: SemanticType)
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
                if random.Next(3) = 0 then
                    let left, afterLeft = generateChild TBool nextVariable
                    let right, afterRight = generateChild TBool afterLeft
                    let op = if random.Next(2) = 0 then And else Or
                    BinOp (op, left, right), afterRight
                else
                    let operandType =
                        choose random (scalarTypes |> List.filter ((<>) TUnit))
                        |> Option.defaultValue TInt64
                    let left, afterLeft = generateChild operandType nextVariable
                    let right, afterRight = generateChild operandType afterLeft
                    let op =
                        match operandType with
                        | TInt64 ->
                            [Eq; Neq; Lt; Gt; Lte; Gte]
                            |> choose random
                            |> Option.defaultValue Eq
                        | _ -> if random.Next(2) = 0 then Eq else Neq
                    BinOp (op, left, right), afterRight
            | TString ->
                let left, afterLeft = generateChild TString nextVariable
                let right, afterRight = generateChild TString afterLeft
                BinOp (StringConcat, left, right), afterRight
            | TTuple [TInt64; TBool] ->
                let left, afterLeft = generateChild TInt64 nextVariable
                let right, afterRight = generateChild TBool afterLeft
                TupleLiteral [left; right], afterRight
            | TList elementType ->
                let element, afterElement = generateChild elementType nextVariable
                ListLiteral [element], afterElement
            | TDict (TString, valueType) ->
                let value, afterValue = generateChild valueType nextVariable
                DictLiteral (TString, valueType, [StringLiteral "a", value]), afterValue
            | TRecord ("FuzzBox", []) ->
                let value, afterValue = generateChild TInt64 nextVariable
                let flag, afterFlag = generateChild TBool afterValue
                RecordLiteral (unresolvedRecordReference "FuzzBox" [],
                    [unresolvedRecordFieldReference "value", value
                     unresolvedRecordFieldReference "flag", flag]), afterFlag
            | _ -> generateLeaf random typ environment, nextVariable

        let generateFeature () =
            match random.Next(16), typ with
            | 0, TInt64 ->
                let left, afterLeft = generateChild TInt64 nextVariable
                let right, afterRight = generateChild TBool afterLeft
                let name = $"fuzz{afterRight}"
                Let (LPTuple (LPVariable name, LPWildcard, []),
                     TupleLiteral [left; right], Var name), afterRight + 1
            | 1, TBool ->
                let left, afterLeft = generateChild TInt64 nextVariable
                let right, afterRight = generateChild TBool afterLeft
                let name = $"fuzz{afterRight}"
                Let (LPTuple (LPWildcard, LPVariable name, []),
                     TupleLiteral [left; right], Var name), afterRight + 1
            | 2, TInt64 ->
                let element, afterElement = generateChild TInt64 nextVariable
                let body, afterBody = generateChild TInt64 afterElement
                let matched = { Patterns = NonEmptyList.singleton (PList [PVar "item"])
                                Guard = None; Body = BinOp (Add, Var "item", body) }
                let fallback = { Patterns = NonEmptyList.singleton PWildcard
                                 Guard = None; Body = Int64Literal 0L }
                Match (ListLiteral [element], [matched; fallback]), afterBody
            | 3, _ ->
                let arg, afterArg = generateChild typ nextVariable
                let parameter = typedLambdaVariable "input" typ
                let lambda = Lambda (NonEmptyList.singleton parameter, Some typ, Var "input")
                Apply (lambda, [], NonEmptyList.singleton arg), afterArg
            | 4, TBool ->
                let value, afterValue = generateChild TString nextVariable
                let interpolated = InterpolatedString [StringText "prefix:"; StringExpr value]
                BinOp (Eq, interpolated, interpolated), afterValue
            | 5, TInt64 ->
                let value, afterValue = generateChild TInt64 nextVariable
                let body, afterBody = generateChild TInt64 afterValue
                let someCase =
                    { Patterns = NonEmptyList.singleton (PConstructor ("Some", [PVar "item"]))
                      Guard = None; Body = BinOp (Add, Var "item", body) }
                let noneCase =
                    { Patterns = NonEmptyList.singleton (PConstructor ("None", []))
                      Guard = None; Body = Int64Literal 0L }
                Match (Constructor (UnresolvedConstructor None, "Some", [value]),
                       [someCase; noneCase]), afterBody
            | 6, TBool ->
                let value, afterValue = generateChild TInt64 nextVariable
                let guarded =
                    { Patterns = NonEmptyList.singleton (PConstructor ("Some", [PVar "item"]))
                      Guard = Some (BinOp (Gt, Var "item", Int64Literal 0L))
                      Body = BoolLiteral true }
                let fallback =
                    { Patterns = NonEmptyList.singleton PWildcard
                      Guard = None; Body = BoolLiteral false }
                Match (Constructor (UnresolvedConstructor None, "Some", [value]),
                       [guarded; fallback]), afterValue
            | 7, TInt64 ->
                let value, afterValue = generateChild TInt64 nextVariable
                let flag, afterFlag = generateChild TBool afterValue
                let box = RecordLiteral (unresolvedRecordReference "FuzzBox" [],
                    [unresolvedRecordFieldReference "value", value
                     unresolvedRecordFieldReference "flag", flag])
                let name = $"fuzz{afterFlag}"
                Let (LPVariable name, box,
                     RecordAccess (Var name, unresolvedRecordFieldReference "value")), afterFlag + 1
            | 8, TBool ->
                let value, afterValue = generateChild TInt64 nextVariable
                let flag, afterFlag = generateChild TBool afterValue
                let box = RecordLiteral (unresolvedRecordReference "FuzzBox" [],
                    [unresolvedRecordFieldReference "value", value
                     unresolvedRecordFieldReference "flag", flag])
                let name = $"fuzz{afterFlag}"
                Let (LPVariable name, box,
                     RecordAccess (Var name, unresolvedRecordFieldReference "flag")), afterFlag + 1
            | 9, TInt64 ->
                let value, afterValue = generateChild TInt64 nextVariable
                Apply (Var "fuzzIdentity", [], NonEmptyList.singleton value), afterValue
            | 10, TInt64 ->
                let value, afterValue = generateChild TInt64 nextVariable
                let okCase =
                    { Patterns = NonEmptyList.singleton (PConstructor ("Ok", [PVar "result"] ))
                      Guard = None; Body = Var "result" }
                let errorCase =
                    { Patterns = NonEmptyList.singleton (PConstructor ("Error", [PWildcard]))
                      Guard = None; Body = Int64Literal 0L }
                Match (Constructor (UnresolvedConstructor None, "Ok", [value]),
                       [okCase; errorCase]), afterValue
            | 11, TInt64 ->
                let original, afterOriginal = generateChild TInt64 nextVariable
                let replacement, afterReplacement = generateChild TInt64 afterOriginal
                let box = RecordLiteral (unresolvedRecordReference "FuzzBox" [],
                    [unresolvedRecordFieldReference "value", original
                     unresolvedRecordFieldReference "flag", BoolLiteral true])
                let name = $"fuzz{afterReplacement}"
                Let (LPVariable name, box,
                     RecordAccess (
                         RecordUpdate (Var name,
                             [unresolvedRecordFieldReference "value", replacement]),
                         unresolvedRecordFieldReference "value")), afterReplacement + 1
            | 12, TInt64 ->
                let first, afterFirst = generateChild TInt64 nextVariable
                let second, afterSecond = generateChild TInt64 afterFirst
                let consCase =
                    { Patterns = NonEmptyList.singleton (PListCons ([PVar "head"], PWildcard))
                      Guard = None; Body = Var "head" }
                let emptyCase =
                    { Patterns = NonEmptyList.singleton (PList [])
                      Guard = None; Body = Int64Literal 0L }
                Match (ListLiteral [first; second], [consCase; emptyCase]), afterSecond
            | 13, TBool ->
                let blob, afterBlob = generateChild TBlob nextVariable
                let blobLength =
                    Apply (Var "Stdlib.Blob.length", [], NonEmptyList.singleton blob)
                BinOp (Eq, blobLength, blobLength), afterBlob
            | 14, TBool ->
                let date, afterDate = generateChild TDateTime nextVariable
                let milliseconds =
                    Apply (Var "Stdlib.DateTime.toMilliseconds", [], NonEmptyList.singleton date)
                BinOp (Eq, milliseconds, milliseconds), afterDate
            | 15, TInt64 ->
                let first, afterFirst = generateChild TInt64 nextVariable
                let stream =
                    Apply (Var "Stdlib.Stream.fromList", [],
                        NonEmptyList.singleton (ListLiteral [first]))
                let contents =
                    Apply (Var "Stdlib.Stream.toList", [], NonEmptyList.singleton stream)
                let one =
                    { Patterns = NonEmptyList.singleton (PList [PVar "item"])
                      Guard = None; Body = Var "item" }
                let fallback =
                    { Patterns = NonEmptyList.singleton PWildcard
                      Guard = None; Body = Int64Literal 0L }
                Match (contents, [one; fallback]), afterFirst
            | _, _ -> generateTypedOperation ()

        match random.Next(7) with
        | 0 -> generateLeaf random typ environment, nextVariable
        | 1 -> generateIf ()
        | 2 -> generateLet ()
        | 3 | 4 -> generateTypedOperation ()
        | _ -> generateFeature ()

let generateProgram (random: Random) (maxDepth: int) : Program =
    let resultType =
        choose random observableTypes |> Option.defaultValue TInt64
    let expression, _ = generateExpr random maxDepth 0 [] resultType
    let recordDefinition =
        TypeDef (RecordDef ("FuzzBox", [], ["value", TInt64; "flag", TBool]))
    let identity =
        FunctionDef {
            Name = "fuzzIdentity"
            TypeParams = []
            Params = NonEmptyList.singleton ("input", TInt64)
            ReturnType = TInt64
            Body = Var "input"
            Recursion = None
        }
    Program [recordDefinition; identity; Expression ([], expression)]

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
        PackageManager = None
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
    (environment: (string * SemanticType) list)
    (expr: Expr)
    : SemanticType option =
    let variableType name =
        environment
        |> List.tryPick (fun (variableName, typ) ->
            if variableName = name then Some typ else None)

    let sameOperandTypes left right =
        match inferGeneratedType environment left, inferGeneratedType environment right with
        | Some leftType, Some rightType when leftType = rightType -> Some leftType
        | _ -> None

    match expr with
    | UnitLiteral -> Some TUnit
    | Int8Literal _ -> Some TInt8
    | Int16Literal _ -> Some TInt16
    | Int32Literal _ -> Some TInt32
    | Int64Literal _ -> Some TInt64
    | Int128Literal _ -> Some TInt128
    | UInt8Literal _ -> Some TUInt8
    | UInt16Literal _ -> Some TUInt16
    | UInt32Literal _ -> Some TUInt32
    | UInt64Literal _ -> Some TUInt64
    | UInt128Literal _ -> Some TUInt128
    | BigIntLiteral _ -> Some TInt
    | BoolLiteral _ -> Some TBool
    | FloatLiteral _ -> Some TFloat64
    | StringLiteral _ -> Some TString
    | CharLiteral _ -> Some TChar
    | InterpolatedString _ -> Some TString
    | Var name -> variableType name
    | TupleLiteral elements ->
        elements
        |> List.fold (fun result element ->
            match result, inferGeneratedType environment element with
            | Some types, Some typ -> Some (types @ [typ])
            | _ -> None) (Some [])
        |> Option.map TTuple
    | TupleAccess (tuple, index) ->
        match inferGeneratedType environment tuple with
        | Some (TTuple elements) -> List.tryItem index elements
        | _ -> None
    | ListLiteral (first :: rest) ->
        inferGeneratedType environment first
        |> Option.bind (fun elementType ->
            if rest |> List.forall (fun item -> inferGeneratedType environment item = Some elementType) then
                Some (TList elementType)
            else None)
    | ListLiteral [] -> None
    | DictLiteral (_, _, (firstKey, firstValue) :: rest) ->
        match inferGeneratedType environment firstKey, inferGeneratedType environment firstValue with
        | Some keyType, Some valueType when
            rest |> List.forall (fun (key, value) ->
                inferGeneratedType environment key = Some keyType &&
                inferGeneratedType environment value = Some valueType) ->
            Some (TDict (keyType, valueType))
        | _ -> None
    | DictLiteral _ -> None
    | RecordLiteral (reference, fields) when reference.SourceTypeName = "FuzzBox" ->
        let fieldType reference =
            match reference.SourceFieldName with
            | "value" -> Some TInt64
            | "flag" -> Some TBool
            | _ -> None
        if List.length fields = 2 &&
           (fields |> List.map (fun (field, _) -> field.SourceFieldName) |> Set.ofList) =
               (Set.ofList ["value"; "flag"]) &&
           (fields |> List.forall (fun (field, value) ->
               match fieldType field, inferGeneratedType environment value with
               | Some expected, Some actual -> expected = actual
               | _ -> false)) then
            Some (TRecord ("FuzzBox", []))
        else None
    | RecordAccess (record, field) ->
        match inferGeneratedType environment record, field.SourceFieldName with
        | Some (TRecord ("FuzzBox", [])), "value" -> Some TInt64
        | Some (TRecord ("FuzzBox", [])), "flag" -> Some TBool
        | _ -> None
    | RecordUpdate (record, updates) ->
        match inferGeneratedType environment record with
        | Some (TRecord ("FuzzBox", []) as recordType) when
            updates |> List.forall (fun (field, value) ->
                match field.SourceFieldName, inferGeneratedType environment value with
                | "value", Some TInt64 | "flag", Some TBool -> true
                | _ -> false) -> Some recordType
        | _ -> None
    | Lambda (parameters, _, body) ->
        let parameters = NonEmptyList.toList parameters
        let typedParameters =
            parameters |> List.choose (fun parameter ->
                match parameter.Pattern, parameter.SourceAnnotation with
                | LPVariable name, Some typ -> Some (name, typ)
                | _ -> None)
        if List.length typedParameters <> List.length parameters then None
        else
            inferGeneratedType (typedParameters @ environment) body
            |> Option.map (fun resultType -> TFunction (List.map snd typedParameters, resultType))
    | Apply (Var name, [], args) when
        List.contains name
            ["Stdlib.Blob.fromString"; "Stdlib.Blob.length"
             "Stdlib.DateTime.fromMilliseconds"; "Stdlib.DateTime.toMilliseconds"
             "Stdlib.Stream.fromList"; "Stdlib.Stream.toList"] ->
        let argument = args.Head
        match name, inferGeneratedType environment argument with
        | "Stdlib.Blob.fromString", Some TString -> Some TBlob
        | "Stdlib.Blob.length", Some TBlob -> Some TInt
        | "Stdlib.DateTime.fromMilliseconds", Some TInt -> Some TDateTime
        | "Stdlib.DateTime.toMilliseconds", Some TDateTime -> Some TInt
        | "Stdlib.Stream.fromList", Some (TList TInt64) -> Some (TStream TInt64)
        | "Stdlib.Stream.toList", Some (TStream TInt64) -> Some (TList TInt64)
        | _ -> None
    | Apply (Lambda (parameters, _, body), [], args) ->
        let parameterList = NonEmptyList.toList parameters
        let argumentList = NonEmptyList.toList args
        if List.length parameterList <> List.length argumentList then None
        else
            let bindings =
                List.zip parameterList argumentList
                |> List.choose (fun (parameter, argument) ->
                    match parameter.Pattern, inferGeneratedType environment argument with
                    | LPVariable name, Some typ -> Some (name, typ)
                    | _ -> None)
            if List.length bindings <> List.length parameterList then None
            else inferGeneratedType (bindings @ environment) body
    | Apply (callee, [], args) ->
        match inferGeneratedType environment callee with
        | Some (TFunction (parameterTypes, resultType)) ->
            let argumentTypes =
                args |> NonEmptyList.toList |> List.map (inferGeneratedType environment)
            if List.map Some parameterTypes = argumentTypes then Some resultType else None
        | _ -> None
    | Match (scrutinee, cases) ->
        let scrutineeType = inferGeneratedType environment scrutinee
        let caseType case =
            let bindings =
                match case.Patterns.Head, scrutineeType with
                | PList [PVar name], Some (TList elementType) -> [(name, elementType)]
                | PListCons ([PVar name], PWildcard), Some (TList elementType) ->
                    [(name, elementType)]
                | PConstructor ("Some", [PVar name]), Some (TSum ("Option", [elementType])) ->
                    [(name, elementType)]
                | PConstructor ("Ok", [PVar name]), Some (TSum ("Result", [okType; _])) ->
                    [(name, okType)]
                | _ -> []
            inferGeneratedType (bindings @ environment) case.Body
        match cases |> List.map caseType with
        | Some typ :: rest when rest |> List.forall ((=) (Some typ)) -> Some typ
        | _ -> None
    | Constructor (UnresolvedConstructor None, "Some", [value]) ->
        inferGeneratedType environment value |> Option.map (fun typ -> TSum ("Option", [typ]))
    | Constructor (UnresolvedConstructor None, "Ok", [value]) ->
        inferGeneratedType environment value
        |> Option.map (fun typ -> TSum ("Result", [typ; TString]))
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
    | Let (LPTuple (first, second, []), binding, body) ->
        let bind pattern typ =
            match pattern with
            | LPVariable name -> [(name, typ)]
            | _ -> []
        match inferGeneratedType environment binding with
        | Some (TTuple [firstType; secondType]) ->
            inferGeneratedType
                (bind first firstType @ bind second secondType @ environment) body
        | _ -> None
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
    | TupleLiteral elements | ListLiteral elements ->
        1 + (elements |> List.sumBy expressionSize)
    | TupleAccess (tuple, _) -> 1 + expressionSize tuple
    | Apply (callee, _, args) ->
        1 + expressionSize callee + (args |> NonEmptyList.toList |> List.sumBy expressionSize)
    | Lambda (_, _, body) -> 1 + expressionSize body
    | Match (scrutinee, cases) ->
        1 + expressionSize scrutinee + (cases |> List.sumBy (fun case -> expressionSize case.Body))
    | RecordLiteral (_, fields) ->
        1 + (fields |> List.sumBy (snd >> expressionSize))
    | RecordAccess (record, _) -> 1 + expressionSize record
    | RecordUpdate (record, updates) ->
        1 + expressionSize record + (updates |> List.sumBy (snd >> expressionSize))
    | _ -> 1

/// Enumerate deterministic local rewrites over the compiler AST. The oracle,
/// not this function, decides whether a rewrite preserves the reported defect.
let rec private oneStepSimplifications (expr: Expr) : Expr list =
    let simplifyElements rebuild elements =
        elements
        |> List.mapi (fun index element ->
            oneStepSimplifications element
            |> List.map (fun candidate ->
                elements
                |> List.mapi (fun currentIndex current ->
                    if currentIndex = index then candidate else current)
                |> rebuild))
        |> List.concat

    let literalSimplifications =
        match expr with
        | Int8Literal value when value <> 0y -> [Int8Literal 0y]
        | Int16Literal value when value <> 0s -> [Int16Literal 0s]
        | Int32Literal value when value <> 0 -> [Int32Literal 0]
        | Int64Literal value when value <> 0L ->
            let towardSign = if value < 0L then -1L else 1L
            [Int64Literal 0L; Int64Literal towardSign]
        | Int128Literal value when value <> Int128.Zero -> [Int128Literal Int128.Zero]
        | BigIntLiteral value when value <> BigInteger.Zero -> [BigIntLiteral BigInteger.Zero]
        | UInt8Literal value when value <> 0uy -> [UInt8Literal 0uy]
        | UInt16Literal value when value <> 0us -> [UInt16Literal 0us]
        | UInt32Literal value when value <> 0u -> [UInt32Literal 0u]
        | UInt64Literal value when value <> 0UL -> [UInt64Literal 0UL]
        | UInt128Literal value when value <> UInt128.Zero -> [UInt128Literal UInt128.Zero]
        | FloatLiteral value when value <> 0.0 -> [FloatLiteral 0.0]
        | BoolLiteral false -> [BoolLiteral true]
        | StringLiteral value when value <> "" -> [StringLiteral ""]
        | CharLiteral value when value <> "a" -> [CharLiteral "a"]
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
        | TupleLiteral elements -> simplifyElements TupleLiteral elements
        | ListLiteral elements ->
            (if List.length elements > 1 then
                 elements
                 |> List.mapi (fun index _ ->
                     ListLiteral (elements |> List.mapi (fun i item -> i, item)
                                           |> List.choose (fun (i, item) -> if i = index then None else Some item)))
             else [])
            @ simplifyElements ListLiteral elements
        | DictLiteral (keyType, valueType, entries) ->
            entries
            |> List.mapi (fun index (key, value) ->
                oneStepSimplifications key
                |> List.map (fun candidate ->
                    DictLiteral (keyType, valueType,
                        entries |> List.mapi (fun i entry -> if i = index then candidate, value else entry)))
                |> fun keyCandidates ->
                    keyCandidates @
                    (oneStepSimplifications value
                     |> List.map (fun candidate ->
                         DictLiteral (keyType, valueType,
                             entries |> List.mapi (fun i entry -> if i = index then key, candidate else entry)))))
            |> List.concat
        | RecordLiteral (reference, fields) ->
            fields
            |> List.mapi (fun index (field, value) ->
                oneStepSimplifications value
                |> List.map (fun candidate ->
                    RecordLiteral (reference,
                        fields |> List.mapi (fun i entry -> if i = index then field, candidate else entry))))
            |> List.concat
        | Constructor (reference, name, fields) ->
            simplifyElements (fun candidates -> Constructor (reference, name, candidates)) fields
        | InterpolatedString parts ->
            parts
            |> List.mapi (fun index part ->
                match part with
                | StringExpr value ->
                    oneStepSimplifications value
                    |> List.map (fun candidate ->
                        InterpolatedString (
                            parts |> List.mapi (fun i current ->
                                if i = index then StringExpr candidate else current)))
                | StringText _ -> [])
            |> List.concat
        | Lambda (parameters, annotation, body) ->
            oneStepSimplifications body
            |> List.map (fun candidate -> Lambda (parameters, annotation, candidate))
        | TupleAccess (tuple, index) ->
            let selected =
                match tuple with
                | TupleLiteral elements -> List.tryItem index elements |> Option.toList
                | _ -> []
            selected @ (oneStepSimplifications tuple
                        |> List.map (fun candidate -> TupleAccess (candidate, index)))
        | Match (scrutinee, cases) ->
            (cases |> List.map (fun case -> case.Body))
            @ (oneStepSimplifications scrutinee
               |> List.map (fun candidate -> Match (candidate, cases)))
            @ (cases
               |> List.mapi (fun index case ->
                   oneStepSimplifications case.Body
                   |> List.map (fun candidate ->
                       Match (scrutinee,
                           cases |> List.mapi (fun i current ->
                               if i = index then { case with Body = candidate } else current))))
               |> List.concat)
        | Apply (callee, typeArgs, args) ->
            match NonEmptyList.toList args with
            | [arg] ->
                [arg]
                @ (oneStepSimplifications callee |> List.map (fun candidate ->
                    Apply (candidate, typeArgs, args)))
                @ (oneStepSimplifications arg |> List.map (fun candidate ->
                    Apply (callee, typeArgs, NonEmptyList.singleton candidate)))
            | _ -> []
        | RecordAccess (record, field) ->
            oneStepSimplifications record
            |> List.map (fun candidate -> RecordAccess (candidate, field))
        | RecordUpdate (record, updates) ->
            (oneStepSimplifications record
             |> List.map (fun candidate -> RecordUpdate (candidate, updates)))
            @ (updates
               |> List.mapi (fun index (field, value) ->
                   oneStepSimplifications value
                   |> List.map (fun candidate ->
                       RecordUpdate (record,
                           updates |> List.mapi (fun i entry ->
                               if i = index then field, candidate else entry))))
               |> List.concat)
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
    let parsed =
        Parser.parseString false source
        |> Result.map semanticProgramOfParsed
        |> Result.bind (fun (Program topLevels) ->
            match topLevels with
            | [Expression (_, expression)] -> Ok ([], expression)
            | [((TypeDef _) as typeDef); ((FunctionDef _) as functionDef); Expression (_, expression)] ->
                Ok ([typeDef; functionDef], expression)
            | _ -> Error "Minimizer input must contain one expression, optionally after the fuzzer declarations")
    match parsed with
    | Error message -> Error $"Cannot minimize source: {message}"
    | Ok (declarations, originalExpr) ->
        let initialEnvironment =
            if List.isEmpty declarations then []
            else ["fuzzIdentity", TFunction ([TInt64], TInt64)]
        match inferGeneratedType initialEnvironment originalExpr with
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
                            match inferGeneratedType initialEnvironment candidateExpr with
                            | Some candidateType when candidateType = originalType ->
                                let candidateSource =
                                    Program (declarations @ [Expression ([], candidateExpr)])
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
