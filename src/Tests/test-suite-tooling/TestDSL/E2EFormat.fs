// E2EFormat.fs - End-to-end test format parser
//
// Parses E2E test files in a simple line-based format.
//
// Format: source = expectation  // optional comment
//
// Example:
//   // Addition tests
//   2 + 3 = 5
//   1 + 2 + 3 = 10  // left and right sides must evaluate to equal values

module TestDSL.E2EFormat

open System

/// Input supplied to the native process. Tests must choose between an already
/// closed stream and a finite byte sequence whose stream closes after delivery.
type TestStdin =
    | Closed
    | Bytes of string

/// Output comparison policy. Presentation tests use ExactBytes so control
/// bytes, whitespace, and the absence of a final newline remain observable.
type OutputMatch =
    | NormalizedText
    | ExactBytes

/// End-to-end test specification
type E2ETest = {
    Name: string
    /// The test expression only (NOT including preamble)
    Source: string
    /// Expected value expression on the right-hand side of `=`.
    /// When set, the runner compares Source and ExpectedValueExpr for value equality.
    ExpectedValueExpr: string option
    /// Preamble code (function/type definitions shared across tests in file)
    Preamble: string
    ExpectedStdout: string option
    ExpectedStderr: string option
    Stdin: TestStdin
    OutputMatch: OutputMatch
    ExpectedExitCode: int
    /// If true, expect the compiler to fail (type error, parse error, etc.)
    ExpectCompileError: bool
    /// Expected error message (substring match) when ExpectCompileError is true
    ExpectedErrorMessage: string option
    /// If set, test is skipped with this reason
    SkipReason: string option
    /// Compiler options for disabling optimizations
    DisableFreeList: bool
    DisableANFOpt: bool
    DisableANFConstFolding: bool
    DisableANFConstProp: bool
    DisableANFCopyProp: bool
    DisableANFDCE: bool
    DisableANFStrengthReduction: bool
    DisableInlining: bool
    DisableTCO: bool
    DisableMIROpt: bool
    DisableMIRConstFolding: bool
    DisableMIRCSE: bool
    DisableMIRCopyProp: bool
    DisableMIRDCE: bool
    DisableMIRCFGSimplify: bool
    DisableMIRLICM: bool
    DisableLIROpt: bool
    DisableLIRPeephole: bool
    DisableFunctionTreeShaking: bool
    DisableLeakCheck: bool
    /// Source file this test came from (for grouping in output)
    SourceFile: string
    /// Maps function names to their definition line numbers in the source file
    /// Used for caching compiled functions across tests
    FunctionLineMap: Map<string, int>
}

/// Optimization flags for test parsing (internal type)
type private OptFlags = {
    DisableFreeList: bool
    DisableANFOpt: bool
    DisableANFConstFolding: bool
    DisableANFConstProp: bool
    DisableANFCopyProp: bool
    DisableANFDCE: bool
    DisableANFStrengthReduction: bool
    DisableInlining: bool
    DisableTCO: bool
    DisableMIROpt: bool
    DisableMIRConstFolding: bool
    DisableMIRCSE: bool
    DisableMIRCopyProp: bool
    DisableMIRDCE: bool
    DisableMIRCFGSimplify: bool
    DisableMIRLICM: bool
    DisableLIROpt: bool
    DisableLIRPeephole: bool
    DisableFunctionTreeShaking: bool
    DisableLeakCheck: bool
    Stdin: TestStdin
    OutputMatch: OutputMatch
}

let private defaultOptFlags = {
    DisableFreeList = false
    DisableANFOpt = false
    DisableANFConstFolding = false
    DisableANFConstProp = false
    DisableANFCopyProp = false
    DisableANFDCE = false
    DisableANFStrengthReduction = false
    DisableInlining = false
    DisableTCO = false
    DisableMIROpt = false
    DisableMIRConstFolding = false
    DisableMIRCSE = false
    DisableMIRCopyProp = false
    DisableMIRDCE = false
    DisableMIRCFGSimplify = false
    DisableMIRLICM = false
    DisableLIROpt = false
    DisableLIRPeephole = false
    DisableFunctionTreeShaking = false
    DisableLeakCheck = false
    Stdin = Closed
    OutputMatch = NormalizedText
}

/// Extract function name from a definition line (e.g., "def buildTree(...)" -> "buildTree")
let private extractFuncName (line: string) : string option =
    let trimmed = line.Trim()
    if trimmed.StartsWith("def ") then
        let afterDef = trimmed.Substring(4).TrimStart()
        let endIdx = afterDef.IndexOfAny([|'('; '<'; ' '|])
        if endIdx > 0 then Some (afterDef.Substring(0, endIdx))
        else None
    else None

/// Parse string literal with escape sequences (\n, \t, \\, \")
let private parseStringLiteral (s: string) : Result<string, string> =
    if not (s.StartsWith("\"") && s.EndsWith("\"")) then
        Error $"String literal must be quoted: {s}"
    else
        let content = s.Substring(1, s.Length - 2)
        let rec loop (i: int) (charsRev: char list) : string =
            if i >= content.Length then
                charsRev |> List.rev |> List.toArray |> String
            elif content.[i] = '\\' && i + 1 < content.Length then
                match content.[i + 1] with
                | 'n' -> loop (i + 2) ('\n' :: charsRev)
                | 'r' -> loop (i + 2) ('\r' :: charsRev)
                | 't' -> loop (i + 2) ('\t' :: charsRev)
                | '\\' -> loop (i + 2) ('\\' :: charsRev)
                | '"' -> loop (i + 2) ('"' :: charsRev)
                | 'u' when i + 5 < content.Length ->
                    let hex = content.Substring(i + 2, 4)
                    match UInt16.TryParse(hex, Globalization.NumberStyles.HexNumber, Globalization.CultureInfo.InvariantCulture) with
                    | true, code -> loop (i + 6) (char code :: charsRev)
                    | false, _ -> loop (i + 2) ('u' :: '\\' :: charsRev)
                | escaped ->
                    loop (i + 2) (escaped :: '\\' :: charsRev)
            else
                loop (i + 1) (content.[i] :: charsRev)

        Ok (loop 0 [])

/// Parse triple-quoted string literal """..."""
let private parseTripleQuotedStringLiteral (s: string) : Result<string, string> =
    if not (s.StartsWith("\"\"\"") && s.EndsWith("\"\"\"")) then
        Error $"Triple-quoted string literal must be wrapped in \"\"\": {s}"
    elif s.Length < 6 then
        Error $"Invalid triple-quoted string literal: {s}"
    else
        Ok (s.Substring(3, s.Length - 6))

/// Parse either a regular quoted string or a triple-quoted string
let private parseAnyStringLiteral (s: string) : Result<string, string> =
    let trimmed = s.Trim()
    if trimmed.StartsWith("\"\"\"") then
        parseTripleQuotedStringLiteral trimmed
    else
        parseStringLiteral trimmed

/// Remove simple outer wrapping parens repeatedly: ((x)) -> x
let private stripOuterParens (s: string) : string =
    let rec loop (current: string) =
        let trimmed = current.Trim()
        if trimmed.Length >= 2 && trimmed.StartsWith("(") && trimmed.EndsWith(")") then
            loop (trimmed.Substring(1, trimmed.Length - 2))
        else
            trimmed
    loop s

/// Parse Builtin.testDerrorMessage / Builtin.testDerrorSqlMessage expectation helpers.
/// Returns Some result when expression matches either helper name.
let private tryParseBuiltinErrorExpectation (exp: string) : Result<string option, string> option =
    let normalized = stripOuterParens exp
    let helperPrefixes = [
        "Builtin.testDerrorMessage"
        "Stdlib.Builtin.testDerrorMessage"
        "Builtin.testDerrorSqlMessage"
        "Stdlib.Builtin.testDerrorSqlMessage"
    ]
    let matchingPrefix =
        helperPrefixes
        |> List.tryFind (fun prefix -> normalized.StartsWith(prefix))

    match matchingPrefix with
    | None -> None
    | Some prefix ->
        let argText = normalized.Substring(prefix.Length).Trim()
        if argText.Length = 0 then
            Some (Ok None)
        else
            let argNormalized =
                let trimmedArg = argText.Trim()
                if trimmedArg.StartsWith("(") && trimmedArg.EndsWith(")") then
                    trimmedArg.Substring(1, trimmedArg.Length - 2).Trim()
                else
                    trimmedArg
            Some (
                parseAnyStringLiteral argNormalized
                |> Result.map Some
            )

/// Parse key=value attribute
let private parseAttribute (attr: string) : Result<string * string, string> =
    match attr.Split([|'='|], 2) with
    | [| key; value |] -> Ok (key.Trim(), value.Trim())
    | _ -> Error $"Invalid attribute format: {attr}"

/// Split string by spaces, respecting quoted strings with escape sequences
/// e.g., 'stdout="hello world" exit=0' -> ['stdout="hello world"'; 'exit=0']
let private splitBySpacesRespectingQuotes (s: string) : string list =
    let tokenFromChars (charsRev: char list) : string =
        charsRev |> List.rev |> List.toArray |> String

    let rec loop (i: int) (inQuotes: bool) (currentRev: char list) (tokensRev: string list) : string list =
        if i >= s.Length then
            match currentRev with
            | [] -> List.rev tokensRev
            | chars -> List.rev (tokenFromChars chars :: tokensRev)
        else
            let c = s.[i]
            if c = '\\' && i + 1 < s.Length then
                loop (i + 2) inQuotes (s.[i + 1] :: c :: currentRev) tokensRev
            elif c = '"' then
                loop (i + 1) (not inQuotes) (c :: currentRev) tokensRev
            elif c = ' ' && not inQuotes then
                match currentRev with
                | [] -> loop (i + 1) inQuotes [] tokensRev
                | chars -> loop (i + 1) inQuotes [] (tokenFromChars chars :: tokensRev)
            else
                loop (i + 1) inQuotes (c :: currentRev) tokensRev

    loop 0 false [] []

/// Find start index of a `//` comment outside quoted strings.
let private findCommentStartOutsideQuotes (line: string) : int option =
    let rec loop (i: int) (inQuotes: bool) : int option =
        if i >= line.Length then
            None
        elif inQuotes && line.[i] = '\\' && i + 1 < line.Length then
            loop (i + 2) inQuotes
        elif line.[i] = '"' then
            loop (i + 1) (not inQuotes)
        elif not inQuotes && line.[i] = '/' && i + 1 < line.Length && line.[i + 1] = '/' then
            Some i
        else
            loop (i + 1) inQuotes
    loop 0 false

/// Check if string starts with expectation keywords (digit, -, stdout, etc.)
let private isExpectationStart (rest: string) : bool =
    let trimmed = rest.TrimStart()
    if trimmed.Length = 0 then false
    elif Char.IsDigit(trimmed.[0]) || trimmed.[0] = '-' then true
    elif trimmed.[0] = '"' || trimmed.[0] = '\'' || trimmed.[0] = '(' || trimmed.[0] = '[' then true
    elif Char.IsLetter(trimmed.[0]) then true
    elif trimmed.StartsWith("exit")
         || trimmed.StartsWith("stdout")
         || trimmed.StartsWith("stderr")
         || trimmed.StartsWith("skip")
         || trimmed.StartsWith("no_free_list")
         || trimmed.StartsWith("disable_leak_check")
         || trimmed.StartsWith("stdin")
         || trimmed.StartsWith("exact_bytes")
         || trimmed.StartsWith("error")
         || trimmed.StartsWith("disable_opt_") then
        true
    else false

let private stripQuotedContent (s: string) : string =
    let rec loop (i: int) (inQuotes: bool) (charsRev: char list) : string =
        if i >= s.Length then
            charsRev |> List.rev |> List.toArray |> String
        else
            let c = s.[i]
            if c = '\\' && i + 1 < s.Length then
                if inQuotes then
                    loop (i + 2) inQuotes charsRev
                else
                    loop (i + 2) inQuotes (s.[i + 1] :: c :: charsRev)
            elif c = '"' then
                loop (i + 1) (not inQuotes) (' ' :: charsRev)
            elif inQuotes then
                loop (i + 1) inQuotes charsRev
            else
                loop (i + 1) inQuotes (c :: charsRev)

    loop 0 false []

let private isExpectationCandidate (rest: string) : bool =
    let trimmed = rest.TrimStart()
    if not (isExpectationStart trimmed) then
        false
    elif trimmed.StartsWith("\"")
         || trimmed.StartsWith("'")
         || trimmed.StartsWith("error")
         || trimmed.StartsWith("stdout")
         || trimmed.StartsWith("stderr")
         || trimmed.StartsWith("skip")
         || trimmed.StartsWith("exit")
         || trimmed.StartsWith("no_free_list")
         || trimmed.StartsWith("disable_leak_check")
         || trimmed.StartsWith("stdin")
         || trimmed.StartsWith("exact_bytes")
         || trimmed.StartsWith("disable_opt_") then
        true
    else
        let lowered = stripQuotedContent trimmed |> fun s -> s.ToLowerInvariant()
        let hasKeyword =
            lowered.StartsWith("let ") || lowered.Contains(" let ")
            || lowered.StartsWith("if ") || lowered.Contains(" if ")
            || lowered.StartsWith("match ") || lowered.Contains(" match ")
            || lowered.StartsWith("then ") || lowered.Contains(" then ")
            || lowered.StartsWith("else ") || lowered.Contains(" else ")
            || lowered.StartsWith("type ") || lowered.Contains(" type ")
            || lowered.StartsWith("def ") || lowered.Contains(" def ")
            || lowered.Contains(" in ")
        not hasKeyword

let private isAttributeKey (key: string) : bool =
    match key with
    | "exit"
    | "stdout"
    | "stderr"
    | "skip"
    | "no_free_list"
    | "disable_leak_check"
    | "stdin"
    | "exact_bytes"
    | "disable_opt_freelist"
    | "disable_opt_anf"
    | "disable_opt_anf_const_folding"
    | "disable_opt_anf_const_prop"
    | "disable_opt_anf_copy_prop"
    | "disable_opt_anf_dce"
    | "disable_opt_anf_strength_reduction"
    | "disable_opt_inline"
    | "disable_opt_tco"
    | "disable_opt_mir"
    | "disable_opt_mir_const_folding"
    | "disable_opt_mir_cse"
    | "disable_opt_mir_copy_prop"
    | "disable_opt_mir_dce"
    | "disable_opt_mir_cfg_simplify"
    | "disable_opt_mir_licm"
    | "disable_opt_lir"
    | "disable_opt_lir_peephole"
    | "disable_opt_dce"
    | "disable_opt_function_tree_shaking" -> true
    | _ -> false

let private parseSimpleStdout (valueText: string) : Result<string, string> =
    let trimmed = valueText.Trim()
    if trimmed.Length = 0 then
        Error "Expected stdout value"
    elif trimmed.StartsWith("\"") then
        match parseStringLiteral trimmed with
        | Ok s -> Ok $"{s}\n"
        | Error e -> Error e
    else
        Ok $"{trimmed}\n"

/// Check if line has ) followed by = <expectation> (for multi-line expression closing)
let private hasUnclosedDelimiters (text: string) : bool =
    let rec scan (i: int) (inQuotes: bool) (parenDepth: int) (bracketDepth: int) (braceDepth: int) : bool =
        if i >= text.Length then
            inQuotes || parenDepth > 0 || bracketDepth > 0 || braceDepth > 0
        elif inQuotes && text.[i] = '\\' && i + 1 < text.Length then
            scan (i + 2) inQuotes parenDepth bracketDepth braceDepth
        elif text.[i] = '"' then
            scan (i + 1) (not inQuotes) parenDepth bracketDepth braceDepth
        elif inQuotes then
            scan (i + 1) inQuotes parenDepth bracketDepth braceDepth
        elif text.[i] = '(' then
            scan (i + 1) inQuotes (parenDepth + 1) bracketDepth braceDepth
        elif text.[i] = ')' then
            scan (i + 1) inQuotes (max 0 (parenDepth - 1)) bracketDepth braceDepth
        elif text.[i] = '[' then
            scan (i + 1) inQuotes parenDepth (bracketDepth + 1) braceDepth
        elif text.[i] = ']' then
            scan (i + 1) inQuotes parenDepth (max 0 (bracketDepth - 1)) braceDepth
        elif text.[i] = '{' then
            scan (i + 1) inQuotes parenDepth bracketDepth (braceDepth + 1)
        elif text.[i] = '}' then
            scan (i + 1) inQuotes parenDepth bracketDepth (max 0 (braceDepth - 1))
        else
            scan (i + 1) inQuotes parenDepth bracketDepth braceDepth
    scan 0 false 0 0 0

let private isIncompleteExpectationHead (afterEq: string) : bool =
    let trimmed = afterEq.Trim()
    if trimmed.Length = 0 then
        true
    elif hasUnclosedDelimiters trimmed then
        true
    else
        false

let private isIdentifierPathHead (afterEq: string) : bool =
    let trimmed = afterEq.Trim()
    if trimmed.Length = 0 then
        false
    else
        trimmed
        |> Seq.forall (fun c -> Char.IsLetterOrDigit(c) || c = '_' || c = '.')

let private hasClosingParenTest (s: string) : bool =
    let rec findPattern
        (i: int)
        (inQuotes: bool)
        (parenDepth: int)
        (bracketDepth: int)
        (braceDepth: int)
        : bool =
        if i >= s.Length then
            false
        elif inQuotes && s.[i] = '\\' && i + 1 < s.Length then
            findPattern (i + 2) inQuotes parenDepth bracketDepth braceDepth
        elif s.[i] = '"' then
            findPattern (i + 1) (not inQuotes) parenDepth bracketDepth braceDepth
        elif not inQuotes && s.[i] = '(' then
            findPattern (i + 1) inQuotes (parenDepth + 1) bracketDepth braceDepth
        elif not inQuotes && s.[i] = ')' then
            let newParenDepth = max 0 (parenDepth - 1)
            let isOutermostClose =
                newParenDepth = 0
                && bracketDepth = 0
                && braceDepth = 0
            if isOutermostClose then
                let rest = s.Substring(i + 1).TrimStart()
                if rest.StartsWith("=") then
                    let afterEq = rest.Substring(1)
                    if isExpectationCandidate afterEq && not (isIncompleteExpectationHead afterEq) then
                        true
                    else
                        findPattern (i + 1) inQuotes newParenDepth bracketDepth braceDepth
                else
                    findPattern (i + 1) inQuotes newParenDepth bracketDepth braceDepth
            else
                findPattern (i + 1) inQuotes newParenDepth bracketDepth braceDepth
        elif not inQuotes && s.[i] = '[' then
            findPattern (i + 1) inQuotes parenDepth (bracketDepth + 1) braceDepth
        elif not inQuotes && s.[i] = ']' then
            findPattern (i + 1) inQuotes parenDepth (max 0 (bracketDepth - 1)) braceDepth
        elif not inQuotes && s.[i] = '{' then
            findPattern (i + 1) inQuotes parenDepth bracketDepth (braceDepth + 1)
        elif not inQuotes && s.[i] = '}' then
            findPattern (i + 1) inQuotes parenDepth bracketDepth (max 0 (braceDepth - 1))
        else
            findPattern (i + 1) inQuotes parenDepth bracketDepth braceDepth
    findPattern 0 false 0 0 0

/// Find the = that separates source from expectations
/// Returns Some index if this looks like a test line, along with how many matches were seen
let private findSeparatorIndexAndCount (s: string) : int option * int =
    let rec findLast
        (i: int)
        (inQuotes: bool)
        (parenDepth: int)
        (bracketDepth: int)
        (braceDepth: int)
        (lastEqualsIdx: int option)
        (count: int)
        : int option * int =
        if i >= s.Length then
            (lastEqualsIdx, count)
        elif inQuotes && s.[i] = '\\' && i + 1 < s.Length then
            findLast (i + 2) inQuotes parenDepth bracketDepth braceDepth lastEqualsIdx count
        elif s.[i] = '"' then
            findLast (i + 1) (not inQuotes) parenDepth bracketDepth braceDepth lastEqualsIdx count
        elif not inQuotes && s.[i] = '(' then
            findLast (i + 1) inQuotes (parenDepth + 1) bracketDepth braceDepth lastEqualsIdx count
        elif not inQuotes && s.[i] = ')' then
            findLast (i + 1) inQuotes (max 0 (parenDepth - 1)) bracketDepth braceDepth lastEqualsIdx count
        elif not inQuotes && s.[i] = '[' then
            findLast (i + 1) inQuotes parenDepth (bracketDepth + 1) braceDepth lastEqualsIdx count
        elif not inQuotes && s.[i] = ']' then
            findLast (i + 1) inQuotes parenDepth (max 0 (bracketDepth - 1)) braceDepth lastEqualsIdx count
        elif not inQuotes && s.[i] = '{' then
            findLast (i + 1) inQuotes parenDepth bracketDepth (braceDepth + 1) lastEqualsIdx count
        elif not inQuotes && s.[i] = '}' then
            findLast (i + 1) inQuotes parenDepth bracketDepth (max 0 (braceDepth - 1)) lastEqualsIdx count
        elif s.[i] = '='
             && not inQuotes
             && parenDepth = 0
             && bracketDepth = 0
             && braceDepth = 0 then
            // Only consider this = as a separator if preceded by whitespace or )
            // This distinguishes "source = expectations" from "exit=139"
            let hasPrecedingSpace = i > 0 && (Char.IsWhiteSpace(s.[i - 1]) || s.[i - 1] = ')')
            if hasPrecedingSpace then
                // Check if this = is followed by an expectation
                let rest = s.Substring(i + 1)
                if isExpectationCandidate rest then
                    findLast (i + 1) inQuotes parenDepth bracketDepth braceDepth (Some i) (count + 1)
                else
                    findLast (i + 1) inQuotes parenDepth bracketDepth braceDepth lastEqualsIdx count
            else
                findLast (i + 1) inQuotes parenDepth bracketDepth braceDepth lastEqualsIdx count
        else
            findLast (i + 1) inQuotes parenDepth bracketDepth braceDepth lastEqualsIdx count
    findLast 0 false 0 0 0 None 0

let private countPotentialSeparators (s: string) : int =
    let rec countSeparators
        (i: int)
        (inQuotes: bool)
        (parenDepth: int)
        (bracketDepth: int)
        (braceDepth: int)
        (count: int)
        : int =
        if i >= s.Length then
            count
        elif inQuotes && s.[i] = '\\' && i + 1 < s.Length then
            countSeparators (i + 2) inQuotes parenDepth bracketDepth braceDepth count
        elif s.[i] = '"' then
            countSeparators (i + 1) (not inQuotes) parenDepth bracketDepth braceDepth count
        elif not inQuotes && s.[i] = '(' then
            countSeparators (i + 1) inQuotes (parenDepth + 1) bracketDepth braceDepth count
        elif not inQuotes && s.[i] = ')' then
            countSeparators (i + 1) inQuotes (max 0 (parenDepth - 1)) bracketDepth braceDepth count
        elif not inQuotes && s.[i] = '[' then
            countSeparators (i + 1) inQuotes parenDepth (bracketDepth + 1) braceDepth count
        elif not inQuotes && s.[i] = ']' then
            countSeparators (i + 1) inQuotes parenDepth (max 0 (bracketDepth - 1)) braceDepth count
        elif not inQuotes && s.[i] = '{' then
            countSeparators (i + 1) inQuotes parenDepth bracketDepth (braceDepth + 1) count
        elif not inQuotes && s.[i] = '}' then
            countSeparators (i + 1) inQuotes parenDepth bracketDepth (max 0 (braceDepth - 1)) count
        elif s.[i] = '='
             && not inQuotes
             && parenDepth = 0
             && bracketDepth = 0
             && braceDepth = 0 then
            let hasPrecedingSpace = i > 0 && (Char.IsWhiteSpace(s.[i - 1]) || s.[i - 1] = ')')
            let isDoubleEq =
                (i + 1 < s.Length && s.[i + 1] = '=')
                || (i > 0 && s.[i - 1] = '=')
            if hasPrecedingSpace && not isDoubleEq then
                countSeparators (i + 1) inQuotes parenDepth bracketDepth braceDepth (count + 1)
            else
                countSeparators (i + 1) inQuotes parenDepth bracketDepth braceDepth count
        else
            countSeparators (i + 1) inQuotes parenDepth bracketDepth braceDepth count
    countSeparators 0 false 0 0 0 0

let private findSeparatorIndex (s: string) : int option =
    let (idx, _) = findSeparatorIndexAndCount s
    idx

/// Check if a line (with comment removed) looks like a test line
let private isTestLine (lineWithoutComment: string) : bool =
    let trimmed = lineWithoutComment.TrimStart()
    let startsWithDefinition =
        trimmed.StartsWith("def ")
        || trimmed.StartsWith("type ")
        || trimmed.StartsWith("let ")
        || trimmed.StartsWith("[<")
    let (idxOpt, count) = findSeparatorIndexAndCount lineWithoutComment
    match idxOpt with
    | None -> false
    | Some _ when startsWithDefinition && countPotentialSeparators lineWithoutComment <= 1 -> false
    | Some _ -> true

/// Parse a single test line with optional preamble to prepend
/// Supports two formats:
///   Value equality: source = expectedValueExpr
///   Explicit attributes: source = [exit=N] [stdout="..."] [stderr="..."]
let private parseTestLineWithPreamble (line: string) (lineNumber: int) (filePath: string) (preamble: string) (funcLineMap: Map<string, int>) : Result<E2ETest, string> =
    // First, remove any comment
    let lineWithoutComment, comment =
        match findCommentStartOutsideQuotes line with
        | Some commentIdx ->
            (line.Substring(0, commentIdx).Trim(),
             Some (line.Substring(commentIdx + 2).Trim()))
        | None ->
            (line, None)

    match findSeparatorIndex lineWithoutComment with
    | None ->
        Error $"Line {lineNumber}: Expected format 'source = expectations', got: {line}"
    | Some equalsIdx ->
        let testExpr = lineWithoutComment.Substring(0, equalsIdx).Trim()
        // Source is just the test expression - preamble is stored separately
        let source = testExpr
        let expectationsStr = lineWithoutComment.Substring(equalsIdx + 1).Trim()
        // Parse expectations:
        // - error / error="..."
        // - skip / skip="..."
        // - Builtin.testDerrorMessage / Builtin.testDerrorSqlMessage
        // - explicit attributes (exit=, stdout=, stderr=, optimization flags)
        // - otherwise: RHS is an expression to compare with Source by value
        //
        // Returns:
        // (expectedValueExpr, exitCode, stdout, stderr, optFlags, expectError, errorMessage, skipReason)
        let parseExpectations (exp: string) : Result<string option * int * string option * string option * OptFlags * bool * string option * string option, string> =
            let trimmed = exp.Trim()
            let tokens = splitBySpacesRespectingQuotes trimmed

            let parseErrorAttributeFlags (attributeTokens: string list) : Result<OptFlags, string> =
                let rec loop (remaining: string list) (current: OptFlags) : Result<OptFlags, string> =
                    match remaining with
                    | [] -> Ok current
                    | token :: rest ->
                        match parseAttribute token with
                        | Error e ->
                            Error e
                        | Ok (key, value) ->
                            Error $"Unknown attribute: {key}={value}"

                loop attributeTokens defaultOptFlags

            // Check for "error" keyword (compiler error expected)
            // Supports: error  or  error="message"
            match tokens with
            | firstToken :: restTokens when firstToken.Equals("error", StringComparison.OrdinalIgnoreCase) ->
                parseErrorAttributeFlags restTokens
                |> Result.map (fun optFlags ->
                    // Expect compilation to fail with exit code 1, no specific message
                    (None, 1, None, None, optFlags, true, None, None))
            | firstToken :: restTokens when firstToken.StartsWith("error=", StringComparison.OrdinalIgnoreCase) ->
                // error="message" format, optionally followed by attributes
                let msgPart = firstToken.Substring(6)  // Skip "error="
                parseStringLiteral msgPart
                |> Result.mapError (fun e -> $"Invalid error message: {e}")
                |> Result.bind (fun msg ->
                    parseErrorAttributeFlags restTokens
                    |> Result.map (fun optFlags -> (None, 1, None, None, optFlags, true, Some msg, None)))
            | firstToken :: [] when firstToken.Equals("sqlerror", StringComparison.OrdinalIgnoreCase) ->
                // Legacy runtime SQL error shorthand with no specific message.
                Ok (None, 1, Some "", None, defaultOptFlags, false, None, None)
            | firstToken :: [] when firstToken.StartsWith("sqlerror=", StringComparison.OrdinalIgnoreCase) ->
                // Legacy runtime SQL error shorthand:
                //   source = sqlerror="message"
                let msgPart = firstToken.Substring(9)  // Skip "sqlerror="
                parseStringLiteral msgPart
                |> Result.mapError (fun e -> $"Invalid sqlerror message: {e}")
                |> Result.map (fun msg -> (None, 1, Some "", Some msg, defaultOptFlags, false, None, None))
            | _ ->
                if trimmed.ToLower() = "skip" then
                    Ok (None, 0, None, None, defaultOptFlags, false, None, Some "Skipped by test")
                elif trimmed.ToLower().StartsWith("skip=") then
                    let reasonPart = trimmed.Substring(5)
                    match parseStringLiteral reasonPart with
                    | Ok reason -> Ok (None, 0, None, None, defaultOptFlags, false, None, Some reason)
                    | Error e -> Error $"Invalid skip reason: {e}"
                else
                    match tryParseBuiltinErrorExpectation trimmed with
                    | Some (Ok expectedErr) ->
                        Ok (None, 1, Some "", expectedErr, defaultOptFlags, false, None, None)
                    | Some (Error e) ->
                        Error $"Invalid Builtin.testDerrorMessage expectation: {e}"
                    | None ->
                        // Explicit-attribute format (exit/stdout/stderr/optimization flags),
                        // with optional leading bare stdout for backward compatibility.
                        // If no attributes are present, treat RHS as a value expression.
                        let mutable exitCode = 0  // default
                        let mutable stdout = None
                        let mutable stderr = None
                        let mutable optFlags = defaultOptFlags
                        let mutable skipReason = None
                        let mutable errors = []

                        // Helper to parse boolean attribute value
                        let parseBool (value: string) (attrName: string) : bool option =
                            match value.ToLower() with
                            | "true" | "1" -> Some true
                            | "false" | "0" -> Some false
                            | _ ->
                                errors <- $"Invalid {attrName} value: {value} (expected true/false)" :: errors
                                None

                        // Split by spaces, respecting quoted strings with escape sequences
                        let tokens = splitBySpacesRespectingQuotes trimmed
                        let attrStartIndex =
                            tokens
                            |> List.tryFindIndex (fun token ->
                                match parseAttribute token with
                                | Ok (key, _) -> isAttributeKey key
                                | Error _ -> false)

                        match attrStartIndex with
                        | None ->
                            // Bare RHS is always treated as a value expression.
                            Ok (Some trimmed, 0, None, None, defaultOptFlags, false, None, None)
                        | Some idx ->
                            let leadingTokens = tokens |> List.take idx
                            let attrTokens = tokens |> List.skip idx
                            let leadingText =
                                if leadingTokens.Length = 0 then
                                    None
                                else
                                    Some (String.concat " " leadingTokens)
                            let mutable sawRuntimeExpectationAttribute = false

                            for token in attrTokens do
                                let attrTrimmed = token.Trim()
                                if attrTrimmed.Length > 0 then
                                    match parseAttribute attrTrimmed with
                                    | Ok (key, value) ->
                                        match key with
                                        | "exit" ->
                                            sawRuntimeExpectationAttribute <- true
                                            match Int32.TryParse(value) with
                                            | true, code -> exitCode <- code
                                            | false, _ -> errors <- $"Invalid exit code: {value}" :: errors
                                        | "stdout" ->
                                            sawRuntimeExpectationAttribute <- true
                                            match parseStringLiteral value with
                                            | Ok s -> stdout <- Some s
                                            | Error e -> errors <- e :: errors
                                        | "stderr" ->
                                            sawRuntimeExpectationAttribute <- true
                                            match parseStringLiteral value with
                                            | Ok s -> stderr <- Some s
                                            | Error e -> errors <- e :: errors
                                        | "skip" ->
                                            sawRuntimeExpectationAttribute <- true
                                            match parseStringLiteral value with
                                            | Ok reason -> skipReason <- Some reason
                                            | Error e -> errors <- $"Invalid skip reason: {e}" :: errors
                                        | "no_free_list" ->
                                            match parseBool value "no_free_list" with
                                            | Some b -> optFlags <- { optFlags with DisableFreeList = b }
                                            | None -> ()
                                        | "disable_opt_freelist" ->
                                            match parseBool value "disable_opt_freelist" with
                                            | Some b -> optFlags <- { optFlags with DisableFreeList = b }
                                            | None -> ()
                                        | "disable_opt_anf" ->
                                            match parseBool value "disable_opt_anf" with
                                            | Some b -> optFlags <- { optFlags with DisableANFOpt = b }
                                            | None -> ()
                                        | "disable_opt_anf_const_folding" ->
                                            match parseBool value "disable_opt_anf_const_folding" with
                                            | Some b -> optFlags <- { optFlags with DisableANFConstFolding = b }
                                            | None -> ()
                                        | "disable_opt_anf_const_prop" ->
                                            match parseBool value "disable_opt_anf_const_prop" with
                                            | Some b -> optFlags <- { optFlags with DisableANFConstProp = b }
                                            | None -> ()
                                        | "disable_opt_anf_copy_prop" ->
                                            match parseBool value "disable_opt_anf_copy_prop" with
                                            | Some b -> optFlags <- { optFlags with DisableANFCopyProp = b }
                                            | None -> ()
                                        | "disable_opt_anf_dce" ->
                                            match parseBool value "disable_opt_anf_dce" with
                                            | Some b -> optFlags <- { optFlags with DisableANFDCE = b }
                                            | None -> ()
                                        | "disable_opt_anf_strength_reduction" ->
                                            match parseBool value "disable_opt_anf_strength_reduction" with
                                            | Some b -> optFlags <- { optFlags with DisableANFStrengthReduction = b }
                                            | None -> ()
                                        | "disable_opt_inline" ->
                                            match parseBool value "disable_opt_inline" with
                                            | Some b -> optFlags <- { optFlags with DisableInlining = b }
                                            | None -> ()
                                        | "disable_opt_tco" ->
                                            match parseBool value "disable_opt_tco" with
                                            | Some b -> optFlags <- { optFlags with DisableTCO = b }
                                            | None -> ()
                                        | "disable_opt_mir" ->
                                            match parseBool value "disable_opt_mir" with
                                            | Some b -> optFlags <- { optFlags with DisableMIROpt = b }
                                            | None -> ()
                                        | "disable_opt_mir_const_folding" ->
                                            match parseBool value "disable_opt_mir_const_folding" with
                                            | Some b -> optFlags <- { optFlags with DisableMIRConstFolding = b }
                                            | None -> ()
                                        | "disable_opt_mir_cse" ->
                                            match parseBool value "disable_opt_mir_cse" with
                                            | Some b -> optFlags <- { optFlags with DisableMIRCSE = b }
                                            | None -> ()
                                        | "disable_opt_mir_copy_prop" ->
                                            match parseBool value "disable_opt_mir_copy_prop" with
                                            | Some b -> optFlags <- { optFlags with DisableMIRCopyProp = b }
                                            | None -> ()
                                        | "disable_opt_mir_dce" ->
                                            match parseBool value "disable_opt_mir_dce" with
                                            | Some b -> optFlags <- { optFlags with DisableMIRDCE = b }
                                            | None -> ()
                                        | "disable_opt_mir_cfg_simplify" ->
                                            match parseBool value "disable_opt_mir_cfg_simplify" with
                                            | Some b -> optFlags <- { optFlags with DisableMIRCFGSimplify = b }
                                            | None -> ()
                                        | "disable_opt_mir_licm" ->
                                            match parseBool value "disable_opt_mir_licm" with
                                            | Some b -> optFlags <- { optFlags with DisableMIRLICM = b }
                                            | None -> ()
                                        | "disable_opt_lir" ->
                                            match parseBool value "disable_opt_lir" with
                                            | Some b -> optFlags <- { optFlags with DisableLIROpt = b }
                                            | None -> ()
                                        | "disable_opt_lir_peephole" ->
                                            match parseBool value "disable_opt_lir_peephole" with
                                            | Some b -> optFlags <- { optFlags with DisableLIRPeephole = b }
                                            | None -> ()
                                        | "disable_opt_dce" ->
                                            match parseBool value "disable_opt_dce" with
                                            | Some b -> optFlags <- { optFlags with DisableFunctionTreeShaking = b }
                                            | None -> ()
                                        | "disable_opt_function_tree_shaking" ->
                                            match parseBool value "disable_opt_function_tree_shaking" with
                                            | Some b -> optFlags <- { optFlags with DisableFunctionTreeShaking = b }
                                            | None -> ()
                                        | "disable_leak_check" ->
                                            match parseBool value "disable_leak_check" with
                                            | Some b -> optFlags <- { optFlags with DisableLeakCheck = b }
                                            | None -> ()
                                        | "stdin" ->
                                            if value = "closed" then
                                                optFlags <- { optFlags with Stdin = Closed }
                                            else
                                                match parseStringLiteral value with
                                                | Ok input -> optFlags <- { optFlags with Stdin = Bytes input }
                                                | Error e -> errors <- $"Invalid stdin: {e}" :: errors
                                        | "exact_bytes" ->
                                            match parseBool value "exact_bytes" with
                                            | Some true -> optFlags <- { optFlags with OutputMatch = ExactBytes }
                                            | Some false -> optFlags <- { optFlags with OutputMatch = NormalizedText }
                                            | None -> ()
                                        | _ -> errors <- $"Unknown attribute: {key}" :: errors
                                    | Error e -> errors <- e :: errors

                            if errors.Length > 0 then
                                Error (String.concat "; " (List.rev errors))
                            elif leadingText.IsSome && not sawRuntimeExpectationAttribute then
                                // Value expectation with trailing compiler flags, e.g.:
                                // `... = 5L disable_opt_anf=true`
                                Ok (leadingText, exitCode, None, None, optFlags, false, None, skipReason)
                            else
                                let leadingStdoutResult =
                                    match leadingText with
                                    | None -> Ok None
                                    | Some text -> parseSimpleStdout text |> Result.map Some

                                let leadingStdout =
                                    match leadingStdoutResult with
                                    | Ok value -> value
                                    | Error e ->
                                        errors <- e :: errors
                                        None

                                if errors.Length > 0 then
                                    Error (String.concat "; " (List.rev errors))
                                else
                                    let combinedStdout =
                                        match leadingStdout, stdout with
                                        | Some _, Some _ ->
                                            Error "Cannot combine bare stdout with stdout= attribute. Use one or the other."
                                        | Some value, None -> Ok (Some value)
                                        | None, Some value -> Ok (Some value)
                                        | None, None -> Ok None

                                    match combinedStdout with
                                    | Ok finalStdout ->
                                        Ok (None, exitCode, finalStdout, stderr, optFlags, false, None, skipReason)
                                    | Error e -> Error e

        match parseExpectations expectationsStr with
        | Ok (expectedValueExpr, exitCode, stdout, stderr, optFlags, expectError, errorMessage, skipReason) ->
            let displayName = comment |> Option.defaultValue source
            Ok {
                Name = $"L{lineNumber}: {displayName}"
                Source = source
                ExpectedValueExpr = expectedValueExpr
                Preamble = preamble
                ExpectedStdout = stdout
                ExpectedStderr = stderr
                Stdin = optFlags.Stdin
                OutputMatch = optFlags.OutputMatch
                ExpectedExitCode = exitCode
                ExpectCompileError = expectError
                ExpectedErrorMessage = errorMessage
                SkipReason = skipReason
                DisableFreeList = optFlags.DisableFreeList
                DisableANFOpt = optFlags.DisableANFOpt
                DisableANFConstFolding = optFlags.DisableANFConstFolding
                DisableANFConstProp = optFlags.DisableANFConstProp
                DisableANFCopyProp = optFlags.DisableANFCopyProp
                DisableANFDCE = optFlags.DisableANFDCE
                DisableANFStrengthReduction = optFlags.DisableANFStrengthReduction
                DisableInlining = optFlags.DisableInlining
                DisableTCO = optFlags.DisableTCO
                DisableMIROpt = optFlags.DisableMIROpt
                DisableMIRConstFolding = optFlags.DisableMIRConstFolding
                DisableMIRCSE = optFlags.DisableMIRCSE
                DisableMIRCopyProp = optFlags.DisableMIRCopyProp
                DisableMIRDCE = optFlags.DisableMIRDCE
                DisableMIRCFGSimplify = optFlags.DisableMIRCFGSimplify
                DisableMIRLICM = optFlags.DisableMIRLICM
                DisableLIROpt = optFlags.DisableLIROpt
                DisableLIRPeephole = optFlags.DisableLIRPeephole
                DisableFunctionTreeShaking = optFlags.DisableFunctionTreeShaking
                DisableLeakCheck = optFlags.DisableLeakCheck
                SourceFile = filePath
                FunctionLineMap = funcLineMap
            }
        | Error e ->
            Error $"Line {lineNumber}: {e}"

/// Parse a multi-line test expression wrapped in parens
/// Format: (expr...\n...) = expectations
let private parseMultilineTest (fullText: string) (startLineNumber: int) (filePath: string) (preamble: string) (funcLineMap: Map<string, int>) : Result<E2ETest, string> =
    let trimmed = fullText.TrimStart()
    if not (trimmed.StartsWith("(")) then
        // Non-parenthesized multiline expressions should be parsed directly
        // using the general separator logic.
        parseTestLineWithPreamble fullText startLineNumber filePath preamble funcLineMap
    else
        // Find the matching close paren for the first opening paren.
        // We only strip "outer parens" when this match is also the separator-close
        // before "= expectations". Otherwise, parse directly to preserve expressions like:
        // (a) + (b) = ...
        let findOuterWrappingCloseParen (text: string) : int option =
            let firstOpen = text.IndexOf('(')
            if firstOpen < 0 then
                None
            else
                let rec loop (i: int) (inQuotes: bool) (depth: int) : int option =
                    if i >= text.Length then
                        None
                    elif inQuotes && text.[i] = '\\' && i + 1 < text.Length then
                        loop (i + 2) inQuotes depth
                    elif text.[i] = '"' then
                        loop (i + 1) (not inQuotes) depth
                    elif inQuotes then
                        loop (i + 1) inQuotes depth
                    elif text.[i] = '(' then
                        loop (i + 1) inQuotes (depth + 1)
                    elif text.[i] = ')' then
                        let newDepth = depth - 1
                        if newDepth = 0 then
                            Some i
                        else
                            loop (i + 1) inQuotes newDepth
                    else
                        loop (i + 1) inQuotes depth
                loop firstOpen false 0

        // Find the `) = <expectation>` pattern that closes the expression
        // We need to find the LAST ) that's followed by ` = <expectation>`
        let rec findClosingParen (i: int) (inQuotes: bool) (lastMatch: int option) : int option =
            if i >= fullText.Length then lastMatch
            elif fullText.[i] = '"' then findClosingParen (i + 1) (not inQuotes) lastMatch
            elif fullText.[i] = ')' && not inQuotes then
                let rest = fullText.Substring(i + 1).TrimStart()
                if rest.StartsWith("=") then
                    let afterEq = rest.Substring(1)
                    if isExpectationCandidate afterEq then
                        findClosingParen (i + 1) inQuotes (Some i)
                    else
                        findClosingParen (i + 1) inQuotes lastMatch
                else
                    findClosingParen (i + 1) inQuotes lastMatch
            else
                findClosingParen (i + 1) inQuotes lastMatch

        match findClosingParen 0 false None with
        | None ->
            // Support multiline forms where the separator appears after
            // additional continuation lines (for example newline pipe forms),
            // not immediately after a closing parenthesis.
            parseTestLineWithPreamble fullText startLineNumber filePath preamble funcLineMap
        | Some closeParenIdx ->
            match findOuterWrappingCloseParen fullText with
            | Some outerClose when outerClose = closeParenIdx ->
                // Extract expression (strip outer parens)
                let exprStart = fullText.IndexOf('(')
                if exprStart < 0 then
                    Error $"Line {startLineNumber}: Multi-line expression missing opening '('"
                else
                    let exprContent = fullText.Substring(exprStart + 1, closeParenIdx - exprStart - 1).Trim()
                    let afterCloseParen = fullText.Substring(closeParenIdx + 1).TrimStart()
                    // afterCloseParen should start with "= expectations"
                    let expectationsStr =
                        if afterCloseParen.StartsWith("=") then afterCloseParen.Substring(1).Trim()
                        else afterCloseParen

                    // Reconstruct as single-line format for parsing
                    // Note: The expression itself might contain = signs, but that's fine since
                    // we've already extracted the expectations part
                    let reconstructed = exprContent + " = " + expectationsStr
                    parseTestLineWithPreamble reconstructed startLineNumber filePath preamble funcLineMap
            | _ ->
                // Parentheses are part of internal expression structure, not an outer wrapper.
                parseTestLineWithPreamble fullText startLineNumber filePath preamble funcLineMap

/// Remove trailing comment from a line
let private stripComment (line: string) : string =
    match findCommentStartOutsideQuotes line with
    | Some commentIdx -> line.Substring(0, commentIdx).Trim()
    | None -> line.Trim()

/// Parse all E2E tests from a single file
/// Supports preamble definitions that are prepended to all tests.
/// Lines that don't match the test format (expr = expectation) are treated as
/// definitions and collected across the file, then prepended to every test.
/// Also supports multi-line test expressions wrapped in parens:
///   (expr
///    continuation...) = expectations
let parseE2ETestFile (path: string) : Result<E2ETest list, string> =
    if not (System.IO.File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let lines = System.IO.File.ReadAllLines(path)
        let allowIndentedTests = path.EndsWith(".dark", StringComparison.OrdinalIgnoreCase)

        let mutable tests = []
        let mutable errors = []
        let mutable preambleLines = []
        // Track function definitions for caching: funcName -> line number
        let mutable functionLineMap : Map<string, int> = Map.empty
        // Multi-line expression state
        let mutable pendingExprLines : string list = []
        let mutable pendingStartLine = 0
        let mutable pendingExprIndentShift = 0
        let mutable inMultilineExpr = false
        let mutable moduleIndentStack : int list = []
        let mutable skipUntilIndex = -1

        let countLeadingSpaces (lineText: string) : int =
            let mutable idx = 0
            while idx < lineText.Length && lineText.[idx] = ' ' do
                idx <- idx + 1
            idx

        let trimLeadingSpaces (n: int) (lineText: string) : string =
            if n <= 0 then
                lineText
            else
                let mutable idx = 0
                let mutable remaining = n
                while idx < lineText.Length && remaining > 0 && lineText.[idx] = ' ' do
                    idx <- idx + 1
                    remaining <- remaining - 1
                lineText.Substring(idx)

        let hasIndentedIdentifierHeadContinuation (rhs: string) (nextIndex: int) : bool =
            if not allowIndentedTests then
                false
            elif not (isIdentifierPathHead rhs) then
                false
            elif nextIndex >= lines.Length then
                false
            else
                let nextLine = lines.[nextIndex]
                let nextTrimmed = nextLine.Trim()
                if nextTrimmed.Length = 0 || nextTrimmed.StartsWith("//") then
                    false
                elif nextLine.Length = 0 || not (Char.IsWhiteSpace(nextLine.[0])) then
                    false
                else
                    let nextWithoutComment = stripComment nextTrimmed
                    let nextStartsDefinition =
                        nextWithoutComment.StartsWith("def ")
                        || nextWithoutComment.StartsWith("type ")
                        || nextWithoutComment.StartsWith("let ")
                        || nextWithoutComment.StartsWith("module ")
                        || nextWithoutComment.StartsWith("[<")
                    not nextStartsDefinition
                    && not (isTestLine nextWithoutComment)

        let hasCompleteSeparator (text: string) : bool =
            match findSeparatorIndex text with
            | Some sepIdx ->
                let rhs = text.Substring(sepIdx + 1)
                not (isIncompleteExpectationHead rhs)
            | None ->
                false

        for i in 0 .. lines.Length - 1 do
            if i <= skipUntilIndex then
                ()
            else
                let line = lines.[i]
                let trimmedLine = line.Trim()
                let lineNumber = i + 1

                // Skip blank lines and comment-only lines
                if trimmedLine.Length > 0 && not (trimmedLine.StartsWith("//")) then
                    if inMultilineExpr then
                        // Accumulating multi-line expression
                        let normalizedLine =
                            if allowIndentedTests then
                                trimLeadingSpaces pendingExprIndentShift line
                            else
                                line
                        pendingExprLines <- normalizedLine :: pendingExprLines

                        // Check whether the accumulated expression now has a closing
                        // ') = <expectation>' pattern. This supports expectations that
                        // appear on the next line after the '=' token.
                        let accumulatedWithoutComments =
                            pendingExprLines
                            |> List.rev
                            |> List.map stripComment
                            |> String.concat "\n"
                        let hasClosingParen = hasClosingParenTest accumulatedWithoutComments
                        let hasCompleteTopLevelSeparator = hasCompleteSeparator accumulatedWithoutComments
                        let hasIdentifierHeadContinuation =
                            match findSeparatorIndex accumulatedWithoutComments with
                            | Some sepIdx ->
                                let rhs = accumulatedWithoutComments.Substring(sepIdx + 1)
                                hasIndentedIdentifierHeadContinuation rhs (i + 1)
                            | None ->
                                false

                        if (hasClosingParen || hasCompleteTopLevelSeparator) && not hasIdentifierHeadContinuation then
                            // Combine all accumulated lines and parse as multi-line test
                            let fullExpr = String.concat "\n" (List.rev pendingExprLines)
                            let preamble = String.concat "\n" (List.rev preambleLines)
                            match parseMultilineTest fullExpr pendingStartLine path preamble functionLineMap with
                            | Ok test ->
                                tests <- test :: tests
                                // Reset multi-line state
                                pendingExprLines <- []
                                pendingExprIndentShift <- 0
                                inMultilineExpr <- false
                            | Error err when hasCompleteTopLevelSeparator && not hasClosingParen ->
                                // A top-level-separator guess can be a false positive when
                                // multiline source still needs additional continuation lines.
                                ()
                            | Error err ->
                                errors <- err :: errors
                                // Reset multi-line state after a hard parse error.
                                pendingExprLines <- []
                                pendingExprIndentShift <- 0
                                inMultilineExpr <- false
                        // else: continue accumulating
                    else
                        let lineWithoutComment = stripComment trimmedLine
                        let lineIndent = countLeadingSpaces line
                        let rec popCompletedModules (stack: int list) : int list =
                            match stack with
                            // When indentation returns to or above a module declaration's
                            // indent level, that module block has ended.
                            | top :: rest when lineIndent <= top -> popCompletedModules rest
                            | _ -> stack
                        moduleIndentStack <- popCompletedModules moduleIndentStack

                        // Check if this starts a multi-line expression: starts with ( but isn't a complete test
                        let isTopLevel = line.Length > 0 && not (Char.IsWhiteSpace(line.[0]))
                        let moduleShift = moduleIndentStack.Length * 2
                        let isModuleLevelIndented = allowIndentedTests && lineIndent = moduleShift
                        let mayStartOrBeTest = isTopLevel || isModuleLevelIndented
                        let isDefinitionStart =
                            lineWithoutComment.StartsWith("def ")
                            || lineWithoutComment.StartsWith("type ")
                            || lineWithoutComment.StartsWith("let ")
                            || lineWithoutComment.StartsWith("module ")
                            || lineWithoutComment.StartsWith("[<")

                        let shouldStartMultilineExpr =
                            mayStartOrBeTest
                            && not (isTestLine lineWithoutComment)
                            && not isDefinitionStart

                        if shouldStartMultilineExpr then
                            // Start accumulating multi-line expression
                            pendingExprIndentShift <- if allowIndentedTests then moduleShift else 0
                            let normalizedStartLine =
                                if allowIndentedTests then
                                    trimLeadingSpaces pendingExprIndentShift line
                                else
                                    line
                            pendingExprLines <- [normalizedStartLine]
                            pendingStartLine <- lineNumber
                            inMultilineExpr <- true
                        elif mayStartOrBeTest && isTestLine lineWithoutComment then
                            // This is a single-line test - parse it with accumulated preamble
                            let preamble = String.concat "\n" (List.rev preambleLines)
                            let hasIncompleteExpectationHead =
                                if allowIndentedTests then
                                    match findSeparatorIndex lineWithoutComment with
                                    | Some sepIdx ->
                                        let rhs = lineWithoutComment.Substring(sepIdx + 1)
                                        let baseIncomplete = isIncompleteExpectationHead rhs
                                        let hasIdentifierHeadContinuation =
                                            if baseIncomplete then
                                                false
                                            else
                                                hasIndentedIdentifierHeadContinuation rhs (i + 1)
                                        baseIncomplete || hasIdentifierHeadContinuation
                                    | None -> false
                                else
                                    false

                            if hasIncompleteExpectationHead then
                                let rec collectContinuation (j: int) (acc: string list) : string list * int =
                                    if j >= lines.Length then
                                        (List.rev acc, j)
                                    else
                                        let nextLine = lines.[j]
                                        let nextTrimmed = nextLine.Trim()
                                        if nextTrimmed.Length = 0 || nextTrimmed.StartsWith("//") then
                                            (List.rev acc, j)
                                        else
                                            let nextIsIndented = nextLine.Length > 0 && Char.IsWhiteSpace(nextLine.[0])
                                            let nextWithoutComment = stripComment nextTrimmed
                                            let nextStartsDefinition =
                                                nextWithoutComment.StartsWith("def ")
                                                || nextWithoutComment.StartsWith("type ")
                                                || nextWithoutComment.StartsWith("let ")
                                                || nextWithoutComment.StartsWith("module ")
                                                || nextWithoutComment.StartsWith("[<")
                                            let needsDelimiterContinuation =
                                                let currentAccumulatedWithoutComments =
                                                    line :: (List.rev acc)
                                                    |> List.map stripComment
                                                    |> String.concat "\n"
                                                hasUnclosedDelimiters currentAccumulatedWithoutComments
                                            if nextIsIndented
                                               && not nextStartsDefinition
                                               && (needsDelimiterContinuation || not (isTestLine nextWithoutComment)) then
                                                collectContinuation (j + 1) (nextLine :: acc)
                                            else
                                                (List.rev acc, j)

                                let (continuationLines, nextIndex) = collectContinuation (i + 1) []
                                if not (List.isEmpty continuationLines) then
                                    let fullExpr = String.concat "\n" (line :: continuationLines)
                                    match parseMultilineTest fullExpr lineNumber path preamble functionLineMap with
                                    | Ok test -> tests <- test :: tests
                                    | Error err -> errors <- err :: errors
                                    skipUntilIndex <- nextIndex - 1
                                else
                                    match parseTestLineWithPreamble trimmedLine lineNumber path preamble functionLineMap with
                                    | Ok test -> tests <- test :: tests
                                    | Error err -> errors <- err :: errors
                            else
                                match parseTestLineWithPreamble trimmedLine lineNumber path preamble functionLineMap with
                                | Ok test -> tests <- test :: tests
                                | Error err -> errors <- err :: errors
                        else
                            let isDarkModuleDecl =
                                allowIndentedTests
                                && (trimmedLine.StartsWith("module ") || trimmedLine.StartsWith("module\t"))

                            if isDarkModuleDecl then
                                let moduleIndent = countLeadingSpaces line
                                let rec popToParent (stack: int list) : int list =
                                    match stack with
                                    | top :: rest when moduleIndent <= top -> popToParent rest
                                    | _ -> stack
                                moduleIndentStack <- moduleIndent :: popToParent moduleIndentStack
                            else
                            // This is a definition line - add to preamble
                            // Preserve original indentation for multi-line definitions
                                let normalizedLine =
                                    if allowIndentedTests then
                                        trimLeadingSpaces moduleShift line
                                    else
                                        line
                                preambleLines <- normalizedLine :: preambleLines
                                // Track function definitions for caching
                                match extractFuncName normalizedLine with
                                | Some funcName -> functionLineMap <- Map.add funcName lineNumber functionLineMap
                                | None -> ()

        // Check for unclosed multi-line expression
        if inMultilineExpr then
            errors <- $"Line {pendingStartLine}: Unclosed multi-line expression (missing ') = <expectation>')" :: errors

        if errors.Length > 0 then
            Error (String.concat "\n" (List.rev errors))
        else
            let fullPreamble = String.concat "\n" (List.rev preambleLines)
            let fullFunctionLineMap = functionLineMap
            let normalizedPath = path.Replace('\\', '/')
            let keepPerTestPreamble =
                allowIndentedTests
                && normalizedPath.Contains("/e2e/upstream/")
            let normalizedTests =
                tests
                |> List.rev
                |> List.map (fun test ->
                    if keepPerTestPreamble then
                        test
                    else
                        { test with
                            Preamble = fullPreamble
                            FunctionLineMap = fullFunctionLineMap })
            Ok normalizedTests

/// Parse E2E test from old-format file (for backward compatibility during transition)
let parseE2ETest (path: string) : Result<E2ETest, string> =
    Error "Old format no longer supported - use parseE2ETestFile instead"
