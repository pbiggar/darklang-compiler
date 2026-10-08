(*
E2EFormat.ml - End-to-end test format parser

Parses E2E test files in a simple line-based format.

Format: source = expectation  // optional comment

Example:
// Addition tests
2 + 3 = 5
1 + 2 + 3 = 10  // left and right sides must evaluate to equal values
Input supplied to the native process. Tests must choose between an already
closed stream and a finite byte sequence whose stream closes after delivery.
Output comparison policy. Presentation tests use ExactBytes so control
bytes, whitespace, and the absence of a final newline remain observable.
Whether an error expectation accepts any language failure or specifically
requires rejection before a native program is executed.
End-to-end test specification
The test expression only (NOT including preamble)
Expected value expression on the right-hand side of `=`.
When set, the runner compares Source and ExpectedValueExpr for value equality.
Preamble code (function/type definitions shared across tests in file)
Positional command-line arguments supplied to the native process.
Environment variables overridden for the native process.
Run this test in its own executable instead of a shared E2E batch.
Expected diagnostic substring when ErrorExpectation is present.
If set, test is skipped with this reason
Select the experimental native pipeline for this runner invocation.
Compiler options for disabling optimizations
Source file this test came from (for grouping in output)
Maps function names to their definition line numbers in the source file
Used for caching compiled functions across tests
Optimization flags for test parsing (internal type)
Extract function name from a definition line (e.g., "def buildTree(...)" -> "buildTree")
Parse string literal with escape sequences (\n, \t, \\, \')
Parse triple-quoted string literal """..."""
Parse either a regular quoted string or a triple-quoted string
Remove simple outer wrapping parens repeatedly: ((x)) -> x
Parse Builtin.testDerrorMessage / Builtin.testDerrorSqlMessage expectation helpers.
Returns Some result when expression matches either helper name.
Parse key=value attribute
Split string by spaces, respecting quoted strings with escape sequences
e.g., 'stdout="hello world" exit=0' -> ['stdout="hello world"'; 'exit=0']
Find start index of a `//` comment outside quoted strings.
Check if string starts with expectation keywords (digit, -, stdout, etc.)
Check if line has ) followed by = <expectation> (for multi-line expression closing)
Find the = that separates source from expectations
Returns Some index if this looks like a test line, along with how many matches were seen
Only consider this = as a separator if preceded by whitespace or )
This distinguishes "source = expectations" from "exit=139"
Check if this = is followed by an expectation
Check if a line (with comment removed) looks like a test line
Parse a single test line with optional preamble to prepend
Supports two formats:
Value equality: source = expectedValueExpr
Explicit attributes: source = [exit=N] [stdout="..."] [stderr="..."]
First, remove any comment
Source is just the test expression - preamble is stored separately
Parse expectations:
- error / error="..."
- skip / skip="..."
- Builtin.testDerrorMessage / Builtin.testDerrorSqlMessage
- explicit attributes (exit=, stdout=, stderr=, optimization flags)
- otherwise: RHS is an expression to compare with Source by value

Returns:
(expectedValueExpr, exitCode, stdout, stderr, optFlags, errorExpectation, errorMessage, skipReason)
The legacy `error` form accepts either compiler rejection or a
runtime language failure. Use `compileerror` to pin the phase.
Supports: error  or  error="message"
Expect a language failure with exit code 1 and no specific message.
error="message" format, optionally followed by attributes
Skip "error="
Legacy runtime SQL error shorthand with no specific message.
Legacy runtime SQL error shorthand:
source = sqlerror="message"
Skip "sqlerror="
Explicit-attribute format (exit/stdout/stderr/optimization flags),
with optional leading bare stdout for backward compatibility.
If no attributes are present, treat RHS as a value expression.
default
Helper to parse boolean attribute value
Split by spaces, respecting quoted strings with escape sequences
Bare RHS is always treated as a value expression.
Value expectation with trailing compiler flags, e.g.:
`... = 5L disable_opt_anf=true`
Parse a multi-line test expression wrapped in parens
Format: (expr...\n...) = expectations
Non-parenthesized multiline expressions should be parsed directly
using the general separator logic.
Find the matching close paren for the first opening paren.
We only strip "outer parens" when this match is also the separator-close
before "= expectations". Otherwise, parse directly to preserve expressions like:
(a) + (b) = ...
Find the `) = <expectation>` pattern that closes the expression
We need to find the LAST ) that's followed by ` = <expectation>`
Support multiline forms where the separator appears after
additional continuation lines (for example newline pipe forms),
not immediately after a closing parenthesis.
Extract expression (strip outer parens)
afterCloseParen should start with "= expectations"
Reconstruct as single-line format for parsing
Note: The expression itself might contain = signs, but that's fine since
we've already extracted the expectations part
Parentheses are part of internal expression structure, not an outer wrapper.
Remove trailing comment from a line
Parse all E2E tests from a single file
Supports preamble definitions that are prepended to all tests.
Lines that don't match the test format (expr = expectation) are treated as
definitions and collected across the file, then prepended to every test.
Also supports multi-line test expressions wrapped in parens:
(expr
continuation...) = expectations
Track function definitions for caching: funcName -> line number
Multi-line expression state
Skip blank lines and comment-only lines
Accumulating multi-line expression
Check whether the accumulated expression now has a closing
') = <expectation>' pattern. This supports expectations that
appear on the next line after the '=' token.
Combine all accumulated lines and parse as multi-line test
Reset multi-line state
A top-level-separator guess can be a false positive when
multiline source still needs additional continuation lines.
Reset multi-line state after a hard parse error.
else: continue accumulating
When indentation returns to or above a module declaration's
indent level, restore the enclosing module's definitions.
Check if this starts a multi-line expression: starts with ( but isn't a complete test
Start accumulating multi-line expression
This is a single-line test - parse it with accumulated preamble
This is a definition line - add to preamble
Preserve original indentation for multi-line definitions
Track function definitions for caching
Check for unclosed multi-line expression
Parse E2E test from old-format file (for backward compatibility during transition)
*)
(* E2EFormat.ml - Parse unchanged source, expectations, options and scoped preambles. *)
[@@@warning "-4-42"]

open Dark_compiler
module StringMap = Map.Make (StringOrder)

type testStdin = Closed | Bytes of string
type outputMatch = NormalizedText | ExactBytes
type errorExpectation = AnyError | CompileError

type e2eTest = {
  name : string;
  sourceLine : int;
  source : string;
  expectedValueExpr : string option;
  preamble : string;
  expectedStdout : string option;
  expectedStderr : string option;
  arguments : string list;
  environment : (string * string) list;
  stdin : testStdin;
  outputMatch : outputMatch;
  isolated : bool;
  expectedExitCode : int;
  errorExpectation : errorExpectation option;
  expectedErrorMessage : string option;
  skipReason : string option;
  disableFreeList : bool;
  disableANFOpt : bool;
  disableANFConstFolding : bool;
  disableANFConstProp : bool;
  disableANFCopyProp : bool;
  disableANFDCE : bool;
  disableANFStrengthReduction : bool;
  disableInlining : bool;
  disableTCO : bool;
  disableMIROpt : bool;
  disableMIRSCCP : bool;
  disableMIRCSE : bool;
  disableMIRDCE : bool;
  disableMIRLICM : bool;
  disableLIROpt : bool;
  disableLIRPeephole : bool;
  disableFunctionTreeShaking : bool;
  disableLeakCheck : bool;
  sourceFile : string;
  functionLineMap : int StringMap.t;
}

type optFlags = {
  disableFreeList : bool;
  disableANFOpt : bool;
  disableANFConstFolding : bool;
  disableANFConstProp : bool;
  disableANFCopyProp : bool;
  disableANFDCE : bool;
  disableANFStrengthReduction : bool;
  disableInlining : bool;
  disableTCO : bool;
  disableMIROpt : bool;
  disableMIRSCCP : bool;
  disableMIRCSE : bool;
  disableMIRDCE : bool;
  disableMIRLICM : bool;
  disableLIROpt : bool;
  disableLIRPeephole : bool;
  disableFunctionTreeShaking : bool;
  disableLeakCheck : bool;
  arguments : string list;
  environment : (string * string) list;
  stdin : testStdin;
  outputMatch : outputMatch;
  isolated : bool;
}

let defaultOptFlags : optFlags =
  {
    disableFreeList = false;
    disableANFOpt = false;
    disableANFConstFolding = false;
    disableANFConstProp = false;
    disableANFCopyProp = false;
    disableANFDCE = false;
    disableANFStrengthReduction = false;
    disableInlining = false;
    disableTCO = false;
    disableMIROpt = false;
    disableMIRSCCP = false;
    disableMIRCSE = false;
    disableMIRDCE = false;
    disableMIRLICM = false;
    disableLIROpt = false;
    disableLIRPeephole = false;
    disableFunctionTreeShaking = false;
    disableLeakCheck = false;
    arguments = [];
    environment = [];
    stdin = Closed;
    outputMatch = NormalizedText;
    isolated = false;
  }

let units = Text.scalars
let text = Text.ofScalars
let length s = Array.length (units s)
let slice s start count = text (Array.sub (units s) start count)
let tail s start = slice s start (length s - start)
let starts = Text.startsWith
let ends = Text.endsWith
let trim = Text.trim
let white u = Uchar.is_valid u && Uucp.White.is_white_space (Uchar.of_int u)

let trimStart s =
  let us = units s in
  let rec loop i =
    if i < Array.length us && white us.(i) then loop (i + 1) else i
  in
  let i = loop 0 in
  text (Array.sub us i (Array.length us - i))

let indexOfChar s c =
  let us = units s in
  let rec loop i =
    if i = Array.length us then None
    else if us.(i) = Char.code c then Some i
    else loop (i + 1)
  in
  loop 0

let extractFuncName line =
  let trimmed = trim line in
  if not (starts trimmed "def ") then None
  else
    let afterDef = trimStart (tail trimmed 4) in
    let us = units afterDef in
    let rec loop i =
      if i = Array.length us then None
      else if List.mem us.(i) [ 40; 60; 32 ] then
        if i > 0 then Some (slice afterDef 0 i) else None
      else loop (i + 1)
    in
    loop 0

let tryHex text =
  let us = units text in
  let asciiWhite u = u = 32 || (u >= 9 && u <= 13) in
  let n = Array.length us in
  let rec leading i =
    if i < n && asciiWhite us.(i) then leading (i + 1) else i
  in
  let rec trailing i =
    if i >= 0 && (asciiWhite us.(i) || us.(i) = 0) then trailing (i - 1) else i
  in
  let first = leading 0 and last = trailing (n - 1) in
  let rec number i value =
    if i > last then Some value
    else
      let u = us.(i) in
      let d =
        if u >= 48 && u <= 57 then u - 48
        else if u >= 65 && u <= 70 then u - 55
        else if u >= 97 && u <= 102 then u - 87
        else -1
      in
      if d < 0 || (value * 16) + d > 65535 then None
      else number (i + 1) ((value * 16) + d)
  in
  if first > last then None else number first 0

let parseStringLiteral s =
  if not (starts s "\"" && ends s "\"") then
    Error ("String literal must be quoted: " ^ s)
  else
    let content = units (slice s 1 (length s - 2)) in
    let rec loop i rev =
      if i >= Array.length content then text (Array.of_list (List.rev rev))
      else if content.(i) = 92 && i + 1 < Array.length content then
        let escaped = content.(i + 1) in
        match escaped with
        | 110 -> loop (i + 2) (10 :: rev)
        | 114 -> loop (i + 2) (13 :: rev)
        | 116 -> loop (i + 2) (9 :: rev)
        | 92 | 34 -> loop (i + 2) (escaped :: rev)
        | 117 when i + 5 < Array.length content -> (
            match tryHex (text (Array.sub content (i + 2) 4)) with
            | Some u -> loop (i + 6) (u :: rev)
            | None -> loop (i + 2) (117 :: 92 :: rev))
        | _ -> loop (i + 2) (escaped :: 92 :: rev)
      else loop (i + 1) (content.(i) :: rev)
    in
    Ok (loop 0 [])

let parseTripleQuotedStringLiteral s =
  if not (starts s "\"\"\"" && ends s "\"\"\"") then
    Error ("Triple-quoted string literal must be wrapped in \"\"\": " ^ s)
  else if length s < 6 then Error ("Invalid triple-quoted string literal: " ^ s)
  else Ok (slice s 3 (length s - 6))

let parseAnyStringLiteral s =
  let s = trim s in
  if starts s "\"\"\"" then parseTripleQuotedStringLiteral s
  else parseStringLiteral s

let rec stripOuterParens s =
  let s = trim s in
  if length s >= 2 && starts s "(" && ends s ")" then
    stripOuterParens (slice s 1 (length s - 2))
  else s

let tryParseBuiltinErrorExpectation exp =
  let normalized = stripOuterParens exp in
  let prefixes =
    [
      "Builtin.testDerrorMessage";
      "Stdlib.Builtin.testDerrorMessage";
      "Darklang.Stdlib.Builtin.testDerrorMessage";
      "Builtin.testDerrorSqlMessage";
      "Stdlib.Builtin.testDerrorSqlMessage";
      "Darklang.Stdlib.Builtin.testDerrorSqlMessage";
    ]
  in
  match List.find_opt (starts normalized) prefixes with
  | None -> None
  | Some prefix ->
      let argText = trim (tail normalized (length prefix)) in
      if length argText = 0 then Some (Ok None)
      else
        let arg =
          if starts argText "(" && ends argText ")" then
            trim (slice argText 1 (length argText - 2))
          else argText
        in
        Some (Result.map Option.some (parseAnyStringLiteral arg))

let parseAttribute attr =
  match indexOfChar attr '=' with
  | Some i -> Ok (trim (slice attr 0 i), trim (tail attr (i + 1)))
  | None -> Error ("Invalid attribute format: " ^ attr)

let splitBySpacesRespectingQuotes s =
  let us = units s in
  let token rev = text (Array.of_list (List.rev rev)) in
  let rec loop i quoted current tokens =
    if i >= Array.length us then
      List.rev (if current = [] then tokens else token current :: tokens)
    else if us.(i) = 92 && i + 1 < Array.length us then
      loop (i + 2) quoted (us.(i + 1) :: 92 :: current) tokens
    else if us.(i) = 34 then loop (i + 1) (not quoted) (34 :: current) tokens
    else if us.(i) = 32 && not quoted then
      loop (i + 1) quoted []
        (if current = [] then tokens else token current :: tokens)
    else loop (i + 1) quoted (us.(i) :: current) tokens
  in
  loop 0 false [] []

let findCommentStartOutsideQuotes line =
  let us = units line in
  let rec loop i quoted =
    if i >= Array.length us then None
    else if quoted && us.(i) = 92 && i + 1 < Array.length us then
      loop (i + 2) quoted
    else if us.(i) = 34 then loop (i + 1) (not quoted)
    else if
      (not quoted) && us.(i) = 47 && i + 1 < Array.length us && us.(i + 1) = 47
    then Some i
    else loop (i + 1) quoted
  in
  loop 0 false

let expectationPrefixes =
  [
    "exit";
    "stdout";
    "stderr";
    "skip";
    "no_free_list";
    "disable_leak_check";
    "stdin";
    "exact_bytes";
    "error";
    "disable_opt_";
  ]

let isExpectationStart rest =
  let s = trimStart rest in
  let us = units s in
  Array.length us > 0
  && (Text.isDigit us.(0)
     || List.mem us.(0) [ 45; 34; 39; 40; 91 ]
     || Text.isLetter us.(0)
     || List.exists (starts s) expectationPrefixes)

let stripQuotedContent s =
  let us = units s in
  let rec loop i quoted rev =
    if i >= Array.length us then text (Array.of_list (List.rev rev))
    else if us.(i) = 92 && i + 1 < Array.length us then
      loop (i + 2) quoted (if quoted then rev else us.(i + 1) :: 92 :: rev)
    else if us.(i) = 34 then loop (i + 1) (not quoted) (32 :: rev)
    else loop (i + 1) quoted (if quoted then rev else us.(i) :: rev)
  in
  loop 0 false []

let isExpectationCandidate rest =
  let s = trimStart rest in
  if not (isExpectationStart s) then false
  else if
    List.exists (starts s) ([ "\""; "'"; "compileerror" ] @ expectationPrefixes)
  then true
  else
    let lowered = Text.lowerInvariant (stripQuotedContent s) in
    not
      (List.exists
         (fun k ->
           starts lowered (k ^ " ") || Text.contains lowered (" " ^ k ^ " "))
         [ "let"; "val"; "if"; "match"; "then"; "else"; "type"; "def" ]
      || Text.contains lowered " in ")

let isAttributeKey = function
  | "exit" | "stdout" | "stderr" | "arg" | "env" | "skip" | "no_free_list"
  | "disable_leak_check" | "stdin" | "exact_bytes" | "isolated"
  | "disable_opt_freelist" | "disable_opt_anf" | "disable_opt_anf_const_folding"
  | "disable_opt_anf_const_prop" | "disable_opt_anf_copy_prop"
  | "disable_opt_anf_dce" | "disable_opt_anf_strength_reduction"
  | "disable_opt_inline" | "disable_opt_tco" | "disable_opt_mir"
  | "disable_opt_mir_sccp" | "disable_opt_mir_cse" | "disable_opt_mir_dce"
  | "disable_opt_mir_licm" | "disable_opt_lir" | "disable_opt_lir_peephole"
  | "disable_opt_dce" | "disable_opt_function_tree_shaking" ->
      true
  | _ -> false

let parseSimpleStdout value =
  let s = trim value in
  if length s = 0 then Error "Expected stdout value"
  else if starts s "\"" then
    Result.map (fun v -> v ^ "\n") (parseStringLiteral s)
  else Ok (s ^ "\n")

let hasUnclosedDelimiters s =
  let us = units s in
  let rec loop i quoted p b c =
    if i >= Array.length us then quoted || p > 0 || b > 0 || c > 0
    else if quoted && us.(i) = 92 && i + 1 < Array.length us then
      loop (i + 2) quoted p b c
    else if us.(i) = 34 then loop (i + 1) (not quoted) p b c
    else if quoted then loop (i + 1) quoted p b c
    else
      match us.(i) with
      | 40 -> loop (i + 1) quoted (p + 1) b c
      | 41 -> loop (i + 1) quoted (max 0 (p - 1)) b c
      | 91 -> loop (i + 1) quoted p (b + 1) c
      | 93 -> loop (i + 1) quoted p (max 0 (b - 1)) c
      | 123 -> loop (i + 1) quoted p b (c + 1)
      | 125 -> loop (i + 1) quoted p b (max 0 (c - 1))
      | _ -> loop (i + 1) quoted p b c
  in
  loop 0 false 0 0 0

let isIncompleteExpectationHead s =
  let s = trim s in
  length s = 0 || hasUnclosedDelimiters s

let isIdentifierPathHead s =
  let s = trim s in
  if length s = 0 then false
  else
    let us = units s in
    let rec finish i =
      if i = Array.length us || List.mem us.(i) [ 32; 9; 13; 10 ] then i
      else finish (i + 1)
    in
    Array.for_all
      (fun u -> Text.isLetter u || Text.isDigit u || u = 95 || u = 46)
      (Array.sub us 0 (finish 0))

let hasClosingParenTest s =
  let us = units s in
  let rec loop i quoted p b c =
    if i >= Array.length us then false
    else if quoted && us.(i) = 92 && i + 1 < Array.length us then
      loop (i + 2) quoted p b c
    else if us.(i) = 34 then loop (i + 1) (not quoted) p b c
    else if quoted then loop (i + 1) quoted p b c
    else
      match us.(i) with
      | 40 -> loop (i + 1) quoted (p + 1) b c
      | 41 ->
          let p = max 0 (p - 1) in
          let found =
            if p = 0 && b = 0 && c = 0 then
              let rest = trimStart (tail s (i + 1)) in
              starts rest "="
              &&
              let after = tail rest 1 in
              isExpectationCandidate after
              && not (isIncompleteExpectationHead after)
            else false
          in
          found || loop (i + 1) quoted p b c
      | 91 -> loop (i + 1) quoted p (b + 1) c
      | 93 -> loop (i + 1) quoted p (max 0 (b - 1)) c
      | 123 -> loop (i + 1) quoted p b (c + 1)
      | 125 -> loop (i + 1) quoted p b (max 0 (c - 1))
      | _ -> loop (i + 1) quoted p b c
  in
  loop 0 false 0 0 0

let findSeparatorIndexAndCount s =
  let us = units s in
  let rec loop i quoted p b c last count =
    if i >= Array.length us then (last, count)
    else if quoted && us.(i) = 92 && i + 1 < Array.length us then
      loop (i + 2) quoted p b c last count
    else if us.(i) = 34 then loop (i + 1) (not quoted) p b c last count
    else if quoted then loop (i + 1) quoted p b c last count
    else
      match us.(i) with
      | 40 -> loop (i + 1) quoted (p + 1) b c last count
      | 41 -> loop (i + 1) quoted (max 0 (p - 1)) b c last count
      | 91 -> loop (i + 1) quoted p (b + 1) c last count
      | 93 -> loop (i + 1) quoted p (max 0 (b - 1)) c last count
      | 123 -> loop (i + 1) quoted p b (c + 1) last count
      | 125 -> loop (i + 1) quoted p b (max 0 (c - 1)) last count
      | 61
        when p = 0 && b = 0 && c = 0 && i > 0
             && (white us.(i - 1) || us.(i - 1) = 41)
             && isExpectationCandidate (tail s (i + 1)) ->
          loop (i + 1) quoted p b c (Some i) (count + 1)
      | _ -> loop (i + 1) quoted p b c last count
  in
  loop 0 false 0 0 0 None 0

let countPotentialSeparators s =
  let us = units s in
  let rec loop i quoted p b c count =
    if i >= Array.length us then count
    else if quoted && us.(i) = 92 && i + 1 < Array.length us then
      loop (i + 2) quoted p b c count
    else if us.(i) = 34 then loop (i + 1) (not quoted) p b c count
    else if quoted then loop (i + 1) quoted p b c count
    else
      match us.(i) with
      | 40 -> loop (i + 1) quoted (p + 1) b c count
      | 41 -> loop (i + 1) quoted (max 0 (p - 1)) b c count
      | 91 -> loop (i + 1) quoted p (b + 1) c count
      | 93 -> loop (i + 1) quoted p (max 0 (b - 1)) c count
      | 123 -> loop (i + 1) quoted p b (c + 1) count
      | 125 -> loop (i + 1) quoted p b (max 0 (c - 1)) count
      | 61
        when p = 0 && b = 0 && c = 0 && i > 0
             && (white us.(i - 1) || us.(i - 1) = 41)
             && us.(i - 1) <> 61
             && (i + 1 = Array.length us || us.(i + 1) <> 61) ->
          loop (i + 1) quoted p b c (count + 1)
      | _ -> loop (i + 1) quoted p b c count
  in
  loop 0 false 0 0 0 0

let findSeparatorIndex s = fst (findSeparatorIndexAndCount s)

let isTestLine s =
  let trimmed = trimStart s in
  let definition =
    List.exists (starts trimmed) [ "def "; "type "; "let "; "val "; "[<" ]
  in
  match findSeparatorIndex s with
  | None -> false
  | Some _ -> not (definition && countPotentialSeparators s <= 1)

let lowerAscii s =
  String.map
    (fun c -> if c >= 'A' && c <= 'Z' then Char.chr (Char.code c + 32) else c)
    s

let parseExpectations exp =
  let trimmed = trim exp in
  let tokens = splitBySpacesRespectingQuotes trimmed in
  let errorFlags = function
    | [] -> Ok defaultOptFlags
    | token :: _ -> (
        match parseAttribute token with
        | Error e -> Error e
        | Ok (k, v) -> Error ("Unknown attribute: " ^ k ^ "=" ^ v))
  in
  let languageError kind prefix first rest offset =
    let message =
      if offset = 0 then Ok None
      else
        Result.map_error
          (fun e -> prefix ^ e)
          (Result.map Option.some (parseStringLiteral (tail first offset)))
    in
    Result.bind message (fun msg ->
        Result.map
          (fun flags -> (None, 1, None, None, flags, Some kind, msg, None))
          (errorFlags rest))
  in
  match tokens with
  | "error" :: rest -> languageError AnyError "" "" rest 0
  | first :: rest when String.starts_with ~prefix:"error=" (lowerAscii first) ->
      languageError AnyError "Invalid error message: " first rest 6
  | "compileerror" :: rest -> languageError CompileError "" "" rest 0
  | first :: rest
    when String.starts_with ~prefix:"compileerror=" (lowerAscii first) ->
      languageError CompileError "Invalid compile error message: " first rest 13
  | [ first ] when lowerAscii first = "sqlerror" ->
      Ok (None, 1, Some "", None, defaultOptFlags, None, None, None)
  | [ first ] when String.starts_with ~prefix:"sqlerror=" (lowerAscii first) ->
      Result.map_error
        (fun e -> "Invalid sqlerror message: " ^ e)
        (Result.map
           (fun msg ->
             (None, 1, Some "", Some msg, defaultOptFlags, None, None, None))
           (parseStringLiteral (tail first 9)))
  | _ -> (
      if Text.lowerInvariant trimmed = "skip" then
        Ok
          ( None,
            0,
            None,
            None,
            defaultOptFlags,
            None,
            None,
            Some "Skipped by test" )
      else if starts (Text.lowerInvariant trimmed) "skip=" then
        Result.map_error
          (fun e -> "Invalid skip reason: " ^ e)
          (Result.map
             (fun reason ->
               (None, 0, None, None, defaultOptFlags, None, None, Some reason))
             (parseStringLiteral (tail trimmed 5)))
      else
        match tryParseBuiltinErrorExpectation trimmed with
        | Some (Ok expected) ->
            Ok (None, 1, Some "", expected, defaultOptFlags, None, None, None)
        | Some (Error e) ->
            Error ("Invalid Builtin.testDerrorMessage expectation: " ^ e)
        | None -> (
            let exitCode = ref 0
            and stdout = ref None
            and stderr = ref None
            and flags = ref defaultOptFlags
            and skipReason = ref None
            and errors = ref [] in
            let error e = errors := e :: !errors in
            let parseBool value name =
              match Text.lowerInvariant value with
              | "true" | "1" -> Some true
              | "false" | "0" -> Some false
              | _ ->
                  error
                    ("Invalid " ^ name ^ " value: " ^ value
                   ^ " (expected true/false)");
                  None
            in
            let rec attrStart i = function
              | [] -> None
              | token :: rest -> (
                  match parseAttribute token with
                  | Ok (k, _) when isAttributeKey k -> Some i
                  | _ -> attrStart (i + 1) rest)
            in
            match attrStart 0 tokens with
            | None ->
                Ok
                  ( Some trimmed,
                    0,
                    None,
                    None,
                    defaultOptFlags,
                    None,
                    None,
                    None )
            | Some idx ->
                let leading = List.take idx tokens in
                let leadingText =
                  if leading = [] then None
                  else Some (String.concat " " leading)
                in
                let runtime = ref false in
                List.iter
                  (fun token ->
                    let attr = trim token in
                    if length attr > 0 then
                      match parseAttribute attr with
                      | Error e -> error e
                      | Ok (key, value) -> (
                          let stringValue prefix update =
                            match parseStringLiteral value with
                            | Ok s -> update s
                            | Error e -> error (prefix ^ e)
                          in
                          let boolean update =
                            match parseBool value key with
                            | Some b -> flags := update !flags b
                            | None -> ()
                          in
                          match key with
                          | "exit" -> (
                              runtime := true;
                              match Text.tryParseInt32 value with
                              | Some v -> exitCode := Int32.to_int v
                              | None -> error ("Invalid exit code: " ^ value))
                          | "stdout" ->
                              runtime := true;
                              stringValue "" (fun s -> stdout := Some s)
                          | "stderr" ->
                              runtime := true;
                              stringValue "" (fun s -> stderr := Some s)
                          | "arg" ->
                              stringValue "Invalid argument: " (fun s ->
                                  flags :=
                                    {
                                      !flags with
                                      arguments = !flags.arguments @ [ s ];
                                    })
                          | "env" ->
                              stringValue "Invalid environment override: "
                                (fun s ->
                                  match indexOfChar s '=' with
                                  | Some i when i > 0 ->
                                      flags :=
                                        {
                                          !flags with
                                          environment =
                                            !flags.environment
                                            @ [ (slice s 0 i, tail s (i + 1)) ];
                                        }
                                  | _ ->
                                      error
                                        "Environment override must be \
                                         NAME=value")
                          | "skip" ->
                              runtime := true;
                              stringValue "Invalid skip reason: " (fun s ->
                                  skipReason := Some s)
                          | "no_free_list" | "disable_opt_freelist" ->
                              boolean (fun f b ->
                                  { f with disableFreeList = b })
                          | "disable_opt_anf" ->
                              boolean (fun f b -> { f with disableANFOpt = b })
                          | "disable_opt_anf_const_folding" ->
                              boolean (fun f b ->
                                  { f with disableANFConstFolding = b })
                          | "disable_opt_anf_const_prop" ->
                              boolean (fun f b ->
                                  { f with disableANFConstProp = b })
                          | "disable_opt_anf_copy_prop" ->
                              boolean (fun f b ->
                                  { f with disableANFCopyProp = b })
                          | "disable_opt_anf_dce" ->
                              boolean (fun f b -> { f with disableANFDCE = b })
                          | "disable_opt_anf_strength_reduction" ->
                              boolean (fun f b ->
                                  { f with disableANFStrengthReduction = b })
                          | "disable_opt_inline" ->
                              boolean (fun f b ->
                                  { f with disableInlining = b })
                          | "disable_opt_tco" ->
                              boolean (fun f b -> { f with disableTCO = b })
                          | "disable_opt_mir" ->
                              boolean (fun f b -> { f with disableMIROpt = b })
                          | "disable_opt_mir_sccp" ->
                              boolean (fun f b -> { f with disableMIRSCCP = b })
                          | "disable_opt_mir_cse" ->
                              boolean (fun f b -> { f with disableMIRCSE = b })
                          | "disable_opt_mir_dce" ->
                              boolean (fun f b -> { f with disableMIRDCE = b })
                          | "disable_opt_mir_licm" ->
                              boolean (fun f b -> { f with disableMIRLICM = b })
                          | "disable_opt_lir" ->
                              boolean (fun f b -> { f with disableLIROpt = b })
                          | "disable_opt_lir_peephole" ->
                              boolean (fun f b ->
                                  { f with disableLIRPeephole = b })
                          | "disable_opt_dce"
                          | "disable_opt_function_tree_shaking" ->
                              boolean (fun f b ->
                                  { f with disableFunctionTreeShaking = b })
                          | "disable_leak_check" ->
                              boolean (fun f b ->
                                  { f with disableLeakCheck = b })
                          | "stdin" ->
                              if value = "closed" then
                                flags := { !flags with stdin = Closed }
                              else
                                stringValue "Invalid stdin: " (fun s ->
                                    flags := { !flags with stdin = Bytes s })
                          | "exact_bytes" ->
                              boolean (fun f b ->
                                  {
                                    f with
                                    outputMatch =
                                      (if b then ExactBytes else NormalizedText);
                                  })
                          | "isolated" ->
                              boolean (fun f b -> { f with isolated = b })
                          | _ -> error ("Unknown attribute: " ^ key)))
                  (List.drop idx tokens);
                if !errors <> [] then
                  Error (String.concat "; " (List.rev !errors))
                else if Option.is_some leadingText && not !runtime then
                  Ok
                    ( leadingText,
                      !exitCode,
                      None,
                      None,
                      !flags,
                      None,
                      None,
                      !skipReason )
                else
                  let leadingStdout =
                    match leadingText with
                    | None -> Ok None
                    | Some v -> Result.map Option.some (parseSimpleStdout v)
                  in
                  Result.bind leadingStdout (fun leading ->
                      match (leading, !stdout) with
                      | Some _, Some _ ->
                          Error
                            "Cannot combine bare stdout with stdout= \
                             attribute. Use one or the other."
                      | _ ->
                          Ok
                            ( None,
                              !exitCode,
                              (match leading with
                              | Some _ -> leading
                              | None -> !stdout),
                              !stderr,
                              !flags,
                              None,
                              None,
                              !skipReason ))))

let parseTestLineWithPreamble line lineNumber filePath preamble funcLineMap =
  let without, comment =
    match findCommentStartOutsideQuotes line with
    | Some i -> (trim (slice line 0 i), Some (trim (tail line (i + 2))))
    | None -> (line, None)
  in
  match findSeparatorIndex without with
  | None ->
      Error
        ("Line " ^ string_of_int lineNumber
       ^ ": Expected format 'source = expectations', got: " ^ line)
  | Some i -> (
      let source = trim (slice without 0 i) in
      let expectations = trim (tail without (i + 1)) in
      match parseExpectations expectations with
      | Error e -> Error ("Line " ^ string_of_int lineNumber ^ ": " ^ e)
      | Ok
          ( expectedValueExpr,
            expectedExitCode,
            expectedStdout,
            expectedStderr,
            flags,
            errorExpectation,
            expectedErrorMessage,
            skipReason ) ->
          let display = Option.value ~default:source comment in
          let expectedValueExpr =
            let raw = tail without (i + 1) in
            (* List continuations retain element columns. A hanging expected
               function head is parsed as its own expression by the fixture DSL. *)
            if Text.contains raw "\n" && starts (trimStart raw) "[" then
              let column =
                match
                  List.rev (String.split_on_char '\n' (slice without 0 (i + 1)))
                with
                | last :: _ -> length last
                | [] ->
                    Crash.crash "Splitting a source prefix must produce a line"
              in
              Option.map
                (fun _ -> String.make column ' ' ^ raw)
                expectedValueExpr
            else expectedValueExpr
          in
          Ok
            {
              name = "L" ^ string_of_int lineNumber ^ ": " ^ display;
              sourceLine = lineNumber;
              source;
              expectedValueExpr;
              preamble;
              expectedStdout;
              expectedStderr;
              arguments = flags.arguments;
              environment = flags.environment;
              stdin = flags.stdin;
              outputMatch = flags.outputMatch;
              isolated = flags.isolated;
              expectedExitCode;
              errorExpectation;
              expectedErrorMessage;
              skipReason;
              disableFreeList = flags.disableFreeList;
              disableANFOpt = flags.disableANFOpt;
              disableANFConstFolding = flags.disableANFConstFolding;
              disableANFConstProp = flags.disableANFConstProp;
              disableANFCopyProp = flags.disableANFCopyProp;
              disableANFDCE = flags.disableANFDCE;
              disableANFStrengthReduction = flags.disableANFStrengthReduction;
              disableInlining = flags.disableInlining;
              disableTCO = flags.disableTCO;
              disableMIROpt = flags.disableMIROpt;
              disableMIRSCCP = flags.disableMIRSCCP;
              disableMIRCSE = flags.disableMIRCSE;
              disableMIRDCE = flags.disableMIRDCE;
              disableMIRLICM = flags.disableMIRLICM;
              disableLIROpt = flags.disableLIROpt;
              disableLIRPeephole = flags.disableLIRPeephole;
              disableFunctionTreeShaking = flags.disableFunctionTreeShaking;
              disableLeakCheck = flags.disableLeakCheck;
              sourceFile = filePath;
              functionLineMap = funcLineMap;
            })

let parseMultilineTest fullText startLineNumber filePath preamble funcLineMap =
  let direct () =
    parseTestLineWithPreamble fullText startLineNumber filePath preamble
      funcLineMap
  in
  if not (starts (trimStart fullText) "(") then direct ()
  else
    let us = units fullText in
    let firstOpen = indexOfChar fullText '(' in
    let outerClose =
      match firstOpen with
      | None -> None
      | Some first ->
          let rec loop i quoted depth =
            if i >= Array.length us then None
            else if quoted && us.(i) = 92 && i + 1 < Array.length us then
              loop (i + 2) quoted depth
            else if us.(i) = 34 then loop (i + 1) (not quoted) depth
            else if quoted then loop (i + 1) quoted depth
            else if us.(i) = 40 then loop (i + 1) quoted (depth + 1)
            else if us.(i) = 41 then
              if depth - 1 = 0 then Some i else loop (i + 1) quoted (depth - 1)
            else loop (i + 1) quoted depth
          in
          loop first false 0
    in
    let rec closing i quoted last =
      if i >= Array.length us then last
      else if us.(i) = 34 then closing (i + 1) (not quoted) last
      else if us.(i) = 41 && not quoted then
        let rest = trimStart (tail fullText (i + 1)) in
        let last =
          if starts rest "=" && isExpectationCandidate (tail rest 1) then Some i
          else last
        in
        closing (i + 1) quoted last
      else closing (i + 1) quoted last
    in
    match (closing 0 false None, outerClose) with
    | Some close, Some outer when close = outer -> (
        match firstOpen with
        | None ->
            Error
              ("Line "
              ^ string_of_int startLineNumber
              ^ ": Multi-line expression missing opening '('")
        | Some first ->
            let inner = slice fullText (first + 1) (close - first - 1) in
            (* Legacy let fixtures delimit a source program with parentheses.
               Keep their opening column without turning declarations local.
               Other expressions retain their grouping, including tuples. *)
            let source =
              if starts (trimStart inner) "let " then " " ^ inner
              else slice fullText first (close - first + 1)
            in
            direct () |> Result.map (fun test -> { test with source }))
    | _ -> direct ()

let stripComment line =
  match findCommentStartOutsideQuotes line with
  | Some i -> trim (slice line 0 i)
  | None -> trim line

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let parseCompileErrorDirective lineNumber line =
  let s = trim line in
  if s = "#compileerror" then Ok None
  else if String.starts_with ~prefix:"#compileerror=" s then
    Result.map_error
      (fun e ->
        "Line " ^ string_of_int lineNumber
        ^ ": Invalid #compileerror directive: " ^ e)
      (Result.map Option.some (parseStringLiteral (tail s 14)))
  else
    Error
      ("Line " ^ string_of_int lineNumber ^ ": Invalid #compileerror directive")

let collectCompileErrorOverrides lines =
  let rec loop i overrides directives =
    if i >= Array.length lines then Ok (overrides, directives)
    else
      let s = trim lines.(i) in
      if not (String.starts_with ~prefix:"#compileerror" s) then
        loop (i + 1) overrides directives
      else if
        i + 1 >= Array.length lines
        ||
        let next = trim lines.(i + 1) in
        next = ""
        || String.starts_with ~prefix:"//" next
        || String.starts_with ~prefix:"#" next
      then
        Error
          ("Line "
          ^ string_of_int (i + 1)
          ^ ": #compileerror must immediately precede a test")
      else
        Result.bind
          (parseCompileErrorDirective (i + 1) s)
          (fun msg ->
            loop (i + 1)
              (IntMap.add (i + 2) msg overrides)
              (IntSet.add (i + 1) directives))
  in
  loop 0 IntMap.empty IntSet.empty

let readLines path =
  let s = FileIO.readText path in
  let n = String.length s in
  let rec loop start i rev =
    if i >= n then
      Array.of_list
        (List.rev
           (if start < n then String.sub s start (n - start) :: rev else rev))
    else if s.[i] = '\r' || s.[i] = '\n' then
      let next =
        if s.[i] = '\r' && i + 1 < n && s.[i + 1] = '\n' then i + 2 else i + 1
      in
      loop next next (String.sub s start (i - start) :: rev)
    else loop start (i + 1) rev
  in
  loop 0 0 []

let countLeadingSpaces s =
  let us = units s in
  let rec loop i =
    if i < Array.length us && us.(i) = 32 then loop (i + 1) else i
  in
  loop 0

let trimLeadingSpaces n s =
  if n <= 0 then s else tail s (min n (countLeadingSpaces s))

let definitionStart s =
  List.exists (starts s) [ "def "; "type "; "let "; "val "; "module "; "[<" ]

let parseE2ETestFile path =
  if not (FileIO.exists path) then Error ("Test file not found: " ^ path)
  else
    let raw = readLines path in
    let allowIndented = String.ends_with ~suffix:".dark" (lowerAscii path) in
    let overrides, directives, directiveErrors =
      match collectCompileErrorOverrides raw with
      | Ok (o, d) -> (o, d, [])
      | Error e -> (IntMap.empty, IntSet.empty, [ e ])
    in
    let lines =
      Array.mapi
        (fun i line -> if IntSet.mem (i + 1) directives then "" else line)
        raw
    in
    let tests = ref []
    and errors = ref directiveErrors
    and preambleLines = ref []
    and functionLineMap = ref StringMap.empty in
    let pending = ref []
    and pendingStart = ref 0
    and pendingShift = ref 0
    and multiline = ref false
    and scopes = ref []
    and skipUntil = ref (-1) in
    let hasIndentedIdentifierHeadContinuation rhs next =
      if
        (not allowIndented)
        || (not (isIdentifierPathHead rhs))
        || next >= Array.length lines
      then false
      else
        let line = lines.(next) in
        let s = trim line in
        let us = units line in
        s <> ""
        && (not (starts s "//"))
        && Array.length us > 0
        && white us.(0)
        &&
        let without = stripComment s in
        (not (definitionStart without)) && not (isTestLine without)
    in
    let hasCompleteSeparator s =
      match findSeparatorIndex s with
      | Some i -> not (isIncompleteExpectationHead (tail s (i + 1)))
      | None -> false
    in
    let reset () =
      pending := [];
      pendingShift := 0;
      multiline := false
    in
    let record = function
      | Ok test -> tests := test :: !tests
      | Error e -> errors := e :: !errors
    in
    for i = 0 to Array.length lines - 1 do
      if i > !skipUntil then
        let line = lines.(i) in
        let s = trim line in
        let number = i + 1 in
        if s <> "" && not (starts s "//") then (
          if !multiline then (
            pending :=
              (if allowIndented then trimLeadingSpaces !pendingShift line
               else line)
              :: !pending;
            let accumulated =
              String.concat "\n" (List.map stripComment (List.rev !pending))
            in
            let closing = hasClosingParenTest accumulated
            and complete = hasCompleteSeparator accumulated in
            let continuation =
              match findSeparatorIndex accumulated with
              | Some sep ->
                  hasIndentedIdentifierHeadContinuation
                    (tail accumulated (sep + 1))
                    (i + 1)
              | None -> false
            in
            if (closing || complete) && not continuation then
              let full = String.concat "\n" (List.rev !pending)
              and preamble = String.concat "\n" (List.rev !preambleLines) in
              match
                parseMultilineTest full !pendingStart path preamble
                  !functionLineMap
              with
              | Ok test ->
                  tests := test :: !tests;
                  reset ()
              | Error _ when complete && not closing -> ()
              | Error e ->
                  errors := e :: !errors;
                  reset ())
          else
            let without = stripComment s and indent = countLeadingSpaces line in
            let rec pop ss ps fs =
              match ss with
              | (moduleIndent, parentPreamble, parentFunctions) :: rest
                when indent <= moduleIndent ->
                  pop rest parentPreamble parentFunctions
              | _ -> (ss, ps, fs)
            in
            let ss, ps, fs = pop !scopes !preambleLines !functionLineMap in
            scopes := ss;
            preambleLines := ps;
            functionLineMap := fs;
            let us = units line in
            let top = Array.length us > 0 && not (white us.(0)) in
            let shift = List.length !scopes * 2 in
            let mayTest = top || (allowIndented && indent = shift) in
            let isTest = isTestLine without in
            if mayTest && (not isTest) && not (definitionStart without) then (
              pendingShift := if allowIndented then shift else 0;
              pending :=
                [
                  (if allowIndented then trimLeadingSpaces !pendingShift line
                   else line);
                ];
              pendingStart := number;
              multiline := true)
            else if mayTest && isTest then
              let preamble = String.concat "\n" (List.rev !preambleLines) in
              let incomplete =
                allowIndented
                &&
                match findSeparatorIndex without with
                | Some sep ->
                    let rhs = tail without (sep + 1) in
                    isIncompleteExpectationHead rhs
                    || hasIndentedIdentifierHeadContinuation rhs (i + 1)
                | None -> false
              in
              if incomplete then
                let rec collect j rev =
                  if j >= Array.length lines then (List.rev rev, j)
                  else
                    let next = lines.(j) in
                    let ns = trim next in
                    let nus = units next in
                    if ns = "" || starts ns "//" then (List.rev rev, j)
                    else
                      let nextWithout = stripComment ns in
                      let needs =
                        hasUnclosedDelimiters
                          (String.concat "\n"
                             (List.map stripComment (line :: List.rev rev)))
                      in
                      if
                        Array.length nus > 0
                        && white nus.(0)
                        && (not (definitionStart nextWithout))
                        && (needs || not (isTestLine nextWithout))
                      then collect (j + 1) (next :: rev)
                      else (List.rev rev, j)
                in
                let continuation, next = collect (i + 1) [] in
                if continuation <> [] then (
                  record
                    (parseMultilineTest
                       (String.concat "\n" (line :: continuation))
                       number path preamble !functionLineMap);
                  skipUntil := next - 1)
                else
                  record
                    (parseTestLineWithPreamble s number path preamble
                       !functionLineMap)
              else
                record
                  (parseTestLineWithPreamble s number path preamble
                     !functionLineMap)
            else if allowIndented && (starts s "module " || starts s "module\t")
            then scopes := (indent, !preambleLines, !functionLineMap) :: !scopes
            else
              let normalized =
                if allowIndented then trimLeadingSpaces shift line else line
              in
              preambleLines := normalized :: !preambleLines;
              match extractFuncName normalized with
              | Some name ->
                  functionLineMap := StringMap.add name number !functionLineMap
              | None -> ())
    done;
    if !multiline then
      errors :=
        ("Line "
        ^ string_of_int !pendingStart
        ^ ": Unclosed multi-line expression (missing ') = <expectation>')")
        :: !errors;
    if !errors <> [] then Error (String.concat "\n" (List.rev !errors))
    else
      let preamble = String.concat "\n" (List.rev !preambleLines) in
      let normalizedPath =
        String.map (fun c -> if c = '\\' then '/' else c) path
      in
      let keepPerTest =
        allowIndented && Text.contains normalizedPath "/e2e/upstream/"
      in
      let parsed = List.rev !tests in
      let parsedLines =
        List.fold_left
          (fun set (t : e2eTest) -> IntSet.add t.sourceLine set)
          IntSet.empty parsed
      in
      let orphaned =
        IntMap.bindings overrides
        |> List.filter_map (fun (line, _) ->
            if IntSet.mem line parsedLines then None else Some line)
      in
      let normalized =
        List.map
          (fun (t : e2eTest) ->
            let t =
              if keepPerTest then t
              else { t with preamble; functionLineMap = !functionLineMap }
            in
            match IntMap.find_opt t.sourceLine overrides with
            | None -> t
            | Some expectedErrorMessage ->
                {
                  t with
                  expectedValueExpr = None;
                  expectedStdout = None;
                  expectedStderr = None;
                  expectedExitCode = 1;
                  errorExpectation = Some CompileError;
                  expectedErrorMessage;
                  skipReason = None;
                })
          parsed
      in
      match orphaned with
      | [] -> Ok normalized
      | line :: _ ->
          Error
            ("Line "
            ^ string_of_int (line - 1)
            ^ ": #compileerror must immediately precede a test")

let parseE2ETest _path =
  Error "Old format no longer supported - use parseE2ETestFile instead"
