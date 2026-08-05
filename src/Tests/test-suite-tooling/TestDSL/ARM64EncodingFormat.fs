// ARM64EncodingFormat.fs - Parser for ARM64 encoding test DSL
//
// Parses .arm64enc test files that specify ARM64 instructions and their
// expected machine code encodings.
//
// Example format:
//   ---NAME---
//   Encode MOVZ instruction
//
//   ---INPUT-ARM64---
//   MOVZ(X0, 42, 0)
//
//   ---OUTPUT-HEX---
//   0xD2800540

module TestDSL.ARM64EncodingFormat

open System
open System.Globalization
open TestDSL.Common
open TestDSL.ARM64Parser
open ARM64

type ARM64EncodingExpectation =
    | EncodesTo of uint32 list
    | EncodingErrorContaining of string

/// ARM64 encoding test case
type ARM64EncodingTest = {
    Name: string
    Instructions: Instr list
    Expectation: ARM64EncodingExpectation
    AssertDifferent: bool  // If true, all hex values should be different
}

/// Parse a hex value from string (e.g., "0xD2800540" -> 0xD2800540u)
let parseHexValue (text: string) : Result<uint32, string> =
    let text = text.Trim()

    if text.StartsWith("0x") || text.StartsWith("0X") then
        let hexStr = text.Substring(2)
        let isHexDigit (c: char) =
            ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
        let hasOnlyHexDigits = hexStr |> Seq.forall isHexDigit

        match UInt32.TryParse(hexStr, NumberStyles.AllowHexSpecifier, CultureInfo.InvariantCulture) with
        | true, value -> Ok value
        | false, _ when hasOnlyHexDigits && hexStr.Length > 8 -> Error $"Hex value too large: '{text}'"
        | false, _ -> Error $"Invalid hex format: '{text}'"
    else
        Error $"Hex value must start with '0x': '{text}'"

let private parseAssertDifferent (text: string) : Result<bool, string> =
    match text.Trim().ToLowerInvariant() with
    | "true" -> Ok true
    | "false" -> Ok false
    | value -> Error $"Invalid ASSERT-DIFFERENT value '{value}' (expected 'true' or 'false')"

/// Parse ARM64 encoding test from file content
let parseARM64EncodingTest (content: string) : Result<ARM64EncodingTest, string> =
    let testFile = parseTestFile content

    // Parse name (optional, default to "ARM64 encoding test")
    let name =
        match getOptionalSection "NAME" testFile with
        | Some text -> text.Trim()
        | None -> "ARM64 encoding test"

    // Parse INPUT-ARM64 section
    match getRequiredSection "INPUT-ARM64" testFile with
    | Error e -> Error e
    | Ok inputText ->
        let parser =
            match getOptionalSection "EXPECT-ERROR" testFile with
            | Some _ -> parseARM64ForEncodingError
            | None -> parseARM64
        match parser inputText with
        | Error e -> Error $"Failed to parse INPUT-ARM64: {e}"
        | Ok instructions ->
            let outputText = getOptionalSection "OUTPUT-HEX" testFile
            let expectedError = getOptionalSection "EXPECT-ERROR" testFile
            match outputText, expectedError with
            | None, None -> Error "ARM64 encoding test requires OUTPUT-HEX or EXPECT-ERROR"
            | Some _, Some _ -> Error "ARM64 encoding test cannot combine OUTPUT-HEX and EXPECT-ERROR"
            | None, Some errorText when String.IsNullOrWhiteSpace errorText ->
                Error "EXPECT-ERROR cannot be empty"
            | None, Some errorText ->
                match getOptionalSection "ASSERT-DIFFERENT" testFile with
                | Some _ -> Error "ASSERT-DIFFERENT requires OUTPUT-HEX"
                | None ->
                    Ok {
                        Name = name
                        Instructions = instructions
                        Expectation = EncodingErrorContaining errorText
                        AssertDifferent = false
                    }
            | Some outputText, None ->
                let hexLines =
                    outputText.Split('\n')
                    |> Array.map (fun line -> line.Trim())
                    |> Array.filter (fun line -> line <> "" && not (line.StartsWith("//")))
                    |> Array.toList

                // Parse each hex value
                let rec parseHexValues acc = function
                    | [] -> Ok (List.rev acc)
                    | line :: rest ->
                        match parseHexValue line with
                        | Error e -> Error e
                        | Ok value -> parseHexValues (value :: acc) rest

                match parseHexValues [] hexLines with
                | Error e -> Error $"Failed to parse OUTPUT-HEX: {e}"
                | Ok hexValues ->
                    // Verify counts match
                    if instructions.Length <> hexValues.Length then
                        Error $"Instruction count ({instructions.Length}) does not match hex value count ({hexValues.Length})"
                    else
                        // Parse ASSERT-DIFFERENT (optional)
                        match getOptionalSection "ASSERT-DIFFERENT" testFile with
                        | Some text ->
                            parseAssertDifferent text
                            |> Result.map (fun assertDifferent -> {
                                Name = name
                                Instructions = instructions
                                Expectation = EncodesTo hexValues
                                AssertDifferent = assertDifferent
                            })
                        | None ->
                            Ok {
                                Name = name
                                Instructions = instructions
                                Expectation = EncodesTo hexValues
                                AssertDifferent = false
                            }
