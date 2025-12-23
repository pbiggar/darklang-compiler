// MIRParser.fs - Parser for MIR (Mid-level IR) DSL
//
// Parses human-readable MIR text into MIR.Program data structures.
//
// Example MIR:
//   v0 <- 42
//   v1 <- v0 + 3
//   ret v1

module TestDSL.MIRParser

open System
open System.Text.RegularExpressions
open MIR
open TestDSL.Common

/// Parse virtual register from text like "v123"
let parseVReg (text: string) : Result<VReg, string> =
    let m = Regex.Match(text.Trim(), @"^v(\d+)$")
    if m.Success then
        Ok (VReg (int m.Groups.[1].Value))
    else
        Error $"Invalid register format '{text}' (expected 'v0', 'v1', etc.)"

/// Parse operand (either a number or a register)
let parseOperand (text: string) : Result<Operand, string> =
    let text = text.Trim()
    // Try parsing as register first
    match parseVReg text with
    | Ok reg -> Ok (Register reg)
    | Error _ ->
        // Try parsing as integer
        match Int64.TryParse(text) with
        | true, n -> Ok (IntConst n)
        | false, _ -> Error $"Invalid operand '{text}' (expected number or register)"

/// Parse binary operator
let parseOp (text: string) : Result<Op, string> =
    match text.Trim() with
    | "+" -> Ok Add
    | "-" -> Ok Sub
    | "*" -> Ok Mul
    | "/" -> Ok Div
    | op -> Error $"Unknown operator '{op}' (expected +, -, *, /)"

/// Parse a single MIR instruction
let parseInstruction (lineNum: int) (line: string) : Result<Instr, string> =
    let line = line.Trim()

    // Try ret pattern: "ret <operand>"
    let retMatch = Regex.Match(line, @"^ret\s+(.+)$")
    if retMatch.Success then
        match parseOperand retMatch.Groups.[1].Value with
        | Ok operand -> Ok (Ret operand)
        | Error e -> Error $"Line {lineNum}: {e}"
    else

    // Try binop pattern: "v0 <- v1 + 3"
    let binopMatch = Regex.Match(line, @"^(v\d+)\s*<-\s*(.+?)\s*([+\-*/])\s*(.+)$")
    if binopMatch.Success then
        match parseVReg binopMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseOperand binopMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok left ->
                match parseOp binopMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok op ->
                    match parseOperand binopMatch.Groups.[4].Value with
                    | Error e -> Error $"Line {lineNum}: {e}"
                    | Ok right -> Ok (BinOp (dest, op, left, right))
    else

    // Try move pattern: "v0 <- 42" or "v0 <- v1"
    let movMatch = Regex.Match(line, @"^(v\d+)\s*<-\s*([^+\-*/]+)$")
    if movMatch.Success then
        match parseVReg movMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseOperand movMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src -> Ok (Mov (dest, src))
    else
        Error $"Line {lineNum}: Invalid instruction format '{line}'"

/// Parse MIR program from text
let parseMIR (text: string) : Result<MIR.Program, string> =
    let lines = stripCommentsAndEmpty text

    // Parse each line into an instruction
    let results = lines |> List.mapi (fun i line -> parseInstruction (i+1) line)

    // Collect all results, stopping at first error
    let rec collectResults acc = function
        | [] -> Ok (List.rev acc)
        | (Ok instr) :: rest -> collectResults (instr :: acc) rest
        | (Error e) :: _ -> Error e

    match collectResults [] results with
    | Ok instrs -> Ok (MIR.Program [MIR.Block instrs])
    | Error e -> Error e
