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
let parseOp (text: string) : Result<BinOp, string> =
    match text.Trim() with
    | "+" -> Ok Add
    | "-" -> Ok Sub
    | "*" -> Ok Mul
    | "/" -> Ok Div
    | "==" -> Ok Eq
    | "!=" -> Ok Neq
    | "<" -> Ok Lt
    | ">" -> Ok Gt
    | "<=" -> Ok Lte
    | ">=" -> Ok Gte
    | "&&" -> Ok And
    | "||" -> Ok Or
    | op -> Error $"Unknown operator '{op}'"

/// Parse a single MIR instruction or terminator
/// Returns either an Instr or a Terminator
let parseInstructionOrTerminator (lineNum: int) (line: string) : Result<Choice<Instr, Terminator>, string> =
    let line = line.Trim()

    // Try ret pattern: "ret <operand>"
    let retMatch = Regex.Match(line, @"^ret\s+(.+)$")
    if retMatch.Success then
        match parseOperand retMatch.Groups.[1].Value with
        | Ok operand -> Ok (Choice2Of2 (Ret operand))
        | Error e -> Error $"Line {lineNum}: {e}"
    else

    // Try binop pattern: "v0 <- v1 + 3"
    let binopMatch = Regex.Match(line, @"^(v\d+)\s*<-\s*(.+?)\s*([+\-*/]|==|!=|<=|>=|<|>|&&|\|\|)\s*(.+)$")
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
                    | Ok right -> Ok (Choice1Of2 (BinOp (dest, op, left, right, AST.TInt64)))
    else

    // Try move pattern: "v0 <- 42" or "v0 <- v1"
    let movMatch = Regex.Match(line, @"^(v\d+)\s*<-\s*([^+\-*/!<>=&|]+)$")
    if movMatch.Success then
        match parseVReg movMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseOperand movMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src -> Ok (Choice1Of2 (Mov (dest, src, None)))
    else
        Error $"Line {lineNum}: Invalid instruction format '{line}'"

/// Parse MIR program from text
/// Parses flat instruction list and wraps in a single-block CFG
let parseMIR (text: string) : Result<MIR.Program, string> =
    let lines =
        text.Split('\n')
        |> Array.map (fun line -> line.Trim())
        |> Array.filter (fun line -> line <> "" && not (line.StartsWith("//")))
        |> Array.toList

    // Parse all instructions/terminators
    let rec parseLines lineNum acc = function
        | [] -> Ok (List.rev acc)
        | line :: rest ->
            match parseInstructionOrTerminator lineNum line with
            | Error e -> Error e
            | Ok result -> parseLines (lineNum + 1) (result :: acc) rest

    match parseLines 1 [] lines with
    | Error e -> Error e
    | Ok [] -> Error "Empty MIR program"
    | Ok parsed ->
        // Split into instructions and terminator
        // The last item should be a terminator (Ret)
        let lastItem = List.last parsed
        let instructions = List.take (List.length parsed - 1) parsed

        match lastItem with
        | Choice2Of2 terminator ->
            // Extract instructions from Choice1Of2
            let instrs =
                instructions
                |> List.choose (function
                    | Choice1Of2 instr -> Some instr
                    | Choice2Of2 _ -> None)

            // Check that all non-last items are instructions
            if List.length instrs <> List.length instructions then
                Error "Only the last line can be a terminator (ret)"
            else
                // Build single-block CFG
                let entryLabel = Label "entry"
                let block = {
                    Label = entryLabel
                    Instrs = instrs
                    Terminator = terminator
                }
                let cfg = {
                    Entry = entryLabel
                    Blocks = Map.ofList [(entryLabel, block)]
                }
                let func = {
                    Name = "_start"
                    Params = []
                    ParamTypes = []
                    ReturnType = AST.TInt64
                    CFG = cfg
                }
                Ok (Program ([func], MIR.emptyStringPool, MIR.emptyFloatPool, Map.empty))
        | Choice1Of2 _ ->
            Error "Last instruction must be a terminator (ret)"
