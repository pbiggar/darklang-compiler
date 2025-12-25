// LIRParser.fs - Parser for LIR (Low-level IR) DSL
//
// Parses human-readable LIR text into LIR.Program data structures.
//
// Example LIR:
//   X1 <- Mov(Imm 42)
//   X2 <- Add(X1, Imm 5)
//   Ret

module TestDSL.LIRParser

open System
open System.Text.RegularExpressions
open LIR
open TestDSL.Common

/// Parse physical register from text like "X0", "X1", etc.
let parsePhysReg (text: string) : Result<PhysReg, string> =
    match text.Trim() with
    | "X0" -> Ok X0
    | "X1" -> Ok X1
    | "X2" -> Ok X2
    | "X3" -> Ok X3
    | "X4" -> Ok X4
    | "X5" -> Ok X5
    | "X6" -> Ok X6
    | "X7" -> Ok X7
    | "X8" -> Ok X8
    | "X9" -> Ok X9
    | "X10" -> Ok X10
    | "X11" -> Ok X11
    | "X12" -> Ok X12
    | "X13" -> Ok X13
    | "X14" -> Ok X14
    | "X15" -> Ok X15
    | "X29" -> Ok X29
    | "X30" -> Ok X30
    | "SP" -> Ok SP
    | reg -> Error $"Invalid physical register '{reg}' (expected X0-X15, X29, X30, or SP)"

/// Parse register (physical or virtual) from text
let parseRegister (text: string) : Result<Reg, string> =
    let text = text.Trim()
    if text.StartsWith("v") then
        let m = Regex.Match(text, @"^v(\d+)$")
        if m.Success then
            Ok (Virtual (int m.Groups.[1].Value))
        else
            Error $"Invalid virtual register '{text}' (expected 'v0', 'v1', etc.)"
    else
        match parsePhysReg text with
        | Ok pr -> Ok (Physical pr)
        | Error e -> Error e

/// Parse operand from text like "Imm 42", "Reg X1", "Stack 0"
let parseOperand (text: string) : Result<Operand, string> =
    let text = text.Trim()

    // Try immediate: "Imm 42"
    let immMatch = Regex.Match(text, @"^Imm\s+(-?\d+)$")
    if immMatch.Success then
        Ok (Imm (int64 immMatch.Groups.[1].Value))
    else

    // Try register: "Reg X1" or "Reg v0"
    let regMatch = Regex.Match(text, @"^Reg\s+(.+)$")
    if regMatch.Success then
        match parseRegister regMatch.Groups.[1].Value with
        | Ok reg -> Ok (LIR.Operand.Reg reg)
        | Error e -> Error e
    else

    // Try stack slot: "Stack 0"
    let stackMatch = Regex.Match(text, @"^Stack\s+(\d+)$")
    if stackMatch.Success then
        Ok (StackSlot (int stackMatch.Groups.[1].Value))
    else
        Error $"Invalid operand '{text}' (expected 'Imm N', 'Reg X', or 'Stack N')"

/// Parse a single LIR instruction or terminator
/// Returns either an Instr or a Terminator
let parseInstructionOrTerminator (lineNum: int) (line: string) : Result<Choice<Instr, Terminator>, string> =
    let line = line.Trim()

    // Try Ret: "Ret"
    if line = "Ret" then
        Ok (Choice2Of2 Ret)
    else

    // Try PrintInt: "PrintInt(X0)" or "PrintInt(v0)"
    let printIntMatch = Regex.Match(line, @"^PrintInt\((.+)\)$")
    if printIntMatch.Success then
        match parseRegister printIntMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok reg -> Ok (Choice1Of2 (PrintInt reg))
    else

    // Try PrintBool: "PrintBool(X0)" or "PrintBool(v0)"
    let printBoolMatch = Regex.Match(line, @"^PrintBool\((.+)\)$")
    if printBoolMatch.Success then
        match parseRegister printBoolMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok reg -> Ok (Choice1Of2 (PrintBool reg))
    else

    // Try Mov: "X1 <- Mov(Imm 42)"
    let movMatch = Regex.Match(line, @"^(.+?)\s*<-\s*Mov\((.+)\)$")
    if movMatch.Success then
        match parseRegister movMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseOperand movMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src -> Ok (Choice1Of2 (Mov (dest, src)))
    else

    // Try Add: "X3 <- Add(X1, Imm 5)"
    let addMatch = Regex.Match(line, @"^(.+?)\s*<-\s*Add\((.+?),\s*(.+)\)$")
    if addMatch.Success then
        match parseRegister addMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseRegister addMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok left ->
                match parseOperand addMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok right -> Ok (Choice1Of2 (Add (dest, left, right)))
    else

    // Try Sub: "X3 <- Sub(X1, Imm 5)"
    let subMatch = Regex.Match(line, @"^(.+?)\s*<-\s*Sub\((.+?),\s*(.+)\)$")
    if subMatch.Success then
        match parseRegister subMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseRegister subMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok left ->
                match parseOperand subMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok right -> Ok (Choice1Of2 (Sub (dest, left, right)))
    else

    // Try Mul: "X3 <- Mul(X1, Reg X2)" - note: Mul requires both operands to be registers
    let mulMatch = Regex.Match(line, @"^(.+?)\s*<-\s*Mul\((.+?),\s*Reg\s+(.+)\)$")
    if mulMatch.Success then
        match parseRegister mulMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseRegister mulMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok left ->
                match parseRegister mulMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok right -> Ok (Choice1Of2 (Mul (dest, left, right)))
    else

    // Try Sdiv: "X3 <- Sdiv(X1, Reg X2)" - note: Sdiv requires both operands to be registers
    let divMatch = Regex.Match(line, @"^(.+?)\s*<-\s*Sdiv\((.+?),\s*Reg\s+(.+)\)$")
    if divMatch.Success then
        match parseRegister divMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseRegister divMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok left ->
                match parseRegister divMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok right -> Ok (Choice1Of2 (Sdiv (dest, left, right)))
    else
        Error $"Line {lineNum}: Invalid instruction format '{line}'"

/// Parse LIR program from text
/// Parses flat instruction list and wraps in a single-block CFG
let parseLIR (text: string) : Result<LIR.Program, string> =
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
    | Ok [] -> Error "Empty LIR program"
    | Ok parsed ->
        // Split into instructions and terminator
        // The last item might be a terminator (Ret) or an instruction
        let lastItem = List.last parsed
        let instructions = List.take (List.length parsed - 1) parsed

        let (instrs, terminator) =
            match lastItem with
            | Choice2Of2 term ->
                // Last item is a terminator
                let instrs =
                    instructions
                    |> List.choose (function
                        | Choice1Of2 instr -> Some instr
                        | Choice2Of2 _ -> None)
                // Check that all non-last items are instructions
                if List.length instrs <> List.length instructions then
                    ([], Ret)  // Will trigger error below
                else
                    (instrs, term)
            | Choice1Of2 lastInstr ->
                // Last item is an instruction - add default Ret terminator
                let allInstrs =
                    parsed
                    |> List.choose (function
                        | Choice1Of2 instr -> Some instr
                        | Choice2Of2 _ -> None)
                // Check that all items are instructions
                if List.length allInstrs <> List.length parsed then
                    ([], Ret)  // Will trigger error
                else
                    (allInstrs, Ret)

        // Build single-block CFG
        let entryLabel = "entry"
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
            CFG = cfg
            StackSize = 0
            UsedCalleeSaved = []
        }
        Ok (Program ([func], MIR.emptyStringPool, MIR.emptyFloatPool))
