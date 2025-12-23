// ARM64Parser.fs - Parser for ARM64 instruction DSL
//
// Parses human-readable ARM64 text into ARM64.Instr data structures.
//
// Example ARM64:
//   MOVZ(X1, 10, 0)
//   SUB_imm(X1, X1, 3)
//   MOV_reg(X0, X1)
//   RET

module TestDSL.ARM64Parser

open System
open System.Text.RegularExpressions
open ARM64
open TestDSL.Common

/// Parse ARM64 register from text like "X0", "X1", etc.
let parseReg (text: string) : Result<Reg, string> =
    match text.Trim() with
    | "X0" -> Ok X0 | "X1" -> Ok X1 | "X2" -> Ok X2 | "X3" -> Ok X3
    | "X4" -> Ok X4 | "X5" -> Ok X5 | "X6" -> Ok X6 | "X7" -> Ok X7
    | "X8" -> Ok X8 | "X9" -> Ok X9 | "X10" -> Ok X10 | "X11" -> Ok X11
    | "X12" -> Ok X12 | "X13" -> Ok X13 | "X14" -> Ok X14 | "X15" -> Ok X15
    | "X16" -> Ok X16 | "X29" -> Ok X29 | "X30" -> Ok X30 | "SP" -> Ok SP
    | reg -> Error $"Invalid ARM64 register '{reg}'"

/// Parse a single ARM64 instruction
let parseInstruction (lineNum: int) (line: string) : Result<Instr, string> =
    let line = line.Trim()

    // Try RET
    if line = "RET" then
        Ok RET
    else

    // Try MOVZ: "MOVZ(X1, 10, 0)"
    let movzMatch = Regex.Match(line, @"^MOVZ\((.+?),\s*(\d+),\s*(\d+)\)$")
    if movzMatch.Success then
        match parseReg movzMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            let imm = uint16 movzMatch.Groups.[2].Value
            let shift = int movzMatch.Groups.[3].Value
            Ok (MOVZ (dest, imm, shift))
    else

    // Try MOVK: "MOVK(X1, 10, 16)"
    let movkMatch = Regex.Match(line, @"^MOVK\((.+?),\s*(\d+),\s*(\d+)\)$")
    if movkMatch.Success then
        match parseReg movkMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            let imm = uint16 movkMatch.Groups.[2].Value
            let shift = int movkMatch.Groups.[3].Value
            Ok (MOVK (dest, imm, shift))
    else

    // Try ADD_imm: "ADD_imm(X1, X0, 5)"
    let addImmMatch = Regex.Match(line, @"^ADD_imm\((.+?),\s*(.+?),\s*(\d+)\)$")
    if addImmMatch.Success then
        match parseReg addImmMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg addImmMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src ->
                let imm = uint16 addImmMatch.Groups.[3].Value
                Ok (ADD_imm (dest, src, imm))
    else

    // Try ADD_reg: "ADD_reg(X1, X0, X2)"
    let addRegMatch = Regex.Match(line, @"^ADD_reg\((.+?),\s*(.+?),\s*(.+?)\)$")
    if addRegMatch.Success then
        match parseReg addRegMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg addRegMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src1 ->
                match parseReg addRegMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok src2 -> Ok (ADD_reg (dest, src1, src2))
    else

    // Try SUB_imm: "SUB_imm(X1, X1, 3)"
    let subImmMatch = Regex.Match(line, @"^SUB_imm\((.+?),\s*(.+?),\s*(\d+)\)$")
    if subImmMatch.Success then
        match parseReg subImmMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg subImmMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src ->
                let imm = uint16 subImmMatch.Groups.[3].Value
                Ok (SUB_imm (dest, src, imm))
    else

    // Try SUB_reg: "SUB_reg(X1, X0, X2)"
    let subRegMatch = Regex.Match(line, @"^SUB_reg\((.+?),\s*(.+?),\s*(.+?)\)$")
    if subRegMatch.Success then
        match parseReg subRegMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg subRegMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src1 ->
                match parseReg subRegMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok src2 -> Ok (SUB_reg (dest, src1, src2))
    else

    // Try MUL: "MUL(X1, X0, X2)"
    let mulMatch = Regex.Match(line, @"^MUL\((.+?),\s*(.+?),\s*(.+?)\)$")
    if mulMatch.Success then
        match parseReg mulMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg mulMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src1 ->
                match parseReg mulMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok src2 -> Ok (MUL (dest, src1, src2))
    else

    // Try SDIV: "SDIV(X1, X0, X2)"
    let sdivMatch = Regex.Match(line, @"^SDIV\((.+?),\s*(.+?),\s*(.+?)\)$")
    if sdivMatch.Success then
        match parseReg sdivMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg sdivMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src1 ->
                match parseReg sdivMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok src2 -> Ok (SDIV (dest, src1, src2))
    else

    // Try MOV_reg: "MOV_reg(X0, X1)"
    let movMatch = Regex.Match(line, @"^MOV_reg\((.+?),\s*(.+?)\)$")
    if movMatch.Success then
        match parseReg movMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg movMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src -> Ok (MOV_reg (dest, src))
    else

    // Try SVC: "SVC(128)"
    let svcMatch = Regex.Match(line, @"^SVC\((\d+)\)$")
    if svcMatch.Success then
        let imm = uint16 svcMatch.Groups.[1].Value
        Ok (SVC imm)
    else
        Error $"Line {lineNum}: Invalid ARM64 instruction format '{line}'"

/// Parse ARM64 program from text
let parseARM64 (text: string) : Result<Instr list, string> =
    let lines = stripCommentsAndEmpty text

    // Parse each line into an instruction
    let results = lines |> List.mapi (fun i line -> parseInstruction (i+1) line)

    // Collect all results, stopping at first error
    let rec collectResults acc = function
        | [] -> Ok (List.rev acc)
        | (Ok instr) :: rest -> collectResults (instr :: acc) rest
        | (Error e) :: _ -> Error e

    collectResults [] results
