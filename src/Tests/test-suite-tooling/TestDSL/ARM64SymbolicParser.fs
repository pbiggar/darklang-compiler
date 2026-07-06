// ARM64SymbolicParser.fs - Parser for symbolic ARM64 instruction DSL
//
// Parses human-readable ARM64 text into ARM64Symbolic.Instr data structures.
//
// Example ARM64:
//   MOVZ(X1, 10, 0)
//   SUB_imm(X1, X1, 3)
//   MOV_reg(X0, X1)
//   RET

module TestDSL.ARM64SymbolicParser

open System
open System.Text.RegularExpressions
open System.Globalization
open ARM64Symbolic
open TestDSL.Common

/// Parse ARM64 register from text like "X0", "X1", etc.
let parseReg (text: string) : Result<ARM64.Reg, string> =
    match text.Trim() with
    | "X0" -> Ok ARM64.X0 | "X1" -> Ok ARM64.X1 | "X2" -> Ok ARM64.X2 | "X3" -> Ok ARM64.X3
    | "X4" -> Ok ARM64.X4 | "X5" -> Ok ARM64.X5 | "X6" -> Ok ARM64.X6 | "X7" -> Ok ARM64.X7
    | "X8" -> Ok ARM64.X8 | "X9" -> Ok ARM64.X9 | "X10" -> Ok ARM64.X10 | "X11" -> Ok ARM64.X11
    | "X12" -> Ok ARM64.X12 | "X13" -> Ok ARM64.X13 | "X14" -> Ok ARM64.X14 | "X15" -> Ok ARM64.X15
    | "X16" -> Ok ARM64.X16 | "X17" -> Ok ARM64.X17
    | "X19" -> Ok ARM64.X19 | "X20" -> Ok ARM64.X20 | "X21" -> Ok ARM64.X21 | "X22" -> Ok ARM64.X22
    | "X23" -> Ok ARM64.X23 | "X24" -> Ok ARM64.X24 | "X25" -> Ok ARM64.X25 | "X26" -> Ok ARM64.X26
    | "X27" -> Ok ARM64.X27 | "X28" -> Ok ARM64.X28
    | "X29" -> Ok ARM64.X29 | "X30" -> Ok ARM64.X30 | "SP" -> Ok ARM64.SP
    | reg -> Error $"Invalid ARM64 register '{reg}'"

/// Parse ARM64 condition from text like "EQ", "NE", etc.
let parseCond (text: string) : Result<ARM64.Condition, string> =
    match text.Trim() with
    | "EQ" -> Ok ARM64.EQ
    | "NE" -> Ok ARM64.NE
    | "LT" -> Ok ARM64.LT
    | "GT" -> Ok ARM64.GT
    | "LE" -> Ok ARM64.LE
    | "GE" -> Ok ARM64.GE
    | cond -> Error $"Invalid ARM64 condition '{cond}'"

let private parseUInt16Operand (lineNum: int) (fieldName: string) (text: string) : Result<uint16, string> =
    match UInt16.TryParse(text.Trim()) with
    | true, value -> Ok value
    | _ -> Error $"Line {lineNum}: Invalid {fieldName} '{text}'"

let private parseInt16Operand (lineNum: int) (fieldName: string) (text: string) : Result<int16, string> =
    match Int16.TryParse(text.Trim()) with
    | true, value -> Ok value
    | _ -> Error $"Line {lineNum}: Invalid {fieldName} '{text}'"

let private parseIntOperand (lineNum: int) (fieldName: string) (text: string) : Result<int, string> =
    match Int32.TryParse(text.Trim()) with
    | true, value -> Ok value
    | _ -> Error $"Line {lineNum}: Invalid {fieldName} '{text}'"

/// Parse symbolic label reference
let parseLabelRef (text: string) : Result<ARM64Symbolic.LabelRef, string> =
    let trimmed = text.Trim()
    if trimmed.StartsWith("data:") then
        let name = trimmed.Substring("data:".Length)
        Ok (ARM64Symbolic.DataLabel (ARM64Symbolic.Named name))
    elif trimmed.StartsWith("str:\"") && trimmed.EndsWith("\"") then
        let inner = trimmed.Substring(5, trimmed.Length - 6)
        let unescaped = inner.Replace("\\\\", "\\").Replace("\\\"", "\"")
        Ok (ARM64Symbolic.DataLabel (ARM64Symbolic.StringLiteral unescaped))
    elif trimmed.StartsWith("float:") then
        let valueText = trimmed.Substring("float:".Length)
        match Double.TryParse(valueText, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, value -> Ok (ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral value))
        | _ -> Error $"Invalid float literal '{valueText}'"
    else
        Ok (ARM64Symbolic.CodeLabel trimmed)

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
            match parseUInt16Operand lineNum "MOVZ immediate" movzMatch.Groups.[2].Value,
                  parseIntOperand lineNum "MOVZ shift" movzMatch.Groups.[3].Value with
            | Ok imm, Ok shift -> Ok (MOVZ (dest, imm, shift))
            | Error e, _ -> Error e
            | _, Error e -> Error e
    else

    // Try MOVK: "MOVK(X1, 10, 16)"
    let movkMatch = Regex.Match(line, @"^MOVK\((.+?),\s*(\d+),\s*(\d+)\)$")
    if movkMatch.Success then
        match parseReg movkMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseUInt16Operand lineNum "MOVK immediate" movkMatch.Groups.[2].Value,
                  parseIntOperand lineNum "MOVK shift" movkMatch.Groups.[3].Value with
            | Ok imm, Ok shift -> Ok (MOVK (dest, imm, shift))
            | Error e, _ -> Error e
            | _, Error e -> Error e
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
                match parseUInt16Operand lineNum "ADD_imm immediate" addImmMatch.Groups.[3].Value with
                | Error e -> Error e
                | Ok imm -> Ok (ADD_imm (dest, src, imm))
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
                match parseUInt16Operand lineNum "SUB_imm immediate" subImmMatch.Groups.[3].Value with
                | Error e -> Error e
                | Ok imm -> Ok (SUB_imm (dest, src, imm))
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

    // Try ADRP: "ADRP(X0, label)"
    let adrpMatch = Regex.Match(line, @"^ADRP\((.+?),\s*(.+)\)$")
    if adrpMatch.Success then
        match parseReg adrpMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseLabelRef adrpMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok labelRef -> Ok (ADRP (dest, labelRef))
    else

    // Try ADD_label: "ADD_label(X0, X1, label)"
    let addLabelMatch = Regex.Match(line, @"^ADD_label\((.+?),\s*(.+?),\s*(.+)\)$")
    if addLabelMatch.Success then
        match parseReg addLabelMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg addLabelMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok src ->
                match parseLabelRef addLabelMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok labelRef -> Ok (ADD_label (dest, src, labelRef))
    else

    // Try ADR: "ADR(X0, label)"
    let adrMatch = Regex.Match(line, @"^ADR\((.+?),\s*(.+)\)$")
    if adrMatch.Success then
        match parseReg adrMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseLabelRef adrMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok labelRef -> Ok (ADR (dest, labelRef))
    else

    // Try SVC: "SVC(128)"
    let svcMatch = Regex.Match(line, @"^SVC\((\d+)\)$")
    if svcMatch.Success then
        match parseUInt16Operand lineNum "SVC immediate" svcMatch.Groups.[1].Value with
        | Error e -> Error e
        | Ok imm -> Ok (SVC imm)
    else

    // Try STP: "STP(X29, X30, SP, -16)"
    let stpMatch = Regex.Match(line, @"^STP\((.+?),\s*(.+?),\s*(.+?),\s*(-?\d+)\)$")
    if stpMatch.Success then
        match parseReg stpMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok reg1 ->
            match parseReg stpMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok reg2 ->
                match parseReg stpMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok addr ->
                    match parseInt16Operand lineNum "STP offset" stpMatch.Groups.[4].Value with
                    | Error e -> Error e
                    | Ok offset -> Ok (STP (reg1, reg2, addr, offset))
    else

    // Try STP_pre: "STP_pre(X29, X30, SP, -16)"
    let stpPreMatch = Regex.Match(line, @"^STP_pre\((.+?),\s*(.+?),\s*(.+?),\s*(-?\d+)\)$")
    if stpPreMatch.Success then
        match parseReg stpPreMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok reg1 ->
            match parseReg stpPreMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok reg2 ->
                match parseReg stpPreMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok addr ->
                    match parseInt16Operand lineNum "STP_pre offset" stpPreMatch.Groups.[4].Value with
                    | Error e -> Error e
                    | Ok offset -> Ok (STP_pre (reg1, reg2, addr, offset))
    else

    // Try LDP: "LDP(X29, X30, SP, 16)"
    let ldpMatch = Regex.Match(line, @"^LDP\((.+?),\s*(.+?),\s*(.+?),\s*(-?\d+)\)$")
    if ldpMatch.Success then
        match parseReg ldpMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok reg1 ->
            match parseReg ldpMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok reg2 ->
                match parseReg ldpMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok addr ->
                    match parseInt16Operand lineNum "LDP offset" ldpMatch.Groups.[4].Value with
                    | Error e -> Error e
                    | Ok offset -> Ok (LDP (reg1, reg2, addr, offset))
    else

    // Try LDP_post: "LDP_post(X29, X30, SP, 16)"
    let ldpPostMatch = Regex.Match(line, @"^LDP_post\((.+?),\s*(.+?),\s*(.+?),\s*(-?\d+)\)$")
    if ldpPostMatch.Success then
        match parseReg ldpPostMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok reg1 ->
            match parseReg ldpPostMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok reg2 ->
                match parseReg ldpPostMatch.Groups.[3].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok addr ->
                    match parseInt16Operand lineNum "LDP_post offset" ldpPostMatch.Groups.[4].Value with
                    | Error e -> Error e
                    | Ok offset -> Ok (LDP_post (reg1, reg2, addr, offset))
    else

    // Try STR: "STR(X0, SP, 8)"
    let strMatch = Regex.Match(line, @"^STR\((.+?),\s*(.+?),\s*(-?\d+)\)$")
    if strMatch.Success then
        match parseReg strMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok src ->
            match parseReg strMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok addr ->
                match parseInt16Operand lineNum "STR offset" strMatch.Groups.[3].Value with
                | Error e -> Error e
                | Ok offset -> Ok (STR (src, addr, offset))
    else

    // Try LDR: "LDR(X0, SP, 8)"
    let ldrMatch = Regex.Match(line, @"^LDR\((.+?),\s*(.+?),\s*(-?\d+)\)$")
    if ldrMatch.Success then
        match parseReg ldrMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg ldrMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok addr ->
                match parseInt16Operand lineNum "LDR offset" ldrMatch.Groups.[3].Value with
                | Error e -> Error e
                | Ok offset -> Ok (LDR (dest, addr, offset))
    else

    // Try STUR: "STUR(X0, X29, -8)"
    let sturMatch = Regex.Match(line, @"^STUR\((.+?),\s*(.+?),\s*(-?\d+)\)$")
    if sturMatch.Success then
        match parseReg sturMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok src ->
            match parseReg sturMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok addr ->
                match parseInt16Operand lineNum "STUR offset" sturMatch.Groups.[3].Value with
                | Error e -> Error e
                | Ok offset -> Ok (STUR (src, addr, offset))
    else

    // Try LDUR: "LDUR(X0, X29, -8)"
    let ldurMatch = Regex.Match(line, @"^LDUR\((.+?),\s*(.+?),\s*(-?\d+)\)$")
    if ldurMatch.Success then
        match parseReg ldurMatch.Groups.[1].Value with
        | Error e -> Error $"Line {lineNum}: {e}"
        | Ok dest ->
            match parseReg ldurMatch.Groups.[2].Value with
            | Error e -> Error $"Line {lineNum}: {e}"
            | Ok addr ->
                match parseInt16Operand lineNum "LDUR offset" ldurMatch.Groups.[3].Value with
                | Error e -> Error e
                | Ok offset -> Ok (LDUR (dest, addr, offset))
    else
        Error $"Line {lineNum}: Invalid instruction format '{line}'"

/// Parse ARM64 program from text
let parseARM64Symbolic (text: string) : Result<Instr list, string> =
    let lines =
        text.Split('\n')
        |> Array.map (fun line -> line.Trim())
        |> Array.filter (fun line -> line <> "" && not (line.StartsWith("//")))
        |> Array.toList

    let rec parseLines lineNum acc = function
        | [] -> Ok (List.rev acc)
        | line :: rest ->
            match parseInstruction lineNum line with
            | Error e -> Error e
            | Ok instr -> parseLines (lineNum + 1) (instr :: acc) rest

    parseLines 1 [] lines
