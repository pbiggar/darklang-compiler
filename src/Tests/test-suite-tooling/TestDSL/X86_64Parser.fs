// X86_64Parser.fs - Parser for the x64 instruction subset used by encoding fixtures.
//
// Uses constructor-style syntax matching the x64 instruction discriminated union.

module TestDSL.X86_64Parser

open System
open System.Text.RegularExpressions
open X86_64

let private parseReg (text: string) : Result<Reg, string> =
    match text.Trim().ToUpperInvariant() with
    | "RAX" -> Ok RAX | "RBX" -> Ok RBX | "RCX" -> Ok RCX | "RDX" -> Ok RDX
    | "RSI" -> Ok RSI | "RDI" -> Ok RDI | "RBP" -> Ok RBP | "RSP" -> Ok RSP
    | "R8" -> Ok R8 | "R9" -> Ok R9 | "R10" -> Ok R10 | "R11" -> Ok R11
    | "R12" -> Ok R12 | "R13" -> Ok R13 | "R14" -> Ok R14 | "R15" -> Ok R15
    | value -> Error $"Invalid x64 register '{value}'"

let private parseFReg (text: string) : Result<FReg, string> =
    match text.Trim().ToUpperInvariant() with
    | "XMM0" -> Ok XMM0 | "XMM1" -> Ok XMM1 | "XMM2" -> Ok XMM2 | "XMM3" -> Ok XMM3
    | "XMM4" -> Ok XMM4 | "XMM5" -> Ok XMM5 | "XMM6" -> Ok XMM6 | "XMM7" -> Ok XMM7
    | "XMM8" -> Ok XMM8 | "XMM9" -> Ok XMM9 | "XMM10" -> Ok XMM10 | "XMM11" -> Ok XMM11
    | "XMM12" -> Ok XMM12 | "XMM13" -> Ok XMM13 | "XMM14" -> Ok XMM14 | "XMM15" -> Ok XMM15
    | value -> Error $"Invalid x64 floating-point register '{value}'"

let private parseCondition (text: string) : Result<Condition, string> =
    match text.Trim().ToUpperInvariant() with
    | "EQ" -> Ok EQ | "NE" -> Ok NE | "LT" -> Ok LT | "GT" -> Ok GT
    | "LE" -> Ok LE | "GE" -> Ok GE | "B" -> Ok B | "A" -> Ok A
    | "BE" -> Ok BE | "AE" -> Ok AE | "P" -> Ok P | "NP" -> Ok NP
    | value -> Error $"Invalid x64 condition '{value}'"

let private parseInt32 (description: string) (text: string) : Result<int32, string> =
    match Int32.TryParse(text.Trim()) with
    | true, value -> Ok value
    | false, _ -> Error $"Invalid {description} '{text.Trim()}' (expected 32-bit integer)"

let private parseShift (text: string) : Result<int, string> =
    match Int32.TryParse(text.Trim()) with
    | true, value -> Ok value
    | false, _ -> Error $"Invalid shift '{text.Trim()}' (expected integer)"

let private twoArgs (name: string) (line: string) =
    Regex.Match(line, $@"^{name}\(([^,]+),\s*([^\)]+)\)$")

let private threeArgs (name: string) (line: string) =
    Regex.Match(line, $@"^{name}\(([^,]+),\s*([^,]+),\s*([^\)]+)\)$")

let private oneArg (name: string) (line: string) =
    Regex.Match(line, $@"^{name}\(([^\)]+)\)$")

let private parseRegReg (constructor: Reg * Reg -> Instr) (name: string) (line: string) : Result<Instr option, string> =
    let matched = twoArgs name line
    if not matched.Success then Ok None
    else
        match parseReg matched.Groups.[1].Value, parseReg matched.Groups.[2].Value with
        | Ok left, Ok right -> Ok (Some (constructor (left, right)))
        | Error msg, _
        | _, Error msg -> Error msg

let private parseRegImmediate (constructor: Reg * int32 -> Instr) (name: string) (line: string) : Result<Instr option, string> =
    let matched = twoArgs name line
    if not matched.Success then Ok None
    else
        match parseReg matched.Groups.[1].Value, parseInt32 "immediate" matched.Groups.[2].Value with
        | Ok reg, Ok immediate -> Ok (Some (constructor (reg, immediate)))
        | Error msg, _
        | _, Error msg -> Error msg

let private parseLine (lineNumber: int) (source: string) : Result<Instr, string> =
    let line = source.Trim()
    let withLine result = result |> Result.mapError (fun msg -> $"Line {lineNumber}: {msg}")

    let labelLike name constructor =
        let matched = oneArg name line
        if matched.Success then Some (constructor (matched.Groups.[1].Value.Trim())) else None

    match line with
    | "RET" -> Ok RET
    | "SYSCALL" -> Ok SYSCALL
    | "CQO" -> Ok CQO
    | _ ->
        match labelLike "Label" Label with
        | Some instruction -> Ok instruction
        | None ->
            match labelLike "JMP" JMP with
            | Some instruction -> Ok instruction
            | None ->
                match labelLike "CALL" CALL with
                | Some instruction -> Ok instruction
                | None ->
                    let unaryReg name constructor =
                        let matched = oneArg name line
                        if not matched.Success then Ok None
                        else parseReg matched.Groups.[1].Value |> Result.map (constructor >> Some)

                    let jcc = twoArgs "Jcc" line
                    if jcc.Success then
                        match parseCondition jcc.Groups.[1].Value with
                        | Ok condition -> Ok (Jcc (condition, jcc.Groups.[2].Value.Trim()))
                        | Error msg -> Error $"Line {lineNumber}: {msg}"
                    else
                        let memory name constructor =
                            let matched = threeArgs name line
                            if not matched.Success then Ok None
                            else
                                match parseReg matched.Groups.[1].Value,
                                      parseReg matched.Groups.[2].Value,
                                      parseInt32 "memory offset" matched.Groups.[3].Value with
                                | Ok first, Ok second, Ok offset -> Ok (Some (constructor (first, second, offset)))
                                | Error msg, _, _
                                | _, Error msg, _
                                | _, _, Error msg -> Error msg

                        let store name constructor =
                            let matched = threeArgs name line
                            if not matched.Success then Ok None
                            else
                                match parseReg matched.Groups.[1].Value,
                                      parseInt32 "memory offset" matched.Groups.[2].Value,
                                      parseReg matched.Groups.[3].Value with
                                | Ok baseAddr, Ok offset, Ok src -> Ok (Some (constructor (baseAddr, offset, src)))
                                | Error msg, _, _
                                | _, Error msg, _
                                | _, _, Error msg -> Error msg

                        let movsdStore = threeArgs "MOVSD_store" line
                        let parsers : (unit -> Result<Instr option, string>) list = [
                            fun () -> parseRegReg MOV_reg "MOV_reg" line
                            fun () -> parseRegImmediate MOV_imm32 "MOV_imm32" line
                            fun () -> memory "MOV_load" MOV_load
                            fun () -> store "MOV_store" MOV_store
                            fun () -> memory "LEA" LEA
                            fun () -> parseRegImmediate ADD_imm "ADD_imm" line
                            fun () -> parseRegImmediate SUB_imm "SUB_imm" line
                            fun () -> parseRegReg XOR_reg "XOR_reg" line
                            fun () -> parseRegReg ADD_reg "ADD_reg" line
                            fun () -> parseRegImmediate CMP_imm "CMP_imm" line
                            fun () -> parseRegReg IMUL_reg "IMUL_reg" line
                            fun () ->
                                let matched = twoArgs "SHL_imm" line
                                if not matched.Success then Ok None
                                else
                                    match parseReg matched.Groups.[1].Value, parseShift matched.Groups.[2].Value with
                                    | Ok reg, Ok shift -> Ok (Some (SHL_imm (reg, shift)))
                                    | Error msg, _
                                    | _, Error msg -> Error msg
                            fun () -> memory "MOV_load_byte" MOV_load_byte
                            fun () ->
                                if not movsdStore.Success then Ok None
                                else
                                    match parseReg movsdStore.Groups.[1].Value,
                                          parseInt32 "memory offset" movsdStore.Groups.[2].Value,
                                          parseFReg movsdStore.Groups.[3].Value with
                                    | Ok baseAddr, Ok offset, Ok src -> Ok (Some (MOVSD_store (baseAddr, offset, src)))
                                    | Error msg, _, _
                                    | _, Error msg, _
                                    | _, _, Error msg -> Error msg
                            fun () -> unaryReg "PUSH" PUSH
                            fun () -> unaryReg "POP" POP
                            fun () -> unaryReg "NEG" NEG
                        ]

                        let rec choose remaining =
                            match remaining with
                            | [] -> Error $"Invalid x64 instruction '{line}'"
                            | parser :: rest ->
                                match parser () with
                                | Error msg -> Error msg
                                | Ok (Some instruction) -> Ok instruction
                                | Ok None -> choose rest
                        choose parsers |> withLine

let parseX64 (text: string) : Result<Instr list, string> =
    let lines =
        text.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n')
        |> Array.mapi (fun index line -> index + 1, line.Trim())
        |> Array.filter (fun (_, line) -> line <> "" && not (line.StartsWith("//")))
        |> Array.toList

    let rec loop parsed remaining =
        match remaining with
        | [] -> Ok (List.rev parsed)
        | (lineNumber, line) :: rest ->
            match parseLine lineNumber line with
            | Ok instruction -> loop (instruction :: parsed) rest
            | Error msg -> Error msg

    match lines with
    | [] -> Error "INPUT-X64 contains no instructions"
    | _ -> loop [] lines
