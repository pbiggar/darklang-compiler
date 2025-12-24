// ANFParser.fs - Parser for ANF (A-Normal Form) DSL
//
// Parses human-readable ANF text into ANF.Program data structures.
//
// Example ANF:
//   let t0 = 3 * 4
//   let t1 = 2 + t0
//   return t1

module TestDSL.ANFParser

open System
open System.Text.RegularExpressions
open ANF
open TestDSL.Common

/// Parse temp ID from text like "t0", "t1", etc.
let parseTempId (text: string) : Result<TempId, string> =
    let m = Regex.Match(text.Trim(), @"^t(\d+)$")
    if m.Success then
        Ok (TempId (int m.Groups.[1].Value))
    else
        Error $"Invalid temp id '{text}' (expected 't0', 't1', etc.)"

/// Parse atom (number or temp variable)
let parseAtom (text: string) : Result<Atom, string> =
    let text = text.Trim()

    // Try temp ID first
    match parseTempId text with
    | Ok tid -> Ok (Var tid)
    | Error _ ->
        // Try parsing as number
        match Int64.TryParse(text) with
        | true, n -> Ok (IntLiteral n)
        | false, _ -> Error $"Invalid atom '{text}' (expected number or temp variable)"

/// Parse binary operator
let parseOp (text: string) : Result<BinOp, string> =
    match text.Trim() with
    | "+" -> Ok Add
    | "-" -> Ok Sub
    | "*" -> Ok Mul
    | "/" -> Ok Div
    | op -> Error $"Invalid operator '{op}' (expected +, -, *, or /)"

/// Parse complex expression (right side of let binding)
let parseCExpr (text: string) : Result<CExpr, string> =
    let text = text.Trim()

    // Try binary operation: "2 + 3" or "t0 * t1"
    let binopMatch = Regex.Match(text, @"^(.+?)\s*([+\-*/])\s*(.+)$")
    if binopMatch.Success then
        match parseAtom binopMatch.Groups.[1].Value with
        | Error e -> Error e
        | Ok left ->
            match parseOp binopMatch.Groups.[2].Value with
            | Error e -> Error e
            | Ok op ->
                match parseAtom binopMatch.Groups.[3].Value with
                | Error e -> Error e
                | Ok right -> Ok (Prim (op, left, right))
    else
        // Just an atom
        match parseAtom text with
        | Ok atom -> Ok (Atom atom)
        | Error e -> Error e

/// Parse ANF expression recursively
let rec parseAExpr (lineNum: int) (lines: string list) : Result<AExpr, string> =
    match lines with
    | [] -> Error "Unexpected end of input (expected 'return')"
    | line :: rest ->
        let line = line.Trim()

        // Try return pattern: "return t0"
        let retMatch = Regex.Match(line, @"^return\s+(.+)$")
        if retMatch.Success then
            match parseAtom retMatch.Groups.[1].Value with
            | Ok atom -> Ok (Return atom)
            | Error e -> Error $"Line {lineNum}: {e}"
        else
            // Try let pattern: "let t0 = 3 + 4"
            let letMatch = Regex.Match(line, @"^let\s+(t\d+)\s*=\s*(.+)$")
            if letMatch.Success then
                match parseTempId letMatch.Groups.[1].Value with
                | Error e -> Error $"Line {lineNum}: {e}"
                | Ok tid ->
                    match parseCExpr letMatch.Groups.[2].Value with
                    | Error e -> Error $"Line {lineNum}: {e}"
                    | Ok cexpr ->
                        match parseAExpr (lineNum + 1) rest with
                        | Error e -> Error e
                        | Ok body -> Ok (Let (tid, cexpr, body))
            else
                Error $"Line {lineNum}: Expected 'let' or 'return', got: {line}"

/// Parse ANF program from text
let parseANF (text: string) : Result<ANF.Program, string> =
    let lines = stripCommentsAndEmpty text
    match parseAExpr 1 lines with
    | Ok expr -> Ok (ANF.Program ([], Some expr))  // No functions, just main expression
    | Error e -> Error e
