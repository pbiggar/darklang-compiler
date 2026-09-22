// Printing.fs - Escape values and select functions for representation-local printers.

module IRPrinting

open ANF
open MIR
open LIR

let internal escapeStringContent (input: string) : string =
    input
    |> String.collect (fun c ->
        match c with
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | '\000' -> "\\0"
        | _ -> string c)

/// Append a type suffix when available
let internal appendTypeSuffix (typOpt: AST.SemanticType option) (value: string) : string =
    match typOpt with
    | None -> value
    | Some typ -> $"{value} : {typ}"

let internal commaSeparated (printer: 'a -> string) (values: 'a list) : string =
    values |> List.map printer |> String.concat ", "

let internal functionNameMatches (filter: string option) (name: string) : bool =
    match filter with
    | None -> true
    | Some pattern -> name.Contains(pattern, System.StringComparison.OrdinalIgnoreCase)

let internal noFunctionMatchText (filter: string option) : string =
    match filter with
    | Some pattern -> $"No functions matched '{pattern}'."
    | None -> "Functions: 0"
