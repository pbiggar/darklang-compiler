// WrittenFormatter.fs - Conservative formatting for validated interpreter syntax.

module WrittenFormatter

open System
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

module WT = LibParser.WrittenTypes

/// A syntax fingerprint without source positions. Reparse checks use this
/// instead of comparing WrittenTypes ranges, which change when text is formatted.
let rec private fingerprint (value: obj) : string =
    if isNull value then "null"
    else
        let typ = value.GetType()
        if typ = typeof<LibParser.Tokenizer.TokenRange>
           || typ = typeof<LibParser.Tokenizer.Pos> then
            ""
        elif FSharpType.IsTuple typ then
            FSharpValue.GetTupleFields value
            |> Array.map fingerprint
            |> String.concat ","
            |> fun fields -> $"({fields})"
        elif FSharpType.IsRecord (typ, true) then
            FSharpValue.GetRecordFields (value, true)
            |> Array.map fingerprint
            |> String.concat ";"
            |> fun fields -> $"{{{fields}}}"
        elif FSharpType.IsUnion (typ, true) then
            let case, fields = FSharpValue.GetUnionFields (value, typ, true)
            fields
            |> Array.map fingerprint
            |> String.concat ","
            |> fun contents -> $"{case.Name}({contents})"
        elif typ.IsArray then
            (value :?> Array)
            |> Seq.cast<obj>
            |> Seq.map fingerprint
            |> String.concat ","
            |> fun contents -> $"[{contents}]"
        else
            sprintf "%A" value

let syntaxKey (sourceFile: WT.SourceFile) : string =
    fingerprint (box sourceFile)

let private parseSource (source: string) : WT.SourceFile option =
    WrittenParsing.parse LibParser.Validation.Script source
    |> Result.toOption
    |> Option.map LibParser.Validation.ValidatedSourceFile.toWrittenTypes

/// Preserve the interpreter's accepted layout while normalizing Unicode and
/// redundant parentheses around atomic application arguments. A candidate is
/// used only when the interpreter reparses it to the same range-free syntax tree.
let format (source: string) (parsed: WT.SourceFile) : string =
    let originalKey = syntaxKey parsed
    let accept candidate fallback =
        match parseSource candidate with
        | Some reparsed when syntaxKey reparsed = originalKey -> candidate
        | _ -> fallback
    let normalized = source.Normalize NormalizationForm.FormC |> fun candidate -> accept candidate source
    let candidate =
        Regex.Replace(
            normalized,
            @"\(([A-Za-z_][A-Za-z0-9_]*|-?[0-9]+L?)\)",
            MatchEvaluator(fun matched -> matched.Groups.[1].Value))
    accept candidate normalized
