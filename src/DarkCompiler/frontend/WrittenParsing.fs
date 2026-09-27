// WrittenParsing.fs - Enter the copied interpreter parser for executable source units.

module WrittenParsing

let parse
    (mode: LibParser.Validation.Mode)
    (source: string)
    : Result<LibParser.Validation.ValidatedSourceFile, string> =
    LibParser.Parser.parseFor mode source
    |> Result.mapError (fun diagnostics ->
        diagnostics
        |> List.map (LibParser.Parser.renderDiagnostic source)
        |> String.concat "\n")
