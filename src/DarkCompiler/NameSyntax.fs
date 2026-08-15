// NameSyntax.fs - Shared lexical and structural contract for source names.
//
// This module is the single parser-facing authority for identifier characters,
// reserved words, blank names, quoted identifiers, and qualified segments.

module NameSyntax

open AST

[<StructuralEquality; StructuralComparison>]
type Identifier =
    | OrdinaryIdentifier of string
    | BlankIdentifier

[<StructuralEquality; StructuralComparison>]
type QualifiedName = private QualifiedName of NonEmptyList<Identifier>

[<RequireQualifiedAccess>]
type Keyword =
    | Let
    | Val
    | In
    | If
    | Elif
    | Then
    | Else
    | Type
    | Of
    | Match
    | With
    | Fun
    | When
    | True
    | False
    | Underscore

type IdentifierToken =
    | IdentifierToken of Identifier
    | KeywordToken of Keyword

/// Parser output before compiler-specific top-level normalization. Names retain
/// their lexical role here; the legacy AST string boundary is crossed only by
/// `normalizeSource`.
type SourceDeclaration =
    | SourceFunction of Identifier * FunctionDef
    | SourceType of Identifier * TypeDef
    | SourceValue of Identifier * Expr
    | SourceNestedModule of QualifiedName * ParsedSource
    | SourceExpression of Expr

and ParsedSource =
    | SourceDeclarations of NonEmptyList<SourceDeclaration>
    | SourceModule of QualifiedName * ParsedSource

/// Why a caller supplied a source unit. Only executable units may contribute
/// an entry expression; library and package units are declarations-only.
[<RequireQualifiedAccess>]
type SourceUnitPurpose =
    | Executable
    | Library
    | Package

/// Stable caller-supplied identity for an independently parsed source unit.
[<StructuralEquality; StructuralComparison>]
type SourceUnitName = private SourceUnitName of string

let sourceUnitName (name: string) : Result<SourceUnitName, string> =
    if System.String.IsNullOrWhiteSpace name then Error "Source unit name must not be empty"
    else Ok (SourceUnitName name)

let sourceUnitNameText (SourceUnitName name) : string = name

type ParsedSourceUnit = {
    Name: SourceUnitName
    Purpose: SourceUnitPurpose
    Source: ParsedSource
}

/// A source-accurate program. Unit and declaration order are observable and
/// retained until whole-program validation has selected an entry.
type SourceProgram = SourceProgram of NonEmptyList<ParsedSourceUnit>

type EntryCandidate = {
    SourceUnit: SourceUnitName
    ModulePath: QualifiedName list
    DeclarationIndex: int
    Expression: Expr
}

/// Executable program after whole-program entry validation. The entry is not
/// optional, so backend entry synthesis cannot invent a fallback.
type ValidatedExecutableProgram = private {
    SourceProgram: SourceProgram
    Entry: EntryCandidate
}

let validatedSourceProgram (program: ValidatedExecutableProgram) : SourceProgram =
    program.SourceProgram

let validatedEntry (program: ValidatedExecutableProgram) : EntryCandidate =
    program.Entry

let isStartCharacter (character: char) : bool =
    System.Char.IsLetter character || character = '_'

let isContinueCharacter (character: char) : bool =
    System.Char.IsLetterOrDigit character || character = '_' || character = '\''

let identifierText (identifier: Identifier) : string =
    match identifier with
    | OrdinaryIdentifier text -> text
    | BlankIdentifier -> ""

let identifierFromText (text: string) : Identifier =
    if text = "" || text = "___" then BlankIdentifier else OrdinaryIdentifier text

let classify (text: string) : IdentifierToken =
    match text with
    | "let" -> KeywordToken Keyword.Let
    | "val" -> KeywordToken Keyword.Val
    | "in" -> KeywordToken Keyword.In
    | "if" -> KeywordToken Keyword.If
    | "elif" -> KeywordToken Keyword.Elif
    | "then" -> KeywordToken Keyword.Then
    | "else" -> KeywordToken Keyword.Else
    | "type" -> KeywordToken Keyword.Type
    | "of" -> KeywordToken Keyword.Of
    | "match" -> KeywordToken Keyword.Match
    | "with" -> KeywordToken Keyword.With
    | "fun" -> KeywordToken Keyword.Fun
    | "when" -> KeywordToken Keyword.When
    | "true" -> KeywordToken Keyword.True
    | "false" -> KeywordToken Keyword.False
    | "_" -> KeywordToken Keyword.Underscore
    | "___" -> IdentifierToken BlankIdentifier
    | ordinary -> IdentifierToken (identifierFromText ordinary)

let reservedWords : Set<string> =
    set [ "let"; "val"; "in"; "if"; "elif"; "then"; "else"; "type"; "of"
          "match"; "with"; "fun"; "when"; "true"; "false"; "_" ]

let isBareIdentifier (identifier: Identifier) : bool =
    match identifier with
    | BlankIdentifier -> false
    | OrdinaryIdentifier text ->
        text.Length > 0
        && isStartCharacter text[0]
        && (text |> Seq.forall isContinueCharacter)
        && not (Set.contains text reservedWords)

let formatIdentifier (identifier: Identifier) : string =
    match identifier with
    | BlankIdentifier -> "___"
    | OrdinaryIdentifier text when isBareIdentifier identifier -> text
    | OrdinaryIdentifier text -> $"``{text}``"

let singleton (identifier: Identifier) : QualifiedName =
    QualifiedName (NonEmptyList.singleton identifier)

let fromNonEmptySegments (value: NonEmptyList<Identifier>) : QualifiedName = QualifiedName value

let append (identifier: Identifier) (QualifiedName segments) : QualifiedName =
    QualifiedName (NonEmptyList.snoc segments identifier)

let concat (QualifiedName first) (QualifiedName second) : QualifiedName =
    QualifiedName (NonEmptyList.appendList first (NonEmptyList.toList second))

let segments (QualifiedName value) : Identifier list = NonEmptyList.toList value

let trySplitLast (name: QualifiedName) : (QualifiedName * Identifier) option =
    match segments name |> List.rev with
    | last :: reversedPrefix ->
        reversedPrefix
        |> List.rev
        |> NonEmptyList.tryFromList
        |> Option.map (fun prefix -> (fromNonEmptySegments prefix, last))
    | [] -> None

let formatQualifiedName (name: QualifiedName) : string =
    name |> segments |> List.map formatIdentifier |> String.concat "."

/// The legacy compiler AST still consumes a string at the parse/resolution
/// boundary. Keep quoted segment delimiters in that string so embedded dots are
/// lossless; NameResolution parses this representation back into segments.
let toLegacySpelling (name: QualifiedName) : string = formatQualifiedName name

let tryParseLegacySpelling (spelling: string) : QualifiedName option =
    let length = spelling.Length
    let rec parseQuoted index chars =
        if index + 1 >= length then None
        elif spelling[index] = '`' && spelling[index + 1] = '`' then
            Some (System.String(List.rev chars |> List.toArray), index + 2)
        else
            parseQuoted (index + 1) (spelling[index] :: chars)
    let rec parseBare index chars =
        if index >= length || spelling[index] = '.' then
            match chars with
            | [] -> None
            | _ -> Some (System.String(List.rev chars |> List.toArray), index)
        else
            parseBare (index + 1) (spelling[index] :: chars)
    let rec loop index acc =
        if index >= length then
            match List.rev acc |> NonEmptyList.tryFromList with
            | Some parsed -> Some (QualifiedName parsed)
            | None -> None
        else
            let parsedSegment =
                if index + 1 < length && spelling[index] = '`' && spelling[index + 1] = '`' then
                    parseQuoted (index + 2) []
                else
                    parseBare index []
            match parsedSegment with
            | None -> None
            | Some (text, nextIndex) ->
                let identifier = identifierFromText text
                if nextIndex = length then loop nextIndex (identifier :: acc)
                elif spelling[nextIndex] = '.' && nextIndex + 1 < length then
                    loop (nextIndex + 1) (identifier :: acc)
                else
                    None
    loop 0 []

let scanOrdinary (input: string) (startIndex: int) : Identifier * int =
    let rec findEnd index =
        if index < input.Length && isContinueCharacter input[index] then findEnd (index + 1)
        else index
    let endIndex = findEnd (startIndex + 1)
    (identifierFromText (input.Substring(startIndex, endIndex - startIndex)), endIndex)

let scanQuoted (input: string) (startIndex: int) : Result<Identifier * int, string> =
    let rec findClose index =
        if index >= input.Length || input[index] = '\n' || input[index] = '\r' then
            Error "Unterminated backtick identifier"
        elif index + 1 < input.Length && input[index] = '`' && input[index + 1] = '`' then
            let text = input.Substring(startIndex + 2, index - startIndex - 2)
            Ok (identifierFromText text, index + 2)
        else
            findClose (index + 1)
    findClose (startIndex + 2)

let tryExtractModuleHeader (source: string) : (QualifiedName * string) option =
    let lines = source.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n') |> Array.toList
    let rec findHeader (prefix: string list) (remaining: string list) =
        match remaining with
        | [] -> None
        | line :: rest ->
            let trimmed = line.Trim()
            if trimmed = "" || trimmed.StartsWith("//") then
                findHeader (line :: prefix) rest
            elif trimmed.StartsWith("module ") then
                let isBlock = trimmed.EndsWith("=")
                let moduleText = trimmed.Substring("module ".Length).Trim()
                let spelling =
                    if isBlock then moduleText.Substring(0, moduleText.Length - 1).Trim()
                    else moduleText
                let body =
                    if isBlock then
                        let significantLines = rest |> List.filter (fun bodyLine -> bodyLine.Trim() <> "")
                        let indentation (sourceLine: string) =
                            sourceLine.Length - sourceLine.TrimStart().Length
                        match significantLines |> List.map indentation |> List.sort with
                        | bodyIndent :: _ when bodyIndent > indentation line ->
                            rest
                            |> List.map (fun bodyLine ->
                                if bodyLine.Trim() = "" then ""
                                elif bodyLine.Length >= bodyIndent then bodyLine.Substring bodyIndent
                                else bodyLine)
                            |> String.concat "\n"
                        | _ -> ""
                    else
                        String.concat "\n" (List.rev prefix @ rest)
                tryParseLegacySpelling spelling |> Option.map (fun name -> (name, body))
            else
                None
    findHeader [] lines

let wrapModules (modules: QualifiedName list) (source: ParsedSource) : ParsedSource =
    List.foldBack (fun moduleName body -> SourceModule (moduleName, body)) modules source

let private entryCandidatesInUnit (unit': ParsedSourceUnit) : EntryCandidate list =
    let rec collect modulePath nextIndex parsed =
        match parsed with
        | SourceModule (moduleName, body) -> collect (modulePath @ [moduleName]) nextIndex body
        | SourceDeclarations declarations ->
            declarations
            |> NonEmptyList.toList
            |> List.fold (fun (index, entries) declaration ->
                match declaration with
                | SourceExpression expression ->
                    (index + 1,
                     entries @
                        [{ SourceUnit = unit'.Name
                           ModulePath = modulePath
                           DeclarationIndex = index
                           Expression = expression }])
                | SourceNestedModule (moduleName, body) ->
                    let (next, nestedEntries) = collect (modulePath @ [moduleName]) (index + 1) body
                    (next, entries @ nestedEntries)
                | SourceFunction _ | SourceType _ | SourceValue _ -> (index + 1, entries)) (nextIndex, [])
    collect [] 0 unit'.Source |> snd

let sourceUnits (SourceProgram units) : ParsedSourceUnit list = NonEmptyList.toList units

let createSourceProgram (units: NonEmptyList<ParsedSourceUnit>) : SourceProgram = SourceProgram units

/// Select exactly one executable entry after checking every unit. Entries in
/// dependency units are always errors, even when an executable entry exists.
let validateExecutableProgram (program: SourceProgram) : Result<ValidatedExecutableProgram, string> =
    let units = sourceUnits program
    let invalidDependencyEntry =
        units
        |> List.tryPick (fun unit' ->
            match unit'.Purpose, entryCandidatesInUnit unit' with
            | SourceUnitPurpose.Executable, _ | _, [] -> None
            | purpose, entries ->
                Some
                    $"Source unit '{sourceUnitNameText unit'.Name}' has {entries.Length} executable entry expression(s), but {purpose} units must contain declarations only")
    match invalidDependencyEntry with
    | Some error -> Error error
    | None ->
        let entries =
            units
            |> List.collect (fun unit' ->
                match unit'.Purpose with
                | SourceUnitPurpose.Executable -> entryCandidatesInUnit unit'
                | SourceUnitPurpose.Library | SourceUnitPurpose.Package -> [])
        match entries with
        | [entry] -> Ok { SourceProgram = program; Entry = entry }
        | [] -> Error "Executable program must contain exactly one entry expression; found 0"
        | _ -> Error $"Executable program must contain exactly one entry expression; found {entries.Length}"

/// Validate a declaration-only composition without manufacturing an entry.
let validateDeclarationProgram (program: SourceProgram) : Result<SourceProgram, string> =
    match sourceUnits program |> List.collect entryCandidatesInUnit with
    | [] -> Ok program
    | entries -> Error $"Declaration-only program must not contain entry expressions; found {entries.Length}"
/// Assign stable structural identities after parsing, while declaration and
/// lexical boundaries are still explicit. Later passes may change group IDs,
/// but never recreate binding/member identity from a spelling.
let private assignParsedRecursiveIdentities (Program topLevels) : Program =
    let parsedMember boundary path (candidate: RecursiveCandidate) : RecursiveBindingInfo =
        ParsedRecursiveBinding {
            Binding = bindingId path
            Boundary = scopeBoundaryId boundary
            Member = recursiveMemberId path
            SourceName = candidate.SourceName
            Kind = candidate.Kind
        }

    let rec assignExpr boundary path expr =
        let child index value = assignExpr boundary (path @ [index]) value
        let mapArgs start args =
            args
            |> NonEmptyList.toList
            |> List.mapi (fun index value -> child (start + index) value)
            |> NonEmptyList.fromList
        match expr with
        | RecursiveLet (RecursiveBindingCandidate candidate, value, body) ->
            let nestedBoundary = path
            RecursiveLet (
                parsedMember boundary path candidate,
                assignExpr nestedBoundary (path @ [0]) value,
                assignExpr nestedBoundary (path @ [1]) body
            )
        | RecursiveLet (recursion, value, body) ->
            RecursiveLet (recursion, child 0 value, child 1 body)
        | Let (pattern, value, body) -> Let (pattern, child 0 value, child 1 body)
        | BoundaryRender (renderer, value) -> BoundaryRender (renderer, child 0 value)
        | BinOp (op, left, right) -> BinOp (op, child 0 left, child 1 right)
        | UnaryOp (op, value) -> UnaryOp (op, child 0 value)
        | If (condition, thenBranch, elseBranch) ->
            If (child 0 condition, child 1 thenBranch, child 2 elseBranch)
        | Sequence (first, next) -> Sequence (child 0 first, child 1 next)
        | Call (name, args) -> Call (name, mapArgs 0 args)
        | TypeApp (name, types, args) -> TypeApp (name, types, mapArgs 0 args)
        | TupleLiteral values -> TupleLiteral (values |> List.mapi child)
        | TupleAccess (tuple, index) -> TupleAccess (child 0 tuple, index)
        | DictLiteral (typ, entries) ->
            DictLiteral (typ, entries |> List.mapi (fun index (key, value) -> (key, child index value)))
        | RecordLiteral (name, fields) ->
            RecordLiteral (name, fields |> List.mapi (fun index (field, value) -> (field, child index value)))
        | RecordUpdate (record, fields) ->
            RecordUpdate (child 0 record, fields |> List.mapi (fun index (field, value) -> (field, child (index + 1) value)))
        | RecordAccess (record, field) -> RecordAccess (child 0 record, field)
        | Constructor (reference, name, payload) -> Constructor (reference, name, payload |> Option.map (child 0))
        | Match (scrutinee, cases) ->
            Match (
                child 0 scrutinee,
                cases
                |> List.mapi (fun caseIndex case ->
                    let casePath = path @ [caseIndex + 1]
                    { case with
                        Guard = case.Guard |> Option.map (assignExpr casePath (casePath @ [0]))
                        Body = assignExpr casePath (casePath @ [1]) case.Body })
            )
        | ListLiteral values -> ListLiteral (values |> List.mapi child)
        | Lambda (parameters, returnAnnotation, body) ->
            let lambdaBoundary = path
            Lambda (parameters, returnAnnotation, assignExpr lambdaBoundary (path @ [0]) body)
        | Apply (func, args) -> Apply (child 0 func, mapArgs 1 args)
        | IndirectApply (func, args) -> IndirectApply (child 0 func, mapArgs 1 args)
        | Closure (name, captures) -> Closure (name, captures |> List.mapi child)
        | InterpolatedString parts ->
            InterpolatedString (
                parts
                |> List.mapi (fun index part ->
                    match part with
                    | StringText _ -> part
                    | StringExpr value -> StringExpr (child index value))
            )
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
        | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
        | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
        | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
        | Var _ | FuncRef _ | RuntimeError _ -> expr

    let assignTopLevel index topLevel =
        let path = [index]
        match topLevel with
        | FunctionDef funcDef ->
            let recursion =
                match funcDef.Recursion with
                | Some (RecursiveBindingCandidate candidate) -> Some (parsedMember [] path candidate)
                | other -> other
            FunctionDef { funcDef with Body = assignExpr path (path @ [0]) funcDef.Body; Recursion = recursion }
        | Expression expr -> Expression (assignExpr path (path @ [0]) expr)
        | TypeDef _ -> topLevel

    Program (topLevels |> List.mapi assignTopLevel)

let normalizeSource (source: ParsedSource) : Result<Program, string> =
    let nameAtPrefix prefix identifier =
        match prefix with
        | None -> identifierText identifier
        | Some moduleName -> moduleName |> append identifier |> toLegacySpelling
    let normalizeTypeName prefix identifier typeDef =
        let normalizedName = nameAtPrefix prefix identifier
        match typeDef with
        | RecordDef (_, typeParams, fields) -> RecordDef (normalizedName, typeParams, fields)
        | SumTypeDef (_, typeParams, variants) -> SumTypeDef (normalizedName, typeParams, variants)
        | TypeAlias (_, typeParams, targetType) -> TypeAlias (normalizedName, typeParams, targetType)
    let rec normalize prefix parsed =
        match parsed with
        | SourceModule (moduleName, body) ->
            let fullModule = prefix |> Option.map (fun outer -> concat outer moduleName) |> Option.defaultValue moduleName
            normalize (Some fullModule) body
        | SourceDeclarations declarations ->
            let rec declarationsToProgram acc remaining =
                match remaining with
                | [] -> Ok (Program (List.rev acc))
                | SourceFunction (identifier, definition) :: rest ->
                    let normalized = { definition with Name = nameAtPrefix prefix identifier }
                    declarationsToProgram (FunctionDef normalized :: acc) rest
                | SourceType (identifier, definition) :: rest ->
                    let normalized = normalizeTypeName prefix identifier definition
                    declarationsToProgram (TypeDef normalized :: acc) rest
                | SourceExpression expression :: rest ->
                    declarationsToProgram (Expression expression :: acc) rest
                | SourceNestedModule (moduleName, body) :: rest ->
                    let nestedPrefix =
                        prefix
                        |> Option.map (fun outer -> concat outer moduleName)
                        |> Option.defaultValue moduleName
                    normalize (Some nestedPrefix) body
                    |> Result.bind (fun (Program nestedItems) ->
                        declarationsToProgram (List.rev nestedItems @ acc) rest)
                | SourceValue _ :: _ ->
                    Error "Top-level value declarations are parsed but native execution is not supported"
            declarations |> NonEmptyList.toList |> declarationsToProgram []
    normalize None source |> Result.map assignParsedRecursiveIdentities
