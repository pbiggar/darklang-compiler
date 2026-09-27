// WrittenSource.fs - Preserve interpreter declarations and module scopes for direct source checking.

module WrittenSource

module WT = LibParser.WrittenTypes

/// The checker receives source syntax with its scope, without constructing a
/// separate untyped semantic program.
type Item =
    | Function of modulePath:string list * WT.FnDecl
    | Value of modulePath:string list * WT.ValueDecl
    | Type of modulePath:string list * WT.TypeDecl
    | Expression of modulePath:string list * WT.Expr

let private moduleSegments (name: string) : Result<string list, string> =
    match NameSyntax.tryParseLegacySpelling name with
    | Some qualified ->
        qualified
        |> NameSyntax.segments
        |> List.map NameSyntax.identifierText
        |> Ok
    | None -> Error $"Invalid parsed module name '{name}'"

let rec private flattenDeclaration
    (modulePath: string list)
    (declaration: WT.Declaration)
    : Result<Item list, string> =
    match declaration with
    | WT.DFunction fn -> Ok [Function (modulePath, fn)]
    | WT.DValue value -> Ok [Value (modulePath, value)]
    | WT.DType typ -> Ok [Type (modulePath, typ)]
    | WT.DExpr expr -> Ok [Expression (modulePath, expr)]
    | WT.DModule modul ->
        moduleSegments (snd modul.name)
        |> Result.bind (fun segments ->
            flattenDeclarations (modulePath @ segments) modul.declarations)
    | WT.DTypeDB _ -> Error "Test-only database declaration in executable source"
    | WT.DTest _ -> Error "Test assertion in executable source"

and private flattenDeclarations
    (modulePath: string list)
    (declarations: WT.Declaration list)
    : Result<Item list, string> =
    declarations
    |> List.fold (fun state declaration ->
        state
        |> Result.bind (fun reversed ->
            flattenDeclaration modulePath declaration
            |> Result.map (fun current -> List.rev current @ reversed))) (Ok [])
    |> Result.map List.rev

let items (validated: LibParser.Validation.ValidatedSourceFile) : Result<Item list, string> =
    let source = LibParser.Validation.ValidatedSourceFile.toWrittenTypes validated
    flattenDeclarations [] source.declarations
    |> Result.map (fun declarations ->
        declarations
        @ (source.exprsToEval |> List.map (fun expr -> Expression ([], expr))))

/// Enforce source-unit entry ownership before composing declarations for checking.
let validateSourceUnits
    (requireEntry: bool)
    (units: (string * NameSyntax.SourceUnitPurpose * LibParser.Validation.ValidatedSourceFile) list)
    : Result<LibParser.Validation.ValidatedSourceFile list, string> =
    units
    |> ResultList.traverse (fun (name, purpose, source) ->
        items source
        |> Result.bind (fun declarations ->
            let count =
                declarations
                |> List.sumBy (function Expression _ -> 1 | _ -> 0)
            if count > 0 && purpose <> NameSyntax.SourceUnitPurpose.Executable then
                Error $"Source unit '{name}' has {count} executable entry expression(s), but {purpose} units must contain declarations only"
            else Ok (source, count)))
    |> Result.bind (fun validated ->
        let count = validated |> List.sumBy snd
        if requireEntry && count <> 1 then
            Error $"Executable program must contain exactly one entry expression; found {count}"
        elif not requireEntry && count <> 0 then
            Error $"Declaration-only program must not contain entry expressions; found {count}"
        else Ok (validated |> List.map fst))

let private qualifiedFn (name: WT.QualifiedFnIdentifier) =
    name.modules |> List.map (fun (identifier, _) -> identifier.name)
    |> fun modules -> String.concat "." (modules @ [name.fn.name])

let private qualifiedType (name: WT.QualifiedTypeIdentifier) =
    name.modules |> List.map (fun (identifier, _) -> identifier.name)
    |> fun modules -> String.concat "." (modules @ [name.typ.name])

let rec private typeNames reference =
    match reference with
    | WT.TCustom name ->
        qualifiedType name :: (name.typeArgs |> List.collect typeNames)
    | WT.TList (_, _, _, inner, _) -> typeNames inner
    | WT.TDict (_, _, _, key, _, value, _) -> typeNames key @ typeNames value
    | WT.TTuple (_, first, _, second, rest, _, _) ->
        [first; second] @ (rest |> List.map snd) |> List.collect typeNames
    | WT.TFn (_, arguments, result) ->
        (arguments |> List.map fst |> List.collect typeNames) @ typeNames result
    | _ -> []

let rec expressionNames expression =
    let many expressions = List.collect expressionNames expressions
    match expression with
    | WT.EFnName (_, name) -> [qualifiedFn name]
    | WT.EVariable (_, name) -> [name]
    | WT.EApply (_, target, types, arguments) ->
        expressionNames target @ (types |> List.collect typeNames) @ many arguments
    | WT.EInfix (_, _, left, right)
    | WT.EStatement (_, left, right) -> many [left; right]
    | WT.ELet (_, _, value, body, _, _) -> many [value; body]
    | WT.EIf (_, condition, thenBranch, elseBranch, _, _, _) ->
        many ([condition; thenBranch] @ (elseBranch |> Option.toList))
    | WT.EList (_, elements, _, _) -> elements |> List.map fst |> many
    | WT.ETuple (_, first, _, second, rest, _, _) ->
        [first; second] @ (rest |> List.map snd) |> many
    | WT.ERecordFieldAccess (_, record, _, _) -> expressionNames record
    | WT.ELambda (_, _, body, _, _) -> expressionNames body
    | WT.ERecord (_, name, fields, _, _) ->
        qualifiedType name ::
        ((name.typeArgs |> List.collect typeNames)
         @ (fields |> List.map (fun (_, _, value) -> value) |> many))
    | WT.EDict (_, entries, _, _, _) ->
        entries |> List.collect (fun (_, key, _, value) -> many [key; value])
    | WT.ERecordUpdate (_, record, updates, _, _, _) ->
        expressionNames record
        @ (updates |> List.map (fun (_, _, value) -> value) |> many)
    | WT.EEnum (_, name, _, fields, _) ->
        qualifiedType name ::
        ((name.typeArgs |> List.collect typeNames) @ many fields)
    | WT.EMatch (_, scrutinee, cases, _, _) ->
        expressionNames scrutinee
        @ (cases
           |> List.collect (fun arm ->
               (arm.whenCondition |> Option.map (snd >> expressionNames) |> Option.defaultValue [])
               @ expressionNames arm.rhs))
    | WT.EPipe (_, initial, segments) ->
        expressionNames initial
        @ (segments
           |> List.collect (fun (_, segment) ->
               match segment with
               | WT.EPipeInfix (_, _, value) -> expressionNames value
               | WT.EPipeLambda (_, _, body, _, _) -> expressionNames body
               | WT.EPipeEnum (_, name, _, fields, _) ->
                   qualifiedType name ::
                   ((name.typeArgs |> List.collect typeNames) @ many fields)
               | WT.EPipeFnCall (_, name, typeArgs, args) ->
                   qualifiedFn name ::
                   ((typeArgs |> List.collect typeNames) @ many args)
               | WT.EPipeVariableOrFnCall (_, name) -> [name]))
    | WT.EString (_, _, segments, _, _) ->
        segments
        |> List.collect (function
            | WT.StringText _ -> []
            | WT.StringInterpolation (_, value, _, _) -> expressionNames value)
    | _ -> []

/// Package lookup needs qualified references from source, not a lowered AST.
let qualifiedNames (units: LibParser.Validation.ValidatedSourceFile list) : Result<string list, string> =
    units
    |> ResultList.traverse items
    |> Result.map (List.concat >> List.collect (function
        | Function (_, definition) ->
            (definition.parameters
             |> List.collect (function
                 | WT.FPUnit _ -> []
                 | WT.FPNormal (_, _, typ, _, _, _, _) -> typeNames typ))
            @ typeNames definition.returnType
            @ expressionNames definition.body
        | Value (_, definition) -> expressionNames definition.body
        | Type (_, definition) ->
            match definition.definition with
            | WT.TDAlias target -> typeNames target
            | WT.TDRecord fields ->
                fields |> List.collect (fun (field, _) -> typeNames field.typ)
            | WT.TDEnum cases ->
                cases
                |> List.collect (fun (_, caseSyntax) ->
                    caseSyntax.fields |> List.collect (fun field -> typeNames field.typ))
        | Expression (_, expression) -> expressionNames expression)
        >> List.filter (fun name -> name.Contains '.')
        >> List.distinct)
