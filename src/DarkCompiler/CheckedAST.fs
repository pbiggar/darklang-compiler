// CheckedAST.fs - Phase-safe syntax accepted by compiler preparation and ANF lowering.
//
// The parser/checker implementation still uses AST internally while resolving
// and inferring source syntax.  Successful checking crosses this boundary once;
// downstream passes cannot represent missing lambda types, unresolved nominal
// references, unchecked values, or partially resolved recursion metadata.

module CheckedAST

open ResultList

type LetPattern =
    | LPUnit
    | LPWildcard
    | LPVariable of string
    | LPTuple of first:LetPattern * second:LetPattern * rest:LetPattern list

type LambdaParameter = {
    Pattern: LetPattern
    Type: AST.Type
}

type RecordReference = {
    TypeName: string
    TypeArgs: AST.Type list
}

type ConstructorReference = {
    TypeName: string
}

type StringPart =
    | StringText of string
    | StringExpr of Expr

and Expr =
    | UnitLiteral
    | Int64Literal of int64
    | Int128Literal of System.Int128
    | Int8Literal of sbyte
    | Int16Literal of int16
    | Int32Literal of int32
    | UInt8Literal of byte
    | UInt16Literal of uint16
    | UInt32Literal of uint32
    | UInt64Literal of uint64
    | UInt128Literal of System.UInt128
    | BigIntLiteral of System.Numerics.BigInteger
    | BoolLiteral of bool
    | StringLiteral of string
    | CharLiteral of string
    | FloatLiteral of float
    | InterpolatedString of StringPart list
    | BinOp of AST.BinOp * Expr * Expr
    | UnaryOp of AST.UnaryOp * Expr
    | Let of pattern:LetPattern * value:Expr * body:Expr
    | RecursiveLet of recursion:AST.TypedRecursiveMember * value:Expr * body:Expr
    | Var of string
    | If of cond:Expr * thenBranch:Expr * elseBranch:Expr
    | Sequence of first:Expr * next:Expr
    | Call of funcName:string * args:AST.NonEmptyList<Expr>
    | TypeApp of funcName:string * typeArgs:AST.Type list * args:AST.NonEmptyList<Expr>
    | TupleLiteral of Expr list
    | TupleAccess of tuple:Expr * index:int
    | DictLiteral of keyType:AST.Type * valueType:AST.Type * entries:(Expr * Expr) list
    | RecordLiteral of reference:RecordReference * fields:(string * Expr) list
    | RecordUpdate of record:Expr * updates:(string * Expr) list
    | RecordAccess of record:Expr * fieldName:string
    | Constructor of reference:ConstructorReference * variantName:string * fields:Expr list
    | Match of scrutinee:Expr * cases:MatchCase list
    | ListLiteral of Expr list
    | Lambda of parameters:AST.NonEmptyList<LambdaParameter> * returnAnnotation:AST.Type option * body:Expr
    | Apply of func:Expr * args:AST.NonEmptyList<Expr>
    | IndirectApply of func:Expr * args:AST.NonEmptyList<Expr>
    | FuncRef of funcName:string
    | Closure of funcName:string * captures:Expr list
    | RuntimeError of message:string
    | BoundaryRender of renderer:string * value:Expr

and MatchCase = {
    Patterns: AST.NonEmptyList<AST.Pattern>
    Guard: Expr option
    Body: Expr
}

type FunctionDef = {
    Name: string
    TypeParams: string list
    Params: AST.NonEmptyList<string * AST.Type>
    ReturnType: AST.Type
    Body: Expr
    Recursion: AST.TypedRecursiveMember option
}

type ValueDef = {
    Name: string
    Type: AST.Type
    Body: Expr
}

type TopLevel =
    | FunctionDef of FunctionDef
    | TypeDef of AST.TypeDef
    | ValueDef of ValueDef
    | Expression of Expr

type Program = Program of TopLevel list

let valueDefName (valueDef: ValueDef) : string = valueDef.Name

let valueDefBody (valueDef: ValueDef) : Expr = valueDef.Body

let programValues (Program topLevels) : Map<string, AST.Type * Expr> =
    topLevels
    |> List.choose (function
        | ValueDef valueDef -> Some (valueDef.Name, (valueDef.Type, valueDef.Body))
        | _ -> None)
    |> Map.ofList

let rec letPatternBindings (pattern: LetPattern) : string list =
    match pattern with
    | LPVariable name -> [name]
    | LPTuple (first, second, rest) ->
        first :: second :: rest |> List.collect letPatternBindings
    | LPUnit | LPWildcard -> []

let rec mapLetPatternBindings (f: string -> string) (pattern: LetPattern) : LetPattern =
    match pattern with
    | LPVariable name -> LPVariable (f name)
    | LPTuple (first, second, rest) ->
        LPTuple (
            mapLetPatternBindings f first,
            mapLetPatternBindings f second,
            rest |> List.map (mapLetPatternBindings f)
        )
    | LPUnit -> LPUnit
    | LPWildcard -> LPWildcard

let recursiveBindingName (memberInfo: AST.TypedRecursiveMember) : string =
    memberInfo.Resolved.Parsed.SourceName

let recursiveBindingId (memberInfo: AST.TypedRecursiveMember) : AST.BindingId =
    memberInfo.Resolved.Parsed.Binding

let recursiveBindingAvailability
    (memberInfo: AST.TypedRecursiveMember)
    : AST.RecursiveAvailability =
    memberInfo.Resolved.Availability

let constructorReferenceTypeName (reference: ConstructorReference) : string =
    reference.TypeName

let resolvedConstructorReference (typeName: string) : ConstructorReference =
    { TypeName = typeName }

let private conversionError location detail =
    Error $"Checked AST construction failed at {location}: {detail}"

let private map2 f first second =
    first
    |> Result.bind (fun firstValue ->
        second |> Result.map (fun secondValue -> f firstValue secondValue))

let rec private convertLetPattern (pattern: AST.LetPattern) : LetPattern =
    match pattern with
    | AST.LPUnit -> LPUnit
    | AST.LPWildcard -> LPWildcard
    | AST.LPVariable name -> LPVariable name
    | AST.LPTuple (first, second, rest) ->
        LPTuple (
            convertLetPattern first,
            convertLetPattern second,
            List.map convertLetPattern rest
        )

let private convertRecordReference (reference: AST.RecordReference) : RecordReference =
    { TypeName = reference.ResolvedTypeName; TypeArgs = reference.TypeArgs }

let private convertConstructorReference
    (location: string)
    (reference: AST.ConstructorReference)
    : Result<ConstructorReference, string> =
    match reference with
    | AST.ResolvedConstructor _ ->
        match AST.constructorReferenceTypeName reference with
        | Some typeName -> Ok { TypeName = typeName }
        | None -> conversionError location "resolved constructor has no declaring type"
    | AST.UnresolvedConstructor _ ->
        conversionError location "constructor reference was not resolved"

let rec ofTypedExpr (location: string) (expr: AST.Expr) : Result<Expr, string> =
    let convert = ofTypedExpr location
    let convertList values = values |> List.map convert |> sequenceResults
    let convertNonEmpty values =
        values
        |> AST.NonEmptyList.toList
        |> convertList
        |> Result.map AST.NonEmptyList.fromList
    let convertFields fields =
        fields
        |> List.map (fun (name, value) -> convert value |> Result.map (fun converted -> name, converted))
        |> sequenceResults
    let convertDictEntries entries =
        entries
        |> List.map (fun (key, value) ->
            map2 (fun convertedKey convertedValue -> convertedKey, convertedValue) (convert key) (convert value))
        |> sequenceResults
    match expr with
    | AST.UnitLiteral -> Ok UnitLiteral
    | AST.Int64Literal value -> Ok (Int64Literal value)
    | AST.Int128Literal value -> Ok (Int128Literal value)
    | AST.Int8Literal value -> Ok (Int8Literal value)
    | AST.Int16Literal value -> Ok (Int16Literal value)
    | AST.Int32Literal value -> Ok (Int32Literal value)
    | AST.UInt8Literal value -> Ok (UInt8Literal value)
    | AST.UInt16Literal value -> Ok (UInt16Literal value)
    | AST.UInt32Literal value -> Ok (UInt32Literal value)
    | AST.UInt64Literal value -> Ok (UInt64Literal value)
    | AST.UInt128Literal value -> Ok (UInt128Literal value)
    | AST.BigIntLiteral value -> Ok (BigIntLiteral value)
    | AST.BoolLiteral value -> Ok (BoolLiteral value)
    | AST.StringLiteral value -> Ok (StringLiteral value)
    | AST.CharLiteral value -> Ok (CharLiteral value)
    | AST.FloatLiteral value -> Ok (FloatLiteral value)
    | AST.InterpolatedString parts ->
        parts
        |> List.map (function
            | AST.StringText text -> Ok (StringText text)
            | AST.StringExpr inner -> convert inner |> Result.map StringExpr)
        |> sequenceResults
        |> Result.map InterpolatedString
    | AST.BinOp (op, left, right) ->
        map2 (fun left right -> BinOp (op, left, right)) (convert left) (convert right)
    | AST.UnaryOp (op, inner) -> convert inner |> Result.map (fun value -> UnaryOp (op, value))
    | AST.Let (pattern, value, body) ->
        map2
            (fun value body -> Let (convertLetPattern pattern, value, body))
            (convert value)
            (convert body)
    | AST.RecursiveLet (recursion, value, body) ->
        match recursion with
        | AST.TypedRecursiveBinding typed ->
            map2 (fun value body -> RecursiveLet (typed, value, body)) (convert value) (convert body)
        | _ -> conversionError location "recursive let has no typed recursion evidence"
    | AST.Var name -> Ok (Var name)
    | AST.If (condition, thenBranch, elseBranch) ->
        convert condition
        |> Result.bind (fun checkedCondition ->
            map2
                (fun thenBranch elseBranch -> If (checkedCondition, thenBranch, elseBranch))
                (convert thenBranch)
                (convert elseBranch))
    | AST.Sequence (first, next) ->
        map2 (fun first next -> Sequence (first, next)) (convert first) (convert next)
    | AST.Call (name, args) -> convertNonEmpty args |> Result.map (fun converted -> Call (name, converted))
    | AST.TypeApp (name, typeArgs, args) ->
        convertNonEmpty args |> Result.map (fun converted -> TypeApp (name, typeArgs, converted))
    | AST.TupleLiteral elements -> convertList elements |> Result.map TupleLiteral
    | AST.TupleAccess (tuple, index) -> convert tuple |> Result.map (fun value -> TupleAccess (value, index))
    | AST.DictLiteral (keyType, valueType, entries) ->
        convertDictEntries entries
        |> Result.map (fun converted -> DictLiteral (keyType, valueType, converted))
    | AST.RecordLiteral (reference, fields) ->
        convertFields fields
        |> Result.map (fun converted -> RecordLiteral (convertRecordReference reference, converted))
    | AST.RecordUpdate (record, updates) ->
        map2 (fun record updates -> RecordUpdate (record, updates)) (convert record) (convertFields updates)
    | AST.RecordAccess (record, fieldName) ->
        convert record |> Result.map (fun value -> RecordAccess (value, fieldName))
    | AST.Constructor (reference, variantName, fields) ->
        map2
            (fun reference fields -> Constructor (reference, variantName, fields))
            (convertConstructorReference location reference)
            (convertList fields)
    | AST.Match (scrutinee, cases) ->
        let convertCase (case: AST.MatchCase) : Result<MatchCase, string> =
            map2
                (fun guard body -> { Patterns = case.Patterns; Guard = guard; Body = body })
                (case.Guard |> Option.map convert |> sequenceOption)
                (convert case.Body)
        map2
            (fun scrutinee cases -> Match (scrutinee, cases))
            (convert scrutinee)
            (cases |> List.map convertCase |> sequenceResults)
    | AST.ListLiteral elements -> convertList elements |> Result.map ListLiteral
    | AST.Lambda (parameters, returnAnnotation, body) ->
        let convertParameter (parameter: AST.LambdaParameter) =
            match parameter.InferredType with
            | Some typ -> Ok { Pattern = convertLetPattern parameter.Pattern; Type = typ }
            | None -> conversionError location "lambda parameter has no inferred type"
        map2
            (fun parameters body -> Lambda (parameters, returnAnnotation, body))
            (parameters
             |> AST.NonEmptyList.toList
             |> List.map convertParameter
             |> sequenceResults
             |> Result.map AST.NonEmptyList.fromList)
            (convert body)
    | AST.Apply (func, args) ->
        map2 (fun func args -> Apply (func, args)) (convert func) (convertNonEmpty args)
    | AST.IndirectApply (func, args) ->
        map2 (fun func args -> IndirectApply (func, args)) (convert func) (convertNonEmpty args)
    | AST.FuncRef name -> Ok (FuncRef name)
    | AST.Closure (name, captures) -> convertList captures |> Result.map (fun values -> Closure (name, values))
    | AST.RuntimeError message -> Ok (RuntimeError message)
    | AST.BoundaryRender (renderer, value) ->
        convert value |> Result.map (fun converted -> BoundaryRender (renderer, converted))

let ofTypedFunction (funcDef: AST.FunctionDef) : Result<FunctionDef, string> =
    let recursion =
        match funcDef.Recursion with
        | None -> Ok None
        | Some (AST.TypedRecursiveBinding typed) -> Ok (Some typed)
        | Some _ -> conversionError $"function '{funcDef.Name}'" "function has no typed recursion evidence"
    map2
        (fun body recursion ->
            {
                Name = funcDef.Name
                TypeParams = funcDef.TypeParams
                Params = funcDef.Params
                ReturnType = funcDef.ReturnType
                Body = body
                Recursion = recursion
            })
        (ofTypedExpr $"function '{funcDef.Name}'" funcDef.Body)
        recursion

let ofTypedProgram (AST.Program topLevels) : Result<Program, string> =
    let convertTopLevel topLevel =
        match topLevel with
        | AST.FunctionDef funcDef -> ofTypedFunction funcDef |> Result.map FunctionDef
        | AST.TypeDef typeDef -> Ok (TypeDef typeDef)
        | AST.ValueDef (AST.CheckedValueDef (name, typ, body)) ->
            ofTypedExpr $"value '{name}'" body
            |> Result.map (fun checkedBody -> ValueDef { Name = name; Type = typ; Body = checkedBody })
        | AST.ValueDef (AST.UncheckedValueDef (name, _)) ->
            conversionError $"value '{name}'" "value definition was not checked"
        | AST.Expression (_, expr) -> ofTypedExpr "entry expression" expr |> Result.map Expression
    topLevels
    |> List.map convertTopLevel
    |> sequenceResults
    |> Result.map Program
