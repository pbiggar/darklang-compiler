// 1.5_TypeChecking.fs - Type Checking Pass (Phase 0)
//
// Simple top-down type checker for the Dark compiler.
//
// Design:
// - Function parameters and return types REQUIRE explicit type signatures (Phase 4+)
// - Let bindings have optional type annotations (Phase 1+)
// - Type checking proceeds top-down (expression context known from surrounding code)
// - No type inference - when type cannot be determined from context, require annotation
//
// Current implementation:
// - Supports the language's scalar, compound, function, and generic types
// - Validates operations against the concrete types they accept
// - Returns Result<Type, TypeError> for functional error handling
//
// Example:
//   Input:  2 + 3 * 4
//   Output: Ok TInt64

module TypeChecking

open AST

/// Generate unique parameter names for partial application
/// Includes function name to avoid variable capture with nested partial applications
let private makePartialParams (funcName: string) (types: Type list) : (string * Type) list =
    let safeName = funcName.Replace('.', '_')
    types |> List.mapi (fun i t -> ($"__partial_{safeName}_{i}", t))

let private toCallArgs (args: Expr list) : NonEmptyList<Expr> =
    match args with
    | [] -> NonEmptyList.singleton UnitLiteral
    | _ -> NonEmptyList.fromList args

let private normalizeNullaryCallArgs (expectedParamCount: int) (args: Expr list) : Expr list =
    if expectedParamCount = 0 && args = [UnitLiteral] then
        []
    else
        args

let private toLambdaParams (parameters: (string * Type) list) : NonEmptyList<LambdaParameter> =
    match parameters |> List.map (fun (name, typ) -> inferredLambdaVariable name typ) |> NonEmptyList.tryFromList with
    | Some nel -> nel
    | None -> Crash.crash "Type checker attempted to construct a lambda with zero parameters"

/// Type errors
type TypeError =
    | TypeMismatch of expected:Type * actual:Type * context:string
    | IfBranchTypeMismatch of expected:Type * actual:Type
    | UndefinedVariable of name:string
    | UndefinedCallTarget of name:string
    | MissingTypeAnnotation of context:string
    | InvalidOperation of op:string * types:Type list
    | IncompatibleEqualityOperands of left:Type * right:Type
    | IncompatibleOrderingOperands of left:Type * right:Type
    | ResolutionFailure of NameResolution.ResolutionError
    | GenericError of string

/// Pretty-print a type for error messages
let rec typeToString (t: Type) : string =
    match t with
    | TInt8 -> "Int8"
    | TInt16 -> "Int16"
    | TInt32 -> "Int32"
    | TInt64 -> "Int64"
    | TInt128 -> "Int128"
    | TInt -> "Int"
    | TUInt8 -> "UInt8"
    | TUInt16 -> "UInt16"
    | TUInt32 -> "UInt32"
    | TUInt64 -> "UInt64"
    | TUInt128 -> "UInt128"
    | TBool -> "Bool"
    | TFloat64 -> "Float"
    | TString -> "String"
    | TBlob -> "Blob"
    | TChar -> "Char"
    | TDateTime -> "DateTime"
    | TUnit -> "Unit"
    | TRuntimeError -> "RuntimeError"
    | TFunction (params', ret) ->
        let paramStr = params' |> List.map typeToString |> String.concat ", "
        $"({paramStr}) -> {typeToString ret}"
    | TTuple elemTypes ->
        let elemsStr = elemTypes |> List.map typeToString |> String.concat ", "
        $"({elemsStr})"
    | TEnumFields fieldTypes ->
        fieldTypes |> List.map typeToString |> String.concat " * "
    | TRecord (name, []) -> name
    | TRecord (name, typeArgs) ->
        let argsStr = typeArgs |> List.map typeToString |> String.concat ", "
        $"{name}<{argsStr}>"
    | TSum (name, []) -> name
    | TSum (name, typeArgs) ->
        let argsStr = typeArgs |> List.map typeToString |> String.concat ", "
        $"{name}<{argsStr}>"
    | TList elemType -> $"List<{typeToString elemType}>"
    | TVar name -> name  // Type variable (for generics)
    | TRawPtr -> "RawPtr"  // Internal raw pointer type
    | TDict (_, valueType) -> $"Dict<{typeToString valueType}>"

/// Pretty-print a type error
let typeErrorToString (err: TypeError) : string =
    match err with
    | TypeMismatch (expected, actual, context) ->
        $"Type mismatch in {context}: expected {typeToString expected}, got {typeToString actual}"
    | IfBranchTypeMismatch (expected, actual) ->
        $"Type mismatch: if branches must have same type: expected {typeToString expected}, got {typeToString actual}"
    | UndefinedVariable name ->
        $"Undefined variable: {name}"
    | UndefinedCallTarget name ->
        $"There is no variable named: {name}"
    | MissingTypeAnnotation context ->
        $"Missing type annotation: {context}"
    | InvalidOperation (op, types) ->
        let typesStr = types |> List.map typeToString |> String.concat ", "
        $"Invalid operation '{op}' on types: {typesStr}"
    | IncompatibleEqualityOperands (left, right) ->
        $"Cannot perform equality check on {typeToString left} and {typeToString right}"
    | IncompatibleOrderingOperands (left, right) ->
        $"Cannot perform numeric operation on {typeToString left} and {typeToString right}"
    | ResolutionFailure error ->
        NameResolution.errorToString error
    | GenericError msg ->
        msg

let private withIndefiniteArticle (s: string) : string =
    if s.Length = 0 then
        s
    else
        match System.Char.ToLowerInvariant(s.[0]) with
        | 'a'
        | 'e'
        | 'i'
        | 'o'
        | 'u' -> $"an {s}"
        | _ -> $"a {s}"

let private describeIfConditionActual (expr: Expr) (actualType: Type) : string =
    match expr with
    | UnitLiteral -> "Unit (())"
    | Int64Literal i -> $"Int64 ({i})"
    | Int128Literal i -> $"Int128 ({i})"
    | Int8Literal i -> $"Int8 ({i})"
    | Int16Literal i -> $"Int16 ({i})"
    | Int32Literal i -> $"Int32 ({i})"
    | UInt8Literal i -> $"UInt8 ({i})"
    | UInt16Literal i -> $"UInt16 ({i})"
    | UInt32Literal i -> $"UInt32 ({i})"
    | UInt64Literal i -> $"UInt64 ({i})"
    | UInt128Literal i -> $"UInt128 ({i})"
    | StringLiteral s -> $"String (\"{s}\")"
    | CharLiteral s -> $"Char (\"{s}\")"
    | FloatLiteral f -> $"Float ({f})"
    | BoolLiteral true -> "Bool (true)"
    | BoolLiteral false -> "Bool (false)"
    | _ -> typeToString actualType

let private ifConditionTypeMismatchMessage (expr: Expr) (actualType: Type) : string =
    let actual = describeIfConditionActual expr actualType
    $"Encountered a condition that must be a Bool, but got {withIndefiniteArticle actual}"

let private describeInterpolationActual (expr: Expr) (actualType: Type) : string =
    match expr with
    | FloatLiteral f ->
        let formatted = string f
        let value =
            if formatted.Contains(".") || formatted.Contains("e") || formatted.Contains("E") then formatted
            else $"{formatted}.0"
        $"a Float ({value})"
    | Int64Literal i -> $"an Int64 ({i})"
    | _ -> withIndefiniteArticle (typeToString actualType)

let private interpolationTypeMismatchMessage (expr: Expr) (actualType: Type) : string =
    let actual = describeInterpolationActual expr actualType
    let conversionModule =
        match actualType with
        | TInt8 -> Some "Int8"
        | TUInt8 -> Some "UInt8"
        | TInt16 -> Some "Int16"
        | TUInt16 -> Some "UInt16"
        | TInt32 -> Some "Int32"
        | TUInt32 -> Some "UInt32"
        | TInt64 -> Some "Int64"
        | TUInt64 -> Some "UInt64"
        | TInt128 -> Some "Int128"
        | TUInt128 -> Some "UInt128"
        | TInt -> Some "Int"
        | TFloat64 -> Some "Float"
        | TBool -> Some "Bool"
        | TChar -> Some "Char"
        | TDateTime -> Some "DateTime"
        | _ -> None
    let hint =
        conversionModule
        |> Option.map (fun moduleName -> $". Try wrapping it with `Stdlib.{moduleName}.toString`.")
        |> Option.defaultValue ""
    $"Expected String in string interpolation, got {actual} instead{hint}"

/// Retain a let-bound literal in interpolation diagnostics. This substitution
/// is deliberately limited to interpolation parts and respects lexical shadowing.
let rec private substituteInterpolationLiteral (name: string) (literal: Expr) (expr: Expr) : Expr =
    let recurse = substituteInterpolationLiteral name literal
    match expr with
    | BoundaryRender (renderer, value) -> BoundaryRender (renderer, recurse value)
    | InterpolatedString parts ->
        parts
        |> List.map (function
            | StringText text -> StringText text
            | StringExpr (Var varName) when varName = name -> StringExpr literal
            | StringExpr inner -> StringExpr (recurse inner))
        |> InterpolatedString
    | Let (pattern, value, body) ->
        let body' =
            if letPatternBindings pattern |> List.contains name then body else recurse body
        Let (pattern, recurse value, body')
    | Lambda (parameters, returnAnnotation, body) when
        parameters
        |> NonEmptyList.toList
        |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
        |> List.contains name ->
        Lambda (parameters, returnAnnotation, body)
    | BinOp (op, left, right) -> BinOp (op, recurse left, recurse right)
    | UnaryOp (op, inner) -> UnaryOp (op, recurse inner)
    | If (condition, thenBranch, elseBranch) -> If (recurse condition, recurse thenBranch, recurse elseBranch)
    | Sequence (first, next) -> Sequence (recurse first, recurse next)
    | Call (functionName, callArgs) -> Call (functionName, NonEmptyList.map recurse callArgs)
    | TypeApp (functionName, typeArgs, callArgs) -> TypeApp (functionName, typeArgs, NonEmptyList.map recurse callArgs)
    | TupleLiteral elements -> TupleLiteral (List.map recurse elements)
    | TupleAccess (tuple, index) -> TupleAccess (recurse tuple, index)
    | DictLiteral (valueType, entries) -> DictLiteral (valueType, entries |> List.map (fun (key, value) -> (key, recurse value)))
    | RecordLiteral (typeName, fields) -> RecordLiteral (typeName, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | RecordUpdate (record, updates) -> RecordUpdate (recurse record, updates |> List.map (fun (field, value) -> (field, recurse value)))
    | RecordAccess (record, field) -> RecordAccess (recurse record, field)
    | Constructor (typeName, variantName, payload) -> Constructor (typeName, variantName, Option.map recurse payload)
    | Match (scrutinee, cases) ->
        Match (
            recurse scrutinee,
            cases
            |> List.map (fun matchCase ->
                { matchCase with Guard = Option.map recurse matchCase.Guard; Body = recurse matchCase.Body })
        )
    | ListLiteral elements -> ListLiteral (List.map recurse elements)
    | ListCons (heads, tail) -> ListCons (List.map recurse heads, recurse tail)
    | Lambda (parameters, returnAnnotation, body) -> Lambda (parameters, returnAnnotation, recurse body)
    | Apply (func, callArgs) -> Apply (recurse func, NonEmptyList.map recurse callArgs)
    | IndirectApply (func, callArgs) -> IndirectApply (recurse func, NonEmptyList.map recurse callArgs)
    | Closure (functionName, captures) -> Closure (functionName, List.map recurse captures)
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
    | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ -> expr

let private isBuiltinUnwrapName (funcName: string) : bool =
    funcName = "Builtin.unwrap"

let private isBuiltinTestRuntimeErrorName (funcName: string) : bool =
    funcName = "Builtin.testRuntimeError"

let private isBuiltinTestNanName (name: string) : bool =
    name = "Builtin.testNan"

let private isRuntimeErrorType (typ: Type) : bool =
    match typ with
    | TRuntimeError -> true
    | _ -> false

let private variantNameEndsWith (suffix: string) (variantName: string) : bool =
    variantName = suffix || variantName.EndsWith($".{suffix}")

let private isKnownFailureConstructorExpr (expr: Expr) : bool =
    match expr with
    | Constructor (_, variantName, None) when variantNameEndsWith "None" variantName ->
        true
    | Constructor (_, variantName, Some _) when variantNameEndsWith "Error" variantName ->
        true
    | _ ->
        false

/// Detect runtime-failing unwrap expressions, including piped/desugared shapes:
/// let x = Option.None in Builtin.unwrap(x)
let rec private isKnownUnwrapFailureExpr (boundExprs: Map<string, Expr>) (expr: Expr) : bool =
    let rec argIsKnownFailure (argExpr: Expr) : bool =
        if isKnownFailureConstructorExpr argExpr then
            true
        else
            match argExpr with
            | Var varName ->
                boundExprs
                |> Map.tryFind varName
                |> Option.exists argIsKnownFailure
            | _ ->
                false

    match expr with
    | Call (funcName, { Head = argExpr; Tail = [] }) when isBuiltinUnwrapName funcName ->
        argIsKnownFailure argExpr
    | Let (LPVariable name, valueExpr, bodyExpr) ->
        isKnownUnwrapFailureExpr (Map.add name valueExpr boundExprs) bodyExpr
    | Let (_, _, bodyExpr) -> isKnownUnwrapFailureExpr boundExprs bodyExpr
    | _ ->
        false

/// Detect known runtime-failing testRuntimeError expressions, including let-bound forms.
let rec private isKnownTestRuntimeErrorExpr (boundExprs: Map<string, Expr>) (expr: Expr) : bool =
    match expr with
    | Call (funcName, { Head = _; Tail = [] }) when isBuiltinTestRuntimeErrorName funcName ->
        true
    | Let (LPVariable name, valueExpr, bodyExpr) ->
        isKnownTestRuntimeErrorExpr (Map.add name valueExpr boundExprs) bodyExpr
    | Let (_, _, bodyExpr) -> isKnownTestRuntimeErrorExpr boundExprs bodyExpr
    | Var varName ->
        boundExprs
        |> Map.tryFind varName
        |> Option.exists (fun boundExpr -> isKnownTestRuntimeErrorExpr boundExprs boundExpr)
    | _ ->
        false

let rec private tryExtractStringLiteral (boundExprs: Map<string, Expr>) (expr: Expr) : string option =
    match expr with
    | StringLiteral s ->
        Some s
    | Var varName ->
        match Map.tryFind varName boundExprs with
        | Some boundExpr ->
            tryExtractStringLiteral boundExprs boundExpr
        | None ->
            // Keep a stable diagnostic when the value is only known at runtime
            // (for example a function parameter passed into Builtin.testRuntimeError).
            Some varName
    | Let (LPVariable name, valueExpr, bodyExpr) ->
        let boundExprs' = Map.add name valueExpr boundExprs
        tryExtractStringLiteral boundExprs' bodyExpr
    | Let (_, _, bodyExpr) -> tryExtractStringLiteral boundExprs bodyExpr
    | _ ->
        None

/// Extract the error message from a known Builtin.testRuntimeError expression, if statically available.
let rec private tryExtractKnownTestRuntimeErrorMessage
    (boundExprs: Map<string, Expr>)
    (expr: Expr)
    : string option =
    match expr with
    | Call (funcName, { Head = argExpr; Tail = [] }) when isBuiltinTestRuntimeErrorName funcName ->
        tryExtractStringLiteral boundExprs argExpr
    | Let (LPVariable name, valueExpr, bodyExpr) ->
        let boundExprs' = Map.add name valueExpr boundExprs
        tryExtractKnownTestRuntimeErrorMessage boundExprs' bodyExpr
    | Let (_, _, bodyExpr) -> tryExtractKnownTestRuntimeErrorMessage boundExprs bodyExpr
    | Var varName ->
        boundExprs
        |> Map.tryFind varName
        |> Option.bind (tryExtractKnownTestRuntimeErrorMessage boundExprs)
    | _ ->
        None

let rec private tryFormatLiteralValue (expr: Expr) : string option =
    match expr with
    | UnitLiteral -> Some "()"
    | Int64Literal i -> Some (string i)
    | Int128Literal i -> Some (string i)
    | Int8Literal i -> Some (string i)
    | Int16Literal i -> Some (string i)
    | Int32Literal i -> Some (string i)
    | UInt8Literal i -> Some (string i)
    | UInt16Literal i -> Some (string i)
    | UInt32Literal i -> Some (string i)
    | UInt64Literal i -> Some (string i)
    | UInt128Literal i -> Some (string i)
    | BigIntLiteral i -> Some (string i)
    | BoolLiteral true -> Some "true"
    | BoolLiteral false -> Some "false"
    | StringLiteral s -> Some $"\"{s}\""
    | CharLiteral c -> Some $"'{c}'"
    | FloatLiteral f -> Some (string f)
    | TupleLiteral elements ->
        elements
        |> List.fold (fun acc element ->
            match acc, tryFormatLiteralValue element with
            | Some rendered, Some item -> Some (rendered @ [item])
            | _ -> None) (Some [])
        |> Option.map (fun items ->
            let joined = String.concat ", " items
            $"({joined})")
    | _ -> None

let rec private formatDeconstructionPattern (pattern: Pattern) : string =
    match pattern with
    | PVar _ -> "[variable]"
    | PWildcard -> "_"
    | PUnit -> "()"
    | PTuple patterns ->
        patterns
        |> List.map formatDeconstructionPattern
        |> String.concat ", "
        |> fun text -> $"({text})"
    | _ -> "[pattern]"

let rec private formatLetDeconstructionPattern (pattern: LetPattern) : string =
    match pattern with
    | LPVariable _ -> "[variable]"
    | LPWildcard -> "_"
    | LPUnit -> "()"
    | LPTuple (first, second, rest) ->
        first :: second :: rest
        |> List.map formatLetDeconstructionPattern
        |> String.concat ", "
        |> fun text -> $"({text})"

let rec private inferredLetPatternType (path: string) (pattern: LetPattern) : Type =
    match pattern with
    | LPUnit -> TUnit
    | LPVariable name -> TVar $"binding_{path}_{name}"
    | LPWildcard -> TVar $"binding_{path}_wildcard"
    | LPTuple (first, second, rest) ->
        first :: second :: rest
        |> List.mapi (fun index inner -> inferredLetPatternType $"{path}_{index}" inner)
        |> TTuple

/// Check the entire let pattern shape before returning any bindings.
let rec private bindLetPatternTypes
    (pattern: LetPattern)
    (valueType: Type)
    : (string * Type) list option =
    match pattern, valueType with
    | LPVariable name, typ -> Some [(name, typ)]
    | LPWildcard, _ -> Some []
    | LPUnit, TUnit -> Some []
    | LPUnit, TVar _ -> Some []
    | LPTuple (first, second, rest), TVar _ ->
        bindLetPatternTypes pattern (inferredLetPatternType "tuple" pattern)
    | LPTuple (first, second, rest), TTuple elementTypes ->
        let patterns = first :: second :: rest
        if List.length patterns <> List.length elementTypes then
            None
        else
            List.zip patterns elementTypes
            |> List.fold (fun bindings (innerPattern, innerType) ->
                match bindings, bindLetPatternTypes innerPattern innerType with
                | Some accumulated, Some innerBindings -> Some (accumulated @ innerBindings)
                | _ -> None) (Some [])
    | LPUnit, _
    | LPTuple _, _ -> None

let private formatFloatLiteralForPatternMismatch (f: float) : string =
    let formatted = string f
    if formatted.Contains(".") || formatted.Contains("e") || formatted.Contains("E") then
        formatted
    else
        $"{formatted}.0"

let private formatListLiteralForNoMatch (elements: Expr list) : string =
    match elements with
    | [] -> "[]"
    | _ ->
        let elementTexts =
            elements
            |> List.map (fun element ->
                match tryFormatLiteralValue element with
                | Some text -> text
                | None -> "<unknown>")
        let joinedElements = String.concat ", " elementTexts
        $"[  {joinedElements}]"

let rec private formatPatternMismatchValue (expr: Expr) : string option =
    match expr with
    | ListLiteral (first :: second :: _) ->
        let firstText =
            match formatPatternMismatchValue first with
            | Some text -> text
            | None -> "<unknown>"
        let secondText =
            match formatPatternMismatchValue second with
            | Some text -> text
            | None -> "<unknown>"
        Some $"[  {firstText}, {secondText}, ..."
    | ListLiteral [single] ->
        // For singleton list mismatches, report the mismatched element value.
        formatPatternMismatchValue single
    | ListLiteral [] ->
        Some "[]"
    | FloatLiteral f ->
        Some (formatFloatLiteralForPatternMismatch f)
    | TupleLiteral elements ->
        let elementTexts =
            elements
            |> List.map (fun element ->
                match formatPatternMismatchValue element with
                | Some text -> text
                | None -> "<unknown>")
        let tupleText = String.concat ", " elementTexts
        Some $"({tupleText})"
    | _ ->
        tryFormatLiteralValue expr

let rec private narrowPatternMismatchExprByType (actualType: Type) (expr: Expr) : Expr =
    match actualType, expr with
    | TList _, _ -> expr
    | TTuple _, _ -> expr
    | _, ListLiteral (first :: _) -> narrowPatternMismatchExprByType actualType first
    | _, TupleLiteral (first :: _) -> narrowPatternMismatchExprByType actualType first
    | _, _ -> expr

let private patternMismatchActualTypeText (actualType: Type) (_scrutineeExpr: Expr) : string =
    typeToString actualType

let private formatPatternMismatchError
    (scrutineeExpr: Expr)
    (actualType: Type)
    (expectedPatternType: Type)
    (expectedPatternTypeTextOverride: string option)
    : string =
    let narrowedExpr = narrowPatternMismatchExprByType actualType scrutineeExpr
    let valueText =
        match formatPatternMismatchValue narrowedExpr with
        | Some text -> text
        | None -> "<unknown>"
    let expectedPatternText =
        match expectedPatternTypeTextOverride with
        | Some typeText -> withIndefiniteArticle typeText
        | None -> withIndefiniteArticle (typeToString expectedPatternType)
    let actualTypeText = patternMismatchActualTypeText actualType scrutineeExpr
    $"Cannot match {actualTypeText} value {valueText} with {expectedPatternText} pattern"

let private formatLegacyParamTypeError
    (functionName: string)
    (paramIndex: int)
    (paramName: string)
    (expectedType: Type)
    (actualType: Type)
    (actualExpr: Expr)
    : string =
    let ordinal =
        match paramIndex with
        | 1 -> "1st"
        | 2 -> "2nd"
        | 3 -> "3rd"
        | _ -> $"{paramIndex}th"

    let actualValue =
        match tryFormatLiteralValue actualExpr with
        | Some v -> v
        | None -> typeToString actualType

    $"{functionName}'s {ordinal} parameter `{paramName}` expects {typeToString expectedType}, but got {typeToString actualType} ({actualValue})"

/// Freshen type parameters - generate new unique names for each type param
/// Returns (fresh type params, substitution map from old to fresh names)
/// Uses index-based naming for deterministic compilation (no global state)
let freshenTypeParams (typeParams: string list) : string list * Map<string, string> =
    let freshParams = typeParams |> List.mapi (fun i baseName -> $"{baseName}${i}")
    let subst = List.zip typeParams freshParams |> Map.ofList
    (freshParams, subst)

/// Apply type variable renaming to a type
let rec applyTypeVarRenaming (subst: Map<string, string>) (t: Type) : Type =
    match t with
    | TVar name ->
        match Map.tryFind name subst with
        | Some newName -> TVar newName
        | None -> t
    | TList elem -> TList (applyTypeVarRenaming subst elem)
    | TDict (k, v) -> TDict (applyTypeVarRenaming subst k, applyTypeVarRenaming subst v)
    | TFunction (paramTypes, retType) ->
        TFunction (List.map (applyTypeVarRenaming subst) paramTypes, applyTypeVarRenaming subst retType)
    | TTuple elems -> TTuple (List.map (applyTypeVarRenaming subst) elems)
    | TEnumFields fields -> TEnumFields (List.map (applyTypeVarRenaming subst) fields)
    | TSum (name, args) -> TSum (name, List.map (applyTypeVarRenaming subst) args)
    | TRecord (name, args) -> TRecord (name, List.map (applyTypeVarRenaming subst) args)
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr -> t

/// Type environment - maps variable names to their types
type TypeEnv = Map<string, Type>

/// Function parameter-name registry - maps function names to ordered parameter names
type FuncParamNameRegistry = Map<string, string list>

/// Type registry - maps record type names to their ordered field definitions.
type TypeRegistry = Map<string, (string * Type) list>

/// Precomputed record metadata reused across separate type-checking invocations.
type RecordTypeInfo = {
    Fields: (string * Type) list
    FieldTypes: Map<string, Type>
    TypeParams: string list
}

/// Indexed view retained in TypeCheckEnv for reuse by separate compilations.
type IndexedTypeRegistry = Map<string, RecordTypeInfo>

/// Sum type registry - maps sum type names to their variant lists (name, tag, payload)
type SumTypeRegistry = Map<string, (string * int * Type option) list>

/// Variant lookup - maps variant names to (type name, type params, tag index, payload type)
/// Type params are the generic type parameters of the containing sum type
type VariantLookup = Map<string, (string * string list * int * Type option)>

let private tryFindVariant
    (constructorReference: ConstructorReference)
    (variantName: string)
    (variantLookup: VariantLookup)
    : (string * string list * int * Type option) option =
    match constructorReferenceTypeName constructorReference with
    | None -> Map.tryFind variantName variantLookup
    | Some constructorTypeName ->
        Map.tryFind $"{constructorTypeName}.{variantName}" variantLookup

let private unqualifiedVariantOwnerCount
    (variantName: string)
    (variantLookup: VariantLookup)
    : int =
    variantLookup
    |> Map.toList
    |> List.choose (fun (lookupName, (typeName, _, _, _)) ->
        if lookupName = $"{typeName}.{variantName}" then Some typeName else None)
    |> List.distinct
    |> List.length

/// Generic function registry and call-site policy controls.
/// `Functions` contains entries only for functions that have type parameters.
type GenericFuncRegistry = {
    Functions: Map<string, string list>
    RequireExplicitTypeArgsForBareCalls: bool
}

/// Alias registry - maps type alias names to (type params, target type)
/// Example: type Id = String -> ("Id", ([], TString))
/// Example: type Outer<a> = Inner<a, Int64> -> ("Outer", (["a"], TSum("Inner", [TVar "a"; TInt64])))
type AliasRegistry = Map<string, (string list * Type)>

/// Type substitution - maps type variable names to concrete types
type Substitution = Map<string, Type>

/// Collected type checking environment - can be passed to compile user code with stdlib
type TypeCheckEnv = {
    TypeReg: TypeRegistry
    IndexedTypeReg: IndexedTypeRegistry
    VariantLookup: VariantLookup
    FuncEnv: TypeEnv
    FuncParamNames: FuncParamNameRegistry
    GenericFuncReg: GenericFuncRegistry
    ModuleRegistry: ModuleRegistry
    AliasReg: AliasRegistry
    ResolutionEnv: NameResolution.ResolutionEnvironment
}

/// Merge two TypeCheckEnv, with overlay taking precedence on conflicts
/// Used for separate compilation: merge stdlib env with user env
let mergeTypeCheckEnv (baseEnv: TypeCheckEnv) (overlay: TypeCheckEnv) : TypeCheckEnv =
    let mergeMap m1 m2 = Map.fold (fun acc k v -> Map.add k v acc) m1 m2
    {
        TypeReg = mergeMap baseEnv.TypeReg overlay.TypeReg
        IndexedTypeReg = mergeMap baseEnv.IndexedTypeReg overlay.IndexedTypeReg
        VariantLookup = mergeMap baseEnv.VariantLookup overlay.VariantLookup
        FuncEnv = mergeMap baseEnv.FuncEnv overlay.FuncEnv
        FuncParamNames = mergeMap baseEnv.FuncParamNames overlay.FuncParamNames
        GenericFuncReg = {
            Functions = mergeMap baseEnv.GenericFuncReg.Functions overlay.GenericFuncReg.Functions
            RequireExplicitTypeArgsForBareCalls =
                baseEnv.GenericFuncReg.RequireExplicitTypeArgsForBareCalls
                || overlay.GenericFuncReg.RequireExplicitTypeArgsForBareCalls
        }
        ModuleRegistry = baseEnv.ModuleRegistry  // Module registry is constant, use base
        AliasReg = mergeMap baseEnv.AliasReg overlay.AliasReg
        ResolutionEnv = NameResolution.merge baseEnv.ResolutionEnv overlay.ResolutionEnv
    }

/// Resolve a type name through the alias registry
/// If the name is an alias, recursively resolve to the underlying type name
let rec resolveTypeName (aliasReg: AliasRegistry) (typeName: string) : string =
    match Map.tryFind typeName aliasReg with
    | Some ([], TRecord (targetName, _)) -> resolveTypeName aliasReg targetName
    | _ -> typeName

/// Apply a substitution to a type, replacing type variables with concrete types
let rec private applySubstWithSeen (seen: Set<string>) (subst: Substitution) (typ: Type) : Type =
    match typ with
    | TVar name ->
        if Set.contains name seen then
            typ
        else
            let seen' = Set.add name seen
            match Map.tryFind name subst with
            | Some concreteType ->
                applySubstWithSeen seen' subst concreteType
            | None ->
                typ  // Unbound type variable remains as-is
    | TFunction (paramTypes, returnType) ->
        TFunction (List.map (applySubstWithSeen seen subst) paramTypes, applySubstWithSeen seen subst returnType)
    | TTuple elemTypes ->
        TTuple (List.map (applySubstWithSeen seen subst) elemTypes)
    | TEnumFields fieldTypes ->
        TEnumFields (List.map (applySubstWithSeen seen subst) fieldTypes)
    | TRecord (name, typeArgs) ->
        TRecord (name, List.map (applySubstWithSeen seen subst) typeArgs)
    | TList elemType ->
        TList (applySubstWithSeen seen subst elemType)
    | TSum (name, typeArgs) ->
        TSum (name, List.map (applySubstWithSeen seen subst) typeArgs)
    | TDict (keyType, valueType) ->
        TDict (applySubstWithSeen seen subst keyType, applySubstWithSeen seen subst valueType)
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
        typ  // Concrete types are unchanged

/// Apply a substitution to a type, replacing type variables with concrete types
let applySubst (subst: Substitution) (typ: Type) : Type =
    applySubstWithSeen Set.empty subst typ

/// Collect type variable names in first-seen order.
let rec collectTypeVarsInType (typ: Type) (acc: string list) : string list =
    let add name =
        if List.contains name acc then acc else acc @ [name]

    match typ with
    | TVar name -> add name
    | TFunction (paramTypes, returnType) ->
        let withParams = paramTypes |> List.fold (fun a t -> collectTypeVarsInType t a) acc
        collectTypeVarsInType returnType withParams
    | TTuple elemTypes ->
        elemTypes |> List.fold (fun a t -> collectTypeVarsInType t a) acc
    | TEnumFields fieldTypes ->
        fieldTypes |> List.fold (fun a t -> collectTypeVarsInType t a) acc
    | TRecord (_, typeArgs) ->
        typeArgs |> List.fold (fun a t -> collectTypeVarsInType t a) acc
    | TSum (_, typeArgs) ->
        typeArgs |> List.fold (fun a t -> collectTypeVarsInType t a) acc
    | TList elemType ->
        collectTypeVarsInType elemType acc
    | TDict (keyType, valueType) ->
        let withKey = collectTypeVarsInType keyType acc
        collectTypeVarsInType valueType withKey
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
        acc

/// Infer record type parameter order from field type variables.
/// This relies on first occurrence order of type variables in field types.
let inferRecordTypeParamsFromFields (fields: (string * Type) list) : string list =
    fields |> List.fold (fun acc (_, fieldType) -> collectTypeVarsInType fieldType acc) []

let private recordTypeInfo (fields: (string * Type) list) : RecordTypeInfo =
    {
        Fields = fields
        FieldTypes = Map.ofList fields
        TypeParams = inferRecordTypeParamsFromFields fields
    }

let private buildRecordFieldSubstitutionFromParams
    (typeParams: string list)
    (typeArgs: Type list)
    : Result<Substitution, string> =
    if List.length typeParams <> List.length typeArgs then
        Error
            $"Record type argument arity mismatch: expected {List.length typeParams}, got {List.length typeArgs}"
    else
        Ok (List.zip typeParams typeArgs |> Map.ofList)

/// Build a substitution for generic record fields from concrete type arguments.
let buildRecordFieldSubstitution (fields: (string * Type) list) (typeArgs: Type list) : Result<Substitution, string> =
    buildRecordFieldSubstitutionFromParams (inferRecordTypeParamsFromFields fields) typeArgs

let rec private resolveAliasTargetType (aliasReg: AliasRegistry) (typ: Type) : Type =
    match typ with
    | TRecord (name, typeArgs) ->
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) when List.length typeArgs <= List.length typeParams ->
            let subst = List.zip (List.take (List.length typeArgs) typeParams) typeArgs |> Map.ofList
            targetType |> applySubst subst |> resolveAliasTargetType aliasReg
        | _ ->
            TRecord (name, List.map (resolveAliasTargetType aliasReg) typeArgs)
    | TSum (name, typeArgs) ->
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) when List.length typeArgs <= List.length typeParams ->
            let subst = List.zip (List.take (List.length typeArgs) typeParams) typeArgs |> Map.ofList
            targetType |> applySubst subst |> resolveAliasTargetType aliasReg
        | _ ->
            TSum (name, List.map (resolveAliasTargetType aliasReg) typeArgs)
    | TFunction (paramTypes, returnType) ->
        TFunction (List.map (resolveAliasTargetType aliasReg) paramTypes, resolveAliasTargetType aliasReg returnType)
    | TTuple elemTypes ->
        TTuple (List.map (resolveAliasTargetType aliasReg) elemTypes)
    | TEnumFields fieldTypes ->
        TEnumFields (List.map (resolveAliasTargetType aliasReg) fieldTypes)
    | TList elemType ->
        TList (resolveAliasTargetType aliasReg elemType)
    | TDict (keyType, valueType) ->
        TDict (resolveAliasTargetType aliasReg keyType, resolveAliasTargetType aliasReg valueType)
    | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
        typ

let private tryResolveGenericRecordAliasFields
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (typeName: string)
    : (string * (string * Type) list) option =
    match resolveAliasTargetType aliasReg (TRecord (typeName, [])) with
    | TRecord (targetName, targetTypeArgs)
    | TSum (targetName, targetTypeArgs) when targetName <> typeName || not (List.isEmpty targetTypeArgs) ->
        match Map.tryFind targetName typeReg with
        | Some targetInfo ->
            match buildRecordFieldSubstitutionFromParams targetInfo.TypeParams targetTypeArgs with
            | Ok subst ->
                let fields =
                    targetInfo.Fields
                    |> List.map (fun (fieldName, fieldType) -> (fieldName, applySubst subst fieldType))
                Some (targetName, fields)
            | Error _ ->
                None
        | None ->
            None
    | _ ->
        None

let private tryResolveRecordLiteralFields
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (typeName: string)
    : (string * (string * Type) list) option =
    let resolvedTypeName = resolveTypeName aliasReg typeName
    match Map.tryFind resolvedTypeName typeReg with
    | Some info ->
        Some (resolvedTypeName, info.Fields)
    | None ->
        tryResolveGenericRecordAliasFields aliasReg typeReg typeName

/// Build a substitution from type parameters and type arguments
let buildSubstitution (typeParams: string list) (typeArgs: Type list) : Result<Substitution, string> =
    if List.length typeParams <> List.length typeArgs then
        Error $"Expected {List.length typeParams} type arguments, got {List.length typeArgs}"
    else
        Ok (List.zip typeParams typeArgs |> Map.ofList)

let private typeArgumentLabel (count: int) : string =
    if count = 1 then
        "type argument"
    else
        "type arguments"

let private argumentLabel (count: int) : string =
    if count = 1 then
        "argument"
    else
        "arguments"

let private formatTypeArgumentArityError (funcName: string) (expectedCount: int) (actualCount: int) : string =
    $"{funcName} expects {expectedCount} {typeArgumentLabel expectedCount}, but got {actualCount} {typeArgumentLabel actualCount}"

let private formatValueArgumentArityError (funcName: string) (expectedCount: int) (actualCount: int) : string =
    $"{funcName} expects {expectedCount} {argumentLabel expectedCount}, but got {actualCount} {argumentLabel actualCount}"

/// Apply a type substitution to an expression
/// This is used to propagate concrete types through nested TypeApp nodes
let rec applySubstToExpr (subst: Substitution) (expr: Expr) : Expr =
    match expr with
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ -> expr
    | BoundaryRender (renderer, value) -> BoundaryRender (renderer, applySubstToExpr subst value)
    | BinOp (op, left, right) ->
        BinOp (op, applySubstToExpr subst left, applySubstToExpr subst right)
    | UnaryOp (op, inner) ->
        UnaryOp (op, applySubstToExpr subst inner)
    | Let (pattern, value, body) ->
        Let (pattern, applySubstToExpr subst value, applySubstToExpr subst body)
    | If (cond, thenBr, elseBr) ->
        If (applySubstToExpr subst cond, applySubstToExpr subst thenBr, applySubstToExpr subst elseBr)
    | Sequence (first, next) ->
        Sequence (applySubstToExpr subst first, applySubstToExpr subst next)
    | Call (funcName, args) ->
        Call (funcName, NonEmptyList.map (applySubstToExpr subst) args)
    | TypeApp (funcName, typeArgs, args) ->
        // Apply substitution to both type arguments and value arguments
        TypeApp (funcName, List.map (applySubst subst) typeArgs, NonEmptyList.map (applySubstToExpr subst) args)
    | TupleLiteral elements ->
        TupleLiteral (List.map (applySubstToExpr subst) elements)
    | TupleAccess (tuple, index) ->
        TupleAccess (applySubstToExpr subst tuple, index)
    | DictLiteral (valueType, entries) ->
        DictLiteral (applySubst subst valueType, entries |> List.map (fun (key, value) -> (key, applySubstToExpr subst value)))
    | RecordLiteral (typeName, fields) ->
        RecordLiteral (typeName, List.map (fun (n, e) -> (n, applySubstToExpr subst e)) fields)
    | RecordUpdate (record, updates) ->
        RecordUpdate (applySubstToExpr subst record, List.map (fun (n, e) -> (n, applySubstToExpr subst e)) updates)
    | RecordAccess (record, fieldName) ->
        RecordAccess (applySubstToExpr subst record, fieldName)
    | Constructor (typeName, variantName, payload) ->
        Constructor (typeName, variantName, Option.map (applySubstToExpr subst) payload)
    | Match (scrutinee, cases) ->
        Match (applySubstToExpr subst scrutinee,
               cases |> List.map (fun mc ->
                   { mc with Guard = mc.Guard |> Option.map (applySubstToExpr subst)
                             Body = applySubstToExpr subst mc.Body }))
    | ListLiteral elements ->
        ListLiteral (List.map (applySubstToExpr subst) elements)
    | ListCons (heads, tail) ->
        ListCons (List.map (applySubstToExpr subst) heads, applySubstToExpr subst tail)
    | Lambda (params', returnAnnotation, body) ->
        let concreteParams =
            params'
            |> NonEmptyList.map (fun parameter ->
                { parameter with
                    SourceAnnotation = parameter.SourceAnnotation |> Option.map (applySubst subst)
                    InferredType = parameter.InferredType |> Option.map (applySubst subst) })
        Lambda (concreteParams, returnAnnotation |> Option.map (applySubst subst), applySubstToExpr subst body)
    | Apply (func, args) ->
        Apply (applySubstToExpr subst func, NonEmptyList.map (applySubstToExpr subst) args)
    | IndirectApply (func, args) ->
        IndirectApply (applySubstToExpr subst func, NonEmptyList.map (applySubstToExpr subst) args)
    | Closure (funcName, captures) ->
        Closure (funcName, List.map (applySubstToExpr subst) captures)
    | InterpolatedString parts ->
        InterpolatedString (parts |> List.map (function
            | StringText s -> StringText s
            | StringExpr e -> StringExpr (applySubstToExpr subst e)))

/// Resolve a type by expanding any type aliases (recursively)
/// Returns the fully resolved type with all aliases replaced by their targets
let rec resolveType (aliasReg: AliasRegistry) (typ: Type) : Type =
    match typ with
    | TRecord (name, typeArgs) ->
        // Resolve type arguments first.
        let resolvedArgs = List.map (resolveType aliasReg) typeArgs
        // Check if this record name is actually a type alias.
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) ->
            if List.length typeParams <> List.length resolvedArgs then
                // Mismatched type args, return as-is (error caught elsewhere)
                TRecord (name, resolvedArgs)
            else
                // Build substitution and apply to target type
                let subst = List.zip typeParams resolvedArgs |> Map.ofList
                let substituted = applySubst subst targetType
                // Recursively resolve in case target is also an alias
                resolveType aliasReg substituted
        | None ->
            // Not an alias, it's a real record type
            TRecord (name, resolvedArgs)
    | TSum (name, typeArgs) ->
        // Check if this sum type name is actually a type alias
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) ->
            // Type alias with (possibly) type arguments
            if List.length typeParams <> List.length typeArgs then
                // Mismatched type args, return as-is (error caught elsewhere)
                typ
            else
                // Build substitution and apply to target type
                let subst = List.zip typeParams typeArgs |> Map.ofList
                let substituted = applySubst subst targetType
                // Recursively resolve in case target is also an alias
                resolveType aliasReg substituted
        | None ->
            // Not an alias, resolve type arguments recursively
            TSum (name, List.map (resolveType aliasReg) typeArgs)
    | TFunction (paramTypes, returnType) ->
        TFunction (List.map (resolveType aliasReg) paramTypes, resolveType aliasReg returnType)
    | TTuple elemTypes ->
        TTuple (List.map (resolveType aliasReg) elemTypes)
    | TEnumFields fieldTypes ->
        TEnumFields (List.map (resolveType aliasReg) fieldTypes)
    | TList elemType ->
        TList (resolveType aliasReg elemType)
    | TDict (keyType, valueType) ->
        TDict (resolveType aliasReg keyType, resolveType aliasReg valueType)
    | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
        typ  // Primitive types and type variables are unchanged

let resolveAliasesInTypeRegistry (aliasReg: AliasRegistry) (typeReg: TypeRegistry) : TypeRegistry =
    typeReg
    |> Map.map (fun _ fields ->
        fields
        |> List.map (fun (fieldName, fieldType) -> (fieldName, resolveType aliasReg fieldType)))

let private canonicalizeBareSumTypeRefs (variantLookup: VariantLookup) (typ: Type) : Type =
    let sumTypeNames =
        variantLookup
        |> Map.toList
        |> List.map (fun (_, (typeName, _, _, _)) -> typeName)
        |> Set.ofList

    let rec canonicalize typ =
        match typ with
        | TRecord (name, []) when Set.contains name sumTypeNames ->
            TSum (name, [])
        | TRecord (name, typeArgs) ->
            TRecord (name, List.map canonicalize typeArgs)
        | TSum (name, typeArgs) ->
            TSum (name, List.map canonicalize typeArgs)
        | TFunction (paramTypes, returnType) ->
            TFunction (List.map canonicalize paramTypes, canonicalize returnType)
        | TTuple elemTypes ->
            TTuple (List.map canonicalize elemTypes)
        | TEnumFields fieldTypes ->
            TEnumFields (List.map canonicalize fieldTypes)
        | TList elemType ->
            TList (canonicalize elemType)
        | TDict (keyType, valueType) ->
            TDict (canonicalize keyType, canonicalize valueType)
        | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
        | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
        | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr ->
            typ

    canonicalize typ

let private canonicalizeTypeRegistry
    (variantLookup: VariantLookup)
    (typeReg: TypeRegistry)
    : IndexedTypeRegistry =
    typeReg
    |> Map.map (fun _ fields ->
        fields
        |> List.map (fun (fieldName, fieldType) ->
            (fieldName, canonicalizeBareSumTypeRefs variantLookup fieldType))
        |> recordTypeInfo)

/// Compare two types for equality, resolving type aliases first
/// This allows "Vec" and "Point" to be considered equal when Vec aliases Point
let typesEqual (aliasReg: AliasRegistry) (t1: Type) (t2: Type) : bool =
    resolveType aliasReg t1 = resolveType aliasReg t2

let private truncateLegacyRecordValueText (text: string) : string =
    if text.Length > 10 then
        $"{text.Substring(0, 10)}..."
    else
        text

let private formatLegacyRecordFieldTypeError
    (aliasReg: AliasRegistry)
    (fieldName: string)
    (expectedType: Type)
    (actualType: Type)
    (actualExpr: Expr)
    : string =
    let expectedText = expectedType |> resolveType aliasReg |> typeToString
    let actualText = actualType |> resolveType aliasReg |> typeToString
    let valueText =
        match tryFormatLiteralValue actualExpr with
        | Some value ->
            truncateLegacyRecordValueText value
        | None ->
            actualText

    $"Failed to create record. Expected {expectedText} for field `{fieldName}`, but got {valueText} ({withIndefiniteArticle actualText})"

/// Internal type-app markers emitted during type checking.
/// These markers are materialized to direct calls before leaving this pass.
type private InternalTypeAppMarker =
    | EqHelperDispatch

/// Internal type-app values carried in `Expr.TypeApp` nodes.
/// We encode/decode them through marker names at the pass boundary.
type private InternalTypeApp =
    | EqHelperDispatchTypeApp of targetType: Type * leftExpr: Expr * rightExpr: Expr

let private internalTypeAppMarkerName (marker: InternalTypeAppMarker) : string =
    match marker with
    | EqHelperDispatch -> "__dark_internal_eq_helper_dispatch"

let private tryParseInternalTypeAppMarker (funcName: string) : InternalTypeAppMarker option =
    if funcName = internalTypeAppMarkerName EqHelperDispatch then
        Some EqHelperDispatch
    else
        None

let private makeInternalTypeApp (internalTypeApp: InternalTypeApp) : Expr =
    match internalTypeApp with
    | EqHelperDispatchTypeApp (targetType, leftExpr, rightExpr) ->
        TypeApp (
            internalTypeAppMarkerName EqHelperDispatch,
            [targetType],
            NonEmptyList.fromList [leftExpr; rightExpr]
        )

let private tryDecodeInternalTypeApp (expr: Expr) : InternalTypeApp option =
    match expr with
    | TypeApp (funcName, [targetType], { Head = leftExpr; Tail = [rightExpr] }) ->
        match tryParseInternalTypeAppMarker funcName with
        | Some EqHelperDispatch ->
            Some (EqHelperDispatchTypeApp (targetType, leftExpr, rightExpr))
        | None ->
            None
    | _ ->
        None

let private sumTypeHasPayload (variantLookup: VariantLookup) (sumTypeName: string) : bool =
    variantLookup
    |> Map.exists (fun _ (variantTypeName, _, _, payloadTypeOpt) ->
        variantTypeName = sumTypeName && payloadTypeOpt.IsSome)

/// Every concrete compound comparable type has one equality entry point.
let rec needsEqHelperForResolvedType (variantLookup: VariantLookup) (typ: Type) : bool =
    match typ with
    | TFunction _ | TList _ | TDict _ | TTuple _ | TRecord _ -> true
    | TSum (sumTypeName, _) -> sumTypeHasPayload variantLookup sumTypeName
    | _ -> false

let private sanitizeHelperNamePrefix (input: string) : string =
    let chars =
        input
        |> Seq.map (fun c -> if System.Char.IsLetterOrDigit c then c else '_')
        |> Seq.truncate 48
        |> Seq.toArray

    if Array.isEmpty chars then
        "type"
    else
        System.String(chars)

/// Stable, deterministic hash used for generated helper function names.
let private stableHelperNameHash (input: string) : uint64 =
    let initial = 14695981039346656037UL
    let prime = 1099511628211UL
    input
    |> Seq.fold (fun acc ch -> (acc ^^^ uint64 (int ch)) * prime) initial

/// Name for a concrete structural equality helper.
let eqHelperName (typ: Type) : string =
    let typeText = typeToString typ
    let prefix = sanitizeHelperNamePrefix typeText
    let hash = stableHelperNameHash typeText
    $"__dark_eq_{prefix}_{hash:x16}"

/// Build a left-associative boolean conjunction chain.
let private chainAndExpr (exprs: Expr list) : Expr =
    match exprs with
    | [] ->
        BoolLiteral true
    | first :: rest ->
        List.fold (fun acc expr -> BinOp (And, acc, expr)) first rest

/// Build an equality expression for two already type-checked operands.
/// For tuple/record/sum, emit a typed internal dispatch marker that will later
/// be rewritten to a call to the generated concrete helper function.
let private buildEqExprForType
    (aliasReg: AliasRegistry)
    (variantLookup: VariantLookup)
    (typ: Type)
    (leftExpr: Expr)
    (rightExpr: Expr)
    : Expr =
    let resolvedType = resolveType aliasReg typ
    match resolvedType with
    | TVar _ ->
        // A generic comparison cannot select a representation-level operation
        // until specialization. Preserve the typed plan through substitution.
        makeInternalTypeApp (EqHelperDispatchTypeApp (resolvedType, leftExpr, rightExpr))
    | TFunction _ ->
        makeInternalTypeApp (EqHelperDispatchTypeApp (resolvedType, leftExpr, rightExpr))
    | TString ->
        Call ("Stdlib.String.equals", NonEmptyList.fromList [leftExpr; rightExpr])
    | TList elemType ->
        let resolvedElemType = resolveType aliasReg elemType
        match resolvedElemType with
        | _ when needsEqHelperForResolvedType variantLookup resolvedElemType ->
            makeInternalTypeApp (EqHelperDispatchTypeApp (resolvedType, leftExpr, rightExpr))
        | _ ->
            TypeApp ("Stdlib.List.equals", [resolvedElemType], NonEmptyList.fromList [leftExpr; rightExpr])
    | _ when needsEqHelperForResolvedType variantLookup resolvedType ->
        makeInternalTypeApp (EqHelperDispatchTypeApp (resolvedType, leftExpr, rightExpr))
    | _ ->
        BinOp (Eq, leftExpr, rightExpr)

/// A comparison is classified while both resolved operand types are available.
/// Equality plans may still contain type variables in a generic body; the
/// internal typed marker carries them through substitution and is materialized
/// only after a concrete specialization exists.
type private ComparisonPlan =
    | EqualityComparison of comparableType:Type
    | OrderingComparison of numericType:Type

let private comparisonNumericType (typ: Type) : bool =
    match typ with
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TFloat64 -> true
    | _ -> false

let private equalityComparableType
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (typ: Type)
    : bool =
    let rec comparable (seen: Set<Type>) (candidate: Type) : bool =
        let resolved = resolveType aliasReg candidate
        if Set.contains resolved seen then
            // Recursive nominal types are admissible when the cycle itself has
            // introduced no rejected payload type.
            true
        else
            let seen = Set.add resolved seen
            let recurse = comparable seen
            match resolved with
            | TVar _ -> true
            | TUnit | TBool | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
            | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
            | TFloat64 | TChar | TString | TDateTime -> true
            | TFunction (parameterTypes, returnType) ->
                List.forall recurse parameterTypes && recurse returnType
            | TTuple elementTypes
            | TEnumFields elementTypes ->
                List.forall recurse elementTypes
            | TList elementType ->
                recurse elementType
            | TDict (keyType, valueType) ->
                // Dict key admission remains owned by Dict. Comparison reuses
                // the key semantics already selected for an admitted key type.
                recurse keyType && recurse valueType
            | TRecord (recordName, typeArgs) ->
                match Map.tryFind recordName typeReg with
                | None -> false
                | Some recordInfo ->
                    match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                    | Error _ -> false
                    | Ok subst ->
                        recordInfo.Fields
                        |> List.forall (fun (_, fieldType) -> recurse (applySubst subst fieldType))
            | TSum (sumName, typeArgs) ->
                variantLookup
                |> Map.toList
                |> List.choose (fun (_, (ownerName, typeParams, _, payloadType)) ->
                    if ownerName <> sumName then None
                    else
                        payloadType
                        |> Option.map (fun payload ->
                            if List.length typeParams = List.length typeArgs then
                                applySubst (List.zip typeParams typeArgs |> Map.ofList) payload
                            else
                                payload))
                |> List.forall recurse
            | TBlob -> true
            | TRawPtr | TRuntimeError -> false

    comparable Set.empty typ

let rec private reconcileComparisonTypes
    (aliasReg: AliasRegistry)
    (leftType: Type)
    (rightType: Type)
    : Type option =
    let leftResolved = resolveType aliasReg leftType
    let rightResolved = resolveType aliasReg rightType
    let rec reconcileMany leftTypes rightTypes acc =
        match leftTypes, rightTypes with
        | [], [] -> Some (List.rev acc)
        | left :: leftRest, right :: rightRest ->
            reconcileComparisonTypes aliasReg left right
            |> Option.bind (fun reconciled ->
                reconcileMany leftRest rightRest (reconciled :: acc))
        | _ -> None

    if leftResolved = rightResolved then
        Some leftResolved
    else
        match leftResolved, rightResolved with
        | TRuntimeError, other
        | other, TRuntimeError -> Some other
        | TVar _, other
        | other, TVar _ -> Some other
        | TList leftElement, TList rightElement ->
            reconcileComparisonTypes aliasReg leftElement rightElement
            |> Option.map TList
        | TDict (leftKey, leftValue), TDict (rightKey, rightValue) ->
            reconcileComparisonTypes aliasReg leftKey rightKey
            |> Option.bind (fun keyType ->
                reconcileComparisonTypes aliasReg leftValue rightValue
                |> Option.map (fun valueType -> TDict (keyType, valueType)))
        | TTuple leftElements, TTuple rightElements ->
            reconcileMany leftElements rightElements [] |> Option.map TTuple
        | TEnumFields leftFields, TEnumFields rightFields ->
            reconcileMany leftFields rightFields [] |> Option.map TEnumFields
        | TRecord (leftName, leftArgs), TRecord (rightName, rightArgs)
            when leftName = rightName ->
            reconcileMany leftArgs rightArgs []
            |> Option.map (fun typeArgs -> TRecord (leftName, typeArgs))
        | TSum (leftName, leftArgs), TSum (rightName, rightArgs)
            when leftName = rightName ->
            reconcileMany leftArgs rightArgs []
            |> Option.map (fun typeArgs -> TSum (leftName, typeArgs))
        | TFunction (leftParams, leftReturn), TFunction (rightParams, rightReturn) ->
            reconcileMany leftParams rightParams []
            |> Option.bind (fun parameterTypes ->
                reconcileComparisonTypes aliasReg leftReturn rightReturn
                |> Option.map (fun returnType -> TFunction (parameterTypes, returnType)))
        | _ -> None

let private classifyComparison
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (op: BinOp)
    (leftType: Type)
    (rightType: Type)
    : Result<ComparisonPlan, TypeError> =
    let leftResolved = resolveType aliasReg leftType
    let rightResolved = resolveType aliasReg rightType
    match op with
    | Eq | Neq ->
        let comparableType = reconcileComparisonTypes aliasReg leftResolved rightResolved
        match comparableType with
        | Some typ when equalityComparableType aliasReg typeReg variantLookup typ ->
            Ok (EqualityComparison typ)
        | _ ->
            Error (IncompatibleEqualityOperands (leftResolved, rightResolved))
    | Lt | Gt | Lte | Gte ->
        match reconcileComparisonTypes aliasReg leftResolved rightResolved with
        | Some numericType when comparisonNumericType numericType ->
            // A literal or other concrete operand constrains the unresolved
            // generic operand. The enclosing call can remain polymorphic when
            // no value reaches that operand (for example List.all []).
            Ok (OrderingComparison numericType)
        | _ ->
            Error (IncompatibleOrderingOperands (leftResolved, rightResolved))
    | _ ->
        Crash.crash $"Non-comparison operator reached comparison classification: {op}"

let private orderingFunctionName (op: BinOp) : string =
    match op with
    | Lt -> "Stdlib.Int.lessThan"
    | Gt -> "Stdlib.Int.greaterThan"
    | Lte -> "Stdlib.Int.lessThanOrEqualTo"
    | Gte -> "Stdlib.Int.greaterThanOrEqualTo"
    | _ -> Crash.crash $"Non-ordering operator has no Int comparison helper: {op}"

let private buildOrderingExprForType
    (op: BinOp)
    (numericType: Type)
    (leftExpr: Expr)
    (rightExpr: Expr)
    : Expr =
    let convertedOperands =
        match numericType with
        | TInt128 ->
            (Call ("__int128_to_int", NonEmptyList.singleton leftExpr),
             Call ("__int128_to_int", NonEmptyList.singleton rightExpr))
        | TUInt128 ->
            (Call ("__uint128_to_int", NonEmptyList.singleton leftExpr),
             Call ("__uint128_to_int", NonEmptyList.singleton rightExpr))
        | _ ->
            (leftExpr, rightExpr)
    match numericType, convertedOperands with
    | (TInt128 | TUInt128), (leftInt, rightInt) ->
        Call (orderingFunctionName op, NonEmptyList.fromList [leftInt; rightInt])
    | _ ->
        BinOp (op, leftExpr, rightExpr)

// =============================================================================
// Free Variable Analysis for Closures
// =============================================================================
// When compiling lambdas, we need to identify which variables from the
// enclosing scope are referenced in the lambda body (free variables).
// Only these need to be captured in the closure.

/// Collect free variables in an expression.
/// Returns the set of variable names that are referenced but not bound locally.
/// bound: Set of names that are currently in scope (not free)
let rec collectFreeVars (expr: Expr) (bound: Set<string>) : Set<string> =
    match expr with
    | BoundaryRender (_, value) -> collectFreeVars value bound
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | RuntimeError _ ->
        Set.empty
    | Var name ->
        if Set.contains name bound || isBuiltinTestNanName name then
            Set.empty
        else
            Set.singleton name
    | BinOp (_, left, right) ->
        Set.union (collectFreeVars left bound) (collectFreeVars right bound)
    | UnaryOp (_, inner) ->
        collectFreeVars inner bound
    | Let (pattern, value, body) ->
        let valueFree = collectFreeVars value bound
        let names = letPatternBindings pattern |> Set.ofList
        let bodyFree = collectFreeVars body (Set.union names bound)
        Set.union valueFree bodyFree
    | If (cond, thenBranch, elseBranch) ->
        let condFree = collectFreeVars cond bound
        let thenFree = collectFreeVars thenBranch bound
        let elseFree = collectFreeVars elseBranch bound
        Set.union condFree (Set.union thenFree elseFree)
    | Sequence (first, next) ->
        Set.union (collectFreeVars first bound) (collectFreeVars next bound)
    | Call (_, args) ->
        args
        |> NonEmptyList.toList
        |> List.map (fun e -> collectFreeVars e bound)
        |> List.fold Set.union Set.empty
    | TypeApp (_, _, args) ->
        args
        |> NonEmptyList.toList
        |> List.map (fun e -> collectFreeVars e bound)
        |> List.fold Set.union Set.empty
    | TupleLiteral elements ->
        elements |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | TupleAccess (tuple, _) ->
        collectFreeVars tuple bound
    | DictLiteral (_, entries) ->
        entries |> List.map (fun (_, e) -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | RecordLiteral (_, fields) ->
        fields |> List.map (fun (_, e) -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | RecordUpdate (record, updates) ->
        let recordFree = collectFreeVars record bound
        let updatesFree = updates |> List.map (fun (_, e) -> collectFreeVars e bound) |> List.fold Set.union Set.empty
        Set.union recordFree updatesFree
    | RecordAccess (record, _) ->
        collectFreeVars record bound
    | Constructor (_, _, payload) ->
        payload |> Option.map (fun e -> collectFreeVars e bound) |> Option.defaultValue Set.empty
    | Match (scrutinee, cases) ->
        let scrutineeFree = collectFreeVars scrutinee bound
        let casesFree = cases |> List.map (fun matchCase ->
            // Collect bindings from all patterns (all patterns in a group bind same vars)
            let patternBindings =
                matchCase.Patterns
                |> NonEmptyList.toList
                |> List.map collectPatternBindings
                |> List.fold Set.union Set.empty
            let bodyBound = Set.union bound patternBindings
            // Include guard free vars if present
            let guardFree = matchCase.Guard |> Option.map (fun g -> collectFreeVars g bodyBound) |> Option.defaultValue Set.empty
            let bodyFree = collectFreeVars matchCase.Body bodyBound
            Set.union guardFree bodyFree)
        Set.union scrutineeFree (casesFree |> List.fold Set.union Set.empty)
    | ListLiteral elements ->
        elements |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | ListCons (headElements, tail) ->
        let headsFree = headElements |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
        let tailFree = collectFreeVars tail bound
        Set.union headsFree tailFree
    | Lambda (parameters, returnAnnotation, body) ->
        let paramNames =
            parameters
            |> NonEmptyList.toList
            |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
            |> Set.ofList
        collectFreeVars body (Set.union bound paramNames)
    | Apply (func, args)
    | IndirectApply (func, args) ->
        let funcFree = collectFreeVars func bound
        let argsFree =
            args
            |> NonEmptyList.toList
            |> List.map (fun e -> collectFreeVars e bound)
            |> List.fold Set.union Set.empty
        Set.union funcFree argsFree
    | FuncRef _ ->
        // Function references don't contribute free variables
        Set.empty
    | Closure (_, captures) ->
        // Closures capture expressions which may have free variables
        captures |> List.map (fun e -> collectFreeVars e bound) |> List.fold Set.union Set.empty
    | InterpolatedString parts ->
        parts |> List.choose (fun part ->
            match part with
            | StringText _ -> None
            | StringExpr e -> Some (collectFreeVars e bound))
        |> List.fold Set.union Set.empty

/// Collect variable names bound by a pattern
and collectPatternBindings (pattern: Pattern) : Set<string> =
    match pattern with
    | PUnit -> Set.empty
    | PWildcard -> Set.empty
    | PVar name -> Set.singleton name
    | PInt64 _
    | PInt128Literal _
    | PInt8Literal _
    | PInt16Literal _
    | PInt32Literal _
    | PUInt8Literal _
    | PUInt16Literal _
    | PUInt32Literal _
    | PUInt64Literal _
    | PUInt128Literal _
    | PBool _
    | PString _
    | PChar _
    | PFloat _ -> Set.empty
    | PConstructor (_, None) -> Set.empty
    | PConstructor (_, Some payload) -> collectPatternBindings payload
    | PTuple patterns ->
        patterns |> List.map collectPatternBindings |> List.fold Set.union Set.empty
    | PRecord (_, fields) ->
        fields |> List.map (fun (_, p) -> collectPatternBindings p) |> List.fold Set.union Set.empty
    | PList patterns ->
        patterns |> List.map collectPatternBindings |> List.fold Set.union Set.empty
    | PListCons (headPatterns, tailPattern) ->
        let headBindings = headPatterns |> List.map collectPatternBindings |> List.fold Set.union Set.empty
        let tailBindings = collectPatternBindings tailPattern
        Set.union headBindings tailBindings

// =============================================================================
// Type Inference for Generic Function Calls
// =============================================================================
// When a generic function is called without explicit type arguments, we infer
// the type arguments from the actual argument types. For example:
//   let identity<'T>(x: T) : T = x
//   identity(42)  // Infers T=int from argument type

/// Match a pattern type against an actual type, extracting type variable bindings.
/// Returns a list of (typeVarName, concreteType) pairs.
/// Example: matchTypes (TVar "T") TInt64 = Ok [("T", TInt64)]
/// Helper for matching concrete types - also handles when actual is a TVar
let matchConcrete (expectedType: Type) (actual: Type) : Result<(string * Type) list, string> =
    if expectedType = TRuntimeError || actual = TRuntimeError then
        // Runtime-error expressions are bottom-like and can inhabit any expected type.
        Ok []
    else
        match actual with
        | t when t = expectedType -> Ok []
        | TVar name -> Ok [(name, expectedType)]  // Bind TVar to concrete type
        | _ -> Error $"Expected {typeToString expectedType}, got {typeToString actual}"

let rec matchTypes (pattern: Type) (actual: Type) : Result<(string * Type) list, string> =
    match pattern with
    | TVar name ->
        // Type variable matches anything - record the binding
        match actual with
        | TVar actualName when actualName = name -> Ok []  // Same var, no binding needed
        | _ -> Ok [(name, actual)]
    | TInt8 -> matchConcrete TInt8 actual
    | TInt16 -> matchConcrete TInt16 actual
    | TInt32 -> matchConcrete TInt32 actual
    | TInt64 -> matchConcrete TInt64 actual
    | TInt128 -> matchConcrete TInt128 actual
    | TInt -> matchConcrete TInt actual
    | TUInt8 -> matchConcrete TUInt8 actual
    | TUInt16 -> matchConcrete TUInt16 actual
    | TUInt32 -> matchConcrete TUInt32 actual
    | TUInt64 -> matchConcrete TUInt64 actual
    | TUInt128 -> matchConcrete TUInt128 actual
    | TBool -> matchConcrete TBool actual
    | TFloat64 -> matchConcrete TFloat64 actual
    | TString ->
        // Char and String share runtime representation.
        match actual with
        | TChar -> Ok []
        | _ -> matchConcrete TString actual
    | TBlob -> matchConcrete TBlob actual
    | TDateTime -> matchConcrete TDateTime actual
    | TChar ->
        match actual with
        | TString -> Ok []
        | _ -> matchConcrete TChar actual
    | TUnit -> matchConcrete TUnit actual
    | TRuntimeError -> matchConcrete TRuntimeError actual
    | TRawPtr -> matchConcrete TRawPtr actual
    | TList patternElem ->
        match actual with
        | TList actualElem -> matchTypes patternElem actualElem
        | TVar name -> Ok [(name, pattern)]  // Bind TVar to List type
        | _ -> Error $"Expected List<...>, got {typeToString actual}"
    | TRecord (name, patternArgs) ->
        match actual with
        | TRecord (n, actualArgs) when n = name ->
            // Unify type arguments if both have them
            if List.length patternArgs <> List.length actualArgs then
                Error $"Record type arity mismatch for {name}"
            else
                List.zip patternArgs actualArgs
                |> List.map (fun (p, a) -> matchTypes p a)
                |> List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) (Ok [])
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Record type
        | _ -> Error $"Expected {name}, got {typeToString actual}"
    | TSum (name, patternArgs) ->
        match actual with
        | TSum (actualName, actualArgs) when name = actualName ->
            if List.length patternArgs <> List.length actualArgs then
                Error $"Sum type arity mismatch for {name}"
            else
                List.zip patternArgs actualArgs
                |> List.map (fun (p, a) -> matchTypes p a)
                |> List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) (Ok [])
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Sum type
        | _ -> Error $"Expected {name}, got {typeToString actual}"
    | TFunction (patternParams, patternRet) ->
        match actual with
        | TFunction (actualParams, actualRet) ->
            if List.length patternParams <> List.length actualParams then
                Error $"Function arity mismatch: expected {List.length patternParams} params, got {List.length actualParams}"
            else
                // Match each parameter type and return type
                let paramResults =
                    List.zip patternParams actualParams
                    |> List.map (fun (p, a) -> matchTypes p a)
                let retResult = matchTypes patternRet actualRet
                // Combine all results
                List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) retResult paramResults
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Function type
        | _ -> Error $"Expected function, got {typeToString actual}"
    | TTuple patternElems ->
        match actual with
        | TTuple actualElems ->
            if List.length patternElems <> List.length actualElems then
                Error $"Tuple size mismatch: expected {List.length patternElems}, got {List.length actualElems}"
            else
                List.zip patternElems actualElems
                |> List.map (fun (p, a) -> matchTypes p a)
                |> List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) (Ok [])
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Tuple type
        | _ -> Error $"Expected tuple, got {typeToString actual}"
    | TEnumFields patternFields ->
        match actual with
        | TTuple actualFields when List.length patternFields = List.length actualFields ->
            List.zip patternFields actualFields
            |> List.map (fun (patternField, actualField) -> matchTypes patternField actualField)
            |> List.fold (fun acc result ->
                match acc, result with
                | Ok bindings, Ok more -> Ok (bindings @ more)
                | Error err, _ -> Error err
                | _, Error err -> Error err) (Ok [])
        | TTuple actualFields ->
            Error $"Enum field arity mismatch: expected {List.length patternFields}, got {List.length actualFields}"
        | _ -> Error $"Expected {List.length patternFields} enum fields, got {typeToString actual}"
    | TDict (patternKey, patternValue) ->
        match actual with
        | TDict (actualKey, actualValue) ->
            // Match both key and value types
            match matchTypes patternKey actualKey, matchTypes patternValue actualValue with
            | Ok keyBindings, Ok valueBindings -> Ok (keyBindings @ valueBindings)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        | TVar varName -> Ok [(varName, pattern)]  // Bind TVar to Dict type
        | _ -> Error $"Expected Dict<...>, got {typeToString actual}"

/// Check if a type contains type variables
let rec containsTVar (typ: Type) : bool =
    match typ with
    | TVar _ -> true
    | TList elemType -> containsTVar elemType
    | TDict (keyType, valueType) -> containsTVar keyType || containsTVar valueType
    | TTuple elemTypes -> List.exists containsTVar elemTypes
    | TRecord (_, typeArgs) -> List.exists containsTVar typeArgs
    | TSum (_, typeArgs) -> List.exists containsTVar typeArgs
    | TFunction (paramTypes, retType) ->
        List.exists containsTVar paramTypes || containsTVar retType
    | _ -> false

/// Check if two types are compatible (can be unified)
/// Type variables in either type can match concrete types
let typesCompatible (expected: Type) (actual: Type) : bool =
    match matchTypes expected actual with
    | Ok _ -> true
    | Error _ -> false

/// Check if two types are compatible after resolving type aliases
/// Combines alias resolution with type variable unification
let typesCompatibleWithAliases (aliasReg: AliasRegistry) (expected: Type) (actual: Type) : bool =
    let resolvedExpected = resolveType aliasReg expected
    let resolvedActual = resolveType aliasReg actual
    typesCompatible resolvedExpected resolvedActual

/// Consolidate bindings, checking for conflicts where the same type variable
/// is bound to different types. Returns a map from type var name to concrete type.
/// When a type var is bound to both a type containing TVars and a concrete type, prefer the concrete type.
let consolidateBindings (bindings: (string * Type) list) : Result<Map<string, Type>, string> =
    bindings
    |> List.fold (fun acc (name, typ) ->
        acc |> Result.bind (fun m ->
            match Map.tryFind name m with
            | None -> Ok (Map.add name typ m)
            | Some existingType ->
                if existingType = typ then
                    Ok m
                elif containsTVar existingType && not (containsTVar typ) then
                    // existing contains TVars, new is concrete - prefer new
                    Ok (Map.add name typ m)
                elif containsTVar typ && not (containsTVar existingType) then
                    // new contains TVars, existing is concrete - keep existing
                    Ok m
                elif containsTVar existingType && containsTVar typ then
                    // Both contain TVars - keep the first one (arbitrary choice)
                    Ok m
                else
                    // Both are concrete but different - that's an error
                    Error $"Type variable {name} has conflicting inferences: {typeToString existingType} vs {typeToString typ}"))
        (Ok Map.empty)

/// Unify a type pattern (may contain TVar) with a concrete type.
/// Returns a substitution mapping type variables to concrete types.
/// Example: unifyTypes (TVar "t") TInt64 = Ok (Map.ofList [("t", TInt64)])
let unifyTypes (pattern: Type) (actual: Type) : Result<Substitution, string> =
    matchTypes pattern actual
    |> Result.bind consolidateBindings

/// Reconcile two types where one might contain type variables.
/// If one type is concrete and the other has type variables that can unify with it,
/// returns the concrete type. If both are concrete and equal, returns the type.
/// If both are concrete and different, returns None.
/// The optional aliasReg parameter allows type alias resolution before comparison.
let reconcileTypes (aliasReg: AliasRegistry option) (t1: Type) (t2: Type) : Type option =
    // Resolve type aliases if registry is provided
    let t1' = aliasReg |> Option.map (fun reg -> resolveType reg t1) |> Option.defaultValue t1
    let t2' = aliasReg |> Option.map (fun reg -> resolveType reg t2) |> Option.defaultValue t2

    if t1' = t2' then
        Some t1'
    elif t1' = TRuntimeError then
        Some t2'
    elif t2' = TRuntimeError then
        Some t1'
    elif t1' = TString && t2' = TChar then
        Some TString
    elif t1' = TChar && t2' = TString then
        Some TChar
    elif containsTVar t1' && not (containsTVar t2') then
        // t2 is concrete, check if t1 can unify with it
        match unifyTypes t1' t2' with
        | Ok _ -> Some t2'  // Return the concrete type
        | Error _ -> None
    elif not (containsTVar t1') && containsTVar t2' then
        // t1 is concrete, check if t2 can unify with it
        match unifyTypes t2' t1' with
        | Ok _ -> Some t1'  // Return the concrete type
        | Error _ -> None
    elif containsTVar t1' && containsTVar t2' then
        // Both have type variables - try to unify
        match unifyTypes t1' t2' with
        | Ok subst -> Some (applySubst subst t1')
        | Error _ -> None
    else
        None

/// Infer type arguments for a generic function call.
/// Given type parameters, parameter types (with type variables), and actual argument types,
/// returns the inferred type arguments in order matching typeParams.
/// Also takes optional function return type and expected return type for additional inference.
let inferTypeArgs (typeParams: string list) (paramTypes: Type list) (argTypes: Type list) (returnType: Type option) (expectedReturnType: Type option) : Result<Type list, string> =
    if List.length paramTypes <> List.length argTypes then
        Error $"Argument count mismatch: expected {List.length paramTypes}, got {List.length argTypes}"
    else
        // Match each parameter type against argument type
        let argMatchResults =
            List.zip paramTypes argTypes
            |> List.map (fun (paramT, argT) -> matchTypes paramT argT)

        // Also match return type against expected return type if both are provided
        let returnMatchResult =
            match returnType, expectedReturnType with
            | Some retT, Some expT -> matchTypes retT expT
            | _ -> Ok []

        // Combine all bindings
        (argMatchResults @ [returnMatchResult])
        |> List.fold (fun acc res ->
            match acc, res with
            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.bind consolidateBindings
        |> Result.bind (fun bindingMap ->
            // Extract type arguments in order of type parameters, preserving unresolved type vars.
            typeParams
            |> List.fold (fun acc paramName ->
                acc |> Result.bind (fun args ->
                    match Map.tryFind paramName bindingMap with
                    | Some typ -> Ok (args @ [typ])
                    | None -> Ok (args @ [TVar paramName])))
                (Ok []))

/// Look up a name already resolved and canonicalized by the semantic boundary.
let tryLookupResolved (name: string) (m: Map<string, 'a>) : ('a * string) option =
    Map.tryFind name m |> Option.map (fun value -> (value, name))

let private paramNameForLegacyError
    (funcParamNameReg: Map<string, string list>)
    (funcName: string)
    (paramIndex: int)
    : string =
    let zeroBasedParamIndex = paramIndex - 1

    let rec tryGetAtIndex (index: int) (remaining: string list) : string option =
        match remaining with
        | [] -> None
        | item :: rest ->
            if index = 0 then Some item else tryGetAtIndex (index - 1) rest

    let resolvedParamName =
        if zeroBasedParamIndex < 0 then
            None
        else
            tryLookupResolved funcName funcParamNameReg
            |> Option.bind (fun (paramNames, _resolvedName) -> tryGetAtIndex zeroBasedParamIndex paramNames)

    match resolvedParamName with
    | Some paramName -> paramName
    | None -> $"arg{paramIndex}"

/// Check expression type top-down, potentially transforming the expression.
/// Parameters:
///   - expr: Expression to type-check
///   - env: Type environment (variable name -> type mappings)
///   - typeReg: Type registry (record type name -> field definitions)
///   - variantLookup: Maps variant names to (type name, tag index)
///   - genericFuncReg: Registry of generic functions (function name -> type params)
///   - expectedType: Optional expected type from context (for checking)
/// Returns: Result<Type * Expr, TypeError>
///   - Type: The type of the expression
///   - Expr: The (possibly transformed) expression
let rec private checkExprWithParamNames
    (funcParamNameReg: Map<string, string list>)
    (expr: Expr)
    (env: TypeEnv)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (genericFuncReg: GenericFuncRegistry)
    (warningSettings: WarningSettings)
    (moduleRegistry: ModuleRegistry)
    (aliasReg: AliasRegistry)
    (expectedType: Type option)
    : Result<Type * Expr, TypeError> =
    let checkExpr
        (innerExpr: Expr)
        (innerEnv: TypeEnv)
        (innerTypeReg: IndexedTypeRegistry)
        (innerVariantLookup: VariantLookup)
        (innerGenericFuncReg: GenericFuncRegistry)
        (innerWarningSettings: WarningSettings)
        (innerModuleRegistry: ModuleRegistry)
        (innerAliasReg: AliasRegistry)
        (innerExpectedType: Type option)
        : Result<Type * Expr, TypeError> =
        checkExprWithParamNames
            funcParamNameReg
            innerExpr
            innerEnv
            innerTypeReg
            innerVariantLookup
            innerGenericFuncReg
            innerWarningSettings
            innerModuleRegistry
            innerAliasReg
            innerExpectedType

    let expectedType =
        expectedType
        |> Option.map (canonicalizeBareSumTypeRefs variantLookup)

    let rec tryFindCallArguments (targetName: string) (candidate: Expr) : Expr list option =
        let tryChildren children = children |> List.tryPick (tryFindCallArguments targetName)
        let letPatternShadows pattern =
            AST.letPatternBindings pattern |> List.contains targetName
        let matchPatternShadows pattern =
            AST.validateBinders (MatchBinderPattern pattern)
            |> Result.map (List.contains targetName)
            |> Result.defaultValue true

        match candidate with
        | Call (name, args) when name = targetName -> Some (NonEmptyList.toList args)
        | Apply (Var name, args) when name = targetName -> Some (NonEmptyList.toList args)
        | Let (pattern, value, body) ->
            match tryFindCallArguments targetName value with
            | Some args -> Some args
            | None when letPatternShadows pattern -> None
            | None -> tryFindCallArguments targetName body
        | Lambda (parameters, returnAnnotation, body) ->
            let shadowed =
                parameters
                |> NonEmptyList.toList
                |> List.collect (fun parameter -> AST.letPatternBindings parameter.Pattern)
                |> List.contains targetName
            if shadowed then None else tryFindCallArguments targetName body
        | Match (scrutinee, cases) ->
            match tryFindCallArguments targetName scrutinee with
            | Some args -> Some args
            | None ->
                cases
                |> List.tryPick (fun case ->
                    let shadows =
                        case.Patterns
                        |> NonEmptyList.toList
                        |> List.exists matchPatternShadows
                    let guardResult =
                        if shadows then None
                        else case.Guard |> Option.bind (tryFindCallArguments targetName)
                    guardResult
                    |> Option.orElseWith (fun () ->
                        if shadows then None
                        else tryFindCallArguments targetName case.Body))
        | BoundaryRender (_, value)
        | UnaryOp (_, value)
        | TupleAccess (value, _)
        | RecordAccess (value, _) -> tryFindCallArguments targetName value
        | BinOp (_, left, right)
        | Sequence (left, right) -> tryChildren [left; right]
        | If (condition, thenBranch, elseBranch) ->
            tryChildren [condition; thenBranch; elseBranch]
        | Call (_, args)
        | TypeApp (_, _, args) -> args |> NonEmptyList.toList |> tryChildren
        | TupleLiteral elements
        | ListLiteral elements -> tryChildren elements
        | DictLiteral (_, entries) -> entries |> List.map snd |> tryChildren
        | RecordLiteral (_, fields) -> fields |> List.map snd |> tryChildren
        | RecordUpdate (record, fields) -> record :: (fields |> List.map snd) |> tryChildren
        | Constructor (_, _, payload) -> payload |> Option.bind (tryFindCallArguments targetName)
        | ListCons (head, tail) -> head @ [tail] |> tryChildren
        | Apply (func, args)
        | IndirectApply (func, args) -> func :: NonEmptyList.toList args |> tryChildren
        | Closure (_, captures) -> tryChildren captures
        | InterpolatedString parts ->
            parts
            |> List.choose (function StringExpr inner -> Some inner | StringText _ -> None)
            |> tryChildren
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
        | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
        | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
        | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
        | Var _ | FuncRef _ | RuntimeError _ -> None

    let inferFunctionExpectationFromArguments
        (parameterCount: int)
        (arguments: Expr list)
        : Type option =
        if List.length arguments <> parameterCount then
            None
        else
            arguments
            |> ResultList.traverse (fun argument ->
                checkExpr argument env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                |> Result.map fst)
            |> Result.toOption
            |> Option.map (fun argumentTypes ->
                TFunction (argumentTypes, TVar "binding_return"))

    let rec tryFindFunctionValueExpectation
        (targetName: string)
        (candidate: Expr)
        : Type option =
        let tryChildren children =
            children |> List.tryPick (tryFindFunctionValueExpectation targetName)
        let fromCall (functionName: string) (arguments: Expr list) =
            match Map.tryFind functionName env with
            | Some (TFunction (parameterTypes, _)) when List.length parameterTypes = List.length arguments ->
                List.zip parameterTypes arguments
                |> List.tryPick (fun (parameterType, argument) ->
                    match argument with
                    | Var name when name = targetName -> Some parameterType
                    | _ -> tryFindFunctionValueExpectation targetName argument)
            | _ -> tryChildren arguments

        match candidate with
        | Call (functionName, arguments) -> fromCall functionName (NonEmptyList.toList arguments)
        | TypeApp (functionName, _, arguments) -> fromCall functionName (NonEmptyList.toList arguments)
        | Let (pattern, value, body) ->
            tryFindFunctionValueExpectation targetName value
            |> Option.orElseWith (fun () ->
                if letPatternBindings pattern |> List.contains targetName then None
                else tryFindFunctionValueExpectation targetName body)
        | Lambda (parameters, _, body) ->
            let shadows =
                parameters
                |> NonEmptyList.toList
                |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
                |> List.contains targetName
            if shadows then None else tryFindFunctionValueExpectation targetName body
        | BoundaryRender (_, value) | UnaryOp (_, value)
        | TupleAccess (value, _) | RecordAccess (value, _) ->
            tryFindFunctionValueExpectation targetName value
        | BinOp (_, left, right) | Sequence (left, right) -> tryChildren [left; right]
        | If (condition, thenBranch, elseBranch) -> tryChildren [condition; thenBranch; elseBranch]
        | TupleLiteral elements | ListLiteral elements -> tryChildren elements
        | DictLiteral (_, entries) -> entries |> List.map snd |> tryChildren
        | RecordLiteral (_, fields) -> fields |> List.map snd |> tryChildren
        | RecordUpdate (record, fields) -> record :: (fields |> List.map snd) |> tryChildren
        | Constructor (_, _, payload) -> payload |> Option.bind (tryFindFunctionValueExpectation targetName)
        | Match (scrutinee, cases) ->
            scrutinee
            :: (cases |> List.collect (fun case -> Option.toList case.Guard @ [case.Body]))
            |> tryChildren
        | ListCons (head, tail) -> head @ [tail] |> tryChildren
        | Apply (func, arguments)
        | IndirectApply (func, arguments) -> func :: NonEmptyList.toList arguments |> tryChildren
        | Closure (_, captures) -> tryChildren captures
        | InterpolatedString parts ->
            parts
            |> List.choose (function StringExpr inner -> Some inner | StringText _ -> None)
            |> tryChildren
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
        | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
        | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
        | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
        | Var _ | FuncRef _ | RuntimeError _ -> None

    match expr with
    | BoundaryRender (renderer, value) ->
        checkExpr value env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.map (fun (_, value') -> (TString, BoundaryRender (renderer, value')))
    | RuntimeError message -> Ok (TRuntimeError, RuntimeError message)
    | UnitLiteral ->
        // Unit literal is always TUnit
        match expectedType with
        | Some expected when not (typesCompatible expected TUnit) ->
            Error (TypeMismatch (expected, TUnit, "unit literal"))
        | _ -> Ok (TUnit, expr)

    | Int64Literal _ ->
        match expectedType with
        | Some TInt64 | None -> Ok (TInt64, expr)
        | Some other ->
            // Handle type variables (e.g., when expected is TVar "t")
            match reconcileTypes (Some aliasReg) other TInt64 with
            | Some TInt64 -> Ok (TInt64, expr)
            | _ -> Error (TypeMismatch (other, TInt64, "integer literal"))

    | Int128Literal _ ->
        match expectedType with
        | Some TInt128 | None -> Ok (TInt128, expr)
        | Some other ->
            match reconcileTypes (Some aliasReg) other TInt128 with
            | Some TInt128 -> Ok (TInt128, expr)
            | _ -> Error (TypeMismatch (other, TInt128, "integer literal"))

    | BigIntLiteral _ ->
        match expectedType with
        | Some TInt | None -> Ok (TInt, expr)
        | Some other -> Error (TypeMismatch (other, TInt, "Int literal"))

    | Int8Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TInt8) ->
            Error (TypeMismatch (expected, TInt8, "Int8 literal"))
        | _ -> Ok (TInt8, expr)

    | Int16Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TInt16) ->
            Error (TypeMismatch (expected, TInt16, "Int16 literal"))
        | _ -> Ok (TInt16, expr)

    | Int32Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TInt32) ->
            Error (TypeMismatch (expected, TInt32, "Int32 literal"))
        | _ -> Ok (TInt32, expr)

    | UInt8Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt8) ->
            Error (TypeMismatch (expected, TUInt8, "UInt8 literal"))
        | _ -> Ok (TUInt8, expr)

    | UInt16Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt16) ->
            Error (TypeMismatch (expected, TUInt16, "UInt16 literal"))
        | _ -> Ok (TUInt16, expr)

    | UInt32Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt32) ->
            Error (TypeMismatch (expected, TUInt32, "UInt32 literal"))
        | _ -> Ok (TUInt32, expr)

    | UInt64Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt64) ->
            Error (TypeMismatch (expected, TUInt64, "UInt64 literal"))
        | _ -> Ok (TUInt64, expr)

    | UInt128Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt128) ->
            Error (TypeMismatch (expected, TUInt128, "UInt128 literal"))
        | _ -> Ok (TUInt128, expr)

    | BoolLiteral _ ->
        // Boolean literals are always TBool
        match expectedType with
        | Some expected when not (typesCompatible expected TBool) ->
            Error (TypeMismatch (expected, TBool, "boolean literal"))
        | _ -> Ok (TBool, expr)

    | StringLiteral _ ->
        // String literals are always TString
        match expectedType with
        | Some expected when not (typesCompatibleWithAliases aliasReg expected TString) ->
            Error (TypeMismatch (expected, TString, "string literal"))
        | _ -> Ok (TString, expr)

    | CharLiteral _ ->
        // Char literals are always TChar (single Extended Grapheme Cluster)
        match expectedType with
        | Some expected when not (typesCompatible expected TChar) ->
            Error (TypeMismatch (expected, TChar, "char literal"))
        | _ -> Ok (TChar, expr)

    | InterpolatedString parts ->
        // Interpolated strings are always TString
        // Check that all expression parts are strings
        let rec checkParts (parts: StringPart list) (checkedParts: StringPart list) : Result<StringPart list, TypeError> =
            match parts with
            | [] -> Ok (List.rev checkedParts)
            | StringText s :: rest ->
                checkParts rest (StringText s :: checkedParts)
            | StringExpr e :: rest ->
                let checkedPartResult =
                    checkExpr e env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TString)
                let normalizedPartResult =
                    match checkedPartResult with
                    | Error (UndefinedVariable name) ->
                        Error (UndefinedCallTarget name)
                    | Error (TypeMismatch (expected, _, _)) when expected = TString ->
                        checkExpr e env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                        |> Result.mapError (fun innerErr ->
                            match innerErr with
                            | UndefinedVariable name -> UndefinedCallTarget name
                            | _ -> innerErr)
                        |> Result.bind (fun (actualType, checkedExpr) ->
                            Error (GenericError (interpolationTypeMismatchMessage checkedExpr actualType)))
                    | _ ->
                        checkedPartResult
                normalizedPartResult
                |> Result.bind (fun (partType, checkedExpr) ->
                    if partType = TString || partType = TChar then
                        checkParts rest (StringExpr checkedExpr :: checkedParts)
                    else
                        Error (GenericError (interpolationTypeMismatchMessage checkedExpr partType)))
        match checkParts parts [] with
        | Ok checkedParts ->
            match expectedType with
            | Some TString | None -> Ok (TString, InterpolatedString checkedParts)
            | Some other -> Error (TypeMismatch (other, TString, "interpolated string"))
        | Error err -> Error err

    | FloatLiteral _ ->
        // Float literals are always TFloat64
        match expectedType with
        | Some expected when not (typesCompatible expected TFloat64) ->
            Error (TypeMismatch (expected, TFloat64, "float literal"))
        | _ -> Ok (TFloat64, expr)

    | BinOp (op, left, right) ->
        match op with
        // Arithmetic operators: T -> T -> T (where T is int or float)
        | Add | Sub | Mul | Div | Mod ->
            let opName =
                match op with
                | Add -> "+"
                | Sub -> "-"
                | Mul -> "*"
                | Div -> "/"
                | Mod -> "%"
                | _ -> Crash.crash $"Non-arithmetic operator reached arithmetic type-checking path: {op}"

            let tryAsNumericType (typ: Type) : Type option =
                match resolveType aliasReg typ with
                | TInt8 | TInt16 | TInt32 | TInt64 | TInt
                | TUInt8 | TUInt16 | TUInt32 | TUInt64
                | TFloat64 as numeric ->
                    Some numeric
                | _ ->
                    None

            match tryExtractKnownTestRuntimeErrorMessage Map.empty left with
            | Some msg ->
                Error (GenericError $"Uncaught exception: {msg}")
            | None ->
                // Check left operand to determine numeric type
                checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                |> Result.bind (fun (leftType, left') ->
                    match tryAsNumericType leftType with
                    | Some leftNumericType ->
                        // Runtime Dark operators inspect both operand values before
                        // deciding whether their numeric representations agree.
                        checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                        |> Result.bind (fun (rightType, right') ->
                            if rightType <> leftNumericType then
                                let errorMessage =
                                    $"Cannot perform numeric operation on {typeToString leftType} and {typeToString rightType}"
                                let evaluatedOperands = "__dark_numeric_operands"
                                Ok (
                                    leftNumericType,
                                    Let (
                                        LPVariable evaluatedOperands,
                                        TupleLiteral [left'; right'],
                                        RuntimeError errorMessage
                                    )
                                )
                            else
                                match expectedType with
                                | Some expected when expected <> leftNumericType ->
                                    Error (TypeMismatch (expected, leftNumericType, $"result of {opName}"))
                                | _ -> Ok (leftNumericType, BinOp (op, left', right')))
                    | None ->
                        match leftType with
                        | TVar _ ->
                            let rightExpectedType =
                                match expectedType with
                                | Some expected ->
                                    match tryAsNumericType expected with
                                    | Some numericExpected -> Some numericExpected
                                    | None -> None
                                | None ->
                                    None
                            checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg rightExpectedType
                            |> Result.bind (fun (rightType, right') ->
                                let inferredNumericType =
                                    match rightExpectedType with
                                    | Some numericExpected ->
                                        Some numericExpected
                                    | None ->
                                        tryAsNumericType rightType
                                match inferredNumericType with
                                | Some numericType ->
                                    match expectedType with
                                    | Some expected when not (typesCompatible expected numericType) ->
                                        Error (TypeMismatch (expected, numericType, $"result of {opName}"))
                                    | _ ->
                                        Ok (numericType, BinOp (op, left', right'))
                                | None ->
                                    Error (InvalidOperation (opName, [leftType])))
                        | other ->
                            Error (InvalidOperation (opName, [other])))

        // Comparison operators: T -> T -> bool
        // Eq and Neq: work on any type (structural equality for complex types)
        // Lt, Gt, Lte, Gte: only work on numeric types
        | Eq | Neq | Lt | Gt | Lte | Gte ->
            let opName =
                match op with
                | Eq -> "=="
                | Neq -> "!="
                | Lt -> "<"
                | Gt -> ">"
                | Lte -> "<="
                | Gte -> ">="
                | _ -> Crash.crash $"Non-comparison operator reached comparison type-checking path: {op}"

            let lambdaLiteralFastPath : Result<Type * Expr, TypeError> option =
                let comparisonResult =
                    checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                    |> Result.bind (fun (leftType, left') ->
                        // Check without context first so rejected comparisons keep
                        // the right operand's actual type in their diagnostic.
                        let rightWithoutContext =
                            checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                        let rightResult =
                            match rightWithoutContext with
                            | Ok (rightType, _) when containsTVar rightType ->
                                checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                            | Ok _ -> rightWithoutContext
                            | Error _ ->
                                checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                        rightResult
                        |> Result.bind (fun (rightType, right') ->
                            classifyComparison aliasReg typeReg variantLookup op leftType rightType
                            |> Result.bind (fun plan ->
                                let comparisonExpr =
                                    match plan with
                                    | EqualityComparison comparableType ->
                                        let equality =
                                            buildEqExprForType aliasReg variantLookup comparableType left' right'
                                        if op = Neq then UnaryOp (Not, equality) else equality
                                    | OrderingComparison numericType ->
                                        buildOrderingExprForType op numericType left' right'
                                match expectedType with
                                | Some TBool | None -> Ok (TBool, comparisonExpr)
                                | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}")))))
                Some comparisonResult

            match lambdaLiteralFastPath with
            | Some result ->
                result
            | None ->
                // Check left operand to determine type
                checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                |> Result.bind (fun (leftType, left') ->
                    match op with
                    | Eq | Neq ->
                    // Equality works on any type - both operands must be same type
                        let rightWithExpected =
                            checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                        let rightResult =
                            match resolveType aliasReg leftType, rightWithExpected with
                            | TSum _, Error (TypeMismatch _) ->
                                // A distinct nominal enum is a valid equality
                                // operand. Retry without forcing the left type;
                                // same-type generic calls keep the contextual
                                // inference from the successful first check.
                                checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                            | _ -> rightWithExpected

                        rightResult
                        |> Result.bind (fun (rightType, right') ->
                            let distinctNominalResult =
                                match resolveType aliasReg leftType, resolveType aliasReg rightType with
                                | TSum (leftName, _), TSum (rightName, _) when leftName <> rightName ->
                                    let evaluatedOperands = "__dark_nominal_equality_operands"
                                    let result = BoolLiteral (op = Neq)
                                    Some (
                                        Ok (
                                            TBool,
                                            Let (
                                                LPVariable evaluatedOperands,
                                                TupleLiteral [left'; right'],
                                                result
                                            )
                                        )
                                    )
                                | _ -> None

                            match distinctNominalResult with
                            | Some result -> result
                            | None ->
                            // In generic contexts, one side can still contain type variables
                            // while the other side has become concrete.
                            match reconcileTypes (Some aliasReg) leftType rightType with
                            | None ->
                                Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                            | Some comparableType ->
                                let tryNamedPartialState (candidate: Expr) : (string * (Type * Expr) list) option =
                                    match candidate with
                                    | Lambda (parameters, returnAnnotation, body) ->
                                        let parameterList = NonEmptyList.toList parameters
                                        let generatedPartial =
                                            parameterList
                                            |> List.forall (fun parameter ->
                                                match parameter.Pattern with
                                                | LPVariable name -> name.StartsWith "__partial_"
                                                | _ -> false)

                                        let callDetails =
                                            match body with
                                            | Call (name, args) -> Some (name, [], NonEmptyList.toList args)
                                            | TypeApp (name, typeArgs, args) -> Some (name, typeArgs, NonEmptyList.toList args)
                                            | _ -> None

                                        match generatedPartial, callDetails with
                                        | true, Some (name, typeArgs, callArgs) ->
                                            let remainingCount = List.length parameterList
                                            let appliedCount = List.length callArgs - remainingCount
                                            let trailingArgs =
                                                if appliedCount >= 0 then List.skip appliedCount callArgs else []
                                            let trailingAreParameters =
                                                appliedCount > 0
                                                && List.length trailingArgs = remainingCount
                                                && List.forall2
                                                    (fun arg parameter ->
                                                        match parameter.Pattern with
                                                        | LPVariable parameterName -> arg = Var parameterName
                                                        | _ -> false)
                                                    trailingArgs
                                                    parameterList

                                            let concreteFunctionType =
                                                match Map.tryFind name env with
                                                | Some functionType when List.isEmpty typeArgs -> Some functionType
                                                | Some functionType ->
                                                    match Map.tryFind name genericFuncReg.Functions with
                                                    | Some typeParams when List.length typeParams = List.length typeArgs ->
                                                        let subst = List.zip typeParams typeArgs |> Map.ofList
                                                        Some (applySubst subst functionType)
                                                    | _ -> None
                                                | None -> None

                                            match trailingAreParameters, concreteFunctionType with
                                            | true, Some (TFunction (parameterTypes, _)) when appliedCount <= List.length parameterTypes ->
                                                let identity =
                                                    if List.isEmpty typeArgs then name
                                                    else
                                                        let typeArgText = typeArgs |> List.map typeToString |> String.concat ", "
                                                        $"{name}<{typeArgText}>"
                                                Some (
                                                    identity,
                                                    List.zip
                                                        (List.take appliedCount parameterTypes)
                                                        (List.take appliedCount callArgs)
                                                )
                                            | _ -> None
                                        | _ -> None
                                    | _ -> None

                                let buildNamedPartialEquality
                                    (leftIdentity: string, leftState: (Type * Expr) list)
                                    (rightIdentity: string, rightState: (Type * Expr) list)
                                    : Expr =
                                    let leftBindings =
                                        leftState
                                        |> List.mapi (fun index (typ, value) -> ($"__dark_partial_left_{index}", typ, value))
                                    let rightBindings =
                                        rightState
                                        |> List.mapi (fun index (typ, value) -> ($"__dark_partial_right_{index}", typ, value))
                                    let stateComparisons =
                                        if leftIdentity = rightIdentity && List.length leftBindings = List.length rightBindings then
                                            List.map2
                                                (fun (leftName, typ, _) (rightName, _, _) ->
                                                    buildEqExprForType
                                                        aliasReg
                                                        variantLookup
                                                        typ
                                                        (Var leftName)
                                                        (Var rightName))
                                                leftBindings
                                                rightBindings
                                        else
                                            [BoolLiteral false]
                                    let comparison = chainAndExpr stateComparisons
                                    (leftBindings @ rightBindings)
                                    |> List.foldBack
                                        (fun (name, _, value) body -> Let (LPVariable name, value, body))
                                        <| comparison

                                let eqExpr =
                                    match resolveType aliasReg comparableType, tryNamedPartialState left', tryNamedPartialState right' with
                                    | TFunction _, Some leftPartial, Some rightPartial ->
                                        buildNamedPartialEquality leftPartial rightPartial
                                    | _ ->
                                        buildEqExprForType aliasReg variantLookup comparableType left' right'
                                let comparisonExpr =
                                    if op = Neq then
                                        UnaryOp (Not, eqExpr)
                                    else
                                        eqExpr
                                match expectedType with
                                | Some TBool | None -> Ok (TBool, comparisonExpr)
                                | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}")))
                    | Lt | Gt | Lte | Gte ->
                        // Ordering only works on numeric types.
                        // Allow unresolved type variables here so guards like
                        // `match Error 5 with | Ok x when x > 2 -> ...` can infer x as Int64.
                        checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                        |> Result.bind (fun (rightType, right') ->
                            match reconcileTypes (Some aliasReg) leftType rightType with
                            | None ->
                                Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                            | Some comparableType ->
                                match comparableType with
                                | TInt8 | TInt16 | TInt32 | TInt64
                                | TInt
                                | TUInt8 | TUInt16 | TUInt32 | TUInt64
                                | TFloat64 ->
                                    match expectedType with
                                    | Some TBool | None -> Ok (TBool, BinOp (op, left', right'))
                                    | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))
                                | other ->
                                    Error (InvalidOperation (opName, [other])))
                    | _ ->
                        Error (GenericError $"Unexpected comparison operator: {opName}"))

        // Boolean operators: bool -> bool -> bool
        | And | Or ->
            let opName = if op = And then "&&" else "||"

            let checkBooleanOperand (operand: Expr) : Result<Expr, TypeError> =
                let checkedOperandResult =
                    match op with
                    | And ->
                        checkExpr operand env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TBool)
                    | Or ->
                        checkExpr operand env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                    | _ ->
                        checkExpr operand env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                match checkedOperandResult with
                | Ok (operandType, operand') when operandType = TBool ->
                    Ok operand'
                | Ok _ ->
                    Error (GenericError $"{opName} only supports Booleans")
                | Error (TypeMismatch (expected, _, _)) when op = And && expected = TBool ->
                    Error (GenericError $"{opName} only supports Booleans")
                | Error err ->
                    Error err

            match (op, tryExtractKnownTestRuntimeErrorMessage Map.empty left) with
            | And, Some msg ->
                Error (GenericError msg)
            | _ ->
                checkBooleanOperand left
                |> Result.bind (fun left' ->
                    let rightIsKnownRuntimeError = isKnownTestRuntimeErrorExpr Map.empty right
                    let shortCircuitResult =
                        match (op, left', rightIsKnownRuntimeError) with
                        | (And, BoolLiteral false, true) -> Some false
                        | (Or, BoolLiteral true, true) -> Some true
                        | _ -> None
                    match shortCircuitResult with
                    | Some result ->
                        match expectedType with
                        | Some TBool | None -> Ok (TBool, BoolLiteral result)
                        | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))
                    | None ->
                        match (op, tryExtractKnownTestRuntimeErrorMessage Map.empty right) with
                        | And, Some msg ->
                            Error (GenericError msg)
                        | _ ->
                            checkBooleanOperand right
                            |> Result.bind (fun right' ->
                                match expectedType with
                                | Some TBool | None -> Ok (TBool, BinOp (op, left', right'))
                                | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))))

        // Bitwise operators: Int -> Int -> Int (same integer type)
        | Shl | Shr | BitAnd | BitOr | BitXor ->
            let opName =
                match op with
                | Shl -> "<<"
                | Shr -> ">>"
                | BitAnd -> "&"
                | BitOr -> "|"
                | BitXor -> "^"
                | _ -> Crash.crash $"Non-bitwise operator reached bitwise type-checking path: {op}"

            let isIntegerType (typ: Type) =
                match typ with
                | TInt8 | TInt16 | TInt32 | TInt64 | TInt
                | TUInt8 | TUInt16 | TUInt32 | TUInt64 -> true
                | _ -> false

            checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.bind (fun (leftType, left') ->
                if not (isIntegerType leftType) then
                    Error (InvalidOperation (opName, [leftType]))
                else
                    checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> leftType then
                            Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some expected when expected <> leftType ->
                                Error (TypeMismatch (expected, leftType, $"result of {opName}"))
                            | _ -> Ok (leftType, BinOp (op, left', right'))))

        // String concatenation: string -> string -> string
        | StringConcat ->
            match tryExtractKnownTestRuntimeErrorMessage Map.empty left with
            | Some msg ->
                Error (GenericError $"Uncaught exception: {msg}")
            | None ->
                checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TString)
                |> Result.bind (fun (leftType, left') ->
                    let isStringLike t = t = TString || t = TChar
                    if not (isStringLike leftType) then
                        Error (InvalidOperation ("++", [leftType]))
                    else
                        match tryExtractKnownTestRuntimeErrorMessage Map.empty right with
                        | Some msg ->
                            Error (GenericError $"Uncaught exception: {msg}")
                        | None ->
                            checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TString)
                            |> Result.bind (fun (rightType, right') ->
                                if not (isStringLike rightType) then
                                    Error (TypeMismatch (TString, rightType, "right operand of ++"))
                                else
                                    match expectedType with
                                    | Some TString | None -> Ok (TString, BinOp (op, left', right'))
                                    | Some other -> Error (TypeMismatch (other, TString, "result of ++"))))

    | UnaryOp (op, inner) ->
        match op with
        | Neg ->
            // Negation works on integer and float numeric types
            checkExpr inner env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.bind (fun (innerType, inner') ->
                match innerType with
                | TInt8 | TInt16 | TInt32 | TInt64 | TInt
                | TUInt8 | TUInt16 | TUInt32 | TUInt64
                | TFloat64 ->
                    match expectedType with
                    | Some expected when expected <> innerType ->
                        Error (TypeMismatch (expected, innerType, "result of negation"))
                    | _ -> Ok (innerType, UnaryOp (op, inner'))
                | other ->
                    Error (InvalidOperation ("-", [other])))

        | Not ->
            // Boolean not works on booleans and returns booleans
            checkExpr inner env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TBool)
            |> Result.bind (fun (innerType, inner') ->
                if innerType <> TBool then
                    Error (TypeMismatch (TBool, innerType, "operand of !"))
                else
                    match expectedType with
                    | Some TBool | None -> Ok (TBool, UnaryOp (op, inner'))
                    | Some other -> Error (TypeMismatch (other, TBool, "result of !")))

        | BitNot ->
            // Bitwise NOT works on integer types and preserves the operand type
            let isIntegerType (typ: Type) =
                match typ with
                | TInt8 | TInt16 | TInt32 | TInt64 | TInt
                | TUInt8 | TUInt16 | TUInt32 | TUInt64 -> true
                | _ -> false

            checkExpr inner env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.bind (fun (innerType, inner') ->
                if not (isIntegerType innerType) then
                    Error (InvalidOperation ("~~~", [innerType]))
                else
                    match expectedType with
                    | Some expected when expected <> innerType ->
                        Error (TypeMismatch (expected, innerType, "result of ~~~"))
                    | _ -> Ok (innerType, UnaryOp (op, inner')))

    | Let (pattern, value, body) ->
        // The RHS is checked in the incoming environment. Only a completely
        // validated and type-compatible pattern extends the continuation.
        let valueExpectedType =
            match pattern, value with
            | LPVariable name, Lambda (parameters, _, _) ->
                tryFindFunctionValueExpectation name body
                |> Option.orElseWith (fun () ->
                    tryFindCallArguments name body
                    |> Option.bind (inferFunctionExpectationFromArguments (parameters |> NonEmptyList.toList |> List.length)))
            | _, ListLiteral [] -> Some (TList (TVar "t"))
            | _ -> None

        checkExpr value env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg valueExpectedType
        |> Result.bind (fun (valueType, value') ->
            let valueType = canonicalizeBareSumTypeRefs variantLookup valueType
            match validateBinders (LetBinderPatterns [pattern]) with
            | Error message -> Error (GenericError message)
            | Ok _ ->
                match bindLetPatternTypes pattern valueType with
                | None ->
                    let renderedValue =
                        tryFormatLiteralValue value'
                        |> Option.defaultValue $"<{typeToString valueType}>"
                    let message =
                        $"Could not deconstruct value {renderedValue} into pattern {formatLetDeconstructionPattern pattern}"
                    Ok (TRuntimeError, Let (pattern, value', RuntimeError message))
                | Some bindings ->
                    let env' =
                        bindings |> List.fold (fun current (name, typ) -> Map.add name typ current) env
                    let bodyForChecking =
                        match pattern, value' with
                        | LPVariable name, (FloatLiteral _ | Int64Literal _) ->
                            substituteInterpolationLiteral name value' body
                        | _ -> body
                    checkExpr bodyForChecking env' typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType
                    |> Result.map (fun (bodyType, body') ->
                        (bodyType, Let (pattern, value', body'))))

    | Var name ->
        if isBuiltinTestNanName name then
            let builtinExpr = Var "Builtin.testNan"
            match expectedType with
            | Some expected ->
                match reconcileTypes (Some aliasReg) expected TFloat64 with
                | Some reconciledType -> Ok (reconciledType, builtinExpr)
                | None -> Error (TypeMismatch (expected, TFloat64, $"variable {name}"))
            | None -> Ok (TFloat64, builtinExpr)
        else
            // Variable reference: look up in environment
            match tryLookupResolved name env with
            | Some (varType, resolvedName) ->
                let varType = canonicalizeBareSumTypeRefs variantLookup varType

                match expectedType with
                | Some expected ->
                    match reconcileTypes (Some aliasReg) expected varType with
                    | Some reconciledType -> Ok (reconciledType, Var resolvedName)
                    | None -> Error (TypeMismatch (expected, varType, $"variable {name}"))
                | None -> Ok (varType, Var resolvedName)
            | None ->
                match Stdlib.tryGetValue name with
                | Some moduleValue ->
                    match expectedType with
                    | Some expected ->
                        match reconcileTypes (Some aliasReg) expected moduleValue.Type with
                        | Some reconciledType -> Ok (reconciledType, Var moduleValue.Name)
                        | None -> Error (TypeMismatch (expected, moduleValue.Type, $"variable {name}"))
                    | None -> Ok (moduleValue.Type, Var moduleValue.Name)
                | None ->
                    // Check if it's a module function (e.g., Stdlib.Int64.add)
                    match Stdlib.tryGetFunction moduleRegistry name with
                    | Some (moduleFunc, resolvedName) ->
                        let funcType = Stdlib.getFunctionType moduleFunc
                        match expectedType with
                        | Some expected ->
                            match reconcileTypes (Some aliasReg) expected funcType with
                            | Some reconciledType -> Ok (reconciledType, Var resolvedName)
                            | None -> Error (TypeMismatch (expected, funcType, $"variable {name}"))
                        | None -> Ok (funcType, Var resolvedName)
                    | None ->
                        Error (UndefinedVariable name)

    | If (cond, thenBranch, elseBranch) ->
        // If expression: condition must be bool, branches must have same type
        checkExpr cond env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (condType, cond') ->
            let normalizedConditionResult : Result<Expr, TypeError> =
                if condType = TBool then
                    Ok cond'
                else
                    let conditionIsKnownFailure =
                        isKnownUnwrapFailureExpr Map.empty cond
                        || isKnownUnwrapFailureExpr Map.empty cond'
                        || isKnownTestRuntimeErrorExpr Map.empty cond
                        || isKnownTestRuntimeErrorExpr Map.empty cond'

                    if conditionIsKnownFailure then
                        checkExpr cond env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TBool)
                        |> Result.bind (fun (resolvedCondType, resolvedCondExpr) ->
                            if resolvedCondType = TBool then
                                Ok resolvedCondExpr
                            else
                                Error (GenericError (ifConditionTypeMismatchMessage resolvedCondExpr resolvedCondType)))
                    else
                        Error (GenericError (ifConditionTypeMismatchMessage cond' condType))

            normalizedConditionResult
            |> Result.bind (fun normalizedCond ->
                checkExpr thenBranch env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType
                |> Result.bind (fun (thenType, then') ->
                    let elseExpectedType =
                        // If an outer context already provides an expected type, keep using it.
                        // Otherwise, use the then-branch type to type-check the else-branch.
                        // This lets bottom-like runtime-failing expressions (e.g. unwrap None)
                        // inhabit the enclosing branch type.
                        match expectedType with
                        | Some outerExpected -> Some outerExpected
                        | None -> Some thenType

                    let elseResult =
                        match checkExpr elseBranch env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg elseExpectedType with
                        | Ok checkedElse ->
                            Ok checkedElse
                        | Error originalErr ->
                            // When no outer expected type exists, we type-check else with then-type context.
                            // If that fails due to contextual mismatch, re-check else unconstrained so the
                            // final diagnostic can report branch-vs-branch mismatch, not literal mismatch.
                            match expectedType, originalErr with
                            | None, TypeMismatch (expectedElse, _, _) when expectedElse = thenType ->
                                checkExpr elseBranch env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                            | _ ->
                                Error originalErr

                    elseResult
                    |> Result.bind (fun (elseType, else') ->
                        let reconciledBranchType =
                            match reconcileTypes (Some aliasReg) thenType elseType with
                            | Some reconciledType ->
                                Some reconciledType
                            | None ->
                                let thenIsKnownFailure =
                                    isKnownUnwrapFailureExpr Map.empty thenBranch
                                    || isKnownUnwrapFailureExpr Map.empty then'

                                let elseIsKnownFailure =
                                    isKnownUnwrapFailureExpr Map.empty elseBranch
                                    || isKnownUnwrapFailureExpr Map.empty else'

                                if thenIsKnownFailure && not elseIsKnownFailure then
                                    Some elseType
                                elif elseIsKnownFailure && not thenIsKnownFailure then
                                    Some thenType
                                else
                                    None

                        match reconciledBranchType with
                        | None ->
                            Error (IfBranchTypeMismatch (thenType, elseType))
                        | Some reconciledType ->
                            match expectedType with
                            | Some expected ->
                                match reconcileTypes (Some aliasReg) expected reconciledType with
                                | Some reconciledExpected -> Ok (reconciledExpected, If (normalizedCond, then', else'))
                                | None -> Error (TypeMismatch (expected, reconciledType, "if expression"))
                            | _ -> Ok (reconciledType, If (normalizedCond, then', else'))))))

    | Sequence (first, next) ->
        // The interpreter checks this at the statement boundary. The compiler
        // enforces the same Unit contract statically and uses only the final
        // expression to determine the sequence's result type.
        checkExpr first env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TUnit)
        |> Result.bind (fun (_, first') ->
            checkExpr next env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType
            |> Result.map (fun (nextType, next') ->
                (nextType, Sequence (first', next'))))

    | Call (funcName, args) ->
        // The resolution boundary has already attached the canonical callable
        // identity. Type checking only validates that identity's signature.
        let args = NonEmptyList.toList args
        if isBuiltinUnwrapName funcName then
            match args with
            | [argExpr] ->
                checkExpr argExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                |> Result.bind (fun (argType, argExpr') ->
                    let unwrapTypeResult =
                        match resolveType aliasReg argType with
                        | TSum ("Stdlib.Option.Option", [valueType]) -> Ok valueType
                        | TSum ("Stdlib.Result.Result", [okType; _]) -> Ok okType
                        | actualType ->
                            Error (GenericError $"Can only unwrap Options and Results, yet got {typeToString actualType}")

                    unwrapTypeResult
                    |> Result.bind (fun outputType ->
                        // `Option.None |> Builtin.unwrap` and `Result.Error(_) |> Builtin.unwrap`
                        // are guaranteed runtime failures. When unconstrained, their payload type
                        // remains a type variable; normalize to Unit to keep IR monomorphic.
                        let normalizedOutputType =
                            if isKnownFailureConstructorExpr argExpr' then
                                match expectedType with
                                // Bottom-like behavior: if context expects a type, use it.
                                | Some expected -> expected
                                // Unconstrained top-level failures still need a concrete type.
                                | None when containsTVar outputType -> TUnit
                                | None -> outputType
                            else
                                outputType

                        match expectedType with
                        | Some expected ->
                            match reconcileTypes (Some aliasReg) expected normalizedOutputType with
                            | Some reconciledType ->
                                Ok (reconciledType, Call ("Builtin.unwrap", NonEmptyList.singleton argExpr'))
                            | None ->
                                Error (TypeMismatch (expected, normalizedOutputType, $"result of call to {funcName}"))
                        | None ->
                            Ok (normalizedOutputType, Call ("Builtin.unwrap", NonEmptyList.singleton argExpr'))))
            | _ ->
                Error (GenericError $"Function {funcName} expects 1 arguments, got {List.length args}")
        elif isBuiltinTestRuntimeErrorName funcName then
            match args with
            | [argExpr] ->
                checkExpr argExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TString)
                |> Result.bind (fun (_argType, argExpr') ->
                    let outputType =
                        match expectedType with
                        | Some expected -> expected
                        | None -> TRuntimeError
                    Ok (outputType, Call ("Builtin.testRuntimeError", NonEmptyList.singleton argExpr')))
            | _ ->
                Error (GenericError $"Function {funcName} expects 1 arguments, got {List.length args}")
        else
            match tryLookupResolved funcName env with
            | Some (TFunction (origParamTypes, origReturnType), resolvedFuncName) ->
                (
            // Check if this is a generic function.
            match tryLookupResolved resolvedFuncName genericFuncReg.Functions with
            | Some (origTypeParams, _) when
                genericFuncReg.RequireExplicitTypeArgsForBareCalls
                && Option.isNone expectedType
                && not (resolvedFuncName.Contains(".")) ->
                // Bare user-defined generic calls must provide explicit type arguments.
                // Module-scoped names (for example Stdlib.List.map) still infer type args.
                let expectedTypeArgCount = List.length origTypeParams
                Error (GenericError (formatTypeArgumentArityError funcName expectedTypeArgCount 0))
            | Some (origTypeParams, _) ->
                // Freshen type params to avoid name clashes with caller's scope
                let (freshTypeParams, renaming) = freshenTypeParams origTypeParams
                let paramTypes = origParamTypes |> List.map (applyTypeVarRenaming renaming)
                let returnType = applyTypeVarRenaming renaming origReturnType
                let typeParams = freshTypeParams
                // Generic function called without explicit type args: infer them
                let numParams = List.length paramTypes
                let args = normalizeNullaryCallArgs numParams args
                let numArgs = List.length args

                if numArgs > numParams then
                    Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
                else if numArgs < numParams then
                    // Partial application of generic function
                    let providedParamTypes = List.take numArgs paramTypes
                    let remainingParamTypes = List.skip numArgs paramTypes

                    // Type-check the provided arguments
                    let rec checkProvidedArgs remaining paramTys accTypes accExprs =
                        match remaining, paramTys with
                        | [], [] -> Ok (List.rev accTypes, List.rev accExprs)
                        | arg :: restArgs, paramT :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                            |> Result.bind (fun (argType, arg') ->
                                checkProvidedArgs restArgs restParams (argType :: accTypes) (arg' :: accExprs))
                        | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                    checkProvidedArgs args providedParamTypes [] []
                    |> Result.bind (fun (argTypes, args') ->
                        // Infer type arguments from provided args (some may remain as TVar)
                        inferTypeArgs typeParams providedParamTypes argTypes (Some returnType) None
                        |> Result.mapError GenericError
                        |> Result.bind (fun inferredTypeArgs ->
                            // Build substitution and compute concrete types for remaining params
                            buildSubstitution typeParams inferredTypeArgs
                            |> Result.mapError GenericError
                            |> Result.bind (fun subst ->
                                let concreteRemainingParamTypes = List.map (applySubst subst) remainingParamTypes
                                let concreteReturnType = applySubst subst returnType
                                let concreteArgs = List.map (applySubstToExpr subst) args'

                                // Create unique parameter names for the remaining parameters
                                let remainingParams = makePartialParams resolvedFuncName concreteRemainingParamTypes

                                // Create the lambda body: TypeApp with all args
                                let allArgs = concreteArgs @ (remainingParams |> List.map (fun (name, _) -> Var name))
                                let lambdaBody = TypeApp (resolvedFuncName, inferredTypeArgs, toCallArgs allArgs)

                                // Create the lambda: fun p0 p1 ... -> funcName<types>(providedArgs, p0, p1, ...)
                                let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                                // The resulting type is a function from remaining params to return type
                                let partialType = TFunction (concreteRemainingParamTypes, concreteReturnType)

                                match expectedType with
                                | Some expected when not (typesCompatible expected partialType) ->
                                    Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                                | _ -> Ok (partialType, lambdaExpr))))
                else
                    // Full application - type-check arguments left-to-right while propagating bindings.
                    let rec checkArgsWithBindings remaining remainingParamTypes accTypes accExprs accBindings =
                        match remaining, remainingParamTypes with
                        | [], [] ->
                            Ok (List.rev accTypes, List.rev accExprs)
                        | arg :: restArgs, paramT :: restParams ->
                            consolidateBindings accBindings
                            |> Result.mapError GenericError
                            |> Result.bind (fun bindingMap ->
                                let concreteParamType = applySubst bindingMap paramT
                                checkExpr
                                    arg
                                    env
                                    typeReg
                                    variantLookup
                                    genericFuncReg
                                    warningSettings
                                    moduleRegistry
                                    aliasReg
                                    (Some concreteParamType)
                                |> Result.bind (fun (argType, arg') ->
                                    match matchTypes concreteParamType argType with
                                    | Ok newBindings ->
                                        let combinedBindings = accBindings @ newBindings
                                        consolidateBindings combinedBindings
                                        |> Result.mapError GenericError
                                        |> Result.bind (fun combinedBindingMap ->
                                            let concreteArgType = applySubst combinedBindingMap argType
                                            checkArgsWithBindings
                                                restArgs
                                                restParams
                                                (concreteArgType :: accTypes)
                                                (arg' :: accExprs)
                                                combinedBindings)
                                    | Error msg ->
                                        Error (TypeMismatch (concreteParamType, argType, $"argument to {funcName}: {msg}")))
                            )
                        | _ ->
                            Error (GenericError "Argument count mismatch")

                    checkArgsWithBindings args paramTypes [] [] []
                    |> Result.bind (fun (argTypes, args') ->
                        // Infer type arguments from parameter types, argument types, and expected return type
                        inferTypeArgs typeParams paramTypes argTypes (Some returnType) expectedType
                        |> Result.mapError GenericError
                        |> Result.bind (fun inferredTypeArgs ->
                            // Build substitution and compute concrete types
                            buildSubstitution typeParams inferredTypeArgs
                            |> Result.mapError GenericError
                            |> Result.bind (fun subst ->
                                let concreteReturnType = applySubst subst returnType
                                // Apply substitution to nested expressions (e.g., inner TypeApp nodes)
                                // This ensures that when empty() returns Dict<k$3, v$4> and we later
                                // infer k$3 -> Int64, v$4 -> Int64, the inner TypeApp gets updated
                                let concreteArgs = List.map (applySubstToExpr subst) args'
                                match expectedType with
                                | Some expected when not (typesCompatible expected concreteReturnType) ->
                                    Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                                | _ ->
                                    // Transform Call to TypeApp with inferred type arguments (using resolved name)
                                    Ok (
                                        concreteReturnType,
                                        TypeApp (resolvedFuncName, inferredTypeArgs, toCallArgs concreteArgs)
                                    ))))

            | None ->
                // Non-generic function: regular call or partial application
                let numParams = List.length origParamTypes
                let args = normalizeNullaryCallArgs numParams args
                let numArgs = List.length args
                if numArgs > numParams then
                    Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
                else if numArgs < numParams then
                    // Partial application: type-check provided args, then create lambda for remaining
                    let providedParamTypes = List.take numArgs origParamTypes
                    let remainingParamTypes = List.skip numArgs origParamTypes

                    // Type-check the provided arguments
                    let rec checkProvidedArgs remaining paramTys accArgs =
                        match remaining, paramTys with
                        | [], [] -> Ok (List.rev accArgs)
                        | arg :: restArgs, paramT :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                            |> Result.bind (fun (argType, arg') ->
                                if typesCompatibleWithAliases aliasReg paramT argType then
                                    checkProvidedArgs restArgs restParams (arg' :: accArgs)
                                else
                                    Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                        | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                    checkProvidedArgs args providedParamTypes []
                    |> Result.bind (fun args' ->
                        // Create unique parameter names for the remaining parameters
                        let remainingParams = makePartialParams resolvedFuncName remainingParamTypes

                        // Create the lambda body: call the original function with all args (using resolved name)
                        let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                        let lambdaBody = Call (resolvedFuncName, toCallArgs allArgs)

                        // Create the lambda: fun p0 p1 ... -> funcName(providedArgs, p0, p1, ...)
                        let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                        // The resulting type is a function from remaining params to return type
                        let partialType = TFunction (remainingParamTypes, origReturnType)

                        match expectedType with
                        | Some expected when not (typesCompatibleWithAliases aliasReg expected partialType) ->
                            Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                        | _ -> Ok (partialType, lambdaExpr))
                else
                    // Check each argument type and collect transformed args
                    let rec checkArgsWithTypes remaining paramTys paramIndex accArgs =
                        match remaining, paramTys with
                        | [], [] -> Ok (List.rev accArgs)
                        | arg :: restArgs, paramT :: restParams ->
                            let paramName =
                                paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                            |> Result.mapError (fun err ->
                                match err with
                                | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                    GenericError
                                        (formatLegacyParamTypeError
                                            funcName
                                            paramIndex
                                            paramName
                                            paramT
                                            actualType
                                            arg)
                                | _ ->
                                    err)
                            |> Result.bind (fun (argType, arg') ->
                                if typesCompatibleWithAliases aliasReg paramT argType then
                                    checkArgsWithTypes restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                                else
                                    Error (
                                        GenericError
                                            (formatLegacyParamTypeError
                                                funcName
                                                paramIndex
                                                paramName
                                                paramT
                                                argType
                                                arg)
                                    ))
                        | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                    checkArgsWithTypes args origParamTypes 1 []
                    |> Result.bind (fun args' ->
                                match expectedType with
                                | Some expected when not (typesCompatibleWithAliases aliasReg expected origReturnType) ->
                                    Error (TypeMismatch (expected, origReturnType, $"result of call to {funcName}"))
                                | _ -> Ok (origReturnType, Call (resolvedFuncName, toCallArgs args')))
                )
            | Some (TVar funcTypeVar, resolvedFuncName) ->
                // In interpreter syntax, higher-order generic parameters may reach call sites
                // before their function shape is concretized (for example in nested List.map).
                // Keep the call typable and let surrounding generic reconciliation specialize it.
                let rec checkArgsWithUnknownCallableType remaining accArgs =
                    match remaining with
                    | [] -> Ok (List.rev accArgs)
                    | arg :: restArgs ->
                        checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                        |> Result.bind (fun (_argType, arg') ->
                            checkArgsWithUnknownCallableType restArgs (arg' :: accArgs))

                checkArgsWithUnknownCallableType args []
                |> Result.map (fun args' ->
                    let inferredReturnType =
                        match expectedType with
                        | Some expected -> expected
                        | None -> TVar $"__call_result_{funcTypeVar}"
                    (inferredReturnType, Call (resolvedFuncName, toCallArgs args')))
            | Some (other, _) ->
                Error (GenericError $"{funcName} is not a function (has type {typeToString other})")
            | None ->
                // Check if it's a module function (e.g., Stdlib.Int64.add, __raw_get)
                match Stdlib.tryGetFunction moduleRegistry funcName with
                | Some (moduleFunc, resolvedFuncName) ->
                    (
                // Freshen type params to avoid name clashes with caller's scope
                let (freshTypeParams, renaming) = freshenTypeParams moduleFunc.TypeParams
                let paramTypes = moduleFunc.ParamTypes |> List.map (applyTypeVarRenaming renaming)
                let returnType = applyTypeVarRenaming renaming moduleFunc.ReturnType
                let typeParams = freshTypeParams
                let numParams = List.length moduleFunc.ParamTypes
                let args = normalizeNullaryCallArgs numParams args
                let numArgs = List.length args
                // Check argument count - allow partial application
                if numArgs > numParams then
                    Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
                else if numArgs < numParams && List.isEmpty typeParams then
                    // Partial application of non-generic module function
                    let providedParamTypes = List.take numArgs paramTypes
                    let remainingParamTypes = List.skip numArgs paramTypes

                    // Type-check the provided arguments
                    let rec checkProvidedArgs remaining paramTys accArgs =
                        match remaining, paramTys with
                        | [], [] -> Ok (List.rev accArgs)
                        | arg :: restArgs, paramT :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                            |> Result.bind (fun (argType, arg') ->
                                if typesEqual aliasReg argType paramT then
                                    checkProvidedArgs restArgs restParams (arg' :: accArgs)
                                else
                                    Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                        | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                    checkProvidedArgs args providedParamTypes []
                    |> Result.bind (fun args' ->
                        // Create unique parameter names for the remaining parameters
                        let remainingParams = makePartialParams resolvedFuncName remainingParamTypes

                        // Create the lambda body: call the original function with all args (using resolved name)
                        let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                        let lambdaBody = Call (resolvedFuncName, toCallArgs allArgs)

                        // Create the lambda: fun p0 p1 ... -> funcName(providedArgs, p0, p1, ...)
                        let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                        // The resulting type is a function from remaining params to return type
                        let partialType = TFunction (remainingParamTypes, returnType)

                        match expectedType with
                        | Some expected when not (typesEqual aliasReg expected partialType) ->
                            Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                        | _ -> Ok (partialType, lambdaExpr))
                else if numArgs < numParams then
                    // Partial application of generic module function
                    let providedParamTypes = List.take numArgs paramTypes
                    let remainingParamTypes = List.skip numArgs paramTypes

                    // Type-check provided arguments and collect bindings for type inference
                    let rec checkArgsAndInfer remaining paramTys accArgs accBindings =
                        match remaining, paramTys with
                        | [], [] -> Ok (List.rev accArgs, accBindings)
                        | arg :: restArgs, paramT :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                            |> Result.bind (fun (argType, arg') ->
                                // Match param type against arg type to get type variable bindings
                                match matchTypes paramT argType with
                                | Ok bindings ->
                                    checkArgsAndInfer restArgs restParams (arg' :: accArgs) (accBindings @ bindings)
                                | Error msg ->
                                    Error (TypeMismatch (paramT, argType, $"argument to {funcName}: {msg}")))
                        | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                    checkArgsAndInfer args providedParamTypes [] []
                    |> Result.bind (fun (args', bindings) ->
                        // Consolidate bindings and build substitution
                        consolidateBindings bindings
                        |> Result.mapError GenericError
                        |> Result.bind (fun bindingMap ->
                            // Build type arguments list from inferred bindings
                            // For partial application, some type params may not be inferrable yet
                            let inferredTypeArgs =
                                typeParams
                                |> List.map (fun paramName ->
                                    match Map.tryFind paramName bindingMap with
                                    | Some typ -> typ
                                    | None -> TVar paramName)  // Keep as type variable if not inferred

                            // Build full substitution (inferred types only, not type vars)
                            let subst = bindingMap

                            // Apply substitution to remaining param types and return type
                            let concreteRemainingTypes = remainingParamTypes |> List.map (applySubst subst)
                            let concreteReturnType = applySubst subst returnType

                            // Create unique parameter names for the remaining parameters
                            let remainingParams = makePartialParams resolvedFuncName concreteRemainingTypes

                            // Create the lambda body: TypeApp call with all args (using resolved name)
                            let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                            let lambdaBody = TypeApp (resolvedFuncName, inferredTypeArgs, toCallArgs allArgs)

                            // Create the lambda
                            let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                            // The resulting type is a function from remaining params to return type
                            let partialType = TFunction (concreteRemainingTypes, concreteReturnType)

                            match expectedType with
                            | Some expected when not (typesCompatible expected partialType) ->
                                Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                            | _ -> Ok (partialType, lambdaExpr)))
                else if not (List.isEmpty typeParams) then
                    // Generic module function: infer type arguments from actual argument types
                    // Type-check arguments left-to-right while propagating inferred bindings.
                    // This lets later args see concrete expectations inferred from earlier args.
                    let rec checkArgsWithBindings remaining remainingParamTypes accTypes accExprs accBindings =
                        match remaining, remainingParamTypes with
                        | [], [] ->
                            Ok (List.rev accTypes, List.rev accExprs)
                        | arg :: restArgs, paramT :: restParams ->
                            consolidateBindings accBindings
                            |> Result.mapError GenericError
                            |> Result.bind (fun bindingMap ->
                                let concreteParamType = applySubst bindingMap paramT
                                checkExpr
                                    arg
                                    env
                                    typeReg
                                    variantLookup
                                    genericFuncReg
                                    warningSettings
                                    moduleRegistry
                                    aliasReg
                                    (Some concreteParamType)
                                |> Result.bind (fun (argType, arg') ->
                                    match matchTypes concreteParamType argType with
                                    | Ok newBindings ->
                                        let combinedBindings = accBindings @ newBindings
                                        consolidateBindings combinedBindings
                                        |> Result.mapError GenericError
                                        |> Result.bind (fun combinedBindingMap ->
                                            let concreteArgType = applySubst combinedBindingMap argType
                                            checkArgsWithBindings
                                                restArgs
                                                restParams
                                                (concreteArgType :: accTypes)
                                                (arg' :: accExprs)
                                                combinedBindings)
                                    | Error msg ->
                                        Error (TypeMismatch (concreteParamType, argType, $"argument to {funcName}: {msg}")))
                            )
                        | _ ->
                            Error (GenericError "Argument count mismatch")

                    checkArgsWithBindings args paramTypes [] [] []
                    |> Result.bind (fun (argTypes, args') ->
                        // Infer type arguments from parameter types, argument types, and expected return type
                        inferTypeArgs typeParams paramTypes argTypes (Some returnType) expectedType
                        |> Result.mapError GenericError
                        |> Result.bind (fun inferredTypeArgs ->
                            // Build substitution and compute concrete types
                            buildSubstitution typeParams inferredTypeArgs
                            |> Result.mapError GenericError
                            |> Result.bind (fun subst ->
                                let concreteReturnType = applySubst subst returnType
                                match expectedType with
                                | Some expected when not (typesCompatible expected concreteReturnType) ->
                                    Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                                | _ ->
                                    // Transform Call to TypeApp with inferred type arguments (using resolved name)
                                    Ok (
                                        concreteReturnType,
                                        TypeApp (resolvedFuncName, inferredTypeArgs, toCallArgs args')
                                    ))))
                else
                    // Non-generic module function: regular call
                    // Check each argument type and collect transformed args
                    let rec checkArgsWithTypes remaining paramTys accArgs =
                        match remaining, paramTys with
                        | [], [] -> Ok (List.rev accArgs)
                        | arg :: restArgs, paramT :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                            |> Result.bind (fun (argType, arg') ->
                                if typesCompatibleWithAliases aliasReg paramT argType then
                                    checkArgsWithTypes restArgs restParams (arg' :: accArgs)
                                else
                                    Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                        | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                    checkArgsWithTypes args paramTypes []
                    |> Result.bind (fun args' ->
                        match expectedType with
                        | Some expected when not (typesCompatibleWithAliases aliasReg expected returnType) ->
                            Error (TypeMismatch (expected, returnType, $"result of call to {funcName}"))
                        | _ -> Ok (returnType, Call (resolvedFuncName, toCallArgs args')))
                    )
                | None ->
                    Error (UndefinedCallTarget funcName)

    | TypeApp (funcName, [targetType], { Head = leftExpr; Tail = [rightExpr] })
        when funcName = internalTypeAppMarkerName EqHelperDispatch ->
        // Specialized programs can be checked again by the E2E preamble
        // planner. Keep this compiler-internal plan well typed without
        // exposing its marker as a source-level function.
        checkExpr leftExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some targetType)
        |> Result.bind (fun (_, leftExpr') ->
            checkExpr rightExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some targetType)
            |> Result.bind (fun (_, rightExpr') ->
                match expectedType with
                | Some expected when not (typesCompatible expected TBool) ->
                    Error (TypeMismatch (expected, TBool, "comparison result"))
                | _ ->
                    Ok (
                        TBool,
                        makeInternalTypeApp
                            (EqHelperDispatchTypeApp (targetType, leftExpr', rightExpr'))
                    )))

    | TypeApp (funcName, typeArgs, args) ->
        // Generic function call with explicit type arguments: func<Type1, Type2>(args)
        // 1. Look up the canonical function identity
        let args = NonEmptyList.toList args
        match tryLookupResolved funcName env with
        | Some (TFunction (paramTypes, returnType), resolvedFuncName) ->
            // 2. Look up type parameters
            match tryLookupResolved resolvedFuncName genericFuncReg.Functions with
            | Some (typeParams, _) ->
                let expectedTypeArgCount = List.length typeParams
                let actualTypeArgCount = List.length typeArgs
                if expectedTypeArgCount <> actualTypeArgCount then
                    Error (
                        GenericError (
                            formatTypeArgumentArityError funcName expectedTypeArgCount actualTypeArgCount
                        )
                    )
                else
                // 3. Build substitution from type params to type args
                    let typeArgs =
                        typeArgs
                        |> List.map (canonicalizeBareSumTypeRefs variantLookup)

                    buildSubstitution typeParams typeArgs
                    |> Result.mapError GenericError
                    |> Result.bind (fun subst ->
                        // 4. Apply substitution to get concrete types
                        let concreteParamTypes =
                            paramTypes
                            |> List.map (applySubst subst)
                            |> List.map (canonicalizeBareSumTypeRefs variantLookup)
                        let concreteReturnType =
                            returnType
                            |> applySubst subst
                            |> canonicalizeBareSumTypeRefs variantLookup

                        // 5. Check argument count - allow partial application
                        let numParams = List.length concreteParamTypes
                        let args = normalizeNullaryCallArgs numParams args
                        let numArgs = List.length args
                        if numArgs > numParams then
                            Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
                        else if numArgs < numParams then
                            // Partial application with explicit type args
                            let providedParamTypes = List.take numArgs concreteParamTypes
                            let remainingParamTypes = List.skip numArgs concreteParamTypes

                            // Type-check the provided arguments
                            let rec checkProvidedArgs remaining paramTys paramIndex accArgs =
                                match remaining, paramTys with
                                | [], [] -> Ok (List.rev accArgs)
                                | arg :: restArgs, paramT :: restParams ->
                                    let paramName =
                                        paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                                    checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                                    |> Result.mapError (fun err ->
                                        match err with
                                        | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                            GenericError
                                                (formatLegacyParamTypeError
                                                    funcName
                                                    paramIndex
                                                    paramName
                                                    paramT
                                                    actualType
                                                    arg)
                                        | _ ->
                                            err)
                                    |> Result.bind (fun (argType, arg') ->
                                        // Use typesCompatible to allow type variables to unify with concrete types
                                        if typesCompatible paramT argType then
                                            checkProvidedArgs restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                                        else
                                            Error (
                                                GenericError
                                                    (formatLegacyParamTypeError
                                                        funcName
                                                        paramIndex
                                                        paramName
                                                        paramT
                                                        argType
                                                        arg)
                                            ))
                                | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                            checkProvidedArgs args providedParamTypes 1 []
                            |> Result.bind (fun args' ->
                                // Create unique parameter names for the remaining parameters
                                let remainingParams = makePartialParams resolvedFuncName remainingParamTypes

                                // Create the lambda body: TypeApp call with all args (using resolved name)
                                let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                                let lambdaBody = TypeApp (resolvedFuncName, typeArgs, toCallArgs allArgs)

                                // Create the lambda
                                let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                                // The resulting type is a function from remaining params to return type
                                let partialType = TFunction (remainingParamTypes, concreteReturnType)

                                match expectedType with
                                | Some expected when not (typesCompatible expected partialType) ->
                                    Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                                | _ -> Ok (partialType, lambdaExpr))
                        else
                            // 6. Type check each argument and collect transformed args
                            let rec checkArgsWithTypes remaining paramTys paramIndex accArgs =
                                match remaining, paramTys with
                                | [], [] -> Ok (List.rev accArgs)
                                | arg :: restArgs, paramT :: restParams ->
                                    let paramName =
                                        paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                                    checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                                    |> Result.mapError (fun err ->
                                        match err with
                                        | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                            GenericError
                                                (formatLegacyParamTypeError
                                                    funcName
                                                    paramIndex
                                                    paramName
                                                    paramT
                                                    actualType
                                                    arg)
                                        | _ ->
                                            err)
                                    |> Result.bind (fun (argType, arg') ->
                                        // Use typesCompatible to allow type variables to unify with concrete types
                                        if typesCompatible paramT argType then
                                            checkArgsWithTypes restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                                        else
                                            Error (
                                                GenericError
                                                    (formatLegacyParamTypeError
                                                        funcName
                                                        paramIndex
                                                        paramName
                                                        paramT
                                                        argType
                                                        arg)
                                            ))
                                | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                            checkArgsWithTypes args concreteParamTypes 1 []
                            |> Result.bind (fun args' ->
                                // 7. Return the concrete return type (using resolved name)
                                // Use typesCompatible to allow type variables to unify with concrete types
                                match expectedType with
                                | Some expected when not (typesCompatible expected concreteReturnType) ->
                                    Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                                | _ ->
                                    Ok (
                                        concreteReturnType,
                                        TypeApp (resolvedFuncName, typeArgs, toCallArgs args')
                                    )))
            | None ->
                Error (GenericError $"Function {funcName} is not generic, use regular call syntax")
        | Some (other, _) ->
            Error (GenericError $"{funcName} is not a function (has type {typeToString other})")
        | None ->
            // Check if it's a generic module function (e.g., __raw_get<v>)
            match Stdlib.tryGetFunction moduleRegistry funcName with
            | Some (moduleFunc, resolvedFuncName) when not (List.isEmpty moduleFunc.TypeParams) ->
                let typeParams = moduleFunc.TypeParams
                let paramTypes = moduleFunc.ParamTypes
                let returnType = moduleFunc.ReturnType
                let expectedTypeArgCount = List.length typeParams
                let actualTypeArgCount = List.length typeArgs
                if expectedTypeArgCount <> actualTypeArgCount then
                    Error (
                        GenericError (
                            formatTypeArgumentArityError funcName expectedTypeArgCount actualTypeArgCount
                        )
                    )
                else
                // Build substitution from type params to type args
                    let typeArgs =
                        typeArgs
                        |> List.map (canonicalizeBareSumTypeRefs variantLookup)

                    buildSubstitution typeParams typeArgs
                    |> Result.mapError GenericError
                    |> Result.bind (fun subst ->
                        // Apply substitution to get concrete types
                        let concreteParamTypes =
                            paramTypes
                            |> List.map (applySubst subst)
                            |> List.map (canonicalizeBareSumTypeRefs variantLookup)
                        let concreteReturnType =
                            returnType
                            |> applySubst subst
                            |> canonicalizeBareSumTypeRefs variantLookup

                        // Check argument count - allow partial application
                        let numParams = List.length concreteParamTypes
                        let args = normalizeNullaryCallArgs numParams args
                        let numArgs = List.length args
                        if numArgs > numParams then
                            Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
                        else if numArgs < numParams then
                            // Partial application with explicit type args
                            let providedParamTypes = List.take numArgs concreteParamTypes
                            let remainingParamTypes = List.skip numArgs concreteParamTypes

                            // Type-check the provided arguments
                            let rec checkProvidedArgs remaining paramTys paramIndex accArgs =
                                match remaining, paramTys with
                                | [], [] -> Ok (List.rev accArgs)
                                | arg :: restArgs, paramT :: restParams ->
                                    let paramName =
                                        paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                                    checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                                    |> Result.mapError (fun err ->
                                        match err with
                                        | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                            GenericError
                                                (formatLegacyParamTypeError
                                                    funcName
                                                    paramIndex
                                                    paramName
                                                    paramT
                                                    actualType
                                                    arg)
                                        | _ ->
                                            err)
                                    |> Result.bind (fun (argType, arg') ->
                                        // Use typesCompatible to allow type variables to unify with concrete types
                                        if typesCompatible paramT argType then
                                            checkProvidedArgs restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                                        else
                                            Error (
                                                GenericError
                                                    (formatLegacyParamTypeError
                                                        funcName
                                                        paramIndex
                                                        paramName
                                                        paramT
                                                        argType
                                                        arg)
                                            ))
                                | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                            checkProvidedArgs args providedParamTypes 1 []
                            |> Result.bind (fun args' ->
                                // Create unique parameter names for the remaining parameters
                                let remainingParams = makePartialParams resolvedFuncName remainingParamTypes

                                // Create the lambda body: TypeApp call with all args (using resolved name)
                                let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                                let lambdaBody = TypeApp (resolvedFuncName, typeArgs, toCallArgs allArgs)

                                // Create the lambda
                                let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                                // The resulting type is a function from remaining params to return type
                                let partialType = TFunction (remainingParamTypes, concreteReturnType)

                                match expectedType with
                                | Some expected when not (typesCompatible expected partialType) ->
                                    Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                                | _ -> Ok (partialType, lambdaExpr))
                        else
                            // Type check each argument and collect transformed args
                            let rec checkArgsWithTypes remaining paramTys paramIndex accArgs =
                                match remaining, paramTys with
                                | [], [] -> Ok (List.rev accArgs)
                                | arg :: restArgs, paramT :: restParams ->
                                    let paramName =
                                        paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                                    checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                                    |> Result.mapError (fun err ->
                                        match err with
                                        | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                            GenericError
                                                (formatLegacyParamTypeError
                                                    funcName
                                                    paramIndex
                                                    paramName
                                                    paramT
                                                    actualType
                                                    arg)
                                        | _ ->
                                            err)
                                    |> Result.bind (fun (argType, arg') ->
                                        // Use typesCompatible to allow type variables to unify with concrete types
                                        if typesCompatible paramT argType then
                                            checkArgsWithTypes restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                                        else
                                            Error (
                                                GenericError
                                                    (formatLegacyParamTypeError
                                                        funcName
                                                        paramIndex
                                                        paramName
                                                        paramT
                                                        argType
                                                        arg)
                                            ))
                                | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                            checkArgsWithTypes args concreteParamTypes 1 []
                            |> Result.bind (fun args' ->
                                // Use typesCompatible to allow type variables to unify with concrete types
                                match expectedType with
                                | Some expected when not (typesCompatible expected concreteReturnType) ->
                                    Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                                | _ ->
                                    Ok (
                                        concreteReturnType,
                                        TypeApp (resolvedFuncName, typeArgs, toCallArgs args')
                                    )))
            | Some (_, _) ->
                Error (GenericError $"Function {funcName} is not generic, use regular call syntax")
            | None ->
                Error (UndefinedCallTarget funcName)

    | TupleLiteral elements ->
        // Type-check each element and build tuple type
        let expectedElemTypes =
            match expectedType with
            | Some expected ->
                match resolveType aliasReg expected with
                | TTuple elemTypes when List.length elemTypes = List.length elements ->
                    elemTypes |> List.map Some
                | _ -> List.replicate (List.length elements) None
            | None -> List.replicate (List.length elements) None

        let rec checkElements elems expectedElems accTypes accExprs =
            match elems, expectedElems with
            | [], [] -> Ok (List.rev accTypes, List.rev accExprs)
            | e :: rest, expectedElem :: expectedRest ->
                checkExpr e env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedElem
                |> Result.bind (fun (elemType, e') ->
                    checkElements rest expectedRest (elemType :: accTypes) (e' :: accExprs))
            | _ -> Ok (List.rev accTypes, List.rev accExprs)
        checkElements elements expectedElemTypes [] []
        |> Result.bind (fun (elemTypes, elements') ->
            // Tuple elements that are known runtime failures should make the whole
            // tuple expression runtime-fail (bottom-like behavior), preserving the
            // left-to-right first failure.
            let firstRuntimeErrorElem =
                elements'
                |> List.tryFind (isKnownTestRuntimeErrorExpr Map.empty)

            match firstRuntimeErrorElem with
            | Some runtimeErrExpr ->
                let outputType =
                    match expectedType with
                    | Some expected -> expected
                    | None -> TRuntimeError

                let runtimeErrCall =
                    match runtimeErrExpr with
                    | Call (funcName, { Head = argExpr; Tail = [] }) when isBuiltinTestRuntimeErrorName funcName ->
                        Call ("Builtin.testRuntimeError", NonEmptyList.singleton argExpr)
                    | _ ->
                        match tryExtractKnownTestRuntimeErrorMessage Map.empty runtimeErrExpr with
                        | Some msg -> Call ("Builtin.testRuntimeError", NonEmptyList.singleton (StringLiteral msg))
                        | None ->
                            Call (
                                "Builtin.testRuntimeError",
                                NonEmptyList.singleton (StringLiteral "<runtime error>")
                            )

                Ok (outputType, runtimeErrCall)
            | None ->
                let tupleType = TTuple elemTypes
                match expectedType with
                | Some expected ->
                    // Resolve type aliases first, then check compatibility for type variables
                    // This allows Pair<Int64> to match (Int64, Int64) when Pair<a> = (a, a)
                    // and (a, b) to match (Int64, Int64) when using generic functions
                    let resolvedExpected = resolveType aliasReg expected
                    if typesCompatible resolvedExpected tupleType then
                        Ok (tupleType, TupleLiteral elements')
                    else
                        Error (TypeMismatch (expected, tupleType, "tuple literal"))
                | None -> Ok (tupleType, TupleLiteral elements'))

    | TupleAccess (tupleExpr, index) ->
        // Check the tuple expression
        checkExpr tupleExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (tupleType, tupleExpr') ->
            match tupleType with
            | TTuple elemTypes ->
                if index < 0 || index >= List.length elemTypes then
                    Error (GenericError $"Tuple index {index} out of bounds (tuple has {List.length elemTypes} elements)")
                else
                    let elemType = List.item index elemTypes
                    match expectedType with
                    | Some expected when expected <> elemType ->
                        Error (TypeMismatch (expected, elemType, $"tuple access .{index}"))
                    | _ -> Ok (elemType, TupleAccess (tupleExpr', index))
            | other ->
                Error (GenericError $"Cannot access .{index} on non-tuple type {typeToString other}"))

    | RecordLiteral (typeName, fields) ->
        // Type name is required (parser enforces this, but check for safety)
        if typeName = "" then
            Error (GenericError "Record literal requires type name: use 'TypeName { field = value, ... }'")
        else
            match tryResolveRecordLiteralFields aliasReg typeReg typeName with
            | None ->
                Error (GenericError $"Unknown record type: {typeName}")
            | Some (resolvedTypeName, expectedFields) ->
                let expectedFields =
                    expectedFields
                    |> List.map (fun (fieldName, fieldType) ->
                        (fieldName, canonicalizeBareSumTypeRefs variantLookup fieldType))

                // Check that all fields are present and have correct types
                let fieldMap = Map.ofList fields

                // Check for missing fields
                let missingFields =
                    expectedFields
                    |> List.filter (fun (fname, _) -> not (Map.containsKey fname fieldMap))
                    |> List.map fst

                if not (List.isEmpty missingFields) then
                    let missingStr = String.concat ", " missingFields
                    Error (GenericError $"Missing fields in record literal: {missingStr}")
                else
                    // Check for extra fields
                    let expectedFieldNames = expectedFields |> List.map fst |> Set.ofList
                    let extraFields =
                        fields
                        |> List.filter (fun (fname, _) -> not (Set.contains fname expectedFieldNames))
                        |> List.map fst

                    if not (List.isEmpty extraFields) then
                        let extraStr = String.concat ", " extraFields
                        Error (GenericError $"Unknown fields in record literal: {extraStr}")
                    else
                        // Type check each field in source order. Record layout order is
                        // applied during lowering, after every initializer has run.
                        let expectedFieldTypes = Map.ofList expectedFields

                        let rec checkFieldsInOrder
                            (remaining: (string * Expr) list)
                            (accFields: (string * Expr) list)
                            (accBindings: (string * Type) list)
                            : Result<(string * Expr) list * (string * Type) list, TypeError> =
                            match remaining with
                            | [] -> Ok (List.rev accFields, accBindings)
                            | (fname, fieldExpr) :: rest ->
                                match Map.tryFind fname expectedFieldTypes with
                                | Some expectedFieldType ->
                                    match
                                        checkExpr
                                            fieldExpr
                                            env
                                            typeReg
                                            variantLookup
                                            genericFuncReg
                                            warningSettings
                                            moduleRegistry
                                            aliasReg
                                            (Some expectedFieldType)
                                    with
                                    | Error (TypeMismatch (_, actualType, _)) ->
                                        Error
                                            (GenericError
                                                (formatLegacyRecordFieldTypeError
                                                    aliasReg
                                                    fname
                                                    expectedFieldType
                                                    actualType
                                                    fieldExpr))
                                    | Error err ->
                                        Error err
                                    | Ok (actualType, fieldExpr') ->
                                        let resolvedExpectedFieldType = resolveType aliasReg expectedFieldType
                                        let resolvedActualType = resolveType aliasReg actualType
                                        match matchTypes resolvedExpectedFieldType resolvedActualType with
                                        | Ok newBindings ->
                                            checkFieldsInOrder
                                                rest
                                                ((fname, fieldExpr') :: accFields)
                                                (accBindings @ newBindings)
                                        | Error _ ->
                                            Error
                                                (GenericError
                                                    (formatLegacyRecordFieldTypeError
                                                        aliasReg
                                                        fname
                                                        expectedFieldType
                                                        actualType
                                                        fieldExpr))
                                | None ->
                                    Crash.crash $"Record field '{fname}' disappeared after validation"

                        checkFieldsInOrder fields [] []
                        |> Result.bind (fun (fields', rawBindings) ->
                            match consolidateBindings rawBindings with
                            | Error msg ->
                                Error (GenericError $"Incompatible generic record field types: {msg}")
                            | Ok subst ->
                                let inferredTypeParams = inferRecordTypeParamsFromFields expectedFields
                                let inferredTypeArgs =
                                    inferredTypeParams
                                    |> List.map (fun name -> Map.tryFind name subst |> Option.defaultValue (TVar name))
                                let inferredRecordType = TRecord (resolvedTypeName, inferredTypeArgs)

                                match expectedType with
                                | Some expected ->
                                    if typesCompatibleWithAliases aliasReg expected inferredRecordType then
                                        Ok (inferredRecordType, RecordLiteral (resolvedTypeName, fields'))
                                    else
                                        Error (TypeMismatch (expected, inferredRecordType, "record literal"))
                                | None ->
                                    Ok (inferredRecordType, RecordLiteral (resolvedTypeName, fields')))

    | RecordUpdate (recordExpr, updates) ->
        // Check the record expression to get its type
        checkExpr recordExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (recordType, recordExpr') ->
            match recordType with
            | TRecord (typeName, typeArgs) ->
                // Resolve type alias before looking up in typeReg
                let resolvedTypeName = resolveTypeName aliasReg typeName
                match Map.tryFind resolvedTypeName typeReg with
                | None ->
                    Error (GenericError $"Unknown record type: {typeName}")
                | Some recordInfo ->
                    let expectedFields = recordInfo.Fields

                    let subst =
                        match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                        | Ok s -> s
                        | Error _ -> Map.empty

                    // Check for unknown fields in update
                    let unknownFields =
                        updates
                        |> List.filter (fun (fname, _) -> not (Map.containsKey fname recordInfo.FieldTypes))
                        |> List.map fst

                    if not (List.isEmpty unknownFields) then
                        let unknownStr = String.concat ", " unknownFields
                        Error (GenericError $"Unknown fields in record update: {unknownStr}")
                    else
                        // Type check each update field
                        let rec checkUpdates remaining accUpdates =
                            match remaining with
                            | [] -> Ok (List.rev accUpdates)
                            | (fname, updateExpr) :: rest ->
                                match Map.tryFind fname recordInfo.FieldTypes with
                                | Some fieldTypePattern ->
                                    let expectedFieldType = applySubst subst fieldTypePattern
                                    checkExpr updateExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some expectedFieldType)
                                    |> Result.bind (fun (actualType, updateExpr') ->
                                        if typesCompatibleWithAliases aliasReg expectedFieldType actualType then
                                            checkUpdates rest ((fname, updateExpr') :: accUpdates)
                                        else
                                            Error (TypeMismatch (expectedFieldType, actualType, $"field {fname} in record update")))
                                | None ->
                                    Error (GenericError $"Field {fname} not found in record type {typeName}")

                        checkUpdates updates []
                        |> Result.map (fun updates' -> (TRecord (resolvedTypeName, typeArgs), RecordUpdate (recordExpr', updates')))
            | other ->
                Error (GenericError $"Cannot use record update syntax on non-record type {typeToString other}"))

    | RecordAccess (recordExpr, fieldName) ->
        // Check the record expression
        checkExpr recordExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (recordType, recordExpr') ->
            match recordType with
            | TRecord (typeName, typeArgs) ->
                // Resolve type alias before looking up in typeReg
                let resolvedTypeName = resolveTypeName aliasReg typeName
                match Map.tryFind resolvedTypeName typeReg with
                | None ->
                    Error (GenericError $"Unknown record type: {typeName}")
                | Some recordInfo ->
                    match Map.tryFind fieldName recordInfo.FieldTypes with
                    | None ->
                        Error (GenericError $"Record type {typeName} has no field '{fieldName}'")
                    | Some fieldTypePattern ->
                        match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                        | Error msg ->
                            Error (GenericError msg)
                        | Ok subst ->
                            let fieldType = applySubst subst fieldTypePattern
                            match expectedType with
                            | Some expected when not (typesCompatibleWithAliases aliasReg expected fieldType) ->
                                Error (TypeMismatch (expected, fieldType, $"field access .{fieldName}"))
                            | _ -> Ok (fieldType, RecordAccess (recordExpr', fieldName))
            | other ->
                Error (GenericError $"Cannot access .{fieldName} on non-record type {typeToString other}"))

    | Constructor (constructorReference, variantName, payload) ->
        // Look up the variant to find its type and expected payload
        if genericFuncReg.RequireExplicitTypeArgsForBareCalls
           && constructorReference = UnresolvedConstructor None
           && unqualifiedVariantOwnerCount variantName variantLookup > 1 then
            Error (GenericError $"Ambiguous constructor: {variantName}")
        else
        let resolvedVariant =
            match tryFindVariant constructorReference variantName variantLookup with
            | Some found -> Some found
            | None when not genericFuncReg.RequireExplicitTypeArgsForBareCalls ->
                Map.tryFind variantName variantLookup
            | None -> None
        match resolvedVariant with
        | None ->
            Error (GenericError $"Unknown constructor: {variantName}")
        | Some (typeName, typeParams, _tag, expectedPayload) ->
            let resolvedReference = resolvedConstructorReference typeName
            let resolvedExpr payload = Constructor (resolvedReference, variantName, payload)
            let payloadArityError =
                match expectedPayload, payload with
                | Some (TEnumFields expectedFields), Some (TupleLiteral actualFields)
                    when List.length expectedFields <> List.length actualFields ->
                    Some (
                        GenericError
                            $"Expected {List.length expectedFields} fields in {typeName}.`{variantName}`, but got {List.length actualFields}"
                    )
                | Some (TEnumFields expectedFields), Some actualPayload
                    when (match actualPayload with | TupleLiteral _ -> false | _ -> true)
                         && List.length expectedFields <> 1 ->
                    Some (
                        GenericError
                            $"Expected {List.length expectedFields} fields in {typeName}.`{variantName}`, but got 1"
                    )
                | _ -> None

            let normalizedExpectedPayload =
                expectedPayload
                |> Option.map (function
                    | TEnumFields fieldTypes -> TTuple fieldTypes
                    | payloadType -> payloadType)

            (match payloadArityError with
             | Some error -> Error error
             | None -> Ok (normalizedExpectedPayload, payload))
            |> Result.bind (fun (expectedPayload, payload) ->
            match expectedPayload, payload with
            | None, None ->
                // Variant without payload, no payload provided - OK
                if List.isEmpty typeParams then
                    // Non-generic type - simple case
                    let sumType = TSum (typeName, [])
                    match expectedType with
                    | Some expected when expected <> sumType ->
                        Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                    | _ -> Ok (sumType, resolvedExpr None)
                else
                    // Generic type with nullary constructor (e.g., None in Option<t>)
                    // Try to get type arguments from expectedType
                    match expectedType with
                    | Some (TSum (expectedName, args)) when expectedName = typeName && List.length args = List.length typeParams ->
                        // Use type args from expected type
                        let sumType = TSum (typeName, args)
                        Ok (sumType, resolvedExpr None)
                    | Some expected ->
                        // Expected type doesn't match - error
                        let sumTypeWithVars = TSum (typeName, typeParams |> List.map TVar)
                        Error (TypeMismatch (expected, sumTypeWithVars, $"constructor {variantName}"))
                    | None ->
                        // No expected type - return type with unresolved type variables
                        // This allows type inference to resolve them later from context
                        let sumType = TSum (typeName, typeParams |> List.map TVar)
                        Ok (sumType, resolvedExpr None)
            | None, Some _ ->
                // Variant doesn't take payload but one was provided
                Error (GenericError $"Constructor {variantName} does not take a payload")
            | Some _, None ->
                // Variant requires payload but none provided
                Error (GenericError $"Constructor {variantName} requires a payload")
            | Some payloadType, Some payloadExpr ->
                // Variant with payload - check payload type
                // For generic types, infer type variables from the payload
                let payloadType = canonicalizeBareSumTypeRefs variantLookup payloadType

                if List.isEmpty typeParams then
                    // Non-generic type - check payload has exact type
                    checkExpr payloadExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some payloadType)
                    |> Result.bind (fun (actualPayloadType, payloadExpr') ->
                        let actualPayloadType = canonicalizeBareSumTypeRefs variantLookup actualPayloadType

                        // Use typesCompatible to allow type variables to match concrete types
                        if not (typesCompatible payloadType actualPayloadType) then
                            Error (TypeMismatch (payloadType, actualPayloadType, $"payload of {variantName}"))
                        else
                            let sumType = TSum (typeName, [])
                            match expectedType with
                            | Some expected ->
                                // Use reconcileTypes to allow type variables to unify with concrete types
                                match reconcileTypes (Some aliasReg) expected sumType with
                                | None -> Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                                | Some reconciledType -> Ok (reconciledType, resolvedExpr (Some payloadExpr'))
                            | None -> Ok (sumType, resolvedExpr (Some payloadExpr')))
                else
                    // Generic type - infer type variables from payload
                    // First, check the payload expression without expected type
                    checkExpr payloadExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                    |> Result.bind (fun (actualPayloadType, payloadExpr') ->
                        let actualPayloadType = canonicalizeBareSumTypeRefs variantLookup actualPayloadType

                        // Try to unify payloadType (may contain TVar) with actualPayloadType
                        match unifyTypes payloadType actualPayloadType with
                        | Error msg ->
                            Error (GenericError $"Type mismatch in {variantName} payload: {msg}")
                        | Ok subst ->
                            // Apply substitution to verify all type vars are resolved
                            let concretePayloadType =
                                payloadType
                                |> applySubst subst
                                |> canonicalizeBareSumTypeRefs variantLookup

                            // Use typesCompatible to allow type variables to match concrete types
                            if not (typesCompatible concretePayloadType actualPayloadType) then
                                Error (TypeMismatch (concretePayloadType, actualPayloadType, $"payload of {variantName}"))
                            else
                                // Build concrete type arguments from substitution
                                // For unresolved type vars, try to get them from expectedType
                                let expectedArgs =
                                    match expectedType with
                                    | Some (TSum (expectedName, args)) when expectedName = typeName && List.length args = List.length typeParams ->
                                        Some args
                                    | _ -> None
                                let typeArgs = typeParams |> List.mapi (fun i p ->
                                    match Map.tryFind p subst with
                                    | Some t -> t
                                    | None ->
                                        // Try to get from expected type args
                                        match expectedArgs with
                                        | Some args -> List.item i args
                                        | None -> TVar p)
                                let sumType = TSum (typeName, typeArgs)
                                match expectedType with
                                | Some expected ->
                                    // Use reconcileTypes to allow type variables to unify with concrete types
                                    match reconcileTypes (Some aliasReg) expected sumType with
                                    | None -> Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                                    | Some reconciledType -> Ok (reconciledType, resolvedExpr (Some payloadExpr'))
                                | None -> Ok (sumType, resolvedExpr (Some payloadExpr'))))

    | Match (scrutinee, cases) ->
        let scrutineeExpectedType =
            match scrutinee with
            | ListLiteral [] -> Some (TList (TVar "t"))
            | _ -> None

        // Type check the scrutinee first
        checkExpr scrutinee env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg scrutineeExpectedType
        |> Result.bind (fun (scrutineeType, scrutinee') ->
            // Extract bindings from a pattern based on scrutinee type
            let rec extractPatternBindings
                (pattern: Pattern)
                (patternType: Type)
                (allowNoMatchForKnownListLengthMismatch: bool)
                : Result<(string * Type) list, TypeError> =
                let ensureLiteralType
                    (expectedType: Type)
                    : Result<(string * Type) list, TypeError> =
                    let expectedPatternTypeTextOverride = None
                    let resolvedPatternType = resolveType aliasReg patternType
                    match resolvedPatternType with
                    | t when isRuntimeErrorType t ->
                        // Runtime error scrutinees are bottom-like: allow typechecking to proceed
                        // so evaluation order preserves the runtime failure at execution time.
                        Ok []
                    | t when t = expectedType ->
                        Ok []
                    | TVar _ ->
                        // Leave unresolved pattern literals flexible until concrete type information arrives.
                        // This is important for patterns like `match [] with | [1L] -> ...`.
                        Ok []
                    | _ ->
                        let message =
                            formatPatternMismatchError
                                scrutinee'
                                resolvedPatternType
                                expectedType
                                expectedPatternTypeTextOverride
                        Error (GenericError message)

                let ensureStringOrCharPatternType () : Result<(string * Type) list, TypeError> =
                    let resolvedPatternType = resolveType aliasReg patternType
                    match resolvedPatternType with
                    | t when isRuntimeErrorType t ->
                        // See ensureLiteralType: preserve runtime-error propagation by not
                        // rejecting pattern type checks on known failing scrutinees.
                        Ok []
                    | TString
                    | TChar
                    | TVar _ ->
                        Ok []
                    | _ ->
                        let message =
                            formatPatternMismatchError scrutinee' resolvedPatternType TString None
                        Error (GenericError message)

                match pattern with
                | PUnit -> ensureLiteralType TUnit
                | PWildcard -> Ok []
                | PInt64 _ -> ensureLiteralType TInt64
                | PInt128Literal _ -> ensureLiteralType TInt128
                | PInt8Literal _ -> ensureLiteralType TInt8
                | PInt16Literal _ -> ensureLiteralType TInt16
                | PInt32Literal _ -> ensureLiteralType TInt32
                | PUInt8Literal _ -> ensureLiteralType TUInt8
                | PUInt16Literal _ -> ensureLiteralType TUInt16
                | PUInt32Literal _ -> ensureLiteralType TUInt32
                | PUInt64Literal _ -> ensureLiteralType TUInt64
                | PUInt128Literal _ -> ensureLiteralType TUInt128
                | PBool _ -> ensureLiteralType TBool
                | PString _ -> ensureStringOrCharPatternType ()
                | PChar _ -> ensureLiteralType TChar
                | PFloat _ -> ensureLiteralType TFloat64
                | PVar name -> Ok [(name, patternType)]
                | PConstructor (variantName, payloadPattern) ->
                    match Map.tryFind variantName variantLookup with
                    | None -> Error (GenericError $"Unknown variant in pattern: {variantName}")
                    | Some (typeName, typeParams, _, payloadType) ->
                        // Get type arguments from scrutinee type to substitute into payload type
                        let typeArgs =
                            match patternType with
                            | TSum (_, args) -> args
                            | _ -> []
                        // Build substitution from type params to type args
                        let subst =
                            if List.length typeParams = List.length typeArgs then
                                List.zip typeParams typeArgs |> Map.ofList
                            else
                                Map.empty
                        match payloadPattern, payloadType with
                        | None, None -> Ok []
                        | None, Some _ ->
                            // Pattern omitted payload for a payload-carrying variant.
                            // Treat as a non-binding pattern; match lowering will make it non-matching.
                            Ok []
                        | Some innerPattern, Some pType ->
                            // Apply substitution to get concrete payload type
                            let concretePayloadType =
                                pType
                                |> applySubst subst
                                |> canonicalizeBareSumTypeRefs variantLookup

                            extractPatternBindings innerPattern concretePayloadType allowNoMatchForKnownListLengthMismatch
                        | Some _, None ->
                            // Pattern supplied payload for a nullary variant.
                            // Treat as a non-binding pattern; match lowering will make it non-matching.
                            Ok []
                | PTuple patterns ->
                    let rec containsVariableBinding (innerPattern: Pattern) : bool =
                        match innerPattern with
                        | PVar _ -> true
                        | PConstructor (_, Some payloadPattern) -> containsVariableBinding payloadPattern
                        | PTuple nestedPatterns
                        | PList nestedPatterns ->
                            nestedPatterns |> List.exists containsVariableBinding
                        | PRecord (_, fieldPatterns) ->
                            fieldPatterns
                            |> List.exists (fun (_, fieldPattern) -> containsVariableBinding fieldPattern)
                        | PListCons (headPatterns, tailPattern) ->
                            List.exists containsVariableBinding headPatterns
                            || containsVariableBinding tailPattern
                        | _ -> false

                    let collectTupleBindingsWithTypes (elementTypes: Type list) : Result<(string * Type) list, TypeError> =
                        List.zip patterns elementTypes
                        |> List.map (fun (p, t) ->
                            extractPatternBindings p t allowNoMatchForKnownListLengthMismatch)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])

                    // Resolve type alias before matching (e.g., Pair<Int64> -> (Int64, Int64))
                    let resolvedPatternType = resolveType aliasReg patternType
                    match resolvedPatternType with
                    | TTuple elementTypes when List.length patterns = List.length elementTypes ->
                        collectTupleBindingsWithTypes elementTypes
                    | TVar tupleTypeVar ->
                        let unresolvedElementTypes =
                            patterns
                            |> List.mapi (fun idx _ -> TVar $"__tuple_elem_{tupleTypeVar}_{idx}")
                        collectTupleBindingsWithTypes unresolvedElementTypes
                    | TTuple _ ->
                        // Tuple arity mismatch in pattern should be treated as a non-match.
                        // Match lowering emits a false condition for this pattern shape.
                        Ok []
                    | _ ->
                        if isRuntimeErrorType resolvedPatternType then
                            // Preserve runtime error propagation for known failing scrutinees.
                            Ok []
                        elif patterns |> List.exists containsVariableBinding then
                            // Keep non-binding behavior for tuple patterns that would otherwise
                            // introduce guard/body variables on an incompatible scrutinee type.
                            Ok []
                        else
                            let valueText =
                                match formatPatternMismatchValue scrutinee' with
                                | Some text -> text
                                | None -> "<unknown>"
                            let message =
                                $"Cannot match {typeToString resolvedPatternType} value {valueText} with a Tuple pattern"
                            Error (GenericError message)
                | PRecord (_, fieldPatterns) ->
                    match patternType with
                    | TRecord (recordName, recordTypeArgs) ->
                        // Resolve type alias before looking up in typeReg
                        let resolvedRecordName = resolveTypeName aliasReg recordName
                        match Map.tryFind resolvedRecordName typeReg with
                        | Some recordInfo ->
                            let subst =
                                match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams recordTypeArgs with
                                | Ok s -> s
                                | Error _ -> Map.empty
                            fieldPatterns
                            |> List.map (fun (fieldName, pat) ->
                                match Map.tryFind fieldName recordInfo.FieldTypes with
                                | Some fieldType ->
                                    let concreteFieldType = applySubst subst fieldType
                                    extractPatternBindings
                                        pat
                                        concreteFieldType
                                        allowNoMatchForKnownListLengthMismatch
                                | None -> Error (GenericError $"Unknown field in pattern: {fieldName}"))
                            |> List.fold (fun acc res ->
                                match acc, res with
                                | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                                | Error e, _ -> Error e
                                | _, Error e -> Error e) (Ok [])
                        | None -> Error (GenericError $"Unknown record type: {recordName}")
                    | _ -> Error (GenericError "Record pattern used on non-record type")
                | PList patterns ->
                    let resolvedPatternType = resolveType aliasReg patternType
                    match resolvedPatternType with
                    | TList elemType ->
                        let hasDefiniteLiteralTypeMismatch =
                            let resolvedElemType = resolveType aliasReg elemType
                            let isKnownMismatch expectedType =
                                match resolvedElemType with
                                | TVar _ -> false
                                | _ -> resolvedElemType <> expectedType
                            let isKnownStringPatternMismatch () =
                                match resolvedElemType with
                                | TVar _
                                | TString
                                | TChar -> false
                                | _ -> true
                            patterns
                            |> List.exists (fun pattern ->
                                match pattern with
                                | PUnit -> isKnownMismatch TUnit
                                | PInt64 _ -> isKnownMismatch TInt64
                                | PInt128Literal _ -> isKnownMismatch TInt128
                                | PInt8Literal _ -> isKnownMismatch TInt8
                                | PInt16Literal _ -> isKnownMismatch TInt16
                                | PInt32Literal _ -> isKnownMismatch TInt32
                                | PUInt8Literal _ -> isKnownMismatch TUInt8
                                | PUInt16Literal _ -> isKnownMismatch TUInt16
                                | PUInt32Literal _ -> isKnownMismatch TUInt32
                                | PUInt64Literal _ -> isKnownMismatch TUInt64
                                | PUInt128Literal _ -> isKnownMismatch TUInt128
                                | PBool _ -> isKnownMismatch TBool
                                | PString _ -> isKnownStringPatternMismatch ()
                                | PChar _ -> isKnownMismatch TChar
                                | PFloat _ -> isKnownMismatch TFloat64
                                | _ -> false)
                        match scrutinee' with
                        | ListLiteral scrutineeElements
                            when allowNoMatchForKnownListLengthMismatch
                                 && hasDefiniteLiteralTypeMismatch
                                 && List.length scrutineeElements <> List.length patterns ->
                            let valueText = formatListLiteralForNoMatch scrutineeElements
                            Error (GenericError $"No match for {valueText}")
                        | _ ->
                            // Each element pattern binds variables of the list's element type
                            patterns
                            |> List.map (fun p ->
                                extractPatternBindings p elemType allowNoMatchForKnownListLengthMismatch)
                            |> List.fold (fun acc res ->
                                match acc, res with
                                | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                                | Error e, _ -> Error e
                                | _, Error e -> Error e) (Ok [])
                    | TVar patternTypeVar ->
                        let unresolvedElemType = TVar $"__list_elem_{patternTypeVar}"
                        patterns
                        |> List.map (fun p ->
                            extractPatternBindings p unresolvedElemType allowNoMatchForKnownListLengthMismatch)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                    | TRuntimeError ->
                        let unresolvedElemType = TVar "__list_elem_runtime_error"
                        patterns
                        |> List.map (fun p ->
                            extractPatternBindings p unresolvedElemType allowNoMatchForKnownListLengthMismatch)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                    | _ ->
                        let valueText =
                            match formatPatternMismatchValue scrutinee' with
                            | Some text -> text
                            | None -> "<unknown>"
                        let message =
                            $"Cannot match {typeToString resolvedPatternType} value {valueText} with a List pattern"
                        Error (GenericError message)
                | PListCons (headPatterns, tailPattern) ->
                    let resolvedPatternType = resolveType aliasReg patternType
                    match resolvedPatternType with
                    | TList elemType ->
                        // Head patterns bind to element type
                        let headBindings =
                            headPatterns
                            |> List.map (fun p ->
                                extractPatternBindings p elemType allowNoMatchForKnownListLengthMismatch)
                            |> List.fold (fun acc res ->
                                match acc, res with
                                | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                                | Error e, _ -> Error e
                                | _, Error e -> Error e) (Ok [])
                        // Tail pattern binds to List<elemType>
                        let tailBindings =
                            extractPatternBindings
                                tailPattern
                                (TList elemType)
                                allowNoMatchForKnownListLengthMismatch
                        match headBindings, tailBindings with
                        | Ok hb, Ok tb -> Ok (hb @ tb)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e
                    | TVar patternTypeVar ->
                        let unresolvedElemType = TVar $"__list_elem_{patternTypeVar}"
                        let headBindings =
                            headPatterns
                            |> List.map (fun p ->
                                extractPatternBindings p unresolvedElemType allowNoMatchForKnownListLengthMismatch)
                            |> List.fold (fun acc res ->
                                match acc, res with
                                | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                                | Error e, _ -> Error e
                                | _, Error e -> Error e) (Ok [])
                        let tailBindings =
                            extractPatternBindings
                                tailPattern
                                (TList unresolvedElemType)
                                allowNoMatchForKnownListLengthMismatch
                        match headBindings, tailBindings with
                        | Ok hb, Ok tb -> Ok (hb @ tb)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e
                    | TRuntimeError ->
                        let unresolvedElemType = TVar "__list_elem_runtime_error"
                        let headBindings =
                            headPatterns
                            |> List.map (fun p ->
                                extractPatternBindings p unresolvedElemType allowNoMatchForKnownListLengthMismatch)
                            |> List.fold (fun acc res ->
                                match acc, res with
                                | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                                | Error e, _ -> Error e
                                | _, Error e -> Error e) (Ok [])
                        let tailBindings =
                            extractPatternBindings
                                tailPattern
                                (TList unresolvedElemType)
                                allowNoMatchForKnownListLengthMismatch
                        match headBindings, tailBindings with
                        | Ok hb, Ok tb -> Ok (hb @ tb)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e
                    | _ ->
                        let valueText =
                            match formatPatternMismatchValue scrutinee' with
                            | Some text -> text
                            | None -> "<unknown>"
                        let message =
                            $"Cannot match {typeToString resolvedPatternType} value {valueText} with a List pattern"
                        Error (GenericError message)

            let rec patternBindingNames (pattern: Pattern) : string list =
                match pattern with
                | PUnit
                | PWildcard
                | PInt64 _
                | PInt128Literal _
                | PInt8Literal _
                | PInt16Literal _
                | PInt32Literal _
                | PUInt8Literal _
                | PUInt16Literal _
                | PUInt32Literal _
                | PUInt64Literal _
                | PUInt128Literal _
                | PBool _
                | PString _
                | PChar _
                | PFloat _ ->
                    []
                | PVar name ->
                    [name]
                | PConstructor (_, payloadOpt) ->
                    match payloadOpt with
                    | Some payloadPattern -> patternBindingNames payloadPattern
                    | None -> []
                | PTuple patterns
                | PList patterns ->
                    patterns |> List.collect patternBindingNames
                | PRecord (_, fieldPatterns) ->
                    fieldPatterns |> List.collect (fun (_, fieldPattern) -> patternBindingNames fieldPattern)
                | PListCons (headPatterns, tailPattern) ->
                    (headPatterns |> List.collect patternBindingNames) @ patternBindingNames tailPattern

            let duplicatePatternBindings (pattern: Pattern) : string list =
                patternBindingNames pattern
                |> List.countBy id
                |> List.choose (fun (name, count) -> if count > 1 then Some name else None)
                |> List.sort

            let formatBindingSet (bindingSet: Set<string>) : string =
                bindingSet
                |> Set.toList
                |> List.sort
                |> String.concat ", "

            let validatePatternGroupBindings (patterns: NonEmptyList<Pattern>) : Result<unit, TypeError> =
                let allPatterns = NonEmptyList.toList patterns

                let rec loop (remaining: Pattern list) (expectedBindings: Set<string> option) : Result<unit, TypeError> =
                    match remaining with
                    | [] -> Ok ()
                    | pattern :: rest ->
                        match validateBinders (MatchBinderPattern pattern) with
                        | Error message -> Error (GenericError message)
                        | Ok _ ->
                            let bindings = collectPatternBindings pattern
                            match expectedBindings with
                            | None -> loop rest (Some bindings)
                            | Some expected when expected = bindings ->
                                loop rest expectedBindings
                            | Some expected ->
                                let expectedText = formatBindingSet expected
                                let actualText = formatBindingSet bindings
                                Error (
                                    GenericError
                                        $"Pattern matches require all branches to provide the same variables - expected [{expectedText}], got [{actualText}]"
                                )

                loop allPatterns None

            let allowNoMatchForKnownListLengthMismatchInThisMatch = List.length cases = 1

            let rec patternAlwaysMatchesType (pattern: Pattern) (patternType: Type) : bool =
                let resolvedPatternType = resolveType aliasReg patternType
                match pattern, resolvedPatternType with
                | PWildcard, _
                | PVar _, _ ->
                    true
                | PUnit, TUnit ->
                    true
                | PTuple patterns, TTuple elementTypes when List.length patterns = List.length elementTypes ->
                    List.zip patterns elementTypes
                    |> List.forall (fun (innerPattern, innerType) ->
                        patternAlwaysMatchesType innerPattern innerType)
                | _ ->
                    false

            let variantNamesMatch (leftName: string) (rightName: string) : bool =
                leftName = rightName
                || leftName.EndsWith($".{rightName}")
                || rightName.EndsWith($".{leftName}")

            let rec combinePatternMatchStatuses (statuses: bool option list) : bool option =
                if statuses |> List.exists (fun status -> status = Some false) then
                    Some false
                elif statuses |> List.forall (fun status -> status = Some true) then
                    Some true
                else
                    None

            let rec patternDefinitelyMatchesExpr (pattern: Pattern) (valueExpr: Expr) : bool option =
                match pattern, valueExpr with
                | PWildcard, _
                | PVar _, _ ->
                    Some true
                | PUnit, UnitLiteral ->
                    Some true
                | PInt64 expected, Int64Literal actual ->
                    Some (expected = actual)
                | PInt128Literal expected, Int128Literal actual ->
                    Some (expected = actual)
                | PInt8Literal expected, Int8Literal actual ->
                    Some (expected = actual)
                | PInt16Literal expected, Int16Literal actual ->
                    Some (expected = actual)
                | PInt32Literal expected, Int32Literal actual ->
                    Some (expected = actual)
                | PUInt8Literal expected, UInt8Literal actual ->
                    Some (expected = actual)
                | PUInt16Literal expected, UInt16Literal actual ->
                    Some (expected = actual)
                | PUInt32Literal expected, UInt32Literal actual ->
                    Some (expected = actual)
                | PUInt64Literal expected, UInt64Literal actual ->
                    Some (expected = actual)
                | PUInt128Literal expected, UInt128Literal actual ->
                    Some (expected = actual)
                | PBool expected, BoolLiteral actual ->
                    Some (expected = actual)
                | PString expected, StringLiteral actual ->
                    Some (expected = actual)
                | PChar expected, CharLiteral actual ->
                    Some (expected = actual)
                | PFloat expected, FloatLiteral actual ->
                    Some (expected = actual)
                | PTuple patterns, TupleLiteral values ->
                    if List.length patterns <> List.length values then
                        Some false
                    else
                        List.zip patterns values
                        |> List.map (fun (innerPattern, innerValue) ->
                            patternDefinitelyMatchesExpr innerPattern innerValue)
                        |> combinePatternMatchStatuses
                | PList patterns, ListLiteral values ->
                    if List.length patterns <> List.length values then
                        Some false
                    else
                        List.zip patterns values
                        |> List.map (fun (innerPattern, innerValue) ->
                            patternDefinitelyMatchesExpr innerPattern innerValue)
                        |> combinePatternMatchStatuses
                | PListCons (headPatterns, tailPattern), ListLiteral values ->
                    if List.length values < List.length headPatterns then
                        Some false
                    else
                        let headValues = values |> List.take (List.length headPatterns)
                        let tailValues = values |> List.skip (List.length headPatterns)
                        let headStatuses =
                            List.zip headPatterns headValues
                            |> List.map (fun (innerPattern, innerValue) ->
                                patternDefinitelyMatchesExpr innerPattern innerValue)
                        let tailStatus = patternDefinitelyMatchesExpr tailPattern (ListLiteral tailValues)
                        combinePatternMatchStatuses (headStatuses @ [tailStatus])
                | PConstructor (patternVariantName, patternPayload), Constructor (_, valueVariantName, valuePayload) ->
                    if not (variantNamesMatch patternVariantName valueVariantName) then
                        Some false
                    else
                        match patternPayload, valuePayload with
                        | None, None ->
                            Some true
                        | Some patternPayloadExpr, Some valuePayloadExpr ->
                            patternDefinitelyMatchesExpr patternPayloadExpr valuePayloadExpr
                        | _ ->
                            Some false
                | _ ->
                    None

            let knownCaseMatchStatus (matchCase: MatchCase) : bool option =
                match matchCase.Guard with
                | Some _ ->
                    None
                | None ->
                    let statuses =
                        matchCase.Patterns
                        |> NonEmptyList.toList
                        |> List.map (fun pattern ->
                            if patternAlwaysMatchesType pattern scrutineeType then
                                Some true
                            else
                                patternDefinitelyMatchesExpr pattern scrutinee')
                    if statuses |> List.exists (fun status -> status = Some true) then
                        Some true
                    elif statuses |> List.forall (fun status -> status = Some false) then
                        Some false
                    else
                        None

            let rec patternIsBinderOnly (pattern: Pattern) : bool =
                match pattern with
                | PVar _
                | PWildcard ->
                    true
                | PTuple patterns
                | PList patterns ->
                    patterns |> List.forall patternIsBinderOnly
                | PListCons (headPatterns, tailPattern) ->
                    (headPatterns |> List.forall patternIsBinderOnly)
                    && patternIsBinderOnly tailPattern
                | _ ->
                    false

            let caseCanShortCircuit (matchCase: MatchCase) : bool =
                Option.isNone matchCase.Guard
                && (
                    matchCase.Patterns
                    |> NonEmptyList.toList
                    |> List.forall patternIsBinderOnly
                )

            // Type check each case and ensure they all return the same type
            // Returns (resultType, transformedCases)
            let rec checkCases (remaining: MatchCase list) (resultType: Type option) (accCases: MatchCase list) : Result<Type * MatchCase list, TypeError> =
                match remaining with
                | [] ->
                    match resultType with
                    | Some t -> Ok (t, List.rev accCases)
                    | None -> Error (GenericError "Match expression must have at least one case")
                | matchCase :: rest ->
                    let caseMatchStatus = knownCaseMatchStatus matchCase
                    validatePatternGroupBindings matchCase.Patterns
                    |> Result.bind (fun () ->
                        // Extract bindings from first pattern after validation.
                        let firstPattern = NonEmptyList.head matchCase.Patterns
                        let allowNoMatchForKnownListLengthMismatch =
                            allowNoMatchForKnownListLengthMismatchInThisMatch
                            && List.isEmpty matchCase.Patterns.Tail
                        extractPatternBindings
                            firstPattern
                            scrutineeType
                            allowNoMatchForKnownListLengthMismatch
                        |> Result.bind (fun bindings ->
                            let caseEnv = List.fold (fun e (name, ty) -> Map.add name ty e) env bindings
                            // Type check guard if present (must be Bool)
                            let guardResult =
                                match matchCase.Guard with
                                | None -> Ok None
                                | Some guardExpr ->
                                    let checkedGuardResult =
                                        checkExpr
                                            guardExpr
                                            caseEnv
                                            typeReg
                                            variantLookup
                                            genericFuncReg
                                            warningSettings
                                            moduleRegistry
                                            aliasReg
                                            (Some TBool)
                                    let normalizedGuardResult =
                                        match checkedGuardResult with
                                        | Error (UndefinedVariable name) ->
                                            Error (UndefinedCallTarget name)
                                        | _ ->
                                            checkedGuardResult
                                    normalizedGuardResult
                                    |> Result.bind (fun (guardType, guard') ->
                                        if guardType = TBool then
                                            Ok (Some guard')
                                        else
                                            Error (TypeMismatch (TBool, guardType, "guard clause")))
                            guardResult
                            |> Result.bind (fun guard' ->
                                let checkedBodyResult =
                                    checkExpr
                                        matchCase.Body
                                        caseEnv
                                        typeReg
                                        variantLookup
                                        genericFuncReg
                                        warningSettings
                                        moduleRegistry
                                        aliasReg
                                        resultType
                                let normalizedBodyResult =
                                    match resultType, checkedBodyResult with
                                    | Some expectedBodyType, Error (TypeMismatch (expectedTypeFromContext, _, mismatchContext))
                                        when expectedTypeFromContext = expectedBodyType
                                             && mismatchContext = "boolean literal" ->
                                        // Retry unconstrained so we can report mismatch against
                                        // the case result type ("match body"), not literal context.
                                        checkExpr
                                            matchCase.Body
                                            caseEnv
                                            typeReg
                                            variantLookup
                                            genericFuncReg
                                            warningSettings
                                            moduleRegistry
                                            aliasReg
                                            None
                                    | _ ->
                                        checkedBodyResult
                                normalizedBodyResult
                                |> Result.bind (fun (bodyType, body') ->
                                    let newCase = { Patterns = matchCase.Patterns; Guard = guard'; Body = body' }
                                    let shouldShortCircuit =
                                        caseMatchStatus = Some true
                                        && caseCanShortCircuit matchCase
                                    match resultType with
                                    | None ->
                                        if shouldShortCircuit then
                                            Ok (bodyType, List.rev (newCase :: accCases))
                                        else
                                            checkCases rest (Some bodyType) (newCase :: accCases)
                                    | Some expected ->
                                        // Use reconcileTypes to handle type variables and type aliases
                                        match reconcileTypes (Some aliasReg) expected bodyType with
                                        | Some reconciledType ->
                                            // Update resultType to the reconciled (concrete) type
                                            if shouldShortCircuit then
                                                Ok (reconciledType, List.rev (newCase :: accCases))
                                            else
                                                checkCases rest (Some reconciledType) (newCase :: accCases)
                                        | None ->
                                            Error (TypeMismatch (expected, bodyType, "match body"))))))

            // Pass expectedType to first case so empty lists, None, etc. get the right type
            checkCases cases expectedType []
            |> Result.bind (fun (matchType, cases') ->
                match expectedType with
                | Some expected ->
                    // Use reconcileTypes for expected type check too
                    match reconcileTypes (Some aliasReg) expected matchType with
                    | Some reconciledType -> Ok (reconciledType, Match (scrutinee', cases'))
                    | None -> Error (TypeMismatch (expected, matchType, "match expression"))
                | None -> Ok (matchType, Match (scrutinee', cases'))))

    | DictLiteral (_, entries) ->
        let duplicateKey =
            entries
            |> List.fold (fun (seen, duplicate) (key, _) ->
                match duplicate with
                | Some _ -> (seen, duplicate)
                | None when Set.contains key seen -> (seen, Some key)
                | None -> (Set.add key seen, None)) (Set.empty, None)
            |> snd

        match duplicateKey with
        | Some key ->
            Error (GenericError $"Cannot add two dictionary entries with the same key `{key}`")
        | None ->
            let expectedValueType =
                match expectedType with
                | Some (TDict (TString, valueType)) -> Some valueType
                | _ -> None

            let finish valueType checkedEntries =
                let dictType = TDict (TString, valueType)
                match expectedType with
                | Some expected ->
                    match reconcileTypes (Some aliasReg) expected dictType with
                    | Some reconciled -> Ok (reconciled, DictLiteral (valueType, checkedEntries))
                    | None -> Error (TypeMismatch (expected, dictType, "Dict literal"))
                | None -> Ok (dictType, DictLiteral (valueType, checkedEntries))

            match entries with
            | [] ->
                finish (Option.defaultValue (TVar "dictValue") expectedValueType) []
            | (firstKey, firstValue) :: rest ->
                checkExpr firstValue env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedValueType
                |> Result.bind (fun (valueType, checkedFirst) ->
                    let rec checkRemaining remaining acc =
                        match remaining with
                        | [] -> Ok (List.rev acc)
                        | (key, value) :: tail ->
                            checkExpr value env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                            |> Result.bind (fun (actualType, checkedValue) ->
                                match reconcileTypes (Some aliasReg) valueType actualType with
                                | Some _ -> checkRemaining tail ((key, checkedValue) :: acc)
                                | None ->
                                    Error (GenericError $"dict values must have one type: key `{key}` has {typeToString actualType}, expected {typeToString valueType}"))
                    checkRemaining rest [(firstKey, checkedFirst)]
                    |> Result.bind (finish valueType))

    | ListLiteral elements ->
        // Type-check elements and infer element type from first element
        match elements with
        | [] ->
            // Empty list: use expected list type or keep a type variable
            match expectedType with
            | Some (TList elemType) -> Ok (TList elemType, ListLiteral [])
            | Some other -> Error (TypeMismatch (other, TList (TVar "t"), "empty list"))
            | None -> Ok (TList (TVar "t"), ListLiteral [])
        | first :: rest ->
            // Use expected list element type for the first element when available, so
            // lambda/list literals in expected contexts reconcile type variables consistently.
            let firstExpectedType =
                match expectedType with
                | Some (TList expectedElemType) -> Some expectedElemType
                | _ -> None
            checkExpr first env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg firstExpectedType
            |> Result.bind (fun (elemType, first') ->
                // Check remaining elements match the inferred type
                let rec checkRest remaining acc =
                    match remaining with
                    | [] -> Ok (List.rev acc)
                    | e :: rs ->
                        checkExpr e env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some elemType)
                        |> Result.bind (fun (eType, e') ->
                            if eType = elemType then checkRest rs (e' :: acc)
                            else Error (TypeMismatch (elemType, eType, "list element")))
                checkRest rest [first']
                |> Result.bind (fun elements' ->
                    let listType = TList elemType
                    match expectedType with
                    | Some expected ->
                        // Use reconcileTypes to allow type variables to unify and resolve type aliases
                        match reconcileTypes (Some aliasReg) expected listType with
                        | Some reconciledType -> Ok (reconciledType, ListLiteral elements')
                        | None -> Error (TypeMismatch (expected, listType, "list literal"))
                    | None -> Ok (listType, ListLiteral elements')))

    | ListCons (headElements, tail) ->
        // Type-check tail first to get element type
        // Pass expected type to tail if it's a list type
        let tailExpectedType =
            match expectedType with
            | Some (TList _) -> expectedType
            | _ -> None
        checkExpr tail env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg tailExpectedType
        |> Result.bind (fun (tailType, tail') ->
            match tailType with
            | TList elemType ->
                // Type-check each head element with the inferred element type
                // Track the most concrete element type as we process elements
                let rec checkHeads elems currentElemType acc =
                    match elems with
                    | [] -> Ok (currentElemType, List.rev acc)
                    | h :: rest ->
                        checkExpr h env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some currentElemType)
                        |> Result.bind (fun (hType, h') ->
                            // Use reconcileTypes to allow type variables to unify with concrete types
                            match reconcileTypes (Some aliasReg) currentElemType hType with
                            | None -> Error (TypeMismatch (currentElemType, hType, "list cons element"))
                            | Some reconciledElemType -> checkHeads rest reconciledElemType (h' :: acc))
                checkHeads headElements elemType []
                |> Result.bind (fun (finalElemType, heads') ->
                    let listType = TList finalElemType
                    match expectedType with
                    | Some expected ->
                        // Use reconcileTypes to allow type variables to unify with concrete types
                        match reconcileTypes (Some aliasReg) expected listType with
                        | None -> Error (TypeMismatch (expected, listType, "list cons"))
                        | Some reconciledType -> Ok (reconciledType, ListCons (heads', tail'))
                    | None -> Ok (listType, ListCons (heads', tail')))
            | other -> Error (TypeMismatch (TList (TVar "t"), other, "list cons tail must be a list")))

    | Lambda (parameters, returnAnnotation, body) ->
        let parametersList = NonEmptyList.toList parameters
        let parameterNames =
            parametersList
            |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
            |> Set.ofList

        let concreteTypeOf candidate =
            let candidateType =
                match candidate with
                | UnitLiteral -> Some TUnit
                | Int64Literal _ -> Some TInt64
                | Int128Literal _ -> Some TInt128
                | BigIntLiteral _ -> Some TInt
                | Int8Literal _ -> Some TInt8
                | Int16Literal _ -> Some TInt16
                | Int32Literal _ -> Some TInt32
                | UInt8Literal _ -> Some TUInt8
                | UInt16Literal _ -> Some TUInt16
                | UInt32Literal _ -> Some TUInt32
                | UInt64Literal _ -> Some TUInt64
                | UInt128Literal _ -> Some TUInt128
                | FloatLiteral _ -> Some TFloat64
                | BoolLiteral _ -> Some TBool
                | StringLiteral _ -> Some TString
                | CharLiteral _ -> Some TChar
                | Var name -> Map.tryFind name env
                | Call (name, _) ->
                    match Map.tryFind name env with
                    | Some (TFunction (_, returnType)) -> Some returnType
                    | _ ->
                        Stdlib.tryGetFunction moduleRegistry name
                        |> Option.map (fun (fn, _) -> fn.ReturnType)
                | _ -> None
            candidateType |> Option.filter (containsTVar >> not)

        let addConstraint name typ constraints =
            if Set.contains name parameterNames && not (containsTVar typ) then
                match Map.tryFind name constraints with
                | Some existing ->
                    match reconcileTypes (Some aliasReg) existing typ with
                    | Some reconciled -> Map.add name reconciled constraints
                    | None -> constraints
                | None -> Map.add name typ constraints
            else
                constraints

        let rec collectConstraints expected candidate constraints =
            let collect expectedType inner current =
                collectConstraints expectedType inner current
            let collectChildren children current =
                children |> List.fold (fun state child -> collect None child state) current
            match candidate with
            | Var name ->
                expected |> Option.map (fun typ -> addConstraint name typ constraints) |> Option.defaultValue constraints
            | BinOp (op, left, right) ->
                match op with
                | Add | Sub | Mul | Div | Mod | Shl | Shr | BitAnd | BitOr | BitXor ->
                    let operandType =
                        expected
                        |> Option.filter (containsTVar >> not)
                        |> Option.orElseWith (fun () -> concreteTypeOf left)
                        |> Option.orElseWith (fun () -> concreteTypeOf right)
                    constraints |> collect operandType left |> collect operandType right
                | Lt | Gt | Lte | Gte | Eq | Neq ->
                    let operandType = concreteTypeOf left |> Option.orElseWith (fun () -> concreteTypeOf right)
                    constraints |> collect operandType left |> collect operandType right
                | StringConcat -> constraints |> collect (Some TString) left |> collect (Some TString) right
                | And | Or -> constraints |> collect (Some TBool) left |> collect (Some TBool) right
            | UnaryOp (Not, inner) -> collect (Some TBool) inner constraints
            | UnaryOp (_, inner) -> collect expected inner constraints
            | Call (name, arguments) ->
                let argumentList = NonEmptyList.toList arguments
                match Map.tryFind name env with
                | Some (TFunction (parameterTypes, _)) when List.length parameterTypes = List.length argumentList ->
                    List.zip parameterTypes argumentList
                    |> List.fold (fun state (parameterType, argument) -> collect (Some parameterType) argument state) constraints
                | _ -> collectChildren argumentList constraints
            | Apply (func, arguments)
            | IndirectApply (func, arguments) ->
                let argumentList = NonEmptyList.toList arguments
                let constraints = collect None func constraints
                match func with
                | Var name ->
                    match Map.tryFind name env with
                    | Some (TFunction (parameterTypes, _)) when List.length argumentList <= List.length parameterTypes ->
                        List.zip (List.take (List.length argumentList) parameterTypes) argumentList
                        |> List.fold (fun state (parameterType, argument) -> collect (Some parameterType) argument state) constraints
                    | _ -> collectChildren argumentList constraints
                | _ -> collectChildren argumentList constraints
            | Let (pattern, value, continuation) ->
                let afterValue = collect None value constraints
                if letPatternBindings pattern |> List.exists (fun name -> Set.contains name parameterNames) then afterValue
                else collect expected continuation afterValue
            | Lambda (nestedParameters, _, nestedBody) ->
                let shadows =
                    nestedParameters
                    |> NonEmptyList.toList
                    |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
                    |> List.exists (fun name -> Set.contains name parameterNames)
                if shadows then
                    constraints
                else
                    let nestedReturnExpectation =
                        match expected with
                        | Some (TFunction (_, returnType)) -> Some returnType
                        | _ -> None
                    collect nestedReturnExpectation nestedBody constraints
            | BoundaryRender (_, value) | TupleAccess (value, _) | RecordAccess (value, _) ->
                collect None value constraints
            | Sequence (first, next) -> constraints |> collect None first |> collect expected next
            | If (condition, thenBranch, elseBranch) ->
                constraints
                |> collect (Some TBool) condition
                |> collect expected thenBranch
                |> collect expected elseBranch
            | TypeApp (_, _, arguments) -> collectChildren (NonEmptyList.toList arguments) constraints
            | TupleLiteral elements | ListLiteral elements -> collectChildren elements constraints
            | DictLiteral (_, entries) -> collectChildren (entries |> List.map snd) constraints
            | RecordLiteral (_, fields) -> collectChildren (fields |> List.map snd) constraints
            | RecordUpdate (record, fields) -> collectChildren (record :: (fields |> List.map snd)) constraints
            | Constructor (_, _, payload) -> payload |> Option.map (fun value -> collect None value constraints) |> Option.defaultValue constraints
            | Match (scrutinee, cases) ->
                let afterScrutinee = collect None scrutinee constraints
                cases
                |> List.fold (fun state case ->
                    state
                    |> fun current -> case.Guard |> Option.map (fun guard -> collect (Some TBool) guard current) |> Option.defaultValue current
                    |> collect expected case.Body) afterScrutinee
            | ListCons (head, tail) -> collectChildren (head @ [tail]) constraints
            | Closure (_, captures) -> collectChildren captures constraints
            | InterpolatedString parts ->
                parts
                |> List.choose (function StringExpr inner -> Some inner | StringText _ -> None)
                |> fun children -> collectChildren children constraints
            | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
            | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
            | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
            | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
            | FuncRef _ | RuntimeError _ -> constraints

        let bodyConstraints = collectConstraints returnAnnotation body Map.empty

        let rec refinePatternType pattern typ =
            match pattern, typ with
            | LPVariable name, _ -> Map.tryFind name bodyConstraints |> Option.defaultValue typ
            | LPTuple (first, second, rest), TTuple elementTypes ->
                let patterns = first :: second :: rest
                if List.length patterns = List.length elementTypes then
                    List.zip patterns elementTypes
                    |> List.map (fun (innerPattern, innerType) -> refinePatternType innerPattern innerType)
                    |> TTuple
                else typ
            | _ -> typ

        let typeCheckLambdaWithParams
            (resolvedParams: (LambdaParameter * Type) list)
            (bodyExpectedType: Type option)
            : Result<Type * Expr, TypeError> =
            let bindingResults =
                resolvedParams
                |> List.map (fun (parameter, typ) -> bindLetPatternTypes parameter.Pattern typ)
            if bindingResults |> List.exists Option.isNone then
                Error (GenericError "Lambda parameter pattern is incompatible with its inferred type")
            else
                let bindings = bindingResults |> List.choose id |> List.concat
                let paramEnv =
                    bindings
                    |> List.fold (fun current (name, typ) -> Map.add name typ current) env
                let effectiveBodyExpectedType =
                    returnAnnotation |> Option.orElse bodyExpectedType
                checkExpr body paramEnv typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg effectiveBodyExpectedType
                |> Result.map (fun (bodyType, body') ->
                    let paramTypes = resolvedParams |> List.map snd
                    let typedParams =
                        resolvedParams
                        |> List.map (fun (parameter, typ) -> { parameter with InferredType = Some typ })
                        |> NonEmptyList.fromList
                    (TFunction (paramTypes, bodyType), Lambda (typedParams, returnAnnotation, body')))

        let initialParams =
            parametersList
            |> List.mapi (fun index parameter ->
                let initialType =
                    parameter.InferredType
                    |> Option.orElse parameter.SourceAnnotation
                    |> Option.defaultValue (inferredLetPatternType $"lambda_{index}" parameter.Pattern)
                (parameter, refinePatternType parameter.Pattern initialType))

        match expectedType with
        | Some (TFunction (expectedParams, expectedRet)) ->
            if List.length expectedParams < List.length parametersList then
                // Interpreter lambdas may expose a prefix of their binders as
                // the callable expected by a higher-order argument. Preserve
                // the remaining binders as a returned lambda so applying the
                // outer closure once has the same curried behavior.
                let (outerParameters, remainingParameters) =
                    List.splitAt (List.length expectedParams) parametersList
                match NonEmptyList.tryFromList outerParameters, NonEmptyList.tryFromList remainingParameters with
                | Some outer, Some remaining ->
                    let nested = Lambda (remaining, returnAnnotation, body)
                    checkExpr
                        (Lambda (outer, None, nested))
                        env
                        typeReg
                        variantLookup
                        genericFuncReg
                        warningSettings
                        moduleRegistry
                        aliasReg
                        expectedType
                | _ -> Error (GenericError "Lambda currying requires non-empty binder groups")
            elif List.length expectedParams <> List.length parametersList then
                Error (GenericError $"Expected {List.length parametersList} arguments, got {List.length expectedParams}")
            else
                let rec reconcileParamTypes
                    (remaining: ((LambdaParameter * Type) * Type) list)
                    (acc: (LambdaParameter * Type) list)
                    : Result<(LambdaParameter * Type) list, TypeError> =
                    match remaining with
                    | [] -> Ok (List.rev acc)
                    | ((parameter, declaredParamType), expectedParamType) :: rest ->
                        match reconcileTypes (Some aliasReg) declaredParamType expectedParamType with
                        | Some reconciledParamType ->
                            reconcileParamTypes rest ((parameter, reconciledParamType) :: acc)
                        | None ->
                            Error (TypeMismatch (expectedParamType, declaredParamType, "lambda parameter type"))

                reconcileParamTypes (List.zip initialParams expectedParams) []
                |> Result.bind (fun reconciledParams ->
                    let bodyExpectedType =
                        if containsTVar expectedRet then
                            None
                        else
                            Some expectedRet
                    typeCheckLambdaWithParams reconciledParams bodyExpectedType
                    |> Result.bind (fun (funcType, lambdaExpr) ->
                        match funcType with
                        | TFunction (paramTypes, bodyType) ->
                            match reconcileTypes (Some aliasReg) expectedRet bodyType with
                            | None ->
                                Error (TypeMismatch (expectedRet, bodyType, "lambda return type"))
                            | Some reconciledRetType ->
                                Ok (TFunction (paramTypes, reconciledRetType), lambdaExpr)
                        | _ ->
                            Error (GenericError "Internal error: lambda did not type-check to a function")))
        | Some other ->
            typeCheckLambdaWithParams initialParams None
            |> Result.bind (fun (funcType, lambdaExpr) ->
                match reconcileTypes (Some aliasReg) other funcType with
                | Some reconciledType -> Ok (reconciledType, lambdaExpr)
                | None -> Error (TypeMismatch (other, funcType, "lambda")))
        | None ->
            typeCheckLambdaWithParams initialParams None

    | Apply (func, args) ->
        let argsList = NonEmptyList.toList args
        let functionExpectedType =
            match func with
            | Lambda (parameters, _, _) ->
                inferFunctionExpectationFromArguments
                    (parameters |> NonEmptyList.toList |> List.length)
                    argsList
            | _ -> None
        // Type-check the function expression
        checkExpr func env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg functionExpectedType
        |> Result.bind (fun (funcType, func') ->
            match funcType with
            | TFunction (paramTypes, returnType) ->
                let numParams = List.length paramTypes
                let argsList = normalizeNullaryCallArgs numParams argsList
                let numArgs = List.length argsList
                // Check argument count - allow partial application
                if numArgs > numParams then
                    Error (GenericError $"Expected {numParams} arguments, got {numArgs}")
                else if numArgs < numParams then
                    // Partial application of lambda/function value
                    let providedParamTypes = List.take numArgs paramTypes
                    let remainingParamTypes = List.skip numArgs paramTypes

                    // Type-check the provided arguments
                    let rec checkProvidedArgs (argExprs: Expr list) (paramTys: Type list) (checkedArgs: Expr list) : Result<Expr list, TypeError> =
                        match argExprs, paramTys with
                        | [], [] -> Ok (List.rev checkedArgs)
                        | arg :: restArgs, paramTy :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramTy)
                            |> Result.bind (fun (argType, arg') ->
                                if typesEqual aliasReg argType paramTy then
                                    checkProvidedArgs restArgs restParams (arg' :: checkedArgs)
                                else
                                    Error (TypeMismatch (paramTy, argType, "function argument")))
                        | _ -> Error (GenericError "Argument count mismatch")

                    checkProvidedArgs argsList providedParamTypes []
                    |> Result.bind (fun args' ->
                        // Create fresh parameter names for the remaining parameters
                        // Use "lambda" as identifier since we're applying a function value, not a named function
                        let remainingParams = makePartialParams "lambda" remainingParamTypes

                        // Create the lambda body: apply the original function with all args
                        let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                        let lambdaBody = Apply (func', toCallArgs allArgs)

                        // Create the lambda: fun p0 p1 ... -> func(providedArgs, p0, p1, ...)
                        let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                        // The resulting type is a function from remaining params to return type
                        let partialType = TFunction (remainingParamTypes, returnType)

                        match expectedType with
                        | Some expected when not (typesEqual aliasReg expected partialType) ->
                            Error (TypeMismatch (expected, partialType, "partial application"))
                        | _ -> Ok (partialType, lambdaExpr))
                else
                    // Check each argument against expected param type
                    let rec checkArgs (argExprs: Expr list) (paramTys: Type list) (checkedArgs: Expr list) : Result<Expr list, TypeError> =
                        match argExprs, paramTys with
                        | [], [] -> Ok (List.rev checkedArgs)
                        | arg :: restArgs, paramTy :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramTy)
                            |> Result.bind (fun (argType, arg') ->
                                if typesEqual aliasReg argType paramTy then
                                    checkArgs restArgs restParams (arg' :: checkedArgs)
                                else
                                    Error (TypeMismatch (paramTy, argType, "function argument")))
                        | _ -> Error (GenericError "Argument count mismatch")
                    checkArgs argsList paramTypes []
                    |> Result.bind (fun args' ->
                        match expectedType with
                        | Some expected when not (typesEqual aliasReg expected returnType) ->
                            Error (TypeMismatch (expected, returnType, "function application result"))
                        | _ -> Ok (returnType, Apply (func', toCallArgs args')))
            | _ ->
                Error (GenericError $"Cannot apply non-function type: {typeToString funcType}"))

    | IndirectApply _ ->
        Crash.crash "IndirectApply is compiler-generated after expression type checking"

    | FuncRef funcName ->
        // Function reference: look up function signature
        match Map.tryFind funcName env with
        | Some funcType ->
            match expectedType with
            | Some expected when expected <> funcType ->
                Error (TypeMismatch (expected, funcType, $"function reference {funcName}"))
            | _ -> Ok (funcType, expr)
        | None ->
            Error (UndefinedVariable funcName)

    | Closure (funcName, captures) ->
        // Closure: function with captured values
        // The closure has the same type as the underlying function (minus closure param)
        // For now, just check the captures and return function type
        let checkCapture (cap: Expr) : Result<Expr, TypeError> =
            checkExpr cap env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.map snd
        let rec checkCaptures (caps: Expr list) (acc: Expr list) : Result<Expr list, TypeError> =
            match caps with
            | [] -> Ok (List.rev acc)
            | cap :: rest ->
                checkCapture cap |> Result.bind (fun cap' -> checkCaptures rest (cap' :: acc))
        checkCaptures captures []
        |> Result.bind (fun captures' ->
            // Look up closure function type
            match Map.tryFind funcName env with
            | Some (TFunction (_ :: restParams, returnType)) ->
                // The closure type is the function type without the closure param
                let closureType = TFunction (restParams, returnType)
                match expectedType with
                | Some expected when expected <> closureType ->
                    Error (TypeMismatch (expected, closureType, $"closure {funcName}"))
                | _ -> Ok (closureType, Closure (funcName, captures'))
            | Some funcType -> Ok (funcType, Closure (funcName, captures'))
            | None -> Error (UndefinedVariable funcName))

type private EqHelperExprMode =
    | ExpandCurrent
    | UseHelperCall

let private makeSimpleMatchCase (pattern: Pattern) (body: Expr) : MatchCase =
    { Patterns = NonEmptyList.singleton pattern
      Guard = None
      Body = body }

let rec private buildEqHelperExpr
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (mode: EqHelperExprMode)
    (typ: Type)
    (leftExpr: Expr)
    (rightExpr: Expr)
    : Expr =
    let resolvedType = resolveType aliasReg typ

    match (mode, resolvedType) with
    | UseHelperCall, helperType when needsEqHelperForResolvedType variantLookup helperType ->
        Call (eqHelperName helperType, NonEmptyList.fromList [leftExpr; rightExpr])

    | ExpandCurrent, TFunction _ ->
        // Lambda lifting stores semantic identity and a capture-aware comparator
        // after the operational code pointer. The comparator is fixed during
        // specialization, so generic function equality requires no type dispatch.
        let leftVar = "__dark_eq_helper_fn_left"
        let rightVar = "__dark_eq_helper_fn_right"
        Let (
            LPVariable leftVar,
            leftExpr,
            Let (
                LPVariable rightVar,
                rightExpr,
                If (
                    BinOp (
                        Eq,
                        TupleAccess (Var leftVar, 1),
                        TupleAccess (Var rightVar, 1)
                    ),
                    IndirectApply (
                        TupleAccess (Var leftVar, 1),
                        NonEmptyList.fromList [Var leftVar; Var rightVar]
                    ),
                    BoolLiteral false
                )
            )
        )

    | ExpandCurrent, TList elemType ->
        let resolvedElemType = resolveType aliasReg elemType
        let leftHead = "__dark_eq_helper_list_left_head"
        let leftTail = "__dark_eq_helper_list_left_tail"
        let rightHead = "__dark_eq_helper_list_right_head"
        let rightTail = "__dark_eq_helper_list_right_tail"
        let headsEqual =
            buildEqHelperExpr
                aliasReg
                typeReg
                variantLookup
                UseHelperCall
                resolvedElemType
                (Var leftHead)
                (Var rightHead)
        let tailsEqual =
            Call (
                eqHelperName resolvedType,
                NonEmptyList.fromList [Var leftTail; Var rightTail]
            )
        let bothEmpty =
            makeSimpleMatchCase (PTuple [PList []; PList []]) (BoolLiteral true)
        let bothNonEmpty =
            makeSimpleMatchCase
                (PTuple [
                    PListCons ([PVar leftHead], PVar leftTail)
                    PListCons ([PVar rightHead], PVar rightTail)
                ])
                (BinOp (And, headsEqual, tailsEqual))
        Match (TupleLiteral [leftExpr; rightExpr], [bothEmpty; bothNonEmpty; makeSimpleMatchCase PWildcard (BoolLiteral false)])

    | _, TList elemType ->
        TypeApp ("Stdlib.List.equals", [resolveType aliasReg elemType], NonEmptyList.fromList [leftExpr; rightExpr])

    | ExpandCurrent, TString ->
        BinOp (Eq, leftExpr, rightExpr)

    | _, TString ->
        Call ("Stdlib.String.equals", NonEmptyList.fromList [leftExpr; rightExpr])

    | ExpandCurrent, TDict (TString, valueType) ->
        let entryType = TTuple [TString; resolveType aliasReg valueType]
        let listType = TList entryType
        let leftEntries = TypeApp ("Stdlib.Dict.toList", [valueType], NonEmptyList.singleton leftExpr)
        let rightEntries = TypeApp ("Stdlib.Dict.toList", [valueType], NonEmptyList.singleton rightExpr)
        buildEqHelperExpr aliasReg typeReg variantLookup UseHelperCall listType leftEntries rightEntries

    | ExpandCurrent, TTuple elemTypes ->
        let leftTupleVar = "__dark_eq_helper_tuple_left"
        let rightTupleVar = "__dark_eq_helper_tuple_right"
        let elementComparisons =
            elemTypes
            |> List.mapi (fun index elemType ->
                buildEqHelperExpr
                    aliasReg
                    typeReg
                    variantLookup
                    UseHelperCall
                    elemType
                    (TupleAccess (Var leftTupleVar, index))
                    (TupleAccess (Var rightTupleVar, index)))
        Let (LPVariable leftTupleVar, leftExpr, Let (LPVariable rightTupleVar, rightExpr, chainAndExpr elementComparisons))

    | ExpandCurrent, TRecord (recordTypeName, typeArgs) ->
        match Map.tryFind recordTypeName typeReg with
        | None ->
            BinOp (Eq, leftExpr, rightExpr)
        | Some recordInfo ->
            let fields = recordInfo.Fields
            let concreteFields =
                match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                | Ok subst ->
                    fields |> List.map (fun (name, fieldType) -> (name, resolveType aliasReg (applySubst subst fieldType)))
                | Error _ ->
                    fields |> List.map (fun (name, fieldType) -> (name, resolveType aliasReg fieldType))

            let leftRecordVar = "__dark_eq_helper_record_left"
            let rightRecordVar = "__dark_eq_helper_record_right"
            let fieldComparisons =
                concreteFields
                |> List.mapi (fun index (_, fieldType) ->
                    buildEqHelperExpr
                        aliasReg
                        typeReg
                        variantLookup
                        UseHelperCall
                        fieldType
                        (TupleAccess (Var leftRecordVar, index))
                        (TupleAccess (Var rightRecordVar, index)))
            Let (LPVariable leftRecordVar, leftExpr, Let (LPVariable rightRecordVar, rightExpr, chainAndExpr fieldComparisons))

    | ExpandCurrent, TSum (sumTypeName, sumTypeArgs) ->
        if not (sumTypeHasPayload variantLookup sumTypeName) then
            BinOp (Eq, leftExpr, rightExpr)
        else
            let variantsForType =
                variantLookup
                |> Map.toList
                |> List.choose (fun (variantName, (variantTypeName, typeParams, tag, payloadOpt)) ->
                    if variantTypeName = sumTypeName then
                        let concretePayloadOpt =
                            match payloadOpt with
                            | Some payloadType when List.length typeParams = List.length sumTypeArgs ->
                                let subst = List.zip typeParams sumTypeArgs |> Map.ofList
                                Some (resolveType aliasReg (applySubst subst payloadType))
                            | Some payloadType ->
                                Some (resolveType aliasReg payloadType)
                            | None ->
                                None
                        Some (variantName, tag, concretePayloadOpt)
                    else
                        None)
                |> List.sortBy (fun (_, tag, _) -> tag)

            let variantCases =
                variantsForType
                |> List.map (fun (variantName, tag, payloadTypeOpt) ->
                    match payloadTypeOpt with
                    | None ->
                        let pairPattern =
                            PTuple [PConstructor (variantName, None); PConstructor (variantName, None)]
                        makeSimpleMatchCase pairPattern (BoolLiteral true)
                    | Some payloadType ->
                        let leftPayloadVar = $"__dark_eq_helper_left_payload_{tag}"
                        let rightPayloadVar = $"__dark_eq_helper_right_payload_{tag}"
                        let payloadEqExpr =
                            buildEqHelperExpr
                                aliasReg
                                typeReg
                                variantLookup
                                UseHelperCall
                                payloadType
                                (Var leftPayloadVar)
                                (Var rightPayloadVar)
                        let pairPattern =
                            PTuple [
                                PConstructor (variantName, Some (PVar leftPayloadVar))
                                PConstructor (variantName, Some (PVar rightPayloadVar))
                            ]
                        makeSimpleMatchCase pairPattern payloadEqExpr)

            let defaultCase = makeSimpleMatchCase PWildcard (BoolLiteral false)
            let sumPairVar = "__dark_eq_helper_sum_pair"
            Let (LPVariable sumPairVar, TupleLiteral [leftExpr; rightExpr], Match (Var sumPairVar, variantCases @ [defaultCase]))

    | _, _ ->
        BinOp (Eq, leftExpr, rightExpr)

let private collectDirectEqHelperDeps
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (typ: Type)
    : Type list =
    let addIfHelperType (candidate: Type) : Type option =
        let resolved = resolveType aliasReg candidate
        if needsEqHelperForResolvedType variantLookup resolved then Some resolved else None

    let resolvedType = resolveType aliasReg typ
    let deps =
        match resolvedType with
        | TList elemType ->
            [elemType] |> List.choose addIfHelperType
        | TDict (TString, valueType) ->
            [TList (TTuple [TString; valueType])] |> List.choose addIfHelperType
        | TTuple elemTypes ->
            elemTypes |> List.choose addIfHelperType
        | TRecord (recordTypeName, typeArgs) ->
            match Map.tryFind recordTypeName typeReg with
            | None ->
                []
            | Some recordInfo ->
                let fields = recordInfo.Fields
                let concreteFields =
                    match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                    | Ok subst ->
                        fields |> List.map (fun (_, fieldType) -> resolveType aliasReg (applySubst subst fieldType))
                    | Error _ ->
                        fields |> List.map (fun (_, fieldType) -> resolveType aliasReg fieldType)
                concreteFields |> List.choose addIfHelperType
        | TSum (sumTypeName, sumTypeArgs) ->
            variantLookup
            |> Map.toList
            |> List.choose (fun (_, (variantTypeName, typeParams, _, payloadOpt)) ->
                if variantTypeName = sumTypeName then
                    match payloadOpt with
                    | Some payloadType when List.length typeParams = List.length sumTypeArgs ->
                        let subst = List.zip typeParams sumTypeArgs |> Map.ofList
                        addIfHelperType (applySubst subst payloadType)
                    | Some payloadType ->
                        addIfHelperType payloadType
                    | None ->
                        None
                else
                    None)
        | _ ->
            []
    deps |> List.distinctBy eqHelperName

type private EqHelperGenerationState = {
    InProgress: Set<string>
    Generated: Map<string, FunctionDef>
}

let rec private ensureEqHelperForType
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (typ: Type)
    (state: EqHelperGenerationState)
    : EqHelperGenerationState =
    let resolvedType = resolveType aliasReg typ
    let rec referencesKnownNominals candidate =
        match resolveType aliasReg candidate with
        | TRecord (name, typeArgs) ->
            Map.containsKey name typeReg && List.forall referencesKnownNominals typeArgs
        | TSum (name, typeArgs) ->
            variantLookup
            |> Map.exists (fun _ (owner, _, _, _) -> owner = name)
            && List.forall referencesKnownNominals typeArgs
        | TList elementType -> referencesKnownNominals elementType
        | TDict (keyType, valueType) ->
            referencesKnownNominals keyType && referencesKnownNominals valueType
        | TTuple elementTypes
        | TEnumFields elementTypes -> List.forall referencesKnownNominals elementTypes
        | TFunction (parameterTypes, returnType) ->
            List.forall referencesKnownNominals parameterTypes
            && referencesKnownNominals returnType
        | _ -> true
    if not (needsEqHelperForResolvedType variantLookup resolvedType)
       || not (referencesKnownNominals resolvedType) then
        state
    else
        let helper = eqHelperName resolvedType
        if Map.containsKey helper state.Generated || Set.contains helper state.InProgress then
            state
        else
            let stateInProgress = { state with InProgress = Set.add helper state.InProgress }
            let deps = collectDirectEqHelperDeps aliasReg typeReg variantLookup resolvedType
            let stateWithDeps =
                deps
                |> List.fold
                    (fun currentState depType ->
                        ensureEqHelperForType aliasReg typeReg variantLookup depType currentState)
                    stateInProgress

            let leftParam = "__dark_eq_left"
            let rightParam = "__dark_eq_right"
            let helperBody =
                buildEqHelperExpr
                    aliasReg
                    typeReg
                    variantLookup
                    ExpandCurrent
                    resolvedType
                    (Var leftParam)
                    (Var rightParam)

            let helperDef : FunctionDef = {
                Name = helper
                TypeParams = []
                Params = NonEmptyList.fromList [ (leftParam, resolvedType); (rightParam, resolvedType) ]
                ReturnType = TBool
                Body = helperBody
            }

            {
                InProgress = Set.remove helper stateWithDeps.InProgress
                Generated = Map.add helper helperDef stateWithDeps.Generated
            }

let rec private collectEqHelperTypesFromExpr (aliasReg: AliasRegistry) (expr: Expr) : Set<Type> =
    let collectFromExprs (exprs: Expr list) : Set<Type> =
        exprs
        |> List.map (collectEqHelperTypesFromExpr aliasReg)
        |> List.fold Set.union Set.empty

    match expr with
    | BoundaryRender (_, value) -> collectEqHelperTypesFromExpr aliasReg value
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ ->
        Set.empty
    | BinOp (_, left, right) ->
        Set.union (collectEqHelperTypesFromExpr aliasReg left) (collectEqHelperTypesFromExpr aliasReg right)
    | UnaryOp (_, inner) ->
        collectEqHelperTypesFromExpr aliasReg inner
    | Let (_, value, body) ->
        Set.union (collectEqHelperTypesFromExpr aliasReg value) (collectEqHelperTypesFromExpr aliasReg body)
    | If (cond, thenBranch, elseBranch) ->
        Set.union
            (collectEqHelperTypesFromExpr aliasReg cond)
            (Set.union
                (collectEqHelperTypesFromExpr aliasReg thenBranch)
                (collectEqHelperTypesFromExpr aliasReg elseBranch))
    | Sequence (first, next) ->
        Set.union
            (collectEqHelperTypesFromExpr aliasReg first)
            (collectEqHelperTypesFromExpr aliasReg next)
    | Call (_, args) ->
        collectFromExprs (NonEmptyList.toList args)
    | TypeApp (_, typeArgs, args) as typeAppExpr ->
        match tryDecodeInternalTypeApp typeAppExpr with
        | Some (EqHelperDispatchTypeApp (targetType, leftExpr, rightExpr)) ->
            Set.add
                (resolveType aliasReg targetType)
                (Set.union
                    (collectEqHelperTypesFromExpr aliasReg leftExpr)
                    (collectEqHelperTypesFromExpr aliasReg rightExpr))
        | None ->
            typeArgs
            |> List.map (resolveType aliasReg)
            |> List.filter (containsTVar >> not)
            |> List.fold
                (fun helpers typ -> Set.add typ helpers)
                (collectFromExprs (NonEmptyList.toList args))
    | TupleLiteral elements ->
        collectFromExprs elements
    | TupleAccess (tupleExpr, _) ->
        collectEqHelperTypesFromExpr aliasReg tupleExpr
    | DictLiteral (_, entries) ->
        entries |> List.map snd |> collectFromExprs
    | RecordLiteral (_, fields) ->
        fields |> List.map snd |> collectFromExprs
    | RecordUpdate (recordExpr, updates) ->
        Set.union
            (collectEqHelperTypesFromExpr aliasReg recordExpr)
            (updates |> List.map snd |> collectFromExprs)
    | RecordAccess (recordExpr, _) ->
        collectEqHelperTypesFromExpr aliasReg recordExpr
    | Constructor (_, _, payload) ->
        payload |> Option.map (collectEqHelperTypesFromExpr aliasReg) |> Option.defaultValue Set.empty
    | Match (scrutinee, cases) ->
        let scrutineeTypes = collectEqHelperTypesFromExpr aliasReg scrutinee
        let caseTypes =
            cases
            |> List.map (fun matchCase ->
                let guardTypes =
                    matchCase.Guard
                    |> Option.map (collectEqHelperTypesFromExpr aliasReg)
                    |> Option.defaultValue Set.empty
                Set.union guardTypes (collectEqHelperTypesFromExpr aliasReg matchCase.Body))
            |> List.fold Set.union Set.empty
        Set.union scrutineeTypes caseTypes
    | ListLiteral elements ->
        collectFromExprs elements
    | ListCons (headElements, tailExpr) ->
        Set.union (collectFromExprs headElements) (collectEqHelperTypesFromExpr aliasReg tailExpr)
    | Lambda (_, _, body) ->
        collectEqHelperTypesFromExpr aliasReg body
    | Apply (funcExpr, args)
    | IndirectApply (funcExpr, args) ->
        Set.union (collectEqHelperTypesFromExpr aliasReg funcExpr) (collectFromExprs (NonEmptyList.toList args))
    | Closure (_, captures) ->
        collectFromExprs captures
    | InterpolatedString parts ->
        parts
        |> List.choose (function
            | StringText _ -> None
            | StringExpr partExpr -> Some (collectEqHelperTypesFromExpr aliasReg partExpr))
        |> List.fold Set.union Set.empty

let rec private materializeEqHelperCallsInExpr (aliasReg: AliasRegistry) (expr: Expr) : Expr =
    let recurse = materializeEqHelperCallsInExpr aliasReg

    match expr with
    | BoundaryRender (renderer, value) -> BoundaryRender (renderer, recurse value)
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ ->
        expr
    | BinOp (op, left, right) ->
        BinOp (op, recurse left, recurse right)
    | UnaryOp (op, inner) ->
        UnaryOp (op, recurse inner)
    | Let (name, value, body) ->
        Let (name, recurse value, recurse body)
    | If (cond, thenBranch, elseBranch) ->
        If (recurse cond, recurse thenBranch, recurse elseBranch)
    | Sequence (first, next) ->
        Sequence (recurse first, recurse next)
    | Call (funcName, args) ->
        Call (funcName, NonEmptyList.map recurse args)
    | TypeApp (funcName, typeArgs, args) as typeAppExpr ->
        match tryDecodeInternalTypeApp typeAppExpr with
        | Some (EqHelperDispatchTypeApp (targetType, leftExpr, rightExpr)) ->
            let helperType = resolveType aliasReg targetType
            Call (eqHelperName helperType, NonEmptyList.fromList [recurse leftExpr; recurse rightExpr])
        | None ->
            TypeApp (funcName, typeArgs, NonEmptyList.map recurse args)
    | TupleLiteral elements ->
        TupleLiteral (List.map recurse elements)
    | TupleAccess (tupleExpr, index) ->
        TupleAccess (recurse tupleExpr, index)
    | DictLiteral (valueType, entries) ->
        DictLiteral (valueType, entries |> List.map (fun (key, value) -> (key, recurse value)))
    | RecordLiteral (typeName, fields) ->
        RecordLiteral (typeName, fields |> List.map (fun (name, fieldExpr) -> (name, recurse fieldExpr)))
    | RecordUpdate (recordExpr, updates) ->
        RecordUpdate (recurse recordExpr, updates |> List.map (fun (name, updateExpr) -> (name, recurse updateExpr)))
    | RecordAccess (recordExpr, fieldName) ->
        RecordAccess (recurse recordExpr, fieldName)
    | Constructor (typeName, variantName, payload) ->
        Constructor (typeName, variantName, payload |> Option.map recurse)
    | Match (scrutinee, cases) ->
        Match (
            recurse scrutinee,
            cases
            |> List.map (fun matchCase -> {
                matchCase with
                    Guard = matchCase.Guard |> Option.map recurse
                    Body = recurse matchCase.Body
            })
        )
    | ListLiteral elements ->
        ListLiteral (List.map recurse elements)
    | ListCons (headElements, tailExpr) ->
        ListCons (List.map recurse headElements, recurse tailExpr)
    | Lambda (parameters, returnAnnotation, body) ->
        Lambda (parameters, returnAnnotation, recurse body)
    | Apply (funcExpr, args) ->
        Apply (recurse funcExpr, NonEmptyList.map recurse args)
    | IndirectApply (funcExpr, args) ->
        IndirectApply (recurse funcExpr, NonEmptyList.map recurse args)
    | Closure (funcName, captures) ->
        Closure (funcName, List.map recurse captures)
    | InterpolatedString parts ->
        InterpolatedString (
            parts
            |> List.map (function
                | StringText text -> StringText text
                | StringExpr partExpr -> StringExpr (recurse partExpr))
        )

let materializeEqHelpersInTopLevels
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (topLevels: TopLevel list)
    : TopLevel list =
    let collectFromTopLevel (topLevel: TopLevel) : Set<Type> =
        match topLevel with
        | FunctionDef funcDef when List.isEmpty funcDef.TypeParams ->
            collectEqHelperTypesFromExpr aliasReg funcDef.Body
        | FunctionDef _ ->
            // Generic templates are retained for later specialization. Their
            // comparison plans are materialized in each concrete copy.
            Set.empty
        | Expression expr ->
            collectEqHelperTypesFromExpr aliasReg expr
        | TypeDef _ ->
            Set.empty

    let rewriteTopLevel (topLevel: TopLevel) : TopLevel =
        match topLevel with
        | FunctionDef funcDef when List.isEmpty funcDef.TypeParams ->
            FunctionDef { funcDef with Body = materializeEqHelperCallsInExpr aliasReg funcDef.Body }
        | FunctionDef _ ->
            topLevel
        | Expression expr ->
            Expression (materializeEqHelperCallsInExpr aliasReg expr)
        | TypeDef _ ->
            topLevel

    let helperTypes =
        topLevels
        |> List.map collectFromTopLevel
        |> List.fold Set.union Set.empty

    let rewrittenTopLevels = topLevels |> List.map rewriteTopLevel

    if Set.isEmpty helperTypes then
        rewrittenTopLevels
    else
        let initialState = {
            InProgress = Set.empty
            Generated = Map.empty
        }

        let finalState =
            helperTypes
            |> Set.toList
            |> List.sortBy typeToString
            |> List.fold
                (fun currentState helperType ->
                    ensureEqHelperForType aliasReg typeReg variantLookup helperType currentState)
                initialState

        let helperTopLevels =
            let existingFunctionNames =
                topLevels
                |> List.choose (function
                    | FunctionDef funcDef -> Some funcDef.Name
                    | _ -> None)
                |> Set.ofList
            finalState.Generated
            |> Map.toList
            |> List.map snd
            |> List.filter (fun helperDef -> not (Set.contains helperDef.Name existingFunctionNames))
            |> List.sortBy (fun helperDef -> helperDef.Name)
            |> List.map FunctionDef

        helperTopLevels @ rewrittenTopLevels

/// Type-check a function definition
/// Returns the transformed function body (with Call -> TypeApp transformations)
let private checkFunctionDef
    (funcParamNameReg: Map<string, string list>)
    (funcDef: FunctionDef)
    (env: TypeEnv)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (genericFuncReg: GenericFuncRegistry)
    (warningSettings: WarningSettings)
    (moduleRegistry: ModuleRegistry)
    (aliasReg: AliasRegistry)
    : Result<FunctionDef, TypeError> =
    let canonicalParams =
        funcDef.Params
        |> NonEmptyList.map (fun (name, typ) ->
            (name, canonicalizeBareSumTypeRefs variantLookup typ))

    let canonicalReturnType =
        canonicalizeBareSumTypeRefs variantLookup funcDef.ReturnType

    let canonicalFuncDef =
        { funcDef with
            Params = canonicalParams
            ReturnType = canonicalReturnType }

    // Build environment with parameters
    let paramEnv =
        canonicalParams
        |> NonEmptyList.toList
        |> List.fold (fun e (name, ty) -> Map.add name ty e) env

    // Check body has return type
    let bodyCheckResult =
        checkExprWithParamNames
            funcParamNameReg
            funcDef.Body
            paramEnv
            typeReg
            variantLookup
            genericFuncReg
            warningSettings
            moduleRegistry
            aliasReg
            (Some canonicalReturnType)

    let bodyCheckWithLegacyInterpreterErrors =
        if genericFuncReg.RequireExplicitTypeArgsForBareCalls then
            bodyCheckResult
            |> Result.mapError (fun err ->
                match err with
                | TypeMismatch (expectedType, actualType, _) when
                    typesCompatibleWithAliases aliasReg expectedType canonicalReturnType
                    && not (isRuntimeErrorType actualType) ->
                    let actualValue =
                        match tryFormatLiteralValue funcDef.Body with
                        | Some value -> value
                        | None -> typeToString actualType
                    GenericError
                        $"{funcDef.Name}'s return value expects {typeToString canonicalReturnType}, but got {typeToString actualType} ({actualValue})"
                | _ ->
                    err)
        else
            bodyCheckResult

    bodyCheckWithLegacyInterpreterErrors
    |> Result.bind (fun (bodyType, body') ->
        let resolvedReturnType = resolveType aliasReg canonicalReturnType
        let resolvedBodyType = resolveType aliasReg bodyType
        let allowGenericReturnSpecialization =
            containsTVar resolvedReturnType
            && not (containsTVar resolvedBodyType)
            && typesCompatibleWithAliases aliasReg resolvedReturnType resolvedBodyType

        if resolvedReturnType = resolvedBodyType || allowGenericReturnSpecialization then
            Ok { canonicalFuncDef with Body = body' }
        else
            Error (TypeMismatch (canonicalReturnType, bodyType, $"function {funcDef.Name} body")))

let private specializeFunctionForTypeCheck
    (funcDef: FunctionDef)
    (typeArgs: Type list)
    : Result<FunctionDef, TypeError> =
    match buildSubstitution funcDef.TypeParams typeArgs with
    | Error msg ->
        Error (GenericError msg)
    | Ok subst ->
        let specializedParams =
            funcDef.Params
            |> NonEmptyList.map (fun (name, typ) -> (name, applySubst subst typ))
        Ok
            { funcDef with
                TypeParams = []
                Params = specializedParams
                ReturnType = applySubst subst funcDef.ReturnType
                Body = applySubstToExpr subst funcDef.Body }

let rec private collectTypeAppSpecs (expr: Expr) : Set<string * Type list> =
    match expr with
    | BoundaryRender (_, value) -> collectTypeAppSpecs value
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | Closure _ | RuntimeError _ ->
        Set.empty
    | BinOp (_, left, right) ->
        Set.union (collectTypeAppSpecs left) (collectTypeAppSpecs right)
    | UnaryOp (_, inner) ->
        collectTypeAppSpecs inner
    | Let (_, value, body) ->
        Set.union (collectTypeAppSpecs value) (collectTypeAppSpecs body)
    | If (cond, thenBranch, elseBranch) ->
        Set.union (collectTypeAppSpecs cond) (Set.union (collectTypeAppSpecs thenBranch) (collectTypeAppSpecs elseBranch))
    | Sequence (first, next) ->
        Set.union (collectTypeAppSpecs first) (collectTypeAppSpecs next)
    | Call (_, args) ->
        args |> NonEmptyList.toList |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty
    | TypeApp (funcName, typeArgs, args) ->
        let argSpecs =
            args |> NonEmptyList.toList |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty
        Set.add (funcName, typeArgs) argSpecs
    | TupleLiteral elements ->
        elements |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty
    | TupleAccess (tuple, _) ->
        collectTypeAppSpecs tuple
    | DictLiteral (_, entries) ->
        entries |> List.map (snd >> collectTypeAppSpecs) |> List.fold Set.union Set.empty
    | RecordLiteral (_, fields) ->
        fields |> List.map (snd >> collectTypeAppSpecs) |> List.fold Set.union Set.empty
    | RecordUpdate (record, updates) ->
        Set.union
            (collectTypeAppSpecs record)
            (updates |> List.map (snd >> collectTypeAppSpecs) |> List.fold Set.union Set.empty)
    | RecordAccess (record, _) ->
        collectTypeAppSpecs record
    | Constructor (_, _, payload) ->
        payload |> Option.map collectTypeAppSpecs |> Option.defaultValue Set.empty
    | Match (scrutinee, cases) ->
        let scrutineeSpecs = collectTypeAppSpecs scrutinee
        let caseSpecs =
            cases
            |> List.map (fun matchCase ->
                Set.union
                    (matchCase.Guard |> Option.map collectTypeAppSpecs |> Option.defaultValue Set.empty)
                    (collectTypeAppSpecs matchCase.Body))
            |> List.fold Set.union Set.empty
        Set.union scrutineeSpecs caseSpecs
    | ListLiteral elements ->
        elements |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty
    | ListCons (headElements, tail) ->
        Set.union
            (headElements |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty)
            (collectTypeAppSpecs tail)
    | Lambda (_, _, body) ->
        collectTypeAppSpecs body
    | Apply (funcExpr, args)
    | IndirectApply (funcExpr, args) ->
        Set.union
            (collectTypeAppSpecs funcExpr)
            (args |> NonEmptyList.toList |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty)
    | InterpolatedString parts ->
        parts
        |> List.choose (fun part ->
            match part with
            | StringText _ -> None
            | StringExpr expr -> Some (collectTypeAppSpecs expr))
        |> List.fold Set.union Set.empty

/// Registries derivable directly from a program's top-level declarations.
type private TopLevelDeclarationSummary = {
    TypeReg: TypeRegistry
    AliasReg: AliasRegistry
    VariantLookup: VariantLookup
    FuncSigs: Map<string, Type list * Type>
    FuncParamNames: FuncParamNameRegistry
    GenericFuncs: Map<string, string list>
}

let private splitDeclaredName (name: string) : NameResolution.NamespaceIdentity * string =
    match NameResolution.tryQualifiedName name with
    | None -> (NameResolution.RootNamespace, name)
    | Some qualifiedName ->
        match NameResolution.qualifiedNameSegments qualifiedName |> List.rev with
        | terminal :: reversedNamespace ->
            match List.rev reversedNamespace |> NonEmptyList.tryFromList with
            | Some path -> (NameResolution.ModuleNamespace path, terminal)
            | None -> (NameResolution.RootNamespace, terminal)
        | [] -> Crash.crash "Qualified name contained no segments"

let private requiredCandidate visibleName identity provenance : NameResolution.Candidate =
    match NameResolution.candidate visibleName identity provenance with
    | Some candidate -> candidate
    | None -> Crash.crash $"Invalid compiler declaration name entered resolution inventory: {visibleName}"

let private declarationResolutionEnvironment
    (topLevels: TopLevel list)
    (moduleRegistry: ModuleRegistry)
    (includeIntrinsicCatalog: bool)
    : NameResolution.ResolutionEnvironment =
    let sourceFunctionNames =
        topLevels
        |> List.choose (function FunctionDef funcDef -> Some funcDef.Name | _ -> None)
        |> Set.ofList
    let registeredFunctionNames =
        Set.union sourceFunctionNames (moduleRegistry |> Map.keys |> Set.ofSeq)
    let visibleFunctionNames (qualifiedName: string) =
        let versionedName = $"{qualifiedName}_v0"
        if qualifiedName.EndsWith("_v0") || Set.contains versionedName registeredFunctionNames then
            [qualifiedName]
        else
            [qualifiedName; versionedName]
    let sourceCandidates =
        topLevels
        |> List.indexed
        |> List.collect (fun (declarationIndex, topLevel) ->
            let declarationId name = $"source:{declarationIndex}:{name}"
            match topLevel with
            | FunctionDef funcDef ->
                let (namespaceIdentity, terminal) = splitDeclaredName funcDef.Name
                let identity =
                    NameResolution.ModuleFunction (
                        namespaceIdentity,
                        terminal,
                        declarationId funcDef.Name
                    )
                visibleFunctionNames funcDef.Name
                |> List.map (fun visibleName ->
                    requiredCandidate
                        visibleName
                        identity
                        (NameResolution.SourceDeclaration funcDef.Name))
            | TypeDef typeDef ->
                let (typeName, variants) =
                    match typeDef with
                    | RecordDef (name, _, _) -> (name, [])
                    | TypeAlias (name, _, _) -> (name, [])
                    | SumTypeDef (name, _, variants) -> (name, variants)
                let typeCandidate =
                    requiredCandidate
                        typeName
                        (NameResolution.UserType typeName)
                        (NameResolution.SourceDeclaration typeName)
                let constructorCandidates =
                    variants
                    |> List.collect (fun variant ->
                        let identity =
                            NameResolution.ConstructorSymbol (typeName, variant.Name)
                        [ requiredCandidate
                            variant.Name
                            identity
                            (NameResolution.SourceDeclaration $"{typeName}.{variant.Name}")
                          requiredCandidate
                            $"{typeName}.{variant.Name}"
                            identity
                            (NameResolution.SourceDeclaration $"{typeName}.{variant.Name}") ])
                typeCandidate :: constructorCandidates
            | Expression _ -> [])

    let intrinsicCandidates =
        moduleRegistry
        |> Map.toList
        |> List.filter (fun (qualifiedName, _) ->
            includeIntrinsicCatalog && not (Set.contains qualifiedName sourceFunctionNames))
        |> List.collect (fun (qualifiedName, _) ->
            let (namespaceIdentity, terminal) = splitDeclaredName qualifiedName
            let identity =
                NameResolution.ModuleFunction (
                    namespaceIdentity,
                    terminal,
                    $"intrinsic:{qualifiedName}"
                )
            visibleFunctionNames qualifiedName
            |> List.map (fun visibleName ->
                requiredCandidate
                    visibleName
                    identity
                    (NameResolution.CompilerExtension qualifiedName)))

    let stdlibValueCandidates =
        Stdlib.allValues
        |> List.map (fun value ->
            let (namespaceIdentity, terminal) = splitDeclaredName value.Name
            requiredCandidate
                value.Name
                (NameResolution.ModuleValue (namespaceIdentity, terminal))
                (NameResolution.ModuleDeclaration value.Name))

    let builtinCandidates =
        [ requiredCandidate
            "Builtin.unwrap"
            (NameResolution.BuiltinFunction ("unwrap", 0))
            (NameResolution.BuiltinRegistration "Builtin.unwrap")
          requiredCandidate
            "Builtin.testRuntimeError"
            (NameResolution.BuiltinFunction ("testRuntimeError", 0))
            (NameResolution.BuiltinRegistration "Builtin.testRuntimeError")
          requiredCandidate
            "Builtin.testNan"
            (NameResolution.BuiltinValue ("testNan", 0))
            (NameResolution.BuiltinRegistration "Builtin.testNan")
          requiredCandidate
            "Builtin.testNan_v0"
            (NameResolution.BuiltinValue ("testNan", 0))
            (NameResolution.BuiltinRegistration "Builtin.testNan") ]

    NameResolution.empty
    |> NameResolution.addCandidates (sourceCandidates @ intrinsicCandidates @ stdlibValueCandidates @ builtinCandidates)

let private resolveProgramNames
    (resolutionEnv: NameResolution.ResolutionEnvironment)
    (program: Program)
    : Result<Program, TypeError> =
    let resolveName context localNames spelling =
        let environment =
            localNames
            |> Set.toList
            |> List.fold (fun env localName ->
                requiredCandidate
                    localName
                    (NameResolution.LocalValue localName)
                    (NameResolution.LexicalBinding localName)
                |> fun candidate -> NameResolution.addCandidate candidate env) resolutionEnv
        NameResolution.resolve context spelling environment
        |> Result.map (fun resolution -> NameResolution.canonicalSpelling resolution.Identity)
        |> Result.mapError ResolutionFailure

    let rec resolveTypeRefs typ =
        let recurse = resolveTypeRefs
        let resolveNamed makeType name typeArgs =
            resolveName NameResolution.ResolutionContext.Type Set.empty name
            |> Result.bind (fun resolvedName ->
                ResultList.traverse recurse typeArgs
                |> Result.map (makeType resolvedName))
        match typ with
        | TRecord (name, typeArgs) -> resolveNamed (fun n args -> TRecord (n, args)) name typeArgs
        | TSum (name, typeArgs) -> resolveNamed (fun n args -> TSum (n, args)) name typeArgs
        | TFunction (parameterTypes, returnType) ->
            ResultList.traverse recurse parameterTypes
            |> Result.bind (fun parameters' -> recurse returnType |> Result.map (fun ret -> TFunction (parameters', ret)))
        | TTuple elementTypes -> ResultList.traverse recurse elementTypes |> Result.map TTuple
        | TEnumFields fieldTypes -> ResultList.traverse recurse fieldTypes |> Result.map TEnumFields
        | TList elementType -> recurse elementType |> Result.map TList
        | TDict (keyType, valueType) ->
            recurse keyType
            |> Result.bind (fun key' -> recurse valueType |> Result.map (fun value' -> TDict (key', value')))
        | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
        | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
        | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr -> Ok typ

    let rec patternBoundNames pattern =
        match pattern with
        | PVar name -> Set.singleton name
        | PConstructor (_, payload) -> payload |> Option.map patternBoundNames |> Option.defaultValue Set.empty
        | PRecord (_, fields) -> fields |> List.map (snd >> patternBoundNames) |> Set.unionMany
        | PTuple patterns | PList patterns -> patterns |> List.map patternBoundNames |> Set.unionMany
        | PListCons (heads, tail) -> Set.union (heads |> List.map patternBoundNames |> Set.unionMany) (patternBoundNames tail)
        | PUnit | PWildcard | PInt64 _ | PInt128Literal _ | PInt8Literal _ | PInt16Literal _
        | PInt32Literal _ | PUInt8Literal _ | PUInt16Literal _ | PUInt32Literal _ | PUInt64Literal _
        | PUInt128Literal _ | PBool _ | PString _ | PChar _ | PFloat _ -> Set.empty

    let rec resolvePattern localNames pattern =
        let recurse = resolvePattern localNames
        match pattern with
        | PConstructor (name, payload) ->
            // Pattern constructor identity is selected against the scrutinee's
            // sum type by the pattern checker; equal case names in other types
            // are therefore not an ambiguity at this syntax-only traversal.
            payload
            |> Option.map recurse
            |> ResultList.sequenceOption
            |> Result.map (fun payload' -> PConstructor (name, payload'))
        | PRecord (typeName, fields) ->
            resolveName NameResolution.ResolutionContext.Type localNames typeName
            |> Result.bind (fun resolvedTypeName ->
                fields
                |> ResultList.traverse (fun (field, fieldPattern) -> recurse fieldPattern |> Result.map (fun p -> (field, p)))
                |> Result.map (fun fields' -> PRecord (resolvedTypeName, fields')))
        | PTuple patterns -> ResultList.traverse recurse patterns |> Result.map PTuple
        | PList patterns -> ResultList.traverse recurse patterns |> Result.map PList
        | PListCons (heads, tail) ->
            ResultList.traverse recurse heads
            |> Result.bind (fun heads' -> recurse tail |> Result.map (fun tail' -> PListCons (heads', tail')))
        | _ -> Ok pattern

    let rec resolveExpr localNames expr =
        let recurse = resolveExpr localNames
        let resolveArgs args = ResultList.traverse recurse (NonEmptyList.toList args) |> Result.map NonEmptyList.fromList
        match expr with
        | Var name ->
            resolveName NameResolution.ResolutionContext.Value localNames name
            |> Result.map Var
        | Call (name, args) ->
            resolveName NameResolution.ResolutionContext.Callable localNames name
            |> Result.bind (fun resolvedName -> resolveArgs args |> Result.map (fun args' -> Call (resolvedName, args')))
        | TypeApp (name, typeArgs, args) ->
            resolveName NameResolution.ResolutionContext.Callable localNames name
            |> Result.bind (fun resolvedName ->
                ResultList.traverse resolveTypeRefs typeArgs
                |> Result.bind (fun types' -> resolveArgs args |> Result.map (fun args' -> TypeApp (resolvedName, types', args'))))
        | FuncRef name -> resolveName NameResolution.ResolutionContext.Callable localNames name |> Result.map FuncRef
        | Constructor (constructorReference, variantName, payload) ->
            let spelling =
                match constructorReferenceTypeName constructorReference with
                | None -> variantName
                | Some typeName -> $"{typeName}.{variantName}"
            resolveName NameResolution.ResolutionContext.Constructor localNames spelling
            |> Result.bind (fun resolvedName ->
                let segments = resolvedName.Split('.') |> Array.toList
                match List.rev segments with
                | caseName :: reversedTypeName ->
                    let resolvedTypeName = reversedTypeName |> List.rev |> String.concat "."
                    payload
                    |> Option.map recurse
                    |> ResultList.sequenceOption
                    |> Result.map (fun payload' ->
                        Constructor (resolvedConstructorReference resolvedTypeName, caseName, payload'))
                | [] -> Error (GenericError "Resolved constructor name contained no segments"))
        | Let (pattern, value, body) ->
            recurse value
            |> Result.bind (fun value' ->
                let bindings = letPatternBindings pattern |> Set.ofList
                resolveExpr (Set.union localNames bindings) body
                |> Result.map (fun body' -> Let (pattern, value', body')))
        | Lambda (parameters, returnAnnotation, body) ->
            let resolveOptionalType = function
                | None -> Ok None
                | Some typ -> resolveTypeRefs typ |> Result.map Some
            parameters
            |> NonEmptyList.toList
            |> ResultList.traverse (fun parameter ->
                resolveOptionalType parameter.SourceAnnotation
                |> Result.bind (fun sourceAnnotation ->
                    resolveOptionalType parameter.InferredType
                    |> Result.map (fun inferredType ->
                        { parameter with SourceAnnotation = sourceAnnotation; InferredType = inferredType })))
            |> Result.bind (fun parameters' ->
                let parameterNames =
                    parameters'
                    |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
                    |> Set.ofList
                resolveOptionalType returnAnnotation
                |> Result.bind (fun returnAnnotation' ->
                    resolveExpr (Set.union localNames parameterNames) body
                    |> Result.map (fun body' -> Lambda (NonEmptyList.fromList parameters', returnAnnotation', body'))))
        | Match (scrutinee, cases) ->
            recurse scrutinee
            |> Result.bind (fun scrutinee' ->
                cases
                |> ResultList.traverse (fun matchCase ->
                    let patterns = NonEmptyList.toList matchCase.Patterns
                    ResultList.traverse (resolvePattern localNames) patterns
                    |> Result.bind (fun patterns' ->
                        let bindings = patterns |> List.map patternBoundNames |> Set.unionMany
                        let caseLocals = Set.union localNames bindings
                        matchCase.Guard
                        |> Option.map (resolveExpr caseLocals)
                        |> ResultList.sequenceOption
                        |> Result.bind (fun guard' ->
                            resolveExpr caseLocals matchCase.Body
                            |> Result.map (fun body' ->
                                { Patterns = NonEmptyList.fromList patterns'; Guard = guard'; Body = body' }))))
                |> Result.map (fun cases' -> Match (scrutinee', cases')))
        | RecordLiteral (typeName, fields) ->
            let resolvedTypeNameResult =
                if typeName = "" then Ok "" else resolveName NameResolution.ResolutionContext.Type localNames typeName
            resolvedTypeNameResult
            |> Result.bind (fun resolvedTypeName ->
                fields
                |> ResultList.traverse (fun (field, value) -> recurse value |> Result.map (fun value' -> (field, value')))
                |> Result.map (fun fields' -> RecordLiteral (resolvedTypeName, fields')))
        | DictLiteral (valueType, entries) ->
            entries
            |> ResultList.traverse (fun (key, value) -> recurse value |> Result.map (fun value' -> (key, value')))
            |> Result.map (fun entries' -> DictLiteral (valueType, entries'))
        | BoundaryRender (renderer, value) -> recurse value |> Result.map (fun value' -> BoundaryRender (renderer, value'))
        | BinOp (op, left, right) -> recurse left |> Result.bind (fun l -> recurse right |> Result.map (fun r -> BinOp (op, l, r)))
        | UnaryOp (op, inner) -> recurse inner |> Result.map (fun inner' -> UnaryOp (op, inner'))
        | If (condition, thenBranch, elseBranch) ->
            recurse condition |> Result.bind (fun c -> recurse thenBranch |> Result.bind (fun t -> recurse elseBranch |> Result.map (fun e -> If (c, t, e))))
        | Sequence (first, next) ->
            recurse first
            |> Result.bind (fun first' -> recurse next |> Result.map (fun next' -> Sequence (first', next')))
        | InterpolatedString parts ->
            parts
            |> ResultList.traverse (function StringText text -> Ok (StringText text) | StringExpr e -> recurse e |> Result.map StringExpr)
            |> Result.map InterpolatedString
        | TupleLiteral elements -> ResultList.traverse recurse elements |> Result.map TupleLiteral
        | TupleAccess (tuple, index) -> recurse tuple |> Result.map (fun tuple' -> TupleAccess (tuple', index))
        | RecordUpdate (record, updates) ->
            recurse record
            |> Result.bind (fun record' -> updates |> ResultList.traverse (fun (field, value) -> recurse value |> Result.map (fun value' -> (field, value'))) |> Result.map (fun updates' -> RecordUpdate (record', updates')))
        | RecordAccess (record, fieldName) -> recurse record |> Result.map (fun record' -> RecordAccess (record', fieldName))
        | ListLiteral elements -> ResultList.traverse recurse elements |> Result.map ListLiteral
        | ListCons (heads, tail) -> ResultList.traverse recurse heads |> Result.bind (fun heads' -> recurse tail |> Result.map (fun tail' -> ListCons (heads', tail')))
        | Apply (func, args) -> recurse func |> Result.bind (fun func' -> resolveArgs args |> Result.map (fun args' -> Apply (func', args')))
        | IndirectApply (func, args) ->
            recurse func
            |> Result.bind (fun func' -> resolveArgs args |> Result.map (fun args' -> IndirectApply (func', args')))
        | Closure (name, captures) ->
            resolveName NameResolution.ResolutionContext.Callable localNames name
            |> Result.bind (fun resolvedName -> ResultList.traverse recurse captures |> Result.map (fun captures' -> Closure (resolvedName, captures')))
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
        | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
        | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | RuntimeError _ -> Ok expr

    let resolveTypeDef typeDef =
        match typeDef with
        | RecordDef (name, typeParams, fields) ->
            fields |> ResultList.traverse (fun (field, typ) -> resolveTypeRefs typ |> Result.map (fun typ' -> (field, typ'))) |> Result.map (fun fields' -> RecordDef (name, typeParams, fields'))
        | SumTypeDef (name, typeParams, variants) ->
            variants |> ResultList.traverse (fun variant -> variant.Payload |> Option.map resolveTypeRefs |> ResultList.sequenceOption |> Result.map (fun payload -> { variant with Payload = payload })) |> Result.map (fun variants' -> SumTypeDef (name, typeParams, variants'))
        | TypeAlias (name, typeParams, targetType) -> resolveTypeRefs targetType |> Result.map (fun target' -> TypeAlias (name, typeParams, target'))

    let resolveTopLevel topLevel =
        match topLevel with
        | FunctionDef funcDef ->
            let parameters = NonEmptyList.toList funcDef.Params
            parameters
            |> ResultList.traverse (fun (name, typ) -> resolveTypeRefs typ |> Result.map (fun typ' -> (name, typ')))
            |> Result.bind (fun parameters' ->
                resolveTypeRefs funcDef.ReturnType
                |> Result.bind (fun returnType' ->
                    let locals = parameters' |> List.map fst |> Set.ofList
                    resolveExpr locals funcDef.Body
                    |> Result.map (fun body' -> FunctionDef { funcDef with Params = NonEmptyList.fromList parameters'; ReturnType = returnType'; Body = body' })))
        | TypeDef typeDef -> resolveTypeDef typeDef |> Result.map TypeDef
        | Expression expr -> resolveExpr Set.empty expr |> Result.map Expression

    let (Program topLevels) = program
    ResultList.traverse resolveTopLevel topLevels |> Result.map Program

let private typeDefName (typeDef: TypeDef) : string =
    match typeDef with
    | RecordDef (name, _, _)
    | SumTypeDef (name, _, _)
    | TypeAlias (name, _, _) -> name

let private typeDefTypeParams (typeDef: TypeDef) : string list =
    match typeDef with
    | RecordDef (_, typeParams, _)
    | SumTypeDef (_, typeParams, _)
    | TypeAlias (_, typeParams, _) -> typeParams

/// Validate the nominal declaration namespace before building lookup maps.
/// Every type name is visible during this pure validation phase, which permits
/// recursive references without allowing later declarations to overwrite an
/// earlier identity.
let private validateTopLevelTypeDeclarations
    (baseEnv: TypeCheckEnv option)
    (topLevels: TopLevel list)
    : Result<unit, TypeError> =
    let typeDefs =
        topLevels
        |> List.choose (function
            | TypeDef typeDef -> Some typeDef
            | _ -> None)

    let baseTypeArities =
        match baseEnv with
        | None -> Map.empty
        | Some env ->
            let recordArities =
                env.IndexedTypeReg
                |> Map.map (fun _ info -> List.length info.TypeParams)
            let aliasArities =
                env.AliasReg
                |> Map.map (fun _ (typeParams, _) -> List.length typeParams)
            let namedArities =
                Map.fold (fun acc name arity -> Map.add name arity acc) recordArities aliasArities
            env.VariantLookup
            |> Map.fold (fun acc _ (typeName, typeParams, _, _) ->
                Map.add typeName (List.length typeParams) acc) namedArities

    let typeArities =
        typeDefs
        |> List.fold (fun arities typeDef ->
            Map.add (typeDefName typeDef) (List.length (typeDefTypeParams typeDef)) arities) baseTypeArities

    let rec validateTypeReference (owner: string) (typ: Type) : Result<unit, TypeError> =
        let validateAll types =
            types
            |> List.fold (fun result item ->
                result |> Result.bind (fun () -> validateTypeReference owner item)) (Ok ())

        match typ with
        | TRecord (name, typeArgs)
        | TSum (name, typeArgs) ->
            match Map.tryFind name typeArities with
            | None -> Error (GenericError $"Unknown type reference: {name} in {owner}")
            | Some expectedArity when expectedArity <> List.length typeArgs ->
                Error (
                    GenericError
                        $"Type argument arity mismatch: {name} expects {expectedArity}, got {List.length typeArgs} in {owner}"
                )
            | Some _ -> validateAll typeArgs
        | TFunction (parameters, result) -> validateAll (parameters @ [result])
        | TTuple types
        | TEnumFields types -> validateAll types
        | TList element -> validateTypeReference owner element
        | TDict (key, value) -> validateAll [key; value]
        | TVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
        | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
        | TBool | TFloat64 | TString | TBlob | TChar | TDateTime | TUnit | TRuntimeError | TRawPtr -> Ok ()

    let duplicateTypeName =
        typeDefs
        |> List.map typeDefName
        |> List.countBy id
        |> List.tryPick (fun (name, count) -> if count > 1 then Some name else None)

    match duplicateTypeName with
    | Some name ->
        Error (GenericError $"Duplicate type declaration: {name}")
    | None ->
        let collidingCaseNames = collidingConstructorCaseNames typeDefs
        let constructorIdentityCollision =
            typeDefs
            |> List.collect (function
                | SumTypeDef (typeName, _, variants) ->
                    variants
                    |> List.filter (fun variant -> Set.contains variant.Name collidingCaseNames)
                    |> List.map (fun variant ->
                        (constructorRuntimeIdentity typeName variant.Name, $"{typeName}.{variant.Name}"))
                | _ -> [])
            |> List.groupBy fst
            |> List.tryPick (fun (identity, entries) ->
                match entries |> List.map snd |> List.distinct with
                | _ :: _ :: _ as names -> Some (identity, names)
                | _ -> None)

        match constructorIdentityCollision with
        | Some (identity, names) ->
            let joinedNames = String.concat ", " names
            Error (
                GenericError
                    $"Constructor identity collision {identity}: {joinedNames}"
            )
        | None ->
        let rec validate remaining =
            match remaining with
            | [] -> Ok ()
            | typeDef :: rest ->
                let typeName = typeDefName typeDef
                let duplicateTypeParam =
                    typeDefTypeParams typeDef
                    |> List.countBy id
                    |> List.tryPick (fun (name, count) -> if count > 1 then Some name else None)

                match duplicateTypeParam with
                | Some param ->
                    Error (GenericError $"Duplicate type parameter: {param} in {typeName}")
                | None ->
                    let declaredTypeParams = typeDefTypeParams typeDef |> Set.ofList
                    let referencedTypeParams =
                        match typeDef with
                        | RecordDef (_, _, fields) ->
                            fields
                            |> List.fold (fun acc (_, fieldType) -> collectTypeVarsInType fieldType acc) []
                        | SumTypeDef (_, _, variants) ->
                            variants
                            |> List.fold (fun acc variant ->
                                match variant.Payload with
                                | Some payloadType -> collectTypeVarsInType payloadType acc
                                | None -> acc) []
                        | TypeAlias (_, _, targetType) ->
                            collectTypeVarsInType targetType []
                    let undeclaredTypeParam =
                        referencedTypeParams
                        |> List.tryFind (fun name -> not (Set.contains name declaredTypeParams))

                    match undeclaredTypeParam with
                    | Some param ->
                        Error (GenericError $"Undeclared type parameter: '{param} in {typeName}")
                    | None ->
                    let referencedTypes =
                        match typeDef with
                        | RecordDef (_, _, fields) -> List.map snd fields
                        | SumTypeDef (_, _, variants) -> variants |> List.choose (fun variant -> variant.Payload)
                        | TypeAlias (_, _, targetType) -> [targetType]
                    let referenceResult =
                        referencedTypes
                        |> List.fold (fun result referencedType ->
                            result
                            |> Result.bind (fun () -> validateTypeReference typeName referencedType)) (Ok ())
                    let declarationResult =
                        match typeDef with
                        | RecordDef (_, _, []) ->
                            Error (GenericError $"Record declaration must contain at least one field: {typeName}")
                        | RecordDef (_, _, fields) ->
                            fields
                            |> List.map fst
                            |> List.countBy id
                            |> List.tryPick (fun (name, count) -> if count > 1 then Some name else None)
                            |> function
                                | Some fieldName ->
                                    Error (GenericError $"Duplicate record field declaration: {typeName}.{fieldName}")
                                | None -> Ok ()
                        | SumTypeDef (_, _, []) ->
                            Error (GenericError $"Enum declaration must contain at least one case: {typeName}")
                        | SumTypeDef (_, _, variants) ->
                            variants
                            |> List.map (fun variant -> variant.Name)
                            |> List.countBy id
                            |> List.tryPick (fun (name, count) -> if count > 1 then Some name else None)
                            |> function
                                | Some caseName ->
                                    Error (GenericError $"Duplicate constructor declaration: {typeName}.{caseName}")
                                | None -> Ok ()
                        | TypeAlias _ -> Ok ()
                    referenceResult
                    |> Result.bind (fun () -> declarationResult)
                    |> Result.bind (fun () -> validate rest)

        validate typeDefs

/// Build all declaration registries after validation and name resolution have
/// established unique nominal type and constructor identities.
let private summarizeTopLevelDeclarations
    (topLevels: TopLevel list)
    : TopLevelDeclarationSummary =
    let empty : TopLevelDeclarationSummary = {
        TypeReg = Map.empty
        AliasReg = Map.empty
        VariantLookup = Map.empty
        FuncSigs = Map.empty
        FuncParamNames = Map.empty
        GenericFuncs = Map.empty
    }

    let typeDefs =
        topLevels
        |> List.choose (function | TypeDef typeDef -> Some typeDef | _ -> None)
    let collidingCaseNames = collidingConstructorCaseNames typeDefs

    topLevels
    |> List.fold (fun summary topLevel ->
        match topLevel with
        | TypeDef (RecordDef (name, _typeParams, fields)) ->
            { summary with TypeReg = Map.add name fields summary.TypeReg }
        | TypeDef (TypeAlias (name, typeParams, targetType)) ->
            { summary with AliasReg = Map.add name (typeParams, targetType) summary.AliasReg }
        | TypeDef (SumTypeDef (typeName, typeParams, variants)) ->
            let variantLookup =
                variants
                |> List.indexed
                |> List.fold (fun lookup (ordinal, variant) ->
                    let tag =
                        if Set.contains variant.Name collidingCaseNames then
                            constructorRuntimeIdentity typeName variant.Name
                        else
                            ordinal
                    let info = (typeName, typeParams, tag, variant.Payload)
                    lookup
                    |> Map.add variant.Name info
                    |> Map.add $"{typeName}.{variant.Name}" info) summary.VariantLookup
            { summary with VariantLookup = variantLookup }
        | FunctionDef funcDef ->
            let parameters = NonEmptyList.toList funcDef.Params
            let genericFuncs =
                match funcDef.TypeParams with
                | [] -> summary.GenericFuncs
                | typeParams -> Map.add funcDef.Name typeParams summary.GenericFuncs
            {
                summary with
                    FuncSigs =
                        Map.add
                            funcDef.Name
                            (List.map snd parameters, funcDef.ReturnType)
                            summary.FuncSigs
                    FuncParamNames =
                        Map.add funcDef.Name (List.map fst parameters) summary.FuncParamNames
                    GenericFuncs = genericFuncs
            }
        | Expression _ ->
            summary) empty

/// Internal: Type-check a program and return the type checking environment
/// This is the core implementation used by checkProgram, checkProgramWithEnv, and checkProgramWithBaseEnv
/// When baseEnv is provided, registries are merged with it (for separate compilation)
let private checkResolvedProgramInternal
    (baseEnv: TypeCheckEnv option)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<Type * Program * TypeCheckEnv, TypeError> =
    let (Program topLevels) = program
    let declarationSummary = summarizeTopLevelDeclarations topLevels

    // Build environment with function signatures from THIS program
    let programFuncEnv =
        declarationSummary.FuncSigs
        |> Map.map (fun _ (paramTypes, returnType) ->
            TFunction (
                List.map (canonicalizeBareSumTypeRefs declarationSummary.VariantLookup) paramTypes,
                canonicalizeBareSumTypeRefs declarationSummary.VariantLookup returnType
            ))

    let programGenericFuncReg : GenericFuncRegistry = {
        Functions = declarationSummary.GenericFuncs
        RequireExplicitTypeArgsForBareCalls = requireExplicitTypeArgsForBareCalls
    }

    // Build module registry once (or reuse from base environment)
    let moduleRegistry =
        match baseEnv with
        | Some existingEnv -> existingEnv.ModuleRegistry
        | None -> Stdlib.buildModuleRegistry ()

    let programResolutionEnv =
        declarationResolutionEnvironment topLevels moduleRegistry (Option.isNone baseEnv)

    let programTypeReg =
        resolveAliasesInTypeRegistry declarationSummary.AliasReg declarationSummary.TypeReg

    let recordVariantLookup =
        match baseEnv with
        | Some existingEnv ->
            Map.fold
                (fun lookup name variant -> Map.add name variant lookup)
                existingEnv.VariantLookup
                declarationSummary.VariantLookup
        | None -> declarationSummary.VariantLookup

    // Build the type check environment for THIS program
    let programEnv : TypeCheckEnv = {
        TypeReg = programTypeReg
        IndexedTypeReg = canonicalizeTypeRegistry recordVariantLookup programTypeReg
        VariantLookup = declarationSummary.VariantLookup
        FuncEnv = programFuncEnv
        FuncParamNames = declarationSummary.FuncParamNames
        GenericFuncReg = programGenericFuncReg
        ModuleRegistry = moduleRegistry
        AliasReg = declarationSummary.AliasReg
        ResolutionEnv = programResolutionEnv
    }

    // Merge with base environment if provided (for separate compilation)
    let typeCheckEnv =
        match baseEnv with
        | Some existingEnv -> mergeTypeCheckEnv existingEnv programEnv
        | None -> programEnv

    // Extract the merged registries for use in type checking
    let variantLookup = typeCheckEnv.VariantLookup
    let typeReg = typeCheckEnv.IndexedTypeReg
    let funcEnv = typeCheckEnv.FuncEnv
    let funcParamNameReg = typeCheckEnv.FuncParamNames
    let genericFuncReg = typeCheckEnv.GenericFuncReg
    let mergedAliasReg = typeCheckEnv.AliasReg

    // Third pass: type check all function definitions and collect transformed top-levels
    // The accumulator contains (type option * TopLevel) pairs where the type is Some for expressions
    let rec checkAllTopLevelsWithTypes remaining accTopLevels =
        match remaining with
        | [] -> Ok (List.rev accTopLevels)
        | topLevel :: rest ->
            match topLevel with
            | FunctionDef funcDef ->
                checkFunctionDef
                    funcParamNameReg
                    funcDef
                    funcEnv
                    typeReg
                    variantLookup
                    genericFuncReg
                    warningSettings
                    moduleRegistry
                    mergedAliasReg
                |> Result.bind (fun funcDef' ->
                    checkAllTopLevelsWithTypes rest ((None, FunctionDef funcDef') :: accTopLevels))
            | TypeDef _ ->
                checkAllTopLevelsWithTypes rest ((None, topLevel) :: accTopLevels)
            | Expression expr ->
                checkExprWithParamNames
                    funcParamNameReg
                    expr
                    funcEnv
                    typeReg
                    variantLookup
                    genericFuncReg
                    warningSettings
                    moduleRegistry
                    mergedAliasReg
                    None
                |> Result.bind (fun (exprType, expr') ->
                    checkAllTopLevelsWithTypes rest ((Some exprType, Expression expr') :: accTopLevels))

    // Type check all top-levels
    checkAllTopLevelsWithTypes topLevels []
    |> Result.bind (fun topLevelsWithTypes ->
        // Extract just the top-levels
        let topLevels' = topLevelsWithTypes |> List.map snd
        let localGenericFuncDefs =
            topLevels'
            |> List.choose (function
                | FunctionDef funcDef when not (List.isEmpty funcDef.TypeParams) ->
                    Some (funcDef.Name, funcDef)
                | _ ->
                    None)
            |> Map.ofList

        let localExplicitSpecs =
            topLevels'
            |> List.map (function
                | FunctionDef funcDef when List.isEmpty funcDef.TypeParams ->
                    collectTypeAppSpecs funcDef.Body
                | Expression expr ->
                    collectTypeAppSpecs expr
                | _ ->
                    Set.empty)
            |> List.fold Set.union Set.empty
            |> Set.filter (fun (funcName, _typeArgs) -> Map.containsKey funcName localGenericFuncDefs)

        let validateLocalSpecialization (funcName, typeArgs) =
            match Map.tryFind funcName localGenericFuncDefs with
            | None ->
                Ok ()
            | Some funcDef ->
                specializeFunctionForTypeCheck funcDef typeArgs
                |> Result.bind (fun specializedFunc ->
                    checkFunctionDef
                        funcParamNameReg
                        specializedFunc
                        funcEnv
                        typeReg
                        variantLookup
                        genericFuncReg
                        warningSettings
                        moduleRegistry
                        mergedAliasReg
                    |> Result.map (fun _ -> ()))

        let validateAllSpecializations specs =
            specs
            |> Set.toList
            |> List.fold
                (fun acc spec ->
                    acc |> Result.bind (fun () -> validateLocalSpecialization spec))
                (Ok ())

        validateAllSpecializations localExplicitSpecs
        |> Result.bind (fun () ->
            let topLevelsWithEqHelpers =
                materializeEqHelpersInTopLevels mergedAliasReg typeReg variantLookup topLevels'
            // Find the type of the main expression (if any)
            let mainExprType = topLevelsWithTypes |> List.tryPick (function (Some t, Expression _) -> Some t | _ -> None)
            match mainExprType with
            | Some typ ->
                // We have a main expression with its type - no need to re-check
                Ok (typ, Program topLevelsWithEqHelpers, typeCheckEnv)
            | None ->
                // No main expression - just functions
                // For now, require a "main" function with signature () -> int
                match Map.tryFind "main" declarationSummary.FuncSigs with
                | Some ([], TInt64) -> Ok (TInt64, Program topLevelsWithEqHelpers, typeCheckEnv)
                | Some _ -> Error (GenericError "main function must have signature () -> int")
                | None -> Error (GenericError "Program must have either a main expression or a main() : int function")))

let private checkProgramInternal
    (baseEnv: TypeCheckEnv option)
    (requireExplicitTypeArgsForBareCalls: bool)
    (validateDeclarations: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<Type * Program * TypeCheckEnv, TypeError> =
    let (Program topLevels) = program
    let declarationValidation =
        if validateDeclarations then
            validateTopLevelTypeDeclarations baseEnv topLevels
        else
            // Only synthetic test preambles skip this: they concatenate
            // declarations that do not coexist in an original source unit.
            Ok ()
    let moduleRegistry =
        match baseEnv with
        | Some existingEnv -> existingEnv.ModuleRegistry
        | None -> Stdlib.buildModuleRegistry ()
    let localResolutionEnv =
        declarationResolutionEnvironment topLevels moduleRegistry (Option.isNone baseEnv)
    let resolutionEnv =
        match baseEnv with
        | Some existingEnv -> NameResolution.merge existingEnv.ResolutionEnv localResolutionEnv
        | None -> localResolutionEnv

    declarationValidation
    |> Result.bind (fun () ->
        resolveProgramNames resolutionEnv program)
    |> Result.bind (checkResolvedProgramInternal baseEnv requireExplicitTypeArgsForBareCalls warningSettings)

/// Type-check a program
/// Returns the type of the main expression and the transformed program
/// The transformed program has Call nodes converted to TypeApp where type inference was applied
let checkProgram (program: Program) : Result<Type * Program, TypeError> =
    checkProgramInternal None false true AST.defaultWarningSettings program
    |> Result.map (fun (typ, prog, _env) -> (typ, prog))

/// Type-check the public interpreter syntax policy without a base environment.
/// Used by focused declaration tests and tools that already parsed an isolated
/// interpreter program.
let checkInterpreterProgram (program: Program) : Result<Type * Program, TypeError> =
    checkProgramInternal None true true AST.defaultWarningSettings program
    |> Result.map (fun (typ, prog, _env) -> (typ, prog))

/// Type-check a program and return the type checking environment
/// Use this when you need to reuse the environment (e.g., for stdlib caching)
let checkProgramWithEnv (program: Program) : Result<Type * Program * TypeCheckEnv, TypeError> =
    checkProgramInternal None false true AST.defaultWarningSettings program

/// Type-check a program with a pre-populated base environment (for separate compilation)
/// The program's definitions are merged with the base environment, allowing lookups
/// of types/functions from both the base (e.g., stdlib) and the program (e.g., user code)
let checkProgramWithBaseEnv (baseEnv: TypeCheckEnv) (program: Program) : Result<Type * Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) false true AST.defaultWarningSettings program

/// Type-check a program with a pre-populated base environment, generic-call policy override,
/// and warning compatibility settings from the compiler driver.
let checkProgramWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<Type * Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) requireExplicitTypeArgsForBareCalls true warningSettings program

/// Analyze a synthetic preamble assembled from otherwise independent tests.
/// Such preambles can repeat declarations that never coexist in a source
/// program, so declaration-namespace validation belongs to each original
/// source rather than this harness artifact.
let checkSyntheticPreambleWithBaseEnvAndSettings
    (baseEnv: TypeCheckEnv)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (program: Program)
    : Result<Type * Program * TypeCheckEnv, TypeError> =
    checkProgramInternal (Some baseEnv) requireExplicitTypeArgsForBareCalls false warningSettings program
