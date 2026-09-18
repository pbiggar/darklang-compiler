// Diagnostics.fs - Represent typing failures and render source-compatible diagnostics.

module CheckingDiagnostics

open AST

let internal makePartialParams (funcName: string) (types: Type list) : (string * Type) list =
    let safeName = funcName.Replace('.', '_')
    types |> List.mapi (fun i t -> ($"__partial_{safeName}_{i}", t))

let internal toCallArgs (args: Expr list) : NonEmptyList<Expr> =
    match args with
    | [] -> NonEmptyList.singleton UnitLiteral
    | _ -> NonEmptyList.fromList args

let internal normalizeNullaryCallArgs (expectedParamCount: int) (args: Expr list) : Expr list =
    if expectedParamCount = 0 && args = [UnitLiteral] then
        []
    else
        args

let internal toLambdaParams (parameters: (string * Type) list) : NonEmptyList<LambdaParameter> =
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
    | PolymorphicRecursion of memberName:string
    | ResolutionFailure of NameResolution.ResolutionError
    | GenericError of string

type internal AliasVisitState =
    | AliasVisiting
    | AliasValidated

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
    | TStream elemType -> $"Stream<{typeToString elemType}>"
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
    | PolymorphicRecursion memberName ->
        $"Polymorphic recursion is not supported inside recursive group member: {memberName}"
    | ResolutionFailure error ->
        NameResolution.errorToString error
    | GenericError msg ->
        msg

let internal withIndefiniteArticle (s: string) : string =
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

let internal ifConditionTypeMismatchMessage (expr: Expr) (actualType: Type) : string =
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

let internal interpolationTypeMismatchMessage (expr: Expr) (actualType: Type) : string =
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
let rec internal substituteInterpolationLiteral (name: string) (literal: Expr) (expr: Expr) : Expr =
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
    | RecursiveLet (recursion, value, body) ->
        if recursiveBindingName recursion = name then
            RecursiveLet (recursion, value, body)
        else
            RecursiveLet (recursion, recurse value, recurse body)
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
    | Lambda (parameters, returnAnnotation, body) -> Lambda (parameters, returnAnnotation, recurse body)
    | Apply (func, callArgs) -> Apply (recurse func, NonEmptyList.map recurse callArgs)
    | IndirectApply (func, callArgs) -> IndirectApply (recurse func, NonEmptyList.map recurse callArgs)
    | Closure (functionName, captures) -> Closure (functionName, List.map recurse captures)
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
    | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ -> expr

let internal isBuiltinUnwrapName (funcName: string) : bool =
    funcName = "Builtin.unwrap"

let internal isBuiltinTestRuntimeErrorName (funcName: string) : bool =
    funcName = "Builtin.testRuntimeError"

/// `crash` is the public source-level bottom operation. The older builtin is
/// retained solely for the test harness.
let private isSourceCrashName (funcName: string) : bool =
    funcName = "Builtin.crash"

let internal isRuntimeFailureName (funcName: string) : bool =
    isBuiltinTestRuntimeErrorName funcName || isSourceCrashName funcName

let internal isBuiltinTestNanName (name: string) : bool =
    name = "Builtin.testNan"

let internal isBuiltinTestInfinityName (name: string) : bool =
    name = "Builtin.testInfinity"

let internal isBuiltinBlobEmptyName (name: string) : bool =
    name = "Builtin.blobEmpty"

let internal isRuntimeErrorType (typ: Type) : bool =
    match typ with
    | TRuntimeError -> true
    | _ -> false

let private variantNameEndsWith (suffix: string) (variantName: string) : bool =
    variantName = suffix || variantName.EndsWith($".{suffix}")

let internal isKnownFailureConstructorExpr (expr: Expr) : bool =
    match expr with
    | Constructor (_, variantName, None) when variantNameEndsWith "None" variantName ->
        true
    | Constructor (_, variantName, Some _) when variantNameEndsWith "Error" variantName ->
        true
    | _ ->
        false

/// Detect runtime-failing unwrap expressions, including piped/desugared shapes:
/// let x = Option.None in Builtin.unwrap(x)
let rec internal isKnownUnwrapFailureExpr (boundExprs: Map<string, Expr>) (expr: Expr) : bool =
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
let rec internal isKnownTestRuntimeErrorExpr (boundExprs: Map<string, Expr>) (expr: Expr) : bool =
    match expr with
    | Call (funcName, { Head = _; Tail = [] }) when isRuntimeFailureName funcName ->
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
let rec internal tryExtractKnownTestRuntimeErrorMessage
    (boundExprs: Map<string, Expr>)
    (expr: Expr)
    : string option =
    match expr with
    | Call (funcName, { Head = argExpr; Tail = [] }) when isRuntimeFailureName funcName ->
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

let rec internal tryFormatLiteralValue (expr: Expr) : string option =
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

let rec internal formatLetDeconstructionPattern (pattern: LetPattern) : string =
    match pattern with
    | LPVariable _ -> "[variable]"
    | LPWildcard -> "_"
    | LPUnit -> "()"
    | LPTuple (first, second, rest) ->
        first :: second :: rest
        |> List.map formatLetDeconstructionPattern
        |> String.concat ", "
        |> fun text -> $"({text})"

let rec internal inferredLetPatternType (path: string) (pattern: LetPattern) : Type =
    match pattern with
    | LPUnit -> TUnit
    | LPVariable name -> TVar $"binding_{path}_{name}"
    | LPWildcard -> TVar $"binding_{path}_wildcard"
    | LPTuple (first, second, rest) ->
        first :: second :: rest
        |> List.mapi (fun index inner -> inferredLetPatternType $"{path}_{index}" inner)
        |> TTuple

/// Check the entire let pattern shape before returning any bindings.
let rec internal bindLetPatternTypes
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

let internal formatListLiteralForNoMatch (elements: Expr list) : string =
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

let rec internal formatPatternMismatchValue (expr: Expr) : string option =
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

let internal formatPatternMismatchError
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

let internal formatLegacyParamTypeError
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

/// The call sites freshened so far in the function being checked. Reset per
/// function so the names a program produces depend only on that function.
let mutable private freshenedCallSites = 0

/// Start numbering freshened call sites from zero again.
let resetFreshening () = freshenedCallSites <- 0

/// Freshen type parameters - generate new unique names for each type param
/// Returns (fresh type params, substitution map from old to fresh names)
/// The names carry the call site's number, not the parameter's index: two calls
/// of generic functions in one body (`map f (andThen p q)`, or a generic seed
/// passed to a generic fold) would otherwise both bind `a$0`, and one call's
/// inference would read the other's bindings.
let freshenTypeParams (typeParams: string list) : string list * Map<string, string> =
    let site = freshenedCallSites
    freshenedCallSites <- freshenedCallSites + 1
    let freshParams = typeParams |> List.map (fun baseName -> $"{baseName}${site}")
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
    | TStream elem -> TStream (applyTypeVarRenaming subst elem)
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
