// ASTPrettyPrinter.fs - Pretty printer for canonical Dark syntax.

module ASTPrettyPrinter

open AST

type private LiteralEscapeContext =
    | StringContent
    | InterpolatedStringText
    | CharContent

let private escapeLiteralContent (context: LiteralEscapeContext) (input: string) : string =
    input
    |> String.collect (fun c ->
        match c, context with
        | '\\', _ -> "\\\\"
        | '"', (StringContent | InterpolatedStringText) -> "\\\""
        | '\'', CharContent -> "\\'"
        | '\n', _ -> "\\n"
        | '\r', _ -> "\\r"
        | '\t', _ -> "\\t"
        | '\000', _ -> "\\0"
        | '{', InterpolatedStringText -> "\\{"
        | '}', InterpolatedStringText -> "\\}"
        | _ -> string c)

let private formatFloatLiteral (value: float) : string =
    let raw = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
    let containsLetters = raw |> Seq.exists System.Char.IsLetter
    if containsLetters || raw.Contains(".") || raw.Contains("E") || raw.Contains("e") then
        raw
    else
        $"{raw}.0"

let private formatIdentifierSegment (name: string) : string =
    NameSyntax.formatIdentifier (NameSyntax.identifierFromText name)

let private formatIdentifierPath (name: string) : string =
    match NameSyntax.tryParseLegacySpelling name with
    | Some parsed -> NameSyntax.formatQualifiedName parsed
    | None -> formatIdentifierSegment name

let rec private formatType (typ: Type) : string =
    match typ with
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
    | TRawPtr -> "RawPtr"
    | TVar name -> formatIdentifierSegment name
    | TList elemType -> $"List<{formatType elemType}>"
    | TStream elemType -> $"Stream<{formatType elemType}>"
    | TDict (TString, valueType) -> $"Dict<{formatType valueType}>"
    | TDict (keyType, valueType) ->
        // Preserve both public type arguments for non-String-keyed Dicts.
        $"Dict<{formatType keyType}, {formatType valueType}>"
    | TTuple elemTypes ->
        let formatElement elemType =
            match elemType with
            | TFunction _ -> $"({formatType elemType})"
            | _ -> formatType elemType
        let elemText = elemTypes |> List.map formatElement |> String.concat " * "
        $"({elemText})"
    | TEnumFields fieldTypes ->
        fieldTypes |> List.map formatType |> String.concat " * "
    | TRecord (name, []) -> formatIdentifierPath name
    | TRecord (name, typeArgs) ->
        let argsText = typeArgs |> List.map formatType |> String.concat ", "
        $"{formatIdentifierPath name}<{argsText}>"
    | TSum (name, []) -> formatIdentifierPath name
    | TSum (name, typeArgs) ->
        let argsText = typeArgs |> List.map formatType |> String.concat ", "
        $"{formatIdentifierPath name}<{argsText}>"
    | TFunction (paramTypes, returnType) ->
        let formatParameter paramType =
            match paramType with
            | TFunction _ -> $"({formatType paramType})"
            | _ -> formatType paramType
        (paramTypes |> List.map formatParameter) @ [formatType returnType]
        |> String.concat " -> "

let private formatBinOp (op: BinOp) : string =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Pow -> "^"
    | Shl -> "<<"
    | Shr -> ">>"
    | BitAnd -> "&"
    | BitOr -> "|||"
    | BitXor -> "^"
    | StringConcat -> "++"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Lte -> "<="
    | Gte -> ">="
    | And -> "&&"
    | Or -> "||"

let private formatUnaryOp (op: UnaryOp) : string =
    match op with
    | Neg -> "-"
    | Not -> "!"
    | BitNot -> "~~~"

let private isComparisonOp (op: BinOp) : bool =
    match op with
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte -> true
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    | Shl
    | Shr
    | BitAnd
    | BitOr
    | BitXor
    | StringConcat
    | And
    | Or -> false

let private binOpPrecedence (op: BinOp) : int =
    match op with
    | Or -> 1
    | And -> 2
    | BitOr -> 3
    | BitXor -> 4
    | BitAnd -> 5
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte -> 6
    | Shl
    | Shr -> 7
    | Add
    | Sub
    | StringConcat -> 8
    | Mul
    | Div
    | Mod -> 9
    | Pow -> 10

let private shouldParenthesizeBinChild (parentOp: BinOp) (isLeftChild: bool) (childOp: BinOp) : bool =
    let parentPrec = binOpPrecedence parentOp
    let childPrec = binOpPrecedence childOp
    if childPrec < parentPrec then
        true
    elif childPrec > parentPrec then
        false
    elif isComparisonOp parentOp then
        true
    elif parentOp = Pow then
        // Exponentiation is right-associative.
        isLeftChild
    else
        // Operators are left-associative: left child can omit equal-precedence
        // parentheses, right child needs them to preserve tree shape.
        not isLeftChild

let rec private isAtomicExpr (expr: Expr) : bool =
    match expr with
    | UnitLiteral
    | Int64Literal _
    | Int128Literal _
    | BigIntLiteral _
    | Int8Literal _
    | Int16Literal _
    | Int32Literal _
    | UInt8Literal _
    | UInt16Literal _
    | UInt32Literal _
    | UInt64Literal _
    | UInt128Literal _
    | BoolLiteral _
    | StringLiteral _
    | CharLiteral _
    | FloatLiteral _
    | InterpolatedString _
    | Var _
    | FuncRef _
    | Call _
    | TypeApp _
    | Apply _ | IndirectApply _
    | TupleLiteral _
    | DictLiteral _
    | RecordLiteral _
    | ListLiteral _
    | Constructor (_, _, None) -> true
    | TupleAccess (tupleExpr, _) -> isAtomicExpr tupleExpr
    | RecordAccess (recordExpr, _) -> isAtomicExpr recordExpr
    | _ -> false

let private parenthesizeIfNeeded (expr: Expr) (text: string) : string =
    if isAtomicExpr expr then text else $"({text})"

let private parenthesizeTupleBaseIfNeeded (expr: Expr) (text: string) : string =
    match expr with
    | TupleAccess _ -> $"({text})"
    | _ -> parenthesizeIfNeeded expr text

let private isUnitLambdaParameter (parameter: LambdaParameter) : bool =
    parameter.Pattern = LPUnit

let private isSyntheticUnitParamList (parameters: NonEmptyList<string * Type>) : bool =
    match NonEmptyList.toList parameters with
    | [(paramName, TUnit)] -> paramName.StartsWith("$unit")
    | _ -> false

let private isUnitArgumentList (args: NonEmptyList<Expr>) : bool =
    match NonEmptyList.toList args with
    | [UnitLiteral] -> true
    | _ -> false

let rec private formatPattern (pattern: Pattern) : string =
    match pattern with
    | PUnit -> "()"
    | PWildcard -> "_"
    | PVar name -> formatIdentifierSegment name
    | PConstructor (name, None) -> formatIdentifierPath name
    | PConstructor (name, Some payload) ->
        let payloadText = formatPattern payload
        $"{formatIdentifierPath name} {payloadText}"
    | POr alternatives ->
        alternatives
        |> NonEmptyList.toList
        |> List.map formatPattern
        |> String.concat " | "
    | PInt64 n ->
        $"{n}L"
    | PBigInt n ->
        $"{n}I"
    | PInt128Literal n ->
        $"{n}Q"
    | PInt8Literal n ->
        $"{n}y"
    | PInt16Literal n ->
        $"{n}s"
    | PInt32Literal n ->
        $"{n}l"
    | PUInt8Literal n ->
        $"{n}uy"
    | PUInt16Literal n ->
        $"{n}us"
    | PUInt32Literal n ->
        $"{n}ul"
    | PUInt64Literal n ->
        $"{n}UL"
    | PUInt128Literal n ->
        $"{n}Z"
    | PBool b -> if b then "true" else "false"
    | PString s -> $"\"{escapeLiteralContent StringContent s}\""
    | PChar c -> $"'{escapeLiteralContent CharContent c}'"
    | PFloat f -> formatFloatLiteral f
    | PTuple patterns ->
        let parts = patterns |> List.map formatPattern |> String.concat ", "
        $"({parts})"
    | PList patterns ->
        let separator = "; "
        let items = patterns |> List.map formatPattern |> String.concat separator
        $"[{items}]"
    | PListCons (head, tail) ->
        let formatHeadPattern pattern =
            let formatted = formatPattern pattern
            match pattern with
            // Cons is right-associative. A cons used as a head therefore needs
            // grouping or reparsing would flatten it into the outer chain.
            | PListCons _ -> $"({formatted})"
            // Constructor payloads are whitespace-delimited and parse a
            // complete pattern, so grouping keeps the outer cons outside the payload.
            | PConstructor (_, Some _) -> $"({formatted})"
            | _ -> formatted
        head
        |> List.map formatHeadPattern
        |> fun headParts -> headParts @ [formatPattern tail]
        |> String.concat " :: "

let rec private formatLetPattern (pattern: LetPattern) : string =
    match pattern with
    | LPUnit -> "()"
    | LPWildcard -> "_"
    | LPVariable name -> formatIdentifierSegment name
    | LPTuple (first, second, rest) ->
        first :: second :: rest
        |> List.map formatLetPattern
        |> String.concat ", "
        |> fun elements -> $"({elements})"

let rec private formatExpr (expr: Expr) : string =
    let isNegativeNumericLiteral (arg: Expr) : bool =
        match arg with
        | Int64Literal n -> n < 0L
        | Int128Literal n -> n < System.Int128.Zero
        | BigIntLiteral n -> n < System.Numerics.BigInteger.Zero
        | Int8Literal n -> n < 0y
        | Int16Literal n -> n < 0s
        | Int32Literal n -> n < 0l
        | FloatLiteral f ->
            // Keep -0.0 wrapped as well; it is lexically ambiguous in application position.
            System.BitConverter.DoubleToInt64Bits(f) < 0L
        | _ -> false

    let formatAppArg (arg: Expr) : string =
        let argText = formatExpr arg
        match arg with
        | _ when isNegativeNumericLiteral arg -> $"({argText})"
        | Constructor (_, _, None) -> $"({argText})"
        | TupleLiteral _ -> $"({argText})"
        | Call _
        | TypeApp _
        | Apply _ | IndirectApply _ -> $"({argText})"
        | _ -> parenthesizeIfNeeded arg argText

    let rec formatAppArgs (args: Expr list) : string list =
        match args with
        | [] -> []
        | [lastArg] -> [formatAppArg lastArg]
        | currentArg :: ((UnitLiteral as nextArg) :: restArgs) ->
            // `f x ()` can be reparsed as applying unit to `x`.
            // Parenthesize the preceding argument to preserve argument boundaries.
            $"({formatAppArg currentArg})"
            :: (formatAppArgs (nextArg :: restArgs))
        | currentArg :: ((TupleLiteral _ as nextArg) :: restArgs) ->
            // `f g (a, b)` can be reparsed as applying `g` to tuple elements.
            // Parenthesize the preceding argument to keep tuple as a separate argument.
            $"({formatAppArg currentArg})"
            :: (formatAppArgs (nextArg :: restArgs))
        | currentArg :: restArgs ->
            formatAppArg currentArg :: formatAppArgs restArgs

    match expr with
    | BoundaryRender (_, value) -> formatExpr value
    | RuntimeError message ->
        let escaped = escapeLiteralContent StringContent message
        $"Builtin.testRuntimeError \"{escaped}\""
    | UnitLiteral -> "()"
    | Int64Literal n ->
        $"{n}L"
    | Int128Literal n ->
        $"{n}Q"
    | BigIntLiteral n ->
        $"{n}"
    | Int8Literal n ->
        $"{n}y"
    | Int16Literal n ->
        $"{n}s"
    | Int32Literal n ->
        $"{n}l"
    | UInt8Literal n ->
        $"{n}uy"
    | UInt16Literal n ->
        $"{n}us"
    | UInt32Literal n ->
        $"{n}ul"
    | UInt64Literal n ->
        $"{n}UL"
    | UInt128Literal n ->
        $"{n}Z"
    | BoolLiteral b -> if b then "true" else "false"
    | StringLiteral s -> $"\"{escapeLiteralContent StringContent s}\""
    | CharLiteral c -> $"'{escapeLiteralContent CharContent c}'"
    | FloatLiteral f -> formatFloatLiteral f
    | InterpolatedString parts ->
        let partsText =
            parts
            |> List.map (function
                | StringText t -> escapeLiteralContent InterpolatedStringText t
                | StringExpr e -> $"{{{formatExpr e}}}")
            |> String.concat ""
        $"$\"{partsText}\""
    | BinOp (op, left, right) ->
        let formatChild (isLeftChild: bool) (child: Expr) : string =
            let childText = formatExpr child
            match child with
            | BinOp (childOp, _, _) ->
                if shouldParenthesizeBinChild op isLeftChild childOp then
                    $"({childText})"
                else
                    childText
            | _ -> parenthesizeIfNeeded child childText
        let leftCanConsumeNegativeNumericArg (expr: Expr) : bool =
            match expr with
            | Var funcName when funcName.Contains "." -> true
            | Call _
            | TypeApp _
            | Apply _ | IndirectApply _
            | Constructor (_, _, None) -> true
            | _ -> false
        let isNumericLiteralExpr (expr: Expr) : bool =
            match expr with
            | Int64Literal _
            | Int128Literal _
            | BigIntLiteral _
            | Int8Literal _
            | Int16Literal _
            | Int32Literal _
            | UInt8Literal _
            | UInt16Literal _
            | UInt32Literal _
            | UInt64Literal _
            | UInt128Literal _
            | FloatLiteral _ -> true
            | _ -> false
        let leftText = formatChild true left
        let rightTextBase = formatChild false right
        let rightText =
            match op with
            | Sub when leftCanConsumeNegativeNumericArg left && isNumericLiteralExpr right ->
                $"({rightTextBase})"
            | _ -> rightTextBase
        $"{leftText} {formatBinOp op} {rightText}"
    | UnaryOp (op, inner) ->
        let innerText = parenthesizeIfNeeded inner (formatExpr inner)
        $"{formatUnaryOp op}{innerText}"
    | Let (LPVariable name, Lambda (parameters, Some returnType, functionBody), body) ->
        let annotatedParameters =
            parameters
            |> NonEmptyList.toList
            |> List.map (fun parameter ->
                match parameter.Pattern, parameter.SourceAnnotation with
                | LPVariable parameterName, Some parameterType ->
                    Some $"({formatIdentifierSegment parameterName}: {formatType parameterType})"
                | _ -> None)
        if annotatedParameters |> List.forall Option.isSome then
            let paramsText = annotatedParameters |> List.choose id |> String.concat " "
            $"let {formatIdentifierSegment name} {paramsText} : {formatType returnType} = {formatExpr functionBody} in {formatExpr body}"
        else
            let lambda = Lambda (parameters, Some returnType, functionBody)
            $"let {formatLetPattern (LPVariable name)} = {formatExpr lambda} in {formatExpr body}"
    | RecursiveLet (recursion, Lambda (parameters, Some returnType, functionBody), body)
        when recursiveBindingKind recursion = NamedLocalFunctionMember ->
        let annotatedParameters =
            parameters
            |> NonEmptyList.toList
            |> List.map (fun parameter ->
                match parameter.Pattern, parameter.SourceAnnotation with
                | LPVariable parameterName, Some parameterType ->
                    Some $"({formatIdentifierSegment parameterName}: {formatType parameterType})"
                | _ -> None)
        let name = recursiveBindingName recursion
        if annotatedParameters |> List.forall Option.isSome then
            let paramsText = annotatedParameters |> List.choose id |> String.concat " "
            $"(let {formatIdentifierSegment name} {paramsText} : {formatType returnType} = {formatExpr functionBody} in {formatExpr body})"
        else
            let lambda = Lambda (parameters, Some returnType, functionBody)
            $"let {formatIdentifierSegment name} = {formatExpr lambda} in {formatExpr body}"
    | RecursiveLet (recursion, value, body) ->
        $"let {formatIdentifierSegment (recursiveBindingName recursion)} = {formatExpr value} in {formatExpr body}"
    | Let (pattern, value, body) ->
        $"let {formatLetPattern pattern} = {formatExpr value} in {formatExpr body}"
    | Var name -> formatIdentifierPath name
    | If (cond, thenBranch, elseBranch) ->
        $"if {formatExpr cond} then {formatExpr thenBranch} else {formatExpr elseBranch}"
    | Sequence (first, next) ->
        $"({formatExpr first}; {formatExpr next})"
    | Call (funcName, args) ->
        let argsList = NonEmptyList.toList args
        let formattedName = formatIdentifierPath funcName
        if isUnitArgumentList args then
            $"{formattedName} ()"
        else
            let argsText = argsList |> formatAppArgs |> String.concat " "
            $"{formattedName} {argsText}"
    | TypeApp (funcName, typeArgs, args) ->
        let argsList = NonEmptyList.toList args
        let typeArgsText = typeArgs |> List.map formatType |> String.concat ", "
        let formattedName = formatIdentifierPath funcName
        let head = $"{formattedName}<{typeArgsText}>"
        if isUnitArgumentList args then
            $"{head} ()"
        else
            let argsText = argsList |> formatAppArgs |> String.concat " "
            $"{head} {argsText}"
    | TupleLiteral elements ->
        let elementsText = elements |> List.map formatExpr |> String.concat ", "
        $"({elementsText})"
    | TupleAccess (tupleExpr, index) ->
        let tupleBaseText = formatExpr tupleExpr
        let tupleText =
            match tupleExpr with
            | Call _ | TypeApp _ | Apply _ | IndirectApply _ ->
                // Space application has no mandatory wrapping.
                // Parenthesize before postfix access so `.0` binds to the call result.
                $"({tupleBaseText})"
            | _ ->
                parenthesizeTupleBaseIfNeeded tupleExpr tupleBaseText
        $"{tupleText}.{index}"
    | DictLiteral (_, _, entries) ->
        let fieldsText =
            entries
            |> List.map (fun (key, value) ->
                $"{formatExpr key}: {formatExpr value}")
            |> String.concat "; "
        $"Dict {{ {fieldsText} }}"
    | RecordLiteral (reference, fields) ->
        let fieldsText =
            fields
            |> List.map (fun (name, value) ->
                $"{formatIdentifierSegment name} = {formatExpr value}")
            |> String.concat ", "
        let typeArgsText =
            match reference.TypeArgs with
            | [] -> ""
            | typeArgs ->
                typeArgs
                |> List.map formatType
                |> String.concat ", "
                |> fun args -> $"<{args}>"
        $"{formatIdentifierPath reference.SourceTypeName}{typeArgsText} {{ {fieldsText} }}"
    | RecordUpdate (recordExpr, updates) ->
        let recordText = formatExpr recordExpr
        let updatesText =
            updates
            |> List.map (fun (name, value) ->
                $"{formatIdentifierSegment name} = {formatExpr value}")
            |> String.concat ", "
        $"{{ {recordText} with {updatesText} }}"
    | RecordAccess (recordExpr, fieldName) ->
        let recordBaseText = formatExpr recordExpr
        let recordText =
            match recordExpr with
            | Call _ | TypeApp _ | Apply _ | IndirectApply _ ->
                // Same ambiguity as tuple access: ensure `.field` applies to call result.
                $"({recordBaseText})"
            | _ ->
                parenthesizeIfNeeded recordExpr recordBaseText
        $"{recordText}.{formatIdentifierSegment fieldName}"
    | Constructor (constructorReference, variantName, payload) ->
        let fullName =
            let formattedVariantName = formatIdentifierSegment variantName
            match constructorReferenceTypeName constructorReference with
            | None -> formattedVariantName
            | Some typeName ->
                $"{formatIdentifierPath typeName}.{formattedVariantName}"
        match payload with
        | None -> fullName
        | Some payloadExpr ->
            let payloadText = formatAppArg payloadExpr
            $"{fullName} {payloadText}"
    | Match (scrutinee, cases) ->
        let scrutineeText = formatExpr scrutinee
        let formatCaseBody (body: Expr) : string =
            let bodyText = formatExpr body
            match body with
            // Without parens, nested match case bars get parsed as outer cases.
            | Match _
            | Let _ -> $"({bodyText})"
            | _ -> bodyText
        let caseText =
            cases
            |> List.map (fun case ->
                let patternsText =
                    case.Patterns
                    |> NonEmptyList.toList
                    |> List.map formatPattern
                    |> String.concat " | "
                let guardText =
                    match case.Guard with
                    | None -> ""
                    | Some guardExpr -> $" when {formatExpr guardExpr}"
                $"| {patternsText}{guardText} -> {formatCaseBody case.Body}")
            |> String.concat " "
        $"match {scrutineeText} with {caseText}"
    | ListLiteral elements ->
        let separator = "; "
        let elementsText = elements |> List.map formatExpr |> String.concat separator
        $"[{elementsText}]"
    | Lambda (parameters, returnAnnotation, body) ->
        let parameterList = NonEmptyList.toList parameters
        match parameterList, body with
        | [ { Pattern = LPVariable paramName; InferredType = Some TBool } ],
          BinOp (And, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
            $"(&&) {formatAppArg rightArg}"
        | [ { Pattern = LPVariable paramName; InferredType = Some TBool } ],
          BinOp (Or, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
            $"(||) {formatAppArg rightArg}"
        | [singleParameter], _ when isUnitLambdaParameter singleParameter ->
            $"fun () -> {formatExpr body}"
        | _ ->
            let paramsText =
                parameterList
                |> List.map (fun parameter -> formatLetPattern parameter.Pattern)
                |> String.concat " "
            $"fun {paramsText} -> {formatExpr body}"
    | Apply (funcExpr, args)
    | IndirectApply (funcExpr, args) ->
        let argsList = NonEmptyList.toList args
        match funcExpr, argsList with
            // Preserve the Apply-vs-Constructor distinction
            // by printing constructor application in pipe form.
            | Constructor _, [singleArg] ->
                $"{formatExpr singleArg} |> {formatExpr funcExpr}"
            | _ ->
                let funcText = parenthesizeIfNeeded funcExpr (formatExpr funcExpr)
                if isUnitArgumentList args then
                    $"{funcText} ()"
                else
                    let argsText = argsList |> formatAppArgs |> String.concat " "
                    $"{funcText} {argsText}"
    | FuncRef funcName -> formatIdentifierPath funcName
    | Closure (funcName, captures) ->
        let capturesText = captures |> List.map formatExpr |> String.concat ", "
        $"Closure({formatIdentifierPath funcName}, [{capturesText}])"

let private formatFunctionDef (funcDef: FunctionDef) : string =
    let typeParamsText =
        if List.isEmpty funcDef.TypeParams then ""
        else
            let joined =
                funcDef.TypeParams
                |> List.map (fun name -> $"'{name}")
                |> String.concat ", "
            $"<{joined}>"
    let paramsText =
        funcDef.Params
        |> NonEmptyList.toList
        |> (fun parameters ->
            if isSyntheticUnitParamList funcDef.Params then
                "()"
            else
                parameters
                |> List.map (fun (name, typ) -> $"({formatIdentifierSegment name}: {formatType typ})")
                |> String.concat " ")
    $"let {formatIdentifierSegment funcDef.Name}{typeParamsText} {paramsText} : {formatType funcDef.ReturnType} = {formatExpr funcDef.Body}"

let private formatTypeDef (typeDef: TypeDef) : string =
    let formatTypeParams (typeParams: string list) : string =
        if List.isEmpty typeParams then ""
        else
            let joined =
                typeParams
                |> List.map (fun name ->
                    $"'{name}")
                |> String.concat ", "
            $"<{joined}>"

    match typeDef with
    | RecordDef (name, typeParams, fields) ->
        let fieldsText =
            fields
            |> List.map (fun (fieldName, fieldType) ->
                $"{formatIdentifierSegment fieldName}: {formatType fieldType}")
            |> String.concat ", "
        $"type {formatIdentifierSegment name}{formatTypeParams typeParams} = {{ {fieldsText} }}"
    | SumTypeDef (name, typeParams, variants) ->
        let variantsText =
            variants
            |> List.map (fun variant ->
                match variant.Payload with
                | None -> formatIdentifierSegment variant.Name
                | Some payloadType ->
                    $"{formatIdentifierSegment variant.Name} of {formatType payloadType}")
            |> String.concat " | "
        let leadingBar = "| "
        $"type {formatIdentifierSegment name}{formatTypeParams typeParams} = {leadingBar}{variantsText}"
    | TypeAlias (name, typeParams, targetType) ->
        $"type {formatIdentifierSegment name}{formatTypeParams typeParams} = {formatType targetType}"

let private formatTopLevel (topLevel: TopLevel) : string =
    match topLevel with
    | FunctionDef funcDef -> formatFunctionDef funcDef
    | TypeDef typeDef -> formatTypeDef typeDef
    | ValueDef valueDef ->
        $"val {formatIdentifierSegment (valueDefName valueDef)} = {formatExpr (valueDefBody valueDef)}"
    | Expression expr -> formatExpr expr

let private tryRestoreModuleDeclaration (topLevel: TopLevel) : (NameSyntax.QualifiedName * TopLevel) option =
    let splitName name =
        NameSyntax.tryParseLegacySpelling name
        |> Option.bind NameSyntax.trySplitLast
        |> Option.map (fun (moduleName, declarationName) ->
            (moduleName, NameSyntax.identifierText declarationName))
    match topLevel with
    | FunctionDef definition ->
        splitName definition.Name
        |> Option.map (fun (moduleName, declarationName) ->
            (moduleName, FunctionDef { definition with Name = declarationName }))
    | ValueDef valueDef ->
        splitName (valueDefName valueDef)
        |> Option.map (fun (moduleName, declarationName) ->
            let restored =
                match valueDef with
                | UncheckedValueDef (_, body) -> UncheckedValueDef (declarationName, body)
                | CheckedValueDef (_, typ, body) -> CheckedValueDef (declarationName, typ, body)
            (moduleName, ValueDef restored))
    | TypeDef (RecordDef (name, typeParams, fields)) ->
        splitName name
        |> Option.map (fun (moduleName, declarationName) ->
            (moduleName, TypeDef (RecordDef (declarationName, typeParams, fields))))
    | TypeDef (SumTypeDef (name, typeParams, variants)) ->
        splitName name
        |> Option.map (fun (moduleName, declarationName) ->
            (moduleName, TypeDef (SumTypeDef (declarationName, typeParams, variants))))
    | TypeDef (TypeAlias (name, typeParams, targetType)) ->
        splitName name
        |> Option.map (fun (moduleName, declarationName) ->
            (moduleName, TypeDef (TypeAlias (declarationName, typeParams, targetType))))
    | Expression _ -> None

let formatProgram (Program items: Program) : string =
    let separator = "\n;\n"
    let restored = items |> List.map tryRestoreModuleDeclaration
    match restored with
    | Some (firstModule, _) :: _
        when restored
             |> List.forall (function
                 | Some (moduleName, _) -> moduleName = firstModule
                 | None -> false) ->
        let declarations =
            restored
            |> List.choose (Option.map snd)
            |> List.map formatTopLevel
            |> String.concat separator
        $"module {NameSyntax.formatQualifiedName firstModule}\n{declarations}"
    | _ -> items |> List.map formatTopLevel |> String.concat separator
