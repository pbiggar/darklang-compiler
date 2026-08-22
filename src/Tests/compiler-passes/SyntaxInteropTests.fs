// SyntaxInteropTests.fs - Tests for compiler/interpreter syntax interop.
//
// Verifies the dedicated interpreter-syntax parser and AST pretty printers for
// both Darklang syntaxes.

module SyntaxInteropTests

open AST
open Parser
open InterpreterParser
open ASTPrettyPrinter
open TypeChecking

type TestResult = Result<unit, string>

let testCompilerLibraryParseInterpreterSyntax () : TestResult =
    let source = "let x = 5L in x"
    match CompilerLibrary.parseProgram CompilerLibrary.InterpreterSyntax false source with
    | Error err -> Error $"CompilerLibrary interpreter parse failed: {err}"
    | Ok (Program [Expression _]) -> Ok ()
    | Ok other -> Error $"Expected single expression program, got: {other}"

let testParseInterpreterLambdaApplication () : TestResult =
    let source = "let inc = fun x -> Stdlib.Int64.add x 1L in inc 41L"
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed: {err}"
    | Ok (Program [Expression expr]) ->
        match expr with
        | RecursiveLet (recursion, Lambda (parameters, returnAnnotation, body), Call ("inc", callArgs))
            when recursiveBindingKind recursion = DirectLambdaValueMember ->
            let paramNames =
                parameters
                |> NonEmptyList.toList
                |> List.choose (fun parameter ->
                    match parameter.Pattern with
                    | LPVariable name -> Some name
                    | _ -> None)
            let callArgsList = callArgs |> NonEmptyList.toList
            match paramNames, callArgsList, body with
            | [paramName], [Int64Literal 41L], Call ("Stdlib.Int64.add", addArgs)
                when paramName = "x"
                     && NonEmptyList.toList addArgs = [Var "x"; Int64Literal 1L] ->
                Ok ()
            | _ ->
                Error $"Unexpected lambda application AST: {expr}"
        | _ -> Error $"Unexpected AST shape: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testParseInterpreterNestedFunctionAfterLetBinding () : TestResult =
    let source =
        "let limit = 10L let sumUpTo (i: Int64) : Int64 = if i > limit then 0L else i + (sumUpTo (i + 1L)) sumUpTo 1L"

    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on nested function after let binding: {err}"
    | Ok (Program [Expression (Let (LPVariable "limit", Int64Literal 10L, RecursiveLet (recursion, Lambda (parameters, returnAnnotation, body), Call ("sumUpTo", callArgs))))])
        when recursiveBindingKind recursion = NamedLocalFunctionMember ->
        match NonEmptyList.toList parameters, body, NonEmptyList.toList callArgs with
        | [{ Pattern = LPVariable "i"; SourceAnnotation = Some AST.TInt64 }], If (BinOp (Gt, Var "i", Var "limit"), Int64Literal 0L, BinOp (Add, Var "i", Call ("sumUpTo", recursiveArgs))), [Int64Literal 1L] ->
            match NonEmptyList.toList recursiveArgs with
            | [BinOp (Add, Var "i", Int64Literal 1L)] -> Ok ()
            | other -> Error $"Unexpected recursive call args for nested function: {other}"
        | _ ->
            Error $"Unexpected nested function AST: parameters={parameters}; body={body}; callArgs={callArgs}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for nested function after let binding: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testInterpreterParserParsesWildcardLambdaParameter () : TestResult =
    let source = "fun _ -> 1L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on wildcard lambda parameter: {err}"
    | Ok (Program [Expression (Lambda (parameters, None, Int64Literal 1L))]) ->
        match NonEmptyList.toList parameters with
        | [{ Pattern = LPWildcard; SourceAnnotation = None; InferredType = None }] ->
            Ok ()
        | other ->
            Error $"Unexpected lambda parameters for wildcard lambda: {other}"
    | Ok other ->
        Error $"Unexpected AST for wildcard lambda parameter: {other}"

let testParseInterpreterTripleQuotedInterpolation () : TestResult =
    let source = "$\"\"\"test {\"1\"}\"\"\""
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed on triple-quoted interpolation: {err}"
    | Ok (Program [Expression (InterpolatedString [StringText "test "; StringExpr (StringLiteral "1")])]) ->
        Ok ()
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for triple-quoted interpolation: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testParseInterpreterNegativeFloatApplicationArgs () : TestResult =
    let source = "Stdlib.Float.multiply -0.0 -1.0"
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed: {err}"
    | Ok (Program [Expression (Call ("Stdlib.Float.multiply", args))]) ->
        match NonEmptyList.toList args with
        | [FloatLiteral left; FloatLiteral right] ->
            if left = -0.0 && right = -1.0 then
                Ok ()
            else
                Error $"Expected negative float args, got left={left}, right={right}"
        | other ->
            Error $"Unexpected arguments for multiply: {other}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for negative float application args: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testParseInterpreterPipeMinusOperatorSection () : TestResult =
    let source = "4L |> (-) 3L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on pipe minus operator section: {err}"
    | Ok (Program [Expression expr]) ->
        match expr with
        | Apply (Lambda (parameters, None, BinOp (Sub, Var leftName, Int64Literal 3L)), args) ->
            let paramNames =
                parameters
                |> NonEmptyList.toList
                |> List.choose (fun parameter ->
                    match parameter.Pattern with
                    | LPVariable name -> Some name
                    | _ -> None)
            let argList = args |> NonEmptyList.toList
            match paramNames, argList with
            | [paramName], [Int64Literal 4L] ->
                if paramName = "$pipe_arg" && leftName = "$pipe_arg" then
                    Ok ()
                else
                    Error $"Unexpected pipe operator-section lambda binding: param={paramName}, left={leftName}"
            | _ ->
                Error $"Unexpected AST for pipe minus operator section: {expr}"
        | _ ->
            Error $"Unexpected AST for pipe minus operator section: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testInterpreterParserParsesLegacyIntSuffix () : TestResult =
    match InterpreterParser.parseString false "1I" with
    | Error err ->
        Error $"Interpreter parser failed on legacy Int suffix: {err}"
    | Ok (Program [Expression (BigIntLiteral value)]) when value = System.Numerics.BigInteger.One ->
        Ok ()
    | Ok other ->
        Error $"Expected BigInt literal program for legacy Int suffix, got: {other}"

let testCompilerParserParsesApostropheTypeArgAtCallSite () : TestResult =
    let source = "Stdlib.Json.parse<'a>(\"5\")"
    match Parser.parseString false source with
    | Error err ->
        Error $"Compiler parser failed on apostrophe type argument call site: {err}"
    | Ok (Program [Expression (TypeApp ("Stdlib.Json.parse", [TVar "a"], args))]) ->
        if NonEmptyList.toList args = [StringLiteral "5"] then Ok ()
        else Error $"Unexpected args for apostrophe type argument call site: {args}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for apostrophe type argument call site: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testCompilerParserParsesApostropheTypeArgSpaceCallSite () : TestResult =
    let source = "Stdlib.Json.parse<'a> \"5\""
    match Parser.parseString false source with
    | Error err ->
        Error $"Compiler parser failed on apostrophe type argument space call site: {err}"
    | Ok (Program [Expression (TypeApp ("Stdlib.Json.parse", [TVar "a"], args))]) ->
        if NonEmptyList.toList args = [StringLiteral "5"] then Ok ()
        else Error $"Unexpected args for apostrophe type argument space call site: {args}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for apostrophe type argument space call site: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testInterpreterParserParsesBareFunctionTypeArgAtCallSite () : TestResult =
    let source = "Stdlib.Json.parse<Int64 -> Int64> \"{}\""
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on bare function type argument call site: {err}"
    | Ok (Program [Expression (TypeApp ("Stdlib.Json.parse", [TFunction ([AST.TInt64], AST.TInt64)], args))]) ->
        if NonEmptyList.toList args = [StringLiteral "{}"] then
            Ok ()
        else
            Error $"Unexpected args for bare function type argument call site: {args}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for bare function type argument call site: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testInterpreterParserParsesSingleTypeArgDictShorthandAtCallSite () : TestResult =
    let source = "Stdlib.Json.parse<Dict<String>> \"{}\""
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on Dict<Value> shorthand type argument call site: {err}"
    | Ok (Program [Expression (TypeApp ("Stdlib.Json.parse", [TDict (AST.TString, AST.TString)], args))]) ->
        if NonEmptyList.toList args = [StringLiteral "{}"] then
            Ok ()
        else
            Error $"Unexpected args for Dict<Value> shorthand type argument call site: {args}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for Dict<Value> shorthand type argument call site: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testInterpreterParserParsesStarTupleTypeArgAtCallSite () : TestResult =
    let source = "Stdlib.Json.serialize<Int64 * String * Int64> (1L, \"two\", 3L)"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on star-tuple type argument call site: {err}"
    | Ok (Program [Expression (TypeApp ("Stdlib.Json.serialize", [TTuple [AST.TInt64; AST.TString; AST.TInt64]], args))]) ->
        match NonEmptyList.toList args with
        | [Int64Literal 1L; StringLiteral "two"; Int64Literal 3L] ->
            Ok ()
        | other ->
            Error $"Unexpected args for star-tuple type argument call site: {other}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for star-tuple type argument call site: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testInterpreterParserParsesUnicodeEscapeSequencesInStrings () : TestResult =
    let source = "Stdlib.String.startsWith_v0 \"E\" \"\\u0014\\u0004\""
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on unicode escape sequences in string literal: {err}"
    | Ok (Program [Expression (Call ("Stdlib.String.startsWith_v0", args))]) ->
        match NonEmptyList.toList args with
        | [StringLiteral "E"; StringLiteral escaped] ->
            if escaped.Length = 2 && int escaped.[0] = 0x0014 && int escaped.[1] = 0x0004 then
                Ok ()
            else
                Error $"Unexpected decoded unicode escape payload: {escaped}"
        | other ->
            Error $"Unexpected args for unicode escape string parse: {other}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for unicode escape string parse: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testInterpreterParserParsesApostropheTypeParamsInFunctionDef () : TestResult =
    let source = "let fnWithTypeArgAndOneParam<'a> (arg: 'a) : 'a = arg"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on apostrophe type params in function definition: {err}"
    | Ok (Program [FunctionDef fnDef]) ->
        if fnDef.TypeParams = ["a"] then
            Ok ()
        else
            Error $"Expected function type params ['a'], got: {fnDef.TypeParams}"
    | Ok other ->
        Error $"Unexpected AST for interpreter apostrophe type parameter function definition: {other}"

let testInterpreterParserParsesApostropheTypeVarInTypeAnnotation () : TestResult =
    let source =
        "let returnsResultOk () : Stdlib.Result.Result<Int64, 'err> =\n"
        + "  Stdlib.Result.Result.Ok 5L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on apostrophe type var in type annotation: {err}"
    | Ok (Program [FunctionDef fnDef]) ->
        match fnDef.ReturnType with
        | TSum ("Stdlib.Result.Result", [AST.TInt64; AST.TVar "err"]) ->
            Ok ()
        | other ->
            Error $"Unexpected return type for apostrophe type var annotation: {typeToString other}"
    | Ok other ->
        Error $"Unexpected AST for interpreter apostrophe type var annotation: {other}"

let testInterpreterParserParsesApostropheTupleTypeVarsInTypeAnnotation () : TestResult =
    let source = "let pairEq (cmp: ('a * 'a) -> Int64) : Int64 = 0L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on apostrophe tuple type vars in type annotation: {err}"
    | Ok (Program [FunctionDef fnDef]) ->
        match NonEmptyList.toList fnDef.Params with
        | [("cmp", TFunction ([TTuple [TVar "a"; TVar "a"]], AST.TInt64))] ->
            Ok ()
        | other ->
            Error $"Unexpected parameters for apostrophe tuple type-var annotation: {other}"
    | Ok other ->
        Error $"Unexpected AST for apostrophe tuple type-var annotation: {other}"

let testInterpreterParserParsesApostropheSuffixedIdentifierName () : TestResult =
    let source = "fun default' -> default'"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on apostrophe-suffixed identifier name: {err}"
    | Ok (Program [Expression (Lambda (parameters, None, Var varName))]) ->
        match NonEmptyList.toList parameters with
        | [{ Pattern = LPVariable "default'"; SourceAnnotation = None; InferredType = None }]
            when varName = "default'" ->
            Ok ()
        | other ->
            Error $"Unexpected lambda parameters/body for apostrophe-suffixed identifier name: {other} / {varName}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for apostrophe-suffixed identifier name: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testInterpreterParserParsesEscapedSingleQuoteInStringLiteral () : TestResult =
    let source = "parse \"\\'hi\\'\""
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on escaped single quote in string literal: {err}"
    | Ok (Program [Expression (Call ("parse", args))]) ->
        match NonEmptyList.toList args with
        | [StringLiteral "'hi'"] ->
            Ok ()
        | other ->
            Error $"Unexpected call args for escaped single quote string literal: {other}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for escaped single quote string literal: {expr}"
    | Ok other ->
        Error $"Expected single expression program for escaped single quote string literal, got: {other}"

let testInterpreterParserParsesNamedVariantPayloads () : TestResult =
    let source =
        "type Inner<'a> =\n"
        + "  | A of a: 'a\n"
        + "  | B of b: Stdlib.Result.Result<Inner<'a>, String>\n"
        + "Inner.B(Stdlib.Result.Result.Ok(Inner.A 5L))"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on named variant payloads: {err}"
    | Ok (Program [TypeDef (SumTypeDef ("Inner", ["a"], variants)); Expression expr]) ->
        let hasExpectedVariants =
            match variants with
            | [ { Name = "A"; Payload = Some _ }; { Name = "B"; Payload = Some _ } ] -> true
            | _ -> false

        let hasExpectedExpression =
            match expr with
            | Constructor (UnresolvedConstructor (Some "Inner"), "B", Some (Constructor (UnresolvedConstructor (Some "Stdlib.Result.Result"), "Ok", Some (Constructor (UnresolvedConstructor (Some "Inner"), "A", Some (Int64Literal 5L)))))) ->
                true
            | _ ->
                false

        if hasExpectedVariants && hasExpectedExpression then
            Ok ()
        else
            Error $"Unexpected AST for named variant payloads: variants={variants}; expr={expr}"
    | Ok other ->
        Error $"Unexpected AST/program shape for named variant payloads: {other}"

let testInterpreterParserParsesNamedTupleVariantPayloads () : TestResult =
    let source =
        "type EnumOfMixedCases =\n"
        + "  | W\n"
        + "  | X of String\n"
        + "  | Y of i: Int64\n"
        + "  | Z of c: String * i: Int64\n"
        + "EnumOfMixedCases.W"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on named tuple variant payloads: {err}"
    | Ok (Program [TypeDef (SumTypeDef ("EnumOfMixedCases", [], variants)); Expression (Constructor (UnresolvedConstructor (Some "EnumOfMixedCases"), "W", None))]) ->
        match variants with
        | [ { Name = "W"; Payload = None }
            { Name = "X"; Payload = Some AST.TString }
            { Name = "Y"; Payload = Some AST.TInt64 }
            { Name = "Z"; Payload = Some (AST.TEnumFields [AST.TString; AST.TInt64]) } ] ->
            Ok ()
        | _ ->
            Error $"Unexpected variants parsed for named tuple variant payloads: {variants}"
    | Ok other ->
        Error $"Unexpected AST/program shape for named tuple variant payloads: {other}"

let testInterpreterParserParsesParenthesizedFunctionTypeAnnotation () : TestResult =
    let source = "let accepts (pred: (Int64 -> Bool)) : Bool = pred 1L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on parenthesized function type annotation: {err}"
    | Ok (Program [FunctionDef fnDef]) ->
        match NonEmptyList.toList fnDef.Params with
        | [("pred", TFunction ([AST.TInt64], AST.TBool))] ->
            Ok ()
        | other ->
            Error $"Unexpected parameters for parenthesized function type annotation: {other}"
    | Ok other ->
        Error $"Unexpected AST for parenthesized function type annotation: {other}"

let testInterpreterParserParsesElifChains () : TestResult =
    let source = "if true then 1L elif false then 2L else 3L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on elif chain: {err}"
    | Ok (Program [Expression expr]) ->
        match expr with
        | If
            (BoolLiteral true,
             Int64Literal 1L,
             If (BoolLiteral false, Int64Literal 2L, Int64Literal 3L)) ->
            Ok ()
        | _ ->
            Error $"Unexpected AST for elif chain: {expr}"
    | Ok other ->
        Error $"Expected single expression program for elif chain, got: {other}"

let testInterpreterParserAllowsAllUnderscoreIdentifiers () : TestResult =
    let source = "fun x ___ -> x + 1L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser rejected all-underscore identifier: {err}"
    | Ok (Program [Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST/program shape for all-underscore identifier: {other}"

let testInterpreterParserParsesCurriedTopLevelLetFunctionDef () : TestResult =
    let source = "let addCurried (x: Int64) (y: Int64) : Int64 = x + y"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on curried top-level let function definition: {err}"
    | Ok (Program [FunctionDef fnDef]) ->
        match NonEmptyList.toList fnDef.Params with
        | [("x", AST.TInt64); ("y", AST.TInt64)] ->
            Ok ()
        | _ ->
            Error $"Unexpected parameters parsed for curried function definition: {fnDef.Params}"
    | Ok other ->
        Error $"Unexpected AST for interpreter curried top-level let function definition: {other}"

let testCompilerParserParsesCurriedFunctionDef () : TestResult =
    let source = "let addCurried(x: Int64)(y: Int64) : Int64 = x + y"
    match Parser.parseString false source with
    | Error err ->
        Error $"Compiler parser failed on curried function definition: {err}"
    | Ok (Program [FunctionDef fnDef]) ->
        match NonEmptyList.toList fnDef.Params with
        | [("x", AST.TInt64); ("y", AST.TInt64)] ->
            Ok ()
        | _ ->
            Error $"Unexpected parameters parsed for compiler curried function definition: {fnDef.Params}"
    | Ok other ->
        Error $"Unexpected AST for compiler curried function definition: {other}"

let testParseInterpreterRecordFunctionFieldType () : TestResult =
    let source =
        "type RecordWithFn = { fn: Int64 -> Int64 }\n"
        + "(let record = RecordWithFn { fn = fun x -> x + 1L } in record.fn 6L)"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on record function field type: {err}"
    | Ok (Program [TypeDef (RecordDef ("RecordWithFn", [], [("fn", TFunction ([AST.TInt64], AST.TInt64))])); Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for record function field type: {other}"

let testParseInterpreterNewlineDelimitedLetBody () : TestResult =
    let source =
        "let y = (fun x -> x + 1L)\n"
        + " Stdlib.List.map_v0 [ 1L; 2L; 3L; 4L ] y"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on newline-delimited let body: {err}"
    | Ok (Program [Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for newline-delimited let body: {other}"

let testParseInterpreterNewlineDelimitedLetBodyAfterAppliedCallValue () : TestResult =
    let source =
        "let _ = Builtin.testSetExpectedExceptionCount 1L\n"
        + " Builtin.darkInternalInfraSchedulingRuleList ()"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on newline-delimited let body after applied call value: {err}"
    | Ok (Program [Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testParseInterpreterLocalLetFunctionBody () : TestResult =
    let source =
        "let limit = 10L\n"
        + " let sumUpTo (i: Int64) : Int64 =\n"
        + "   if i > limit then 0L else i + (sumUpTo (i + 1L))\n"
        + " sumUpTo 1L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on local let function body: {err}"
    | Ok (Program [Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testParseInterpreterParenthesizedSequenceWithTrailingLet () : TestResult =
    let source =
        "(Stdlib.DB.set\n"
        + "    (OuterRecord { name = \"joe\"; details = InnerRecord { numbers = AliasOfVeryInnerRecord { age = 41L } } })\n"
        + "    \"jjj\"\n"
        + "    TestNestedRecord\n"
        + "   Stdlib.DB.set\n"
        + "     (OuterRecord { name = \"frank\"; details = InnerRecord { numbers = VeryInnerRecord { age = 22L } } })\n"
        + "     \"fff\"\n"
        + "     TestNestedRecord\n"
        + "   let shouldBeJustJoe =\n"
        + "     Stdlib.DB.query TestNestedRecord (fun p -> p.details.numbers.age == 41L)\n"
        + "   Stdlib.List.length shouldBeJustJoe)"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on parenthesized sequence with trailing let: {err}"
    | Ok (Program [Expression (Sequence (_, Let _))]) ->
        Ok ()
    | Ok (Program [Expression expr]) ->
        Error $"Expected an explicit sequence ending in a let-expression, got: {expr}"
    | Ok other ->
        Error $"Expected single expression program for parenthesized sequence with trailing let, got: {other}"

let testConditionalSequenceSameSourceShape () : TestResult =
    let source = "if false then (() ; 1L) elif true then (() ; 2L) else (() ; 3L)"
    let expected =
        If (
            BoolLiteral false,
            Sequence (UnitLiteral, Int64Literal 1L),
            If (
                BoolLiteral true,
                Sequence (UnitLiteral, Int64Literal 2L),
                Sequence (UnitLiteral, Int64Literal 3L)
            )
        )

    match Parser.parseString false source, InterpreterParser.parseString false source with
    | Ok (Program [Expression compilerExpr]), Ok (Program [Expression interpreterExpr])
        when compilerExpr = expected && interpreterExpr = expected ->
        Ok ()
    | compilerResult, interpreterResult ->
        Error $"Conditional/sequence AST mismatch: compiler={compilerResult}; interpreter={interpreterResult}"

let testConditionalSyntaxErrorsMatch () : TestResult =
    let cases =
        [
            ("if true 1 else 2", "Expected 'then' after if condition")
            ("if false then 1 elif true 2 else 3", "Expected 'then' after elif condition")
        ]

    cases
    |> List.fold (fun result (source, expectedError) ->
        result
        |> Result.bind (fun () ->
            match Parser.parseString false source, InterpreterParser.parseString false source with
            | Error compilerError, Error interpreterError
                when compilerError = expectedError && interpreterError = expectedError ->
                Ok ()
            | compilerResult, interpreterResult ->
                Error
                    $"Conditional parse-error mismatch for '{source}': compiler={compilerResult}; interpreter={interpreterResult}")) (Ok ())

let testInterpreterParserDoesNotTreatTupleBodyAsCallableAcrossTopLevelBoundary () : TestResult =
    let source =
        "let tupleValue () : (Int64, List<Int64>) =\n"
        + "  (0L, [])\n"
        + "0L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on tuple-body top-level boundary: {err}"
    | Ok (Program [FunctionDef fnDef; Expression (Int64Literal 0L)]) ->
        match fnDef.Body with
        | TupleLiteral [Int64Literal 0L; ListLiteral []] ->
            Ok ()
        | other ->
            Error $"Expected tuple literal body, got: {other}"
    | Ok (Program [FunctionDef fnDef]) ->
        Error $"Expected trailing top-level expression after function body, got function-only program with body: {fnDef.Body}"
    | Ok other ->
        Error $"Unexpected AST for tuple-body top-level boundary: {other}"

let testTypeCheckInterpreterRecordFunctionFieldLambda () : TestResult =
    let source =
        "type RecordWithFn = { fn: Int64 -> Int64 }\n"
        + "(let record = RecordWithFn { fn = fun x -> x + 1L } in record.fn 6L)"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed before type checking record function field lambda: {err}"
    | Ok ast ->
        match TypeChecking.checkProgram ast with
        | Error err ->
            Error $"Type checking failed for record function field lambda: {typeErrorToString err}"
        | Ok (resultType, _) when resultType = AST.TInt64 ->
            Ok ()
        | Ok (resultType, _) ->
            Error $"Expected TInt64 result for record function field lambda, got {typeToString resultType}"

let testStdlibRegistryExcludesNonIntrinsicFloatMultiply () : TestResult =
    let registry = Stdlib.buildModuleRegistry ()
    match Map.tryFind "Stdlib.Float.multiply" registry with
    | Some _ ->
        Error "Expected Stdlib.Float.multiply to be omitted from module registry (non-intrinsic)"
    | None ->
        Ok ()

let testStdlibRegistryIncludesIntrinsicFloatSqrt () : TestResult =
    let registry = Stdlib.buildModuleRegistry ()
    match Map.tryFind "Stdlib.Float.sqrt" registry with
    | Some _ -> Ok ()
    | None -> Error "Expected Stdlib.Float.sqrt to remain in module registry (intrinsic)"

let testCompilerParserParsesBacktickIdentifiers () : TestResult =
    let source =
        "type Sample = { ``true``: Bool, ``false``: Bool }\n"
        + "Sample { ``true`` = true, ``false`` = false }"
    match Parser.parseString false source with
    | Error err ->
        Error $"Compiler parser failed on backtick identifiers: {err}"
    | Ok (Program [TypeDef _; Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for compiler backtick identifiers: {other}"

let testInterpreterParserParsesBareIntLiteral () : TestResult =
    let source = "let x = 5 in x"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on bare integer literal: {err}"
    | Ok (Program [Expression (Let (LPVariable "x", BigIntLiteral value, Var "x"))])
        when value = System.Numerics.BigInteger(5) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for bare integer literal: {other}"

let testInterpreterParserParsesUpstreamIntSuffixLiteral () : TestResult =
    let source = "1I"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on upstream I-suffixed integer literal: {err}"
    | Ok (Program [Expression (BigIntLiteral n)]) when n = System.Numerics.BigInteger.One ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for upstream I-suffixed integer literal: {other}"

let testInterpreterParserParsesOversizedUpstreamIntSuffixLiteral () : TestResult =
    let source = "1606938044258990275541962092341162602522202993782792835301376I"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on oversized upstream I-suffixed integer literal: {err}"
    | Ok (Program [Expression (BigIntLiteral n)]) when n.ToString() = "1606938044258990275541962092341162602522202993782792835301376" ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for oversized upstream I-suffixed integer literal: {other}"

let testInterpreterParserParsesNegativeInt8MinLiteral () : TestResult =
    let source = "Stdlib.Int128.fromInt8_v0 -128y"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on negative Int8 minimum literal: {err}"
    | Ok (Program [Expression (Call ("Stdlib.Int128.fromInt8_v0", args))]) ->
        match NonEmptyList.toList args with
        | [Int8Literal n] when n = -128y ->
            Ok ()
        | other ->
            Error $"Unexpected args for negative Int8 minimum literal parse: {other}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for negative Int8 minimum literal parse: {expr}"
    | Ok other ->
        Error $"Expected single expression program for negative Int8 minimum literal parse, got: {other}"

let testInterpreterParserParsesCommaSeparatedLists () : TestResult =
    let source = "[1L, 2L, 3L]"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on comma-separated list literal: {err}"
    | Ok (Program [Expression (ListLiteral [Int64Literal 1L; Int64Literal 2L; Int64Literal 3L])]) ->
        Ok ()
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for comma-separated list literal: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testInterpreterParserParsesNewlineDelimitedListElements () : TestResult =
    let source =
        "[ EnumTestRecord\n"
        + "    { x = \"goodbye\"\n"
        + "      y = MyEnum.B }\n"
        + "  EnumTestRecord\n"
        + "    { x = \"hello\"\n"
        + "      y = MyEnum.A } ]"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on newline-delimited list elements: {err}"
    | Ok (Program [Expression (ListLiteral elements)]) ->
        if List.length elements = 2 then
            Ok ()
        else
            Error $"Expected 2 elements in newline-delimited list literal, got {List.length elements}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for newline-delimited list literal: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

let testBothParsersNormalizeListSeparators () : TestResult =
    let source = "[1L +\n0L; 2L // layout comment\n3L,]"
    let expected =
        Program
            [Expression
                (ListLiteral
                    [BinOp (Add, Int64Literal 1L, Int64Literal 0L)
                     Int64Literal 2L
                     Int64Literal 3L])]
    match Parser.parseString false source, InterpreterParser.parseString false source with
    | Ok compilerProgram, Ok interpreterProgram
        when compilerProgram = expected && interpreterProgram = expected ->
        Ok ()
    | Error error, _ | _, Error error ->
        Error $"List separator probe failed to parse: {error}"
    | other ->
        Error $"List separators did not normalize to one AST: {other}"

let testBothParsersNormalizeRightAssociativeConsPatterns () : TestResult =
    let source = "match xs with | head :: second :: tail -> head | [] -> 0L"
    let hasCanonicalConsPattern program =
        match program with
        | Program [Expression (Match (Var "xs", firstCase :: _))] ->
            match firstCase.Patterns.Head with
            | PListCons ([PVar "head"; PVar "second"], PVar "tail") -> true
            | _ -> false
        | _ -> false

    match Parser.parseString false source, InterpreterParser.parseString false source with
    | Ok compilerProgram, Ok interpreterProgram
        when hasCanonicalConsPattern compilerProgram && hasCanonicalConsPattern interpreterProgram ->
        Ok ()
    | Error error, _ | _, Error error ->
        Error $"Cons-pattern probe failed to parse: {error}"
    | other ->
        Error $"Cons patterns did not normalize to canonical right-associative PListCons: {other}"

let testBothParsersNormalizeRightAssociativeListAppend () : TestResult =
    let source = "[1L] @ [2L] @ [3L]"
    let isCanonicalAppend program =
        match program with
        | Program [Expression (Call ("Stdlib.List.append", outerArgs))] ->
            match NonEmptyList.toList outerArgs with
            | [ListLiteral [Int64Literal 1L]; Call ("Stdlib.List.append", innerArgs)] ->
                NonEmptyList.toList innerArgs =
                    [ListLiteral [Int64Literal 2L]; ListLiteral [Int64Literal 3L]]
            | _ -> false
        | _ -> false

    match Parser.parseString false source, InterpreterParser.parseString false source with
    | Ok compilerProgram, Ok interpreterProgram
        when isCanonicalAppend compilerProgram && isCanonicalAppend interpreterProgram ->
        Ok ()
    | Error error, _ | _, Error error ->
        Error $"List-append probe failed to parse: {error}"
    | other ->
        Error $"List append did not normalize to right-associative List.append calls: {other}"

let testBothParsersRejectListSpread () : TestResult =
    let expressionSpread = "[1L, ...tail]"
    let patternSpread = "match xs with | [head, ...tail] -> head | _ -> 0L"
    match
        Parser.parseString false expressionSpread,
        InterpreterParser.parseString false expressionSpread,
        Parser.parseString false patternSpread,
        InterpreterParser.parseString false patternSpread
    with
    | Error _, Error _, Error _, Error _ -> Ok ()
    | results -> Error $"Former list spread syntax was unexpectedly accepted: {results}"

let testInterpreterParserParsesBacktickIdentifiers () : TestResult =
    let source =
        "type Sample = { ``ALLCAPS``: Int64; ``true``: Bool }\n"
        + "Sample { ``ALLCAPS`` = 1L; ``true`` = true }"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on backtick identifiers: {err}"
    | Ok (Program [TypeDef _; Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for backtick identifiers: {other}"

let testInterpreterParserDoesNotCrossRecordFieldBoundaryWithApplication () : TestResult =
    let source =
        "type Sample = { negInfinity: Float; ``true``: Bool }\n"
        + "Sample\n"
        + "  { negInfinity = Builtin.testNegativeInfinity\n"
        + "    ``true`` = true }"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser crossed record field boundary while parsing application: {err}"
    | Ok (Program [TypeDef _; Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for record-field boundary application case: {other}"

let testInterpreterParserDoesNotCrossRecordFieldBoundaryWithQualifiedConstructor () : TestResult =
    let source =
        "type Sample = { option: Stdlib.Option.Option<Int64>; next: Int64 }\n"
        + "Sample\n"
        + "  { option = Stdlib.Option.Option.None\n"
        + "    next = 5L }"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser crossed record field boundary while parsing qualified constructor payload: {err}"
    | Ok (Program [TypeDef _; Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for qualified-constructor record-field boundary case: {other}"

let testInterpreterParserRejectsBareTupleExpression () : TestResult =
    let source = "1L, 2L, 3L"
    match InterpreterParser.parseString false source with
    | Error _ -> Ok ()
    | Ok _ -> Error "Interpreter parser accepted a bare tuple expression outside a match scrutinee"

let testInterpreterTupleSyntaxBoundaries () : TestResult =
    let rejectedSources = [ "(1L,)"; "let pair = (1L, 2L) in pair.0" ]
    let rejected =
        rejectedSources
        |> List.forall (fun source ->
            match InterpreterParser.parseString false source with
            | Error _ -> true
            | Ok _ -> false)
    match InterpreterParser.parseString false "match 1L, 2L with | (a, b) -> a" with
    | Ok _ when rejected -> Ok ()
    | Ok _ -> Error "Interpreter parser accepted singleton/projection tuple syntax"
    | Error err -> Error $"Interpreter parser rejected bare match tuple scrutinee: {err}"

let testInterpreterParserParsesPipeOperatorSections () : TestResult =
    let source = "5L |> (*) 2L |> (<) 40L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on pipe operator sections: {err}"
    | Ok (Program [Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for pipe operator sections: {other}"

let testInterpreterParserParsesQualifiedRecordLiteral () : TestResult =
    let source = "Foo.Bar { name = \"a\"; ``type`` = 1L }"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on qualified record literal: {err}"
    | Ok (Program [Expression (RecordLiteral (reference, fields))]) when reference.SourceTypeName = "Foo.Bar" ->
        let expected = [("name", StringLiteral "a"); ("type", Int64Literal 1L)]
        if fields = expected then
            Ok ()
        else
            Error $"Unexpected fields for qualified record literal: {fields}"
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for qualified record literal: {expr}"
    | Ok other ->
        Error $"Unexpected program shape for qualified record literal: {other}"

let testInterpreterParserParsesConstructorOverApplicationChain () : TestResult =
    let source = "33L |> MyEnum.A 21L 42L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on constructor over-application chain: {err}"
    | Ok (Program [Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST/program shape for constructor over-application chain: {other}"

let testInterpreterParserRejectsTypedLambdaParameters () : TestResult =
    let source =
        "fun (x: '__interp_lambda_0_0_x) (y: '__interp_lambda_0_1_y) -> x"

    match InterpreterParser.parseString false source with
    | Error err when err.Contains "Lambda parameter annotations are not supported" -> Ok ()
    | Error err -> Error $"Unexpected typed-lambda diagnostic: {err}"
    | Ok other -> Error $"Expected typed lambda parameters to be rejected, got: {other}"

let testInterpreterParserRejectsCompilerOnlyAdtForms () : TestResult =
    let rejectedSources = [
        "type NoLeadingBar = NoLeadingBarCase\nNoLeadingBarCase"
        "type BareGeneric<a> = | BareGenericCase of a\nBareGeneric.BareGenericCase 1L"
        "type Qualified.Declaration = | QualifiedCase\nQualified.Declaration.QualifiedCase"
    ]
    let rec verify sources =
        match sources with
        | [] -> Ok ()
        | source :: rest ->
            match InterpreterParser.parseString false source with
            | Error _ -> verify rest
            | Ok parsed ->
                match TypeChecking.checkInterpreterProgram parsed with
                | Error _ -> verify rest
                | Ok _ -> Error $"Expected compiler-only ADT syntax to be rejected, got: {parsed}"
    verify rejectedSources

let testQualifiedNameSegmentsPreserveQuotedDots () : TestResult =
    match NameSyntax.tryParseLegacySpelling "A.``b.c``" with
    | None -> Error "Expected the qualified name to parse"
    | Some name ->
        let segments =
            name
            |> NameSyntax.segments
            |> List.map NameSyntax.identifierText
        if segments = ["A"; "b.c"] then Ok ()
        else Error $"Expected two lossless segments, got: {segments}"

let testQualificationStopsAtLowercaseField () : TestResult =
    match Parser.parseString false "A.value.field" with
    | Ok (Program [Expression (RecordAccess (Var "A.value", "field"))]) -> Ok ()
    | Error error -> Error $"Qualified-field probe failed to parse: {error}"
    | Ok other -> Error $"Unexpected qualified-field AST: {other}"

let testBlankIdentifierHasExplicitTypedShape () : TestResult =
    match
        NameSyntax.classify "___",
        NameSyntax.classify "",
        NameSyntax.tryParseLegacySpelling "A..B",
        NameSyntax.tryParseLegacySpelling "A.````"
    with
    | NameSyntax.IdentifierToken NameSyntax.BlankIdentifier,
      NameSyntax.IdentifierToken NameSyntax.BlankIdentifier,
      None,
      Some quotedBlank ->
        match NameSyntax.segments quotedBlank with
        | [NameSyntax.OrdinaryIdentifier "A"; NameSyntax.BlankIdentifier] -> Ok ()
        | other -> Error $"Unexpected quoted blank qualified shape: {other}"
    | other -> Error $"Expected typed blanks and malformed empty-segment rejection, got: {other}"

let testModuleBlockNormalizesAtExplicitBoundary () : TestResult =
    let source = "module Outer =\n  module Inner =\n    let value(x: Int64) : Int64 = x"
    match InterpreterParser.parseSourceString false source, Parser.parseString false source with
    | Ok (NameSyntax.SourceModule (outer, NameSyntax.SourceModule (inner, _))),
      Ok (Program [FunctionDef definition])
        when NameSyntax.formatQualifiedName outer = "Outer"
             && NameSyntax.formatQualifiedName inner = "Inner"
             && definition.Name = "Outer.Inner.value" -> Ok ()
    | Error error, _ | _, Error error -> Error $"Module block failed to parse: {error}"
    | other -> Error $"Unexpected parsed/normalized module shapes: {other}"

let testTopLevelValueHasParsedShapeBeforeAotBoundary () : TestResult =
    let source = "val answer = 42"
    match Parser.parseSourceString false source, Parser.parseString false source with
    | Ok (NameSyntax.SourceDeclarations declarations), Error error ->
        match NonEmptyList.toList declarations with
        | [NameSyntax.SourceValue (NameSyntax.OrdinaryIdentifier "answer", Int64Literal 42L)]
            when error.Contains "native execution is not supported" -> Ok ()
        | other -> Error $"Unexpected parsed value declaration: {other}"
    | other -> Error $"Unexpected value parse/normalization results: {other}"

let private parsedUnit name purpose source : Result<NameSyntax.ParsedSourceUnit, string> =
    NameSyntax.sourceUnitName name
    |> Result.bind (fun unitName ->
        InterpreterParser.parseSourceString false source
        |> Result.map (fun parsed ->
            { NameSyntax.ParsedSourceUnit.Name = unitName
              Purpose = purpose
              Source = parsed }))

let testSourceProgramPreservesUnitOrderAndPurpose () : TestResult =
    match
        parsedUnit "dependency.dark" NameSyntax.SourceUnitPurpose.Library "let helper(x: Int64) : Int64 = x",
        parsedUnit "app.dark" NameSyntax.SourceUnitPurpose.Executable "helper 42L"
    with
    | Ok dependency, Ok executable ->
        let program =
            NameSyntax.createSourceProgram
                (NonEmptyList.fromList [dependency; executable])
        match NameSyntax.sourceUnits program, NameSyntax.validateExecutableProgram program with
        | [first; second], Ok validated
            when NameSyntax.sourceUnitNameText first.Name = "dependency.dark"
                 && first.Purpose = NameSyntax.SourceUnitPurpose.Library
                 && NameSyntax.sourceUnitNameText second.Name = "app.dark"
                 && NameSyntax.sourceUnitNameText (NameSyntax.validatedEntry validated).SourceUnit = "app.dark" -> Ok ()
        | other -> Error $"Unexpected composed source program: {other}"
    | Error error, _ | _, Error error -> Error error

let testSourceProgramRejectsEntryInDependencyUnit () : TestResult =
    match parsedUnit "dependency.dark" NameSyntax.SourceUnitPurpose.Package "42L" with
    | Error error -> Error error
    | Ok dependency ->
        let program = NameSyntax.createSourceProgram (NonEmptyList.singleton dependency)
        match NameSyntax.validateExecutableProgram program with
        | Error error when error.Contains("must contain declarations only") -> Ok ()
        | other -> Error $"Expected dependency-entry rejection, got: {other}"

let testSourceProgramRequiresExactlyOneEntry () : TestResult =
    match
        parsedUnit "one.dark" NameSyntax.SourceUnitPurpose.Executable "1L",
        parsedUnit "two.dark" NameSyntax.SourceUnitPurpose.Executable "2L"
    with
    | Ok one, Ok two ->
        let program = NameSyntax.createSourceProgram (NonEmptyList.fromList [one; two])
        match NameSyntax.validateExecutableProgram program with
        | Error error when error.Contains("exactly one") && error.EndsWith("found 2") -> Ok ()
        | other -> Error $"Expected multiple-entry rejection, got: {other}"
    | Error error, _ | _, Error error -> Error error

let testDeclarationProgramNeedsNoDummyEntry () : TestResult =
    match parsedUnit "library.dark" NameSyntax.SourceUnitPurpose.Library "let id(x: Int64) : Int64 = x" with
    | Error error -> Error error
    | Ok library ->
        let program = NameSyntax.createSourceProgram (NonEmptyList.singleton library)
        match NameSyntax.validateDeclarationProgram program with
        | Ok _ -> Ok ()
        | Error error -> Error $"Declaration-only program should validate: {error}"

let tests = [
    ("compiler library interpreter parse", testCompilerLibraryParseInterpreterSyntax)
    ("parse interpreter lambda/application", testParseInterpreterLambdaApplication)
    ("parse interpreter nested function after let binding", testParseInterpreterNestedFunctionAfterLetBinding)
    ("parse interpreter wildcard lambda parameter", testInterpreterParserParsesWildcardLambdaParameter)
    ("parse interpreter triple-quoted interpolation", testParseInterpreterTripleQuotedInterpolation)
    ("parse interpreter negative float application args", testParseInterpreterNegativeFloatApplicationArgs)
    ("parse interpreter pipe minus operator section", testParseInterpreterPipeMinusOperatorSection)
    ("parse interpreter legacy Int suffix", testInterpreterParserParsesLegacyIntSuffix)
    ("parse compiler apostrophe type argument call site", testCompilerParserParsesApostropheTypeArgAtCallSite)
    ("parse compiler apostrophe type argument space call site", testCompilerParserParsesApostropheTypeArgSpaceCallSite)
    ("parse interpreter bare function type argument call site", testInterpreterParserParsesBareFunctionTypeArgAtCallSite)
    ("parse interpreter Dict<Value> shorthand type argument call site", testInterpreterParserParsesSingleTypeArgDictShorthandAtCallSite)
    ("parse interpreter star-tuple type argument call site", testInterpreterParserParsesStarTupleTypeArgAtCallSite)
    ("parse interpreter unicode escape sequences in strings", testInterpreterParserParsesUnicodeEscapeSequencesInStrings)
    ("parse interpreter apostrophe type params in function def", testInterpreterParserParsesApostropheTypeParamsInFunctionDef)
    ("parse interpreter apostrophe type var in annotation", testInterpreterParserParsesApostropheTypeVarInTypeAnnotation)
    ("parse interpreter apostrophe tuple type vars in annotation", testInterpreterParserParsesApostropheTupleTypeVarsInTypeAnnotation)
    ("parse interpreter apostrophe-suffixed identifier name", testInterpreterParserParsesApostropheSuffixedIdentifierName)
    ("parse interpreter escaped single quote string literal", testInterpreterParserParsesEscapedSingleQuoteInStringLiteral)
    ("parse interpreter named variant payloads", testInterpreterParserParsesNamedVariantPayloads)
    ("parse interpreter named tuple variant payloads", testInterpreterParserParsesNamedTupleVariantPayloads)
    ("parse interpreter parenthesized function type annotation", testInterpreterParserParsesParenthesizedFunctionTypeAnnotation)
    ("parse interpreter elif chains", testInterpreterParserParsesElifChains)
    ("parse interpreter all-underscore identifier", testInterpreterParserAllowsAllUnderscoreIdentifiers)
    ("parse interpreter curried top-level let function def", testInterpreterParserParsesCurriedTopLevelLetFunctionDef)
    ("parse compiler curried function def", testCompilerParserParsesCurriedFunctionDef)
    ("parse interpreter record function field type", testParseInterpreterRecordFunctionFieldType)
    ("parse interpreter newline-delimited let body", testParseInterpreterNewlineDelimitedLetBody)
    ("parse interpreter newline-delimited let body after applied call value", testParseInterpreterNewlineDelimitedLetBodyAfterAppliedCallValue)
    ("parse interpreter local let function body", testParseInterpreterLocalLetFunctionBody)
    ("parse interpreter parenthesized sequence with trailing let", testParseInterpreterParenthesizedSequenceWithTrailingLet)
    ("parse conditional sequence to same syntax shape", testConditionalSequenceSameSourceShape)
    ("conditional syntax errors match", testConditionalSyntaxErrorsMatch)
    ("parse interpreter tuple-body top-level boundary", testInterpreterParserDoesNotTreatTupleBodyAsCallableAcrossTopLevelBoundary)
    ("typecheck interpreter record-function-field lambda", testTypeCheckInterpreterRecordFunctionFieldLambda)
    ("stdlib registry excludes non-intrinsic float multiply", testStdlibRegistryExcludesNonIntrinsicFloatMultiply)
    ("stdlib registry includes intrinsic float sqrt", testStdlibRegistryIncludesIntrinsicFloatSqrt)
    ("parse compiler backtick identifiers", testCompilerParserParsesBacktickIdentifiers)
    ("parse bare int literal", testInterpreterParserParsesBareIntLiteral)
    ("parse upstream I-suffixed int literal", testInterpreterParserParsesUpstreamIntSuffixLiteral)
    ("parse oversized upstream I-suffixed int literal", testInterpreterParserParsesOversizedUpstreamIntSuffixLiteral)
    ("parse negative int8 minimum literal", testInterpreterParserParsesNegativeInt8MinLiteral)
    ("parse comma-separated lists", testInterpreterParserParsesCommaSeparatedLists)
    ("parse newline-delimited list elements", testInterpreterParserParsesNewlineDelimitedListElements)
    ("normalize list separators in both parsers", testBothParsersNormalizeListSeparators)
    ("normalize right-associative cons patterns in both parsers", testBothParsersNormalizeRightAssociativeConsPatterns)
    ("normalize right-associative list append in both parsers", testBothParsersNormalizeRightAssociativeListAppend)
    ("reject list spread in both parsers", testBothParsersRejectListSpread)
    ("parse backtick identifiers", testInterpreterParserParsesBacktickIdentifiers)
    ("record field boundary blocks space application", testInterpreterParserDoesNotCrossRecordFieldBoundaryWithApplication)
    ("record field boundary blocks qualified constructor payload", testInterpreterParserDoesNotCrossRecordFieldBoundaryWithQualifiedConstructor)
    ("reject bare tuple expression", testInterpreterParserRejectsBareTupleExpression)
    ("interpreter tuple syntax boundaries", testInterpreterTupleSyntaxBoundaries)
    ("parse pipe operator sections", testInterpreterParserParsesPipeOperatorSections)
    ("parse qualified record literal", testInterpreterParserParsesQualifiedRecordLiteral)
    ("parse constructor over-application chain", testInterpreterParserParsesConstructorOverApplicationChain)
    ("typed lambda parameters are rejected", testInterpreterParserRejectsTypedLambdaParameters)
    ("reject compiler-only ADT forms in interpreter syntax", testInterpreterParserRejectsCompilerOnlyAdtForms)
    ("quoted dots remain one qualified segment", testQualifiedNameSegmentsPreserveQuotedDots)
    ("qualification stops at lowercase field", testQualificationStopsAtLowercaseField)
    ("blank identifiers use an explicit typed shape", testBlankIdentifierHasExplicitTypedShape)
    ("module blocks normalize at explicit boundary", testModuleBlockNormalizesAtExplicitBoundary)
    ("top-level values reach the explicit AOT boundary", testTopLevelValueHasParsedShapeBeforeAotBoundary)
    ("source program preserves unit order and purpose", testSourceProgramPreservesUnitOrderAndPurpose)
    ("source program rejects dependency entries", testSourceProgramRejectsEntryInDependencyUnit)
    ("source program requires exactly one entry", testSourceProgramRequiresExactlyOneEntry)
    ("declaration program needs no dummy entry", testDeclarationProgramNeedsNoDummyEntry)
]
