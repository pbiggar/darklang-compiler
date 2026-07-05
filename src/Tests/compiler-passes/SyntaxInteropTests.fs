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
        | Let ("inc", Lambda (parameters, body), Call ("inc", callArgs)) ->
            let paramNames = parameters |> NonEmptyList.toList |> List.map fst
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
    | Ok (Program [Expression (Let ("limit", Int64Literal 10L, Let ("sumUpTo", Lambda (parameters, body), Call ("sumUpTo", callArgs))))]) ->
        match NonEmptyList.toList parameters, body, NonEmptyList.toList callArgs with
        | [("i", AST.TInt64)], If (BinOp (Gt, Var "i", Var "limit"), Int64Literal 0L, BinOp (Add, Var "i", Call ("sumUpTo", recursiveArgs))), [Int64Literal 1L] ->
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
    | Ok (Program [Expression (Lambda (parameters, Int64Literal 1L))]) ->
        match NonEmptyList.toList parameters with
        | [ (paramName, TVar typeVarName) ] when paramName.StartsWith "lambdaWildcard" && typeVarName.StartsWith "__interp_lambda_" ->
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
        | Apply (Lambda (parameters, BinOp (Sub, Var leftName, Int64Literal 3L)), args) ->
            let paramNames = parameters |> NonEmptyList.toList |> List.map fst
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
    | Ok (Program [Expression (Int128Literal value)]) when value = System.Int128.One ->
        Ok ()
    | Ok other ->
        Error $"Expected Int128 literal program for legacy Int suffix, got: {other}"

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
    let source = "fun (default': 'a) -> default'"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on apostrophe-suffixed identifier name: {err}"
    | Ok (Program [Expression (Lambda (parameters, Var varName))]) ->
        match NonEmptyList.toList parameters with
        | [ ("default'", TVar "a") ] when varName = "default'" ->
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
            | Constructor ("Inner", "B", Some (Constructor ("Stdlib.Result.Result", "Ok", Some (Constructor ("Inner", "A", Some (Int64Literal 5L)))))) ->
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
    | Ok (Program [TypeDef (SumTypeDef ("EnumOfMixedCases", [], variants)); Expression (Constructor ("EnumOfMixedCases", "W", None))]) ->
        match variants with
        | [ { Name = "W"; Payload = None }
            { Name = "X"; Payload = Some AST.TString }
            { Name = "Y"; Payload = Some AST.TInt64 }
            { Name = "Z"; Payload = Some (AST.TTuple [AST.TString; AST.TInt64]) } ] ->
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
    | Ok (Program [Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Expected single expression program for parenthesized sequence with trailing let, got: {other}"

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

let testPrettyPrintInterpreterSyntax () : TestResult =
    let source = "let x = 5 in Stdlib.Int64.add(x, 1)"
    match Parser.parseString false source with
    | Error err -> Error $"Compiler parser failed: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram InterpreterSyntax ast
        let expected = "let x = 5L in Stdlib.Int64.add x 1L"
        if printed = expected then
            Ok ()
        else
            Error $"Interpreter pretty-printer mismatch. Expected '{expected}', got '{printed}'"

let testPrettyPrintInterpreterSyntaxParenthesizesNestedCallArg () : TestResult =
    let source = "Builtin.unwrap(Stdlib.List.head(xs))"
    match Parser.parseString false source with
    | Error err -> Error $"Compiler parser failed: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram InterpreterSyntax ast
        let expected = "Builtin.unwrap (Stdlib.List.head xs)"
        if printed = expected then
            Ok ()
        else
            Error $"Interpreter nested-call argument pretty-print mismatch. Expected '{expected}', got '{printed}'"

let testPrettyPrintCompilerSyntax () : TestResult =
    let source = "let x = 5L in Stdlib.Int64.add x 1L"
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram CompilerSyntax ast
        let expected = "let x = 5 in Stdlib.Int64.add(x, 1)"
        if printed = expected then
            Ok ()
        else
            Error $"Compiler pretty-printer mismatch. Expected '{expected}', got '{printed}'"

let testPrettyPrintCompilerSyntaxPreservesConstructorApplyRoundtrip () : TestResult =
    let source = "3L |> Stdlib.Result.Result.Ok"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed for constructor-apply compiler roundtrip source: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram CompilerSyntax ast
        match Parser.parseString false printed with
        | Error err ->
            Error $"Compiler parser failed reparsing constructor-apply pretty output: {err}\nPrinted: {printed}"
        | Ok reparsedAst ->
            if reparsedAst = ast then
                Ok ()
            else
                Error $"Constructor apply AST changed after compiler pretty roundtrip.\nPrinted: {printed}\nOriginal: {ast}\nReparsed: {reparsedAst}"

let testPrettyPrintInterpreterSyntaxPreservesConstructorApplyRoundtrip () : TestResult =
    let source = "3L |> Stdlib.Result.Result.Ok"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed for constructor-apply interpreter roundtrip source: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram InterpreterSyntax ast
        match InterpreterParser.parseString false printed with
        | Error err ->
            Error $"Interpreter parser failed reparsing constructor-apply interpreter pretty output: {err}\nPrinted: {printed}"
        | Ok reparsedAst ->
            if reparsedAst = ast then
                Ok ()
            else
                Error $"Constructor apply AST changed after interpreter pretty roundtrip.\nPrinted: {printed}\nOriginal: {ast}\nReparsed: {reparsedAst}"

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

let testPrettyPrintCompilerSyntaxEscapesKeywordFieldNames () : TestResult =
    let source =
        "type Sample = { ``true``: Bool; ``false``: Bool }\n"
        + "Sample { ``true`` = true; ``false`` = false }"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed before compiler pretty-print keyword-escape test: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram CompilerSyntax ast
        if not (printed.Contains "``true``") || not (printed.Contains "``false``") then
            Error $"Compiler pretty-printer failed to escape keyword field names: {printed}"
        else
            match Parser.parseString false printed with
            | Error err ->
                Error $"Compiler parser failed to parse escaped keyword field names: {err}\nPrinted: {printed}"
            | Ok reparsedAst ->
                if reparsedAst = ast then
                    Ok ()
                else
                    Error $"Compiler pretty-print escaped keyword field names changed AST.\nPrinted: {printed}\nReparsed: {reparsedAst}"

let testInterpreterParserParsesBareIntLiteral () : TestResult =
    let source = "let x = 5 in x"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on bare integer literal: {err}"
    | Ok (Program [Expression (Let ("x", Int64Literal 5L, Var "x"))]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for bare integer literal: {other}"

let testInterpreterParserParsesUpstreamIntSuffixLiteral () : TestResult =
    let source = "1I"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed on upstream I-suffixed integer literal: {err}"
    | Ok (Program [Expression (Int128Literal n)]) when n = System.Int128.One ->
        Ok ()
    | Ok other ->
        Error $"Unexpected AST for upstream I-suffixed integer literal: {other}"

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

let testInterpreterParserRejectsCompilerLambdaSyntax () : TestResult =
    let source = "let inc = (x: Int64) => x + 1 in inc 4L"
    match InterpreterParser.parseString false source with
    | Ok _ -> Error "Expected interpreter parser to reject compiler lambda syntax '(x: T) => ...'"
    | Error _ -> Ok ()

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

let testInterpreterParserParsesBareTupleExpression () : TestResult =
    let source = "1L, 2L, 3L"
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Interpreter parser failed on bare tuple expression: {err}"
    | Ok (Program [Expression (TupleLiteral [Int64Literal 1L; Int64Literal 2L; Int64Literal 3L])]) ->
        Ok ()
    | Ok (Program [Expression expr]) ->
        Error $"Unexpected AST for bare tuple expression: {expr}"
    | Ok other ->
        Error $"Expected single expression program, got: {other}"

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
    | Ok (Program [Expression (RecordLiteral ("Foo.Bar", fields))]) ->
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

let testPrettyPrintInterpreterSyntaxPreservesImplicitCurriedLambdaRoundtrip () : TestResult =
    let source = "fun x y -> x"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed for implicit curried lambda roundtrip source: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram InterpreterSyntax ast
        match InterpreterParser.parseString false printed with
        | Error err ->
            Error $"Interpreter parser failed when reparsing pretty-printed implicit curried lambda: {err}\nPrinted: {printed}"
        | Ok reparsedAst ->
            if reparsedAst = ast then
                Ok ()
            else
                Error $"Implicit curried lambda AST changed after interpreter pretty roundtrip.\nPrinted: {printed}\nOriginal: {ast}\nReparsed: {reparsedAst}"

let testPrettyPrintInterpreterSyntaxPreservesTupleApplyInsideListRoundtrip () : TestResult =
    let source =
        "(Stdlib.Html.link\n"
        + "  [ (\"rel\", Stdlib.Option.Option.Some \"stylesheet\")\n"
        + "    (\"href\", Stdlib.Option.Option.Some \"./style.css\") ])\n"
        + "|> nodeToString"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed for tuple-apply-in-list roundtrip source: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram InterpreterSyntax ast
        match InterpreterParser.parseString false printed with
        | Error err ->
            Error $"Interpreter parser failed reparsing tuple-apply-in-list pretty output: {err}\nPrinted: {printed}"
        | Ok reparsedAst ->
            if reparsedAst = ast then
                Ok ()
            else
                Error $"Tuple-apply-in-list AST changed after interpreter pretty roundtrip.\nPrinted: {printed}\nOriginal: {ast}\nReparsed: {reparsedAst}"

let testPrettyPrintInterpreterSyntaxPreservesNestedTupleApplyInsideListRoundtrip () : TestResult =
    let source =
        "(Stdlib.Html.input\n"
        + "  [ (\"type\", Stdlib.Option.Option.Some \"text\")\n"
        + "    (\"name\", Stdlib.Option.Option.Some \"name\")\n"
        + "    (\"id\", Stdlib.Option.Option.Some \"name\") ])\n"
        + "|> nodeToString"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed for nested tuple-apply-in-list roundtrip source: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram InterpreterSyntax ast
        match InterpreterParser.parseString false printed with
        | Error err ->
            Error $"Interpreter parser failed reparsing nested tuple-apply-in-list pretty output: {err}\nPrinted: {printed}"
        | Ok reparsedAst ->
            if reparsedAst = ast then
                Ok ()
            else
                Error $"Nested tuple-apply-in-list AST changed after interpreter pretty roundtrip.\nPrinted: {printed}\nOriginal: {ast}\nReparsed: {reparsedAst}"

let testPrettyPrintInterpreterSyntaxPreservesUncurriedLambdaApplyRoundtrip () : TestResult =
    let source =
        "let v =\n"
        + "    Stdlib.String.map \"a string\" (fun x ->\n"
        + "      let _ = Builtin.testIncrementSideEffectCounter_v0 false in 'c')\n"
        + "   (v, Builtin.testSideEffectCount_v0 ())"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed for uncurried-lambda-apply roundtrip source: {err}"
    | Ok ast ->
        let printed = ASTPrettyPrinter.formatProgram InterpreterSyntax ast
        match InterpreterParser.parseString false printed with
        | Error err ->
            Error $"Interpreter parser failed reparsing uncurried-lambda-apply pretty output: {err}\nPrinted: {printed}"
        | Ok reparsedAst ->
            let normalizeInterpLambdaSeeds (program: AST.Program) : string =
                let astDebug = sprintf "%A" program
                System.Text.RegularExpressions.Regex.Replace(astDebug, "__interp_lambda_[0-9]+_", "__interp_lambda_N_")

            if reparsedAst = ast || normalizeInterpLambdaSeeds reparsedAst = normalizeInterpLambdaSeeds ast then
                Ok ()
            else
                Error $"Uncurried lambda-apply AST changed after interpreter pretty roundtrip.\nPrinted: {printed}\nOriginal: {ast}\nReparsed: {reparsedAst}"

let testInterpreterParserParsesNestedFunctionAfterLetBinding () : TestResult =
    let source =
        "let limit = 10L\n"
        + "let sumUpTo (i: Int64) : Int64 =\n"
        + "  if i > limit then 0L else i + (sumUpTo (i + 1L))\n"
        + "sumUpTo 1L"
    match InterpreterParser.parseString false source with
    | Error err ->
        Error $"Interpreter parser failed for nested function after let binding: {err}"
    | Ok (Program [Expression _]) ->
        Ok ()
    | Ok other ->
        Error $"Unexpected program shape for nested function after let binding: {other}"

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
    ("parse interpreter record function field type", testParseInterpreterRecordFunctionFieldType)
    ("parse interpreter newline-delimited let body", testParseInterpreterNewlineDelimitedLetBody)
    ("parse interpreter newline-delimited let body after applied call value", testParseInterpreterNewlineDelimitedLetBodyAfterAppliedCallValue)
    ("parse interpreter local let function body", testParseInterpreterLocalLetFunctionBody)
    ("parse interpreter parenthesized sequence with trailing let", testParseInterpreterParenthesizedSequenceWithTrailingLet)
    ("parse interpreter tuple-body top-level boundary", testInterpreterParserDoesNotTreatTupleBodyAsCallableAcrossTopLevelBoundary)
    ("typecheck interpreter record-function-field lambda", testTypeCheckInterpreterRecordFunctionFieldLambda)
    ("stdlib registry excludes non-intrinsic float multiply", testStdlibRegistryExcludesNonIntrinsicFloatMultiply)
    ("stdlib registry includes intrinsic float sqrt", testStdlibRegistryIncludesIntrinsicFloatSqrt)
    ("pretty-print interpreter syntax", testPrettyPrintInterpreterSyntax)
    ("pretty-print interpreter nested call arg", testPrettyPrintInterpreterSyntaxParenthesizesNestedCallArg)
    ("pretty-print interpreter preserves constructor apply roundtrip", testPrettyPrintInterpreterSyntaxPreservesConstructorApplyRoundtrip)
    ("pretty-print compiler syntax", testPrettyPrintCompilerSyntax)
    ("pretty-print compiler preserves constructor apply roundtrip", testPrettyPrintCompilerSyntaxPreservesConstructorApplyRoundtrip)
    ("parse compiler backtick identifiers", testCompilerParserParsesBacktickIdentifiers)
    ("pretty-print compiler escapes keyword field names", testPrettyPrintCompilerSyntaxEscapesKeywordFieldNames)
    ("parse bare int literal", testInterpreterParserParsesBareIntLiteral)
    ("parse upstream I-suffixed int literal", testInterpreterParserParsesUpstreamIntSuffixLiteral)
    ("parse negative int8 minimum literal", testInterpreterParserParsesNegativeInt8MinLiteral)
    ("reject compiler lambda syntax", testInterpreterParserRejectsCompilerLambdaSyntax)
    ("parse comma-separated lists", testInterpreterParserParsesCommaSeparatedLists)
    ("parse newline-delimited list elements", testInterpreterParserParsesNewlineDelimitedListElements)
    ("parse backtick identifiers", testInterpreterParserParsesBacktickIdentifiers)
    ("record field boundary blocks space application", testInterpreterParserDoesNotCrossRecordFieldBoundaryWithApplication)
    ("record field boundary blocks qualified constructor payload", testInterpreterParserDoesNotCrossRecordFieldBoundaryWithQualifiedConstructor)
    ("parse bare tuple expression", testInterpreterParserParsesBareTupleExpression)
    ("parse pipe operator sections", testInterpreterParserParsesPipeOperatorSections)
    ("parse qualified record literal", testInterpreterParserParsesQualifiedRecordLiteral)
    ("parse constructor over-application chain", testInterpreterParserParsesConstructorOverApplicationChain)
    ("pretty-print interpreter preserves implicit curried lambda roundtrip", testPrettyPrintInterpreterSyntaxPreservesImplicitCurriedLambdaRoundtrip)
    ("pretty-print interpreter preserves tuple-apply-in-list roundtrip", testPrettyPrintInterpreterSyntaxPreservesTupleApplyInsideListRoundtrip)
    ("pretty-print interpreter preserves nested tuple-apply-in-list roundtrip", testPrettyPrintInterpreterSyntaxPreservesNestedTupleApplyInsideListRoundtrip)
    ("pretty-print interpreter preserves uncurried-lambda-apply roundtrip", testPrettyPrintInterpreterSyntaxPreservesUncurriedLambdaApplyRoundtrip)
    ("parse interpreter nested function after let binding", testInterpreterParserParsesNestedFunctionAfterLetBinding)
]
