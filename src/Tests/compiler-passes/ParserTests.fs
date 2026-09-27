// ParserTests.fs - Focused syntax and lexer checks for the copied interpreter parser.

module ParserTests

module WT = LibParser.WrittenTypes

type TestResult = Result<unit, string>

let private parseSource source =
    WrittenParsing.parse LibParser.Validation.Script source
    |> Result.map LibParser.Validation.ValidatedSourceFile.toWrittenTypes

let private testLongNumericTokenStreamIsStackSafe () : TestResult =
    let literalCount = 2000
    let source = List.replicate literalCount "0" |> String.concat " "
    match LibParser.Lexer.tokenize source with
    | Ok (tokens, []) when List.length tokens >= literalCount -> Ok ()
    | Ok (tokens, diagnostics) ->
        Error $"Expected {literalCount} numeric tokens without diagnostics; got {tokens.Length} tokens and {diagnostics.Length} diagnostics"
    | Error err -> Error err

let private testTupleLetDoesNotOpenNestedFunctionLayout () : TestResult =
    let source =
        """let pairsToStrings (pairs: List<(String * String)>) : List<String> =
    Stdlib.List.map<(String * String), String> pairs (fun pair -> let (key, value) = pair in key ++ value)
let identity (value: String) : String = value"""
    parseSource source
    |> Result.bind (fun parsed ->
        match parsed.declarations with
        | [WT.DFunction _; WT.DFunction _] -> Ok ()
        | declarations -> Error $"Expected two function declarations, got {declarations.Length}")

let private testSpaceApplicationKeepsMultipleArguments () : TestResult =
    parseSource "let recurse (a: Int8) (b: Int8) : Int8 = recurse a b"
    |> Result.bind (fun parsed ->
        match parsed.declarations with
        | [WT.DFunction { body = WT.EApply (_, _, _, [WT.EVariable (_, "a"); WT.EVariable (_, "b")]) }] -> Ok ()
        | declarations -> Error $"Expected a two-argument call, got {declarations}")

let private testSubtractionFollowsParenthesizedCall () : TestResult =
    parseSource "let dropLast (value: String) : Int64 = (Stdlib.String.__byteLength value) - 1L"
    |> Result.bind (fun parsed ->
        match parsed.declarations with
        | [WT.DFunction { body = WT.EInfix (_, (_, WT.InfixFnCall WT.ArithmeticMinus), WT.EApply _, WT.EInt64 _) }] -> Ok ()
        | declarations -> Error $"Expected subtraction after a call, got {declarations}")

let private testTopLevelExpressionFollowsFunctionDeclaration () : TestResult =
    parseSource "let identity (value: Int64) : Int64 = value\n\nidentity 1L"
    |> Result.bind (fun parsed ->
        match parsed.declarations, parsed.exprsToEval with
        | [WT.DFunction _], [WT.EApply _] -> Ok ()
        | declarations, expressions ->
            Error $"Expected a function then an entry expression, got {declarations.Length} declarations and {expressions.Length} expressions")

let private testModuleExpressionRetainsItsResolutionScope () : TestResult =
    WrittenParsing.parse LibParser.Validation.Script "module Darklang.Example.Nested\n\n1L"
    |> Result.bind WrittenSource.items
    |> Result.bind (function
        | [WrittenSource.Expression (["Darklang"; "Example"; "Nested"], WT.EInt64 _)] -> Ok ()
        | items -> Error $"Expected a module-scoped expression, got {items}")

let private testCopiedInterpreterPowerAndXorGrammar () : TestResult =
    let check source expected =
        parseSource source
        |> Result.bind (fun parsed ->
            match parsed.exprsToEval with
            | [WT.EInfix (_, (_, WT.InfixFnCall actual), _, _)] when actual = expected -> Ok ()
            | expressions -> Error $"Unexpected interpreter parse for {source}: {expressions}")
    check "2 ** 3" WT.ArithmeticPower
    |> Result.bind (fun () -> check "2 ^ 3" WT.BitwiseXor)

let tests : (string * (unit -> TestResult)) list = [
    ("Long numeric token streams are stack safe", testLongNumericTokenStreamIsStackSafe)
    ("Tuple lets do not open nested function layout", testTupleLetDoesNotOpenNestedFunctionLayout)
    ("Space application keeps multiple arguments", testSpaceApplicationKeepsMultipleArguments)
    ("Subtraction follows a parenthesized call", testSubtractionFollowsParenthesizedCall)
    ("Top-level expressions follow function declarations", testTopLevelExpressionFollowsFunctionDeclaration)
    ("Module expressions retain their resolution scope", testModuleExpressionRetainsItsResolutionScope)
    ("Copied interpreter distinguishes power and xor", testCopiedInterpreterPowerAndXorGrammar)
]
