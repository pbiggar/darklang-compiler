// ParserTests.fs - Focused invariants for canonical parser token construction.
//
// These tests cover lexer properties that are not observable through a small
// end-to-end program, including stack safety across long token streams.

module ParserTests

open AST

type TestResult = Result<unit, string>

let private testLongNumericTokenStreamIsStackSafe () : TestResult =
    let literalCount = 2000
    let source = List.replicate literalCount "0" |> String.concat " "
    match InterpreterParser.lex source with
    | Ok tokens when List.length tokens = literalCount + 1 -> Ok ()
    | Ok tokens ->
        Error $"Expected {literalCount + 1} tokens including TEOF, got {List.length tokens}"
    | Error err -> Error err

let private testTupleLetDoesNotOpenNestedFunctionLayout () : TestResult =
    let source =
        """let pairsToStrings(pairs: List<(String, String)>) : List<String> =
    Stdlib.List.map<(String, String), String>(pairs, fun pair -> let (key, value) = pair in key ++ value)
let identity(value: String) : String = value"""
    match InterpreterParser.parseString false source with
    | Ok _ -> Ok ()
    | Error err -> Error err

let private testParenthesizedCallKeepsMultipleArguments () : TestResult =
    let source = "let recurse(a: Int8, b: Int8) : Int8 = recurse(a, b)"
    match InterpreterParser.parseString false source with
    | Ok (Program [FunctionDef definition]) ->
        match definition.Body with
        | Call ("recurse", args) when NonEmptyList.toList args = [Var "a"; Var "b"] -> Ok ()
        | body -> Error $"Expected a two-argument call, got {body}"
    | Ok program -> Error $"Expected one function declaration, got {program}"
    | Error err -> Error err

let private testSubtractionFollowsParenthesizedCall () : TestResult =
    let source =
        "let dropLast(value: String) : Int64 = Stdlib.String.__byteLength(value) - 1L"
    match InterpreterParser.parseString true source with
    | Ok (Program [FunctionDef definition]) ->
        match definition.Body with
        | BinOp (Sub, Call ("Stdlib.String.__byteLength", args), Int64Literal 1L)
            when NonEmptyList.toList args = [Var "value"] -> Ok ()
        | body -> Error $"Expected subtraction from a one-argument call, got {body}"
    | Ok program -> Error $"Expected one function declaration, got {program}"
    | Error err -> Error err

let private testAdjacentCallGroupsStayCurried () : TestResult =
    let source =
        "let apply(fn: (Int64) -> (Int64) -> Int64) : Int64 = fn(1L)(2L)"
    match InterpreterParser.parseString false source with
    | Ok (Program [FunctionDef definition]) ->
        match definition.Body with
        | Apply (Call ("fn", firstArgs), secondArgs)
            when NonEmptyList.toList firstArgs = [Int64Literal 1L]
                 && NonEmptyList.toList secondArgs = [Int64Literal 2L] -> Ok ()
        | body -> Error $"Expected two curried call groups, got {body}"
    | Ok program -> Error $"Expected one function declaration, got {program}"
    | Error err -> Error err

let private testTopLevelExpressionFollowsFunctionDeclaration () : TestResult =
    let source =
        "let identity(value: Int64) : Int64 = value\nidentity(1L)"
    match InterpreterParser.parseString false source with
    | Ok (Program [FunctionDef definition; Expression expression]) ->
        match definition.Body, expression with
        | Var "value", Call ("identity", args)
            when NonEmptyList.toList args = [Int64Literal 1L] -> Ok ()
        | body, result ->
            Error $"Expected a separate function body and top-level call, got {body} and {result}"
    | Ok program -> Error $"Expected a function declaration followed by an expression, got {program}"
    | Error err -> Error err

let tests : (string * (unit -> TestResult)) list = [
    ("Long numeric token streams are stack safe", testLongNumericTokenStreamIsStackSafe)
    ("Tuple lets do not open nested function layout", testTupleLetDoesNotOpenNestedFunctionLayout)
    ("Parenthesized calls keep multiple arguments", testParenthesizedCallKeepsMultipleArguments)
    ("Subtraction follows a parenthesized call", testSubtractionFollowsParenthesizedCall)
    ("Adjacent call groups stay curried", testAdjacentCallGroupsStayCurried)
    ("Top-level expressions follow function declarations", testTopLevelExpressionFollowsFunctionDeclaration)
]
