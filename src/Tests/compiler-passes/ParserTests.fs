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
    match Parser.lex source with
    | Ok tokens when List.length tokens = literalCount + 1 -> Ok ()
    | Ok tokens ->
        Error $"Expected {literalCount + 1} tokens including TEOF, got {List.length tokens}"
    | Error err -> Error err

let private testTupleLetDoesNotOpenNestedFunctionLayout () : TestResult =
    let source =
        """let pairsToStrings (pairs: List<(String * String)>) : List<String> =
    Stdlib.List.map<(String * String), String> pairs (fun pair -> let (key, value) = pair in key ++ value)
let identity (value: String) : String = value"""
    match Parser.parseString false source with
    | Ok _ -> Ok ()
    | Error err -> Error err

let private testSpaceApplicationKeepsMultipleArguments () : TestResult =
    let source = "let recurse (a: Int8) (b: Int8) : Int8 = recurse a b"
    match Parser.parseString false source with
    | Ok (Program [FunctionDef definition]) ->
        match definition.Body with
        | Apply (Var "recurse", [], args) when NonEmptyList.toList args = [Var "a"; Var "b"] -> Ok ()
        | body -> Error $"Expected a two-argument call, got {body}"
    | Ok program -> Error $"Expected one function declaration, got {program}"
    | Error err -> Error err

let private testSubtractionFollowsParenthesizedCall () : TestResult =
    let source =
        "let dropLast (value: String) : Int64 = (Stdlib.String.__byteLength value) - 1L"
    match Parser.parseString true source with
    | Ok (Program [FunctionDef definition]) ->
        match definition.Body with
        | BinOp (Sub, Apply (Var "Stdlib.String.__byteLength", [], args), Int64Literal 1L)
            when NonEmptyList.toList args = [Var "value"] -> Ok ()
        | body -> Error $"Expected subtraction from a one-argument call, got {body}"
    | Ok program -> Error $"Expected one function declaration, got {program}"
    | Error err -> Error err

let private testNegativeLiteralRemainsAFunctionArgument () : TestResult =
    let source =
        "let byteLength (value: String) : Int64 = Stdlib.String.__byteLength value -1L"
    match Parser.parseString true source with
    | Ok (Program [FunctionDef definition]) ->
        match definition.Body with
        | Apply (Var "Stdlib.String.__byteLength", [], args)
            when NonEmptyList.toList args = [Var "value"; Int64Literal -1L] -> Ok ()
        | body -> Error $"Expected a negative second argument, got {body}"
    | Ok program -> Error $"Expected one function declaration, got {program}"
    | Error err -> Error err

let private testSpaceApplicationStaysCurried () : TestResult =
    let source =
        "let apply (fn: Int64 -> Int64 -> Int64) : Int64 = fn 1L 2L"
    match Parser.parseString false source with
    | Ok (Program [FunctionDef definition]) ->
        match definition.Body with
        | Apply (Var "fn", [], args)
            when NonEmptyList.toList args = [Int64Literal 1L; Int64Literal 2L] -> Ok ()
        | body -> Error $"Expected two space-applied arguments, got {body}"
    | Ok program -> Error $"Expected one function declaration, got {program}"
    | Error err -> Error err

let private testTopLevelExpressionFollowsFunctionDeclaration () : TestResult =
    let source =
        "let identity (value: Int64) : Int64 = value\nidentity 1L"
    match Parser.parseString false source with
    | Ok (Program [FunctionDef definition; Expression (_, expression)]) ->
        match definition.Body, expression with
        | Var "value", Apply (Var "identity", [], args)
            when NonEmptyList.toList args = [Int64Literal 1L] -> Ok ()
        | body, result ->
            Error $"Expected a separate function body and top-level call, got {body} and {result}"
    | Ok program -> Error $"Expected a function declaration followed by an expression, got {program}"
    | Error err -> Error err

let private testModuleExpressionRetainsItsResolutionScope () : TestResult =
    let source = "module Darklang.Example.Nested\n\n1L"
    match Parser.parseString false source with
    | Ok (Program [Expression (["Darklang"; "Example"; "Nested"], Int64Literal 1L)]) -> Ok ()
    | Ok program -> Error $"Expected a module-scoped expression, got {program}"
    | Error err -> Error err

let private testFlattenParamGroupsRestoresSourceOrder () : TestResult =
    let groupsRev =
        [[("third", AST.PTString)]; [("second", AST.PTBool)]; [("first", AST.PTInt64)]]

    match Parser.flattenParamGroups groupsRev with
    | [("first", AST.PTInt64); ("second", AST.PTBool); ("third", AST.PTString)] -> Ok ()
    | parameters -> Error $"Expected parameter groups in source order, got {parameters}"

let tests : (string * (unit -> TestResult)) list = [
    ("Long numeric token streams are stack safe", testLongNumericTokenStreamIsStackSafe)
    ("Tuple lets do not open nested function layout", testTupleLetDoesNotOpenNestedFunctionLayout)
    ("Space application keeps multiple arguments", testSpaceApplicationKeepsMultipleArguments)
    ("Subtraction follows a parenthesized call", testSubtractionFollowsParenthesizedCall)
    ("Negative literals remain function arguments", testNegativeLiteralRemainsAFunctionArgument)
    ("Space application stays curried", testSpaceApplicationStaysCurried)
    ("Top-level expressions follow function declarations", testTopLevelExpressionFollowsFunctionDeclaration)
    ("Module expressions retain their resolution scope", testModuleExpressionRetainsItsResolutionScope)

    ("Flattened parameter groups retain source order", testFlattenParamGroupsRestoresSourceOrder)
]
