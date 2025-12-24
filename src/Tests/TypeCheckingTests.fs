// TypeCheckingTests.fs - Unit tests for type checking pass
//
// Tests the type checker for Phase 0 (integers only)
// Will be extended in future phases for booleans, variables, functions, etc.
//
// NOTE: All tests now return Result<> instead of using failwith

module TypeCheckingTests

open AST
open TypeChecking

/// Test result type
type TestResult = Result<unit, string>

/// Helper to check that type checking succeeds with expected type
let expectType (expr: Expr) (expectedType: Type) : TestResult =
    let program = Program [Expression expr]
    match checkProgram program with
    | Ok actualType ->
        if actualType = expectedType then
            Ok ()
        else
            Error $"Expected {typeToString expectedType}, got {typeToString actualType}"
    | Error err ->
        Error $"Type checking failed: {typeErrorToString err}"

/// Test that integer literals have type TInt64
let testIntLiteral () : TestResult =
    expectType (IntLiteral 42L) TInt64

/// Test that addition of integers has type TInt64
let testAddition () : TestResult =
    expectType (BinOp (Add, IntLiteral 2L, IntLiteral 3L)) TInt64

/// Test that subtraction of integers has type TInt64
let testSubtraction () : TestResult =
    expectType (BinOp (Sub, IntLiteral 10L, IntLiteral 5L)) TInt64

/// Test that multiplication of integers has type TInt64
let testMultiplication () : TestResult =
    expectType (BinOp (Mul, IntLiteral 7L, IntLiteral 6L)) TInt64

/// Test that division of integers has type TInt64
let testDivision () : TestResult =
    expectType (BinOp (Div, IntLiteral 20L, IntLiteral 4L)) TInt64

/// Test that negation of integers has type TInt64
let testNegation () : TestResult =
    expectType (UnaryOp (Neg, IntLiteral 42L)) TInt64

/// Test nested operations
let testNestedOperations () : TestResult =
    // 2 + 3 * 4
    let expr = BinOp (Add, IntLiteral 2L, BinOp (Mul, IntLiteral 3L, IntLiteral 4L))
    expectType expr TInt64

/// Test complex nested expression
let testComplexExpression () : TestResult =
    // (10 + 20) * (30 - 15) / 2
    let expr =
        BinOp (Div,
            BinOp (Mul,
                BinOp (Add, IntLiteral 10L, IntLiteral 20L),
                BinOp (Sub, IntLiteral 30L, IntLiteral 15L)),
            IntLiteral 2L)
    expectType expr TInt64

/// Run all type checking unit tests
/// Returns Ok () if all pass, Error with first failure message if any fail
let runAll () : TestResult =
    let tests = [
        ("Integer literal", testIntLiteral)
        ("Addition", testAddition)
        ("Subtraction", testSubtraction)
        ("Multiplication", testMultiplication)
        ("Division", testDivision)
        ("Negation", testNegation)
        ("Nested operations", testNestedOperations)
        ("Complex expression", testComplexExpression)
    ]

    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
