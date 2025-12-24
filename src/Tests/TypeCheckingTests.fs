// TypeCheckingTests.fs - Unit tests for type checking pass
//
// Tests the type checker for Phase 0 (integers only)
// Will be extended in future phases for booleans, variables, functions, etc.

module TypeCheckingTests

open AST
open TypeChecking

/// Test that integer literals have type TInt64
let testIntLiteral () =
    let expr = IntLiteral 42L
    let program = Program expr
    match checkProgram program with
    | Ok TInt64 -> ()
    | Ok other -> failwith $"Expected TInt64, got {typeToString other}"
    | Error err -> failwith $"Type checking failed: {typeErrorToString err}"

/// Test that addition of integers has type TInt64
let testAddition () =
    let expr = BinOp (Add, IntLiteral 2L, IntLiteral 3L)
    let program = Program expr
    match checkProgram program with
    | Ok TInt64 -> ()
    | Ok other -> failwith $"Expected TInt64, got {typeToString other}"
    | Error err -> failwith $"Type checking failed: {typeErrorToString err}"

/// Test that subtraction of integers has type TInt64
let testSubtraction () =
    let expr = BinOp (Sub, IntLiteral 10L, IntLiteral 5L)
    let program = Program expr
    match checkProgram program with
    | Ok TInt64 -> ()
    | Ok other -> failwith $"Expected TInt64, got {typeToString other}"
    | Error err -> failwith $"Type checking failed: {typeErrorToString err}"

/// Test that multiplication of integers has type TInt64
let testMultiplication () =
    let expr = BinOp (Mul, IntLiteral 7L, IntLiteral 6L)
    let program = Program expr
    match checkProgram program with
    | Ok TInt64 -> ()
    | Ok other -> failwith $"Expected TInt64, got {typeToString other}"
    | Error err -> failwith $"Type checking failed: {typeErrorToString err}"

/// Test that division of integers has type TInt64
let testDivision () =
    let expr = BinOp (Div, IntLiteral 20L, IntLiteral 4L)
    let program = Program expr
    match checkProgram program with
    | Ok TInt64 -> ()
    | Ok other -> failwith $"Expected TInt64, got {typeToString other}"
    | Error err -> failwith $"Type checking failed: {typeErrorToString err}"

/// Test that negation of integers has type TInt64
let testNegation () =
    let expr = UnaryOp (Neg, IntLiteral 42L)
    let program = Program expr
    match checkProgram program with
    | Ok TInt64 -> ()
    | Ok other -> failwith $"Expected TInt64, got {typeToString other}"
    | Error err -> failwith $"Type checking failed: {typeErrorToString err}"

/// Test nested operations
let testNestedOperations () =
    // 2 + 3 * 4
    let expr = BinOp (Add, IntLiteral 2L, BinOp (Mul, IntLiteral 3L, IntLiteral 4L))
    let program = Program expr
    match checkProgram program with
    | Ok TInt64 -> ()
    | Ok other -> failwith $"Expected TInt64, got {typeToString other}"
    | Error err -> failwith $"Type checking failed: {typeErrorToString err}"

/// Test complex nested expression
let testComplexExpression () =
    // (10 + 20) * (30 - 15) / 2
    let expr =
        BinOp (Div,
            BinOp (Mul,
                BinOp (Add, IntLiteral 10L, IntLiteral 20L),
                BinOp (Sub, IntLiteral 30L, IntLiteral 15L)),
            IntLiteral 2L)
    let program = Program expr
    match checkProgram program with
    | Ok TInt64 -> ()
    | Ok other -> failwith $"Expected TInt64, got {typeToString other}"
    | Error err -> failwith $"Type checking failed: {typeErrorToString err}"

/// Run all type checking unit tests
let runAll () =
    testIntLiteral()
    testAddition()
    testSubtraction()
    testMultiplication()
    testDivision()
    testNegation()
    testNestedOperations()
    testComplexExpression()
