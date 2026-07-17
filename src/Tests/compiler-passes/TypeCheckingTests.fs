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
    | Ok (actualType, _) ->
        if actualType = expectedType then
            Ok ()
        else
            Error $"Expected {typeToString expectedType}, got {typeToString actualType}"
    | Error err ->
        Error $"Type checking failed: {typeErrorToString err}"

/// Test that integer literals have type TInt64
let testInt64Literal () : TestResult =
    expectType (Int64Literal 42L) TInt64

/// Test that Int128 literals have type TInt128
let testInt128Literal () : TestResult =
    expectType (Int128Literal (System.Int128.Parse "42")) TInt128

/// Test that UInt128 literals have type TUInt128
let testUInt128Literal () : TestResult =
    expectType (UInt128Literal (System.UInt128.Parse "42")) TUInt128

let rec countMatches (expr: Expr) : int =
    let childMatches =
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
        | Var _
        | FuncRef _ ->
            []
        | InterpolatedString parts ->
            parts
            |> List.choose (function
                | StringText _ -> None
                | StringExpr e -> Some e)
        | BinOp (_, left, right) ->
            [left; right]
        | UnaryOp (_, inner) ->
            [inner]
        | Let (_, value, body) ->
            [value; body]
        | If (cond, thenBranch, elseBranch) ->
            [cond; thenBranch; elseBranch]
        | Call (_, args)
        | TypeApp (_, _, args) ->
            NonEmptyList.toList args
        | TupleLiteral args
        | ListLiteral args ->
            args
        | TupleAccess (tuple, _) ->
            [tuple]
        | RecordLiteral (_, fields) ->
            fields |> List.map snd
        | RecordUpdate (recordExpr, updates) ->
            recordExpr :: (updates |> List.map snd)
        | RecordAccess (recordExpr, _) ->
            [recordExpr]
        | Constructor (_, _, payload) ->
            payload |> Option.toList
        | Match (scrutinee, cases) ->
            scrutinee :: (cases |> List.map (fun c -> c.Body))
        | ListCons (head, tail) ->
            head @ [tail]
        | Lambda (_, body) ->
            [body]
        | Apply (func, args) ->
            func :: NonEmptyList.toList args
        | Closure (_, captures) ->
            captures

    let childCount = childMatches |> List.sumBy countMatches
    match expr with
    | Match _ -> childCount + 1
    | _ -> childCount

/// Sum equality should lower to one pair-match instead of nested match trees.
let testSumEqualityUsesSinglePairMatch () : TestResult =
    let sumDef =
        TypeDef (
            SumTypeDef (
                "ChoiceTc",
                ["a"; "b"],
                [
                    { Name = "ChoiceLeftTc"; Payload = Some (TVar "a") }
                    { Name = "ChoiceRightTc"; Payload = Some (TVar "b") }
                ]
            )
        )

    let eqExpr =
        Let (
            "a",
            Constructor ("ChoiceTc", "ChoiceLeftTc", Some (Int64Literal 1L)),
            Let (
                "b",
                Constructor ("ChoiceTc", "ChoiceLeftTc", Some (Int64Literal 1L)),
                BinOp (Eq, Var "a", Var "b")
            )
        )

    let program = Program [sumDef; Expression eqExpr]

    match checkProgram program with
    | Ok (actualType, Program topLevels) ->
        if actualType <> TBool then
            Error $"Expected Bool result type, got {typeToString actualType}"
        else
            let helperDefs =
                topLevels
                |> List.choose (function
                    | FunctionDef funcDef when funcDef.Name.StartsWith("__dark_eq_") -> Some funcDef
                    | _ -> None)

            let expressionMatchCount =
                topLevels
                |> List.choose (function
                    | Expression typedExpr -> Some (countMatches typedExpr)
                    | _ -> None)
                |> List.tryHead
                |> Option.defaultValue -1

            match helperDefs with
            | [] ->
                Error "Expected generated structural equality helper function for sum equality"
            | helperDef :: _ ->
                let helperMatchCount = countMatches helperDef.Body
                if helperMatchCount <> 1 then
                    Error $"Expected helper body to contain one Match, got {helperMatchCount}"
                elif expressionMatchCount <> 0 then
                    Error $"Expected top-level expression to call helper without Match nodes, got {expressionMatchCount}"
                else
                    Ok ()
    | Error err ->
        Error $"Type checking failed: {typeErrorToString err}"

/// Record access should report invalid generic record arity instead of
/// silently treating the field's type variables as concrete field types.
let testRecordAccessRejectsInvalidRecordArity () : TestResult =
    let recordDef =
        TypeDef (RecordDef ("ArityBoxTc", ["a"], [("value", TVar "a")]))

    let funcDef =
        FunctionDef {
            Name = "main"
            TypeParams = []
            Params = NonEmptyList.singleton ("box", TRecord ("ArityBoxTc", [TInt64; TBool]))
            ReturnType = TInt64
            Body = RecordAccess (Var "box", "value")
        }

    let program = Program [recordDef; funcDef]

    match checkProgram program with
    | Ok _ ->
        Error "Expected invalid record type argument arity to fail type checking"
    | Error (GenericError msg) when msg.Contains("Record type argument arity mismatch") ->
        Ok ()
    | Error err ->
        Error $"Expected record arity mismatch, got: {typeErrorToString err}"

/// Test that addition of integers has type TInt64
let testAddition () : TestResult =
    expectType (BinOp (Add, Int64Literal 2L, Int64Literal 3L)) TInt64

/// Test that subtraction of integers has type TInt64
let testSubtraction () : TestResult =
    expectType (BinOp (Sub, Int64Literal 10L, Int64Literal 5L)) TInt64

/// Test that multiplication of integers has type TInt64
let testMultiplication () : TestResult =
    expectType (BinOp (Mul, Int64Literal 7L, Int64Literal 6L)) TInt64

/// Test that division of integers has type TInt64
let testDivision () : TestResult =
    expectType (BinOp (Div, Int64Literal 20L, Int64Literal 4L)) TInt64

/// Test that negation of integers has type TInt64
let testNegation () : TestResult =
    expectType (UnaryOp (Neg, Int64Literal 42L)) TInt64

/// Test nested operations
let testNestedOperations () : TestResult =
    // 2 + 3 * 4
    let expr = BinOp (Add, Int64Literal 2L, BinOp (Mul, Int64Literal 3L, Int64Literal 4L))
    expectType expr TInt64

/// Test complex nested expression
let testComplexExpression () : TestResult =
    // (10 + 20) * (30 - 15) / 2
    let expr =
        BinOp (Div,
            BinOp (Mul,
                BinOp (Add, Int64Literal 10L, Int64Literal 20L),
                BinOp (Sub, Int64Literal 30L, Int64Literal 15L)),
            Int64Literal 2L)
    expectType expr TInt64

let tests = [
    ("Integer literal", testInt64Literal)
    ("Int128 literal", testInt128Literal)
    ("UInt128 literal", testUInt128Literal)
    ("Sum equality uses single pair match", testSumEqualityUsesSinglePairMatch)
    ("Record access rejects invalid record arity", testRecordAccessRejectsInvalidRecordArity)
    ("Addition", testAddition)
    ("Subtraction", testSubtraction)
    ("Multiplication", testMultiplication)
    ("Division", testDivision)
    ("Negation", testNegation)
    ("Nested operations", testNestedOperations)
    ("Complex expression", testComplexExpression)
]

/// Run all type checking unit tests
/// Returns Ok () if all pass, Error with first failure message if any fail
let runAll () : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
