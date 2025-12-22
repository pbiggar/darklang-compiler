module ParserTests

open NUnit.Framework
open FsUnit
open AST
open Parser

// Lexer tests
[<Test>]
let ``Lexer can tokenize integer`` () =
    let tokens = lex "42"
    tokens |> should equal [TInt 42L; TEOF]

[<Test>]
let ``Lexer can tokenize addition`` () =
    let tokens = lex "2 + 3"
    tokens |> should equal [TInt 2L; TPlus; TInt 3L; TEOF]

[<Test>]
let ``Lexer can tokenize all operators`` () =
    let tokens = lex "1 + 2 - 3 * 4 / 5"
    tokens |> should equal [
        TInt 1L; TPlus; TInt 2L; TMinus; TInt 3L;
        TStar; TInt 4L; TSlash; TInt 5L; TEOF
    ]

[<Test>]
let ``Lexer can tokenize parentheses`` () =
    let tokens = lex "(2 + 3)"
    tokens |> should equal [TLParen; TInt 2L; TPlus; TInt 3L; TRParen; TEOF]

[<Test>]
let ``Lexer handles whitespace`` () =
    let tokens = lex "  42  "
    tokens |> should equal [TInt 42L; TEOF]

// Parser tests
[<Test>]
let ``Parser can parse integer literal`` () =
    let program = parseString "42"
    program |> should equal (Program (IntLiteral 42L))

[<Test>]
let ``Parser can parse addition`` () =
    let program = parseString "2 + 3"
    program |> should equal (Program (BinOp (Add, IntLiteral 2L, IntLiteral 3L)))

[<Test>]
let ``Parser can parse subtraction`` () =
    let program = parseString "5 - 3"
    program |> should equal (Program (BinOp (Sub, IntLiteral 5L, IntLiteral 3L)))

[<Test>]
let ``Parser can parse multiplication`` () =
    let program = parseString "3 * 4"
    program |> should equal (Program (BinOp (Mul, IntLiteral 3L, IntLiteral 4L)))

[<Test>]
let ``Parser can parse division`` () =
    let program = parseString "10 / 2"
    program |> should equal (Program (BinOp (Div, IntLiteral 10L, IntLiteral 2L)))

[<Test>]
let ``Parser respects operator precedence`` () =
    // 2 + 3 * 4 should parse as 2 + (3 * 4)
    let program = parseString "2 + 3 * 4"
    let expected = Program (BinOp (Add, IntLiteral 2L, BinOp (Mul, IntLiteral 3L, IntLiteral 4L)))
    program |> should equal expected

[<Test>]
let ``Parser handles parentheses`` () =
    // (2 + 3) * 4 should parse as (2 + 3) * 4
    let program = parseString "(2 + 3) * 4"
    let expected = Program (BinOp (Mul, BinOp (Add, IntLiteral 2L, IntLiteral 3L), IntLiteral 4L))
    program |> should equal expected

[<Test>]
let ``Parser handles left associativity for addition`` () =
    // 1 + 2 + 3 should parse as (1 + 2) + 3
    let program = parseString "1 + 2 + 3"
    let expected = Program (BinOp (Add, BinOp (Add, IntLiteral 1L, IntLiteral 2L), IntLiteral 3L))
    program |> should equal expected

[<Test>]
let ``Parser handles complex nested expression`` () =
    // (10 - 2) * (3 + 1)
    let program = parseString "(10 - 2) * (3 + 1)"
    let left = BinOp (Sub, IntLiteral 10L, IntLiteral 2L)
    let right = BinOp (Add, IntLiteral 3L, IntLiteral 1L)
    let expected = Program (BinOp (Mul, left, right))
    program |> should equal expected
