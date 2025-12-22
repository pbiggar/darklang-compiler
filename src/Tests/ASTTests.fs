module ASTTests

open NUnit.Framework
open FsUnit
open AST

[<Test>]
let ``Can create integer literal`` () =
    let expr = IntLiteral 42L
    expr |> should equal (IntLiteral 42L)

[<Test>]
let ``Can create addition expression`` () =
    let left = IntLiteral 2L
    let right = IntLiteral 3L
    let expr = BinOp (Add, left, right)

    match expr with
    | BinOp (Add, IntLiteral 2L, IntLiteral 3L) -> ()
    | _ -> Assert.Fail("Unexpected expression structure")

[<Test>]
let ``Can create nested expression`` () =
    // 2 + (3 * 4)
    let inner = BinOp (Mul, IntLiteral 3L, IntLiteral 4L)
    let outer = BinOp (Add, IntLiteral 2L, inner)

    match outer with
    | BinOp (Add, IntLiteral 2L, BinOp (Mul, IntLiteral 3L, IntLiteral 4L)) -> ()
    | _ -> Assert.Fail("Unexpected expression structure")

[<Test>]
let ``Can create all operator types`` () =
    let add = BinOp (Add, IntLiteral 1L, IntLiteral 2L)
    let sub = BinOp (Sub, IntLiteral 5L, IntLiteral 3L)
    let mul = BinOp (Mul, IntLiteral 3L, IntLiteral 4L)
    let div = BinOp (Div, IntLiteral 10L, IntLiteral 2L)

    // Just verify they all construct without errors
    add |> should not' (equal null)
    sub |> should not' (equal null)
    mul |> should not' (equal null)
    div |> should not' (equal null)

[<Test>]
let ``Can create program`` () =
    let expr = IntLiteral 42L
    let program = Program expr

    match program with
    | Program (IntLiteral 42L) -> ()
    | _ -> Assert.Fail("Unexpected program structure")
