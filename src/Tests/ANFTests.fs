module ANFTests

open NUnit.Framework
open FsUnit
open AST
open ANF

/// Helper to convert AST.BinOp to ANF.BinOp
let convertOp (op: AST.BinOp) : ANF.BinOp =
    match op with
    | AST.Add -> ANF.Add
    | AST.Sub -> ANF.Sub
    | AST.Mul -> ANF.Mul
    | AST.Div -> ANF.Div

[<Test>]
let ``Convert integer literal to ANF`` () =
    let ast = AST.IntLiteral 42L
    let (anf, _) = ANF.toANF ast ANF.initialVarGen

    match anf with
    | Return (IntLiteral 42L) -> ()
    | _ -> Assert.Fail($"Expected Return (IntLiteral 42), got {anf}")

[<Test>]
let ``Convert simple addition to ANF`` () =
    // 2 + 3
    let ast = AST.BinOp (AST.Add, AST.IntLiteral 2L, AST.IntLiteral 3L)
    let (anf, _) = ANF.toANF ast ANF.initialVarGen

    match anf with
    | Let (TempId 0, Prim (Add, IntLiteral 2L, IntLiteral 3L), Return (Var (TempId 0))) -> ()
    | _ -> Assert.Fail($"Unexpected ANF structure: {anf}")

[<Test>]
let ``Convert nested expression to ANF`` () =
    // 2 + (3 * 4)
    // Should become:
    // let t0 = 3 * 4
    // let t1 = 2 + t0
    // return t1
    let inner = AST.BinOp (AST.Mul, AST.IntLiteral 3L, AST.IntLiteral 4L)
    let ast = AST.BinOp (AST.Add, AST.IntLiteral 2L, inner)
    let (anf, _) = ANF.toANF ast ANF.initialVarGen

    match anf with
    | Let (TempId 0, Prim (Mul, IntLiteral 3L, IntLiteral 4L),
           Let (TempId 1, Prim (Add, IntLiteral 2L, Var (TempId 0)),
                Return (Var (TempId 1)))) -> ()
    | _ -> Assert.Fail($"Unexpected ANF structure: {anf}")

[<Test>]
let ``Convert deeply nested expression to ANF`` () =
    // (1 + 2) + (3 * 4)
    // Should become:
    // let t0 = 1 + 2
    // let t1 = 3 * 4
    // let t2 = t0 + t1
    // return t2
    let left = AST.BinOp (AST.Add, AST.IntLiteral 1L, AST.IntLiteral 2L)
    let right = AST.BinOp (AST.Mul, AST.IntLiteral 3L, AST.IntLiteral 4L)
    let ast = AST.BinOp (AST.Add, left, right)
    let (anf, _) = ANF.toANF ast ANF.initialVarGen

    match anf with
    | Let (TempId 0, Prim (Add, IntLiteral 1L, IntLiteral 2L),
           Let (TempId 1, Prim (Mul, IntLiteral 3L, IntLiteral 4L),
                Let (TempId 2, Prim (Add, Var (TempId 0), Var (TempId 1)),
                     Return (Var (TempId 2))))) -> ()
    | _ -> Assert.Fail($"Unexpected ANF structure: {anf}")

[<Test>]
let ``VarGen generates fresh variables`` () =
    let gen0 = ANF.initialVarGen
    let (TempId id1, gen1) = ANF.freshVar gen0
    let (TempId id2, gen2) = ANF.freshVar gen1
    let (TempId id3, _) = ANF.freshVar gen2

    id1 |> should equal 0
    id2 |> should equal 1
    id3 |> should equal 2

[<Test>]
let ``Convert subtraction to ANF`` () =
    let ast = AST.BinOp (AST.Sub, AST.IntLiteral 5L, AST.IntLiteral 3L)
    let (anf, _) = ANF.toANF ast ANF.initialVarGen

    match anf with
    | Let (TempId 0, Prim (Sub, IntLiteral 5L, IntLiteral 3L), Return (Var (TempId 0))) -> ()
    | _ -> Assert.Fail($"Unexpected ANF structure: {anf}")

[<Test>]
let ``Convert division to ANF`` () =
    let ast = AST.BinOp (AST.Div, AST.IntLiteral 10L, AST.IntLiteral 2L)
    let (anf, _) = ANF.toANF ast ANF.initialVarGen

    match anf with
    | Let (TempId 0, Prim (Div, IntLiteral 10L, IntLiteral 2L), Return (Var (TempId 0))) -> ()
    | _ -> Assert.Fail($"Unexpected ANF structure: {anf}")
