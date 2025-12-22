module LIRTests

open NUnit.Framework
open FsUnit
open MIR
open LIR

[<Test>]
let ``Convert MIR return to LIR`` () =
    // MIR: ret 42
    let mirProgram = MIR.Program [MIR.Block [MIR.Ret (MIR.IntConst 42L)]]
    let lirProgram = LIR.toLIR mirProgram

    match lirProgram with
    | LIR.Program [func] ->
        func.Name |> should equal "_start"
        func.Body.Length |> should be (greaterThan 0)
        // Should have mov to X0 and ret
    | _ -> Assert.Fail("Expected single function")

[<Test>]
let ``Convert MIR binop to LIR`` () =
    // MIR: binop v0, add, 2, 3; ret v0
    let mirProgram = MIR.Program [
        MIR.Block [
            MIR.BinOp (MIR.VReg 0, MIR.Add, MIR.IntConst 2L, MIR.IntConst 3L)
            MIR.Ret (MIR.Register (MIR.VReg 0))
        ]
    ]
    let lirProgram = LIR.toLIR mirProgram

    match lirProgram with
    | LIR.Program [func] ->
        func.Body.Length |> should be (greaterThan 0)
    | _ -> Assert.Fail("Expected single function")

[<Test>]
let ``Convert MIR mov to LIR`` () =
    // MIR: mov v0, 42; ret v0
    let mirProgram = MIR.Program [
        MIR.Block [
            MIR.Mov (MIR.VReg 0, MIR.IntConst 42L)
            MIR.Ret (MIR.Register (MIR.VReg 0))
        ]
    ]
    let lirProgram = LIR.toLIR mirProgram

    match lirProgram with
    | LIR.Program [func] ->
        func.Body.Length |> should be (greaterThan 0)
    | _ -> Assert.Fail("Expected single function")

[<Test>]
let ``Virtual registers before allocation`` () =
    // MIR with virtual registers should produce LIR with virtual registers
    let mirProgram = MIR.Program [
        MIR.Block [
            MIR.BinOp (MIR.VReg 0, MIR.Add, MIR.IntConst 2L, MIR.IntConst 3L)
            MIR.Ret (MIR.Register (MIR.VReg 0))
        ]
    ]
    let lirProgram = LIR.toLIR mirProgram

    match lirProgram with
    | LIR.Program [func] ->
        // Before allocation, should have Virtual registers
        let hasVirtual = func.Body |> List.exists (fun instr ->
            match instr with
            | Mov (Virtual _, _) -> true
            | Add (Virtual _, _, _) -> true
            | _ -> false
        )
        hasVirtual |> should equal true
    | _ -> Assert.Fail("Expected single function")

[<Test>]
let ``Convert all MIR operators to LIR`` () =
    // Test Add, Sub, Mul, Div
    let testOp op =
        let mirProgram = MIR.Program [
            MIR.Block [
                MIR.BinOp (MIR.VReg 0, op, MIR.IntConst 10L, MIR.IntConst 2L)
                MIR.Ret (MIR.Register (MIR.VReg 0))
            ]
        ]
        let lirProgram = LIR.toLIR mirProgram
        match lirProgram with
        | LIR.Program [func] -> func.Body.Length |> should be (greaterThan 0)
        | _ -> Assert.Fail("Expected single function")

    testOp MIR.Add
    testOp MIR.Sub
    testOp MIR.Mul
    testOp MIR.Div
