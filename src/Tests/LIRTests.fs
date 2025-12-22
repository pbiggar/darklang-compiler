module LIRTests

open NUnit.Framework
open FsUnit
open MIR
open LIR
open MIR_to_LIR
open RegisterAllocation

[<Test>]
let ``Convert MIR return to LIR`` () =
    // MIR: ret 42
    let mirProgram = MIR.Program [MIR.Block [MIR.Ret (MIR.IntConst 42L)]]
    let lirProgram = MIR_to_LIR.toLIR mirProgram

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
    let lirProgram = MIR_to_LIR.toLIR mirProgram

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
    let lirProgram = MIR_to_LIR.toLIR mirProgram

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
    let lirProgram = MIR_to_LIR.toLIR mirProgram

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
        let lirProgram = MIR_to_LIR.toLIR mirProgram
        match lirProgram with
        | LIR.Program [func] -> func.Body.Length |> should be (greaterThan 0)
        | _ -> Assert.Fail("Expected single function")

    testOp MIR.Add
    testOp MIR.Sub
    testOp MIR.Mul
    testOp MIR.Div

// Register allocation tests
[<Test>]
let ``Allocate registers for simple program`` () =
    // Program with a few virtual registers
    let func = {
        Name = "_start"
        Body = [
            Mov (Virtual 0, Imm 42L)
            Mov (Physical X0, Reg (Virtual 0))
            Ret
        ]
        StackSize = 0
    }
    let allocated = RegisterAllocation.allocateRegisters func

    // Should have physical registers
    allocated.Instrs.Length |> should be (greaterThan 0)
    let hasPhysical = allocated.Instrs |> List.exists (fun instr ->
        match instr with
        | Mov (Physical _, _) -> true
        | _ -> false
    )
    hasPhysical |> should equal true

[<Test>]
let ``All virtual registers replaced after allocation`` () =
    let func = {
        Name = "_start"
        Body = [
            Mov (Virtual 0, Imm 1L)
            Mov (Virtual 1, Imm 2L)
            Add (Virtual 2, Virtual 0, Reg (Virtual 1))
            Mov (Physical X0, Reg (Virtual 2))
            Ret
        ]
        StackSize = 0
    }
    let allocated = RegisterAllocation.allocateRegisters func

    // No virtual registers should remain
    let hasVirtual = allocated.Instrs |> List.exists (fun instr ->
        match instr with
        | Mov (Virtual _, _) -> true
        | Add (Virtual _, _, _) -> true
        | Mov (_, Reg (Virtual _)) -> true
        | _ -> false
    )
    hasVirtual |> should equal false

[<Test>]
let ``Allocated program runs through pipeline`` () =
    // End-to-end test: MIR -> LIR -> Allocate
    let mirProgram = MIR.Program [
        MIR.Block [
            MIR.BinOp (MIR.VReg 0, MIR.Add, MIR.IntConst 2L, MIR.IntConst 3L)
            MIR.Ret (MIR.Register (MIR.VReg 0))
        ]
    ]
    let lirProgram = MIR_to_LIR.toLIR mirProgram

    match lirProgram with
    | LIR.Program [func] ->
        let allocated = RegisterAllocation.allocateRegisters func
        allocated.Instrs.Length |> should be (greaterThan 0)
    | _ -> Assert.Fail("Expected single function")
