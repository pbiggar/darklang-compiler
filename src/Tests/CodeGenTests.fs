module CodeGenTests

open NUnit.Framework
open FsUnit
open LIR
open CodeGen

[<Test>]
let ``Generate ARM64 for simple MOV`` () =
    // LIR: mov x0, #42
    let lirFunc = {
        Name = "_start"
        Body = [
            Mov (Physical X0, Imm 42L)
        ]
        StackSize = 0
    }
    let lirProgram = LIR.Program [lirFunc]
    let arm64Instrs = CodeGen.generateARM64 lirProgram

    arm64Instrs.Length |> should be (greaterThan 0)

[<Test>]
let ``Generate ARM64 for ADD`` () =
    // LIR: add x0, x1, #5
    let lirFunc = {
        Name = "_start"
        Body = [
            Add (Physical X0, Physical X1, Imm 5L)
        ]
        StackSize = 0
    }
    let lirProgram = LIR.Program [lirFunc]
    let arm64Instrs = CodeGen.generateARM64 lirProgram

    arm64Instrs.Length |> should be (greaterThan 0)

[<Test>]
let ``Generate ARM64 for MUL`` () =
    // LIR: mul x0, x1, x2
    let lirFunc = {
        Name = "_start"
        Body = [
            Mul (Physical X0, Physical X1, Physical X2)
        ]
        StackSize = 0
    }
    let lirProgram = LIR.Program [lirFunc]
    let arm64Instrs = CodeGen.generateARM64 lirProgram

    arm64Instrs.Length |> should be (greaterThan 0)

[<Test>]
let ``Generate ARM64 for RET`` () =
    // LIR: ret
    let lirFunc = {
        Name = "_start"
        Body = [Ret]
        StackSize = 0
    }
    let lirProgram = LIR.Program [lirFunc]
    let arm64Instrs = CodeGen.generateARM64 lirProgram

    arm64Instrs |> should contain ARM64.RET

[<Test>]
let ``Generate ARM64 for full program`` () =
    // LIR program with multiple instructions
    let lirFunc = {
        Name = "_start"
        Body = [
            Mov (Physical X1, Imm 2L)
            Mov (Physical X2, Imm 3L)
            Add (Physical X0, Physical X1, Reg (Physical X2))
            Ret
        ]
        StackSize = 0
    }
    let lirProgram = LIR.Program [lirFunc]
    let arm64Instrs = CodeGen.generateARM64 lirProgram

    // Should have at least 4 instructions (possibly more for loading immediates)
    arm64Instrs.Length |> should be (greaterThanOrEqualTo 4)

[<Test>]
let ``Generate ARM64 handles all operators`` () =
    let testOp lirInstr =
        let lirFunc = { Name = "_start"; Body = [lirInstr; Ret]; StackSize = 0 }
        let lirProgram = LIR.Program [lirFunc]
        let arm64Instrs = CodeGen.generateARM64 lirProgram
        arm64Instrs.Length |> should be (greaterThan 0)

    testOp (Add (Physical X0, Physical X1, Imm 1L))
    testOp (Sub (Physical X0, Physical X1, Imm 1L))
    testOp (Mul (Physical X0, Physical X1, Physical X2))
    testOp (Sdiv (Physical X0, Physical X1, Physical X2))
