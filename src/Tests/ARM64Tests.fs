module ARM64Tests

open NUnit.Framework
open FsUnit
open ARM64

[<Test>]
let ``Encode MOVZ instruction`` () =
    // MOVZ X0, #42
    let instr = MOVZ (X0, 42us, 0)
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should not' (equal 0u)

[<Test>]
let ``Encode MOVK instruction`` () =
    // MOVK X1, #100, LSL #16
    let instr = MOVK (X1, 100us, 16)
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should not' (equal 0u)

[<Test>]
let ``Encode ADD immediate`` () =
    // ADD X0, X1, #5
    let instr = ADD_imm (X0, X1, 5us)
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should not' (equal 0u)

[<Test>]
let ``Encode ADD register`` () =
    // ADD X0, X1, X2
    let instr = ADD_reg (X0, X1, X2)
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should not' (equal 0u)

[<Test>]
let ``Encode SUB immediate`` () =
    // SUB X0, X1, #3
    let instr = SUB_imm (X0, X1, 3us)
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should not' (equal 0u)

[<Test>]
let ``Encode SUB register`` () =
    // SUB X0, X1, X2
    let instr = SUB_reg (X0, X1, X2)
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should not' (equal 0u)

[<Test>]
let ``Encode MUL instruction`` () =
    // MUL X0, X1, X2
    let instr = MUL (X0, X1, X2)
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should not' (equal 0u)

[<Test>]
let ``Encode SDIV instruction`` () =
    // SDIV X0, X1, X2
    let instr = SDIV (X0, X1, X2)
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should not' (equal 0u)

[<Test>]
let ``Encode MOV register`` () =
    // MOV X0, X1
    let instr = MOV_reg (X0, X1)
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should not' (equal 0u)

[<Test>]
let ``Encode RET instruction`` () =
    // RET
    let instr = RET
    let encoded = ARM64.encode instr

    encoded.Length |> should equal 1
    encoded.[0] |> should equal 0xD65F03C0u

[<Test>]
let ``Register encoding correctness`` () =
    // Verify register numbers
    ARM64.encodeReg X0 |> should equal 0u
    ARM64.encodeReg X1 |> should equal 1u
    ARM64.encodeReg X15 |> should equal 15u
    ARM64.encodeReg X30 |> should equal 30u
    ARM64.encodeReg SP |> should equal 31u

[<Test>]
let ``MOVZ encodes immediate correctly`` () =
    // MOVZ X0, #0 should have different encoding than MOVZ X0, #1
    let instr0 = MOVZ (X0, 0us, 0)
    let instr1 = MOVZ (X0, 1us, 0)
    let encoded0 = ARM64.encode instr0
    let encoded1 = ARM64.encode instr1

    encoded0.[0] |> should not' (equal encoded1.[0])

[<Test>]
let ``Different registers produce different encodings`` () =
    // ADD X0, X1, #5 vs ADD X2, X1, #5
    let instr0 = ADD_imm (X0, X1, 5us)
    let instr2 = ADD_imm (X2, X1, 5us)
    let encoded0 = ARM64.encode instr0
    let encoded2 = ARM64.encode instr2

    encoded0.[0] |> should not' (equal encoded2.[0])
