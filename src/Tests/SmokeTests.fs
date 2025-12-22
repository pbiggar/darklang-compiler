module SmokeTests

open NUnit.Framework
open FsUnit

[<Test>]
let ``Test framework smoke test`` () =
    // Verify FsUnit is working
    1 + 1 |> should equal 2

[<Test>]
let ``Verify basic F# types work`` () =
    let result = 42
    result |> should equal 42

[<Test>]
let ``Verify list operations work`` () =
    let numbers = [1; 2; 3]
    List.length numbers |> should equal 3
    List.sum numbers |> should equal 6

[<Test>]
let ``End-to-end compilation of simple expression`` () =
    // Compile "2 + 3" through the entire pipeline
    let source = "2 + 3"

    // Parse
    let ast = Parser.parseString source
    let (AST.Program expr) = ast

    // Convert to ANF
    let (anfExpr, _) = ANF.toANF expr (ANF.VarGen 0)
    let anfProgram = ANF.Program anfExpr

    // Convert to MIR
    let (mirProgram, _) = MIR.toMIR anfProgram (MIR.RegGen 0)

    // Convert to LIR
    let lirProgram = LIR.toLIR mirProgram

    // Allocate registers
    let (LIR.Program funcs) = lirProgram
    let func = List.head funcs
    let allocResult = LIR.allocateRegisters func
    let allocatedFunc = { func with Body = allocResult.Instrs; StackSize = allocResult.StackSize }
    let allocatedProgram = LIR.Program [allocatedFunc]

    // Generate ARM64 code
    let arm64Code = CodeGen.generateARM64 allocatedProgram

    // Verify we got some ARM64 instructions
    arm64Code.Length |> should be (greaterThan 0)

    // Verify it ends with RET
    arm64Code |> List.last |> should equal ARM64.RET

    // Encode to machine code
    let machineCode = arm64Code |> List.collect ARM64.encode

    // Generate binary
    let binary = Binary.createExecutable machineCode

    // Verify binary structure
    binary.Length |> should be (greaterThan 0)
    binary.[0] |> should equal 0xCFuy  // Mach-O magic

[<Test>]
let ``End-to-end compilation of arithmetic expression`` () =
    // Compile "2 + 3 * 4" through the entire pipeline
    let source = "2 + 3 * 4"

    // Parse
    let ast = Parser.parseString source

    // Convert to ANF
    let (AST.Program expr) = ast
    let (anfExpr, _) = ANF.toANF expr (ANF.VarGen 0)
    let anfProgram = ANF.Program anfExpr

    // Convert to MIR
    let (mirProgram, _) = MIR.toMIR anfProgram (MIR.RegGen 0)

    // Convert to LIR
    let lirProgram = LIR.toLIR mirProgram

    // Allocate registers
    let (LIR.Program funcs) = lirProgram
    let func = List.head funcs
    let allocResult = LIR.allocateRegisters func
    let allocatedFunc = { func with Body = allocResult.Instrs; StackSize = allocResult.StackSize }
    let allocatedProgram = LIR.Program [allocatedFunc]

    // Generate ARM64 code
    let arm64Code = CodeGen.generateARM64 allocatedProgram

    // Encode to machine code
    let machineCode = arm64Code |> List.collect ARM64.encode

    // Generate binary
    let binary = Binary.createExecutable machineCode

    // Verify binary was created
    binary.Length |> should be (greaterThan 0)
