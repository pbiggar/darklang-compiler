module MIRTests

open NUnit.Framework
open FsUnit
open ANF
open MIR
open ANF_to_MIR

[<Test>]
let ``Convert ANF integer literal to MIR`` () =
    // ANF: return 42
    let anfExpr = ANF.Return (ANF.IntLiteral 42L)
    let anfProgram = ANF.Program anfExpr
    let (mirProgram, _) = ANF_to_MIR.toMIR anfProgram MIR.initialRegGen

    match mirProgram with
    | MIR.Program [Block instrs] ->
        match instrs with
        | [Ret (IntConst 42L)] -> ()
        | _ -> Assert.Fail($"Unexpected instructions: {instrs}")
    | _ -> Assert.Fail($"Unexpected program structure")

[<Test>]
let ``Convert ANF simple addition to MIR`` () =
    // ANF: let t0 = 2 + 3; return t0
    let anfExpr = ANF.Let (TempId 0, ANF.Prim (ANF.Add, ANF.IntLiteral 2L, ANF.IntLiteral 3L),
                           ANF.Return (ANF.Var (TempId 0)))
    let anfProgram = ANF.Program anfExpr
    let (mirProgram, _) = ANF_to_MIR.toMIR anfProgram MIR.initialRegGen

    match mirProgram with
    | MIR.Program [Block instrs] ->
        instrs.Length |> should be (greaterThan 0)
        // Should have instructions to load constants and perform binop
    | _ -> Assert.Fail($"Unexpected program structure")

[<Test>]
let ``Convert ANF nested expression to MIR`` () =
    // ANF for 2 + (3 * 4):
    // let t0 = 3 * 4
    // let t1 = 2 + t0
    // return t1
    let anfExpr =
        ANF.Let (TempId 0, ANF.Prim (ANF.Mul, ANF.IntLiteral 3L, ANF.IntLiteral 4L),
                 ANF.Let (TempId 1, ANF.Prim (ANF.Add, ANF.IntLiteral 2L, ANF.Var (TempId 0)),
                          ANF.Return (ANF.Var (TempId 1))))
    let anfProgram = ANF.Program anfExpr
    let (mirProgram, _) = ANF_to_MIR.toMIR anfProgram MIR.initialRegGen

    match mirProgram with
    | MIR.Program [Block instrs] ->
        instrs.Length |> should be (greaterThan 0)
        // Should have mul and add operations
    | _ -> Assert.Fail($"Unexpected program structure")

[<Test>]
let ``RegGen generates fresh registers`` () =
    let gen0 = MIR.initialRegGen
    let (VReg id1, gen1) = MIR.freshReg gen0
    let (VReg id2, gen2) = MIR.freshReg gen1
    let (VReg id3, _) = MIR.freshReg gen2

    id1 |> should equal 0
    id2 |> should equal 1
    id3 |> should equal 2

[<Test>]
let ``Convert ANF with all operators to MIR`` () =
    // Test Add
    let anfAdd = ANF.Let (TempId 0, ANF.Prim (ANF.Add, ANF.IntLiteral 1L, ANF.IntLiteral 2L),
                          ANF.Return (ANF.Var (TempId 0)))
    let (Program [Block addInstrs], _) = ANF_to_MIR.toMIR (ANF.Program anfAdd) MIR.initialRegGen
    addInstrs.Length |> should be (greaterThan 0)

    // Test Sub
    let anfSub = ANF.Let (TempId 0, ANF.Prim (ANF.Sub, ANF.IntLiteral 5L, ANF.IntLiteral 3L),
                          ANF.Return (ANF.Var (TempId 0)))
    let (Program [Block subInstrs], _) = ANF_to_MIR.toMIR (ANF.Program anfSub) MIR.initialRegGen
    subInstrs.Length |> should be (greaterThan 0)

    // Test Div
    let anfDiv = ANF.Let (TempId 0, ANF.Prim (ANF.Div, ANF.IntLiteral 10L, ANF.IntLiteral 2L),
                          ANF.Return (ANF.Var (TempId 0)))
    let (Program [Block divInstrs], _) = ANF_to_MIR.toMIR (ANF.Program anfDiv) MIR.initialRegGen
    divInstrs.Length |> should be (greaterThan 0)
