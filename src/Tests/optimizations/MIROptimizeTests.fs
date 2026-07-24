// MIROptimizeTests.fs - Unit tests for MIR optimizer fixpoint behavior
//
// Verifies that MIR optimization re-runs to a fixed point when copy propagation
// creates new CSE opportunities.

module MIROptimizeTests

open MIR
open MIR_Optimize
open IRPrinter

type TestResult = Result<unit, string>

let private singleOptimizedFunction (testName: string) (functions: Function list) : Result<Function, string> =
    match functions with
    | [func] -> Ok func
    | [] -> Error $"{testName}: optimizer returned no functions"
    | _ -> Error $"{testName}: optimizer returned multiple functions"

let private optimizedBlockForLabel
    (testName: string)
    (label: Label)
    (optimizedFunc: Function)
    : Result<BasicBlock, string> =
    match Map.tryFind label optimizedFunc.CFG.Blocks with
    | Some block -> Ok block
    | None -> Error $"{testName}: optimizer removed expected block {label}"

let testCseAfterCopyPropFixpoint () : TestResult =
    let entry = Label "entry"
    let block: BasicBlock = {
        Label = entry
        Instrs = [
            Mov (VReg 2, Register (VReg 0), Some AST.TInt64)
            BinOp (VReg 3, Add, Register (VReg 2), Register (VReg 1), AST.TInt64)
            BinOp (VReg 4, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
            BinOp (VReg 5, Add, Register (VReg 3), Register (VReg 4), AST.TInt64)
        ]
        Terminator = Ret (Register (VReg 5))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [ (entry, block) ]
    }
    let func: Function = {
        Name = "fixpoint_cse"
        TypedParams = [
            { Reg = VReg 0; Type = AST.TInt64 }
            { Reg = VReg 1; Type = AST.TInt64 }
        ]
        ReturnType = AST.TInt64
        CFG = cfg
        FloatRegs = Set.empty
    }
    let program = Program ([func], Map.empty, Map.empty)

    let (Program (functions, _, _)) = optimizeProgram program
    match singleOptimizedFunction "testCseAfterCopyPropFixpoint" functions with
    | Error e -> Error e
    | Ok optimizedFunc ->
        match optimizedBlockForLabel "testCseAfterCopyPropFixpoint" entry optimizedFunc with
        | Error e -> Error e
        | Ok optimizedBlock ->
            let expectedInstrs = [
                BinOp (VReg 3, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
                BinOp (VReg 5, Add, Register (VReg 3), Register (VReg 3), AST.TInt64)
            ]
            let expectedBlock = { block with Instrs = expectedInstrs }

            if optimizedBlock = expectedBlock then
                Ok ()
            else
                let actual = formatMIR (Program ([optimizedFunc], Map.empty, Map.empty))
                Error $"MIR optimization did not reach fixpoint.\nActual:\n{actual}"

let testDceRemovesSelfReferentialDeadPhi () : TestResult =
    let entry = Label "entry"
    let loop = Label "loop"
    let exitLabel = Label "exit"

    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = []
        Terminator = Jump loop
    }

    let loopBlock: BasicBlock = {
        Label = loop
        Instrs = [
            Phi (
                VReg 1,
                [
                    (Register (VReg 0), entry)
                    (Register (VReg 1), loop)
                ],
                Some AST.TBool
            )
        ]
        Terminator = Branch (Register (VReg 0), exitLabel, loop)
    }

    let exitBlock: BasicBlock = {
        Label = exitLabel
        Instrs = []
        Terminator = Ret (Register (VReg 0))
    }

    let cfg: CFG = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, entryBlock)
                (loop, loopBlock)
                (exitLabel, exitBlock)
            ]
    }

    let func: Function = {
        Name = "dead_phi_cycle"
        TypedParams = [{ Reg = VReg 0; Type = AST.TBool }]
        ReturnType = AST.TBool
        CFG = cfg
        FloatRegs = Set.empty
    }

    let program = Program ([func], Map.empty, Map.empty)
    let (Program (functions, _, _)) = optimizeProgram program
    match singleOptimizedFunction "testDceRemovesSelfReferentialDeadPhi" functions with
    | Error e -> Error e
    | Ok optimizedFunc ->
        match optimizedBlockForLabel "testDceRemovesSelfReferentialDeadPhi" loop optimizedFunc with
        | Error e -> Error e
        | Ok optimizedLoop ->
            let hasPhi =
                optimizedLoop.Instrs
                |> List.exists (function
                    | Phi _ -> true
                    | _ -> false)

            if hasPhi then
                let actual = formatMIR (Program ([optimizedFunc], Map.empty, Map.empty))
                Error $"Expected dead self-referential phi to be removed by DCE.\nActual:\n{actual}"
            else
                Ok ()

let testCfgSimplifyRemovesRetPhiJoin () : TestResult =
    let entry = Label "entry"
    let thenLabel = Label "then"
    let elseLabel = Label "else"
    let joinLabel = Label "join"

    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = []
        Terminator = Branch (Register (VReg 0), thenLabel, elseLabel)
    }

    let thenBlock: BasicBlock = {
        Label = thenLabel
        Instrs = []
        Terminator = Jump joinLabel
    }

    let elseBlock: BasicBlock = {
        Label = elseLabel
        Instrs = []
        Terminator = Jump joinLabel
    }

    let joinBlock: BasicBlock = {
        Label = joinLabel
        Instrs = [
            Phi (
                VReg 1,
                [
                    (Int64Const 1L, thenLabel)
                    (Int64Const 2L, elseLabel)
                ],
                Some AST.TInt64
            )
        ]
        Terminator = Ret (Register (VReg 1))
    }

    let cfg: CFG = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, entryBlock)
                (thenLabel, thenBlock)
                (elseLabel, elseBlock)
                (joinLabel, joinBlock)
            ]
    }

    let func: Function = {
        Name = "ret_phi_join"
        TypedParams = [{ Reg = VReg 0; Type = AST.TBool }]
        ReturnType = AST.TInt64
        CFG = cfg
        FloatRegs = Set.empty
    }

    let program = Program ([func], Map.empty, Map.empty)
    let (Program (functions, _, _)) = optimizeProgram program
    let optimizedFunc = functions |> List.head
    let blocks = optimizedFunc.CFG.Blocks

    let joinRemoved = not (Map.containsKey joinLabel blocks)
    let thenRet =
        match Map.tryFind thenLabel blocks with
        | Some block -> block.Terminator = Ret (Int64Const 1L)
        | None -> false
    let elseRet =
        match Map.tryFind elseLabel blocks with
        | Some block -> block.Terminator = Ret (Int64Const 2L)
        | None -> false

    if joinRemoved && thenRet && elseRet then
        Ok ()
    else
        let actual = formatMIR (Program ([optimizedFunc], Map.empty, Map.empty))
        Error $"Expected ret-phi join simplification.\nActual:\n{actual}"

let testSelfComparisonFoldingRequiresConcreteSafeType () : TestResult =
    let sameOperand = Register (VReg 0)

    let cases = [
        ("generic equality", Eq, AST.TVar "a")
        ("float equality", Eq, AST.TFloat64)
        ("string equality", Eq, AST.TString)
        ("generic less-than", Lt, AST.TVar "a")
        ("float less-than", Lt, AST.TFloat64)
        ("generic greater-or-equal", Gte, AST.TVar "a")
        ("float greater-or-equal", Gte, AST.TFloat64)
    ]

    let folded =
        cases
        |> List.choose (fun (name, op, opType) ->
            match tryFoldBinOp op sameOperand sameOperand opType with
            | Some result -> Some $"{name} folded to {result}"
            | None -> None)

    match folded with
    | [] -> Ok ()
    | first :: _ -> Error $"Expected self-comparison with non-concrete-safe type to stay unfolded, but {first}"

let testSelfComparisonFoldingRequiresSameRegister () : TestResult =
    let left = Register (VReg 1)
    let right = Register (VReg 0)

    let cases = [
        ("equality", Eq)
        ("inequality", Neq)
        ("less-than", Lt)
        ("greater-than", Gt)
        ("less-or-equal", Lte)
        ("greater-or-equal", Gte)
    ]

    let folded =
        cases
        |> List.choose (fun (name, op) ->
            match tryFoldBinOp op left right AST.TInt64 with
            | Some result -> Some $"{name} folded to {result}"
            | None -> None)

    match folded with
    | [] -> Ok ()
    | first :: _ -> Error $"Expected comparison of distinct registers to stay unfolded, but {first}"

let tests = [
    ("MIR optimize fixed point CSE after copy prop", testCseAfterCopyPropFixpoint)
    ("MIR optimize removes dead self-referential phi", testDceRemovesSelfReferentialDeadPhi)
    ("MIR optimize removes ret-phi join blocks", testCfgSimplifyRemovesRetPhiJoin)
    ("MIR self-comparison folding requires concrete safe type", testSelfComparisonFoldingRequiresConcreteSafeType)
    ("MIR self-comparison folding requires same register", testSelfComparisonFoldingRequiresSameRegister)
]
