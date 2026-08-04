// MIROptimizeTests.fs - Unit tests for MIR optimizer fixpoint behavior
//
// Verifies MIR optimizer transformations that depend on fixpoint iteration or
// require direct construction of CFG shapes not preserved by earlier passes.

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
    | None ->
        let actual = formatMIR (Program ([optimizedFunc], Map.empty, Map.empty))
        Error $"{testName}: optimizer removed expected block {label}.\nActual:\n{actual}"

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

let testCseReusesDominatingExpressions () : TestResult =
    let entry = Label "entry"
    let bridge = Label "bridge"
    let child = Label "child"
    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [
            BinOp (VReg 2, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
            UnaryOp (VReg 3, Neg, Register (VReg 0))
        ]
        Terminator = Jump bridge
    }
    let bridgeBlock: BasicBlock = {
        Label = bridge
        Instrs = []
        Terminator = Jump child
    }
    let childBlock: BasicBlock = {
        Label = child
        Instrs = [
            BinOp (VReg 4, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
            UnaryOp (VReg 5, Neg, Register (VReg 0))
        ]
        Terminator = Ret (Register (VReg 4))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, entryBlock)
                (bridge, bridgeBlock)
                (child, childBlock)
            ]
    }

    let (optimized, changed) = applyCSE cfg
    let expectedChild = {
        childBlock with
            Instrs = [
                Mov (VReg 4, Register (VReg 2), None)
                Mov (VReg 5, Register (VReg 3), None)
            ]
    }

    match Map.tryFind child optimized.Blocks with
    | Some actualChild when changed && actualChild = expectedChild -> Ok ()
    | _ ->
        let func = {
            Name = "dominating_cse"
            TypedParams = [
                { Reg = VReg 0; Type = AST.TInt64 }
                { Reg = VReg 1; Type = AST.TInt64 }
            ]
            ReturnType = AST.TInt64
            CFG = optimized
            FloatRegs = Set.empty
        }
        let actual = formatMIR (Program ([func], Map.empty, Map.empty))
        Error $"Expected binary and unary expressions from the dominating entry block to be reused.\nActual:\n{actual}"

let testCsePreservesExpressionsAcrossSiblingBlocks () : TestResult =
    let entry = Label "entry"
    let left = Label "left"
    let right = Label "right"
    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = []
        Terminator = Branch (Register (VReg 2), left, right)
    }
    let leftBlock: BasicBlock = {
        Label = left
        Instrs = [
            BinOp (VReg 3, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
            UnaryOp (VReg 4, Neg, Register (VReg 0))
        ]
        Terminator = Ret (Register (VReg 3))
    }
    let rightBlock: BasicBlock = {
        Label = right
        Instrs = [
            BinOp (VReg 5, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
            UnaryOp (VReg 6, Neg, Register (VReg 0))
        ]
        Terminator = Ret (Register (VReg 5))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, entryBlock)
                (left, leftBlock)
                (right, rightBlock)
            ]
    }

    let (optimized, changed) = applyCSE cfg

    if not changed && optimized = cfg then
        Ok ()
    else
        let func = {
            Name = "sibling_cse"
            TypedParams = [
                { Reg = VReg 0; Type = AST.TInt64 }
                { Reg = VReg 1; Type = AST.TInt64 }
                { Reg = VReg 2; Type = AST.TBool }
            ]
            ReturnType = AST.TInt64
            CFG = optimized
            FloatRegs = Set.empty
        }
        let actual = formatMIR (Program ([func], Map.empty, Map.empty))
        Error $"Expected duplicate expressions in non-dominating sibling blocks to remain independent.\nActual:\n{actual}"

let testCseDoesNotReuseExpressionsAcrossRefCountDecrement () : TestResult =
    let entry = Label "entry"
    let child = Label "child"
    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [
            BinOp (VReg 3, Add, Register (VReg 1), Register (VReg 2), AST.TInt64)
            RefCountDecString (Register (VReg 0))
        ]
        Terminator = Jump child
    }
    let childBlock: BasicBlock = {
        Label = child
        Instrs = [
            BinOp (VReg 4, Add, Register (VReg 1), Register (VReg 2), AST.TInt64)
        ]
        Terminator = Ret (Register (VReg 4))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, entryBlock); (child, childBlock)]
    }

    let (optimized, changed) = applyCSE cfg

    if not changed && optimized = cfg then
        Ok ()
    else
        let func = {
            Name = "refcount_barrier_cse"
            TypedParams = [
                { Reg = VReg 0; Type = AST.TString }
                { Reg = VReg 1; Type = AST.TInt64 }
                { Reg = VReg 2; Type = AST.TInt64 }
            ]
            ReturnType = AST.TInt64
            CFG = optimized
            FloatRegs = Set.empty
        }
        let actual = formatMIR (Program ([func], Map.empty, Map.empty))
        Error $"Expected the reference-count decrement to invalidate available expressions.\nActual:\n{actual}"

let testCseDoesNotExtendExpressionsAcrossCalls () : TestResult =
    let entry = Label "entry"
    let child = Label "child"
    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [
            BinOp (VReg 2, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
            Call (VReg 3, "observe", [], [], AST.TUnit)
        ]
        Terminator = Jump child
    }
    let childBlock: BasicBlock = {
        Label = child
        Instrs = [
            BinOp (VReg 4, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
        ]
        Terminator = Ret (Register (VReg 4))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, entryBlock); (child, childBlock)]
    }

    let (optimized, changed) = applyCSE cfg

    if not changed && optimized = cfg then
        Ok ()
    else
        let func = {
            Name = "call_barrier_cse"
            TypedParams = [
                { Reg = VReg 0; Type = AST.TInt64 }
                { Reg = VReg 1; Type = AST.TInt64 }
            ]
            ReturnType = AST.TInt64
            CFG = optimized
            FloatRegs = Set.empty
        }
        let actual = formatMIR (Program ([func], Map.empty, Map.empty))
        Error $"Expected the call to prevent extension of expression availability.\nActual:\n{actual}"

let testCseDoesNotExportNonScalarBinaryTypes () : TestResult =
    let entry = Label "entry"
    let child = Label "child"
    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [BinOp (VReg 2, Add, Register (VReg 0), Register (VReg 1), AST.TUnit)]
        Terminator = Jump child
    }
    let childBlock: BasicBlock = {
        Label = child
        Instrs = [BinOp (VReg 3, Add, Register (VReg 0), Register (VReg 1), AST.TUnit)]
        Terminator = Ret (Register (VReg 3))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, entryBlock); (child, childBlock)]
    }

    let (optimized, changed) = applyCSE cfg

    if not changed && optimized = cfg then
        Ok ()
    else
        Error "Expected a binary expression with the non-scalar TUnit type to remain block-local"

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
    match singleOptimizedFunction "testCfgSimplifyRemovesRetPhiJoin" functions with
    | Error e -> Error e
    | Ok optimizedFunc ->
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

let testEmptyBlockRemovalRewritesPhiSourceToPredecessor () : TestResult =
    let entry = Label "entry"
    let empty = Label "empty"
    let join = Label "join"

    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [Mov (VReg 0, Int64Const 41L, Some AST.TInt64)]
        Terminator = Jump empty
    }

    let emptyBlock: BasicBlock = {
        Label = empty
        Instrs = []
        Terminator = Jump join
    }

    let joinBlock: BasicBlock = {
        Label = join
        Instrs = [
            Phi (
                VReg 1,
                [(Register (VReg 0), empty)],
                Some AST.TInt64
            )
            BinOp (VReg 2, Add, Register (VReg 1), Int64Const 1L, AST.TInt64)
        ]
        Terminator = Ret (Register (VReg 2))
    }

    let cfg: CFG = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, entryBlock)
                (empty, emptyBlock)
                (join, joinBlock)
            ]
    }

    let (optimized, changed) = simplifyEmptyBlocks cfg

    match Map.tryFind join optimized.Blocks with
    | Some block ->
        match block.Instrs with
        | Phi (_, [(Register (VReg 0), sourceLabel)], _) :: _ when changed && sourceLabel = entry -> Ok ()
        | _ ->
            let actual = formatMIR (Program ([{ Name = "empty_phi"; TypedParams = []; ReturnType = AST.TInt64; CFG = optimized; FloatRegs = Set.empty }], Map.empty, Map.empty))
            Error $"Expected phi source to be rewritten from removed empty block to entry predecessor.\nActual:\n{actual}"
    | None ->
        Error "Expected join block to remain after empty block removal"

let testSameTargetBranchBecomesJumpAndDropsCondition () : TestResult =
    let entry = Label "entry"
    let target = Label "target"

    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [
            BinOp (
                VReg 2,
                Eq,
                Register (VReg 0),
                Register (VReg 1),
                AST.TBool
            )
        ]
        Terminator = Branch (Register (VReg 2), target, target)
    }

    let targetBlock: BasicBlock = {
        Label = target
        Instrs = []
        Terminator = Ret (Int64Const 1L)
    }

    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, entryBlock); (target, targetBlock)]
    }

    let optimized = optimizeCFG cfg
    match Map.tryFind entry optimized.Blocks with
    | Some block when block.Instrs = [] && block.Terminator = Jump target -> Ok ()
    | _ ->
        let actual =
            formatMIR (
                Program (
                    [{
                        Name = "same_target_branch"
                        TypedParams = [
                            { Reg = VReg 0; Type = AST.TBool }
                            { Reg = VReg 1; Type = AST.TBool }
                        ]
                        ReturnType = AST.TInt64
                        CFG = optimized
                        FloatRegs = Set.empty
                    }],
                    Map.empty,
                    Map.empty
                )
            )
        Error $"Expected same-target branch to become a jump and its dead condition to be removed.\nActual:\n{actual}"

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
    ("MIR CSE reuses dominating binary and unary expressions", testCseReusesDominatingExpressions)
    ("MIR CSE preserves binary and unary expressions across siblings", testCsePreservesExpressionsAcrossSiblingBlocks)
    ("MIR CSE invalidates expressions at reference-count decrements", testCseDoesNotReuseExpressionsAcrossRefCountDecrement)
    ("MIR CSE does not extend expressions across calls", testCseDoesNotExtendExpressionsAcrossCalls)
    ("MIR CSE does not export non-scalar binary types", testCseDoesNotExportNonScalarBinaryTypes)
    ("MIR optimize removes dead self-referential phi", testDceRemovesSelfReferentialDeadPhi)
    ("MIR optimize removes ret-phi join blocks", testCfgSimplifyRemovesRetPhiJoin)
    ("MIR empty block removal rewrites phi source to predecessor", testEmptyBlockRemovalRewritesPhiSourceToPredecessor)
    ("MIR same-target branch becomes jump and drops condition", testSameTargetBranchBecomesJumpAndDropsCondition)
    ("MIR self-comparison folding requires concrete safe type", testSelfComparisonFoldingRequiresConcreteSafeType)
    ("MIR self-comparison folding requires same register", testSelfComparisonFoldingRequiresSameRegister)
]
