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

let private directCallCount (funcName: string) (func: Function) : int =
    func.CFG.Blocks
    |> Map.toList
    |> List.collect (fun (_, block) -> block.Instrs)
    |> List.filter (fun instr ->
        match instr with
        | Call (_, calledName, _, _, _) -> calledName = funcName
        | _ -> false)
    |> List.length

let private scalarIdentityFunction (name: string) : Function =
    let entry = Label $"{name}_entry"
    {
        Name = name
        TypedParams = [{ Reg = VReg 0; Type = AST.TInt64 }]
        ReturnType = AST.TInt64
        CFG = {
            Entry = entry
            Blocks =
                Map.ofList [
                    (entry, {
                        Label = entry
                        Instrs = []
                        Terminator = Ret (Register (VReg 0))
                    })
                ]
        }
        FloatRegs = Set.empty
    }

let testCseReusesEffectFreeDirectScalarCalls () : TestResult =
    let entry = Label "caller_entry"
    let caller = {
        (scalarIdentityFunction "caller") with
            CFG = {
                Entry = entry
                Blocks =
                    Map.ofList [
                        (entry, {
                            Label = entry
                            Instrs = [
                                Call (VReg 1, "pure", [Register (VReg 0)], [AST.TInt64], AST.TInt64)
                                Call (VReg 2, "pure", [Register (VReg 0)], [AST.TInt64], AST.TInt64)
                                BinOp (VReg 3, Add, Register (VReg 1), Register (VReg 2), AST.TInt64)
                            ]
                            Terminator = Ret (Register (VReg 3))
                        })
                    ]
            }
    }
    let (Program (functions, _, _)) =
        optimizeProgram
            (Program ([scalarIdentityFunction "pure"; caller], Map.empty, Map.empty))

    match functions |> List.tryFind (fun func -> func.Name = "caller") with
    | Some optimizedCaller when directCallCount "pure" optimizedCaller = 1 -> Ok ()
    | Some optimizedCaller ->
        let remainingCalls = directCallCount "pure" optimizedCaller
        Error
            $"Expected one effect-free direct call after CSE, found {remainingCalls}."
    | None -> Error "Expected optimized caller function"

let testCseReusesDominatingEffectFreeDirectScalarCalls () : TestResult =
    let entry = Label "caller_entry"
    let child = Label "caller_child"
    let cfg = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, {
                    Label = entry
                    Instrs = [Call (VReg 1, "pure", [Register (VReg 0)], [AST.TInt64], AST.TInt64)]
                    Terminator = Jump child
                })
                (child, {
                    Label = child
                    Instrs = [Call (VReg 2, "pure", [Register (VReg 0)], [AST.TInt64], AST.TInt64)]
                    Terminator = Ret (Register (VReg 2))
                })
            ]
    }
    let (optimized, changed) =
        applyCSEWithEffectFreeCalls (Set.ofList ["pure"]) cfg
    let remainingCalls =
        optimized.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) -> block.Instrs)
        |> List.filter (function | Call (_, "pure", _, _, _) -> true | _ -> false)
        |> List.length
    if changed && remainingCalls = 1 then Ok ()
    else Error $"Expected one dominating effect-free call after CSE, found {remainingCalls}."

let testCseDirectCallsRespectBarriersAndScalarTypes () : TestResult =
    let entry = Label "entry"
    let call resultType dest =
        Call (dest, "pure", [Register (VReg 0)], [resultType], resultType)
    let verifyUnchanged name middle resultType =
        let cfg = {
            Entry = entry
            Blocks =
                Map.ofList [
                    (entry, {
                        Label = entry
                        Instrs = [call resultType (VReg 1); middle; call resultType (VReg 2)]
                        Terminator = Ret (Register (VReg 2))
                    })
                ]
        }
        let (optimized, changed) =
            applyCSEWithEffectFreeCalls (Set.ofList ["pure"]) cfg
        let remainingCalls =
            match Map.tryFind entry optimized.Blocks with
            | Some block ->
                block.Instrs
                |> List.filter (function | Call (_, "pure", _, _, _) -> true | _ -> false)
                |> List.length
            | None -> 0
        if not changed && remainingCalls = 2 then Ok ()
        else Error $"Expected {name} to prevent direct-call CSE."

    [ verifyUnchanged
          "an unproven call"
          (Call (VReg 3, "observe", [], [], AST.TUnit))
          AST.TInt64
      verifyUnchanged "a heap allocation" (HeapAlloc (VReg 3, 16)) AST.TInt64
      verifyUnchanged
          "a reference-count decrement"
          (RefCountDec (VReg 3, 8, GenericHeap, None))
          AST.TInt64
      verifyUnchanged
          "a managed result type"
          (Mov (VReg 3, Int64Const 0L, None))
          AST.TString ]
    |> List.tryPick (function | Error error -> Some error | Ok () -> None)
    |> function | Some error -> Error error | None -> Ok ()

let testCseDoesNotReuseThrowingDirectCalls () : TestResult =
    let entry = Label "entry"
    let throwing = {
        (scalarIdentityFunction "throwing") with
            CFG = {
                Entry = entry
                Blocks =
                    Map.ofList [
                        (entry, {
                            Label = entry
                            Instrs = [RuntimeError "boom"]
                            Terminator = Ret (Int64Const 0L)
                        })
                    ]
            }
    }
    let caller = {
        (scalarIdentityFunction "throwing_caller") with
            CFG = {
                Entry = entry
                Blocks =
                    Map.ofList [
                        (entry, {
                            Label = entry
                            Instrs = [
                                Call (VReg 1, "throwing", [Register (VReg 0)], [AST.TInt64], AST.TInt64)
                                Call (VReg 2, "throwing", [Register (VReg 0)], [AST.TInt64], AST.TInt64)
                            ]
                            Terminator = Ret (Register (VReg 2))
                        })
                    ]
            }
    }
    let (Program (functions, _, _)) =
        optimizeProgram (Program ([throwing; caller], Map.empty, Map.empty))
    match functions |> List.tryFind (fun func -> func.Name = "throwing_caller") with
    | Some optimizedCaller when directCallCount "throwing" optimizedCaller = 2 -> Ok ()
    | Some optimizedCaller ->
        let remainingCalls = directCallCount "throwing" optimizedCaller
        Error
            $"Expected throwing calls to remain, found {remainingCalls}."
    | None -> Error "Expected optimized throwing caller function"

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

let testCseReusesDominatingScalarHeapLoad () : TestResult =
    let entry = Label "entry"
    let bridge = Label "bridge"
    let child = Label "child"
    let valueType = AST.TInt64
    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [HeapLoad (VReg 1, VReg 0, 8, Some valueType)]
        Terminator = Jump bridge
    }
    let bridgeBlock: BasicBlock = {
        Label = bridge
        Instrs = []
        Terminator = Jump child
    }
    let childBlock: BasicBlock = {
        Label = child
        Instrs = [HeapLoad (VReg 2, VReg 0, 8, Some valueType)]
        Terminator = Ret (Register (VReg 2))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, entryBlock); (bridge, bridgeBlock); (child, childBlock)]
    }

    let (optimized, changed) = applyCSE cfg
    let expectedChild = {
        childBlock with
            Instrs = [Mov (VReg 2, Register (VReg 1), Some valueType)]
    }

    match Map.tryFind child optimized.Blocks with
    | Some actualChild when changed && actualChild = expectedChild -> Ok ()
    | _ ->
        let func = {
            Name = "dominating_scalar_heap_load_cse"
            TypedParams = [{ Reg = VReg 0; Type = AST.TTuple [valueType; valueType] }]
            ReturnType = valueType
            CFG = optimized
            FloatRegs = Set.empty
        }
        let actual = formatMIR (Program ([func], Map.empty, Map.empty))
        Error $"Expected an exact scalar heap load from the dominating entry block to be reused.\nActual:\n{actual}"

let testCseDoesNotReuseDominatingScalarHeapLoadAcrossBarriers () : TestResult =
    let entry = Label "entry"
    let child = Label "child"
    let valueType = AST.TInt64
    let barriers = [
        ("call", Call (VReg 3, "observe", [], [], AST.TUnit))
        ("heap allocation", HeapAlloc (VReg 3, 16))
        ("heap store", HeapStore (VReg 0, 8, Int64Const 99L, Some valueType))
        ("raw memory read", RawGet (VReg 3, Register (VReg 0), Int64Const 0L, Some valueType))
        ("raw memory write", RawWriteWord (Register (VReg 0), Int64Const 0L, Int64Const 99L))
        ("reference count", RefCountInc (VReg 0, 16, GenericHeap, None))
        ("non-scalar load", HeapLoad (VReg 3, VReg 0, 16, Some AST.TString))
    ]

    let rec checkBarriers remaining =
        match remaining with
        | [] -> Ok ()
        | (barrierName, barrier) :: rest ->
            let entryBlock: BasicBlock = {
                Label = entry
                Instrs = [HeapLoad (VReg 1, VReg 0, 8, Some valueType); barrier]
                Terminator = Jump child
            }
            let childBlock: BasicBlock = {
                Label = child
                Instrs = [HeapLoad (VReg 2, VReg 0, 8, Some valueType)]
                Terminator = Ret (Register (VReg 2))
            }
            let cfg: CFG = {
                Entry = entry
                Blocks = Map.ofList [(entry, entryBlock); (child, childBlock)]
            }
            let (optimized, changed) = applyCSE cfg

            if not changed && optimized = cfg then
                checkBarriers rest
            else
                Error $"Expected {barrierName} to invalidate a dominated scalar heap load"

    checkBarriers barriers

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

let testCseKeepsScalarHeapLoadsAvailableAcrossFloatSqrt () : TestResult =
    let entry = Label "entry"
    let block: BasicBlock = {
        Label = entry
        Instrs = [
            HeapLoad (VReg 1, VReg 0, 8, Some AST.TFloat64)
            FloatSqrt (VReg 2, Register (VReg 1))
            HeapLoad (VReg 3, VReg 0, 8, Some AST.TFloat64)
        ]
        Terminator = Ret (Register (VReg 3))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, block)]
    }

    let (optimized, changed) = applyCSE cfg
    let expected = {
        block with
            Instrs = [
                HeapLoad (VReg 1, VReg 0, 8, Some AST.TFloat64)
                FloatSqrt (VReg 2, Register (VReg 1))
                Mov (VReg 3, Register (VReg 1), Some AST.TFloat64)
            ]
    }

    match Map.tryFind entry optimized.Blocks with
    | Some actual when changed && actual = expected -> Ok ()
    | _ -> Error "Expected FloatSqrt to preserve exact scalar heap-load availability"

let testCseDoesNotExportScalarHeapLoadsAcrossFloatSqrt () : TestResult =
    let entry = Label "entry"
    let child = Label "child"
    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [
            HeapLoad (VReg 1, VReg 0, 8, Some AST.TFloat64)
            FloatSqrt (VReg 2, Register (VReg 1))
        ]
        Terminator = Jump child
    }
    let childBlock: BasicBlock = {
        Label = child
        Instrs = [HeapLoad (VReg 3, VReg 0, 8, Some AST.TFloat64)]
        Terminator = Ret (Register (VReg 3))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, entryBlock); (child, childBlock)]
    }

    let (optimized, changed) = applyCSE cfg

    if not changed && optimized = cfg then Ok ()
    else Error "Expected FloatSqrt to stop scalar heap-load availability at the block boundary"

let testCseDoesNotKeepDirectCallsAvailableAcrossFloatSqrt () : TestResult =
    let entry = Label "entry"
    let block: BasicBlock = {
        Label = entry
        Instrs = [
            Call (VReg 1, "pure", [], [], AST.TFloat64)
            FloatSqrt (VReg 2, Register (VReg 1))
            Call (VReg 3, "pure", [], [], AST.TFloat64)
        ]
        Terminator = Ret (Register (VReg 3))
    }
    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, block)]
    }

    let (optimized, changed) =
        applyCSEWithEffectFreeCalls (Set.singleton "pure") cfg

    if not changed && optimized = cfg then Ok ()
    else Error "Expected FloatSqrt to retain the conservative direct-call CSE boundary"

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

let testLinearBlockMergePreservesPhiSources () : TestResult =
    let entry = Label "entry"
    let body = Label "body"
    let alternate = Label "alternate"
    let join = Label "join"

    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [BinOp (VReg 2, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)]
        Terminator = Jump body
    }

    let bodyBlock: BasicBlock = {
        Label = body
        Instrs = [
            Phi (VReg 3, [(Register (VReg 2), entry)], Some AST.TInt64)
            BinOp (VReg 4, Add, Register (VReg 3), Int64Const 1L, AST.TInt64)
        ]
        Terminator = Jump join
    }

    let alternateBlock: BasicBlock = {
        Label = alternate
        Instrs = [Mov (VReg 5, Int64Const 0L, Some AST.TInt64)]
        Terminator = Jump join
    }

    let joinBlock: BasicBlock = {
        Label = join
        Instrs = [
            Phi (
                VReg 6,
                [(Register (VReg 4), body); (Register (VReg 5), alternate)],
                Some AST.TInt64
            )
        ]
        Terminator = Ret (Register (VReg 6))
    }

    let cfg: CFG = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, entryBlock)
                (body, bodyBlock)
                (alternate, alternateBlock)
                (join, joinBlock)
            ]
    }

    let (optimized, changed) = mergeLinearBlocks cfg
    let expectedEntry = {
        entryBlock with
            Instrs =
                entryBlock.Instrs
                @ [
                    Mov (VReg 3, Register (VReg 2), Some AST.TInt64)
                    BinOp (VReg 4, Add, Register (VReg 3), Int64Const 1L, AST.TInt64)
                ]
            Terminator = Jump join
    }
    let expectedJoin = {
        joinBlock with
            Instrs = [
                Phi (
                    VReg 6,
                    [(Register (VReg 4), entry); (Register (VReg 5), alternate)],
                    Some AST.TInt64
                )
            ]
    }

    if changed
       && not (Map.containsKey body optimized.Blocks)
       && Map.tryFind entry optimized.Blocks = Some expectedEntry
       && Map.tryFind join optimized.Blocks = Some expectedJoin then
        Ok ()
    else
        let actual =
            formatMIR (Program ([{ Name = "linear_phi"; TypedParams = []; ReturnType = AST.TInt64; CFG = optimized; FloatRegs = Set.empty }], Map.empty, Map.empty))
        Error $"Expected linear block merge to preserve phi values and source labels.\nActual:\n{actual}"

let testLinearBlockMergeExposesLocalCSE () : TestResult =
    let entry = Label "entry"
    let body = Label "body"
    let firstAdd = BinOp (VReg 2, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)

    let entryBlock: BasicBlock = {
        Label = entry
        Instrs = [firstAdd]
        Terminator = Jump body
    }

    let bodyBlock: BasicBlock = {
        Label = body
        Instrs = [
            BinOp (VReg 3, Add, Register (VReg 0), Register (VReg 1), AST.TInt64)
            BinOp (VReg 4, Add, Register (VReg 2), Register (VReg 3), AST.TInt64)
        ]
        Terminator = Ret (Register (VReg 4))
    }

    let cfg: CFG = {
        Entry = entry
        Blocks = Map.ofList [(entry, entryBlock); (body, bodyBlock)]
    }

    let optimized = optimizeCFG cfg
    let expectedBlock = {
        entryBlock with
            Instrs = [
                firstAdd
                BinOp (VReg 4, Add, Register (VReg 2), Register (VReg 2), AST.TInt64)
            ]
            Terminator = Ret (Register (VReg 4))
    }

    if optimized.Blocks = Map.ofList [(entry, expectedBlock)] then
        Ok ()
    else
        let actual =
            formatMIR (Program ([{ Name = "linear_cse"; TypedParams = []; ReturnType = AST.TInt64; CFG = optimized; FloatRegs = Set.empty }], Map.empty, Map.empty))
        Error $"Expected linear block merge to expose duplicate expressions to local CSE.\nActual:\n{actual}"

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
    | Some block when block.Instrs = [] && block.Terminator = Ret (Int64Const 1L) && Map.count optimized.Blocks = 1 -> Ok ()
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

type private EstablishedEdge =
    | TrueEdge
    | FalseEdge

let private basicBlock label instrs terminator : BasicBlock = {
    Label = label
    Instrs = instrs
    Terminator = terminator
}

let private expectRedundantSuccessorBranchEliminated
    (edge: EstablishedEdge)
    : TestResult =
    let entry = Label "entry"
    let successor = Label "successor"
    let sibling = Label "sibling"
    let trueResult = Label "true_result"
    let falseResult = Label "false_result"

    let (entryTerminator, expectedSuccessorTerminator) =
        match edge with
        | TrueEdge ->
            (Branch (Register (VReg 0), successor, sibling), Jump trueResult)
        | FalseEdge ->
            (Branch (Register (VReg 0), sibling, successor), Jump falseResult)

    let before: CFG = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, basicBlock entry [] entryTerminator)
                (successor, basicBlock successor [] (Branch (Register (VReg 0), trueResult, falseResult)))
                (sibling, basicBlock sibling [] (Ret (Int64Const 2L)))
                (trueResult, basicBlock trueResult [] (Ret (Int64Const 1L)))
                (falseResult, basicBlock falseResult [] (Ret (Int64Const 0L)))
            ]
    }

    let (after, changed) = simplifyBranchesKnownFromPredecessor before
    match Map.tryFind successor after.Blocks with
    | Some block when changed && block.Terminator = expectedSuccessorTerminator -> Ok ()
    | _ -> Error $"Expected successor terminator {expectedSuccessorTerminator} after simplifying {edge}"

let testTrueEdgeEliminatesRedundantSuccessorBranch () : TestResult =
    expectRedundantSuccessorBranchEliminated TrueEdge

let testFalseEdgeEliminatesRedundantSuccessorBranch () : TestResult =
    expectRedundantSuccessorBranchEliminated FalseEdge

let testMultiplePredecessorsKeepRepeatedSuccessorBranch () : TestResult =
    let entry = Label "entry"
    let alternate = Label "alternate"
    let successor = Label "successor"
    let trueResult = Label "true_result"
    let falseResult = Label "false_result"

    let cfg: CFG = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, basicBlock entry [] (Branch (Register (VReg 0), successor, alternate)))
                (alternate, basicBlock alternate [] (Jump successor))
                (successor, basicBlock successor [] (Branch (Register (VReg 0), trueResult, falseResult)))
                (trueResult, basicBlock trueResult [] (Ret (Int64Const 1L)))
                (falseResult, basicBlock falseResult [] (Ret (Int64Const 0L)))
            ]
    }

    let (optimized, changed) = simplifyBranchesKnownFromPredecessor cfg
    if not changed && optimized = cfg then
        Ok ()
    else
        let func = {
            Name = "multiple_predecessor_branch"
            TypedParams = [{ Reg = VReg 0; Type = AST.TBool }]
            ReturnType = AST.TInt64
            CFG = optimized
            FloatRegs = Set.empty
        }
        let actual = formatMIR (Program ([func], Map.empty, Map.empty))
        Error $"Expected a repeated condition with multiple predecessor edges to remain.\nActual:\n{actual}"

let testRedundantSuccessorBranchTrimsRemovedPhiEdge () : TestResult =
    let entry = Label "entry"
    let successor = Label "successor"
    let sibling = Label "sibling"
    let kept = Label "kept"
    let join = Label "join"

    let joinPhi =
        Phi (
            VReg 1,
            [
                (Int64Const 10L, successor)
                (Int64Const 20L, sibling)
                (Int64Const 30L, kept)
            ],
            Some AST.TInt64
        )

    let cfg: CFG = {
        Entry = entry
        Blocks =
            Map.ofList [
                (entry, basicBlock entry [] (Branch (Register (VReg 0), successor, sibling)))
                (successor, basicBlock successor [] (Branch (Register (VReg 0), kept, join)))
                (sibling, basicBlock sibling [] (Jump join))
                (kept, basicBlock kept [] (Jump join))
                (join, basicBlock join [joinPhi] (Ret (Register (VReg 1))))
            ]
    }

    let (simplified, branchChanged) = simplifyBranchesKnownFromPredecessor cfg
    let (trimmed, phiChanged) = eliminateUnreachableBlocks simplified
    match Map.tryFind join trimmed.Blocks with
    | Some joinBlock ->
        match joinBlock.Instrs with
        | [Phi (_, sources, _)] ->
            let sourceLabels = sources |> List.map snd |> Set.ofList
            let expectedLabels = Set.ofList [sibling; kept]
            if branchChanged && phiChanged && sourceLabels = expectedLabels then Ok ()
            else Error $"Expected phi sources {expectedLabels}, got {sourceLabels}"
        | _ -> Error "Expected the join block to retain one phi instruction"
    | None -> Error "Expected the reachable join block to remain"

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

let testLicmCanonicalizesMultipleLoopEntries () : TestResult =
    let entry = Label "entry"
    let left = Label "left"
    let right = Label "right"
    let header = Label "header"
    let latch = Label "latch"
    let exit = Label "exit"
    let preheader = Label "header_preheader"
    let cfg = {
        Entry = entry
        Blocks = Map.ofList [
            (entry, basicBlock entry [] (Branch (Register (VReg 0), left, right)))
            (left, basicBlock left [] (Jump header))
            (right, basicBlock right [] (Jump header))
            (header, basicBlock header [
                Phi (VReg 2, [(Register (VReg 1), left); (Register (VReg 1), right); (Register (VReg 4), latch)], Some AST.TInt64)
                BinOp (VReg 3, Mul, Register (VReg 0), Register (VReg 1), AST.TInt64)
            ] (Branch (Register (VReg 5), latch, exit)))
            (latch, basicBlock latch [] (Jump header))
            (exit, basicBlock exit [] (Ret (Register (VReg 3))))
        ]
    }
    let (optimized, changed) = applyLoopInvariantCodeMotion cfg
    match Map.tryFind preheader optimized.Blocks, Map.tryFind header optimized.Blocks with
    | Some preheaderBlock, Some headerBlock ->
        let entriesRewritten =
            [left; right]
            |> List.forall (fun label ->
                match Map.tryFind label optimized.Blocks with
                | Some block -> block.Terminator = Jump preheader
                | None -> false)
        let phiRewritten =
            match headerBlock.Instrs with
            | Phi (_, [(Register preheaderValue, source); (Register (VReg 4), latchSource)], _) :: rest ->
                source = preheader
                && latchSource = latch
                && preheaderBlock.Instrs = [Phi (preheaderValue, [(Register (VReg 1), left); (Register (VReg 1), right)], Some AST.TInt64); BinOp (VReg 3, Mul, Register (VReg 0), Register (VReg 1), AST.TInt64)]
                && not (rest |> List.contains (BinOp (VReg 3, Mul, Register (VReg 0), Register (VReg 1), AST.TInt64)))
            | _ -> false
        if changed && entriesRewritten && phiRewritten then Ok ()
        else Error "Expected multiple loop entries to merge through a deterministic preheader and hoist the invariant multiply"
    | _ -> Error "Expected a dedicated preheader and preserved loop header"

let testLicmRetainsExistingLoopPreheader () : TestResult =
    let entry = Label "entry"
    let preheader = Label "preheader"
    let header = Label "header"
    let latch = Label "latch"
    let exit = Label "exit"
    let invariant = BinOp (VReg 2, Mul, Register (VReg 0), Register (VReg 1), AST.TInt64)
    let cfg = {
        Entry = entry
        Blocks = Map.ofList [
            (entry, basicBlock entry [] (Jump preheader))
            (preheader, basicBlock preheader [] (Jump header))
            (header, basicBlock header [invariant] (Branch (Register (VReg 3), latch, exit)))
            (latch, basicBlock latch [] (Jump header))
            (exit, basicBlock exit [] (Ret (Register (VReg 2))))
        ]
    }
    let (optimized, changed) = applyLoopInvariantCodeMotion cfg
    match Map.tryFind preheader optimized.Blocks, Map.tryFind header optimized.Blocks with
    | Some preheaderBlock, Some headerBlock when changed && preheaderBlock.Instrs = [invariant] && headerBlock.Instrs = [] -> Ok ()
    | _ -> Error "Expected the existing preheader to be retained and used for LICM"

let testLicmCanonicalizesNestedLoopEntry () : TestResult =
    let entry = Label "entry"
    let left = Label "left"
    let right = Label "right"
    let outerHeader = Label "outer_header"
    let innerHeader = Label "inner_header"
    let innerLatch = Label "inner_latch"
    let outerLatch = Label "outer_latch"
    let outerExit = Label "outer_exit"
    let innerPreheader = Label "inner_header_preheader"
    let invariant = BinOp (VReg 3, Mul, Register (VReg 0), Register (VReg 1), AST.TInt64)
    let cfg = {
        Entry = entry
        Blocks = Map.ofList [
            (entry, basicBlock entry [] (Branch (Register (VReg 5), left, right)))
            (left, basicBlock left [] (Jump outerHeader))
            (right, basicBlock right [] (Jump outerHeader))
            (outerHeader, basicBlock outerHeader [] (Branch (Register (VReg 2), innerHeader, outerExit)))
            (innerHeader, basicBlock innerHeader [invariant] (Branch (Register (VReg 4), innerLatch, outerLatch)))
            (innerLatch, basicBlock innerLatch [] (Jump innerHeader))
            (outerLatch, basicBlock outerLatch [] (Jump outerHeader))
            (outerExit, basicBlock outerExit [] (Ret (Register (VReg 3))))
        ]
    }
    let (optimized, changed) = applyLoopInvariantCodeMotion cfg
    match Map.tryFind innerPreheader optimized.Blocks, Map.tryFind outerHeader optimized.Blocks, Map.tryFind innerHeader optimized.Blocks with
    | Some preheaderBlock, Some outerHeaderBlock, Some innerHeaderBlock
        when changed
             && outerHeaderBlock.Terminator = Branch (Register (VReg 2), innerPreheader, outerExit)
             && preheaderBlock.Instrs = [invariant]
             && innerHeaderBlock.Instrs = [] -> Ok ()
    | _ -> Error "Expected nested-loop entry canonicalization to preserve the outer loop and hoist the inner invariant"

let tests = [
    ("MIR CSE reuses effect-free direct scalar calls", testCseReusesEffectFreeDirectScalarCalls)
    ("MIR CSE reuses dominating effect-free direct scalar calls", testCseReusesDominatingEffectFreeDirectScalarCalls)
    ("MIR CSE direct calls respect barriers and scalar types", testCseDirectCallsRespectBarriersAndScalarTypes)
    ("MIR CSE does not reuse throwing direct calls", testCseDoesNotReuseThrowingDirectCalls)
    ("MIR optimize fixed point CSE after copy prop", testCseAfterCopyPropFixpoint)
    ("MIR CSE reuses dominating binary and unary expressions", testCseReusesDominatingExpressions)
    ("MIR CSE reuses dominating scalar heap loads", testCseReusesDominatingScalarHeapLoad)
    ("MIR CSE scalar heap load barriers", testCseDoesNotReuseDominatingScalarHeapLoadAcrossBarriers)
    ("MIR CSE preserves binary and unary expressions across siblings", testCsePreservesExpressionsAcrossSiblingBlocks)
    ("MIR CSE invalidates expressions at reference-count decrements", testCseDoesNotReuseExpressionsAcrossRefCountDecrement)
    ("MIR CSE does not extend expressions across calls", testCseDoesNotExtendExpressionsAcrossCalls)
    ("MIR CSE does not export non-scalar binary types", testCseDoesNotExportNonScalarBinaryTypes)
    ("MIR CSE keeps scalar heap loads available across FloatSqrt", testCseKeepsScalarHeapLoadsAvailableAcrossFloatSqrt)
    ("MIR CSE does not export scalar heap loads across FloatSqrt", testCseDoesNotExportScalarHeapLoadsAcrossFloatSqrt)
    ("MIR CSE does not keep direct calls available across FloatSqrt", testCseDoesNotKeepDirectCallsAvailableAcrossFloatSqrt)
    ("MIR optimize removes dead self-referential phi", testDceRemovesSelfReferentialDeadPhi)
    ("MIR optimize removes ret-phi join blocks", testCfgSimplifyRemovesRetPhiJoin)
    ("MIR empty block removal rewrites phi source to predecessor", testEmptyBlockRemovalRewritesPhiSourceToPredecessor)
    ("MIR linear block merge preserves phi sources", testLinearBlockMergePreservesPhiSources)
    ("MIR linear block merge exposes local CSE", testLinearBlockMergeExposesLocalCSE)
    ("MIR same-target branch becomes jump and drops condition", testSameTargetBranchBecomesJumpAndDropsCondition)
    ("MIR true edge eliminates redundant successor branch", testTrueEdgeEliminatesRedundantSuccessorBranch)
    ("MIR false edge eliminates redundant successor branch", testFalseEdgeEliminatesRedundantSuccessorBranch)
    ("MIR multiple predecessors keep repeated successor branch", testMultiplePredecessorsKeepRepeatedSuccessorBranch)
    ("MIR redundant successor branch trims removed phi edge", testRedundantSuccessorBranchTrimsRemovedPhiEdge)
    ("MIR self-comparison folding requires concrete safe type", testSelfComparisonFoldingRequiresConcreteSafeType)
    ("MIR self-comparison folding requires same register", testSelfComparisonFoldingRequiresSameRegister)
    ("MIR LICM canonicalizes multiple loop entries", testLicmCanonicalizesMultipleLoopEntries)
    ("MIR LICM retains existing loop preheader", testLicmRetainsExistingLoopPreheader)
    ("MIR LICM canonicalizes nested loop entry", testLicmCanonicalizesNestedLoopEntry)
]
