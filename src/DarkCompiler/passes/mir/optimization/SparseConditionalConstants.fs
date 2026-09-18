// SparseConditionalConstants.fs - Joint SSA constant and CFG-edge analysis.

module MIRSparseConditionalConstants

open MIR
open MIROptimizationFacts
open MIRConstants

type private LatticeValue =
    | Unknown
    | Constant of Operand
    | Overdefined

type private AnalysisState = {
    Values: Map<VReg, LatticeValue>
    ExecutableBlocks: Set<Label>
    ExecutableEdges: Set<Label * Label>
    Worklist: Label list
    Pending: Set<Label>
}

let private sameConstant left right =
    match left, right with
    | Int64Const leftValue, Int64Const rightValue -> leftValue = rightValue
    | BoolConst leftValue, BoolConst rightValue -> leftValue = rightValue
    | _ -> false

let private mergeLattice current incoming =
    match current, incoming with
    | Overdefined, _
    | _, Overdefined -> Overdefined
    | Unknown, value
    | value, Unknown -> value
    | Constant left, Constant right when sameConstant left right -> current
    | Constant _, Constant _ -> Overdefined

let private isTrackedScalarType valueType =
    match valueType with
    | Some AST.TInt8
    | Some AST.TInt16
    | Some AST.TInt32
    | Some AST.TInt64
    | Some AST.TUInt8
    | Some AST.TUInt16
    | Some AST.TUInt32
    | Some AST.TUInt64
    | Some AST.TBool
    | Some AST.TChar
    | Some AST.TDateTime
    | Some AST.TUnit -> true
    | _ -> false

let private operandValue (values: Map<VReg, LatticeValue>) operand =
    match operand with
    | Int64Const _
    | BoolConst _ -> Constant operand
    | Register register -> Map.tryFind register values |> Option.defaultValue Unknown
    | _ -> Overdefined

let private resolvedOperand values operand =
    match operandValue values operand with
    | Constant constant -> constant
    | Unknown
    | Overdefined -> operand

let private evaluatedOperationValue values operands folded =
    match folded with
    | Some result -> operandValue values result
    | None ->
        let operandValues = List.map (operandValue values) operands
        if List.contains Overdefined operandValues then Overdefined else Unknown

let private instructionValue
    (state: AnalysisState)
    (blockLabel: Label)
    (instr: Instr)
    : (VReg * LatticeValue) option =
    let values = state.Values
    match instr with
    | Mov (destination, source, valueType) ->
        let value =
            if isTrackedScalarType valueType then operandValue values source
            else Overdefined
        Some (destination, value)
    | BinOp (destination, op, left, right, operandType) ->
        if isTrackedScalarType (Some operandType) then
            let resolvedLeft = resolvedOperand values left
            let resolvedRight = resolvedOperand values right
            Some (
                destination,
                evaluatedOperationValue
                    values
                    [left; right]
                    (tryFoldBinOp op resolvedLeft resolvedRight operandType)
            )
        else
            Some (destination, Overdefined)
    | UnaryOp (destination, op, source) ->
        let folded =
            match op, resolvedOperand values source with
            | Neg, Int64Const value -> Some (Int64Const (-value))
            | Not, BoolConst value -> Some (BoolConst (not value))
            | _ -> None
        Some (destination, evaluatedOperationValue values [source] folded)
    | Phi (destination, sources, valueType) ->
        let value =
            if not (isTrackedScalarType valueType) then
                Overdefined
            else
                sources
                |> List.filter (fun (_, predecessor) ->
                    Set.contains (predecessor, blockLabel) state.ExecutableEdges)
                |> List.fold (fun merged (source, _) ->
                    mergeLattice merged (operandValue values source)) Unknown
        Some (destination, value)
    | other ->
        getInstrDest other
        |> Option.map (fun destination -> (destination, Overdefined))

let private buildRegisterUsers (cfg: CFG) : Map<VReg, Set<Label>> =
    let addUser label users register =
        let current = Map.tryFind register users |> Option.defaultValue Set.empty
        Map.add register (Set.add label current) users

    cfg.Blocks
    |> Map.fold (fun users label block ->
        let instructionUsers =
            block.Instrs
            |> List.fold (fun acc instr -> foldInstrUses (addUser label) acc instr) users
        foldTerminatorUses (addUser label) instructionUsers block.Terminator) Map.empty

let private enqueueBlock label state =
    if Set.contains label state.ExecutableBlocks && not (Set.contains label state.Pending) then
        {
            state with
                Worklist = label :: state.Worklist
                Pending = Set.add label state.Pending
        }
    else
        state

let private updateValue users destination incoming state =
    let current = Map.tryFind destination state.Values |> Option.defaultValue Unknown
    let merged = mergeLattice current incoming
    if merged = current then
        state
    else
        let updated = { state with Values = Map.add destination merged state.Values }
        Map.tryFind destination users
        |> Option.defaultValue Set.empty
        |> Set.fold (fun acc label -> enqueueBlock label acc) updated

let private activateEdge cfg source target state =
    let edge = (source, target)
    if Set.contains edge state.ExecutableEdges then
        state
    else
        let withEdge = {
            state with
                ExecutableEdges = Set.add edge state.ExecutableEdges
                ExecutableBlocks = Set.add target state.ExecutableBlocks
        }
        let withTarget =
            if Map.containsKey target cfg.Blocks then enqueueBlock target withEdge
            else Crash.crash $"SCCP edge targets missing block {target}"
        withTarget

let private analyze (cfg: CFG) : AnalysisState =
    let users = buildRegisterUsers cfg
    let definitions =
        cfg.Blocks
        |> Map.fold (fun registers _ block ->
            block.Instrs
            |> List.fold (fun acc instr ->
                match getInstrDest instr with
                | Some destination -> Set.add destination acc
                | None -> acc) registers) Set.empty
    let liveIns =
        users
        |> Map.fold (fun registers register _ ->
            if Set.contains register definitions then registers
            else Set.add register registers) Set.empty
    let initialValues =
        liveIns
        |> Set.fold (fun values register -> Map.add register Overdefined values) Map.empty
    let initial = {
        Values = initialValues
        ExecutableBlocks = Set.singleton cfg.Entry
        ExecutableEdges = Set.empty
        Worklist = [cfg.Entry]
        Pending = Set.singleton cfg.Entry
    }

    let rec analyzeWorklist state =
        match state.Worklist with
        | [] -> state
        | label :: remaining ->
            let withoutCurrent = {
                state with
                    Worklist = remaining
                    Pending = Set.remove label state.Pending
            }
            match Map.tryFind label cfg.Blocks with
            | None -> Crash.crash $"SCCP worklist contains missing block {label}"
            | Some block ->
                let afterInstructions =
                    block.Instrs
                    |> List.fold (fun current instr ->
                        match instructionValue current label instr with
                        | Some (destination, value) -> updateValue users destination value current
                        | None -> current) withoutCurrent

                let afterTerminator =
                    match block.Terminator with
                    | Ret _ -> afterInstructions
                    | Jump target -> activateEdge cfg label target afterInstructions
                    | Branch (condition, trueTarget, falseTarget) ->
                        match operandValue afterInstructions.Values condition with
                        | Constant (BoolConst true) ->
                            activateEdge cfg label trueTarget afterInstructions
                        | Constant (BoolConst false) ->
                            activateEdge cfg label falseTarget afterInstructions
                        | Unknown
                        | Constant _
                        | Overdefined ->
                            afterInstructions
                            |> activateEdge cfg label trueTarget
                            |> activateEdge cfg label falseTarget
                analyzeWorklist afterTerminator

    analyzeWorklist initial

let private hasFloatValues (cfg: CFG) =
    cfg.Blocks
    |> Map.exists (fun _ block ->
        block.Instrs
        |> List.exists (fun instr ->
            match instr with
            | Mov (_, _, Some AST.TFloat64)
            | BinOp (_, _, _, _, AST.TFloat64)
            | Call (_, _, _, _, AST.TFloat64)
            | IndirectCall (_, _, _, _, AST.TFloat64)
            | ClosureCall (_, _, _, _, AST.TFloat64)
            | HeapLoad (_, _, _, Some AST.TFloat64)
            | RawGet (_, _, _, Some AST.TFloat64)
            | Phi (_, _, Some AST.TFloat64)
            | FloatSqrt _
            | FloatAbs _
            | FloatNeg _
            | Int64ToFloat _ -> true
            | _ -> false))

let private applyToSupportedCFG (cfg: CFG) : CFG * bool =
    let analysis = analyze cfg
    let constantFor register =
        match Map.tryFind register analysis.Values with
        | Some (Constant constant) -> Some constant
        | Some Unknown
        | Some Overdefined
        | None -> None

    let rewriteInstruction blockLabel instr =
        match instr with
        | Mov (destination, _, valueType) ->
            match constantFor destination with
            | Some constant -> Mov (destination, constant, valueType)
            | None -> instr
        | BinOp (destination, _, _, _, operandType) ->
            match constantFor destination with
            | Some constant -> Mov (destination, constant, Some operandType)
            | None -> instr
        | Phi (destination, sources, valueType) ->
            match constantFor destination with
            | Some constant -> Mov (destination, constant, valueType)
            | None ->
                let executableSources =
                    sources
                    |> List.filter (fun (_, predecessor) ->
                        Set.contains (predecessor, blockLabel) analysis.ExecutableEdges)
                Phi (destination, executableSources, valueType)
        | _ -> instr

    let rewriteTerminator terminator =
        match terminator with
        | Branch (_, trueTarget, falseTarget) when trueTarget = falseTarget ->
            Jump trueTarget
        | Branch (BoolConst true, trueTarget, _) -> Jump trueTarget
        | Branch (BoolConst false, _, falseTarget) -> Jump falseTarget
        | Branch (Register condition, trueTarget, falseTarget) ->
            match constantFor condition with
            | Some (BoolConst true) -> Jump trueTarget
            | Some (BoolConst false) -> Jump falseTarget
            | _ -> terminator
        | _ -> terminator

    let blocks =
        cfg.Blocks
        |> Map.filter (fun label _ -> Set.contains label analysis.ExecutableBlocks)
        |> Map.map (fun label block ->
            {
                block with
                    Instrs = List.map (rewriteInstruction label) block.Instrs
                    Terminator = rewriteTerminator block.Terminator
            })

    let optimized = { cfg with Blocks = blocks }
    (optimized, optimized <> cfg)

let applySparseConditionalConstantPropagation (cfg: CFG) : CFG * bool =
    if hasFloatValues cfg then
        (cfg, false)
    else
        applyToSupportedCFG cfg
