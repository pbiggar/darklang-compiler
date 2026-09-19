// SparseConditionalConstants.fs - Joint SSA constant and CFG-edge analysis.

module MIRSparseConditionalConstants

open MIR
open MIROptimizationFacts
open MIRConstants

type private LatticeValue =
    | Unknown
    | Constant of Operand
    | IntegerRange of minimum:int64 * maximum:int64
    | AggregateRefs of Set<VReg>
    | Overdefined

type private AnalysisState = {
    Values: Map<VReg, LatticeValue>
    ExecutableBlocks: Set<Label>
    ExecutableEdges: Set<Label * Label>
    HeapValues: Map<VReg, Map<int, LatticeValue>>
    TrackHeapValues: bool
    CallResults: Map<AST.FunctionId, Operand>
    Worklist: Label list
    Pending: Set<Label>
}

let private sameConstant left right =
    match left, right with
    | Int64Const leftValue, Int64Const rightValue -> leftValue = rightValue
    | BoolConst leftValue, BoolConst rightValue -> leftValue = rightValue
    | FloatSymbol leftValue, FloatSymbol rightValue ->
        (System.BitConverter.DoubleToInt64Bits leftValue) = (System.BitConverter.DoubleToInt64Bits rightValue)
    | StringSymbol leftValue, StringSymbol rightValue -> leftValue = rightValue
    | FuncAddr leftValue, FuncAddr rightValue -> leftValue = rightValue
    | _ -> false

let private rangeContaining leftMinimum leftMaximum rightMinimum rightMaximum =
    IntegerRange (min leftMinimum rightMinimum, max leftMaximum rightMaximum)

let private mergeLattice current incoming =
    match current, incoming with
    | Overdefined, _
    | _, Overdefined -> Overdefined
    | Unknown, value
    | value, Unknown -> value
    | Constant left, Constant right when sameConstant left right -> current
    | Constant (Int64Const leftValue), Constant (Int64Const rightValue) ->
        rangeContaining leftValue leftValue rightValue rightValue
    | Constant (Int64Const value), IntegerRange (minimum, maximum)
    | IntegerRange (minimum, maximum), Constant (Int64Const value) ->
        rangeContaining minimum maximum value value
    | IntegerRange (leftMinimum, leftMaximum), IntegerRange (rightMinimum, rightMaximum) ->
        rangeContaining leftMinimum leftMaximum rightMinimum rightMaximum
    | AggregateRefs left, AggregateRefs right ->
        let merged = Set.union left right
        if Set.count merged <= 16 then AggregateRefs merged else Overdefined
    | Constant _, Constant _ -> Overdefined
    | Constant _, IntegerRange _
    | IntegerRange _, Constant _
    | Constant _, AggregateRefs _
    | AggregateRefs _, Constant _
    | IntegerRange _, AggregateRefs _
    | AggregateRefs _, IntegerRange _ -> Overdefined

let private integerRangeForType valueType =
    match valueType with
    | AST.TInt8 -> Some (IntegerRange (int64 System.SByte.MinValue, int64 System.SByte.MaxValue))
    | AST.TInt16 -> Some (IntegerRange (int64 System.Int16.MinValue, int64 System.Int16.MaxValue))
    | AST.TInt32 -> Some (IntegerRange (int64 System.Int32.MinValue, int64 System.Int32.MaxValue))
    | AST.TUInt8 -> Some (IntegerRange (0L, int64 System.Byte.MaxValue))
    | AST.TUInt16 -> Some (IntegerRange (0L, int64 System.UInt16.MaxValue))
    | AST.TUInt32 -> Some (IntegerRange (0L, int64 System.UInt32.MaxValue))
    | _ -> None

let private operandValue (values: Map<VReg, LatticeValue>) operand =
    match operand with
    | Int64Const _
    | BoolConst _
    | FloatSymbol _
    | StringSymbol _
    | FuncAddr _ -> Constant operand
    | Register register -> Map.tryFind register values |> Option.defaultValue Unknown

let private valueForType valueType value =
    let compatible =
        match valueType, value with
        | AST.TFloat64, Constant (FloatSymbol _)
        | AST.TString, Constant (StringSymbol _)
        | AST.TChar, Constant (StringSymbol _)
        | AST.TBool, Constant (BoolConst _)
        | (AST.TInt64 | AST.TRawPtr | AST.TFunction _), Constant (FuncAddr _)
        | (AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
          | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
          | AST.TDateTime | AST.TUnit), Constant (Int64Const _)
        | (AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32), IntegerRange _
        | (AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TList _ | AST.TDict _), AggregateRefs _
        | AST.TSum _, Constant (Int64Const _)
        | (AST.TList _ | AST.TDict _), Constant (Int64Const 0L)
        | _, Unknown -> true
        | _, Overdefined -> true
        | _ -> false
    if compatible then
        match value with
        | Overdefined -> integerRangeForType valueType |> Option.defaultValue Overdefined
        | _ -> value
    else
        Overdefined

let private typedOperandValue values valueType operand =
    operandValue values operand |> valueForType valueType

let private resolvedOperand values operand =
    match operandValue values operand with
    | Constant constant -> constant
    | Unknown
    | IntegerRange _
    | AggregateRefs _
    | Overdefined -> operand

let private evaluatedOperationValue values operands folded =
    match folded with
    | Some result -> operandValue values result
    | None ->
        let operandValues = List.map (operandValue values) operands
        if List.contains Overdefined operandValues then Overdefined else Unknown

let private tryFoldFloatBinOp op left right =
    match op, left, right with
    | Add, FloatSymbol leftValue, FloatSymbol rightValue -> Some (FloatSymbol (leftValue + rightValue))
    | Sub, FloatSymbol leftValue, FloatSymbol rightValue -> Some (FloatSymbol (leftValue - rightValue))
    | Mul, FloatSymbol leftValue, FloatSymbol rightValue -> Some (FloatSymbol (leftValue * rightValue))
    | Div, FloatSymbol leftValue, FloatSymbol rightValue -> Some (FloatSymbol (leftValue / rightValue))
    | Mod, FloatSymbol leftValue, FloatSymbol rightValue -> Some (FloatSymbol (leftValue % rightValue))
    | Eq, FloatSymbol leftValue, FloatSymbol rightValue -> Some (BoolConst (leftValue = rightValue))
    | Neq, FloatSymbol leftValue, FloatSymbol rightValue -> Some (BoolConst (leftValue <> rightValue))
    | Lt, FloatSymbol leftValue, FloatSymbol rightValue -> Some (BoolConst (leftValue < rightValue))
    | Gt, FloatSymbol leftValue, FloatSymbol rightValue -> Some (BoolConst (leftValue > rightValue))
    | Lte, FloatSymbol leftValue, FloatSymbol rightValue -> Some (BoolConst (leftValue <= rightValue))
    | Gte, FloatSymbol leftValue, FloatSymbol rightValue -> Some (BoolConst (leftValue >= rightValue))
    | _ -> None

let private tryRangeComparison op left right =
    let bounds value =
        match value with
        | Constant (Int64Const number) -> Some (number, number)
        | IntegerRange (minimum, maximum) -> Some (minimum, maximum)
        | _ -> None
    match bounds left, bounds right with
    | Some (leftMinimum, leftMaximum), Some (rightMinimum, rightMaximum) ->
        match op with
        | Eq when leftMaximum < rightMinimum || rightMaximum < leftMinimum -> Some (BoolConst false)
        | Eq when leftMinimum = leftMaximum && leftMinimum = rightMinimum && rightMinimum = rightMaximum -> Some (BoolConst true)
        | Neq when leftMaximum < rightMinimum || rightMaximum < leftMinimum -> Some (BoolConst true)
        | Neq when leftMinimum = leftMaximum && leftMinimum = rightMinimum && rightMinimum = rightMaximum -> Some (BoolConst false)
        | Lt when leftMaximum < rightMinimum -> Some (BoolConst true)
        | Lt when leftMinimum >= rightMaximum -> Some (BoolConst false)
        | Gt when leftMinimum > rightMaximum -> Some (BoolConst true)
        | Gt when leftMaximum <= rightMinimum -> Some (BoolConst false)
        | Lte when leftMaximum <= rightMinimum -> Some (BoolConst true)
        | Lte when leftMinimum > rightMaximum -> Some (BoolConst false)
        | Gte when leftMinimum >= rightMaximum -> Some (BoolConst true)
        | Gte when leftMaximum < rightMinimum -> Some (BoolConst false)
        | _ -> None
    | _ -> None

let private operationValue values operands folded =
    match folded with
    | Some result -> operandValue values result
    | None ->
        if List.contains Overdefined operands then Overdefined
        elif List.contains Unknown operands then Unknown
        else Overdefined

let private instructionValue
    (state: AnalysisState)
    (blockLabel: Label)
    (instr: Instr)
    : (VReg * LatticeValue) option =
    let values = state.Values
    match instr with
    | Mov (destination, source, valueType) ->
        let value =
            match valueType with
            | Some typ -> typedOperandValue values typ source
            | None -> operandValue values source
        Some (destination, value)
    | BinOp (destination, op, left, right, operandType) ->
        let leftValue = typedOperandValue values operandType left
        let rightValue = typedOperandValue values operandType right
        let resolvedLeft = resolvedOperand values left
        let resolvedRight = resolvedOperand values right
        let folded =
            match operandType with
            | AST.TFloat64 -> tryFoldFloatBinOp op resolvedLeft resolvedRight
            | _ ->
                match tryFoldBinOp op resolvedLeft resolvedRight operandType with
                | Some result -> Some result
                | None -> tryRangeComparison op leftValue rightValue
        Some (destination, operationValue values [leftValue; rightValue] folded)
    | UnaryOp (destination, op, source) ->
        let folded =
            match op, resolvedOperand values source with
            | Neg, Int64Const value -> Some (Int64Const (-value))
            | Not, BoolConst value -> Some (BoolConst (not value))
            | _ -> None
        Some (destination, evaluatedOperationValue values [source] folded)
    | Phi (destination, sources, valueType) ->
        let value =
            sources
            |> List.filter (fun (_, predecessor) ->
                Set.contains (predecessor, blockLabel) state.ExecutableEdges)
            |> List.fold (fun merged (source, _) ->
                let incoming =
                    match valueType with
                    | Some typ -> typedOperandValue values typ source
                    | None -> operandValue values source
                mergeLattice merged incoming) Unknown
        Some (destination, value)
    | HeapAlloc (destination, _) when state.TrackHeapValues ->
        Some (destination, AggregateRefs (Set.singleton destination))
    | HeapAlloc (destination, _) -> Some (destination, Overdefined)
    | HeapLoad (destination, address, offset, valueType) ->
        let value =
            match Map.tryFind address values |> Option.defaultValue Unknown with
            | AggregateRefs allocations ->
                allocations
                |> Set.fold (fun merged allocation ->
                    let incoming =
                        state.HeapValues
                        |> Map.tryFind allocation
                        |> Option.bind (Map.tryFind offset)
                        |> Option.defaultValue Unknown
                    mergeLattice merged incoming) Unknown
            | Unknown -> Unknown
            | Constant _
            | IntegerRange _
            | Overdefined -> Overdefined
        let typedValue =
            match valueType with
            | Some typ -> valueForType typ value
            | None -> value
        Some (destination, typedValue)
    | StringConcat (destination, first, second, remaining) ->
        let values = first :: second :: remaining |> List.map (operandValue values)
        let strings =
            values
            |> List.choose (function Constant (StringSymbol value) -> Some value | _ -> None)
        let value =
            if List.length strings = List.length values then
                Constant (StringSymbol (String.concat "" strings))
            elif List.contains Overdefined values then Overdefined
            else Unknown
        Some (destination, value)
    | CanonicalBufferEq (destination, kind, left, right) ->
        let leftValue = operandValue values left
        let rightValue = operandValue values right
        let value =
            match kind, leftValue, rightValue with
            | _, Constant (StringSymbol leftString), Constant (StringSymbol rightString) ->
                Constant (BoolConst (leftString = rightString))
            | _ when leftValue = Overdefined || rightValue = Overdefined -> Overdefined
            | _ -> Unknown
        Some (destination, value)
    | FloatSqrt (destination, source) ->
        let folded =
            match resolvedOperand values source with
            | FloatSymbol value -> Some (FloatSymbol (sqrt value))
            | _ -> None
        Some (destination, evaluatedOperationValue values [source] folded)
    | FloatAbs (destination, source) ->
        let folded =
            match resolvedOperand values source with
            | FloatSymbol value -> Some (FloatSymbol (abs value))
            | _ -> None
        Some (destination, evaluatedOperationValue values [source] folded)
    | FloatNeg (destination, source) ->
        let folded =
            match resolvedOperand values source with
            | FloatSymbol value -> Some (FloatSymbol (-value))
            | _ -> None
        Some (destination, evaluatedOperationValue values [source] folded)
    | Int64ToFloat (destination, source) ->
        let folded =
            match resolvedOperand values source with
            | Int64Const value -> Some (FloatSymbol (float value))
            | _ -> None
        Some (destination, evaluatedOperationValue values [source] folded)
    | FloatToInt64 (destination, source) ->
        let folded =
            match resolvedOperand values source with
            | FloatSymbol value ->
                let int64Minimum = -9223372036854775808.0
                let int64MaximumExclusive = 9223372036854775808.0
                if System.Double.IsFinite value && value >= int64Minimum && value < int64MaximumExclusive then
                    Some (Int64Const (int64 (System.Math.Truncate value)))
                else
                    None
            | _ -> None
        Some (destination, evaluatedOperationValue values [source] folded)
    | FloatToBits (destination, source) ->
        let folded =
            match resolvedOperand values source with
            | FloatSymbol value -> Some (Int64Const (System.BitConverter.DoubleToInt64Bits value))
            | _ -> None
        Some (destination, evaluatedOperationValue values [source] folded)
    | Call (destination, functionName, _, _, returnType) ->
        let value =
            match Map.tryFind functionName state.CallResults with
            | Some result -> typedOperandValue values returnType result
            | None -> integerRangeForType returnType |> Option.defaultValue Overdefined
        Some (destination, value)
    | IndirectCall (destination, _, _, _, returnType)
    | ClosureCall (destination, _, _, _, returnType) ->
        Some (destination, integerRangeForType returnType |> Option.defaultValue Overdefined)
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

let private recordHeapStore users address offset source state =
    if not state.TrackHeapValues then
        state
    else
        match Map.tryFind address state.Values |> Option.defaultValue Unknown with
        | AggregateRefs allocations ->
            let incoming = operandValue state.Values source
            let updatedHeap, changed =
                allocations
                |> Set.fold (fun (heap, anyChanged) allocation ->
                    let fields = Map.tryFind allocation heap |> Option.defaultValue Map.empty
                    let current = Map.tryFind offset fields |> Option.defaultValue Unknown
                    let merged = mergeLattice current incoming
                    if merged = current then
                        (heap, anyChanged)
                    else
                        (Map.add allocation (Map.add offset merged fields) heap, true)) (state.HeapValues, false)
            if changed then
                let withHeap = { state with HeapValues = updatedHeap }
                Map.tryFind address users
                |> Option.defaultValue Set.empty
                |> Set.fold (fun queued label -> enqueueBlock label queued) withHeap
            else
                state
        | Unknown
        | Constant _
        | IntegerRange _
        | Overdefined -> state

let private analyze (callResults: Map<AST.FunctionId, Operand>) (cfg: CFG) : AnalysisState =
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
    let trackHeapValues =
        let hasAggregatePhi =
            cfg.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | Phi (_, _, Some (AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TList _ | AST.TDict _)) -> true
                    | _ -> false))
        let hasHeapLoad =
            cfg.Blocks
            |> Map.exists (fun _ block -> block.Instrs |> List.exists (function HeapLoad _ -> true | _ -> false))
        hasAggregatePhi && hasHeapLoad
    let initial = {
        Values = initialValues
        ExecutableBlocks = Set.singleton cfg.Entry
        ExecutableEdges = Set.empty
        HeapValues = Map.empty
        TrackHeapValues = trackHeapValues
        CallResults = callResults
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
                        let afterHeapEffects =
                            match instr with
                            | HeapStore (address, offset, source, _) ->
                                recordHeapStore users address offset source current
                            | _ -> current
                        match instructionValue afterHeapEffects label instr with
                        | Some (destination, value) -> updateValue users destination value afterHeapEffects
                        | None -> afterHeapEffects) withoutCurrent

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
                        | IntegerRange _
                        | AggregateRefs _
                        | Overdefined ->
                            afterInstructions
                            |> activateEdge cfg label trueTarget
                            |> activateEdge cfg label falseTarget
                analyzeWorklist afterTerminator

    analyzeWorklist initial

let private applyToCFG (callResults: Map<AST.FunctionId, Operand>) (cfg: CFG) : CFG * bool =
    let analysis = analyze callResults cfg
    let constantFor register =
        match Map.tryFind register analysis.Values with
        | Some (Constant constant) -> Some constant
        | Some Unknown
        | Some (IntegerRange _)
        | Some (AggregateRefs _)
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
            | Some constant ->
                let resultType =
                    match instr with
                    | BinOp (_, (Eq | Neq | Lt | Gt | Lte | Gte | And | Or), _, _, _) -> AST.TBool
                    | _ -> operandType
                Mov (destination, constant, Some resultType)
            | None -> instr
        | UnaryOp (destination, _, _) ->
            match constantFor destination with
            | Some constant -> Mov (destination, constant, None)
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
        | CanonicalBufferEq (destination, _, _, _) ->
            match constantFor destination with
            | Some (BoolConst _ as constant) -> Mov (destination, constant, Some AST.TBool)
            | _ -> instr
        | FloatSqrt (destination, _)
        | FloatAbs (destination, _)
        | FloatNeg (destination, _)
        | Int64ToFloat (destination, _) ->
            match constantFor destination with
            | Some (FloatSymbol _ as constant) -> Mov (destination, constant, Some AST.TFloat64)
            | _ -> instr
        | FloatToBits (destination, _) ->
            match constantFor destination with
            | Some (Int64Const _ as constant) -> Mov (destination, constant, Some AST.TUInt64)
            | _ -> instr
        | FloatToInt64 (destination, _) ->
            match constantFor destination with
            | Some (Int64Const _ as constant) -> Mov (destination, constant, Some AST.TInt64)
            | _ -> instr
        | HeapLoad (destination, _, _, valueType) ->
            match constantFor destination, valueType with
            | Some (FloatSymbol _ as constant), Some AST.TFloat64 ->
                Mov (destination, constant, valueType)
            | Some ((Int64Const _ | BoolConst _) as constant), _ ->
                Mov (destination, constant, valueType)
            | _ -> instr
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

let applySparseConditionalConstantPropagationWithCallResults
    (callResults: Map<AST.FunctionId, Operand>)
    (cfg: CFG)
    : CFG * bool =
    let hasConditionalBranch =
        cfg.Blocks
        |> Map.exists (fun _ block ->
            match block.Terminator with
            | Branch _ -> true
            | Ret _
            | Jump _ -> false)
    if hasConditionalBranch then applyToCFG callResults cfg else (cfg, false)

let applySparseConditionalConstantPropagation (cfg: CFG) : CFG * bool =
    applySparseConditionalConstantPropagationWithCallResults Map.empty cfg
