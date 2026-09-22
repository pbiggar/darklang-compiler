// Induction.fs - Reduce affine induction expressions in verified loop shapes.

module MIRInduction

open MIR
open MIROptimizationFacts
open MIRLoopTopology

type private AffineInductionCandidate = {
    Header: Label
    Preheader: Label
    Latch: Label
    InitialValue: Operand
    AffineValue: VReg
    Coefficient: Operand
    PreheaderCoefficient: Operand
    Offset: Operand
    PreheaderOffset: Operand
    OffsetOperator: BinOp
    ValueType: AST.SemanticType
    ScaleInstr: Instr
    AffineInstr: Instr
}

let internal nextRegisterId (cfg: CFG) : int =
    cfg.Blocks
    |> Map.fold (fun registers _ block ->
        let instrRegisters =
            block.Instrs
            |> List.fold (fun acc instr ->
                let acc' = Set.union acc (getInstrUses instr)
                match getInstrDest instr with
                | Some dest -> Set.add dest acc'
                | None -> acc'
            ) Set.empty
        Set.unionMany [registers; instrRegisters; getTerminatorUses block.Terminator]
    ) Set.empty
    |> Set.fold (fun highest (VReg id) -> max highest id) -1
    |> fun highest -> highest + 1

let internal resolveLatchCopy (instrs: Instr list) (register: VReg) : VReg =
    let rec resolve visited current =
        if Set.contains current visited then
            current
        else
            let source =
                instrs
                |> List.tryPick (function
                    | Mov (dest, Register source, _) when dest = current -> Some source
                    | _ -> None)
            match source with
            | Some source -> resolve (Set.add current visited) source
            | None -> current
    resolve Set.empty register

let private isNativeWrappingIntegerType (valueType: AST.SemanticType) : bool =
    match valueType with
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> true
    | _ -> false

let internal isIncrementByOne
    (valueType: AST.SemanticType)
    (inductionPhi: VReg)
    (nextValue: VReg)
    (instr: Instr)
    : bool =
    match instr with
    | BinOp (dest, Add, Register source, Int64Const 1L, instructionType)
    | BinOp (dest, Add, Int64Const 1L, Register source, instructionType)
        when instructionType = valueType ->
        dest = nextValue && source = inductionPhi
    | _ -> false

let private registerInstrUsers (cfg: CFG) (value: VReg) : (Label * Instr) list =
    cfg.Blocks
    |> Map.toList
    |> List.collect (fun (label, block) ->
        block.Instrs
        |> List.choose (fun instr ->
            if Set.contains value (getInstrUses instr) then Some (label, instr) else None))

let private terminatorUsesRegister (cfg: CFG) (value: VReg) : bool =
    cfg.Blocks
    |> Map.exists (fun _ block -> Set.contains value (getTerminatorUses block.Terminator))

let private isAffineOperandLoopInvariant
    (preheader: Label)
    (latch: Label)
    (loopBlocks: Set<Label>)
    (cfg: CFG)
    (valueType: AST.SemanticType)
    (operand: Operand)
    : bool =
    match operand with
    | Register register ->
        let definitions =
            loopBlocks
            |> Set.toList
            |> List.collect (fun label ->
                Map.tryFind label cfg.Blocks
                |> Option.map (fun block -> block.Instrs)
                |> Option.defaultValue [])
            |> List.filter (fun instr -> getInstrDest instr = Some register)

        match definitions with
        | [] -> true
        | [Phi (destination, sources, Some phiType)]
            when destination = register && phiType = valueType ->
            let preheaderSources = sources |> List.filter (fun (_, label) -> label = preheader)
            let latchSources = sources |> List.filter (fun (_, label) -> label = latch)
            match sources, preheaderSources, latchSources, Map.tryFind latch cfg.Blocks with
            | [_; _], [_], [(Register backedge, _)], Some latchBlock ->
                resolveLatchCopy latchBlock.Instrs backedge = register
            | _ -> false
        | _ -> false
    | Int64Const _ -> true
    | _ -> false

let private affineOperandAtPreheader
    (headerInstrs: Instr list)
    (preheader: Label)
    (valueType: AST.SemanticType)
    (operand: Operand)
    : Operand =
    match operand with
    | Register register ->
        headerInstrs
        |> List.tryPick (function
            | Phi (destination, sources, Some phiType)
                when destination = register && phiType = valueType ->
                sources
                |> List.tryPick (fun (source, label) -> if label = preheader then Some source else None)
            | _ -> None)
        |> Option.defaultValue operand
    | _ -> operand

let private tryAffineExpression
    (cfg: CFG)
    (latchLabel: Label)
    (latch: BasicBlock)
    (inductionPhi: VReg)
    (valueType: AST.SemanticType)
    (isLoopInvariant: Operand -> bool)
    : (VReg * Operand * Operand * BinOp * Instr * Instr) option =
    let candidates =
        latch.Instrs
        |> List.collect (fun scaleInstr ->
            match scaleInstr with
            | BinOp (scaledValue, Shl, Register source, Int64Const 1L, instructionType)
                when source = inductionPhi && instructionType = valueType ->
                latch.Instrs
                |> List.choose (fun affineInstr ->
                    match affineInstr with
                    | BinOp (affineValue, (Add | Sub as offsetOperator), Register scaledSource, offset, affineType)
                        when scaledSource = scaledValue && affineType = valueType && isLoopInvariant offset ->
                        Some (scaledValue, affineValue, Int64Const 2L, offset, offsetOperator, scaleInstr, affineInstr)
                    | BinOp (affineValue, Add, offset, Register scaledSource, affineType)
                        when scaledSource = scaledValue && affineType = valueType && isLoopInvariant offset ->
                        Some (scaledValue, affineValue, Int64Const 2L, offset, Add, scaleInstr, affineInstr)
                    | _ -> None)
            | BinOp (scaledValue, Mul, Register source, coefficient, instructionType)
                when source = inductionPhi && instructionType = valueType && isLoopInvariant coefficient ->
                latch.Instrs
                |> List.choose (fun affineInstr ->
                    match affineInstr with
                    | BinOp (affineValue, (Add | Sub as offsetOperator), Register scaledSource, offset, affineType)
                        when scaledSource = scaledValue && affineType = valueType && isLoopInvariant offset ->
                        Some (scaledValue, affineValue, coefficient, offset, offsetOperator, scaleInstr, affineInstr)
                    | BinOp (affineValue, Add, offset, Register scaledSource, affineType)
                        when scaledSource = scaledValue && affineType = valueType && isLoopInvariant offset ->
                        Some (scaledValue, affineValue, coefficient, offset, Add, scaleInstr, affineInstr)
                    | _ -> None)
            | BinOp (scaledValue, Mul, coefficient, Register source, instructionType)
                when source = inductionPhi && instructionType = valueType && isLoopInvariant coefficient ->
                latch.Instrs
                |> List.choose (fun affineInstr ->
                    match affineInstr with
                    | BinOp (affineValue, (Add | Sub as offsetOperator), Register scaledSource, offset, affineType)
                        when scaledSource = scaledValue && affineType = valueType && isLoopInvariant offset ->
                        Some (scaledValue, affineValue, coefficient, offset, offsetOperator, scaleInstr, affineInstr)
                    | BinOp (affineValue, Add, offset, Register scaledSource, affineType)
                        when scaledSource = scaledValue && affineType = valueType && isLoopInvariant offset ->
                        Some (scaledValue, affineValue, coefficient, offset, Add, scaleInstr, affineInstr)
                    | _ -> None)
            | _ -> [])

    match candidates with
    | [(scaledValue, affineValue, coefficient, offset, offsetOperator, scaleInstr, affineInstr)] ->
        let scaledUsers = registerInstrUsers cfg scaledValue
        let affineUsers = registerInstrUsers cfg affineValue
        let affineUsesOnlyInLatch =
            not (List.isEmpty affineUsers)
            && affineUsers |> List.forall (fun (label, _) -> label = latchLabel)

        if scaledUsers = [(latchLabel, affineInstr)]
           && affineUsesOnlyInLatch
           && not (terminatorUsesRegister cfg scaledValue)
           && not (terminatorUsesRegister cfg affineValue) then
            Some (affineValue, coefficient, offset, offsetOperator, scaleInstr, affineInstr)
        else
            None
    | _ -> None

let private tryAffineInductionCandidate
    (cfg: CFG)
    (predecessors: Map<Label, Label list>)
    (header: Label)
    (loopBlocks: Set<Label>)
    : AffineInductionCandidate option =
    let headerPredecessors = Map.tryFind header predecessors |> Option.defaultValue []
    let outsidePredecessors =
        headerPredecessors |> List.filter (fun label -> not (Set.contains label loopBlocks))
    let insidePredecessors =
        headerPredecessors |> List.filter (fun label -> Set.contains label loopBlocks)

    match outsidePredecessors, insidePredecessors, Map.tryFind header cfg.Blocks with
    | [preheader], [latchLabel], Some headerBlock
        when loopBlocks = Set.ofList [header; latchLabel] ->
        match Map.tryFind preheader cfg.Blocks, Map.tryFind latchLabel cfg.Blocks with
        | Some preheaderBlock, Some latch
            when preheaderBlock.Terminator = Jump header
                 && latch.Terminator = Jump header ->
            let candidates =
                headerBlock.Instrs
                |> List.choose (fun instr ->
                    match instr with
                    | Phi (inductionPhi, sources, Some valueType)
                        when isNativeWrappingIntegerType valueType ->
                        let initialSources =
                            sources |> List.filter (fun (_, source) -> source = preheader)
                        let backedgeSources =
                            sources |> List.filter (fun (_, source) -> source = latchLabel)
                        match sources, initialSources, backedgeSources with
                        | [ _; _ ], [(initialValue, _)], [(Register nextValue, _)] ->
                            let resolvedNext = resolveLatchCopy latch.Instrs nextValue
                            let advancesByOne =
                                latch.Instrs |> List.exists (isIncrementByOne valueType inductionPhi resolvedNext)
                            let isLoopInvariant =
                                isAffineOperandLoopInvariant preheader latchLabel loopBlocks cfg valueType
                            match advancesByOne, tryAffineExpression cfg latchLabel latch inductionPhi valueType isLoopInvariant with
                            | true, Some (affineValue, coefficient, offset, offsetOperator, scaleInstr, affineInstr) ->
                                Some {
                                    Header = header
                                    Preheader = preheader
                                    Latch = latchLabel
                                    InitialValue = initialValue
                                    AffineValue = affineValue
                                    Coefficient = coefficient
                                    PreheaderCoefficient = affineOperandAtPreheader headerBlock.Instrs preheader valueType coefficient
                                    Offset = offset
                                    PreheaderOffset = affineOperandAtPreheader headerBlock.Instrs preheader valueType offset
                                    OffsetOperator = offsetOperator
                                    ValueType = valueType
                                    ScaleInstr = scaleInstr
                                    AffineInstr = affineInstr
                                }
                            | _ -> None
                        | _ -> None
                    | _ -> None)

            match candidates with
            | [candidate] -> Some candidate
            | _ -> None
        | _ -> None
    | _ -> None

let private addPhiAfterPhis (phi: Instr) (instrs: Instr list) : Instr list =
    let rec insert remaining =
        match remaining with
        | (Phi _ as existingPhi) :: rest -> existingPhi :: insert rest
        | rest -> phi :: rest
    insert instrs

let private insertAfterLastUse (value: VReg) (inserted: Instr list) (instrs: Instr list) : Instr list =
    let rec insert remaining =
        match remaining with
        | [] -> ([], false)
        | instr :: rest ->
            let (rest', alreadyInserted) = insert rest
            if alreadyInserted then
                (instr :: rest', true)
            elif Set.contains value (getInstrUses instr) then
                (instr :: inserted @ rest', true)
            else
                (instr :: rest', false)

    let (instrs', insertedAfterUse) = insert instrs
    if insertedAfterUse then
        instrs'
    else
        Crash.crash "insertAfterLastUse: affine induction value has no latch use"

(*
Recognize a two-block native-width integer loop with an `i + 1` backedge and a unique
`a * i + b` use chain. `a` and `b` must be loop-invariant, so the preheader can
compute the initial value and the latch can advance the derived phi by `a` with
the same wrapping arithmetic. Reject extra scaled-value uses and non-canonical
control flow so the rewrite remains a local SSA substitution.
*)
let internal applyAffineInductionStrengthReductionWithTopology
    (topology: LoopTopology)
    (cfg: CFG)
    : CFG * bool =
    let candidate =
        topology.Loops
        |> Map.toList
        |> List.tryPick (fun (header, loopBlocks) ->
            tryAffineInductionCandidate
                cfg
                topology.Predecessors
                header
                loopBlocks)

    match candidate with
    | None -> (cfg, false)
    | Some candidate ->
        let firstFreshRegister = nextRegisterId cfg
        let (initialScaleOperator, initialScaleOperand) =
            match candidate.ScaleInstr with
            | BinOp (_, Shl, _, _, instructionType) when instructionType = candidate.ValueType ->
                (Shl, Int64Const 1L)
            | _ -> (Mul, candidate.PreheaderCoefficient)
        let initialScaled = VReg firstFreshRegister
        let initialAffine = VReg (firstFreshRegister + 1)
        let nextAffine = VReg (firstFreshRegister + 2)
        let nextAffinePhiSource = VReg (firstFreshRegister + 3)
        let preheaderInstrs = [
            BinOp (
                initialScaled,
                initialScaleOperator,
                candidate.InitialValue,
                initialScaleOperand,
                candidate.ValueType
            )
            BinOp (
                initialAffine,
                candidate.OffsetOperator,
                Register initialScaled,
                candidate.PreheaderOffset,
                candidate.ValueType
            )
        ]
        let derivedPhi =
            Phi (
                candidate.AffineValue,
                [
                    (Register initialAffine, candidate.Preheader)
                    (Register nextAffinePhiSource, candidate.Latch)
                ],
                Some candidate.ValueType
            )
        let advanceDerived =
            BinOp (
                nextAffine,
                Add,
                Register candidate.AffineValue,
                candidate.Coefficient,
                candidate.ValueType
            )
        let copyDerivedToPhiSource =
            Mov (nextAffinePhiSource, Register nextAffine, Some candidate.ValueType)

        let blocks =
            cfg.Blocks
            |> Map.map (fun label block ->
                if label = candidate.Preheader then
                    { block with Instrs = block.Instrs @ preheaderInstrs }
                elif label = candidate.Header then
                    { block with Instrs = addPhiAfterPhis derivedPhi block.Instrs }
                elif label = candidate.Latch then
                    let instrs =
                        block.Instrs
                        |> List.filter (fun instr ->
                            instr <> candidate.ScaleInstr && instr <> candidate.AffineInstr)
                        |> insertAfterLastUse
                            candidate.AffineValue
                            [advanceDerived; copyDerivedToPhiSource]
                    { block with Instrs = instrs }
                else
                    block)
        ({ cfg with Blocks = blocks }, true)

let applyAffineInductionStrengthReduction (cfg: CFG) : CFG * bool =
    match tryBuildLoopTopology cfg with
    | None -> (cfg, false)
    | Some topology ->
        applyAffineInductionStrengthReductionWithTopology topology cfg
