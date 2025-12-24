// 3_ANF_to_MIR.fs - MIR Transformation (Pass 3)
//
// Transforms ANF into MIR with Control Flow Graph (CFG).
//
// Algorithm:
// - Converts ANF expressions into MIR CFG with basic blocks
// - Maps ANF temporary variables to MIR virtual registers
// - Converts ANF If expressions into conditional branches with basic blocks
// - Each basic block has a label, instructions, and a terminator
//
// Example (with if):
//   if x then 10 else 20
//   →
//   entry:
//     branch x, then_block, else_block
//   then_block:
//     v0 <- 10
//     jump join_block
//   else_block:
//     v1 <- 20
//     jump join_block
//   join_block:
//     v2 <- phi(v0, v1)  // (simplified - actual implementation uses registers)
//     ret v2

module ANF_to_MIR

/// Convert ANF.BinOp to MIR.BinOp
let convertBinOp (op: ANF.BinOp) : MIR.BinOp =
    match op with
    | ANF.Add -> MIR.Add
    | ANF.Sub -> MIR.Sub
    | ANF.Mul -> MIR.Mul
    | ANF.Div -> MIR.Div
    | ANF.Eq -> MIR.Eq
    | ANF.Neq -> MIR.Neq
    | ANF.Lt -> MIR.Lt
    | ANF.Gt -> MIR.Gt
    | ANF.Lte -> MIR.Lte
    | ANF.Gte -> MIR.Gte
    | ANF.And -> MIR.And
    | ANF.Or -> MIR.Or

/// Convert ANF.UnaryOp to MIR.UnaryOp
let convertUnaryOp (op: ANF.UnaryOp) : MIR.UnaryOp =
    match op with
    | ANF.Neg -> MIR.Neg
    | ANF.Not -> MIR.Not

/// Map ANF TempId to MIR virtual register
let tempToVReg (ANF.TempId id) : MIR.VReg = MIR.VReg id

/// Convert ANF Atom to MIR Operand
let atomToOperand (atom: ANF.Atom) : MIR.Operand =
    match atom with
    | ANF.IntLiteral n -> MIR.IntConst n
    | ANF.BoolLiteral b -> MIR.BoolConst b
    | ANF.Var tempId -> MIR.Register (tempToVReg tempId)

/// CFG builder state
type CFGBuilder = {
    Blocks: Map<MIR.Label, MIR.BasicBlock>
    LabelGen: MIR.LabelGen
    RegGen: MIR.RegGen
}

/// Convert ANF expression to CFG
/// Returns: (final value operand, CFG builder with all blocks)
let rec convertExpr
    (expr: ANF.AExpr)
    (currentLabel: MIR.Label)
    (currentInstrs: MIR.Instr list)
    (builder: CFGBuilder)
    : MIR.Operand * CFGBuilder =

    match expr with
    | ANF.Return atom ->
        // Return: end current block with Ret terminator
        let operand = atomToOperand atom
        let block = {
            MIR.Label = currentLabel
            MIR.Instrs = currentInstrs
            MIR.Terminator = MIR.Ret operand
        }
        let builder' = { builder with Blocks = Map.add currentLabel block builder.Blocks }
        (operand, builder')

    | ANF.Let (tempId, cexpr, rest) ->
        // Let binding: handle based on cexpr type
        let destReg = tempToVReg tempId

        match cexpr with
        | ANF.IfValue (condAtom, thenAtom, elseAtom) ->
            // IfValue requires control flow blocks
            // 1. End current block with branch on condition
            // 2. Create then-block (assigns thenAtom to destReg, jumps to join)
            // 3. Create else-block (assigns elseAtom to destReg, jumps to join)
            // 4. Create join-block (continues with rest)

            let condOp = atomToOperand condAtom
            let (thenLabel, labelGen1) = MIR.freshLabel builder.LabelGen
            let (elseLabel, labelGen2) = MIR.freshLabel labelGen1
            let (joinLabel, labelGen3) = MIR.freshLabel labelGen2

            // Current block ends with branch
            let currentBlock = {
                MIR.Label = currentLabel
                MIR.Instrs = currentInstrs
                MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
            }

            // Then block: assign thenAtom to destReg, jump to join
            let thenBlock = {
                MIR.Label = thenLabel
                MIR.Instrs = [MIR.Mov (destReg, atomToOperand thenAtom)]
                MIR.Terminator = MIR.Jump joinLabel
            }

            // Else block: assign elseAtom to destReg, jump to join
            let elseBlock = {
                MIR.Label = elseLabel
                MIR.Instrs = [MIR.Mov (destReg, atomToOperand elseAtom)]
                MIR.Terminator = MIR.Jump joinLabel
            }

            let builder' = {
                builder with
                    Blocks = builder.Blocks
                             |> Map.add currentLabel currentBlock
                             |> Map.add thenLabel thenBlock
                             |> Map.add elseLabel elseBlock
                    LabelGen = labelGen3
            }

            // Continue with rest in join block (no instructions yet)
            convertExpr rest joinLabel [] builder'

        | _ ->
            // Simple CExpr: add instruction to current block, continue
            let instr =
                match cexpr with
                | ANF.Atom atom ->
                    MIR.Mov (destReg, atomToOperand atom)
                | ANF.Prim (op, leftAtom, rightAtom) ->
                    MIR.BinOp (destReg, convertBinOp op, atomToOperand leftAtom, atomToOperand rightAtom)
                | ANF.UnaryPrim (op, atom) ->
                    MIR.UnaryOp (destReg, convertUnaryOp op, atomToOperand atom)
                | ANF.Call (funcName, args) ->
                    let argOperands = List.map atomToOperand args
                    MIR.Call (destReg, funcName, argOperands)
                | ANF.IfValue _ ->
                    // Already handled above
                    failwith "Unreachable: IfValue already handled"

            let newInstrs = currentInstrs @ [instr]
            convertExpr rest currentLabel newInstrs builder

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // If expression:
        // 1. End current block with Branch terminator
        // 2. Create then-block and else-block
        // 3. Create join-block where both branches meet
        // 4. Both branches put result in same register and jump to join

        let condOp = atomToOperand condAtom

        // Generate labels for then, else, and join blocks
        let (thenLabel, labelGen1) = MIR.freshLabel builder.LabelGen
        let (elseLabel, labelGen2) = MIR.freshLabel labelGen1
        let (joinLabel, labelGen3) = MIR.freshLabel labelGen2

        // Create a register to hold the result from both branches
        let (resultReg, regGen1) = MIR.freshReg builder.RegGen

        // End current block with conditional branch
        let currentBlock = {
            MIR.Label = currentLabel
            MIR.Instrs = currentInstrs
            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
        }

        let builder1 = {
            Blocks = Map.add currentLabel currentBlock builder.Blocks
            LabelGen = labelGen3
            RegGen = regGen1
        }

        // Convert then-branch: result goes into resultReg, then jump to join
        let (thenResult, thenJoinOpt, builder2) = convertExprToOperand thenBranch thenLabel [] builder1

        // If then-branch created blocks (nested if), patch its join block
        // Otherwise, create a simple block that moves result and jumps
        let builder3 =
            match thenJoinOpt with
            | Some nestedJoinLabel ->
                // Patch the nested join block to jump to our join instead of returning
                match Map.tryFind nestedJoinLabel builder2.Blocks with
                | Some nestedJoinBlock ->
                    let patchedBlock = {
                        nestedJoinBlock with
                            Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, thenResult)]
                            Terminator = MIR.Jump joinLabel
                    }
                    { builder2 with Blocks = Map.add nestedJoinLabel patchedBlock builder2.Blocks }
                | None -> builder2  // Should not happen
            | None ->
                // Simple expression - create block that moves result and jumps
                let thenBlock = {
                    MIR.Label = thenLabel
                    MIR.Instrs = [MIR.Mov (resultReg, thenResult)]
                    MIR.Terminator = MIR.Jump joinLabel
                }
                { builder2 with Blocks = Map.add thenLabel thenBlock builder2.Blocks }

        // Convert else-branch: result goes into resultReg, then jump to join
        let (elseResult, elseJoinOpt, builder4) = convertExprToOperand elseBranch elseLabel [] builder3

        // Same logic for else-branch
        let builder5 =
            match elseJoinOpt with
            | Some nestedJoinLabel ->
                match Map.tryFind nestedJoinLabel builder4.Blocks with
                | Some nestedJoinBlock ->
                    let patchedBlock = {
                        nestedJoinBlock with
                            Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, elseResult)]
                            Terminator = MIR.Jump joinLabel
                    }
                    { builder4 with Blocks = Map.add nestedJoinLabel patchedBlock builder4.Blocks }
                | None -> builder4  // Should not happen
            | None ->
                let elseBlock = {
                    MIR.Label = elseLabel
                    MIR.Instrs = [MIR.Mov (resultReg, elseResult)]
                    MIR.Terminator = MIR.Jump joinLabel
                }
                { builder4 with Blocks = Map.add elseLabel elseBlock builder4.Blocks }

        // Create join block that returns the result
        let joinBlock = {
            MIR.Label = joinLabel
            MIR.Instrs = []
            MIR.Terminator = MIR.Ret (MIR.Register resultReg)
        }
        let builder6 = { builder5 with Blocks = Map.add joinLabel joinBlock builder5.Blocks }

        // Return the result operand
        let resultOp = MIR.Register resultReg
        (resultOp, builder6)

/// Helper: convert expression and extract final operand
/// Returns: (operand, optional join label if blocks were created, builder)
/// - If join label is Some(label), the expression created blocks ending at that join block
/// - If join label is None, no blocks were created (simple expression)
and convertExprToOperand
    (expr: ANF.AExpr)
    (startLabel: MIR.Label)
    (startInstrs: MIR.Instr list)
    (builder: CFGBuilder)
    : MIR.Operand * MIR.Label option * CFGBuilder =

    match expr with
    | ANF.Return atom ->
        // If we have accumulated instructions from Let bindings, create a block
        // Otherwise just return the operand
        let operand = atomToOperand atom
        if List.isEmpty startInstrs then
            (operand, None, builder)
        else
            // Create a block with accumulated instructions
            // Use temporary Ret terminator - caller will patch if needed
            let block = {
                MIR.Label = startLabel
                MIR.Instrs = startInstrs
                MIR.Terminator = MIR.Ret operand
            }
            let builder' = { builder with Blocks = Map.add startLabel block builder.Blocks }
            (operand, Some startLabel, builder')

    | ANF.Let (tempId, cexpr, rest) ->
        let destReg = tempToVReg tempId

        match cexpr with
        | ANF.IfValue (condAtom, thenAtom, elseAtom) ->
            // IfValue requires control flow - similar to convertExpr version
            let condOp = atomToOperand condAtom
            let (thenLabel, labelGen1) = MIR.freshLabel builder.LabelGen
            let (elseLabel, labelGen2) = MIR.freshLabel labelGen1
            let (joinLabel, labelGen3) = MIR.freshLabel labelGen2

            // Current block ends with branch
            let startBlock = {
                MIR.Label = startLabel
                MIR.Instrs = startInstrs
                MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
            }

            // Then block: assign thenAtom to destReg, jump to join
            let thenBlock = {
                MIR.Label = thenLabel
                MIR.Instrs = [MIR.Mov (destReg, atomToOperand thenAtom)]
                MIR.Terminator = MIR.Jump joinLabel
            }

            // Else block: assign elseAtom to destReg, jump to join
            let elseBlock = {
                MIR.Label = elseLabel
                MIR.Instrs = [MIR.Mov (destReg, atomToOperand elseAtom)]
                MIR.Terminator = MIR.Jump joinLabel
            }

            let builder' = {
                builder with
                    Blocks = builder.Blocks
                             |> Map.add startLabel startBlock
                             |> Map.add thenLabel thenBlock
                             |> Map.add elseLabel elseBlock
                    LabelGen = labelGen3
            }

            // Continue with rest in join block (no instructions yet)
            convertExprToOperand rest joinLabel [] builder'

        | _ ->
            // Simple CExpr: create instruction and accumulate
            let instr =
                match cexpr with
                | ANF.Atom atom -> MIR.Mov (destReg, atomToOperand atom)
                | ANF.Prim (op, leftAtom, rightAtom) ->
                    MIR.BinOp (destReg, convertBinOp op, atomToOperand leftAtom, atomToOperand rightAtom)
                | ANF.UnaryPrim (op, atom) ->
                    MIR.UnaryOp (destReg, convertUnaryOp op, atomToOperand atom)
                | ANF.Call (funcName, args) ->
                    let argOperands = List.map atomToOperand args
                    MIR.Call (destReg, funcName, argOperands)
                | ANF.IfValue _ ->
                    // Already handled above
                    failwith "Unreachable: IfValue already handled"

            // Let bindings accumulate instructions, pass through join label
            convertExprToOperand rest startLabel (startInstrs @ [instr]) builder

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // If expression: creates blocks with branch/jump/join structure
        let condOp = atomToOperand condAtom
        let (thenLabel, labelGen1) = MIR.freshLabel builder.LabelGen
        let (elseLabel, labelGen2) = MIR.freshLabel labelGen1
        let (joinLabel, labelGen3) = MIR.freshLabel labelGen2
        let (resultReg, regGen1) = MIR.freshReg builder.RegGen

        let startBlock = {
            MIR.Label = startLabel
            MIR.Instrs = startInstrs
            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
        }

        let builder1 = {
            Blocks = Map.add startLabel startBlock builder.Blocks
            LabelGen = labelGen3
            RegGen = regGen1
        }

        // Convert then-branch
        let (thenResult, thenJoinOpt, builder2) = convertExprToOperand thenBranch thenLabel [] builder1

        // If then-branch created blocks (nested if), patch its join block
        // Otherwise, create a simple block that moves result and jumps
        let builder3 =
            match thenJoinOpt with
            | Some nestedJoinLabel ->
                // Patch the nested join block to jump to our join instead of returning
                match Map.tryFind nestedJoinLabel builder2.Blocks with
                | Some nestedJoinBlock ->
                    let patchedBlock = {
                        nestedJoinBlock with
                            Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, thenResult)]
                            Terminator = MIR.Jump joinLabel
                    }
                    { builder2 with Blocks = Map.add nestedJoinLabel patchedBlock builder2.Blocks }
                | None -> builder2  // Should not happen
            | None ->
                // Simple expression - create block that moves result and jumps
                let thenBlock = {
                    MIR.Label = thenLabel
                    MIR.Instrs = [MIR.Mov (resultReg, thenResult)]
                    MIR.Terminator = MIR.Jump joinLabel
                }
                { builder2 with Blocks = Map.add thenLabel thenBlock builder2.Blocks }

        // Convert else-branch
        let (elseResult, elseJoinOpt, builder4) = convertExprToOperand elseBranch elseLabel [] builder3

        // Same logic for else-branch
        let builder5 =
            match elseJoinOpt with
            | Some nestedJoinLabel ->
                match Map.tryFind nestedJoinLabel builder4.Blocks with
                | Some nestedJoinBlock ->
                    let patchedBlock = {
                        nestedJoinBlock with
                            Instrs = nestedJoinBlock.Instrs @ [MIR.Mov (resultReg, elseResult)]
                            Terminator = MIR.Jump joinLabel
                    }
                    { builder4 with Blocks = Map.add nestedJoinLabel patchedBlock builder4.Blocks }
                | None -> builder4  // Should not happen
            | None ->
                let elseBlock = {
                    MIR.Label = elseLabel
                    MIR.Instrs = [MIR.Mov (resultReg, elseResult)]
                    MIR.Terminator = MIR.Jump joinLabel
                }
                { builder4 with Blocks = Map.add elseLabel elseBlock builder4.Blocks }

        // Create join block
        let joinBlock = {
            MIR.Label = joinLabel
            MIR.Instrs = []
            MIR.Terminator = MIR.Ret (MIR.Register resultReg)
        }
        let builder6 = { builder5 with Blocks = Map.add joinLabel joinBlock builder5.Blocks }

        // Return result register and our join label for potential patching by caller
        (MIR.Register resultReg, Some joinLabel, builder6)

/// Convert an ANF function to a MIR function
let convertANFFunction (anfFunc: ANF.Function) (regGen: MIR.RegGen) : MIR.Function * MIR.RegGen =
    // Create initial builder
    let initialBuilder = {
        RegGen = regGen
        LabelGen = MIR.initialLabelGen
        Blocks = Map.empty
    }

    // Create entry label for CFG (internal to function body)
    let entryLabel = MIR.Label $"{anfFunc.Name}_body"

    // Allocate VRegs for parameters
    let (paramVRegs, builder1) =
        anfFunc.Params
        |> List.fold (fun (vregs, builder) _ ->
            let (vreg, regGen') = MIR.freshReg builder.RegGen
            (vregs @ [vreg], { builder with RegGen = regGen' }))
            ([], initialBuilder)

    // Convert function body to CFG
    let (_, finalBuilder) = convertExpr anfFunc.Body entryLabel [] builder1

    let cfg = {
        MIR.Entry = entryLabel
        MIR.Blocks = finalBuilder.Blocks
    }

    let mirFunc = {
        MIR.Name = anfFunc.Name
        MIR.Params = paramVRegs
        MIR.CFG = cfg
    }

    (mirFunc, finalBuilder.RegGen)

/// Convert ANF program to MIR program
let toMIR (program: ANF.Program) (regGen: MIR.RegGen) : MIR.Program * MIR.RegGen =
    let (ANF.Program (functions, mainExpr)) = program

    // Convert all functions to MIR
    let (mirFuncs, regGen1) =
        functions
        |> List.fold (fun (funcs, rg) anfFunc ->
            let (mirFunc, rg') = convertANFFunction anfFunc rg
            (funcs @ [mirFunc], rg')) ([], regGen)

    // Convert main expression (if any) to a synthetic "_start" function
    let (allFuncs, finalRegGen) =
        match mainExpr with
        | Some expr ->
            // Create _start function from main expression
            let entryLabel = MIR.Label "_start_body"
            let initialBuilder = {
                RegGen = regGen1
                LabelGen = MIR.initialLabelGen
                Blocks = Map.empty
            }
            let (_, finalBuilder) = convertExpr expr entryLabel [] initialBuilder
            let cfg = {
                MIR.Entry = entryLabel
                MIR.Blocks = finalBuilder.Blocks
            }
            let startFunc = {
                MIR.Name = "_start"
                MIR.Params = []
                MIR.CFG = cfg
            }
            (mirFuncs @ [startFunc], finalBuilder.RegGen)
        | None ->
            // No main expression - check if there's a main() function
            let hasMainFunc = functions |> List.exists (fun f -> f.Name = "main")
            if hasMainFunc then
                // Create _start function that calls main()
                let entryLabel = MIR.Label "_start_body"
                let (resultReg, regGen2) = MIR.freshReg regGen1

                // Create block that calls main and returns result
                let block = {
                    MIR.Label = entryLabel
                    MIR.Instrs = [MIR.Call (resultReg, "main", [])]
                    MIR.Terminator = MIR.Ret (MIR.Register resultReg)
                }

                let cfg = {
                    MIR.Entry = entryLabel
                    MIR.Blocks = Map.ofList [(entryLabel, block)]
                }

                let startFunc = {
                    MIR.Name = "_start"
                    MIR.Params = []
                    MIR.CFG = cfg
                }
                (mirFuncs @ [startFunc], regGen2)
            else
                failwith "Program must have either a main expression or a main() function"

    (MIR.Program allFuncs, finalRegGen)
