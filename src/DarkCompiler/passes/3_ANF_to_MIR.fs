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
    | ANF.Mod -> MIR.Mod
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

/// Sequence a list of Results into a Result of list
let sequenceResults (results: Result<'a, string> list) : Result<'a list, string> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | r :: rest ->
            match r with
            | Ok v -> loop (v :: acc) rest
            | Error e -> Error e
    loop [] results

/// Map ANF TempId to MIR virtual register
let tempToVReg (ANF.TempId id) : MIR.VReg = MIR.VReg id

/// Find the maximum TempId in an atom (returns -1 if no TempId)
let maxTempIdInAtom (atom: ANF.Atom) : int =
    match atom with
    | ANF.Var (ANF.TempId id) -> id
    | _ -> -1

/// Find the maximum TempId in a CExpr
let maxTempIdInCExpr (cexpr: ANF.CExpr) : int =
    match cexpr with
    | ANF.Atom atom -> maxTempIdInAtom atom
    | ANF.Prim (_, left, right) ->
        max (maxTempIdInAtom left) (maxTempIdInAtom right)
    | ANF.UnaryPrim (_, atom) -> maxTempIdInAtom atom
    | ANF.IfValue (cond, thenVal, elseVal) ->
        max (maxTempIdInAtom cond) (max (maxTempIdInAtom thenVal) (maxTempIdInAtom elseVal))
    | ANF.Call (_, args) ->
        args |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.IndirectCall (func, args) ->
        max (maxTempIdInAtom func) (args |> List.map maxTempIdInAtom |> List.fold max -1)
    | ANF.TupleAlloc atoms ->
        atoms |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.TupleGet (tuple, _) -> maxTempIdInAtom tuple
    | ANF.StringConcat (left, right) ->
        max (maxTempIdInAtom left) (maxTempIdInAtom right)
    | ANF.RefCountInc (atom, _) -> maxTempIdInAtom atom
    | ANF.RefCountDec (atom, _) -> maxTempIdInAtom atom
    | ANF.Print (atom, _) -> maxTempIdInAtom atom
    | ANF.ClosureAlloc (_, captures) ->
        captures |> List.map maxTempIdInAtom |> List.fold max -1
    | ANF.ClosureCall (closure, args) ->
        max (maxTempIdInAtom closure) (args |> List.map maxTempIdInAtom |> List.fold max -1)
    | ANF.FileReadText path -> maxTempIdInAtom path
    | ANF.FileExists path -> maxTempIdInAtom path
    | ANF.FileWriteText (path, content) -> max (maxTempIdInAtom path) (maxTempIdInAtom content)
    | ANF.FileAppendText (path, content) -> max (maxTempIdInAtom path) (maxTempIdInAtom content)

/// Find the maximum TempId in an AExpr
let rec maxTempIdInAExpr (expr: ANF.AExpr) : int =
    match expr with
    | ANF.Let (ANF.TempId id, cexpr, body) ->
        max id (max (maxTempIdInCExpr cexpr) (maxTempIdInAExpr body))
    | ANF.Return atom -> maxTempIdInAtom atom
    | ANF.If (cond, thenBranch, elseBranch) ->
        max (maxTempIdInAtom cond) (max (maxTempIdInAExpr thenBranch) (maxTempIdInAExpr elseBranch))

/// Find the maximum TempId in a function
let maxTempIdInFunction (func: ANF.Function) : int =
    let paramMax =
        func.Params
        |> List.map (fun (ANF.TempId id) -> id)
        |> List.fold max -1
    max paramMax (maxTempIdInAExpr func.Body)

/// Find the maximum TempId in an ANF program
let maxTempIdInProgram (program: ANF.Program) : int =
    let (ANF.Program (functions, mainExpr)) = program
    let funcMax =
        functions
        |> List.map maxTempIdInFunction
        |> List.fold max -1
    let mainMax = maxTempIdInAExpr mainExpr
    max funcMax mainMax

/// Collect all string literals from an ANF atom
let collectStringsFromAtom (atom: ANF.Atom) : string list =
    match atom with
    | ANF.StringLiteral s -> [s]
    | _ -> []

/// Collect all float literals from an ANF atom
let collectFloatsFromAtom (atom: ANF.Atom) : float list =
    match atom with
    | ANF.FloatLiteral f -> [f]
    | _ -> []

/// Collect all string literals from a CExpr
let collectStringsFromCExpr (cexpr: ANF.CExpr) : string list =
    match cexpr with
    | ANF.Atom atom -> collectStringsFromAtom atom
    | ANF.Prim (_, left, right) ->
        collectStringsFromAtom left @ collectStringsFromAtom right
    | ANF.UnaryPrim (_, atom) -> collectStringsFromAtom atom
    | ANF.IfValue (cond, thenAtom, elseAtom) ->
        collectStringsFromAtom cond @
        collectStringsFromAtom thenAtom @
        collectStringsFromAtom elseAtom
    | ANF.Call (_, args) ->
        args |> List.collect collectStringsFromAtom
    | ANF.IndirectCall (func, args) ->
        collectStringsFromAtom func @ (args |> List.collect collectStringsFromAtom)
    | ANF.TupleAlloc elems ->
        elems |> List.collect collectStringsFromAtom
    | ANF.TupleGet (tupleAtom, _) ->
        collectStringsFromAtom tupleAtom
    | ANF.StringConcat (left, right) ->
        collectStringsFromAtom left @ collectStringsFromAtom right
    | ANF.RefCountInc (atom, _) -> collectStringsFromAtom atom
    | ANF.RefCountDec (atom, _) -> collectStringsFromAtom atom
    | ANF.Print (atom, _) -> collectStringsFromAtom atom
    | ANF.ClosureAlloc (_, captures) -> captures |> List.collect collectStringsFromAtom
    | ANF.ClosureCall (closure, args) ->
        collectStringsFromAtom closure @ (args |> List.collect collectStringsFromAtom)
    | ANF.FileReadText path -> collectStringsFromAtom path
    | ANF.FileExists path -> collectStringsFromAtom path
    | ANF.FileWriteText (path, content) -> collectStringsFromAtom path @ collectStringsFromAtom content
    | ANF.FileAppendText (path, content) -> collectStringsFromAtom path @ collectStringsFromAtom content

/// Collect all float literals from a CExpr
let collectFloatsFromCExpr (cexpr: ANF.CExpr) : float list =
    match cexpr with
    | ANF.Atom atom -> collectFloatsFromAtom atom
    | ANF.Prim (_, left, right) ->
        collectFloatsFromAtom left @ collectFloatsFromAtom right
    | ANF.UnaryPrim (_, atom) -> collectFloatsFromAtom atom
    | ANF.IfValue (cond, thenAtom, elseAtom) ->
        collectFloatsFromAtom cond @
        collectFloatsFromAtom thenAtom @
        collectFloatsFromAtom elseAtom
    | ANF.Call (_, args) ->
        args |> List.collect collectFloatsFromAtom
    | ANF.IndirectCall (func, args) ->
        collectFloatsFromAtom func @ (args |> List.collect collectFloatsFromAtom)
    | ANF.TupleAlloc elems ->
        elems |> List.collect collectFloatsFromAtom
    | ANF.TupleGet (tupleAtom, _) ->
        collectFloatsFromAtom tupleAtom
    | ANF.StringConcat (left, right) ->
        collectFloatsFromAtom left @ collectFloatsFromAtom right
    | ANF.RefCountInc (atom, _) -> collectFloatsFromAtom atom
    | ANF.RefCountDec (atom, _) -> collectFloatsFromAtom atom
    | ANF.Print (atom, _) -> collectFloatsFromAtom atom
    | ANF.ClosureAlloc (_, captures) -> captures |> List.collect collectFloatsFromAtom
    | ANF.ClosureCall (closure, args) ->
        collectFloatsFromAtom closure @ (args |> List.collect collectFloatsFromAtom)
    | ANF.FileReadText path -> collectFloatsFromAtom path
    | ANF.FileExists path -> collectFloatsFromAtom path
    | ANF.FileWriteText (path, content) -> collectFloatsFromAtom path @ collectFloatsFromAtom content
    | ANF.FileAppendText (path, content) -> collectFloatsFromAtom path @ collectFloatsFromAtom content

/// Collect all string literals from an ANF expression
let rec collectStringsFromExpr (expr: ANF.AExpr) : string list =
    match expr with
    | ANF.Return atom -> collectStringsFromAtom atom
    | ANF.Let (_, cexpr, rest) ->
        collectStringsFromCExpr cexpr @ collectStringsFromExpr rest
    | ANF.If (cond, thenBranch, elseBranch) ->
        collectStringsFromAtom cond @
        collectStringsFromExpr thenBranch @
        collectStringsFromExpr elseBranch

/// Collect all float literals from an ANF expression
let rec collectFloatsFromExpr (expr: ANF.AExpr) : float list =
    match expr with
    | ANF.Return atom -> collectFloatsFromAtom atom
    | ANF.Let (_, cexpr, rest) ->
        collectFloatsFromCExpr cexpr @ collectFloatsFromExpr rest
    | ANF.If (cond, thenBranch, elseBranch) ->
        collectFloatsFromAtom cond @
        collectFloatsFromExpr thenBranch @
        collectFloatsFromExpr elseBranch

/// Collect all string literals from an ANF function
let collectStringsFromFunction (func: ANF.Function) : string list =
    collectStringsFromExpr func.Body

/// Collect all float literals from an ANF function
let collectFloatsFromFunction (func: ANF.Function) : float list =
    collectFloatsFromExpr func.Body

/// Collect all string literals from an ANF program
let collectStringsFromProgram (program: ANF.Program) : string list =
    let (ANF.Program (functions, mainExpr)) = program
    let funcStrings = functions |> List.collect collectStringsFromFunction
    let mainStrings = collectStringsFromExpr mainExpr
    funcStrings @ mainStrings

/// Collect all float literals from an ANF program
let collectFloatsFromProgram (program: ANF.Program) : float list =
    let (ANF.Program (functions, mainExpr)) = program
    let funcFloats = functions |> List.collect collectFloatsFromFunction
    let mainFloats = collectFloatsFromExpr mainExpr
    funcFloats @ mainFloats

/// Build a string pool from a list of strings (deduplicates)
let buildStringPool (strings: string list) : MIR.StringPool =
    strings
    |> List.fold (fun pool s ->
        let (_, pool') = MIR.addString pool s
        pool') MIR.emptyStringPool

/// Build a float pool from a list of floats (deduplicates)
let buildFloatPool (floats: float list) : MIR.FloatPool =
    floats
    |> List.fold (fun pool f ->
        let (_, pool') = MIR.addFloat pool f
        pool') MIR.emptyFloatPool

/// Build a lookup map from string content to pool index
let buildStringLookup (pool: MIR.StringPool) : Map<string, int> =
    pool.Strings
    |> Map.fold (fun lookup idx (s, _) -> Map.add s idx lookup) Map.empty

/// Build a lookup map from float value to pool index
let buildFloatLookup (pool: MIR.FloatPool) : Map<float, int> =
    pool.Floats
    |> Map.fold (fun lookup idx f -> Map.add f idx lookup) Map.empty

/// CFG builder state - includes lookups to avoid mutable module-level state
/// which would cause race conditions in parallel test execution
type CFGBuilder = {
    Blocks: Map<MIR.Label, MIR.BasicBlock>
    LabelGen: MIR.LabelGen
    RegGen: MIR.RegGen
    StringLookup: Map<string, int>
    FloatLookup: Map<float, int>
    TypeMap: ANF.TypeMap
    TypeReg: Map<string, (string * AST.Type) list>
    FuncName: string  // For generating unique labels per function
}

/// Get payload size for an atom used in reference counting
/// Returns Error if type lookup fails
let getPayloadSizeForAtom (builder: CFGBuilder) (atom: ANF.Atom) : Result<int, string> =
    match atom with
    | ANF.Var tid ->
        match Map.tryFind tid builder.TypeMap with
        | Some typ -> Ok (ANF.payloadSize typ builder.TypeReg)
        | None -> Error $"Internal error: type not found for {tid} in TypeMap"
    | _ -> Error "Internal error: RefCount operation on non-variable atom"

/// Convert ANF Atom to MIR Operand using lookups from builder
/// Returns Error if float/string lookup fails (internal invariant violation)
let atomToOperand (builder: CFGBuilder) (atom: ANF.Atom) : Result<MIR.Operand, string> =
    match atom with
    | ANF.UnitLiteral -> Ok (MIR.IntConst 0L)  // Unit is represented as 0
    | ANF.IntLiteral n -> Ok (MIR.IntConst n)
    | ANF.BoolLiteral b -> Ok (MIR.BoolConst b)
    | ANF.FloatLiteral f ->
        match Map.tryFind f builder.FloatLookup with
        | Some idx -> Ok (MIR.FloatRef idx)
        | None -> Error $"Internal error: float literal {f} not found in pool"
    | ANF.StringLiteral s ->
        match Map.tryFind s builder.StringLookup with
        | Some idx -> Ok (MIR.StringRef idx)
        | None -> Error $"Internal error: string literal not found in pool"
    | ANF.Var tempId -> Ok (MIR.Register (tempToVReg tempId))
    | ANF.FuncRef funcName -> Ok (MIR.FuncAddr funcName)

/// Convert ANF expression to CFG
/// Returns: Result of (final value operand, CFG builder with all blocks)
let rec convertExpr
    (expr: ANF.AExpr)
    (currentLabel: MIR.Label)
    (currentInstrs: MIR.Instr list)
    (builder: CFGBuilder)
    : Result<MIR.Operand * CFGBuilder, string> =

    match expr with
    | ANF.Return atom ->
        // Return: end current block with Ret terminator
        atomToOperand builder atom
        |> Result.bind (fun operand ->
            let block = {
                MIR.Label = currentLabel
                MIR.Instrs = currentInstrs
                MIR.Terminator = MIR.Ret operand
            }
            let builder' = { builder with Blocks = Map.add currentLabel block builder.Blocks }
            Ok (operand, builder'))

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

            atomToOperand builder condAtom
            |> Result.bind (fun condOp ->
                atomToOperand builder thenAtom
                |> Result.bind (fun thenOp ->
                    atomToOperand builder elseAtom
                    |> Result.bind (fun elseOp ->
                        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
                        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
                        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2

                        // Current block ends with branch
                        let currentBlock = {
                            MIR.Label = currentLabel
                            MIR.Instrs = currentInstrs
                            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
                        }

                        // Then block: assign thenAtom to destReg, jump to join
                        let thenBlock = {
                            MIR.Label = thenLabel
                            MIR.Instrs = [MIR.Mov (destReg, thenOp)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        // Else block: assign elseAtom to destReg, jump to join
                        let elseBlock = {
                            MIR.Label = elseLabel
                            MIR.Instrs = [MIR.Mov (destReg, elseOp)]
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
                        convertExpr rest joinLabel [] builder')))

        | _ ->
            // Simple CExpr: add instruction(s) to current block, continue
            let instrsResult =
                match cexpr with
                | ANF.Atom atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op)])
                | ANF.Prim (op, leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.BinOp (destReg, convertBinOp op, leftOp, rightOp)]))
                | ANF.UnaryPrim (op, atom) ->
                    atomToOperand builder atom
                    |> Result.map (fun operand ->
                        [MIR.UnaryOp (destReg, convertUnaryOp op, operand)])
                | ANF.Call (funcName, args) ->
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.Call (destReg, funcName, argOperands)])
                | ANF.IndirectCall (func, args) ->
                    atomToOperand builder func
                    |> Result.bind (fun funcOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.IndirectCall (destReg, funcOp, argOperands)]))
                | ANF.ClosureAlloc (funcName, captures) ->
                    // Allocate closure: (func_addr, cap1, cap2, ...)
                    let numSlots = 1 + List.length captures  // func_ptr + captures
                    let sizeBytes = numSlots * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store function pointer at offset 0
                    let storeFuncInstr = MIR.HeapStore (destReg, 0, MIR.FuncAddr funcName)
                    // Store captured values at offsets 8, 16, ...
                    captures
                    |> List.mapi (fun i cap -> (i, cap))
                    |> List.map (fun (i, cap) ->
                        atomToOperand builder cap
                        |> Result.map (fun op -> MIR.HeapStore (destReg, (i + 1) * 8, op)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeFuncInstr :: storeInstrs)
                | ANF.ClosureCall (closure, args) ->
                    // Call through closure: extract func_ptr, call with (closure, args...)
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureCall (destReg, closureOp, argOperands)]))
                | ANF.TupleAlloc elems ->
                    // Allocate heap space: 8 bytes per element
                    let sizeBytes = List.length elems * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store each element at its offset
                    elems
                    |> List.mapi (fun i elem -> (i, elem))
                    |> List.map (fun (i, elem) ->
                        atomToOperand builder elem
                        |> Result.map (fun op -> MIR.HeapStore (destReg, i * 8, op)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeInstrs)
                | ANF.TupleGet (tupleAtom, index) ->
                    // Tuple should always be a variable in ANF
                    match tupleAtom with
                    | ANF.Var tid ->
                        let tupleReg = tempToVReg tid
                        Ok [MIR.HeapLoad (destReg, tupleReg, index * 8)]
                    | _ ->
                        Error "Internal error: Tuple access on non-variable (ANF invariant violated)"
                | ANF.IfValue _ ->
                    // This case is handled above; reaching here indicates a bug
                    Error "Internal error: IfValue should have been handled in outer match"
                | ANF.RefCountInc (atom, payloadSize) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountInc (tempToVReg tid, payloadSize)]
                    | _ -> Error "Internal error: RefCountInc on non-variable"
                | ANF.RefCountDec (atom, payloadSize) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountDec (tempToVReg tid, payloadSize)]
                    | _ -> Error "Internal error: RefCountDec on non-variable"
                | ANF.Print (atom, valueType) ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Print (op, valueType)])
                | ANF.StringConcat (leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.StringConcat (destReg, leftOp, rightOp)]))
                | ANF.FileReadText pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileReadText (destReg, pathOp)])
                | ANF.FileExists pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileExists (destReg, pathOp)])
                | ANF.FileWriteText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileWriteText (destReg, pathOp, contentOp)]))
                | ANF.FileAppendText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileAppendText (destReg, pathOp, contentOp)]))

            match instrsResult with
            | Error err -> Error err
            | Ok instrs ->
                let newInstrs = currentInstrs @ instrs
                convertExpr rest currentLabel newInstrs builder

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // If expression:
        // 1. End current block with Branch terminator
        // 2. Create then-block and else-block
        // 3. Create join-block where both branches meet
        // 4. Both branches put result in same register and jump to join

        atomToOperand builder condAtom
        |> Result.bind (fun condOp ->

        // Generate labels for then, else, and join blocks
        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2

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
            StringLookup = builder.StringLookup
            FloatLookup = builder.FloatLookup
            TypeMap = builder.TypeMap
            TypeReg = builder.TypeReg
            FuncName = builder.FuncName
        }

        // Convert then-branch: result goes into resultReg, then jump to join
        match convertExprToOperand thenBranch thenLabel [] builder1 with
        | Error err -> Error err
        | Ok (thenResult, thenJoinOpt, builder2) ->

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
        match convertExprToOperand elseBranch elseLabel [] builder3 with
        | Error err -> Error err
        | Ok (elseResult, elseJoinOpt, builder4) ->

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
        Ok (resultOp, builder6))

/// Helper: convert expression and extract final operand
/// Returns: Result of (operand, optional join label if blocks were created, builder)
/// - If join label is Some(label), the expression created blocks ending at that join block
/// - If join label is None, no blocks were created (simple expression)
and convertExprToOperand
    (expr: ANF.AExpr)
    (startLabel: MIR.Label)
    (startInstrs: MIR.Instr list)
    (builder: CFGBuilder)
    : Result<MIR.Operand * MIR.Label option * CFGBuilder, string> =

    match expr with
    | ANF.Return atom ->
        // If we have accumulated instructions from Let bindings, create a block
        // Otherwise just return the operand
        atomToOperand builder atom
        |> Result.bind (fun operand ->
            if List.isEmpty startInstrs then
                Ok (operand, None, builder)
            else
                // Create a block with accumulated instructions
                // Use temporary Ret terminator - caller will patch if needed
                let block = {
                    MIR.Label = startLabel
                    MIR.Instrs = startInstrs
                    MIR.Terminator = MIR.Ret operand
                }
                let builder' = { builder with Blocks = Map.add startLabel block builder.Blocks }
                Ok (operand, Some startLabel, builder'))

    | ANF.Let (tempId, cexpr, rest) ->
        let destReg = tempToVReg tempId

        match cexpr with
        | ANF.IfValue (condAtom, thenAtom, elseAtom) ->
            // IfValue requires control flow - similar to convertExpr version
            atomToOperand builder condAtom
            |> Result.bind (fun condOp ->
                atomToOperand builder thenAtom
                |> Result.bind (fun thenOp ->
                    atomToOperand builder elseAtom
                    |> Result.bind (fun elseOp ->
                        let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
                        let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
                        let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2

                        // Current block ends with branch
                        let startBlock = {
                            MIR.Label = startLabel
                            MIR.Instrs = startInstrs
                            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
                        }

                        // Then block: assign thenAtom to destReg, jump to join
                        let thenBlock = {
                            MIR.Label = thenLabel
                            MIR.Instrs = [MIR.Mov (destReg, thenOp)]
                            MIR.Terminator = MIR.Jump joinLabel
                        }

                        // Else block: assign elseAtom to destReg, jump to join
                        let elseBlock = {
                            MIR.Label = elseLabel
                            MIR.Instrs = [MIR.Mov (destReg, elseOp)]
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
                        convertExprToOperand rest joinLabel [] builder')))

        | _ ->
            // Simple CExpr: create instruction(s) and accumulate
            let instrsResult =
                match cexpr with
                | ANF.Atom atom ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Mov (destReg, op)])
                | ANF.Prim (op, leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.BinOp (destReg, convertBinOp op, leftOp, rightOp)]))
                | ANF.UnaryPrim (op, atom) ->
                    atomToOperand builder atom
                    |> Result.map (fun operand ->
                        [MIR.UnaryOp (destReg, convertUnaryOp op, operand)])
                | ANF.Call (funcName, args) ->
                    args
                    |> List.map (atomToOperand builder)
                    |> sequenceResults
                    |> Result.map (fun argOperands ->
                        [MIR.Call (destReg, funcName, argOperands)])
                | ANF.IndirectCall (func, args) ->
                    atomToOperand builder func
                    |> Result.bind (fun funcOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.IndirectCall (destReg, funcOp, argOperands)]))
                | ANF.ClosureAlloc (funcName, captures) ->
                    // Allocate closure: (func_addr, cap1, cap2, ...)
                    let numSlots = 1 + List.length captures  // func_ptr + captures
                    let sizeBytes = numSlots * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store function pointer at offset 0
                    let storeFuncInstr = MIR.HeapStore (destReg, 0, MIR.FuncAddr funcName)
                    // Store captured values at offsets 8, 16, ...
                    captures
                    |> List.mapi (fun i cap -> (i, cap))
                    |> List.map (fun (i, cap) ->
                        atomToOperand builder cap
                        |> Result.map (fun op -> MIR.HeapStore (destReg, (i + 1) * 8, op)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeFuncInstr :: storeInstrs)
                | ANF.ClosureCall (closure, args) ->
                    // Call through closure: extract func_ptr, call with (closure, args...)
                    atomToOperand builder closure
                    |> Result.bind (fun closureOp ->
                        args
                        |> List.map (atomToOperand builder)
                        |> sequenceResults
                        |> Result.map (fun argOperands ->
                            [MIR.ClosureCall (destReg, closureOp, argOperands)]))
                | ANF.TupleAlloc elems ->
                    // Allocate heap space: 8 bytes per element
                    let sizeBytes = List.length elems * 8
                    let allocInstr = MIR.HeapAlloc (destReg, sizeBytes)
                    // Store each element at its offset
                    elems
                    |> List.mapi (fun i elem -> (i, elem))
                    |> List.map (fun (i, elem) ->
                        atomToOperand builder elem
                        |> Result.map (fun op -> MIR.HeapStore (destReg, i * 8, op)))
                    |> sequenceResults
                    |> Result.map (fun storeInstrs -> allocInstr :: storeInstrs)
                | ANF.TupleGet (tupleAtom, index) ->
                    // Tuple should always be a variable in ANF
                    match tupleAtom with
                    | ANF.Var tid ->
                        let tupleReg = tempToVReg tid
                        Ok [MIR.HeapLoad (destReg, tupleReg, index * 8)]
                    | _ ->
                        Error "Internal error: Tuple access on non-variable (ANF invariant violated)"
                | ANF.IfValue _ ->
                    // This case is handled above; reaching here indicates a bug
                    Error "Internal error: IfValue should have been handled in outer match"
                | ANF.RefCountInc (atom, payloadSize) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountInc (tempToVReg tid, payloadSize)]
                    | _ -> Error "Internal error: RefCountInc on non-variable"
                | ANF.RefCountDec (atom, payloadSize) ->
                    match atom with
                    | ANF.Var tid ->
                        Ok [MIR.RefCountDec (tempToVReg tid, payloadSize)]
                    | _ -> Error "Internal error: RefCountDec on non-variable"
                | ANF.Print (atom, valueType) ->
                    atomToOperand builder atom
                    |> Result.map (fun op -> [MIR.Print (op, valueType)])
                | ANF.StringConcat (leftAtom, rightAtom) ->
                    atomToOperand builder leftAtom
                    |> Result.bind (fun leftOp ->
                        atomToOperand builder rightAtom
                        |> Result.map (fun rightOp ->
                            [MIR.StringConcat (destReg, leftOp, rightOp)]))
                | ANF.FileReadText pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileReadText (destReg, pathOp)])
                | ANF.FileExists pathAtom ->
                    atomToOperand builder pathAtom
                    |> Result.map (fun pathOp -> [MIR.FileExists (destReg, pathOp)])
                | ANF.FileWriteText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileWriteText (destReg, pathOp, contentOp)]))
                | ANF.FileAppendText (pathAtom, contentAtom) ->
                    atomToOperand builder pathAtom
                    |> Result.bind (fun pathOp ->
                        atomToOperand builder contentAtom
                        |> Result.map (fun contentOp ->
                            [MIR.FileAppendText (destReg, pathOp, contentOp)]))

            // Let bindings accumulate instructions, pass through join label
            match instrsResult with
            | Error err -> Error err
            | Ok instrs ->
                convertExprToOperand rest startLabel (startInstrs @ instrs) builder

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // If expression: creates blocks with branch/jump/join structure
        atomToOperand builder condAtom
        |> Result.bind (fun condOp ->
            let (thenLabel, labelGen1) = MIR.freshLabelWithPrefix builder.FuncName builder.LabelGen
            let (elseLabel, labelGen2) = MIR.freshLabelWithPrefix builder.FuncName labelGen1
            let (joinLabel, labelGen3) = MIR.freshLabelWithPrefix builder.FuncName labelGen2
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
                StringLookup = builder.StringLookup
                FloatLookup = builder.FloatLookup
                TypeMap = builder.TypeMap
                TypeReg = builder.TypeReg
                FuncName = builder.FuncName
            }

            // Convert then-branch
            match convertExprToOperand thenBranch thenLabel [] builder1 with
            | Error err -> Error err
            | Ok (thenResult, thenJoinOpt, builder2) ->

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
            match convertExprToOperand elseBranch elseLabel [] builder3 with
            | Error err -> Error err
            | Ok (elseResult, elseJoinOpt, builder4) ->

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
            Ok (MIR.Register resultReg, Some joinLabel, builder6))

/// Convert an ANF function to a MIR function
let convertANFFunction (anfFunc: ANF.Function) (regGen: MIR.RegGen) (strLookup: Map<string, int>) (fltLookup: Map<float, int>) (typeMap: ANF.TypeMap) (typeReg: Map<string, (string * AST.Type) list>) : Result<MIR.Function * MIR.RegGen, string> =
    // Create initial builder with lookups
    let initialBuilder = {
        RegGen = regGen
        LabelGen = MIR.initialLabelGen
        Blocks = Map.empty
        StringLookup = strLookup
        FloatLookup = fltLookup
        TypeMap = typeMap
        TypeReg = typeReg
        FuncName = anfFunc.Name
    }

    // Create entry label for CFG (internal to function body)
    let entryLabel = MIR.Label $"{anfFunc.Name}_body"

    // Convert ANF parameter TempIds to MIR VRegs
    // Must use tempToVReg to preserve the TempId values, not fresh VRegs,
    // because the body uses Var (TempId n) which converts to VReg n
    let paramVRegs = anfFunc.Params |> List.map tempToVReg

    // Convert function body to CFG
    match convertExpr anfFunc.Body entryLabel [] initialBuilder with
    | Error err -> Error err
    | Ok (_, finalBuilder) ->

    let cfg = {
        MIR.Entry = entryLabel
        MIR.Blocks = finalBuilder.Blocks
    }

    let mirFunc = {
        MIR.Name = anfFunc.Name
        MIR.Params = paramVRegs
        MIR.CFG = cfg
    }

    Ok (mirFunc, finalBuilder.RegGen)

/// Convert ANF program to MIR program
let toMIR (program: ANF.Program) (_regGen: MIR.RegGen) (typeMap: ANF.TypeMap) (typeReg: Map<string, (string * AST.Type) list>) : Result<MIR.Program * MIR.RegGen, string> =
    let (ANF.Program (functions, mainExpr)) = program

    // Critical: freshReg must generate VRegs that don't conflict with TempId-derived VRegs.
    // tempToVReg (TempId n) → VReg n, so freshReg must start past the max TempId used.
    let maxId = maxTempIdInProgram program
    let regGen = MIR.RegGen (maxId + 1)

    // Phase 1: Collect all strings and floats, build pools and lookups
    // Lookups are local (not mutable module-level) to avoid race conditions in parallel tests
    let allStrings = collectStringsFromProgram program
    let stringPool = buildStringPool allStrings
    let strLookup = buildStringLookup stringPool

    let allFloats = collectFloatsFromProgram program
    let floatPool = buildFloatPool allFloats
    let fltLookup = buildFloatLookup floatPool

    // Phase 2: Convert all functions to MIR
    let rec convertFunctions funcs rg remaining =
        match remaining with
        | [] -> Ok (funcs, rg)
        | anfFunc :: rest ->
            match convertANFFunction anfFunc rg strLookup fltLookup typeMap typeReg with
            | Error err -> Error err
            | Ok (mirFunc, rg') -> convertFunctions (funcs @ [mirFunc]) rg' rest

    match convertFunctions [] regGen functions with
    | Error err -> Error err
    | Ok (mirFuncs, regGen1) ->

    // Convert main expression to a synthetic "_start" function
    let entryLabel = MIR.Label "_start_body"
    let initialBuilder = {
        RegGen = regGen1
        LabelGen = MIR.initialLabelGen
        Blocks = Map.empty
        StringLookup = strLookup
        FloatLookup = fltLookup
        TypeMap = typeMap
        TypeReg = typeReg
        FuncName = "_start"
    }
    match convertExpr mainExpr entryLabel [] initialBuilder with
    | Error err -> Error err
    | Ok (_, finalBuilder) ->
    let cfg = {
        MIR.Entry = entryLabel
        MIR.Blocks = finalBuilder.Blocks
    }
    let startFunc = {
        MIR.Name = "_start"
        MIR.Params = []
        MIR.CFG = cfg
    }
    let allFuncs = mirFuncs @ [startFunc]
    Ok (MIR.Program (allFuncs, stringPool, floatPool), finalBuilder.RegGen)
