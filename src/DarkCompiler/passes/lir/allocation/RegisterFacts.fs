// RegisterFacts.fs - Classify integer and floating register definitions and uses.

module RegisterFacts

open AllocationModel

// ============================================================================
// Liveness Analysis
// ============================================================================

/// Get virtual register IDs used (read) by an instruction
let getUsedVRegs (instr: LIR.Instr) : int list =
    let regToVReg (reg: LIR.Reg) : int option =
        match reg with
        | LIR.Virtual id -> Some id
        | LIR.Physical _ -> None

    let operandToVReg (op: LIR.Operand) : int option =
        match op with
        | LIR.Reg reg -> regToVReg reg
        | _ -> None

    match instr with
    | LIR.Mov (_, src) ->
        operandToVReg src |> Option.toList
    | LIR.Store (_, src) ->
        regToVReg src |> Option.toList
    | LIR.Add (_, left, right) | LIR.Sub (_, left, right) ->
        (regToVReg left |> Option.toList) @ (operandToVReg right |> Option.toList)
    | LIR.Mul (_, left, right) | LIR.Sdiv (_, left, right) | LIR.Udiv (_, left, right)
    | LIR.And (_, left, right) | LIR.Orr (_, left, right) | LIR.Eor (_, left, right)
    | LIR.Lsl (_, left, right) | LIR.Lsr (_, left, right) | LIR.Asr (_, left, right) ->
        (regToVReg left |> Option.toList) @ (regToVReg right |> Option.toList)
    | LIR.Lsl_imm (_, src, _) | LIR.Lsr_imm (_, src, _) | LIR.Asr_imm (_, src, _) | LIR.And_imm (_, src, _)
    | LIR.Neg (_, src) ->
        regToVReg src |> Option.toList
    | LIR.Msub (_, mulLeft, mulRight, sub) ->
        (regToVReg mulLeft |> Option.toList)
        @ (regToVReg mulRight |> Option.toList)
        @ (regToVReg sub |> Option.toList)
    | LIR.Madd (_, mulLeft, mulRight, add) ->
        (regToVReg mulLeft |> Option.toList)
        @ (regToVReg mulRight |> Option.toList)
        @ (regToVReg add |> Option.toList)
    | LIR.Cmp (left, right) ->
        (regToVReg left |> Option.toList) @ (operandToVReg right |> Option.toList)
    | LIR.Cset (_, _) -> []
    | LIR.Mvn (_, src) ->
        regToVReg src |> Option.toList
    | LIR.Sxtb (_, src) | LIR.Sxth (_, src) | LIR.Sxtw (_, src)
    | LIR.Uxtb (_, src) | LIR.Uxth (_, src) | LIR.Uxtw (_, src) ->
        regToVReg src |> Option.toList
    | LIR.Call (_, _, args) ->
        args |> List.choose operandToVReg
    | LIR.TailCall (_, args) ->
        args |> List.choose operandToVReg
    | LIR.IndirectCall (_, func, args) ->
        let funcVReg = regToVReg func |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        funcVReg @ argsVRegs
    | LIR.IndirectTailCall (func, args) ->
        let funcVReg = regToVReg func |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        funcVReg @ argsVRegs
    | LIR.ClosureAlloc (_, _, captures) ->
        captures |> List.choose operandToVReg
    | LIR.ClosureCall (_, closure, args) ->
        let closureVReg = regToVReg closure |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        closureVReg @ argsVRegs
    | LIR.ClosureTailCall (closure, args) ->
        let closureVReg = regToVReg closure |> Option.toList
        let argsVRegs = args |> List.choose operandToVReg
        closureVReg @ argsVRegs
    | LIR.PrintInt64 reg | LIR.PrintUInt64 reg | LIR.PrintBool reg
    | LIR.PrintInt64NoNewline reg | LIR.PrintUInt64NoNewline reg | LIR.PrintBoolNoNewline reg
    | LIR.PrintHeapStringNoNewline reg | LIR.RuntimeErrorString reg | LIR.PrintList (reg, _)
    | LIR.PrintSum (reg, _) | LIR.PrintRecord (reg, _, _) ->
        regToVReg reg |> Option.toList
    | LIR.PrintFloatNoNewline _ -> []  // FP register, not GP
    | LIR.PrintChars _ -> []  // No registers used
    | LIR.StdoutWrite (_, value, _) -> operandToVReg value |> Option.toList
    | LIR.StdinReadLine _ -> []
    | LIR.PrintBlob reg -> regToVReg reg |> Option.toList
    | LIR.HeapAlloc (_, _) -> []
    | LIR.HeapStore (addr, _, src, valueType) ->
        let a = regToVReg addr |> Option.toList
        // For float values, the src register is an FVirtual (handled by float allocation)
        // so we don't include it in integer liveness
        let s =
            match valueType with
            | Some AST.TFloat64 -> []
            | _ -> operandToVReg src |> Option.toList
        a @ s
    | LIR.HeapLoad (_, addr, _) ->
        regToVReg addr |> Option.toList
    | LIR.RefCountInc (addr, _, _, _) ->
        regToVReg addr |> Option.toList
    | LIR.RefCountDec (addr, _, _, _) ->
        regToVReg addr |> Option.toList
    | LIR.StringConcat (_, first, second, remaining) ->
        first :: second :: remaining |> List.collect (operandToVReg >> Option.toList)
    | LIR.CanonicalBufferEq (_, _, left, right) ->
        (operandToVReg left |> Option.toList) @ (operandToVReg right |> Option.toList)
    | LIR.PrintHeapString reg ->
        regToVReg reg |> Option.toList
    | LIR.FileReadText (_, path) ->
        operandToVReg path |> Option.toList
    | LIR.FileExists (_, path) ->
        operandToVReg path |> Option.toList
    | LIR.FileWriteText (_, path, content) ->
        (operandToVReg path |> Option.toList) @ (operandToVReg content |> Option.toList)
    | LIR.FileAppendText (_, path, content) ->
        (operandToVReg path |> Option.toList) @ (operandToVReg content |> Option.toList)
    | LIR.FileDelete (_, path) ->
        operandToVReg path |> Option.toList
    | LIR.FileSetExecutable (_, path) ->
        operandToVReg path |> Option.toList
    | LIR.FileWriteFromPtr (_, path, ptr, length) ->
        (operandToVReg path |> Option.toList)
        @ (regToVReg ptr |> Option.toList)
        @ (regToVReg length |> Option.toList)
    | LIR.RawAlloc (_, numBytes) ->
        regToVReg numBytes |> Option.toList
    | LIR.MappedAlloc (_, numBytes) ->
        regToVReg numBytes |> Option.toList
    | LIR.RawFree ptr ->
        regToVReg ptr |> Option.toList
    | LIR.MappedFree ptr ->
        regToVReg ptr |> Option.toList
    | LIR.RawGet (_, ptr, byteOffset) ->
        (regToVReg ptr |> Option.toList) @ (regToVReg byteOffset |> Option.toList)
    | LIR.RawGetByte (_, ptr, byteOffset) ->
        (regToVReg ptr |> Option.toList) @ (regToVReg byteOffset |> Option.toList)
    | LIR.RawWriteWord (ptr, byteOffset, value) ->
        (regToVReg ptr |> Option.toList)
        @ (regToVReg byteOffset |> Option.toList)
        @ (regToVReg value |> Option.toList)
    | LIR.RawWriteByte (ptr, byteOffset, value) ->
        (regToVReg ptr |> Option.toList)
        @ (regToVReg byteOffset |> Option.toList)
        @ (regToVReg value |> Option.toList)
    | LIR.RawSlotInit (ptr, byteOffset, value, _) ->
        (regToVReg ptr |> Option.toList)
        @ (regToVReg byteOffset |> Option.toList)
        @ (regToVReg value |> Option.toList)
    // Int64ToFloat uses an integer source register
    | LIR.Int64ToFloat (_, src) ->
        regToVReg src |> Option.toList
    | LIR.RefCountIncString str ->
        operandToVReg str |> Option.toList
    | LIR.RefCountDecString str ->
        operandToVReg str |> Option.toList
    | LIR.RefCountIncBlob bytes ->
        operandToVReg bytes |> Option.toList
    | LIR.RefCountDecBlob bytes ->
        operandToVReg bytes |> Option.toList
    | LIR.RefCountIncInt value
    | LIR.RefCountDecInt value ->
        operandToVReg value |> Option.toList
    | LIR.RandomInt64 _ ->
        []  // No operands to read
    | LIR.DateTimeNow _ ->
        []  // No operands to read
    | LIR.Sleep _ ->
        []  // Float delay is tracked by getUsedFVRegs
    | LIR.CliNative (_, _, args) ->
        args |> List.choose operandToVReg
    | LIR.FloatToString _ ->
        []  // Float value is in FP register, tracked by getUsedFVRegs
    // ArgMoves/TailArgMoves contain operands that use virtual registers
    | LIR.ArgMoves moves ->
        moves |> List.choose (fun (_, op) -> operandToVReg op)
    | LIR.TailArgMoves moves ->
        moves |> List.choose (fun (_, op) -> operandToVReg op)
    // Phi sources are NOT regular uses - they are used at predecessor exits, not at the phi's block
    // The liveness analysis handles phi sources specially in computeLivenessBitsRaw
    | LIR.Phi _ -> []
    | _ -> []

/// Get virtual register ID defined (written) by an instruction
let getDefinedVReg (instr: LIR.Instr) : int option =
    let regToVReg (reg: LIR.Reg) : int option =
        match reg with
        | LIR.Virtual id -> Some id
        | LIR.Physical _ -> None

    match instr with
    | LIR.Mov (dest, _) -> regToVReg dest
    | LIR.Add (dest, _, _) | LIR.Sub (dest, _, _) -> regToVReg dest
    | LIR.Mul (dest, _, _) | LIR.Sdiv (dest, _, _) | LIR.Udiv (dest, _, _) | LIR.Msub (dest, _, _, _) | LIR.Madd (dest, _, _, _) -> regToVReg dest
    | LIR.Cset (dest, _) -> regToVReg dest
    | LIR.And (dest, _, _) | LIR.And_imm (dest, _, _) | LIR.Orr (dest, _, _) | LIR.Eor (dest, _, _)
    | LIR.Lsl (dest, _, _) | LIR.Lsr (dest, _, _) | LIR.Asr (dest, _, _)
    | LIR.Lsl_imm (dest, _, _) | LIR.Lsr_imm (dest, _, _) | LIR.Asr_imm (dest, _, _) -> regToVReg dest
    | LIR.Neg (dest, _) | LIR.Mvn (dest, _) -> regToVReg dest
    | LIR.Sxtb (dest, _) | LIR.Sxth (dest, _) | LIR.Sxtw (dest, _)
    | LIR.Uxtb (dest, _) | LIR.Uxth (dest, _) | LIR.Uxtw (dest, _) -> regToVReg dest
    | LIR.Call (dest, _, _) -> regToVReg dest
    | LIR.TailCall _ -> None  // Tail calls don't return to caller
    | LIR.IndirectCall (dest, _, _) -> regToVReg dest
    | LIR.IndirectTailCall _ -> None  // Indirect tail calls don't return to caller
    | LIR.ClosureAlloc (dest, _, _) -> regToVReg dest
    | LIR.ClosureCall (dest, _, _) -> regToVReg dest
    | LIR.ClosureTailCall _ -> None  // Closure tail calls don't return to caller
    | LIR.HeapAlloc (dest, _) -> regToVReg dest
    | LIR.HeapLoad (dest, _, _) -> regToVReg dest
    | LIR.StringConcat (dest, _, _, _) -> regToVReg dest
    | LIR.CanonicalBufferEq (dest, _, _, _) -> regToVReg dest
    | LIR.StdinReadLine (_, dest) -> regToVReg dest
    | LIR.LoadFuncAddr (dest, _) -> regToVReg dest
    | LIR.FileReadText (dest, _) -> regToVReg dest
    | LIR.FileExists (dest, _) -> regToVReg dest
    | LIR.FileWriteText (dest, _, _) -> regToVReg dest
    | LIR.FileAppendText (dest, _, _) -> regToVReg dest
    | LIR.FileDelete (dest, _) -> regToVReg dest
    | LIR.FileSetExecutable (dest, _) -> regToVReg dest
    | LIR.FileWriteFromPtr (dest, _, _, _) -> regToVReg dest
    | LIR.RawAlloc (dest, _) -> regToVReg dest
    | LIR.MappedAlloc (dest, _) -> regToVReg dest
    | LIR.RawGet (dest, _, _) -> regToVReg dest
    | LIR.RawGetByte (dest, _, _) -> regToVReg dest
    | LIR.RawFree _ -> None
    | LIR.MappedFree _ -> None
    | LIR.RawWriteWord _ -> None
    | LIR.RawWriteByte _ -> None
    | LIR.RawSlotInit _ -> None
    // FloatToInt64 defines an integer destination register
    | LIR.FloatToInt64 (dest, _) -> regToVReg dest
    // FloatToBits defines an integer destination register
    | LIR.FloatToBits (dest, _) -> regToVReg dest
    // FpToGp defines an integer destination register
    | LIR.FpToGp (dest, _) -> regToVReg dest
    | LIR.RefCountIncString _ -> None
    | LIR.RefCountDecString _ -> None
    | LIR.RefCountIncBlob _ -> None
    | LIR.RefCountDecBlob _ -> None
    | LIR.RefCountIncInt _ -> None
    | LIR.RefCountDecInt _ -> None
    | LIR.RandomInt64 dest -> regToVReg dest
    | LIR.DateTimeNow dest -> regToVReg dest
    | LIR.CliNative (dest, _, _) -> regToVReg dest
    | LIR.FloatToString (dest, _) -> regToVReg dest
    // Phi defines its destination at block entry
    | LIR.Phi (dest, _, _) -> regToVReg dest
    | _ -> None

// ============================================================================
// Float Register Liveness Analysis
// ============================================================================

let private isFixedFVRegId (id: int) : bool =
    id = 1000 || id = 1001 || id = 2000 || (id >= 3000 && id < 4000)

/// Get FVirtual register IDs used (read) by an instruction
let getUsedFVRegs (instr: LIR.Instr) : int list =
    let fregToId (freg: LIR.FReg) : int option =
        match freg with
        | LIR.FVirtual id when isFixedFVRegId id -> None
        | LIR.FVirtual id -> Some id
        | LIR.FPhysical _ -> None

    match instr with
    | LIR.FMov (_, src) -> fregToId src |> Option.toList
    | LIR.FAdd (_, left, right) | LIR.FSub (_, left, right)
    | LIR.FMul (_, left, right) | LIR.FDiv (_, left, right) ->
        [fregToId left; fregToId right] |> List.choose id
    | LIR.FNeg (_, src) | LIR.FAbs (_, src) | LIR.FSqrt (_, src) ->
        fregToId src |> Option.toList
    | LIR.FCmp (left, right) ->
        [fregToId left; fregToId right] |> List.choose id
    | LIR.FloatToInt64 (_, src) -> fregToId src |> Option.toList
    | LIR.FloatToBits (_, src) -> fregToId src |> Option.toList
    | LIR.FpToGp (_, src) -> fregToId src |> Option.toList
    | LIR.PrintFloat freg | LIR.PrintFloatNoNewline freg ->
        fregToId freg |> Option.toList
    | LIR.FArgMoves moves ->
        moves |> List.choose (fun (_, src) -> fregToId src)
    | LIR.FPhi _ -> []  // Phi sources handled specially
    | LIR.FloatToString (_, value) -> fregToId value |> Option.toList
    | LIR.Sleep (_, delayMs) -> fregToId delayMs |> Option.toList
    // HeapStore with float value: the Virtual register ID is shared with FVirtual
    | LIR.HeapStore (_, _, LIR.Reg (LIR.Virtual vregId), Some AST.TFloat64) -> [vregId]
    | _ -> []

/// Get FVirtual register ID defined (written) by an instruction
let getDefinedFVReg (instr: LIR.Instr) : int option =
    let fregToId (freg: LIR.FReg) : int option =
        match freg with
        | LIR.FVirtual id when isFixedFVRegId id -> None
        | LIR.FVirtual id -> Some id
        | LIR.FPhysical _ -> None

    match instr with
    | LIR.FMov (dest, _) -> fregToId dest
    | LIR.FAdd (dest, _, _) | LIR.FSub (dest, _, _)
    | LIR.FMul (dest, _, _) | LIR.FDiv (dest, _, _) -> fregToId dest
    | LIR.FNeg (dest, _) | LIR.FAbs (dest, _) | LIR.FSqrt (dest, _) -> fregToId dest
    | LIR.FLoad (dest, _) -> fregToId dest
    | LIR.Int64ToFloat (dest, _) -> fregToId dest
    | LIR.GpToFp (dest, _) -> fregToId dest
    | LIR.FPhi (dest, _) -> fregToId dest
    | _ -> None

/// Get virtual register used by terminator
let getTerminatorUsedVRegs (term: LIR.Terminator) : int list =
    match term with
    | LIR.Branch (LIR.Virtual id, _, _) -> [id]
    | LIR.BranchZero (LIR.Virtual id, _, _) -> [id]
    | LIR.BranchBitZero (LIR.Virtual id, _, _, _) -> [id]
    | LIR.BranchBitNonZero (LIR.Virtual id, _, _, _) -> [id]
    | LIR.CondBranch _ -> []  // CondBranch uses condition flags, not a register
    | _ -> []

let private classifyInstr (instr: LIR.Instr) : InstrRegisterFacts =
    let intPhiUses =
        match instr with
        | LIR.Phi (_, sources, _) ->
            sources
            |> List.choose (fun (source, predecessor) ->
                match source with
                | LIR.Reg (LIR.Virtual id) -> Some (id, predecessor)
                | _ -> None)
        | _ -> []
    let floatPhiUses =
        match instr with
        | LIR.FPhi (_, sources) ->
            sources
            |> List.choose (fun (source, predecessor) ->
                match source with
                | LIR.FVirtual id -> Some (id, predecessor)
                | LIR.FPhysical _ -> None)
        | _ -> []
    {
        Instr = instr
        IntUses = getUsedVRegs instr
        IntDef = getDefinedVReg instr
        IntPhiUses = intPhiUses
        FloatUses = getUsedFVRegs instr
        FloatDef = getDefinedFVReg instr
        FloatPhiUses = floatPhiUses
    }

let internal classifyBlocks (blocks: LIR.BasicBlock array) : ClassifiedBlock array =
    blocks
    |> Array.map (fun block ->
        let mutable hasPhiNodes = false
        let instrFacts =
            block.Instrs
            |> List.map (fun instr ->
                match instr with
                | LIR.Phi _
                | LIR.FPhi _ -> hasPhiNodes <- true
                | _ -> ()
                classifyInstr instr)
            |> List.toArray
        {
            Block = block
            InstrFacts = instrFacts
            TerminatorUses = getTerminatorUsedVRegs block.Terminator
            HasPhiNodes = hasPhiNodes
        })
