// Printer.fs - Format LIR instructions and scoped or summarized dumps.

module LIRPrinter

open MemoryModel
open ANF
open MIR
open LIR
open IRPrinting
open ANFPrinter

/// Pretty-print LIR physical register
let private prettyPrintLIRPhysReg = function
    | LIR.X0 -> "X0" | LIR.X1 -> "X1" | LIR.X2 -> "X2" | LIR.X3 -> "X3"
    | LIR.X4 -> "X4" | LIR.X5 -> "X5" | LIR.X6 -> "X6" | LIR.X7 -> "X7"
    | LIR.X8 -> "X8" | LIR.X9 -> "X9" | LIR.X10 -> "X10" | LIR.X11 -> "X11"
    | LIR.X12 -> "X12" | LIR.X13 -> "X13" | LIR.X14 -> "X14" | LIR.X15 -> "X15"
    | LIR.X16 -> "X16" | LIR.X17 -> "X17"
    | LIR.X19 -> "X19" | LIR.X20 -> "X20" | LIR.X21 -> "X21" | LIR.X22 -> "X22"
    | LIR.X23 -> "X23" | LIR.X24 -> "X24" | LIR.X25 -> "X25" | LIR.X26 -> "X26"
    | LIR.X27 -> "X27"
    | LIR.X29 -> "X29" | LIR.X30 -> "X30" | LIR.SP -> "SP"

/// Pretty-print LIR register
let private prettyPrintLIRReg = function
    | LIR.Physical pr -> prettyPrintLIRPhysReg pr
    | LIR.Virtual n -> $"v{n}"

/// Pretty-print LIR FP physical register
let private prettyPrintLIRPhysFPReg = function
    | LIR.D0 -> "D0" | LIR.D1 -> "D1" | LIR.D2 -> "D2" | LIR.D3 -> "D3"
    | LIR.D4 -> "D4" | LIR.D5 -> "D5" | LIR.D6 -> "D6" | LIR.D7 -> "D7"
    | LIR.D8 -> "D8" | LIR.D9 -> "D9" | LIR.D10 -> "D10" | LIR.D11 -> "D11"
    | LIR.D12 -> "D12" | LIR.D13 -> "D13" | LIR.D14 -> "D14" | LIR.D15 -> "D15"

/// Pretty-print LIR FP register
let private prettyPrintLIRFReg = function
    | LIR.FPhysical pr -> prettyPrintLIRPhysFPReg pr
    | LIR.FVirtual n -> $"fv{n}"

/// Pretty-print LIR operand
let private prettyPrintLIROperand = function
    | LIR.Imm n -> $"Imm {n}"
    | LIR.FloatImm f -> $"FloatImm {f}"
    | LIR.Reg reg -> $"Reg {prettyPrintLIRReg reg}"
    | LIR.StackSlot n -> $"Stack {n}"
    | LIR.StringSymbol value -> $"str[{escapeStringContent value}]"
    | LIR.FloatSymbol value -> $"float[{value}]"
    | LIR.FuncAddr name -> $"&{name}"

let private prettyPrintLIRRcKind = function
    | LIR.GenericHeap -> "generic"
    | LIR.StreamHeap -> "stream"
    | LIR.TaggedList -> "list"
    | LIR.DictHeap -> "dict"
    | LIR.ClosureHeap -> "closure"

/// Pretty-print LIR instruction
let private prettyPrintLIRInstr (instr: LIR.Instr) : string =
    match instr with
    | LIR.Mov (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Mov({prettyPrintLIROperand src})"
    | LIR.Phi (dest, sources, _) ->
        let srcs = sources |> commaSeparated (fun (op, LIR.Label lbl) -> $"({prettyPrintLIROperand op}, {lbl})")
        $"{prettyPrintLIRReg dest} <- Phi([{srcs}])"
    | LIR.FPhi (dest, sources) ->
        let srcs = sources |> commaSeparated (fun (freg, LIR.Label lbl) -> $"({prettyPrintLIRFReg freg}, {lbl})")
        $"{prettyPrintLIRFReg dest} <- FPhi([{srcs}])"
    | LIR.Store (offset, src) ->
        $"Store(Stack {offset}, {prettyPrintLIRReg src})"
    | LIR.Add (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Add({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Sub (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Sub({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Mul (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Mul({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Sdiv (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Sdiv({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Udiv (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Udiv({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        $"{prettyPrintLIRReg dest} <- Msub({prettyPrintLIRReg mulLeft}, {prettyPrintLIRReg mulRight}, {prettyPrintLIRReg sub})"
    | LIR.Madd (dest, mulLeft, mulRight, add) ->
        $"{prettyPrintLIRReg dest} <- Madd({prettyPrintLIRReg mulLeft}, {prettyPrintLIRReg mulRight}, {prettyPrintLIRReg add})"
    | LIR.Select (dest, whenTrue, whenFalse, cond) ->
        $"{prettyPrintLIRReg dest} <- Select({cond}, {prettyPrintLIRReg whenTrue}, {prettyPrintLIRReg whenFalse})"
    | LIR.Cmp (left, right) ->
        $"Cmp({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Cset (dest, cond) ->
        $"{prettyPrintLIRReg dest} <- Cset({cond})"
    | LIR.And (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- And({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.And_imm (dest, src, imm) ->
        $"{prettyPrintLIRReg dest} <- And_imm({prettyPrintLIRReg src}, #{imm})"
    | LIR.Orr (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Orr({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.Eor (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Eor({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.Lsl (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsl({prettyPrintLIRReg src}, {prettyPrintLIRReg shift})"
    | LIR.Lsr (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsr({prettyPrintLIRReg src}, {prettyPrintLIRReg shift})"
    | LIR.Asr (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Asr({prettyPrintLIRReg src}, {prettyPrintLIRReg shift})"
    | LIR.Lsl_imm (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsl_imm({prettyPrintLIRReg src}, #{shift})"
    | LIR.Lsr_imm (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsr_imm({prettyPrintLIRReg src}, #{shift})"
    | LIR.Asr_imm (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Asr_imm({prettyPrintLIRReg src}, #{shift})"

    | LIR.Neg (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Neg({prettyPrintLIRReg src})"
    | LIR.Mvn (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Mvn({prettyPrintLIRReg src})"
    | LIR.Sxtb (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Sxtb({prettyPrintLIRReg src})"
    | LIR.Sxth (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Sxth({prettyPrintLIRReg src})"
    | LIR.Sxtw (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Sxtw({prettyPrintLIRReg src})"
    | LIR.Uxtb (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Uxtb({prettyPrintLIRReg src})"
    | LIR.Uxth (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Uxth({prettyPrintLIRReg src})"
    | LIR.Uxtw (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Uxtw({prettyPrintLIRReg src})"
    | LIR.Call (dest, funcName, args) ->
        let argStr = args |> commaSeparated prettyPrintLIROperand
        $"{prettyPrintLIRReg dest} <- Call({funcName}, [{argStr}])"
    | LIR.TailCall (funcName, args) ->
        let argStr = args |> commaSeparated prettyPrintLIROperand
        $"TailCall({funcName}, [{argStr}])"
    | LIR.IndirectCall (dest, func, args) ->
        let argStr = args |> commaSeparated prettyPrintLIROperand
        $"{prettyPrintLIRReg dest} <- IndirectCall({prettyPrintLIRReg func}, [{argStr}])"
    | LIR.IndirectTailCall (func, args) ->
        let argStr = args |> commaSeparated prettyPrintLIROperand
        $"IndirectTailCall({prettyPrintLIRReg func}, [{argStr}])"
    | LIR.ClosureAlloc (dest, funcName, captures) ->
        let capsStr = captures |> commaSeparated prettyPrintLIROperand
        $"{prettyPrintLIRReg dest} <- ClosureAlloc({funcName}, [{capsStr}])"
    | LIR.ClosureCall (dest, closure, args) ->
        let argStr = args |> commaSeparated prettyPrintLIROperand
        $"{prettyPrintLIRReg dest} <- ClosureCall({prettyPrintLIRReg closure}, [{argStr}])"
    | LIR.ClosureTailCall (closure, args) ->
        let argStr = args |> commaSeparated prettyPrintLIROperand
        $"ClosureTailCall({prettyPrintLIRReg closure}, [{argStr}])"
    | LIR.SaveRegs (intRegs, floatRegs) ->
        let intStr = intRegs |> List.map string |> String.concat ", "
        let floatStr = floatRegs |> List.map string |> String.concat ", "
        $"SaveRegs([{intStr}], [{floatStr}])"
    | LIR.RestoreRegs (intRegs, floatRegs) ->
        let intStr = intRegs |> List.map string |> String.concat ", "
        let floatStr = floatRegs |> List.map string |> String.concat ", "
        $"RestoreRegs([{intStr}], [{floatStr}])"
    | LIR.ArgMoves moves ->
        let moveStrs = moves |> List.map (fun (dest, src) -> $"{dest} <- {prettyPrintLIROperand src}")
        let movesText = String.concat ", " moveStrs
        $"ArgMoves({movesText})"
    | LIR.TailArgMoves moves ->
        let moveStrs = moves |> List.map (fun (dest, src) -> $"{dest} <- {prettyPrintLIROperand src}")
        let movesText = String.concat ", " moveStrs
        $"TailArgMoves({movesText})"
    | LIR.FArgMoves moves ->
        let moveStrs = moves |> List.map (fun (dest, src) -> $"{dest} <- {prettyPrintLIRFReg src}")
        let movesText = String.concat ", " moveStrs
        $"FArgMoves({movesText})"
    | LIR.PrintInt64 reg ->
        $"PrintInt64({prettyPrintLIRReg reg})"
    | LIR.PrintUInt64 reg ->
        $"PrintUInt64({prettyPrintLIRReg reg})"
    | LIR.PrintBool reg ->
        $"PrintBool({prettyPrintLIRReg reg})"
    | LIR.PrintFloat freg ->
        $"PrintFloat({prettyPrintLIRFReg freg})"
    | LIR.PrintString value ->
        $"PrintString(str[{escapeStringContent value}], len={value.Length})"
    | LIR.StdoutWrite (_, value, appendNewline) ->
        $"StdoutWrite({prettyPrintLIROperand value}, newline={appendNewline})"
    | LIR.StdinReadLine (_, dest) ->
        $"{prettyPrintLIRReg dest} <- StdinReadLine()"
    | LIR.RuntimeError message ->
        $"RuntimeError(\"{escapeStringContent message}\")"
    | LIR.RuntimeErrorString reg ->
        $"RuntimeErrorString({prettyPrintLIRReg reg})"
    | LIR.PrintChars chars ->
        let s = chars |> List.map (fun b -> char b) |> System.String.Concat
        $"PrintChars(\"{escapeStringContent s}\")"
    | LIR.PrintBlob reg ->
        $"PrintBlob({prettyPrintLIRReg reg})"
    | LIR.PrintInt64NoNewline reg ->
        $"PrintIntNoNewline({prettyPrintLIRReg reg})"
    | LIR.PrintUInt64NoNewline reg ->
        $"PrintUInt64NoNewline({prettyPrintLIRReg reg})"
    | LIR.PrintBoolNoNewline reg ->
        $"PrintBoolNoNewline({prettyPrintLIRReg reg})"
    | LIR.PrintFloatNoNewline freg ->
        $"PrintFloatNoNewline({prettyPrintLIRFReg freg})"
    | LIR.PrintHeapStringNoNewline reg ->
        $"PrintHeapStringNoNewline({prettyPrintLIRReg reg})"
    | LIR.PrintList (listPtr, elemType) ->
        $"PrintList({prettyPrintLIRReg listPtr}, {elemType})"
    | LIR.PrintSum (sumPtr, variants) ->
        $"PrintSum({prettyPrintLIRReg sumPtr}, {variants})"
    | LIR.PrintRecord (recordPtr, typeName, fields) ->
        $"PrintRecord({prettyPrintLIRReg recordPtr}, {typeName}, {fields})"
    | LIR.Exit -> "Exit"
    | LIR.FMov (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FMov({prettyPrintLIRFReg src})"
    | LIR.FLoad (dest, value) ->
        $"{prettyPrintLIRFReg dest} <- FLoad(float[{value}])"
    | LIR.FSpillLoad (dest, stackSlot) ->
        $"{prettyPrintLIRFReg dest} <- FSpillLoad(Stack {stackSlot})"
    | LIR.FSpillStore (stackSlot, src) ->
        $"FSpillStore(Stack {stackSlot}, {prettyPrintLIRFReg src})"
    | LIR.FAdd (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FAdd({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FSub (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FSub({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FMul (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FMul({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FMadd (dest, left, right, addend) ->
        $"{prettyPrintLIRFReg dest} <- FMadd({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right}, {prettyPrintLIRFReg addend})"
    | LIR.FDiv (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FDiv({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FNeg (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FNeg({prettyPrintLIRFReg src})"
    | LIR.FAbs (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FAbs({prettyPrintLIRFReg src})"
    | LIR.FSqrt (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FSqrt({prettyPrintLIRFReg src})"
    | LIR.FCmp (left, right) ->
        $"FCmp({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.Int64ToFloat (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- Int64ToFloat({prettyPrintLIRReg src})"
    | LIR.FloatToInt64 (dest, src) ->
        $"{prettyPrintLIRReg dest} <- FloatToInt64({prettyPrintLIRFReg src})"
    | LIR.FloatToBits (dest, src) ->
        $"{prettyPrintLIRReg dest} <- FloatToBits({prettyPrintLIRFReg src})"
    | LIR.GpToFp (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- GpToFp({prettyPrintLIRReg src})"
    | LIR.FpToGp (dest, src) ->
        $"{prettyPrintLIRReg dest} <- FpToGp({prettyPrintLIRFReg src})"
    | LIR.HeapAlloc (dest, sizeBytes) ->
        $"{prettyPrintLIRReg dest} <- HeapAlloc({sizeBytes})"
    | LIR.HeapStore (addr, offset, src, _valueType) ->
        $"HeapStore({prettyPrintLIRReg addr}, {offset}, {prettyPrintLIROperand src})"
    | LIR.HeapLoad (dest, addr, offset) ->
        $"{prettyPrintLIRReg dest} <- HeapLoad({prettyPrintLIRReg addr}, {offset})"
    | LIR.RefCountInc (addr, payloadSize, kind, _) ->
        $"RefCountInc({prettyPrintLIRReg addr}, {payloadSize}, {prettyPrintLIRRcKind kind})"
    | LIR.RefCountDec (addr, payloadSize, kind, _) ->
        $"RefCountDec({prettyPrintLIRReg addr}, {payloadSize}, {prettyPrintLIRRcKind kind})"
    | LIR.StringConcat (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- StringConcat({prettyPrintLIROperand left}, {prettyPrintLIROperand right})"
    | LIR.CanonicalBufferEq (dest, kind, left, right) ->
        $"{prettyPrintLIRReg dest} <- CanonicalBufferEq[{prettyPrintCanonicalBufferKind kind}]({prettyPrintLIROperand left}, {prettyPrintLIROperand right})"
    | LIR.PrintHeapString reg ->
        $"PrintHeapString({prettyPrintLIRReg reg})"
    | LIR.LoadFuncAddr (dest, funcName) ->
        $"{prettyPrintLIRReg dest} <- LoadFuncAddr({funcName})"
    | LIR.FileReadText (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileReadText({prettyPrintLIROperand path})"
    | LIR.FileExists (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileExists({prettyPrintLIROperand path})"
    | LIR.FileWriteText (dest, path, content) ->
        $"{prettyPrintLIRReg dest} <- FileWriteText({prettyPrintLIROperand path}, {prettyPrintLIROperand content})"
    | LIR.FileAppendText (dest, path, content) ->
        $"{prettyPrintLIRReg dest} <- FileAppendText({prettyPrintLIROperand path}, {prettyPrintLIROperand content})"
    | LIR.FileDelete (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileDelete({prettyPrintLIROperand path})"
    | LIR.FileSetExecutable (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileSetExecutable({prettyPrintLIROperand path})"
    | LIR.FileWriteFromPtr (dest, path, ptr, length) ->
        $"{prettyPrintLIRReg dest} <- FileWriteFromPtr({prettyPrintLIROperand path}, {prettyPrintLIRReg ptr}, {prettyPrintLIRReg length})"
    | LIR.RawAlloc (dest, numBytes) ->
        $"{prettyPrintLIRReg dest} <- RawAlloc({prettyPrintLIRReg numBytes})"
    | LIR.MappedAlloc (dest, numBytes) ->
        $"{prettyPrintLIRReg dest} <- MappedAlloc({prettyPrintLIRReg numBytes})"
    | LIR.RawFree ptr ->
        $"RawFree({prettyPrintLIRReg ptr})"
    | LIR.MappedFree ptr ->
        $"MappedFree({prettyPrintLIRReg ptr})"
    | LIR.RawGet (dest, ptr, byteOffset) ->
        $"{prettyPrintLIRReg dest} <- RawGet({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset})"
    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        $"{prettyPrintLIRReg dest} <- RawGetByte({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset})"
    | LIR.RawWriteWord (ptr, byteOffset, value) ->
        $"RawWriteWord({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset}, {prettyPrintLIRReg value})"
    | LIR.RawWriteByte (ptr, byteOffset, value) ->
        $"RawWriteByte({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset}, {prettyPrintLIRReg value})"
    | LIR.RawSlotInit (ptr, byteOffset, value, valueType) ->
        $"RawSlotInit({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset}, {prettyPrintLIRReg value}) : {valueType}"
    | LIR.RefCountIncString str ->
        $"RefCountIncString({prettyPrintLIROperand str})"
    | LIR.RefCountDecString str ->
        $"RefCountDecString({prettyPrintLIROperand str})"
    | LIR.RefCountIncBlob bytes ->
        $"RefCountIncBlob({prettyPrintLIROperand bytes})"
    | LIR.RefCountDecBlob bytes ->
        $"RefCountDecBlob({prettyPrintLIROperand bytes})"
    | LIR.RandomInt64 dest ->
        $"{prettyPrintLIRReg dest} <- RandomInt64()"
    | LIR.DateTimeNow dest ->
        $"{prettyPrintLIRReg dest} <- DateTimeNow()"
    | LIR.Sleep (effectId, delayMs) ->
        $"Sleep#{effectId}({prettyPrintLIRFReg delayMs})"
    | LIR.CliNative (dest, operation, args) ->
        let argText = args |> commaSeparated prettyPrintLIROperand
        $"{prettyPrintLIRReg dest} <- CliNative.{operation}({argText})"
    | LIR.FloatToString (dest, value) ->
        $"{prettyPrintLIRReg dest} <- FloatToString({prettyPrintLIRFReg value})"
    | LIR.CoverageHit exprId ->
        $"CoverageHit({exprId})"

/// Pretty-print symbolic LIR terminator
let private prettyPrintLIRTerminator (term: LIR.Terminator) : string =
    match term with
    | LIR.Ret -> "Ret"
    | LIR.Branch (cond, trueLabel, falseLabel) ->
        $"Branch({prettyPrintLIRReg cond}, {trueLabel}, {falseLabel})"
    | LIR.BranchZero (cond, zeroLabel, nonZeroLabel) ->
        $"BranchZero({prettyPrintLIRReg cond}, {zeroLabel}, {nonZeroLabel})"
    | LIR.BranchBitZero (reg, bit, zeroLabel, nonZeroLabel) ->
        $"BranchBitZero({prettyPrintLIRReg reg}, #{bit}, {zeroLabel}, {nonZeroLabel})"
    | LIR.BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel) ->
        $"BranchBitNonZero({prettyPrintLIRReg reg}, #{bit}, {nonZeroLabel}, {zeroLabel})"
    | LIR.CondBranch (cond, trueLabel, falseLabel) ->
        $"CondBranch({cond}, {trueLabel}, {falseLabel})"
    | LIR.Jump label -> $"Jump({label})"

/// Format symbolic LIR program with CFG structure
let formatLIR (LIR.Program (functions, _, _)) : string =
    let prettyPrintCalleeSaved (regs: LIR.PhysReg list) : string =
        regs
        |> List.map prettyPrintLIRPhysReg
        |> String.concat ", "

    let funcStrs =
        functions
        |> List.map (fun func ->
            let blockStrs =
                func.CFG.Blocks
                |> Map.toList
                |> List.sortBy fst
                |> List.map (fun (label, block) ->
                    let instrStrs =
                        block.Instrs
                        |> List.map prettyPrintLIRInstr
                        |> List.map (fun line -> $"    {line}")
                        |> String.concat "\n"
                    let termStr = $"    {prettyPrintLIRTerminator block.Terminator}"
                    $"  {label}:\n{instrStrs}\n{termStr}")
                |> String.concat "\n"
            let calleeSavedText = prettyPrintCalleeSaved func.UsedCalleeSaved
            $"{func.Name}:\n  StackSize: {func.StackSize}\n  UsedCalleeSaved: [{calleeSavedText}]\n{blockStrs}")
        |> String.concat "\n\n"
    funcStrs

/// Format only matching LIR functions, optionally as block/instruction counts.
let formatLIRDump (filter: string option) (summary: bool) (LIR.Program (functions, variants, records)) : string =
    let selected = functions |> List.filter (fun func -> functionNameMatches filter func.Name)
    match selected, summary with
    | [], _ when Option.isSome filter -> noFunctionMatchText filter
    | _, true ->
        let functionLines =
            selected
            |> List.map (fun func ->
                let blockCount = Map.count func.CFG.Blocks
                let instructionCount =
                    func.CFG.Blocks
                    |> Map.toList
                    |> List.sumBy (fun (_, block) -> List.length block.Instrs)
                $"{func.Name}: {blockCount} blocks, {instructionCount} instructions")
        String.concat "\n" ($"Functions: {List.length selected}" :: functionLines)
    | _, false -> formatLIR (LIR.Program (selected, variants, records))
