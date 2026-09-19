// Instructions.fs - Exhaustively dispatch LIR operations to typed arm64 emitters.

module ARM64Instructions

open ARM64CodeGenTypes

let rec convertInstr (ctx: CodeGenContext) (instr: LIR.Instr) : Result<ARM64Symbolic.Instr list, string> =
    match instr with
    | LIR.Phi _ ->
        ARM64EmitInteger.emitPhi ctx

    | LIR.FPhi _ ->
        ARM64EmitFloatingPoint.emitFPhi ctx

    | LIR.Mov (dest, src) ->
        ARM64EmitInteger.emitMov ctx dest src

    | LIR.Store (offset, src) ->
        ARM64EmitInteger.emitStore ctx offset src

    | LIR.Add (dest, left, right) ->
        ARM64EmitInteger.emitAdd ctx dest left right

    | LIR.Sub (dest, left, right) ->
        ARM64EmitInteger.emitSub ctx dest left right

    | LIR.Mul (dest, left, right) ->
        ARM64EmitInteger.emitMul ctx dest left right

    | LIR.Sdiv (dest, left, right) ->
        ARM64EmitInteger.emitSdiv ctx dest left right

    | LIR.Udiv (dest, left, right) ->
        ARM64EmitInteger.emitUdiv ctx dest left right

    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        ARM64EmitInteger.emitMsub ctx dest mulLeft mulRight sub

    | LIR.Madd (dest, mulLeft, mulRight, add) ->
        ARM64EmitInteger.emitMadd ctx dest mulLeft mulRight add

    | LIR.Cmp (left, right) ->
        ARM64EmitInteger.emitCmp ctx left right

    | LIR.Cset (dest, cond) ->
        ARM64EmitInteger.emitCset ctx dest cond

    | LIR.And (dest, left, right) ->
        ARM64EmitInteger.emitAnd ctx dest left right

    | LIR.And_imm (dest, src, imm) ->
        ARM64EmitInteger.emitAnd_imm ctx dest src imm

    | LIR.Orr (dest, left, right) ->
        ARM64EmitInteger.emitOrr ctx dest left right

    | LIR.Eor (dest, left, right) ->
        ARM64EmitInteger.emitEor ctx dest left right

    | LIR.Lsl (dest, src, shift) ->
        ARM64EmitInteger.emitLsl ctx dest src shift

    | LIR.Lsr (dest, src, shift) ->
        ARM64EmitInteger.emitLsr ctx dest src shift

    | LIR.Asr (dest, src, shift) ->
        ARM64EmitInteger.emitAsr ctx dest src shift

    | LIR.Lsl_imm (dest, src, shift) ->
        ARM64EmitInteger.emitLsl_imm ctx dest src shift

    | LIR.Lsr_imm (dest, src, shift) ->
        ARM64EmitInteger.emitLsr_imm ctx dest src shift

    | LIR.Asr_imm (dest, src, shift) ->
        ARM64EmitInteger.emitAsr_imm ctx dest src shift

    | LIR.Neg (dest, src) ->
        ARM64EmitInteger.emitNeg ctx dest src

    | LIR.Mvn (dest, src) ->
        ARM64EmitInteger.emitMvn ctx dest src

    | LIR.Sxtb (dest, src) ->
        ARM64EmitInteger.emitSxtb ctx dest src

    | LIR.Sxth (dest, src) ->
        ARM64EmitInteger.emitSxth ctx dest src

    | LIR.Sxtw (dest, src) ->
        ARM64EmitInteger.emitSxtw ctx dest src

    | LIR.Uxtb (dest, src) ->
        ARM64EmitInteger.emitUxtb ctx dest src

    | LIR.Uxth (dest, src) ->
        ARM64EmitInteger.emitUxth ctx dest src

    | LIR.Uxtw (dest, src) ->
        ARM64EmitInteger.emitUxtw ctx dest src

    | LIR.PrintBool reg ->
        ARM64EmitPrinting.emitPrintBool ctx reg

    | LIR.PrintChars chars ->
        ARM64EmitPrinting.emitPrintChars ctx chars

    | LIR.PrintBlob reg ->
        ARM64EmitPrinting.emitPrintBlob ctx reg

    | LIR.PrintInt64NoNewline reg ->
        ARM64EmitPrinting.emitPrintInt64NoNewline ctx reg

    | LIR.PrintUInt64NoNewline reg ->
        ARM64EmitPrinting.emitPrintUInt64NoNewline ctx reg

    | LIR.PrintBoolNoNewline reg ->
        ARM64EmitPrinting.emitPrintBoolNoNewline ctx reg

    | LIR.PrintFloatNoNewline freg ->
        ARM64EmitPrinting.emitPrintFloatNoNewline ctx freg

    | LIR.PrintHeapStringNoNewline reg ->
        ARM64EmitPrinting.emitPrintHeapStringNoNewline ctx reg

    | LIR.PrintList (listPtr, elemType) ->
        ARM64EmitPrinting.emitPrintList ctx listPtr elemType

    | LIR.PrintSum (sumPtr, variants) ->
        ARM64EmitPrinting.emitPrintSum ctx convertInstr sumPtr variants

    | LIR.PrintRecord (recordPtr, typeName, fields) ->
        ARM64EmitPrinting.emitPrintRecord ctx recordPtr typeName fields

    | LIR.Call (dest, funcName, args) ->
        ARM64EmitCalls.emitCall ctx dest funcName args

    | LIR.TailCall (funcName, args) ->
        ARM64EmitCalls.emitTailCall ctx funcName args

    | LIR.IndirectCall (dest, func, args) ->
        ARM64EmitCalls.emitIndirectCall ctx dest func args

    | LIR.IndirectTailCall (func, args) ->
        ARM64EmitCalls.emitIndirectTailCall ctx func args

    | LIR.ClosureAlloc (dest, funcName, captures) ->
        ARM64EmitInteger.emitClosureAlloc ctx dest funcName captures

    | LIR.ClosureCall (dest, funcPtr, args) ->
        ARM64EmitCalls.emitClosureCall ctx dest funcPtr args

    | LIR.ClosureTailCall (funcPtr, args) ->
        ARM64EmitCalls.emitClosureTailCall ctx funcPtr args

    | LIR.SaveRegs (intRegs, floatRegs) ->
        ARM64EmitCalls.emitSaveRegs ctx intRegs floatRegs

    | LIR.RestoreRegs (intRegs, floatRegs) ->
        ARM64EmitCalls.emitRestoreRegs ctx intRegs floatRegs

    | LIR.ArgMoves moves ->
        ARM64EmitInteger.emitArgMoves ctx moves

    | LIR.TailArgMoves moves ->
        ARM64EmitInteger.emitTailArgMoves ctx moves

    | LIR.FArgMoves moves ->
        ARM64EmitFloatingPoint.emitFArgMoves ctx moves

    | LIR.PrintInt64 reg ->
        ARM64EmitPrinting.emitPrintInt64 ctx reg

    | LIR.PrintUInt64 reg ->
        ARM64EmitPrinting.emitPrintUInt64 ctx reg

    | LIR.Exit ->
        ARM64EmitInteger.emitExit ctx

    | LIR.PrintFloat freg ->
        ARM64EmitPrinting.emitPrintFloat ctx freg

    | LIR.PrintString value ->
        ARM64EmitPrinting.emitPrintString ctx value

    | LIR.StdoutWrite (effectId, value, appendNewline) ->
        ARM64EmitInteger.emitStdoutWrite ctx effectId value appendNewline

    | LIR.StdinReadLine (effectId, dest) ->
        ARM64EmitInteger.emitStdinReadLine ctx effectId dest

    | LIR.RuntimeError message ->
        ARM64EmitInteger.emitRuntimeError ctx message

    | LIR.RuntimeErrorString messageReg ->
        ARM64EmitInteger.emitRuntimeErrorString ctx messageReg

    | LIR.FMov (dest, src) ->
        ARM64EmitFloatingPoint.emitFMov ctx dest src

    | LIR.FLoad (dest, value) ->
        ARM64EmitFloatingPoint.emitFLoad ctx dest value

    | LIR.FSpillLoad (dest, stackSlot) ->
        ARM64EmitFloatingPoint.emitFSpillLoad ctx dest stackSlot

    | LIR.FSpillStore (stackSlot, src) ->
        ARM64EmitFloatingPoint.emitFSpillStore ctx stackSlot src

    | LIR.FAdd (dest, left, right) ->
        ARM64EmitFloatingPoint.emitFAdd ctx dest left right

    | LIR.FSub (dest, left, right) ->
        ARM64EmitFloatingPoint.emitFSub ctx dest left right

    | LIR.FMul (dest, left, right) ->
        ARM64EmitFloatingPoint.emitFMul ctx dest left right

    | LIR.FDiv (dest, left, right) ->
        ARM64EmitFloatingPoint.emitFDiv ctx dest left right

    | LIR.FNeg (dest, src) ->
        ARM64EmitFloatingPoint.emitFNeg ctx dest src

    | LIR.FAbs (dest, src) ->
        ARM64EmitFloatingPoint.emitFAbs ctx dest src

    | LIR.FSqrt (dest, src) ->
        ARM64EmitFloatingPoint.emitFSqrt ctx dest src

    | LIR.FCmp (left, right) ->
        ARM64EmitFloatingPoint.emitFCmp ctx left right

    | LIR.Int64ToFloat (dest, src) ->
        ARM64EmitInteger.emitInt64ToFloat ctx dest src

    | LIR.FloatToInt64 (dest, src) ->
        ARM64EmitFloatingPoint.emitFloatToInt64 ctx dest src

    | LIR.GpToFp (dest, src) ->
        ARM64EmitInteger.emitGpToFp ctx dest src

    | LIR.FpToGp (dest, src) ->
        ARM64EmitFloatingPoint.emitFpToGp ctx dest src

    | LIR.FloatToBits (dest, src) ->
        ARM64EmitFloatingPoint.emitFloatToBits ctx dest src

    | LIR.HeapAlloc (dest, sizeBytes) ->
        ARM64EmitMemory.emitHeapAlloc ctx dest sizeBytes

    | LIR.HeapStore (addr, offset, src, valueType) ->
        ARM64EmitMemory.emitHeapStore ctx addr offset src valueType

    | LIR.HeapLoad (dest, addr, offset) ->
        ARM64EmitMemory.emitHeapLoad ctx dest addr offset

    | LIR.RefCountInc (addr, payloadSize, kind, _) ->
        ARM64EmitReferenceCounts.emitRefCountInc ctx addr payloadSize kind

    | LIR.RefCountDec (addr, payloadSize, kind, metadata) ->
        ARM64EmitReferenceCounts.emitRefCountDec ctx addr payloadSize kind metadata

    | LIR.CanonicalBufferEq (dest, _, left, right) ->
        ARM64EmitBuffers.emitCanonicalBufferEq ctx dest left right

    | LIR.StringConcat (dest, first, second, remaining) ->
        ARM64EmitBuffers.emitStringConcat ctx dest first second remaining

    | LIR.PrintHeapString reg ->
        ARM64EmitPrinting.emitPrintHeapString ctx reg

    | LIR.LoadFuncAddr (dest, funcName) ->
        ARM64EmitCalls.emitLoadFuncAddr ctx dest funcName

    | LIR.FileReadText (dest, path) ->
        ARM64EmitFiles.emitFileReadText ctx dest path

    | LIR.FileExists (dest, path) ->
        ARM64EmitFiles.emitFileExists ctx dest path

    | LIR.FileWriteText (dest, path, content) ->
        ARM64EmitFiles.emitFileWriteText ctx dest path content

    | LIR.FileAppendText (dest, path, content) ->
        ARM64EmitFiles.emitFileAppendText ctx dest path content

    | LIR.FileDelete (dest, path) ->
        ARM64EmitFiles.emitFileDelete ctx dest path

    | LIR.FileSetExecutable (dest, path) ->
        ARM64EmitFiles.emitFileSetExecutable ctx dest path

    | LIR.FileWriteFromPtr (dest, path, ptr, length) ->
        ARM64EmitFiles.emitFileWriteFromPtr ctx dest path ptr length

    | LIR.MappedAlloc (dest, numBytes) ->
        ARM64EmitMemory.emitMappedAlloc ctx dest numBytes

    | LIR.MappedFree ptr ->
        ARM64EmitMemory.emitMappedFree ctx ptr

    | LIR.RawAlloc (dest, numBytes) ->
        ARM64EmitMemory.emitRawAlloc ctx dest numBytes

    | LIR.RawFree ptr ->
        ARM64EmitMemory.emitRawFree ctx ptr

    | LIR.RawGet (dest, ptr, byteOffset) ->
        ARM64EmitMemory.emitRawGet ctx dest ptr byteOffset

    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        ARM64EmitMemory.emitRawGetByte ctx dest ptr byteOffset

    | LIR.RawWriteWord (ptr, byteOffset, value) ->
        ARM64EmitMemory.emitRawWriteWord ctx ptr byteOffset value

    | LIR.RawSlotInit (ptr, byteOffset, value, valueType) ->
        ARM64EmitMemory.emitRawSlotInit ctx ptr byteOffset value valueType

    | LIR.RawWriteByte (ptr, byteOffset, value) ->
        ARM64EmitMemory.emitRawWriteByte ctx ptr byteOffset value

    | LIR.RefCountIncString str ->
        ARM64EmitReferenceCounts.emitRefCountIncString ctx str
    | LIR.RefCountIncBlob str ->
        ARM64EmitReferenceCounts.emitRefCountIncString ctx str
    | LIR.RefCountIncInt value ->
        ARM64EmitReferenceCounts.emitRefCountIncInt ctx value

    | LIR.RefCountDecString str ->
        ARM64EmitReferenceCounts.emitRefCountDecString ctx str
    | LIR.RefCountDecBlob str ->
        ARM64EmitReferenceCounts.emitRefCountDecString ctx str
    | LIR.RefCountDecInt value ->
        ARM64EmitReferenceCounts.emitRefCountDecInt ctx value

    | LIR.RandomInt64 dest ->
        ARM64EmitNativeEffects.emitRandomInt64 ctx dest

    | LIR.DateTimeNow dest ->
        ARM64EmitNativeEffects.emitDateTimeNow ctx dest

    | LIR.Sleep (effectId, delayMs) ->
        ARM64EmitNativeEffects.emitSleep ctx effectId delayMs

    | LIR.CliNative (dest, operation, args) ->
        ARM64EmitNativeEffects.emitCliNative ctx dest operation args

    | LIR.FloatToString (dest, value) ->
        ARM64EmitFloatingPoint.emitFloatToString ctx dest value

    | LIR.CoverageHit exprId ->
        ARM64EmitNativeEffects.emitCoverageHit ctx exprId
