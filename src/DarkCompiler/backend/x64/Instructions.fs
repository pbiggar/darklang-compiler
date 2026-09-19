// Instructions.fs - Exhaustively dispatch LIR operations to typed x64 emitters.

module X64Instructions

open X64CodeGenTypes
open X64ReleaseSelection
open X64FieldReferenceCounts
open X64ListReferenceCounts
open X64InstructionContext

let internal translateInstr
    (comparisonContext: ComparisonContext option)
    (ctx: FuncCtx)
    (instr: LIR.Instr)
    : Result<X86_64.Instr list, string> =
    match instr with
    | LIR.Mov (dest, src) ->
        X64EmitInteger.emitMov ctx dest src

    | LIR.Store (stackSlot, src) ->
        X64EmitInteger.emitStore ctx stackSlot src

    | LIR.Add (dest, left, right) ->
        X64EmitInteger.emitAdd ctx dest left right

    | LIR.Sub (dest, left, right) ->
        X64EmitInteger.emitSub ctx dest left right

    | LIR.Mul (dest, left, right) ->
        X64EmitInteger.emitMul ctx dest left right

    | LIR.Sdiv (dest, left, right) ->
        X64EmitInteger.emitSdiv ctx dest left right

    | LIR.Udiv (dest, left, right) ->
        X64EmitInteger.emitUdiv ctx dest left right

    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        X64EmitInteger.emitMsub ctx dest mulLeft mulRight sub

    | LIR.Cmp (left, right) ->
        X64EmitInteger.emitCmp ctx left right

    | LIR.Cset (dest, cond) ->
        X64EmitInteger.emitCset ctx comparisonContext dest cond

    | LIR.And (dest, left, right) ->
        X64EmitInteger.emitAnd ctx dest left right

    | LIR.And_imm (dest, src, imm) ->
        X64EmitInteger.emitAnd_imm ctx dest src imm

    | LIR.Orr (dest, left, right) ->
        X64EmitInteger.emitOrr ctx dest left right

    | LIR.Eor (dest, left, right) ->
        X64EmitInteger.emitEor ctx dest left right

    | LIR.Lsl_imm (dest, src, shift) ->
        X64EmitInteger.emitLsl_imm ctx dest src shift

    | LIR.Lsr_imm (dest, src, shift) ->
        X64EmitInteger.emitLsr_imm ctx dest src shift

    | LIR.Asr_imm (dest, src, shift) ->
        X64EmitInteger.emitAsr_imm ctx dest src shift

    | LIR.Neg (dest, src) ->
        X64EmitInteger.emitNeg ctx dest src

    | LIR.Mvn (dest, src) ->
        X64EmitInteger.emitMvn ctx dest src

    | LIR.Sxtb (dest, src) ->
        X64EmitInteger.emitSxtb ctx dest src

    | LIR.Sxth (dest, src) ->
        X64EmitInteger.emitSxth ctx dest src

    | LIR.Sxtw (dest, src) ->
        X64EmitInteger.emitSxtw ctx dest src

    | LIR.Uxtb (dest, src) ->
        X64EmitInteger.emitUxtb ctx dest src

    | LIR.Exit ->
        X64EmitInteger.emitExit ctx

    | LIR.PrintChars bytes ->
        X64EmitPrinting.emitPrintChars ctx bytes

    | LIR.PrintInt64 reg ->
        X64EmitPrinting.emitPrintInt64 ctx reg

    | LIR.PrintUInt64 reg ->
        X64EmitPrinting.emitPrintUInt64 ctx reg

    | LIR.PrintInt64NoNewline reg ->
        X64EmitPrinting.emitPrintInt64NoNewline ctx reg

    | LIR.PrintUInt64NoNewline reg ->
        X64EmitPrinting.emitPrintUInt64NoNewline ctx reg

    | LIR.PrintBool reg ->
        X64EmitPrinting.emitPrintBool ctx reg

    | LIR.PrintBoolNoNewline reg ->
        X64EmitPrinting.emitPrintBoolNoNewline ctx reg

    | LIR.PrintHeapString reg ->
        X64EmitPrinting.emitPrintHeapString ctx reg

    | LIR.PrintHeapStringNoNewline reg ->
        X64EmitPrinting.emitPrintHeapStringNoNewline ctx reg

    | LIR.PrintString str ->
        X64EmitPrinting.emitPrintString ctx str

    | LIR.StdoutWrite (_, value, appendNewline) ->
        X64EmitInteger.emitStdoutWrite ctx value appendNewline

    | LIR.StdinReadLine (_, dest) ->
        X64EmitInteger.emitStdinReadLine ctx dest

    | LIR.RuntimeError msg ->
        X64EmitInteger.emitRuntimeError ctx msg

    | LIR.RuntimeErrorString messageReg ->
        X64EmitInteger.emitRuntimeErrorString ctx messageReg

    | LIR.SaveRegs (intRegs, floatRegs) ->
        X64EmitCalls.emitSaveRegs ctx intRegs floatRegs

    | LIR.RestoreRegs (intRegs, floatRegs) ->
        X64EmitCalls.emitRestoreRegs ctx intRegs floatRegs

    | LIR.ArgMoves moves ->
        X64EmitInteger.emitArgMoves ctx moves

    | LIR.TailArgMoves moves ->
        X64EmitInteger.emitTailArgMoves ctx moves

    | LIR.Call (dest, funcName, _args) ->
        X64EmitCalls.emitCall ctx dest funcName _args

    | LIR.TailCall (funcName, _args) ->
        X64EmitCalls.emitTailCall ctx funcName _args

    | LIR.IndirectCall (dest, func, _args) ->
        X64EmitCalls.emitIndirectCall ctx dest func _args

    | LIR.IndirectTailCall (func, _args) ->
        X64EmitCalls.emitIndirectTailCall ctx func _args

    | LIR.LoadFuncAddr (dest, funcName) ->
        X64EmitCalls.emitLoadFuncAddr ctx dest funcName

    | LIR.FArgMoves moves ->
        X64EmitFloatingPoint.emitFArgMoves ctx moves

    | LIR.Phi (dest, _, _) ->
        X64EmitInteger.emitPhi ctx dest

    | LIR.FPhi (_, _) ->
        X64EmitFloatingPoint.emitFPhi ctx

    | LIR.HeapAlloc (dest, sizeBytes) ->
        X64EmitMemory.emitHeapAlloc ctx dest sizeBytes

    | LIR.HeapStore (addr, offset, src, _) ->
        X64EmitMemory.emitHeapStore ctx addr offset src

    | LIR.HeapLoad (dest, addr, offset) ->
        X64EmitMemory.emitHeapLoad ctx dest addr offset

    | LIR.FMov (dest, src) ->
        X64EmitFloatingPoint.emitFMov ctx dest src

    | LIR.FLoad (dest, value) ->
        X64EmitFloatingPoint.emitFLoad ctx dest value

    | LIR.FSpillLoad (dest, stackSlot) ->
        X64EmitFloatingPoint.emitFSpillLoad ctx dest stackSlot

    | LIR.FSpillStore (stackSlot, src) ->
        X64EmitFloatingPoint.emitFSpillStore ctx stackSlot src

    | LIR.FAdd (dest, left, right) ->
        X64EmitFloatingPoint.emitFAdd ctx dest left right

    | LIR.FSub (dest, left, right) ->
        X64EmitFloatingPoint.emitFSub ctx dest left right

    | LIR.FMul (dest, left, right) ->
        X64EmitFloatingPoint.emitFMul ctx dest left right

    | LIR.FDiv (dest, left, right) ->
        X64EmitFloatingPoint.emitFDiv ctx dest left right

    | LIR.FNeg (dest, src) ->
        X64EmitFloatingPoint.emitFNeg ctx dest src

    | LIR.FAbs (dest, src) ->
        X64EmitFloatingPoint.emitFAbs ctx dest src

    | LIR.FSqrt (dest, src) ->
        X64EmitFloatingPoint.emitFSqrt ctx dest src

    | LIR.FCmp (left, right) ->
        X64EmitFloatingPoint.emitFCmp ctx left right

    | LIR.Int64ToFloat (dest, src) ->
        X64EmitInteger.emitInt64ToFloat ctx dest src

    | LIR.FloatToInt64 (dest, src) ->
        X64EmitFloatingPoint.emitFloatToInt64 ctx dest src

    | LIR.GpToFp (dest, src) ->
        X64EmitInteger.emitGpToFp ctx dest src

    | LIR.FpToGp (dest, src) ->
        X64EmitFloatingPoint.emitFpToGp ctx dest src

    | LIR.FloatToBits (dest, src) ->
        X64EmitFloatingPoint.emitFloatToBits ctx dest src

    | LIR.RefCountInc (addr, payloadSize, kind, _) ->
        X64EmitReferenceCounts.emitRefCountInc ctx addr payloadSize kind

    | LIR.RefCountDec (addr, payloadSize, kind, metadata) ->
        X64EmitReferenceCounts.emitRefCountDec ctx addr payloadSize kind metadata

    | LIR.RefCountIncString str
    | LIR.RefCountIncBlob str ->
        X64EmitReferenceCounts.emitRefCountIncString ctx str

    | LIR.RefCountDecString str
    | LIR.RefCountDecBlob str ->
        X64EmitReferenceCounts.emitRefCountDecString ctx str

    | LIR.CanonicalBufferEq (dest, _, left, right) ->
        X64EmitBuffers.emitCanonicalBufferEq ctx dest left right

    | LIR.StringConcat (dest, first, second, remaining) ->
        X64EmitBuffers.emitStringConcat ctx dest first second remaining

    | LIR.CoverageHit _ ->
        X64EmitNativeEffects.emitCoverageHit ctx

    | LIR.Lsl (dest, src, shift) ->
        X64EmitInteger.emitLsl ctx dest src shift

    | LIR.Lsr (dest, src, shift) ->
        X64EmitInteger.emitLsr ctx dest src shift

    | LIR.Asr (dest, src, shift) ->
        X64EmitInteger.emitAsr ctx dest src shift

    | LIR.Uxth (dest, src) ->
        X64EmitInteger.emitUxth ctx dest src

    | LIR.Uxtw (dest, src) ->
        X64EmitInteger.emitUxtw ctx dest src

    | LIR.ClosureAlloc (dest, funcName, captures) ->
        X64EmitInteger.emitClosureAlloc ctx dest funcName captures

    | LIR.ClosureCall (dest, closure, _args) ->
        X64EmitCalls.emitClosureCall ctx dest closure _args

    | LIR.ClosureTailCall (closure, _args) ->
        X64EmitCalls.emitClosureTailCall ctx closure _args

    | LIR.MappedAlloc (dest, numBytes) ->
        X64EmitMemory.emitMappedAlloc ctx dest numBytes

    | LIR.MappedFree ptr ->
        X64EmitMemory.emitMappedFree ctx ptr

    | LIR.RawAlloc (dest, numBytes) ->
        X64EmitMemory.emitRawAlloc ctx dest numBytes

    | LIR.RawFree ptr ->
        X64EmitMemory.emitRawFree ctx ptr

    | LIR.RawGet (dest, ptr, byteOffset) ->
        X64EmitMemory.emitRawGet ctx dest ptr byteOffset

    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        X64EmitMemory.emitRawGetByte ctx dest ptr byteOffset

    | LIR.RawWriteWord (ptr, byteOffset, value) ->
        X64EmitMemory.emitRawWriteWord ctx ptr byteOffset value

    | LIR.RawSlotInit (ptr, byteOffset, value, valueType) ->
        X64EmitMemory.emitRawSlotInit ctx ptr byteOffset value valueType

    | LIR.RawWriteByte (ptr, byteOffset, value) ->
        X64EmitMemory.emitRawWriteByte ctx ptr byteOffset value

    | LIR.RandomInt64 dest ->
        X64EmitNativeEffects.emitRandomInt64 ctx dest

    | LIR.DateTimeNow dest ->
        X64EmitNativeEffects.emitDateTimeNow ctx dest

    | LIR.Sleep (effectId, delayMs) ->
        X64EmitNativeEffects.emitSleep ctx effectId delayMs

    | LIR.CliNative (dest, operation, args) ->
        X64EmitNativeEffects.emitCliNative ctx dest operation args

    | LIR.Madd (dest, mulLeft, mulRight, add) ->
        X64EmitInteger.emitMadd ctx dest mulLeft mulRight add

    | LIR.PrintFloat freg ->
        X64EmitPrinting.emitPrintFloat ctx freg

    | LIR.PrintFloatNoNewline freg ->
        X64EmitPrinting.emitPrintFloatNoNewline ctx freg

    | LIR.FloatToString (dest, src) ->
        X64EmitFloatingPoint.emitFloatToString ctx dest src

    | LIR.PrintList (listPtr, _elemType) ->
        X64EmitPrinting.emitPrintList ctx listPtr _elemType

    | LIR.PrintSum (sumPtr, _variants) ->
        X64EmitPrinting.emitPrintSum ctx sumPtr _variants

    | LIR.PrintRecord (recordPtr, _typeName, _fields) ->
        X64EmitPrinting.emitPrintRecord ctx recordPtr _typeName _fields

    | LIR.PrintBlob reg ->
        X64EmitPrinting.emitPrintBlob ctx reg

    | LIR.FileReadText (dest, path) ->
        X64EmitFiles.emitFileReadText ctx dest path

    | LIR.FileWriteText (dest, path, content) | LIR.FileAppendText (dest, path, content) ->
        X64EmitFiles.emitFileWriteText ctx instr dest path content

    | LIR.FileExists (dest, path) ->
        X64EmitFiles.emitFileExists ctx dest path

    | LIR.FileDelete (dest, _) ->
        X64EmitFiles.emitFileDelete ctx dest

    | LIR.FileSetExecutable (dest, _) ->
        X64EmitFiles.emitFileSetExecutable ctx dest

    | LIR.FileWriteFromPtr (dest, _, _, _) ->
        X64EmitFiles.emitFileWriteFromPtr ctx dest
