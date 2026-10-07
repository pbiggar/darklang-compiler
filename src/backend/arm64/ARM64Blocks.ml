(* Blocks.fs - Lower terminators and order blocks for target fallthrough. *)
open ARM64CodeGenTypes
open ARM64Operands
open ARM64Instructions
(*
   Convert LIR terminator to ARM64 instructions
   epilogueLabel: the label to jump to for function return (handles stack cleanup)
   Jump to function epilogue (handles stack cleanup and RET)
   Branch if register is non-zero (true), otherwise fall through to else
   Use CBNZ (compare and branch if not zero) to true label
   Then unconditional branch to false label
   Branch if register is zero, otherwise fall through to non-zero case
   Use CBZ (compare and branch if zero) to zero label
   Then unconditional branch to non-zero label
   Branch if specified bit is zero, otherwise fall through to non-zero case
   Use TBZ (test bit and branch if zero) to zero label
   Branch if specified bit is non-zero, otherwise fall through to zero case
   Use TBNZ (test bit and branch if not zero) to non-zero label
   Then unconditional branch to zero label
   Branch based on condition flags (set by previous CMP)
   Use B.cond to true label, then unconditional branch to false label
*)
let convertTerminator epilogueLabel nextLabel terminator=match terminator with
 | LIR.Ret -> if nextLabel=None then Ok [] else Ok [Symbolic.B_label epilogueLabel]
 | LIR.Branch (reg,LIR.Label trueLbl,LIR.Label falseLbl) -> lirRegToARM64Reg reg |> Result.map (fun reg -> [Symbolic.CBNZ (reg,trueLbl)]@(if nextLabel=Some falseLbl then [] else [Symbolic.B_label falseLbl]))
 | LIR.BranchZero (reg,LIR.Label zeroLbl,LIR.Label nonZeroLbl) -> lirRegToARM64Reg reg |> Result.map (fun reg -> [Symbolic.CBZ (reg,zeroLbl)]@(if nextLabel=Some nonZeroLbl then [] else [Symbolic.B_label nonZeroLbl]))
 | LIR.BranchBitZero (reg,bit,LIR.Label zeroLbl,LIR.Label nonZeroLbl) -> lirRegToARM64Reg reg |> Result.map (fun reg -> [Symbolic.TBZ_label (reg,bit,zeroLbl)]@(if nextLabel=Some nonZeroLbl then [] else [Symbolic.B_label nonZeroLbl]))
 | LIR.BranchBitNonZero (reg,bit,LIR.Label nonZeroLbl,LIR.Label zeroLbl) -> lirRegToARM64Reg reg |> Result.map (fun reg -> [Symbolic.TBNZ_label (reg,bit,nonZeroLbl)]@(if nextLabel=Some zeroLbl then [] else [Symbolic.B_label zeroLbl]))
 | LIR.Jump (LIR.Label label) -> if nextLabel=Some label then Ok [] else Ok [Symbolic.B_label label]
 | LIR.CondBranch (condition,LIR.Label trueLbl,LIR.Label falseLbl) ->
  let condition=match condition with LIR.EQ->Symbolic.EQ|LIR.NE->Symbolic.NE|LIR.LT->Symbolic.LT|LIR.GT->Symbolic.GT|LIR.LE->Symbolic.LE|LIR.GE->Symbolic.GE|LIR.ULT->Symbolic.LO|LIR.UGT->Symbolic.HI|LIR.ULE->Symbolic.LS|LIR.UGE->Symbolic.HS in
  Ok ([Symbolic.B_cond_label (condition,trueLbl)]@(if nextLabel=Some falseLbl then [] else [Symbolic.B_label falseLbl]))
(*
   Convert LIR basic block to ARM64 instructions (with label)
   epilogueLabel: passed through to terminator for Ret handling
*)
let lirInstructionCaseNames=[|"Mov";"Phi";"Store";"Add";"Sub";"Mul";"Sdiv";"Udiv";"Msub";"Madd";"Cmp";"Cset";"Select";"And";"And_imm";"Orr";"Eor";"Lsl";"Lsr";"Asr";"Lsl_imm";"Lsr_imm";"Asr_imm";"Neg";"Mvn";"Sxtb";"Sxth";"Sxtw";"Uxtb";"Uxth";"Uxtw";"Call";"TailCall";"IndirectCall";"IndirectTailCall";"ClosureAlloc";"ClosureCall";"ClosureTailCall";"SaveRegs";"RestoreRegs";"ArgMoves";"TailArgMoves";"FArgMoves";"PrintInt64";"PrintUInt64";"PrintBool";"PrintInt64NoNewline";"PrintUInt64NoNewline";"PrintBoolNoNewline";"PrintFloat";"PrintFloatNoNewline";"PrintString";"StdoutWrite";"StdinReadLine";"RuntimeError";"RuntimeErrorString";"PrintHeapStringNoNewline";"PrintChars";"PrintBlob";"PrintList";"PrintSum";"PrintRecord";"Exit";"FPhi";"FMov";"FLoad";"FSpillLoad";"FSpillStore";"FAdd";"FSub";"FMul";"FMadd";"FDiv";"FNeg";"FAbs";"FSqrt";"FCmp";"Int64ToFloat";"FloatToInt64";"FloatToBits";"GpToFp";"FpToGp";"HeapAlloc";"HeapStore";"HeapLoad";"RefCountInc";"RefCountDec";"StringConcat";"CanonicalBufferEq";"PrintHeapString";"LoadFuncAddr";"FileReadBlob";"FileExists";"FileWriteBlob";"FileAppendText";"FileDelete";"FileCreateDirectory";"FileSetExecutable";"FileWriteFromPtr";"RawAlloc";"MappedAlloc";"RawFree";"MappedFree";"RawGet";"RawGetByte";"RawWriteWord";"RawWriteByte";"RawSlotInit";"RefCountIncString";"RefCountDecString";"RefCountIncBlob";"RefCountDecBlob";"RefCountIncInt";"RefCountDecInt";"RandomInt64";"DateTimeNow";"Sleep";"CliNative";"FloatToString";"CoverageHit"|]
let readLirInstructionTag = function
 | LIR.Mov _ -> 0
 | LIR.Phi _ -> 1
 | LIR.Store _ -> 2
 | LIR.Add _ -> 3
 | LIR.Sub _ -> 4
 | LIR.Mul _ -> 5
 | LIR.Sdiv _ -> 6
 | LIR.Udiv _ -> 7
 | LIR.Msub _ -> 8
 | LIR.Madd _ -> 9
 | LIR.Cmp _ -> 10
 | LIR.Cset _ -> 11
 | LIR.Select _ -> 12
 | LIR.And _ -> 13
 | LIR.And_imm _ -> 14
 | LIR.Orr _ -> 15
 | LIR.Eor _ -> 16
 | LIR.Lsl _ -> 17
 | LIR.Lsr _ -> 18
 | LIR.Asr _ -> 19
 | LIR.Lsl_imm _ -> 20
 | LIR.Lsr_imm _ -> 21
 | LIR.Asr_imm _ -> 22
 | LIR.Neg _ -> 23
 | LIR.Mvn _ -> 24
 | LIR.Sxtb _ -> 25
 | LIR.Sxth _ -> 26
 | LIR.Sxtw _ -> 27
 | LIR.Uxtb _ -> 28
 | LIR.Uxth _ -> 29
 | LIR.Uxtw _ -> 30
 | LIR.Call _ -> 31
 | LIR.TailCall _ -> 32
 | LIR.IndirectCall _ -> 33
 | LIR.IndirectTailCall _ -> 34
 | LIR.ClosureAlloc _ -> 35
 | LIR.ClosureCall _ -> 36
 | LIR.ClosureTailCall _ -> 37
 | LIR.SaveRegs _ -> 38
 | LIR.RestoreRegs _ -> 39
 | LIR.ArgMoves _ -> 40
 | LIR.TailArgMoves _ -> 41
 | LIR.FArgMoves _ -> 42
 | LIR.PrintInt64 _ -> 43
 | LIR.PrintUInt64 _ -> 44
 | LIR.PrintBool _ -> 45
 | LIR.PrintInt64NoNewline _ -> 46
 | LIR.PrintUInt64NoNewline _ -> 47
 | LIR.PrintBoolNoNewline _ -> 48
 | LIR.PrintFloat _ -> 49
 | LIR.PrintFloatNoNewline _ -> 50
 | LIR.PrintString _ -> 51
 | LIR.StdoutWrite _ -> 52
 | LIR.StdinReadLine _ -> 53
 | LIR.RuntimeError _ -> 54
 | LIR.RuntimeErrorString _ -> 55
 | LIR.PrintHeapStringNoNewline _ -> 56
 | LIR.PrintChars _ -> 57
 | LIR.PrintBlob _ -> 58
 | LIR.PrintList _ -> 59
 | LIR.PrintSum _ -> 60
 | LIR.PrintRecord _ -> 61
 | LIR.Exit -> 62
 | LIR.FPhi _ -> 63
 | LIR.FMov _ -> 64
 | LIR.FLoad _ -> 65
 | LIR.FSpillLoad _ -> 66
 | LIR.FSpillStore _ -> 67
 | LIR.FAdd _ -> 68
 | LIR.FSub _ -> 69
 | LIR.FMul _ -> 70
 | LIR.FMadd _ -> 71
 | LIR.FDiv _ -> 72
 | LIR.FNeg _ -> 73
 | LIR.FAbs _ -> 74
 | LIR.FSqrt _ -> 75
 | LIR.FCmp _ -> 76
 | LIR.Int64ToFloat _ -> 77
 | LIR.FloatToInt64 _ -> 78
 | LIR.FloatToBits _ -> 79
 | LIR.GpToFp _ -> 80
 | LIR.FpToGp _ -> 81
 | LIR.HeapAlloc _ -> 82
 | LIR.HeapStore _ -> 83
 | LIR.HeapLoad _ -> 84
 | LIR.RefCountInc _ -> 85
 | LIR.RefCountDec _ -> 86
 | LIR.StringConcat _ -> 87
 | LIR.CanonicalBufferEq _ -> 88
 | LIR.PrintHeapString _ -> 89
 | LIR.LoadFuncAddr _ -> 90
 | LIR.FileReadBlob _ -> 91
 | LIR.FileExists _ -> 92
 | LIR.FileWriteBlob _ -> 93
 | LIR.FileAppendText _ -> 94
 | LIR.FileDelete _ -> 95
 | LIR.FileCreateDirectory _ -> 96
 | LIR.FileSetExecutable _ -> 97
 | LIR.FileWriteFromPtr _ -> 98
 | LIR.RawAlloc _ -> 99
 | LIR.MappedAlloc _ -> 100
 | LIR.RawFree _ -> 101
 | LIR.MappedFree _ -> 102
 | LIR.RawGet _ -> 103
 | LIR.RawGetByte _ -> 104
 | LIR.RawWriteWord _ -> 105
 | LIR.RawWriteByte _ -> 106
 | LIR.RawSlotInit _ -> 107
 | LIR.RefCountIncString _ -> 108
 | LIR.RefCountDecString _ -> 109
 | LIR.RefCountIncBlob _ -> 110
 | LIR.RefCountDecBlob _ -> 111
 | LIR.RefCountIncInt _ -> 112
 | LIR.RefCountDecInt _ -> 113
 | LIR.RandomInt64 _ -> 114
 | LIR.DateTimeNow _ -> 115
 | LIR.Sleep _ -> 116
 | LIR.CliNative _ -> 117
 | LIR.FloatToString _ -> 118
 | LIR.CoverageHit _ -> 119
let lirInstructionOpcode instruction=
 let tag=readLirInstructionTag instruction in
 if tag<0 || tag>=Array.length lirInstructionCaseNames then Crash.crash (Printf.sprintf "ARM64 LIR profiling received invalid instruction tag %d" tag) else lirInstructionCaseNames.(tag)
let lirInstructionProfileDetail = function
 | LIR.RefCountInc (_,payloadSize,kind,metadata) | LIR.RefCountDec (_,payloadSize,kind,metadata) ->
  let sourceType=Option.bind metadata (fun value -> value.MemoryModel.sourceType) |> Option.map CheckingDiagnostics.typeToString |> Option.value ~default:"unknown" in
  let kind=match kind with LIR.GenericHeap->"GenericHeap"|LIR.StreamHeap->"StreamHeap"|LIR.TaggedList->"TaggedList"|LIR.DictHeap->"DictHeap"|LIR.ClosureHeap->"ClosureHeap" in Printf.sprintf "%s:%d:%s" kind payloadSize sourceType
 | _ -> ""
[@@warning "-4"]
(*
   Emit label for this block
*)
let convertBlock (ctx:codeGenContext) epilogueLabel nextBlock (block:LIR.basicBlock)=
 let LIR.Label label=block.LIR.label in
 let results=List.mapi (fun index instruction ->
  let instructionCtx={ctx with instructionSite=Printf.sprintf "%s_%d" label index} in
  match ctx.recordLirOpExpansion with
  | None -> convertInstr instructionCtx instruction
  | Some record ->
   let started=Mtime_clock.elapsed_ns () in
   convertInstr instructionCtx instruction |> Result.map (fun instructions ->
    let elapsedTicks=Int64.sub (Mtime_clock.elapsed_ns ()) started in
    record ctx.functionName (lirInstructionOpcode instruction) (lirInstructionProfileDetail instruction) (List.length instructions) elapsedTicks;instructions)) block.LIR.instrs in
 match ResultList.collectResults Fun.id results with
 | Error error -> Error error
 | Ok instructions ->
  let nextLabel=Option.map (fun (next:LIR.basicBlock) -> let LIR.Label label=next.LIR.label in label) nextBlock in
  convertTerminator epilogueLabel nextLabel block.LIR.terminator |> Result.map (fun terminator -> Symbolic.Label label::(instructions@terminator))
(*
   Convert LIR CFG to ARM64 instructions
   epilogueLabel: passed through to blocks for Ret handling
*)
let convertCFG (ctx:codeGenContext) epilogueLabel cfg=
 match LIR.layoutBlocks cfg |> Result.map_error (fun error -> "ARM64 codegen: function "^ctx.functionName^": "^error) with
 | Error error -> Error error
 | Ok blocks ->
  let results=List.mapi (fun index block -> convertBlock ctx epilogueLabel (List.nth_opt blocks (index+1)) block) blocks in ResultList.collectResults Fun.id results
