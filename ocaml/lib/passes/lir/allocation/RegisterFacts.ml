(* RegisterFacts.fs - Classify integer and floating register definitions and uses. *)
[@@@warning "-4"]
open AllocationModel
let regToVReg = function LIR.Virtual id -> Some id | LIR.Physical _ -> None
let operandToVReg = function LIR.Reg reg -> regToVReg reg | _ -> None
(*
   Liveness Analysis
   Get virtual register IDs used (read) by an instruction
   FP register, not GP
   No registers used
   For float values, the src register is an FVirtual (handled by float allocation)
   so we don't include it in integer liveness
   Int64ToFloat uses an integer source register
   No operands to read
   Float delay is tracked by getUsedFVRegs
   Float value is in FP register, tracked by getUsedFVRegs
   ArgMoves/TailArgMoves contain operands that use virtual registers
   Phi sources are NOT regular uses - they are used at predecessor exits, not at the phi's block
   The liveness analysis handles phi sources specially in computeLivenessBitsRaw
*)
let getUsedVRegs instr = match instr with
    | LIR.Mov (_, src) -> operandToVReg src |> Option.to_list
    | LIR.Store (_, src) -> regToVReg src |> Option.to_list
    | LIR.Add (_, left, right) | LIR.Sub (_, left, right) -> (regToVReg left |> Option.to_list) @ (operandToVReg right |> Option.to_list)
    | LIR.Mul (_, left, right) | LIR.Sdiv (_, left, right) | LIR.Udiv (_, left, right)
    | LIR.And (_, left, right) | LIR.Orr (_, left, right) | LIR.Eor (_, left, right)
    | LIR.Lsl (_, left, right) | LIR.Lsr (_, left, right) | LIR.Asr (_, left, right) -> (regToVReg left |> Option.to_list) @ (regToVReg right |> Option.to_list)
    | LIR.Lsl_imm (_, src, _) | LIR.Lsr_imm (_, src, _) | LIR.Asr_imm (_, src, _) | LIR.And_imm (_, src, _)
    | LIR.Neg (_, src) -> regToVReg src |> Option.to_list
    | LIR.Msub (_, mulLeft, mulRight, sub) -> (regToVReg mulLeft |> Option.to_list) @ (regToVReg mulRight |> Option.to_list) @ (regToVReg sub |> Option.to_list)
    | LIR.Madd (_, mulLeft, mulRight, add) -> (regToVReg mulLeft |> Option.to_list) @ (regToVReg mulRight |> Option.to_list) @ (regToVReg add |> Option.to_list)
    | LIR.Cmp (left, right) -> (regToVReg left |> Option.to_list) @ (operandToVReg right |> Option.to_list)
    | LIR.Cset (_, _) -> []
    | LIR.Select (_, whenTrue, whenFalse, _) -> [regToVReg whenTrue; regToVReg whenFalse] |> List.filter_map Fun.id
    | LIR.Mvn (_, src) -> regToVReg src |> Option.to_list
    | LIR.Sxtb (_, src) | LIR.Sxth (_, src) | LIR.Sxtw (_, src)
    | LIR.Uxtb (_, src) | LIR.Uxth (_, src) | LIR.Uxtw (_, src) -> regToVReg src |> Option.to_list
    | LIR.Call (_, _, args) -> args |> List.filter_map operandToVReg
    | LIR.TailCall (_, args) -> args |> List.filter_map operandToVReg
    | LIR.IndirectCall (_, func, args) -> let funcVReg = regToVReg func |> Option.to_list in let argsVRegs = args |> List.filter_map operandToVReg in funcVReg @ argsVRegs
    | LIR.IndirectTailCall (func, args) -> let funcVReg = regToVReg func |> Option.to_list in let argsVRegs = args |> List.filter_map operandToVReg in funcVReg @ argsVRegs
    | LIR.ClosureAlloc (_, _, captures) -> captures |> List.filter_map operandToVReg
    | LIR.ClosureCall (_, closure, args) -> let closureVReg = regToVReg closure |> Option.to_list in let argsVRegs = args |> List.filter_map operandToVReg in closureVReg @ argsVRegs
    | LIR.ClosureTailCall (closure, args) -> let closureVReg = regToVReg closure |> Option.to_list in let argsVRegs = args |> List.filter_map operandToVReg in closureVReg @ argsVRegs
    | LIR.PrintInt64 reg | LIR.PrintUInt64 reg | LIR.PrintBool reg
    | LIR.PrintInt64NoNewline reg | LIR.PrintUInt64NoNewline reg | LIR.PrintBoolNoNewline reg
    | LIR.PrintHeapStringNoNewline reg | LIR.RuntimeErrorString reg | LIR.PrintList (reg, _)
    | LIR.PrintSum (reg, _, _) | LIR.PrintRecord (reg, _, _) -> regToVReg reg |> Option.to_list
    | LIR.PrintFloatNoNewline _ -> []
    | LIR.PrintChars _ -> []
    | LIR.StdoutWrite (_, value, _) -> operandToVReg value |> Option.to_list
    | LIR.StdinReadLine _ -> []
    | LIR.PrintBlob reg -> regToVReg reg |> Option.to_list
    | LIR.HeapAlloc (_, _) -> []
    | LIR.HeapStore (addr, _, src, valueType) -> let a = Option.to_list (regToVReg addr) in let s = match valueType with Some AST.TFloat64 -> [] | _ -> Option.to_list (operandToVReg src) in a @ s
    | LIR.HeapLoad (_, addr, _) -> regToVReg addr |> Option.to_list
    | LIR.RefCountInc (addr, _, _, _) -> regToVReg addr |> Option.to_list
    | LIR.RefCountDec (addr, _, _, _) -> regToVReg addr |> Option.to_list
    | LIR.StringConcat (_, first, second, remaining) -> first :: second :: remaining |> List.concat_map (fun op -> Option.to_list (operandToVReg op))
    | LIR.CanonicalBufferEq (_, _, left, right) -> (operandToVReg left |> Option.to_list) @ (operandToVReg right |> Option.to_list)
    | LIR.PrintHeapString reg -> regToVReg reg |> Option.to_list
    | LIR.FileReadBlob (_, path) -> operandToVReg path |> Option.to_list
    | LIR.FileExists (_, path) -> operandToVReg path |> Option.to_list
    | LIR.FileWriteBlob (_, path, content) -> (operandToVReg path |> Option.to_list) @ (operandToVReg content |> Option.to_list)
    | LIR.FileAppendText (_, path, content) -> (operandToVReg path |> Option.to_list) @ (operandToVReg content |> Option.to_list)
    | LIR.FileDelete (_, path) -> operandToVReg path |> Option.to_list
    | LIR.FileCreateDirectory (_, path) -> operandToVReg path |> Option.to_list
    | LIR.FileSetExecutable (_, path) -> operandToVReg path |> Option.to_list
    | LIR.FileWriteFromPtr (_, path, ptr, length) -> (operandToVReg path |> Option.to_list) @ (regToVReg ptr |> Option.to_list) @ (regToVReg length |> Option.to_list)
    | LIR.RawAlloc (_, numBytes) -> regToVReg numBytes |> Option.to_list
    | LIR.MappedAlloc (_, numBytes) -> regToVReg numBytes |> Option.to_list
    | LIR.RawFree ptr -> regToVReg ptr |> Option.to_list
    | LIR.MappedFree ptr -> regToVReg ptr |> Option.to_list
    | LIR.RawGet (_, ptr, byteOffset) -> (regToVReg ptr |> Option.to_list) @ (regToVReg byteOffset |> Option.to_list)
    | LIR.RawGetByte (_, ptr, byteOffset) -> (regToVReg ptr |> Option.to_list) @ (regToVReg byteOffset |> Option.to_list)
    | LIR.RawWriteWord (ptr, byteOffset, value) -> (regToVReg ptr |> Option.to_list) @ (regToVReg byteOffset |> Option.to_list) @ (regToVReg value |> Option.to_list)
    | LIR.RawWriteByte (ptr, byteOffset, value) -> (regToVReg ptr |> Option.to_list) @ (regToVReg byteOffset |> Option.to_list) @ (regToVReg value |> Option.to_list)
    | LIR.RawSlotInit (ptr, byteOffset, value, _) -> (regToVReg ptr |> Option.to_list) @ (regToVReg byteOffset |> Option.to_list) @ (regToVReg value |> Option.to_list)
    | LIR.Int64ToFloat (_, src) -> regToVReg src |> Option.to_list
    | LIR.RefCountIncString str -> operandToVReg str |> Option.to_list
    | LIR.RefCountDecString str -> operandToVReg str |> Option.to_list
    | LIR.RefCountIncBlob bytes -> operandToVReg bytes |> Option.to_list
    | LIR.RefCountDecBlob bytes -> operandToVReg bytes |> Option.to_list
    | LIR.RefCountIncInt value
    | LIR.RefCountDecInt value -> operandToVReg value |> Option.to_list
    | LIR.RandomInt64 _ -> []
    | LIR.DateTimeNow _ -> []
    | LIR.Sleep _ -> []
    | LIR.CliNative (_, _, args) -> args |> List.filter_map operandToVReg
    | LIR.FloatToString _ -> []
    | LIR.ArgMoves moves -> moves |> List.filter_map (fun (_, op) -> operandToVReg op)
    | LIR.TailArgMoves moves -> moves |> List.filter_map (fun (_, op) -> operandToVReg op)
    | LIR.Phi _ -> []
    | _ -> []
(*
   Get virtual register ID defined (written) by an instruction
   Tail calls don't return to caller
   Indirect tail calls don't return to caller
   Closure tail calls don't return to caller
   FloatToInt64 defines an integer destination register
   FloatToBits defines an integer destination register
   FpToGp defines an integer destination register
   Phi defines its destination at block entry
*)
let getDefinedVReg instr = match instr with
    | LIR.Mov (dest, _) -> regToVReg dest
    | LIR.Add (dest, _, _) | LIR.Sub (dest, _, _) -> regToVReg dest
    | LIR.Mul (dest, _, _) | LIR.Sdiv (dest, _, _) | LIR.Udiv (dest, _, _) | LIR.Msub (dest, _, _, _) | LIR.Madd (dest, _, _, _) -> regToVReg dest
    | LIR.Cset (dest, _) -> regToVReg dest
    | LIR.Select (dest, _, _, _) -> regToVReg dest
    | LIR.And (dest, _, _) | LIR.And_imm (dest, _, _) | LIR.Orr (dest, _, _) | LIR.Eor (dest, _, _)
    | LIR.Lsl (dest, _, _) | LIR.Lsr (dest, _, _) | LIR.Asr (dest, _, _)
    | LIR.Lsl_imm (dest, _, _) | LIR.Lsr_imm (dest, _, _) | LIR.Asr_imm (dest, _, _) -> regToVReg dest
    | LIR.Neg (dest, _) | LIR.Mvn (dest, _) -> regToVReg dest
    | LIR.Sxtb (dest, _) | LIR.Sxth (dest, _) | LIR.Sxtw (dest, _)
    | LIR.Uxtb (dest, _) | LIR.Uxth (dest, _) | LIR.Uxtw (dest, _) -> regToVReg dest
    | LIR.Call (dest, _, _) -> regToVReg dest
    | LIR.TailCall _ -> None
    | LIR.IndirectCall (dest, _, _) -> regToVReg dest
    | LIR.IndirectTailCall _ -> None
    | LIR.ClosureAlloc (dest, _, _) -> regToVReg dest
    | LIR.ClosureCall (dest, _, _) -> regToVReg dest
    | LIR.ClosureTailCall _ -> None
    | LIR.HeapAlloc (dest, _) -> regToVReg dest
    | LIR.HeapLoad (dest, _, _) -> regToVReg dest
    | LIR.StringConcat (dest, _, _, _) -> regToVReg dest
    | LIR.CanonicalBufferEq (dest, _, _, _) -> regToVReg dest
    | LIR.StdinReadLine (_, dest) -> regToVReg dest
    | LIR.LoadFuncAddr (dest, _) -> regToVReg dest
    | LIR.FileReadBlob (dest, _) -> regToVReg dest
    | LIR.FileExists (dest, _) -> regToVReg dest
    | LIR.FileWriteBlob (dest, _, _) -> regToVReg dest
    | LIR.FileAppendText (dest, _, _) -> regToVReg dest
    | LIR.FileDelete (dest, _) -> regToVReg dest
    | LIR.FileCreateDirectory (dest, _) -> regToVReg dest
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
    | LIR.FloatToInt64 (dest, _) -> regToVReg dest
    | LIR.FloatToBits (dest, _) -> regToVReg dest
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
    | LIR.Phi (dest, _, _) -> regToVReg dest
    | _ -> None
(*
   Float Register Liveness Analysis
*)
let isFixedFVRegId id = id = -1000 || id = -1001 || id = -1002 || id = -2000
let fregToId = function LIR.FVirtual id when isFixedFVRegId id -> None | LIR.FVirtual id -> Some id | LIR.FPhysical _ -> None
(*
   Get FVirtual register IDs used (read) by an instruction
   Phi sources handled specially
   HeapStore with float value: the Virtual register ID is shared with FVirtual
*)
let getUsedFVRegs instr = match instr with
    | LIR.FMov (_, src) -> fregToId src |> Option.to_list
    | LIR.FAdd (_, left, right) | LIR.FSub (_, left, right)
    | LIR.FMul (_, left, right) | LIR.FDiv (_, left, right) -> [fregToId left; fregToId right] |> List.filter_map Fun.id
    | LIR.FMadd (_, left, right, addend) -> [fregToId left; fregToId right; fregToId addend] |> List.filter_map Fun.id
    | LIR.FNeg (_, src) | LIR.FAbs (_, src) | LIR.FSqrt (_, src) -> fregToId src |> Option.to_list
    | LIR.FCmp (left, right) -> [fregToId left; fregToId right] |> List.filter_map Fun.id
    | LIR.FloatToInt64 (_, src) -> fregToId src |> Option.to_list
    | LIR.FloatToBits (_, src) -> fregToId src |> Option.to_list
    | LIR.FpToGp (_, src) -> fregToId src |> Option.to_list
    | LIR.PrintFloat freg | LIR.PrintFloatNoNewline freg -> fregToId freg |> Option.to_list
    | LIR.FArgMoves moves -> moves |> List.filter_map (fun (_, src) -> fregToId src)
    | LIR.FPhi _ -> []
    | LIR.FloatToString (_, value) -> fregToId value |> Option.to_list
    | LIR.Sleep (_, delayMs) -> fregToId delayMs |> Option.to_list
    | LIR.HeapStore (_, _, LIR.Reg (LIR.Virtual vregId), Some AST.TFloat64) -> [vregId]
    | _ -> []
(*
   Get FVirtual register ID defined (written) by an instruction
*)
let getDefinedFVReg instr = match instr with
    | LIR.FMov (dest, _) -> fregToId dest
    | LIR.FAdd (dest, _, _) | LIR.FSub (dest, _, _)
    | LIR.FMul (dest, _, _) | LIR.FDiv (dest, _, _) -> fregToId dest
    | LIR.FMadd (dest, _, _, _) -> fregToId dest
    | LIR.FNeg (dest, _) | LIR.FAbs (dest, _) | LIR.FSqrt (dest, _) -> fregToId dest
    | LIR.FLoad (dest, _) -> fregToId dest
    | LIR.Int64ToFloat (dest, _) -> fregToId dest
    | LIR.GpToFp (dest, _) -> fregToId dest
    | LIR.FPhi (dest, _) -> fregToId dest
    | _ -> None
(*
   Get virtual register used by terminator
   CondBranch uses condition flags, not a register
*)
let getTerminatorUsedVRegs term = match term with
    | LIR.Branch (LIR.Virtual id, _, _) -> [id]
    | LIR.BranchZero (LIR.Virtual id, _, _) -> [id]
    | LIR.BranchBitZero (LIR.Virtual id, _, _, _) -> [id]
    | LIR.BranchBitNonZero (LIR.Virtual id, _, _, _) -> [id]
    | LIR.CondBranch _ -> []
    | _ -> []
let classifyInstr instr =
 let intPhiUses = match instr with LIR.Phi (_, sources, _) -> List.filter_map (fun (source, predecessor) -> match source with LIR.Reg (LIR.Virtual id) -> Some (id, predecessor) | _ -> None) sources | _ -> [] in
 let floatPhiUses = match instr with LIR.FPhi (_, sources) -> List.filter_map (fun (source, predecessor) -> match source with LIR.FVirtual id -> Some (id, predecessor) | LIR.FPhysical _ -> None) sources | _ -> [] in
 {instr;intUses=getUsedVRegs instr;intDef=getDefinedVReg instr;intPhiUses;floatUses=getUsedFVRegs instr;floatDef=getDefinedFVReg instr;floatPhiUses}
let classifyBlocks blocks = Array.map (fun (block : LIR.basicBlock) ->
 let hasPhiNodes = ref false in
 let instrFacts = List.map (fun instr -> (match instr with LIR.Phi _ | LIR.FPhi _ -> hasPhiNodes := true | _ -> ());classifyInstr instr) block.LIR.instrs |> Array.of_list in
 {block;instrFacts;terminatorUses=getTerminatorUsedVRegs block.LIR.terminator;hasPhiNodes= !hasPhiNodes}) blocks
