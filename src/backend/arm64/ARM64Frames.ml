(*
   ARM64Frames.ml - Generate aligned frames and callee-saved register handling.
*)
open ARM64Operands
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let mul a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let int16 value=let low=value land 0xffff in if low>=0x8000 then low-0x10000 else low
(*
   Generate STP instructions to save callee-saved register pairs
   Returns instructions and total bytes pushed
   Process in pairs. If odd number, pad with X27 (or just save single)
   Single register: use STR instead of STP
*)
let generateCalleeSavedSaves regs =
 let rec savePairs remaining offset acc=match remaining with
 | [] -> List.rev acc,offset
 | [single] -> let instr=Symbolic.STR (lirPhysRegToARM64Reg single,Symbolic.SP,int16 offset) in List.rev (instr::acc),add offset 8
 | r1::r2::rest -> let instr=Symbolic.STP (lirPhysRegToARM64Reg r1,lirPhysRegToARM64Reg r2,Symbolic.SP,int16 offset) in savePairs rest (add offset 16) (instr::acc) in
 if regs=[] then [],0 else savePairs regs 0 []
(*
   Generate LDP instructions to restore callee-saved register pairs
*)
let generateCalleeSavedRestores regs =
 let rec restorePairs remaining offset acc=match remaining with
 | [] -> List.rev acc
 | [single] -> let instr=Symbolic.LDR (lirPhysRegToARM64Reg single,Symbolic.SP,int16 offset) in List.rev (instr::acc)
 | r1::r2::rest -> let instr=Symbolic.LDP (lirPhysRegToARM64Reg r1,lirPhysRegToARM64Reg r2,Symbolic.SP,int16 offset) in restorePairs rest (add offset 16) (instr::acc) in
 if regs=[] then [] else restorePairs regs 0 []
(*
   Calculate stack space needed for callee-saved registers (16-byte aligned)
   16-byte aligned
*)
let calleeSavedStackSpace regs=let count=List.length regs in if count=0 then 0 else mul ((add (mul count 8) 15)/16) 16
let floatCalleeSavedStackSpace regs=let count=List.length regs in if count=0 then 0 else mul ((add (mul count 8) 15)/16) 16
let generateFloatCalleeSavedSaves regs baseOffset =
 let rec emit remaining offset=match remaining with
 | [] -> []
 | [reg] -> [Symbolic.STR_fp (lirPhysFPRegToARM64FReg reg,Symbolic.SP,int16 offset)]
 | first::second::rest -> Symbolic.STP_fp (lirPhysFPRegToARM64FReg first,lirPhysFPRegToARM64FReg second,Symbolic.SP,int16 offset)::emit rest (add offset 16) in emit regs baseOffset
let generateFloatCalleeSavedRestores regs baseOffset =
 let rec emit remaining offset=match remaining with
 | [] -> []
 | [reg] -> [Symbolic.LDR_fp (lirPhysFPRegToARM64FReg reg,Symbolic.SP,int16 offset)]
 | first::second::rest -> Symbolic.LDP_fp (lirPhysFPRegToARM64FReg first,lirPhysFPRegToARM64FReg second,Symbolic.SP,int16 offset)::emit rest (add offset 16) in emit regs baseOffset
(*
   Generate function prologue
   Saves FP, LR, callee-saved registers, and allocates stack space
   Prologue sequence:
   1. Save FP (X29) and LR (X30) with pre-indexed addressing (combines SUB and STP)
   2. Set FP = SP: MOV X29, SP
   3. Allocate stack space for spills and callee-saved registers
   4. Save callee-saved registers
   Use pre-indexed STP to save FP/LR and decrement SP in one instruction
   Calculate total additional stack space needed
   Allocate all stack space at once (for spills + callee-saved)
   Save callee-saved registers at [SP]
   (callee-saved are at the bottom of the frame, spill space is above them)
*)
let generatePrologue usedCalleeSaved usedCalleeSavedF stackSize =
 let saveFpLr=[Symbolic.STP_pre (Symbolic.X29,Symbolic.X30,Symbolic.SP,-16)] in let setFp=[Symbolic.MOV_reg (Symbolic.X29,Symbolic.SP)] in
 let calleeSavedSpace=calleeSavedStackSpace usedCalleeSaved in let totalExtraStack=add (add stackSize calleeSavedSpace) (floatCalleeSavedStackSpace usedCalleeSavedF) in
 let allocStack=if totalExtraStack>0 then [Symbolic.SUB_imm (Symbolic.SP,Symbolic.SP,totalExtraStack land 0xffff)] else [] in
 let saveCalleeSavedInstrs,_=generateCalleeSavedSaves usedCalleeSaved in let saveFloatInstrs=generateFloatCalleeSavedSaves usedCalleeSavedF calleeSavedSpace in
 saveFpLr@setFp@allocStack@saveCalleeSavedInstrs@saveFloatInstrs
(*
   Generate function epilogue
   Restores callee-saved registers, FP, LR, and returns
   Epilogue sequence (reverse of prologue):
   1. Restore callee-saved registers from [SP + stackSize]
   2. Deallocate stack space (spills + callee-saved) at once
   3. Restore FP and LR with post-indexed addressing (combines LDP and ADD)
   4. Return: RET
   Restore callee-saved registers from [SP]
   (callee-saved are at the bottom of the frame, spill space is above them)
   Deallocate all stack space at once
   Use post-indexed LDP to restore FP/LR and increment SP in one instruction
*)
let generateEpilogue usedCalleeSaved usedCalleeSavedF stackSize =
 let calleeSavedSpace=calleeSavedStackSpace usedCalleeSaved in let restoreCalleeSavedInstrs=generateCalleeSavedRestores usedCalleeSaved in let restoreFloatInstrs=generateFloatCalleeSavedRestores usedCalleeSavedF calleeSavedSpace in
 let totalExtraStack=add (add stackSize calleeSavedSpace) (floatCalleeSavedStackSpace usedCalleeSavedF) in let deallocStack=if totalExtraStack>0 then [Symbolic.ADD_imm (Symbolic.SP,Symbolic.SP,totalExtraStack land 0xffff)] else [] in
 let restoreFpLr=[Symbolic.LDP_post (Symbolic.X29,Symbolic.X30,Symbolic.SP,16)] in let ret=[Symbolic.RET] in
 restoreFloatInstrs@restoreCalleeSavedInstrs@deallocStack@restoreFpLr@ret
