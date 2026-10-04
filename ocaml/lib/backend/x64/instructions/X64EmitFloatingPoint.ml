(* FloatingPoint.fs - Emit x64 instructions for floatingpoint operations. *)
[@@@warning "-4"]
open X64Operands
open X64CodeGenTypes
open X64InstructionContext
(*
   Float arguments are parallel moves: a source may be overwritten by an
   earlier destination, so cycles are broken through a stack slot.
*)
let emitFArgMoves (_ctx:funcCtx) moves=
 let resolvedMoves=List.map (fun (destPhys,srcFreg) -> match srcFreg with LIR.FPhysical srcPhys->lirFRegToX86 destPhys,lirFRegToX86 srcPhys|LIR.FVirtual id->Crash.crash (Printf.sprintf "Unresolved virtual float register f%d in FArgMoves" id)) moves in
 ParallelMoves.resolve resolvedMoves (fun srcReg -> Some srcReg) |> List.concat_map (function
  | ParallelMoves.SaveToTemp src->[X86_64.SUB_imm (X86_64.RSP,16l);X86_64.MOVSD_store (X86_64.RSP,0l,src)]
  | ParallelMoves.Move (dest,src)->[X86_64.MOVSD_reg (dest,src)]
  | ParallelMoves.MoveFromTemp dest->[X86_64.MOVSD_load (dest,X86_64.RSP,0l);X86_64.ADD_imm (X86_64.RSP,16l)]) |> fun instructions -> Ok instructions
let emitFPhi (_ctx:funcCtx)=Ok []
(*
   Register allocation represents a parallel-move cycle temp as f-2000.
   Keep that value on the stack so every XMM register remains allocatable.
*)
let emitFMov (_ctx:funcCtx) dest src=match dest,src with
 | LIR.FVirtual (-1),LIR.FPhysical srcPhys->Ok [X86_64.MOVSD_reg (X86_64.XMM14,lirFRegToX86 srcPhys)]
 | LIR.FPhysical destPhys,LIR.FVirtual (-1)->Ok [X86_64.MOVSD_reg (lirFRegToX86 destPhys,X86_64.XMM14)]
 | LIR.FVirtual (-2000),LIR.FPhysical srcPhys->let s=lirFRegToX86 srcPhys in Ok [X86_64.SUB_imm (X86_64.RSP,16l);X86_64.MOVSD_store (X86_64.RSP,0l,s)]
 | LIR.FPhysical destPhys,LIR.FVirtual (-2000)->let d=lirFRegToX86 destPhys in Ok [X86_64.MOVSD_load (d,X86_64.RSP,0l);X86_64.ADD_imm (X86_64.RSP,16l)]
 | LIR.FPhysical destPhys,LIR.FPhysical srcPhys->let d=lirFRegToX86 destPhys and s=lirFRegToX86 srcPhys in Ok (if d=s then [] else [X86_64.MOVSD_reg (d,s)])
 | _->Error "FMov with unresolved virtual FP register"
(*
   Load float immediate via GP register
*)
let emitFLoad (_ctx:funcCtx) dest value=match dest with LIR.FPhysical dp->let d=lirFRegToX86 dp in let bits=Int64.bits_of_float value in Ok (loadImm64 scratch bits@[X86_64.MOVQ_from_gp (d,scratch)])|_->Error "FLoad with virtual FP register"
let emitFSpillLoad ctx dest stackSlot=match dest with LIR.FPhysical destPhys->Ok [X86_64.MOVSD_load (lirFRegToX86 destPhys,X86_64.RBP,Int32.of_int (adjustStackOffset ctx stackSlot))]|_->Error "FSpillLoad with virtual FP register"
let emitFSpillStore ctx stackSlot src=match src with LIR.FPhysical srcPhys->Ok [X86_64.MOVSD_store (X86_64.RBP,Int32.of_int (adjustStackOffset ctx stackSlot),lirFRegToX86 srcPhys)]|_->Error "FSpillStore with virtual FP register"
(*
   commutative: swap operands
*)
let emitFAdd (_ctx:funcCtx) dest left right=match dest,left,right with
 | LIR.FPhysical dp,LIR.FPhysical lp,LIR.FPhysical rp->let d=lirFRegToX86 dp and l=lirFRegToX86 lp and r=lirFRegToX86 rp in if d=r && d<>l then Ok [X86_64.ADDSD (d,l)] else let setup=if d<>l then [X86_64.MOVSD_reg (d,l)] else [] in Ok (setup@[X86_64.ADDSD (d,r)])
 | _->Error "FAdd with virtual FP register"
let emitFSub (_ctx:funcCtx) dest left right=match dest,left,right with
 | LIR.FPhysical dp,LIR.FPhysical lp,LIR.FPhysical rp->let d=lirFRegToX86 dp and l=lirFRegToX86 lp and r=lirFRegToX86 rp in if d=r && d<>l then Ok (withPreservedFloatScratch [d;l;r] (fun temp -> [X86_64.MOVSD_reg (temp,l);X86_64.SUBSD (temp,r);X86_64.MOVSD_reg (d,temp)])) else let setup=if d<>l then [X86_64.MOVSD_reg (d,l)] else [] in Ok (setup@[X86_64.SUBSD (d,r)])
 | _->Error "FSub with virtual FP register"
(*
   commutative: swap operands
*)
let emitFMul (_ctx:funcCtx) dest left right=match dest,left,right with
 | LIR.FPhysical dp,LIR.FPhysical lp,LIR.FPhysical rp->let d=lirFRegToX86 dp and l=lirFRegToX86 lp and r=lirFRegToX86 rp in if d=r && d<>l then Ok [X86_64.MULSD (d,l)] else let setup=if d<>l then [X86_64.MOVSD_reg (d,l)] else [] in Ok (setup@[X86_64.MULSD (d,r)])
 | _->Error "FMul with virtual FP register"
let emitFDiv (_ctx:funcCtx) dest left right=match dest,left,right with
 | LIR.FPhysical dp,LIR.FPhysical lp,LIR.FPhysical rp->let d=lirFRegToX86 dp and l=lirFRegToX86 lp and r=lirFRegToX86 rp in if d=r && d<>l then Ok (withPreservedFloatScratch [d;l;r] (fun temp -> [X86_64.MOVSD_reg (temp,l);X86_64.DIVSD (temp,r);X86_64.MOVSD_reg (d,temp)])) else let setup=if d<>l then [X86_64.MOVSD_reg (d,l)] else [] in Ok (setup@[X86_64.DIVSD (d,r)])
 | _->Error "FDiv with virtual FP register"
let emitFNeg (_ctx:funcCtx) dest src=match dest,src with
 | LIR.FPhysical dp,LIR.FPhysical sp->let d=lirFRegToX86 dp and s=lirFRegToX86 sp in Ok (withPreservedFloatScratch [d;s] (fun temp -> loadImm64 scratch Int64.min_int@[X86_64.MOVQ_from_gp (temp,scratch);X86_64.MOVSD_reg (d,s);X86_64.XORPD (d,temp)]))
 | _->Error "FNeg with virtual FP register"
(*
   Abs: clear sign bit using ANDPD with 0x7FFFFFFFFFFFFFFF mask.
   We don't have ANDPD in our ISA, but we can use the GP trick:
   1. Move float to GP register
   2. AND with 0x7FFFFFFFFFFFFFFF
   3. Move back to float register
   Move float bits to GP, AND with mask to clear sign bit, move back
*)
let emitFAbs (_ctx:funcCtx) dest src=match dest,src with
 | LIR.FPhysical dp,LIR.FPhysical sp->let d=lirFRegToX86 dp and s=lirFRegToX86 sp in Ok ([X86_64.MOVQ_to_gp (scratch,s)]@loadImm64 X86_64.RCX Int64.max_int@[X86_64.AND_reg (scratch,X86_64.RCX);X86_64.MOVQ_from_gp (d,scratch)])
 | _->Error "FAbs with virtual FP register"
let emitFSqrt (_ctx:funcCtx) dest src=match dest,src with LIR.FPhysical dp,LIR.FPhysical sp->Ok [X86_64.SQRTSD (lirFRegToX86 dp,lirFRegToX86 sp)]|_->Error "FSqrt with virtual FP register"
let emitFCmp (_ctx:funcCtx) left right=match left,right with LIR.FPhysical lp,LIR.FPhysical rp->Ok [X86_64.UCOMISD (lirFRegToX86 lp,lirFRegToX86 rp)]|_->Error "FCmp with virtual FP register"
let emitFloatToInt64 (_ctx:funcCtx) dest src=match src with LIR.FPhysical sp->resolveReg dest |> Result.map (fun destReg -> [X86_64.CVTTSD2SI (destReg,lirFRegToX86 sp)])|_->Error "FloatToInt64 with virtual FP register"
let emitFpToGp (_ctx:funcCtx) dest src=match src with LIR.FPhysical sp->resolveReg dest |> Result.map (fun destReg -> [X86_64.MOVQ_to_gp (destReg,lirFRegToX86 sp)])|_->Error "FpToGp with virtual FP register"
let emitFloatToBits (_ctx:funcCtx) dest src=match src with LIR.FPhysical sp->resolveReg dest |> Result.map (fun destReg -> [X86_64.MOVQ_to_gp (destReg,lirFRegToX86 sp)])|_->Error "FloatToBits with virtual FP register"
(*
   Call Stdlib.Float.toString(D0)
*)
let emitFloatToString (_ctx:funcCtx) dest src=match src with
 | LIR.FPhysical fp->let xmm=lirFRegToX86 fp in resolveReg dest |> Result.map (fun destReg -> (if xmm<>X86_64.XMM0 then [X86_64.MOVSD_reg (X86_64.XMM0,xmm)] else [])@[X86_64.CALL "Darklang.Stdlib.Float.toString"]@(if destReg<>X86_64.RAX then [X86_64.MOV_reg (destReg,X86_64.RAX)] else []))
 | _->Error "FloatToString with virtual FP register"
