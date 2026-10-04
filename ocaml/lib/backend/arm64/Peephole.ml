(*
   Peephole.fs - Optimize symbolic target instructions with register-lifetime checks.
*)
[@@@warning "-4"]
(*
   Classify one instruction while proving that a register value is dead.
   Control flow is a conservative barrier because this local peephole does not
   construct a CFG for the final symbolic instruction stream.
*)
type registerLifetimeStep =
    | Unrelated
    | Overwritten
    | ReadOrControlFlow




let registerLifetimeStep
    (target:Symbolic.reg)
    (instr:Symbolic.instr)
    =
    let classify (reads:Symbolic.reg list) (writes:Symbolic.reg list) =
        if List.mem target reads then ReadOrControlFlow
        else if List.mem target writes then Overwritten
        else Unrelated

    in
    match instr with
    | Symbolic.MOVZ (dest, _, _)
    | Symbolic.MOVN (dest, _, _)
    | Symbolic.CSET (dest, _)
    | Symbolic.ADRP (dest, _)
    | Symbolic.ADR (dest, _)
    | Symbolic.FMOV_to_gp (dest, _)
    | Symbolic.UMOV_byte (dest, _)
    | Symbolic.FCVTZS (dest, _) ->
        classify [] [dest]
    | Symbolic.CSEL (dest, whenTrue, whenFalse, _) ->
        classify [whenTrue; whenFalse] [dest]
    | Symbolic.MOVK (dest, _, _) ->
        classify [dest] [dest]
    | Symbolic.ADD_imm (dest, src, _)
    | Symbolic.SUB_imm (dest, src, _)
    | Symbolic.SUB_imm12 (dest, src, _)
    | Symbolic.SUBS_imm (dest, src, _)
    | Symbolic.AND_imm (dest, src, _)
    | Symbolic.LSL_imm (dest, src, _)
    | Symbolic.LSR_imm (dest, src, _)
    | Symbolic.ASR_imm (dest, src, _)
    | Symbolic.ADD_label (dest, src, _) ->
        classify [src] [dest]
    | Symbolic.MVN (dest, src)
    | Symbolic.MOV_reg (dest, src)
    | Symbolic.NEG (dest, src)
    | Symbolic.SXTB (dest, src)
    | Symbolic.SXTH (dest, src)
    | Symbolic.SXTW (dest, src)
    | Symbolic.UXTB (dest, src)
    | Symbolic.UXTH (dest, src)
    | Symbolic.UXTW (dest, src) ->
        classify [src] [dest]
    | Symbolic.ADD_reg (dest, src1, src2)
    | Symbolic.SUB_reg (dest, src1, src2)
    | Symbolic.MUL (dest, src1, src2)
    | Symbolic.SDIV (dest, src1, src2)
    | Symbolic.UDIV (dest, src1, src2)
    | Symbolic.AND_reg (dest, src1, src2)
    | Symbolic.BIC_reg (dest, src1, src2)
    | Symbolic.ORR_reg (dest, src1, src2)
    | Symbolic.EOR_reg (dest, src1, src2)
    | Symbolic.LSL_reg (dest, src1, src2)
    | Symbolic.LSR_reg (dest, src1, src2)
    | Symbolic.ASR_reg (dest, src1, src2) ->
        classify [src1; src2] [dest]
    | Symbolic.ADD_shifted (dest, src1, src2, _)
    | Symbolic.SUB_shifted (dest, src1, src2, _)
    | Symbolic.ADD_extended (dest, src1, src2, _)
    | Symbolic.SUB_extended (dest, src1, src2, _) ->
        classify [src1; src2] [dest]
    | Symbolic.MSUB (dest, src1, src2, src3)
    | Symbolic.MADD (dest, src1, src2, src3) ->
        classify [src1; src2; src3] [dest]
    | Symbolic.CMP_imm (src, _) ->
        classify [src] []
    | Symbolic.CMP_reg (src1, src2) ->
        classify [src1; src2] []
    | Symbolic.STRB (src, addr, _)
    | Symbolic.STR (src, addr, _)
    | Symbolic.STUR (src, addr, _) ->
        classify [src; addr] []
    | Symbolic.STRB_reg (src, addr) ->
        classify [src; addr] []
    | Symbolic.LDRB (dest, addr, index) ->
        classify [addr; index] [dest]
    | Symbolic.LDRB_imm (dest, addr, _)
    | Symbolic.LDR (dest, addr, _)
    | Symbolic.LDUR (dest, addr, _) ->
        classify [addr] [dest]
    | Symbolic.STP (reg1, reg2, addr, _) ->
        classify [reg1; reg2; addr] []
    | Symbolic.STP_pre (reg1, reg2, addr, _) ->
        classify [reg1; reg2; addr] [addr]
    | Symbolic.LDP (reg1, reg2, addr, _) ->
        classify [addr] [reg1; reg2]
    | Symbolic.LDP_post (reg1, reg2, addr, _) ->
        classify [addr] [reg1; reg2; addr]
    | Symbolic.LDR_fp (_, addr, _)
    | Symbolic.STR_fp (_, addr, _)
    | Symbolic.STP_fp (_, _, addr, _)
    | Symbolic.LDP_fp (_, _, addr, _) ->
        classify [addr] []
    | Symbolic.FMOV_from_gp (_, src)
    | Symbolic.SCVTF (_, src) ->
        classify [src] []
    | Symbolic.FADD _
    | Symbolic.FSUB _
    | Symbolic.FMUL _
    | Symbolic.FMADD _
    | Symbolic.FDIV _
    | Symbolic.FNEG _
    | Symbolic.FABS _
    | Symbolic.FSQRT _
    | Symbolic.FCMP _
    | Symbolic.FMOV_reg _
    | Symbolic.FMOV_zero _
    | Symbolic.FMOV_imm _
    | Symbolic.CNT_8B _
    | Symbolic.ADDV_8B _ ->
        Unrelated
    | Symbolic.BL _
    | Symbolic.BLR _
    | Symbolic.BR _
    | Symbolic.CBZ _
    | Symbolic.CBNZ _
    | Symbolic.B_label _
    | Symbolic.B_cond_label _
    | Symbolic.CBZ_offset _
    | Symbolic.CBNZ_offset _
    | Symbolic.TBZ _
    | Symbolic.TBNZ _
    | Symbolic.TBZ_label _
    | Symbolic.TBNZ_label _
    | Symbolic.B _
    | Symbolic.B_cond _
    | Symbolic.RET
    | Symbolic.SVC _
    | Symbolic.Label _ ->
        ReadOrControlFlow

let overwrittenBeforeReadOrEnd
    (target:Symbolic.reg)
    (instrs:Symbolic.instr list)
    =
    let rec check remaining =
        match remaining with
        | [] -> true
        | instr :: rest ->
            match registerLifetimeStep target instr with
            | Unrelated -> check rest
            | Overwritten -> true
            | ReadOrControlFlow -> false

    in check instrs


(*
   Return the condition that selects the complementary control-flow edge.
*)
let invertCondition (condition:ARM64.condition) =
    match condition with
    | ARM64.EQ -> ARM64.NE
    | ARM64.NE -> ARM64.EQ
    | ARM64.LT -> ARM64.GE
    | ARM64.GT -> ARM64.LE
    | ARM64.LE -> ARM64.GT
    | ARM64.GE -> ARM64.LT
    | ARM64.LO -> ARM64.HS
    | ARM64.HI -> ARM64.LS
    | ARM64.LS -> ARM64.HI
    | ARM64.HS -> ARM64.LO















(*
   Peephole optimization pass
   Patterns:
   1. SUB_imm + CMP #0 → SUBS (fuse subtract and compare)
   2. MOV Xn, Xn → remove (redundant self-move)
   3. FMOV Dn, Dn → remove (redundant FP self-move)
   4. ADD Xn, Xn, #0 → remove (add zero)
   5. SUB Xn, Xn, #0 → remove (subtract zero)
   6. B_label X + Label X → remove branch (branch to next instruction)
   7. CMP #0 + B.EQ → CBZ (compare zero and branch equal)
   8. CMP #0 + B.NE → CBNZ (compare zero and branch not equal)
   9. AND Xn, Xn, Xn → MOV (AND with self is identity)
   10. ORR Xn, Xn, Xn → MOV (OR with self is identity)
   11. MOVN #0 + EOR + AND → BIC (bit clear when the inverted temporary is overwritten)
   12. B.cond true + B false + true: → B.!cond false + true: (fall through)
   Fuse SUB + CMP #0 into SUBS
   Fuse CMP #0 + B.EQ into CBZ
   Fuse CMP #0 + B.NE into CBNZ
   Fuse x & (y EOR -1) into BIC x, y. Requiring AND to overwrite the
   EOR destination proves that the inverted temporary is dead here.
   Remove redundant self-move (integer)
   Remove redundant self-move (FP)
   Remove add zero
   Remove subtract zero
   Make an immediately following true target the fallthrough edge.
   AND with self is identity - simplify to MOV if dest differs from operand
   dest = src AND src = src, remove entirely
   OR with self is identity - simplify to MOV if dest differs from operand
   dest = src OR src = src, remove entirely
   Remove branch to next instruction
   Preserve the multiply-by-constant forms produced with a single-use
   shift temporary. Their shared source makes the shifted ADD exact even
   when the flat symbolic stream reaches a control-flow barrier next.
   Fold a dead shifted value into either register position of addition.
   Fold zero/sign extensions into the extended-register add form.
   Pair aligned stack-frame stores. Restrict this to SP-relative memory:
   arbitrary heap/runtime stores can carry ordering and provenance that
   are not represented in the symbolic instruction stream.
*)
let peepholeOptimize (instrs:Symbolic.instr list) =
    let rec optimize acc remaining =
        match remaining with
        | [] -> List.rev acc

        | Symbolic.SUB_imm (dest, src, imm) :: Symbolic.CMP_imm (cmpReg, 0) :: rest when dest = cmpReg ->
            optimize (Symbolic.SUBS_imm (dest, src, imm) :: acc) rest

        | Symbolic.CMP_imm (reg, 0) :: Symbolic.B_cond_label (ARM64.EQ, label) :: rest ->
            optimize (Symbolic.CBZ (reg, label) :: acc) rest

        | Symbolic.CMP_imm (reg, 0) :: Symbolic.B_cond_label (ARM64.NE, label) :: rest ->
            optimize (Symbolic.CBNZ (reg, label) :: acc) rest


        | Symbolic.MOVN (allOnes, 0, 0)
          :: Symbolic.EOR_reg (inverted, value, eorMask)
          :: Symbolic.AND_reg (dest, left, andRight)
          :: rest
            when eorMask = allOnes
                 && andRight = inverted
                 && dest = inverted
                 && allOnes <> inverted
                 && allOnes <> left
                 && allOnes <> value
                 && inverted <> left
                 && overwrittenBeforeReadOrEnd allOnes rest ->
            optimize (Symbolic.BIC_reg (dest, left, value) :: acc) rest
        | Symbolic.MOVN (allOnes, 0, 0)
          :: Symbolic.EOR_reg (inverted, eorMask, value)
          :: Symbolic.AND_reg (dest, left, andRight)
          :: rest
            when eorMask = allOnes
                 && andRight = inverted
                 && dest = inverted
                 && allOnes <> inverted
                 && allOnes <> left
                 && allOnes <> value
                 && inverted <> left
                 && overwrittenBeforeReadOrEnd allOnes rest ->
            optimize (Symbolic.BIC_reg (dest, left, value) :: acc) rest

        | Symbolic.MOV_reg (dest, src) :: rest when dest = src ->
            optimize acc rest

        | Symbolic.FMOV_reg (dest, src) :: rest when dest = src ->
            optimize acc rest

        | Symbolic.ADD_imm (dest, src, 0) :: rest when dest = src ->
            optimize acc rest

        | Symbolic.SUB_imm (dest, src, 0) :: rest when dest = src ->
            optimize acc rest

        | Symbolic.B_cond_label (condition, trueTarget)
          :: Symbolic.B_label falseTarget
          :: Symbolic.Label label
          :: rest
            when trueTarget = label ->
            let branch =
                Symbolic.B_cond_label (invertCondition condition, falseTarget)
            in optimize (Symbolic.Label label :: branch :: acc) rest

        | Symbolic.AND_reg (dest, src1, src2) :: rest when src1 = src2 ->
            if dest = src1 then
                optimize acc rest
            else
                optimize (Symbolic.MOV_reg (dest, src1) :: acc) rest

        | Symbolic.ORR_reg (dest, src1, src2) :: rest when src1 = src2 ->
            if dest = src1 then
                optimize acc rest
            else
                optimize (Symbolic.MOV_reg (dest, src1) :: acc) rest

        | Symbolic.B_label target :: Symbolic.Label lbl :: rest when target = lbl ->
            optimize (Symbolic.Label lbl :: acc) rest



        | Symbolic.LSL_imm (lslDest, lslSrc, shift) :: Symbolic.ADD_reg (addDest, addSrc1, addSrc2) :: rest
            when lslDest = addSrc2 && lslSrc = addSrc1 ->
            optimize (Symbolic.ADD_shifted (addDest, addSrc1, lslSrc, shift) :: acc) rest
        | Symbolic.LSL_imm (lslDest, lslSrc, shift) :: Symbolic.ADD_reg (addDest, addSrc1, addSrc2) :: rest
            when lslDest = addSrc1 && lslSrc = addSrc2 ->
            optimize (Symbolic.ADD_shifted (addDest, addSrc2, lslSrc, shift) :: acc) rest

        | Symbolic.LSL_imm (lslDest, lslSrc, shift) :: Symbolic.ADD_reg (addDest, addSrc1, addSrc2) :: rest
            when lslDest = addSrc2
                 && addSrc1 <> lslDest
                 && (addDest = lslDest || overwrittenBeforeReadOrEnd lslDest rest) ->
            optimize (Symbolic.ADD_shifted (addDest, addSrc1, lslSrc, shift) :: acc) rest
        | Symbolic.LSL_imm (lslDest, lslSrc, shift) :: Symbolic.ADD_reg (addDest, addSrc1, addSrc2) :: rest
            when lslDest = addSrc1
                 && addSrc2 <> lslDest
                 && (addDest = lslDest || overwrittenBeforeReadOrEnd lslDest rest) ->
            optimize (Symbolic.ADD_shifted (addDest, addSrc2, lslSrc, shift) :: acc) rest
        | Symbolic.LSL_imm (lslDest, lslSrc, shift) :: Symbolic.SUB_reg (subDest, subSrc1, subSrc2) :: rest
            when lslDest = subSrc2
                 && subSrc1 <> lslDest
                 && (subDest = lslDest || overwrittenBeforeReadOrEnd lslDest rest) ->
            optimize (Symbolic.SUB_shifted (subDest, subSrc1, lslSrc, shift) :: acc) rest

        | (Symbolic.UXTB (extended, src) as extension) :: Symbolic.ADD_reg (dest, left, right) :: rest
        | (Symbolic.UXTH (extended, src) as extension) :: Symbolic.ADD_reg (dest, left, right) :: rest
        | (Symbolic.UXTW (extended, src) as extension) :: Symbolic.ADD_reg (dest, left, right) :: rest
        | (Symbolic.SXTB (extended, src) as extension) :: Symbolic.ADD_reg (dest, left, right) :: rest
        | (Symbolic.SXTH (extended, src) as extension) :: Symbolic.ADD_reg (dest, left, right) :: rest
        | (Symbolic.SXTW (extended, src) as extension) :: Symbolic.ADD_reg (dest, left, right) :: rest
            when (extended = right || extended = left)
                 && left <> right
                 && (dest = extended || overwrittenBeforeReadOrEnd extended rest) ->
            let baseReg = if extended = right then left else right
            in let extend =
                match extension with
                | Symbolic.UXTB _ -> ARM64.ExtendUXTB | Symbolic.UXTH _ -> ARM64.ExtendUXTH | Symbolic.UXTW _ -> ARM64.ExtendUXTW
                | Symbolic.SXTB _ -> ARM64.ExtendSXTB | Symbolic.SXTH _ -> ARM64.ExtendSXTH | Symbolic.SXTW _ -> ARM64.ExtendSXTW
                | _ -> failwith "ARM64 extension combine received a non-extension"
            in optimize (Symbolic.ADD_extended (dest, baseReg, src, extend) :: acc) rest



        | Symbolic.STR (src1, addr1, offset1)
            :: Symbolic.STR (src2, addr2, offset2)
            :: rest
            when addr1 = ARM64.SP && addr2 = ARM64.SP
                 && offset2 = Int32.to_int (Int32.add (Int32.of_int offset1) 8l) && offset1 mod 16 = 0
                 && offset1 >= 0 && offset1 <= 504 ->
            optimize (Symbolic.STP (src1, src2, ARM64.SP, offset1) :: acc) rest
        | Symbolic.STR_fp (src1, addr1, offset1)
            :: Symbolic.STR_fp (src2, addr2, offset2)
            :: rest
            when addr1 = ARM64.SP && addr2 = ARM64.SP
                 && offset2 = Int32.to_int (Int32.add (Int32.of_int offset1) 8l) && offset1 mod 16 = 0
                 && offset1 >= 0 && offset1 <= 504 ->
            optimize (Symbolic.STP_fp (src1, src2, ARM64.SP, offset1) :: acc) rest
        | instr :: rest ->
            optimize (instr :: acc) rest
    in optimize [] instrs
