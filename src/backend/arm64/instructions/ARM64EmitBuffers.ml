(*
   ARM64EmitBuffers.ml - Emit arm64 instructions for buffers operations.
*)
[@@@warning "-4"]

let bind f value = Result.bind value f

let physicalName = function
  | LIR.X0 -> "X0"
  | LIR.X1 -> "X1"
  | LIR.X2 -> "X2"
  | LIR.X3 -> "X3"
  | LIR.X4 -> "X4"
  | LIR.X5 -> "X5"
  | LIR.X6 -> "X6"
  | LIR.X7 -> "X7"
  | LIR.X8 -> "X8"
  | LIR.X9 -> "X9"
  | LIR.X10 -> "X10"
  | LIR.X11 -> "X11"
  | LIR.X12 -> "X12"
  | LIR.X13 -> "X13"
  | LIR.X14 -> "X14"
  | LIR.X15 -> "X15"
  | LIR.X16 -> "X16"
  | LIR.X17 -> "X17"
  | LIR.X19 -> "X19"
  | LIR.X20 -> "X20"
  | LIR.X21 -> "X21"
  | LIR.X22 -> "X22"
  | LIR.X23 -> "X23"
  | LIR.X24 -> "X24"
  | LIR.X25 -> "X25"
  | LIR.X26 -> "X26"
  | LIR.X27 -> "X27"
  | LIR.X29 -> "X29"
  | LIR.X30 -> "X30"
  | LIR.SP -> "SP"

let operandText operand =
  let open StructuralValue in
  let reg = function
    | LIR.Physical p -> Union ("Physical", [ Scalar (physicalName p) ])
    | LIR.Virtual n -> Union ("Virtual", [ Scalar (string_of_int n) ])
  in
  StructuralFormat.format
    (match operand with
    | LIR.Imm n -> Union ("Imm", [ Scalar (Int64.to_string n ^ "L") ])
    | LIR.FloatImm value ->
        Union ("FloatImm", [ Scalar (FloatFormat.structural value) ])
    | LIR.Reg r -> Union ("Reg", [ reg r ])
    | LIR.StackSlot n -> Union ("StackSlot", [ Scalar (string_of_int n) ])
    | LIR.StringSymbol text -> Union ("StringSymbol", [ Text text ])
    | LIR.FloatSymbol value ->
        Union ("FloatSymbol", [ Scalar (FloatFormat.structural value) ])
    | LIR.FuncAddr id -> Union ("FuncAddr", [ AST.DiagnosticFormatting.func id ]))

open ARM64CodeGenTypes
open HeapAllocation
open LeakAccounting
open ARM64Operands

(*
   Canonical buffers share the [refcount:8][length:8][data:N] layout. Compare the
   representation directly without allocating or calling stdlib code.
*)
let emitCanonicalBufferEq (ctx : codeGenContext)
    (kind : MemoryModel.canonicalBufferKind) (dest : LIR.reg)
    (left : LIR.operand) (right : LIR.operand) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      let materialize operand target =
        match operand with
        | LIR.Reg reg ->
            lirRegToARM64Reg reg
            |> Result.map (fun source ->
                if source = target then []
                else [ Symbolic.MOV_reg (target, source) ])
        | LIR.StringSymbol value ->
            let labelRef = stringDataLabel value in
            Ok
              [
                Symbolic.ADRP (target, labelRef);
                Symbolic.ADD_label (target, target, labelRef);
              ]
        | _ -> Error "CanonicalBufferEq requires StringSymbol or Reg operands"
      in
      let label suffix =
        Printf.sprintf "__canonical_buffer_eq_%s_%s_%s" ctx.functionName
          ctx.instructionSite suffix
      in
      let wordLoop = label "words" in
      let byteLoop = label "bytes" in
      let equalLabel = label "equal" in
      let unequalLabel = label "unequal" in
      let doneLabel = label "done" in
      materialize left Symbolic.X8
      |> bind (fun leftInstrs ->
          materialize right Symbolic.X9
          |> Result.map (fun rightInstrs ->
              leftInstrs @ rightInstrs
              @ [
                  Symbolic.CMP_reg (Symbolic.X8, Symbolic.X9);
                  Symbolic.B_cond_label (Symbolic.EQ, equalLabel);
                ]
              @ (if
                   kind = MemoryModel.NullableUtf8String
                   || kind = MemoryModel.NullableGraphemeCluster
                 then
                   [
                     Symbolic.CMP_imm (Symbolic.X8, 0);
                     Symbolic.B_cond_label (Symbolic.EQ, unequalLabel);
                     Symbolic.CMP_imm (Symbolic.X9, 0);
                     Symbolic.B_cond_label (Symbolic.EQ, unequalLabel);
                   ]
                 else [])
              @ [
                  Symbolic.LDR (Symbolic.X10, Symbolic.X8, 8);
                  Symbolic.LDR (Symbolic.X12, Symbolic.X9, 8);
                  Symbolic.CMP_reg (Symbolic.X10, Symbolic.X12);
                  Symbolic.B_cond_label (Symbolic.NE, unequalLabel);
                  Symbolic.ADD_imm (Symbolic.X8, Symbolic.X8, 16);
                  Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 16);
                  Symbolic.Label wordLoop;
                  Symbolic.CMP_imm (Symbolic.X10, 8);
                  Symbolic.B_cond_label (Symbolic.LT, byteLoop);
                  Symbolic.LDR (Symbolic.X11, Symbolic.X8, 0);
                  Symbolic.LDR (Symbolic.X13, Symbolic.X9, 0);
                  Symbolic.CMP_reg (Symbolic.X11, Symbolic.X13);
                  Symbolic.B_cond_label (Symbolic.NE, unequalLabel);
                  Symbolic.ADD_imm (Symbolic.X8, Symbolic.X8, 8);
                  Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 8);
                  Symbolic.SUB_imm (Symbolic.X10, Symbolic.X10, 8);
                  Symbolic.B_label wordLoop;
                  Symbolic.Label byteLoop;
                  Symbolic.CMP_imm (Symbolic.X10, 0);
                  Symbolic.B_cond_label (Symbolic.EQ, equalLabel);
                  Symbolic.LDRB_imm (Symbolic.X11, Symbolic.X8, 0);
                  Symbolic.LDRB_imm (Symbolic.X13, Symbolic.X9, 0);
                  Symbolic.CMP_reg (Symbolic.X11, Symbolic.X13);
                  Symbolic.B_cond_label (Symbolic.NE, unequalLabel);
                  Symbolic.ADD_imm (Symbolic.X8, Symbolic.X8, 1);
                  Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 1);
                  Symbolic.SUB_imm (Symbolic.X10, Symbolic.X10, 1);
                  Symbolic.B_label byteLoop;
                  Symbolic.Label equalLabel;
                  Symbolic.MOVZ (Symbolic.X11, 1, 0);
                  Symbolic.B_label doneLabel;
                  Symbolic.Label unequalLabel;
                  Symbolic.MOVZ (Symbolic.X11, 0, 0);
                  Symbolic.Label doneLabel;
                ]
              @
              if destReg = Symbolic.X11 then []
              else [ Symbolic.MOV_reg (destReg, Symbolic.X11) ])))

(*
   String concatenation:
   Dynamic and literal strings share [refcount:8][length:8][data:N].
   Register usage:
   X9  = left data address (for literal: string address, for heap: addr+8)
   X10 = left length
   X11 = right data address
   X12 = right length
   X13 = total length
   X14 = result pointer
   X15 = temp for byte copy
   Algorithm:
   1. Load left address and length into X9, X10
   2. Load right address and length into X11, X12
   3. Calculate total length: X13 = X10 + X12
   4. Allocate: total + 16 bytes using bump allocator
   5. Store refcount and total length in the fixed header
   6. Copy left bytes to [X14+16]
   7. Copy right bytes after the left bytes
   9. Move result to dest
   Helper: load operand address and length into registers
   Literal string: address via ADRP+ADD, length from UTF-8 bytes
   Skip the literal's fixed header to get its data address.
   Dynamic string: length at [reg+8], data at [reg+16].
   Load both operands
   Calculate total length
   Allocate: totalLen + 16 bytes (8 for length, 8 for refcount)
   Using bump allocator (X28 = bump pointer)
   X14 = total + 16
   Align up
   ~7 mask (lower bits)
   Bits 16-31
   Bits 32-47
   Bits 48-63
   X14 = aligned size
   X14 = current heap ptr (result)
   X15 = total + 16
   Align
   ~7 mask again (X15 was clobbered)
   Bump heap pointer
   Copy left bytes after the fixed header.
   IMPORTANT: Don't use X0-X7 as temps - they may hold function arguments!
   Strategy: Use pointer-bumping loops instead of indexed addressing
   X15 = source pointer (starts at X9, bumped each iteration)
   X16 = dest pointer (starts at X14+8, bumped each iteration)
   X13 = remaining count (starts at X10, decremented, reused since we stored total already)
   0: X15 = src ptr
   2: X13 = remaining = len1
   Loop: if X13 == 0, done (skip 7 instructions to exit past B at index 9)
   3: Skip 7 instructions if done -> index 10 (past end)
   4: X8 = byte at [X15]
   5: [X16] = byte
   6: X15++ (src ptr)
   7: X16++ (dest ptr)
   8: X13-- (remaining)
   9: Loop back to CBZ (index 3)
   Copy right bytes: loop copying X12 bytes from X11 to [X14+8+X10]
   X15 = source pointer (starts at X11)
   X16 = dest pointer (starts at X14+8+X10, already in X16 from copyLeft end)
   X13 = remaining count (use X12)
   Note: X16 is already at X14+8+len1 after copyLeft loop ends!
   0: X15 = src ptr (right string)
   1: X13 = remaining = len2
   Loop: if X13 == 0, done (skip 7 instructions to exit past B at index 8)
   2: Skip 7 instructions if done -> index 9 (past end)
   3: X8 = byte at [X15]
   4: [X16] = byte
   5: X15++ (src ptr)
   6: X16++ (dest ptr)
   7: X13-- (remaining)
   8: Loop back to CBZ (index 2)
   Move result to dest
*)
let emitStringConcatBinary (ctx : codeGenContext) (dest : LIR.reg)
    (left : LIR.operand) (right : LIR.operand) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      let loadOperandInfo (operand : LIR.operand) (addrReg : Symbolic.reg)
          (lenReg : Symbolic.reg) =
        match operand with
        | LIR.StringSymbol value ->
            let len = utf8Len value in
            let labelRef = stringDataLabel value in
            Ok
              ([
                 Symbolic.ADRP (addrReg, labelRef);
                 Symbolic.ADD_label (addrReg, addrReg, labelRef);
                 Symbolic.ADD_imm (addrReg, addrReg, 16);
               ]
              @ loadImmediate lenReg (Int64.of_int len))
        | LIR.Reg reg ->
            lirRegToARM64Reg reg
            |> Result.map (fun srcReg ->
                [
                  Symbolic.LDR (lenReg, srcReg, 8);
                  Symbolic.ADD_imm (addrReg, srcReg, 16);
                ])
        | other ->
            Error
              ("StringConcat requires StringSymbol or Reg operand, got: "
             ^ operandText other)
      in
      loadOperandInfo left Symbolic.X9 Symbolic.X10
      |> bind (fun leftInstrs ->
          loadOperandInfo right Symbolic.X11 Symbolic.X12
          |> Result.map (fun rightInstrs ->
              let calcTotal =
                [ Symbolic.ADD_reg (Symbolic.X13, Symbolic.X10, Symbolic.X12) ]
              in
              let allocate =
                [
                  Symbolic.ADD_imm (Symbolic.X14, Symbolic.X13, 16);
                  Symbolic.ADD_imm (Symbolic.X14, Symbolic.X14, 7);
                  Symbolic.MOVZ (Symbolic.X15, 0xFFF8, 0);
                  Symbolic.MOVK (Symbolic.X15, 0xFFFF, 16);
                  Symbolic.MOVK (Symbolic.X15, 0xFFFF, 32);
                  Symbolic.MOVK (Symbolic.X15, 0xFFFF, 48);
                  Symbolic.AND_reg (Symbolic.X14, Symbolic.X14, Symbolic.X15);
                  Symbolic.MOV_reg (Symbolic.X14, Symbolic.X28);
                  Symbolic.ADD_imm (Symbolic.X15, Symbolic.X13, 16);
                  Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 7);
                  Symbolic.MOVZ (Symbolic.X0, 0xFFF8, 0);
                  Symbolic.MOVK (Symbolic.X0, 0xFFFF, 16);
                  Symbolic.MOVK (Symbolic.X0, 0xFFFF, 32);
                  Symbolic.MOVK (Symbolic.X0, 0xFFFF, 48);
                  Symbolic.AND_reg (Symbolic.X15, Symbolic.X15, Symbolic.X0);
                  Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X15);
                ]
              in
              let storeHeader =
                [
                  Symbolic.MOVZ (Symbolic.X15, 1, 0);
                  Symbolic.STR (Symbolic.X15, Symbolic.X14, 0);
                  Symbolic.STR (Symbolic.X13, Symbolic.X14, 8);
                ]
              in
              let copyLeft =
                [
                  Symbolic.MOV_reg (Symbolic.X15, Symbolic.X9);
                  Symbolic.ADD_imm (Symbolic.X16, Symbolic.X14, 16);
                  Symbolic.MOV_reg (Symbolic.X13, Symbolic.X10);
                  Symbolic.CBZ_offset (Symbolic.X13, 7);
                  Symbolic.LDRB_imm (Symbolic.X8, Symbolic.X15, 0);
                  Symbolic.STRB_reg (Symbolic.X8, Symbolic.X16);
                  Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
                  Symbolic.ADD_imm (Symbolic.X16, Symbolic.X16, 1);
                  Symbolic.SUB_imm (Symbolic.X13, Symbolic.X13, 1);
                  Symbolic.B (-6);
                ]
              in
              let copyRight =
                [
                  Symbolic.MOV_reg (Symbolic.X15, Symbolic.X11);
                  Symbolic.MOV_reg (Symbolic.X13, Symbolic.X12);
                  Symbolic.CBZ_offset (Symbolic.X13, 7);
                  Symbolic.LDRB_imm (Symbolic.X8, Symbolic.X15, 0);
                  Symbolic.STRB_reg (Symbolic.X8, Symbolic.X16);
                  Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
                  Symbolic.ADD_imm (Symbolic.X16, Symbolic.X16, 1);
                  Symbolic.SUB_imm (Symbolic.X13, Symbolic.X13, 1);
                  Symbolic.B (-6);
                ]
              in
              let moveResult = [ Symbolic.MOV_reg (destReg, Symbolic.X14) ] in
              leftInstrs @ rightInstrs @ calcTotal @ allocate @ storeHeader
              @ copyLeft @ copyRight @ moveResult @ generateLeakCounterInc ctx)))

(*
   Lower a concat tree as one length pass, one allocation, and one ordered copy pass.
*)
let emitStringConcatMany (ctx : codeGenContext) (dest : LIR.reg)
    (first : LIR.operand) (second : LIR.operand) (remaining : LIR.operand list)
    =
  let operands = first :: second :: remaining in
  let loadOperandInfo (operand : LIR.operand) =
    match operand with
    | LIR.StringSymbol value ->
        let labelRef = stringDataLabel value in
        Ok
          ([
             Symbolic.ADRP (Symbolic.X9, labelRef);
             Symbolic.ADD_label (Symbolic.X9, Symbolic.X9, labelRef);
             Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 16);
           ]
          @ loadImmediate Symbolic.X10 (Int64.of_int (utf8Len value)))
    | LIR.Reg reg ->
        lirRegToARM64Reg reg
        |> Result.map (fun source ->
            [
              Symbolic.LDR (Symbolic.X10, source, 8);
              Symbolic.ADD_imm (Symbolic.X9, source, 16);
            ])
    | LIR.StackSlot offset ->
        loadStackSlot Symbolic.X9 offset
        |> Result.map (fun loads ->
            loads
            @ [
                Symbolic.LDR (Symbolic.X10, Symbolic.X9, 8);
                Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 16);
              ])
    | other ->
        Error
          ("StringConcat requires string operands, got: " ^ operandText other)
  in
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      operands
      |> ResultList.mapResults loadOperandInfo
      |> Result.map (fun operandLoads ->
          let measure =
            [ Symbolic.MOVZ (Symbolic.X13, 0, 0) ]
            @ (operandLoads
              |> List.concat_map (fun loads ->
                  loads
                  @ [
                      Symbolic.ADD_reg (Symbolic.X13, Symbolic.X13, Symbolic.X10);
                    ]))
          in
          let allocate =
            [
              Symbolic.ADD_imm (Symbolic.X15, Symbolic.X13, 23);
              Symbolic.MOVZ (Symbolic.X17, 0xFFF8, 0);
              Symbolic.MOVK (Symbolic.X17, 0xFFFF, 16);
              Symbolic.MOVK (Symbolic.X17, 0xFFFF, 32);
              Symbolic.MOVK (Symbolic.X17, 0xFFFF, 48);
              Symbolic.AND_reg (Symbolic.X15, Symbolic.X15, Symbolic.X17);
              Symbolic.MOV_reg (Symbolic.X14, Symbolic.X28);
              Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X15);
              Symbolic.MOVZ (Symbolic.X15, 1, 0);
              Symbolic.STR (Symbolic.X15, Symbolic.X14, 0);
              Symbolic.STR (Symbolic.X13, Symbolic.X14, 8);
              Symbolic.ADD_imm (Symbolic.X16, Symbolic.X14, 16);
            ]
          in
          let copyOne loads =
            loads
            @ [
                Symbolic.MOV_reg (Symbolic.X15, Symbolic.X9);
                Symbolic.MOV_reg (Symbolic.X13, Symbolic.X10);
                Symbolic.CBZ_offset (Symbolic.X13, 7);
                Symbolic.LDRB_imm (Symbolic.X8, Symbolic.X15, 0);
                Symbolic.STRB_reg (Symbolic.X8, Symbolic.X16);
                Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
                Symbolic.ADD_imm (Symbolic.X16, Symbolic.X16, 1);
                Symbolic.SUB_imm (Symbolic.X13, Symbolic.X13, 1);
                Symbolic.B (-6);
              ]
          in
          measure @ allocate
          @ (operandLoads |> List.concat_map copyOne)
          @ [ Symbolic.MOV_reg (destReg, Symbolic.X14) ]
          @ generateLeakCounterInc ctx))

let emitStringConcat ctx dest first second remaining =
  match remaining with
  | [] -> emitStringConcatBinary ctx dest first second
  | _ -> emitStringConcatMany ctx dest first second remaining
