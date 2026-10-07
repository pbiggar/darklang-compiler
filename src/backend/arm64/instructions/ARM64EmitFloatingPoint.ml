(*
   ARM64EmitFloatingPoint.ml - Emit arm64 instructions for floatingpoint operations.
*)
[@@@warning "-4"]

let bind f value = Result.bind value f
let neg n = Int32.to_int (Int32.neg (Int32.of_int n))

open ARM64CodeGenTypes
open HeapAllocation
open LeakAccounting
open ARM64Operands

(*
   Float phi nodes should be eliminated before code generation (by register allocation)
*)
let emitFPhi (_ctx : codeGenContext) =
  Error "Float phi nodes should be eliminated before code generation"

(*
   Float argument moves - move float values to D0-D7
   Uses parallel move resolution to handle register conflicts correctly
   First, convert all source FRegs to ARM64 FRegs
   Use ParallelMoves.resolve to get the correct move order
   We treat ARM64Symbolic.FReg as both dest and src type
   Convert actions to ARM64 instructions
   Use reserved D16 as the temporary register for cycle breaking.
   Save to D16 (temp) - using upper SIMD register
   Move from D16 (temp) to destination
*)
let emitFArgMoves (_ctx : codeGenContext)
    (moves : (LIR.physFPReg * LIR.fReg) list) =
  let resolvedMoves =
    moves
    |> List.map (fun (destPhysReg, srcFReg) ->
        let destARM64 = lirPhysFPRegToARM64FReg destPhysReg in
        match lirFRegToARM64FReg srcFReg with
        | Ok srcARM64 -> Ok (destARM64, srcARM64)
        | Error e -> Error e)
    |> List.fold_left
         (fun acc r ->
           match (acc, r) with
           | Ok moves, Ok move -> Ok (move :: moves)
           | Error e, _ -> Error e
           | _, Error e -> Error e)
         (Ok [])
    |> Result.map List.rev
  in
  match resolvedMoves with
  | Error e -> Error e
  | Ok armMoves ->
      let getSrcReg (srcReg : Symbolic.fReg) = Some srcReg in
      let actions = ParallelMoves.resolve armMoves getSrcReg in
      actions
      |> List.concat_map (function
        | ParallelMoves.SaveToTemp srcReg ->
            [ Symbolic.FMOV_reg (Symbolic.D16, srcReg) ]
        | ParallelMoves.Move (dest, src) ->
            if dest <> src then [ Symbolic.FMOV_reg (dest, src) ] else []
        | ParallelMoves.MoveFromTemp dest ->
            [ Symbolic.FMOV_reg (dest, Symbolic.D16) ])
      |> fun value -> Ok value

let emitFMov (_ctx : codeGenContext) (dest : LIR.fReg) (src : LIR.fReg) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg src
      |> Result.map (fun srcReg -> [ Symbolic.FMOV_reg (destReg, srcReg) ]))

(*
   Load page address of float
   Add page offset
   Load float from [X9]
*)
let emitFLoad (_ctx : codeGenContext) (dest : LIR.fReg) (value : float) =
  lirFRegToARM64FReg dest
  |> Result.map (fun destReg ->
      if Int64.bits_of_float value = 0L then [ Symbolic.FMOV_zero destReg ]
      else if Option.is_some (ARM64.tryEncodeFmovFloatImmediate value) then
        [ Symbolic.FMOV_imm (destReg, value) ]
      else
        let labelRef = floatDataLabel value in
        [
          Symbolic.ADRP (Symbolic.X9, labelRef);
          Symbolic.ADD_label (Symbolic.X9, Symbolic.X9, labelRef);
          Symbolic.LDR_fp (destReg, Symbolic.X9, 0);
        ])

let floatStackAddress (stackSlot : int) =
  if stackSlot < 0 && neg stackSlot <= 4095 then
    Ok
      [
        Symbolic.SUB_imm (Symbolic.X10, Symbolic.X29, neg stackSlot land 65535);
      ]
  else if stackSlot >= 0 && stackSlot <= 4095 then
    Ok [ Symbolic.ADD_imm (Symbolic.X10, Symbolic.X29, stackSlot land 65535) ]
  else
    Error
      (Printf.sprintf
         "Float stack offset %d exceeds supported range (-4095 to +4095)"
         stackSlot)

let emitFSpillLoad (_ctx : codeGenContext) (dest : LIR.fReg) (stackSlot : int) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      floatStackAddress stackSlot
      |> Result.map (fun address ->
          address @ [ Symbolic.LDR_fp (destReg, Symbolic.X10, 0) ]))

let emitFSpillStore (_ctx : codeGenContext) (stackSlot : int) (src : LIR.fReg) =
  lirFRegToARM64FReg src
  |> bind (fun srcReg ->
      floatStackAddress stackSlot
      |> Result.map (fun address ->
          address @ [ Symbolic.STR_fp (srcReg, Symbolic.X10, 0) ]))

let emitFAdd (_ctx : codeGenContext) (dest : LIR.fReg) (left : LIR.fReg)
    (right : LIR.fReg) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg left
      |> bind (fun leftReg ->
          lirFRegToARM64FReg right
          |> Result.map (fun rightReg ->
              [ Symbolic.FADD (destReg, leftReg, rightReg) ])))

let emitFSub (_ctx : codeGenContext) (dest : LIR.fReg) (left : LIR.fReg)
    (right : LIR.fReg) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg left
      |> bind (fun leftReg ->
          lirFRegToARM64FReg right
          |> Result.map (fun rightReg ->
              [ Symbolic.FSUB (destReg, leftReg, rightReg) ])))

let emitFMul (_ctx : codeGenContext) (dest : LIR.fReg) (left : LIR.fReg)
    (right : LIR.fReg) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg left
      |> bind (fun leftReg ->
          lirFRegToARM64FReg right
          |> Result.map (fun rightReg ->
              [ Symbolic.FMUL (destReg, leftReg, rightReg) ])))

let emitFMadd (_ctx : codeGenContext) (dest : LIR.fReg) (left : LIR.fReg)
    (right : LIR.fReg) (addend : LIR.fReg) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg left
      |> bind (fun leftReg ->
          lirFRegToARM64FReg right
          |> bind (fun rightReg ->
              lirFRegToARM64FReg addend
              |> Result.map (fun addendReg ->
                  [ Symbolic.FMADD (destReg, leftReg, rightReg, addendReg) ]))))

let emitFDiv (_ctx : codeGenContext) (dest : LIR.fReg) (left : LIR.fReg)
    (right : LIR.fReg) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg left
      |> bind (fun leftReg ->
          lirFRegToARM64FReg right
          |> Result.map (fun rightReg ->
              [ Symbolic.FDIV (destReg, leftReg, rightReg) ])))

let emitFNeg (_ctx : codeGenContext) (dest : LIR.fReg) (src : LIR.fReg) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg src
      |> Result.map (fun srcReg -> [ Symbolic.FNEG (destReg, srcReg) ]))

let emitFAbs (_ctx : codeGenContext) (dest : LIR.fReg) (src : LIR.fReg) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg src
      |> Result.map (fun srcReg -> [ Symbolic.FABS (destReg, srcReg) ]))

let emitFSqrt (_ctx : codeGenContext) (dest : LIR.fReg) (src : LIR.fReg) =
  lirFRegToARM64FReg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg src
      |> Result.map (fun srcReg -> [ Symbolic.FSQRT (destReg, srcReg) ]))

let emitFCmp (_ctx : codeGenContext) (left : LIR.fReg) (right : LIR.fReg) =
  lirFRegToARM64FReg left
  |> bind (fun leftReg ->
      lirFRegToARM64FReg right
      |> Result.map (fun rightReg -> [ Symbolic.FCMP (leftReg, rightReg) ]))

let emitFloatToInt64 (_ctx : codeGenContext) (dest : LIR.reg) (src : LIR.fReg) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg src
      |> Result.map (fun srcReg -> [ Symbolic.FCVTZS (destReg, srcReg) ]))

(*
   Move bits from FP register to GP register (for floats stored to list)
*)
let emitFpToGp (_ctx : codeGenContext) (dest : LIR.reg) (src : LIR.fReg) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg src
      |> Result.map (fun srcReg -> [ Symbolic.FMOV_to_gp (destReg, srcReg) ]))

(*
   Copy Float64 bits to UInt64 (uses FMOV to GP register)
*)
let emitFloatToBits (_ctx : codeGenContext) (dest : LIR.reg) (src : LIR.fReg) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg src
      |> Result.map (fun srcReg -> [ Symbolic.FMOV_to_gp (destReg, srcReg) ]))

(*
   Heap operations
   Convert float in FP register to heap string
*)
let emitFloatToString (ctx : codeGenContext) (dest : LIR.reg) (value : LIR.fReg)
    =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      lirFRegToARM64FReg value
      |> Result.map (fun valueReg ->
          runtimeInstrs (FloatFormatting.generateFloatToString destReg valueReg)
          @ generateLeakCounterInc ctx))
