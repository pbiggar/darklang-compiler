(* X64EmitInteger.ml - Emit x64 instructions for integer operations. *)
[@@@warning "-4"]

open X64Operands
open X64CodeGenTypes
open X64InstructionContext
module X = X86_64

let bind f value = Result.bind value f
let add32 a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let mul32 a b = Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))

module PhysMapBase = Map.Make (struct
  type t = LIR.physReg

  let compare = Stdlib.compare
end)

module PhysMap = struct
  include PhysMapBase

  let ofList xs = List.fold_left (fun m (k, v) -> add k v m) empty xs
end

let distinct xs =
  List.fold_left
    (fun kept x -> if List.mem x kept then kept else kept @ [ x ])
    [] xs

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

let emitMov (ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.operand)
    =
  resolveReg dest
  |> bind (fun destReg ->
      match src with
      | LIR.Imm value -> Ok (loadImm64 destReg value)
      | LIR.Reg srcReg ->
          resolveReg srcReg
          |> Result.map (fun srcX86 ->
              if destReg = srcX86 then [] else [ X.MOV_reg (destReg, srcX86) ])
      | LIR.StackSlot offset ->
          Ok
            [
              X.MOV_load
                ( destReg,
                  X.RBP,
                  Int32.of_int
                    (X64InstructionContext.adjustStackOffset ctx offset) );
            ]
      | LIR.StringSymbol value -> Ok (emitStringLiteral destReg value)
      | LIR.FuncAddr funcName ->
          Ok [ X.LEA_rip (destReg, functionName ctx funcName) ]
      | LIR.FloatImm value | LIR.FloatSymbol value ->
          (*  Store float bits in GP register *)
          let bits = Int64.bits_of_float value in
          Ok (loadImm64 destReg bits))

let emitStore (ctx : X64CodeGenTypes.funcCtx) (stackSlot : int) (src : LIR.reg)
    =
  (*  Stack slots are byte offsets from FP, adjusted past callee-saved pushes *)
  resolveReg src
  |> Result.map (fun srcReg ->
      [
        X.MOV_store
          ( X.RBP,
            Int32.of_int (X64InstructionContext.adjustStackOffset ctx stackSlot),
            srcReg );
      ])

let emitAdd (ctx : X64CodeGenTypes.funcCtx)
    (comparisonContext : X64InstructionContext.comparisonContext option)
    (dest : LIR.reg) (left : LIR.reg) (right : LIR.operand) =
  resolveReg dest
  |> bind (fun destReg ->
      resolveReg left
      |> bind (fun leftReg ->
          match right with
          | LIR.Imm value when value >= -2147483648L && value <= 2147483647L ->
              if Option.is_none comparisonContext && destReg <> leftReg then
                Ok [ X.LEA (destReg, leftReg, Int64.to_int32 value) ]
              else
                let setup =
                  if destReg <> leftReg then [ X.MOV_reg (destReg, leftReg) ]
                  else []
                in
                Ok (setup @ [ X.ADD_imm (destReg, Int64.to_int32 value) ])
          | LIR.Imm value ->
              if destReg = scratch then
                (*  dest is R11: can't use scratch for imm. Use PUSH/POP RCX. *)
                Ok
                  ([ X.PUSH X.RCX ] @ loadImm64 X.RCX value
                  @ [ X.ADD_reg (destReg, X.RCX); X.POP X.RCX ])
              else
                let setup =
                  if destReg <> leftReg then [ X.MOV_reg (destReg, leftReg) ]
                  else []
                in
                Ok
                  (setup @ loadImm64 scratch value
                  @ [ X.ADD_reg (destReg, scratch) ])
          | LIR.Reg rightReg ->
              resolveReg rightReg
              |> Result.map (fun rightX86 ->
                  if destReg = rightX86 && destReg <> leftReg then
                    (*  dest is right operand: ADD is commutative, so just swap *)
                    [ X.ADD_reg (destReg, leftReg) ]
                  else if
                    Option.is_none comparisonContext
                    && destReg <> leftReg && destReg <> rightX86
                    && rightX86 <> X.RSP && rightX86 <> X.R12
                  then [ X.LEA_index (destReg, leftReg, rightX86, 1, 0l) ]
                  else
                    let setup =
                      if destReg <> leftReg then
                        [ X.MOV_reg (destReg, leftReg) ]
                      else []
                    in
                    setup @ [ X.ADD_reg (destReg, rightX86) ])
          | LIR.StackSlot offset ->
              let adjOff =
                Int32.of_int
                  (X64InstructionContext.adjustStackOffset ctx offset)
              in
              if destReg = scratch && leftReg = scratch then
                (*  Both dest and left are R11: use PUSH/POP to avoid clobbering *)
                Ok
                  [
                    X.PUSH X.RCX;
                    X.MOV_load (X.RCX, X.RBP, adjOff);
                    X.ADD_reg (destReg, X.RCX);
                    X.POP X.RCX;
                  ]
              else
                let setup =
                  if destReg <> leftReg then [ X.MOV_reg (destReg, leftReg) ]
                  else []
                in
                Ok (setup @ [ X.ADD_load (destReg, X.RBP, adjOff) ])
          | _ -> Error ("Unsupported Add right operand: " ^ operandText right)))

let emitSub (ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (left : LIR.reg)
    (right : LIR.operand) =
  resolveReg dest
  |> bind (fun destReg ->
      resolveReg left
      |> bind (fun leftReg ->
          match right with
          | LIR.Imm value when value >= -2147483648L && value <= 2147483647L ->
              let setup =
                if destReg <> leftReg then [ X.MOV_reg (destReg, leftReg) ]
                else []
              in
              Ok (setup @ [ X.SUB_imm (destReg, Int64.to_int32 value) ])
          | LIR.Imm value ->
              if destReg = scratch then
                Ok
                  ([ X.PUSH X.RCX ] @ loadImm64 X.RCX value
                  @ [ X.SUB_reg (destReg, X.RCX); X.POP X.RCX ])
              else
                let setup =
                  if destReg <> leftReg then [ X.MOV_reg (destReg, leftReg) ]
                  else []
                in
                Ok
                  (setup @ loadImm64 scratch value
                  @ [ X.SUB_reg (destReg, scratch) ])
          | LIR.Reg rightReg ->
              resolveReg rightReg
              |> Result.map (fun rightX86 ->
                  if destReg = rightX86 && destReg <> leftReg then
                    if destReg = scratch then
                      (*  dest=right=R11, left is different: use PUSH/POP *)
                      [
                        X.PUSH X.RCX;
                        X.MOV_reg (X.RCX, leftReg);
                        X.SUB_reg (X.RCX, rightX86);
                        X.MOV_reg (destReg, X.RCX);
                        X.POP X.RCX;
                      ]
                    else
                      [
                        X.MOV_reg (scratch, leftReg);
                        X.SUB_reg (scratch, rightX86);
                        X.MOV_reg (destReg, scratch);
                      ]
                  else
                    let setup =
                      if destReg <> leftReg then
                        [ X.MOV_reg (destReg, leftReg) ]
                      else []
                    in
                    setup @ [ X.SUB_reg (destReg, rightX86) ])
          | LIR.StackSlot offset ->
              let adjOff =
                Int32.of_int
                  (X64InstructionContext.adjustStackOffset ctx offset)
              in
              if destReg = scratch && leftReg = scratch then
                Ok
                  [
                    X.PUSH X.RCX;
                    X.MOV_load (X.RCX, X.RBP, adjOff);
                    X.SUB_reg (destReg, X.RCX);
                    X.POP X.RCX;
                  ]
              else
                let setup =
                  if destReg <> leftReg then [ X.MOV_reg (destReg, leftReg) ]
                  else []
                in
                Ok (setup @ [ X.SUB_load (destReg, X.RBP, adjOff) ])
          | _ -> Error ("Unsupported Sub right operand: " ^ operandText right)))

let emitMul (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (left : LIR.reg)
    (right : LIR.reg) =
  (*  x86_64 IMUL r64, r/m64 — dest = dest * src *)
  (*  Must handle case where dest == right (would clobber right when setting up left) *)
  resolveReg dest
  |> bind (fun destReg ->
      resolveReg left
      |> bind (fun leftReg ->
          resolveReg right
          |> Result.map (fun rightReg ->
              if destReg = rightReg && destReg <> leftReg then
                if destReg = scratch then
                  (*  dest=right=R11, left different: MUL is commutative, swap *)
                  [ X.IMUL_reg (destReg, leftReg) ]
                else
                  [
                    X.MOV_reg (scratch, leftReg);
                    X.IMUL_reg (scratch, rightReg);
                    X.MOV_reg (destReg, scratch);
                  ]
              else
                let setup =
                  if destReg <> leftReg then [ X.MOV_reg (destReg, leftReg) ]
                  else []
                in
                setup @ [ X.IMUL_reg (destReg, rightReg) ])))

let emitSdiv (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (left : LIR.reg)
    (right : LIR.reg) =
  (*  IDIV: RDX:RAX / src → RAX=quotient, RDX=remainder. *)
  (*  Clobbers both RAX and RDX. Save/restore RDX using the red zone *)
  (*  (below RSP) to avoid changing RSP. *)
  (*  Special case: INT64_MIN / -1 traps with #DE (SIGFPE). LIR.Sdiv *)
  (*  is defined to wrap to INT64_MIN, so detect the case and bypass. *)
  resolveReg dest
  |> bind (fun destReg ->
      resolveReg left
      |> bind (fun leftReg ->
          resolveReg right
          |> Result.map (fun rightReg ->
              let overflowLabel = freshLabel "idiv_overflow" in
              let doneLabel = freshLabel "idiv_done" in
              let divisor =
                if rightReg = X.RAX || rightReg = X.RDX then scratch
                else rightReg
              in
              let saveDivisor =
                if rightReg = X.RAX || rightReg = X.RDX then
                  [ X.MOV_reg (scratch, rightReg) ]
                else []
              in
              let moveLeft =
                if leftReg <> X.RAX then [ X.MOV_reg (X.RAX, leftReg) ] else []
                (*  Check for INT64_MIN / -1 overflow *)
              in
              saveDivisor @ moveLeft
              @ [ X.CMP_imm (divisor, -1l) ]
              @ [ X.Jcc (X.NE, doneLabel) ]
              (*  divisor is -1, check if dividend is INT64_MIN *)
              @ loadImm64 scratch Int64.min_int
              @ [ X.CMP_reg (X.RAX, scratch) ]
              @ [ X.Jcc (X.EQ, overflowLabel) ]
              (*  Normal IDIV path *)
              @ [ X.Label doneLabel ]
              (*  Restore divisor if it was moved to scratch for the CMP *)
              @ (if rightReg = X.RAX || rightReg = X.RDX then
                   [ X.MOV_reg (scratch, divisor) ]
                   (*  re-setup (was clobbered by INT64_MIN load) *)
                 else [])
              @ (if rightReg = X.RAX || rightReg = X.RDX then
                   [ X.MOV_reg (scratch, rightReg) ]
                 else [])
              @ (if leftReg <> X.RAX then [ X.MOV_reg (X.RAX, leftReg) ] else [])
              @ [ X.MOV_store (X.RSP, -8l, X.RDX) ]
              @ [ X.CQO; X.IDIV divisor ]
              @ (if destReg = X.RDX then [ X.MOV_reg (scratch, X.RAX) ]
                 else if destReg <> X.RAX then [ X.MOV_reg (destReg, X.RAX) ]
                 else [])
              @ [ X.MOV_load (X.RDX, X.RSP, -8l) ]
              @ (if destReg = X.RDX then [ X.MOV_reg (X.RDX, scratch) ] else [])
              @ [ X.JMP (overflowLabel ^ "_end") ]
              (*  Overflow path: return INT64_MIN *)
              @ [ X.Label overflowLabel ]
              @ loadImm64 destReg Int64.min_int
              @ [ X.Label (overflowLabel ^ "_end") ])))

let emitUdiv (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (left : LIR.reg)
    (right : LIR.reg) =
  resolveReg dest
  |> bind (fun destReg ->
      resolveReg left
      |> bind (fun leftReg ->
          resolveReg right
          |> Result.map (fun rightReg ->
              let divisor =
                if rightReg = X.RAX || rightReg = X.RDX then scratch
                else rightReg
              in
              let saveDivisor =
                if rightReg = X.RAX || rightReg = X.RDX then
                  [ X.MOV_reg (scratch, rightReg) ]
                else []
              in
              saveDivisor
              @ (if leftReg <> X.RAX then [ X.MOV_reg (X.RAX, leftReg) ] else [])
              @ [
                  X.MOV_store (X.RSP, -8l, X.RDX);
                  X.XOR_reg (X.RDX, X.RDX);
                  X.DIV divisor;
                ]
              @ (if destReg = X.RDX then [ X.MOV_reg (scratch, X.RAX) ]
                 else if destReg <> X.RAX then [ X.MOV_reg (destReg, X.RAX) ]
                 else [])
              @ [ X.MOV_load (X.RDX, X.RSP, -8l) ]
              @ if destReg = X.RDX then [ X.MOV_reg (X.RDX, scratch) ] else [])))

let emitMsub (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg)
    (mulLeft : LIR.reg) (mulRight : LIR.reg) (sub : LIR.reg) =
  (*  dest = sub - mulLeft * mulRight *)
  (*  No fused instruction on x86_64. Preserve a nonoperand temporary so *)
  (*  physical X11 remains valid as an allocated destination. *)
  resolveReg dest
  |> bind (fun destReg ->
      resolveReg mulLeft
      |> bind (fun mlReg ->
          resolveReg mulRight
          |> bind (fun mrReg ->
              resolveReg sub
              |> Result.map (fun subReg ->
                  let temp =
                    arithmeticTempExcluding [ destReg; mlReg; mrReg; subReg ]
                  in
                  [
                    X.PUSH temp;
                    X.MOV_reg (temp, mlReg);
                    X.IMUL_reg (temp, mrReg);
                  ]
                  @ (if destReg <> subReg then [ X.MOV_reg (destReg, subReg) ]
                     else [])
                  @ [ X.SUB_reg (destReg, temp); X.POP temp ]))))

let emitCmp (ctx : X64CodeGenTypes.funcCtx) (left : LIR.reg)
    (right : LIR.operand) =
  resolveReg left
  |> bind (fun leftReg ->
      match right with
      | LIR.Imm value when value >= -2147483648L && value <= 2147483647L ->
          Ok [ X.CMP_imm (leftReg, Int64.to_int32 value) ]
      | LIR.Imm value ->
          if leftReg = scratch then
            Ok
              ([ X.PUSH X.RCX ] @ loadImm64 X.RCX value
              @ [ X.CMP_reg (leftReg, X.RCX); X.POP X.RCX ])
          else Ok (loadImm64 scratch value @ [ X.CMP_reg (leftReg, scratch) ])
      | LIR.Reg rightReg ->
          resolveReg rightReg
          |> Result.map (fun rightX86 ->
              if leftReg = scratch && rightX86 = scratch then
                (*  Both are R11 - always equal, just emit CMP R11, R11 *)
                [ X.CMP_reg (scratch, scratch) ]
              else [ X.CMP_reg (leftReg, rightX86) ])
      | LIR.StackSlot offset ->
          let adjOff =
            Int32.of_int (X64InstructionContext.adjustStackOffset ctx offset)
          in
          if leftReg = scratch then
            Ok
              [
                X.PUSH X.RCX;
                X.MOV_load (X.RCX, X.RBP, adjOff);
                X.CMP_reg (leftReg, X.RCX);
                X.POP X.RCX;
              ]
          else
            Ok
              [
                X.MOV_load (scratch, X.RBP, adjOff); X.CMP_reg (leftReg, scratch);
              ]
      | _ -> Error ("Unsupported Cmp right operand: " ^ operandText right))

let emitCset (_ctx : X64CodeGenTypes.funcCtx)
    (comparisonContext : X64InstructionContext.comparisonContext option)
    (dest : LIR.reg) (cond : LIR.condition) =
  match comparisonContext with
  | None ->
      Error "x64 codegen: Cset without a preceding comparison in the same block"
  | Some comparisonContext ->
      resolveReg dest
      |> Result.map (fun destReg ->
          if comparisonContext = FloatComparison then
            match cond with
            | LIR.EQ ->
                (*  Float EQ: ordered AND equal (ZF=1 AND PF=0) *)
                (*  SETE + SETNP, then AND *)
                [
                  X.SETcc (X.EQ, destReg);
                  X.MOVZX_byte (destReg, destReg);
                  X.SETcc (X.NP, scratch);
                  X.MOVZX_byte (scratch, scratch);
                  X.AND_reg (destReg, scratch);
                ]
            | LIR.NE ->
                (*  Float NE: unordered OR not equal (ZF=0 OR PF=1) *)
                (*  SETNE + SETP, then OR *)
                [
                  X.SETcc (X.NE, destReg);
                  X.MOVZX_byte (destReg, destReg);
                  X.SETcc (X.P, scratch);
                  X.MOVZX_byte (scratch, scratch);
                  X.OR_reg (destReg, scratch);
                ]
            | LIR.LT | LIR.ULT ->
                [ X.SETcc (X.B, destReg); X.MOVZX_byte (destReg, destReg) ]
            | LIR.GT | LIR.UGT ->
                [ X.SETcc (X.A, destReg); X.MOVZX_byte (destReg, destReg) ]
            | LIR.LE | LIR.ULE ->
                [ X.SETcc (X.BE, destReg); X.MOVZX_byte (destReg, destReg) ]
            | LIR.GE | LIR.UGE ->
                [ X.SETcc (X.AE, destReg); X.MOVZX_byte (destReg, destReg) ]
          else
            let x86Cond =
              match cond with
              | LIR.EQ -> X.EQ
              | LIR.NE -> X.NE
              | LIR.LT -> X.LT
              | LIR.GT -> X.GT
              | LIR.LE -> X.LE
              | LIR.GE -> X.GE
              | LIR.ULT -> X.B
              | LIR.UGT -> X.A
              | LIR.ULE -> X.BE
              | LIR.UGE -> X.AE
            in
            [ X.SETcc (x86Cond, destReg); X.MOVZX_byte (destReg, destReg) ])

let emitSelect (_ctx : X64CodeGenTypes.funcCtx)
    (comparisonContext : X64InstructionContext.comparisonContext option)
    (dest : LIR.reg) (whenTrue : LIR.reg) (whenFalse : LIR.reg)
    (cond : LIR.condition) =
  match comparisonContext with
  | Some IntegerComparison ->
      resolveReg dest
      |> bind (fun destReg ->
          resolveReg whenTrue
          |> bind (fun trueReg ->
              resolveReg whenFalse
              |> Result.map (fun falseReg ->
                  let condition =
                    match cond with
                    | LIR.EQ -> X.EQ
                    | LIR.NE -> X.NE
                    | LIR.LT -> X.LT
                    | LIR.GT -> X.GT
                    | LIR.LE -> X.LE
                    | LIR.GE -> X.GE
                    | LIR.ULT -> X.B
                    | LIR.UGT -> X.A
                    | LIR.ULE -> X.BE
                    | LIR.UGE -> X.AE
                  in
                  let inverse =
                    match condition with
                    | X.EQ -> X.NE
                    | X.NE -> X.EQ
                    | X.LT -> X.GE
                    | X.GE -> X.LT
                    | X.GT -> X.LE
                    | X.LE -> X.GT
                    | X.B -> X.AE
                    | X.AE -> X.B
                    | X.A -> X.BE
                    | X.BE -> X.A
                    | X.P -> X.NP
                    | X.NP -> X.P
                  in
                  if trueReg = falseReg then
                    if destReg = trueReg then []
                    else [ X.MOV_reg (destReg, trueReg) ]
                  else if destReg = trueReg then
                    [ X.CMOVcc (inverse, destReg, falseReg) ]
                  else
                    (if destReg = falseReg then []
                     else [ X.MOV_reg (destReg, falseReg) ])
                    @ [ X.CMOVcc (condition, destReg, trueReg) ])))
  | Some FloatComparison ->
      Error "x64 codegen: integer Select after floating-point comparison"
  | None ->
      Error
        "x64 codegen: Select without a preceding comparison in the same block"

let emitAnd (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (left : LIR.reg)
    (right : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg left
      |> bind (fun l ->
          resolveReg right
          |> Result.map (fun r ->
              if d = r && d <> l then [ X.AND_reg (d, l) ]
                (*  AND is commutative *)
              else
                (if d <> l then [ X.MOV_reg (d, l) ] else [])
                @ [ X.AND_reg (d, r) ])))

let emitAnd_imm (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg)
    (src : LIR.reg) (imm : int64) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src
      |> Result.map (fun s ->
          let setup = if d <> s then [ X.MOV_reg (d, s) ] else [] in
          if imm >= -2147483648L && imm <= 2147483647L then
            setup @ [ X.AND_imm (d, Int64.to_int32 imm) ]
          else if d = scratch then
            [ X.PUSH X.RCX ] @ setup @ loadImm64 X.RCX imm
            @ [ X.AND_reg (d, X.RCX); X.POP X.RCX ]
          else setup @ loadImm64 scratch imm @ [ X.AND_reg (d, scratch) ]))

let emitOrr (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (left : LIR.reg)
    (right : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg left
      |> bind (fun l ->
          resolveReg right
          |> Result.map (fun r ->
              if d = r && d <> l then [ X.OR_reg (d, l) ]
                (*  OR is commutative *)
              else
                (if d <> l then [ X.MOV_reg (d, l) ] else [])
                @ [ X.OR_reg (d, r) ])))

let emitEor (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (left : LIR.reg)
    (right : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg left
      |> bind (fun l ->
          resolveReg right
          |> Result.map (fun r ->
              if d = r && d <> l then [ X.XOR_reg (d, l) ]
                (*  XOR is commutative *)
              else
                (if d <> l then [ X.MOV_reg (d, l) ] else [])
                @ [ X.XOR_reg (d, r) ])))

let emitLsl_imm (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg)
    (src : LIR.reg) (shift : int) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src
      |> Result.map (fun s ->
          (if d <> s then [ X.MOV_reg (d, s) ] else [])
          @ [ X.SHL_imm (d, shift) ]))

let emitLsr_imm (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg)
    (src : LIR.reg) (shift : int) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src
      |> Result.map (fun s ->
          (if d <> s then [ X.MOV_reg (d, s) ] else [])
          @ [ X.SHR_imm (d, shift) ]))

let emitAsr_imm (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg)
    (src : LIR.reg) (shift : int) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src
      |> Result.map (fun s ->
          (if d <> s then [ X.MOV_reg (d, s) ] else [])
          @ [ X.SAR_imm (d, shift) ]))

let emitNeg (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src
      |> Result.map (fun s ->
          (if d <> s then [ X.MOV_reg (d, s) ] else []) @ [ X.NEG d ]))

let emitMvn (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src
      |> Result.map (fun s ->
          (if d <> s then [ X.MOV_reg (d, s) ] else []) @ [ X.NOT d ]))

let emitSxtb (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src |> Result.map (fun s -> [ X.MOVSX_byte (d, s) ]))

let emitSxth (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src |> Result.map (fun s -> [ X.MOVSX_word (d, s) ]))

let emitSxtw (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg) =
  resolveReg dest
  |> bind (fun d -> resolveReg src |> Result.map (fun s -> [ X.MOVSXD (d, s) ]))

let emitUxtb (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src |> Result.map (fun s -> [ X.MOVZX_byte (d, s) ]))

let emitExit (_ctx : X64CodeGenTypes.funcCtx) =
  (*  RDI should already contain the exit code *)
  Ok genExitSyscall

let emitStdoutWrite (ctx : X64CodeGenTypes.funcCtx) (value : LIR.operand)
    (appendNewline : bool) =
  let writeLoop prefix =
    let loopLabel = freshLabel (prefix ^ "_write") in
    let retryLabel = freshLabel (prefix ^ "_retry") in
    let doneLabel = freshLabel (prefix ^ "_done") in
    [
      X.Label loopLabel;
      X.CMP_imm (X.RDX, 0l);
      X.Jcc (X.LE, doneLabel);
      X.Label retryLabel;
    ]
    @ genWriteSyscall
    @ [
        X.CMP_imm (X.RAX, -4l);
        (*  Linux EINTR *)
        X.Jcc (X.EQ, retryLabel);
        X.CMP_imm (X.RAX, 0l);
        X.Jcc (X.LE, doneLabel);
        X.ADD_reg (X.RSI, X.RAX);
        X.SUB_reg (X.RDX, X.RAX);
        X.JMP loopLabel;
        X.Label doneLabel;
      ]
  in
  let setupValue =
    match value with
    | LIR.Reg reg ->
        resolveReg reg
        |> Result.map (fun src ->
            [
              X.MOV_reg (X.R10, src);
              X.MOV_load (X.RDX, X.R10, 8l);
              X.LEA (X.RSI, X.R10, 16l);
            ])
    | LIR.StackSlot offset ->
        Ok
          [
            X.MOV_load
              ( X.R10,
                X.RBP,
                Int32.of_int
                  (X64InstructionContext.adjustStackOffset ctx offset) );
            X.MOV_load (X.RDX, X.R10, 8l);
            X.LEA (X.RSI, X.R10, 16l);
          ]
    | LIR.StringSymbol text ->
        Ok
          (emitStringLiteralNoRefCount X.R10 text
          @ [ X.MOV_load (X.RDX, X.R10, 8l); X.LEA (X.RSI, X.R10, 16l) ])
    | _ -> Error "StdoutWrite requires a String operand"
  in
  setupValue
  |> Result.map (fun setup ->
      let saved =
        [ X.RAX; X.RDI; X.RSI; X.RDX; X.RCX; X.R8; X.R9; X.R10; X.R11 ]
      in
      let save = saved |> List.map (fun r -> X.PUSH r) in
      let restore = saved |> List.rev |> List.map (fun r -> X.POP r) in
      let newline =
        if not appendNewline then []
        else
          [ X.SUB_imm (X.RSP, 8l) ]
          @ loadImm64 scratch 10L
          @ [
              X.MOV_store (X.RSP, 0l, scratch);
              X.MOV_imm32 (X.RDI, 1l);
              X.MOV_reg (X.RSI, X.RSP);
              X.MOV_imm32 (X.RDX, 1l);
            ]
          @ writeLoop "stdout_newline"
          @ [ X.ADD_imm (X.RSP, 8l) ]
      in
      save @ setup
      @ [ X.MOV_imm32 (X.RDI, 1l) ]
      @ writeLoop "stdout" @ newline @ restore)

let emitStdinReadLine (ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) =
  resolveReg dest
  |> Result.map (fun destReg ->
      let readLabel = freshLabel "stdin_read" in
      let retryLabel = freshLabel "stdin_retry" in
      let gotByteLabel = freshLabel "stdin_byte" in
      let finishLabel = freshLabel "stdin_finish" in
      let noCrLabel = freshLabel "stdin_no_cr" in
      let saved =
        [ X.RAX; X.RDI; X.RSI; X.RDX; X.RCX; X.R8; X.R9; X.R10; X.R11 ]
      in
      let save = saved |> List.map (fun r -> X.PUSH r) in
      let restore = saved |> List.rev |> List.map (fun r -> X.POP r) in
      let savedBytes = Int32.of_int ((List.length saved * 8) + 8) in
      save
      @ [ X.SUB_imm (X.RSP, 16l) ]
      @ loadImm64 X.R10 0L
      @ [
          X.MOV_store (X.RSP, 0l, X.R10);
          X.Label readLabel;
          X.MOV_load (X.R10, X.RSP, 0l);
          X.LEA (X.RSI, heapPtr, 16l);
          X.ADD_reg (X.RSI, X.R10);
          X.XOR_reg (X.RDI, X.RDI);
          X.MOV_imm32 (X.RDX, 1l);
          X.Label retryLabel;
        ]
      @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.read)
      @ [
          X.SYSCALL;
          X.CMP_imm (X.RAX, -4l);
          X.Jcc (X.EQ, retryLabel);
          X.CMP_imm (X.RAX, 1l);
          X.Jcc (X.EQ, gotByteLabel);
          X.JMP finishLabel;
          X.Label gotByteLabel;
          X.MOV_load_byte (X.R11, X.RSI, 0l);
          X.CMP_imm (X.R11, 10l);
          X.Jcc (X.EQ, finishLabel);
          X.ADD_imm (X.R10, 1l);
          X.MOV_store (X.RSP, 0l, X.R10);
          X.JMP readLabel;
          X.Label finishLabel;
          X.MOV_load (X.R10, X.RSP, 0l);
          X.CMP_imm (X.R10, 0l);
          X.Jcc (X.LE, noCrLabel);
          X.LEA (X.RSI, heapPtr, 15l);
          X.ADD_reg (X.RSI, X.R10);
          X.MOV_load_byte (X.R11, X.RSI, 0l);
          X.CMP_imm (X.R11, 13l);
          X.Jcc (X.NE, noCrLabel);
          X.SUB_imm (X.R10, 1l);
          X.Label noCrLabel;
          X.MOV_imm32 (X.RAX, 1l);
          X.MOV_store (heapPtr, 0l, X.RAX);
          X.MOV_store (heapPtr, 8l, X.R10);
          X.MOV_reg (X.R11, X.R10);
          X.ADD_imm (X.R11, 7l);
          X.AND_imm (X.R11, -8l);
        ]
      @ [
          X.MOV_store (X.RSP, 8l, heapPtr);
          X.ADD_imm (X.R11, 16l);
          X.ADD_reg (heapPtr, X.R11);
        ]
      @ genLeakCounterInc ctx
      @ [ X.ADD_imm (X.RSP, 16l) ]
      @ restore
      @ [ X.MOV_load (destReg, X.RSP, Int32.neg savedBytes) ])

let emitRuntimeError (_ctx : X64CodeGenTypes.funcCtx) (msg : string) =
  Ok (emitStringLiteral X.R8 msg @ [ X.JMP runtimeErrorHandlerLabel ])

let emitRuntimeErrorString (_ctx : X64CodeGenTypes.funcCtx)
    (messageReg : LIR.reg) =
  resolveReg messageReg
  |> Result.map (fun resolvedMessageReg ->
      [ X.MOV_reg (X.R8, resolvedMessageReg) ]
      @ [
          X.MOV_load (X.RDX, X.R8, 8l);
          X.LEA (X.RSI, X.R8, 16l);
          X.MOV_imm32 (X.RDI, 2l);
        ]
      @ genWriteSyscall @ loadImm64 X.RDI 1L @ genExitSyscall)

let emitArgMoves (ctx : X64CodeGenTypes.funcCtx)
    (moves : (LIR.physReg * LIR.operand) list) =
  (*  Parallel move resolution for function arguments. *)
  (*  Must handle case where a source register is also a destination of another *)
  (*  move (e.g., X1 <- X21; X4 <- X1 — second move must read ORIGINAL X1). *)
  (*  *)
  (*  Strategy: save all source registers that will be clobbered to scratch stack, *)
  (*  then perform all moves using saved values where needed. *)
  let generateMove (destPhys, srcOp) =
    let destX86 = lirRegToX86 destPhys in
    match srcOp with
    | LIR.Imm value -> Ok (loadImm64 destX86 value)
    | LIR.Reg (LIR.Physical srcPhys) ->
        if srcPhys = destPhys then Ok []
        else
          (*  If source will be clobbered by an earlier move, we need the *)
          (*  saved value. For simplicity, we use a two-pass approach: *)
          (*  all moves from Reg sources that ARE destinations get saved first. *)
          Ok [ X.MOV_reg (destX86, lirRegToX86 srcPhys) ]
    | LIR.Reg (LIR.Virtual _) -> Error "Virtual register in ArgMoves"
    | LIR.StackSlot offset ->
        Ok
          [
            X.MOV_load
              ( destX86,
                X.RBP,
                Int32.of_int
                  (X64InstructionContext.adjustStackOffset ctx offset) );
          ]
    | LIR.StringSymbol value -> Ok (emitStringLiteral destX86 value)
    | LIR.FloatSymbol value ->
        let bits = Int64.bits_of_float value in
        Ok (loadImm64 destX86 bits)
        (*  Store float bits in GP register (for passing as arg) *)
    | LIR.FuncAddr funcName ->
        Ok [ X.LEA_rip (destX86, functionName ctx funcName) ]
    | _ -> Error ("Unsupported ArgMoves operand: " ^ operandText srcOp)
    (*  Two-pass approach to handle parallel move conflicts: *)
    (*  1. Find source registers that are also destinations (will be clobbered) *)
    (*  2. Save those to the red zone before any moves *)
    (*  3. Do all moves, using red zone values for clobbered sources *)
  in
  let destRegSet = moves |> List.map fst |> List.sort_uniq Stdlib.compare in
  let clobberedSources =
    moves
    |> List.filter_map (fun (_, srcOp) ->
        match srcOp with
        | LIR.Reg (LIR.Physical srcPhys) ->
            if List.mem srcPhys destRegSet then Some srcPhys else None
        | _ -> None)
    |> distinct
    (*  Save clobbered sources to red zone (below RSP, no RSP adjustment) *)
    (*  Use offsets -16, -24, -32, etc. (-8 is used by IDIV) *)
  in
  let saveInstrs =
    clobberedSources
    |> List.mapi (fun i reg ->
        let offset = -16 - (i * 8) in
        X.MOV_store (X.RSP, Int32.of_int offset, lirRegToX86 reg))
    (*  Build a map from clobbered source to red zone offset *)
  in
  let clobberedOffsets =
    clobberedSources
    |> List.mapi (fun i reg -> (reg, -16 - (i * 8)))
    |> PhysMap.ofList
    (*  Generate moves, using red zone for clobbered sources *)
  in
  let generateMoveWithSave (destPhys, srcOp) =
    let destX86 = lirRegToX86 destPhys in
    match srcOp with
    | LIR.Reg (LIR.Physical srcPhys) when PhysMap.mem srcPhys clobberedOffsets
      ->
        if srcPhys = destPhys then Ok []
        else
          let offset = PhysMap.find srcPhys clobberedOffsets in
          Ok [ X.MOV_load (destX86, X.RSP, Int32.of_int offset) ]
    | _ -> generateMove (destPhys, srcOp)
  in
  let rec genMoves acc remaining =
    match remaining with
    | [] -> Ok (List.rev acc |> List.concat)
    | m :: rest -> (
        match generateMoveWithSave m with
        | Error e -> Error e
        | Ok instrs -> genMoves (instrs :: acc) rest)
  in
  genMoves [] moves |> Result.map (fun moveInstrs -> saveInstrs @ moveInstrs)

let emitTailArgMoves (ctx : X64CodeGenTypes.funcCtx)
    (moves : (LIR.physReg * LIR.operand) list) =
  (*  Same parallel move resolution as ArgMoves *)
  let destRegSet = moves |> List.map fst |> List.sort_uniq Stdlib.compare in
  let clobberedSources =
    moves
    |> List.filter_map (fun (_, srcOp) ->
        match srcOp with
        | LIR.Reg (LIR.Physical srcPhys) when List.mem srcPhys destRegSet ->
            Some srcPhys
        | _ -> None)
    |> distinct
  in
  let saveInstrs =
    clobberedSources
    |> List.mapi (fun i reg ->
        X.MOV_store (X.RSP, Int32.of_int (-16 - (i * 8)), lirRegToX86 reg))
  in
  let clobberedOffsets =
    clobberedSources
    |> List.mapi (fun i reg -> (reg, -16 - (i * 8)))
    |> PhysMap.ofList
  in
  let generateMove (destPhys, srcOp) =
    let destX86 = lirRegToX86 destPhys in
    match srcOp with
    | LIR.Imm value -> Ok (loadImm64 destX86 value)
    | LIR.Reg (LIR.Physical srcPhys) when PhysMap.mem srcPhys clobberedOffsets
      ->
        if srcPhys = destPhys then Ok []
        else
          Ok
            [
              X.MOV_load
                ( destX86,
                  X.RSP,
                  Int32.of_int (PhysMap.find srcPhys clobberedOffsets) );
            ]
    | LIR.Reg (LIR.Physical srcPhys) ->
        if srcPhys = destPhys then Ok []
        else Ok [ X.MOV_reg (destX86, lirRegToX86 srcPhys) ]
    | LIR.StackSlot offset ->
        Ok
          [
            X.MOV_load
              ( destX86,
                X.RBP,
                Int32.of_int
                  (X64InstructionContext.adjustStackOffset ctx offset) );
          ]
    | LIR.StringSymbol value -> Ok (emitStringLiteral destX86 value)
    | LIR.FloatSymbol value ->
        let bits = Int64.bits_of_float value in
        Ok (loadImm64 destX86 bits)
    | LIR.FuncAddr funcName ->
        Ok [ X.LEA_rip (destX86, functionName ctx funcName) ]
    | _ -> Error ("Unsupported TailArgMoves operand: " ^ operandText srcOp)
  in
  let rec genMoves acc remaining =
    match remaining with
    | [] -> Ok (List.rev acc |> List.concat)
    | m :: rest -> (
        match generateMove m with
        | Error e -> Error e
        | Ok instrs -> genMoves (instrs :: acc) rest)
  in
  genMoves [] moves |> Result.map (fun moveInstrs -> saveInstrs @ moveInstrs)

let emitPhi (_ctx : X64CodeGenTypes.funcCtx) (_dest : LIR.reg) =
  (*  Phi nodes should be eliminated before codegen (SSA destruction) *)
  (*  If we see one, it's a no-op — the parallel moves handle it *)
  Ok []

let emitInt64ToFloat (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.fReg)
    (src : LIR.reg) =
  match dest with
  | LIR.FPhysical dp ->
      resolveReg src
      |> Result.map (fun srcReg -> [ X.CVTSI2SD (lirFRegToX86 dp, srcReg) ])
  | _ -> Error "Int64ToFloat with virtual FP register"

let emitGpToFp (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.fReg)
    (src : LIR.reg) =
  match dest with
  | LIR.FPhysical dp ->
      resolveReg src
      |> Result.map (fun srcReg -> [ X.MOVQ_from_gp (lirFRegToX86 dp, srcReg) ])
  | _ -> Error "GpToFp with virtual FP register"

let emitLsl (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg)
    (shift : LIR.reg) =
  (*  SHL by register: shift amount must be in CL (lower byte of RCX) *)
  (*  Save/restore RCX if it's not the shift operand or dest (clobber not modeled by regalloc) *)
  resolveReg dest
  |> bind (fun d ->
      resolveReg src
      |> bind (fun s ->
          resolveReg shift
          |> Result.map (fun shReg ->
              let needSaveRCX = shReg <> X.RCX && d <> X.RCX in
              let save = if needSaveRCX then [ X.PUSH X.RCX ] else [] in
              let restore = if needSaveRCX then [ X.POP X.RCX ] else [] in
              if d = shReg && d <> s then
                (*  dest == shift register: moving src to dest would clobber shift. *)
                (*  Use scratch to save src, then move shift to RCX, then put src in dest. *)
                (*  This handles the case where s = RCX (which MOV RCX,shReg would clobber). *)
                save
                @ [ X.MOV_reg (scratch, s) ]
                @ (if shReg <> X.RCX then [ X.MOV_reg (X.RCX, shReg) ] else [])
                @ [ X.MOV_reg (d, scratch) ]
                @ [ X.SHL_cl d ] @ restore
              else if d = X.RCX && shReg <> X.RCX then
                (*  dest is RCX: MOV d,s then MOV RCX,shReg would clobber src in d. *)
                (*  Use scratch to hold value, shift there, move result back. *)
                [
                  X.MOV_reg (scratch, s);
                  X.MOV_reg (X.RCX, shReg);
                  X.SHL_cl scratch;
                  X.MOV_reg (X.RCX, scratch);
                ]
              else
                save
                @ (if d <> s then [ X.MOV_reg (d, s) ] else [])
                @ (if shReg <> X.RCX then [ X.MOV_reg (X.RCX, shReg) ] else [])
                @ [ X.SHL_cl d ] @ restore)))

let emitLsr (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg)
    (shift : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src
      |> bind (fun s ->
          resolveReg shift
          |> Result.map (fun shReg ->
              let needSaveRCX = shReg <> X.RCX && d <> X.RCX in
              let save = if needSaveRCX then [ X.PUSH X.RCX ] else [] in
              let restore = if needSaveRCX then [ X.POP X.RCX ] else [] in
              if d = shReg && d <> s then
                (*  dest == shift: save src via scratch to avoid clobbering when s=RCX *)
                save
                @ [ X.MOV_reg (scratch, s) ]
                @ (if shReg <> X.RCX then [ X.MOV_reg (X.RCX, shReg) ] else [])
                @ [ X.MOV_reg (d, scratch) ]
                @ [ X.SHR_cl d ] @ restore
              else if d = X.RCX && shReg <> X.RCX then
                (*  dest is RCX: use scratch to avoid clobbering *)
                [
                  X.MOV_reg (scratch, s);
                  X.MOV_reg (X.RCX, shReg);
                  X.SHR_cl scratch;
                  X.MOV_reg (X.RCX, scratch);
                ]
              else
                save
                @ (if d <> s then [ X.MOV_reg (d, s) ] else [])
                @ (if shReg <> X.RCX then [ X.MOV_reg (X.RCX, shReg) ] else [])
                @ [ X.SHR_cl d ] @ restore)))

let emitAsr (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg)
    (shift : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src
      |> bind (fun s ->
          resolveReg shift
          |> Result.map (fun shReg ->
              let needSaveRCX = shReg <> X.RCX && d <> X.RCX in
              let save = if needSaveRCX then [ X.PUSH X.RCX ] else [] in
              let restore = if needSaveRCX then [ X.POP X.RCX ] else [] in
              if d = shReg && d <> s then
                save
                @ [ X.MOV_reg (scratch, s) ]
                @ (if shReg <> X.RCX then [ X.MOV_reg (X.RCX, shReg) ] else [])
                @ [ X.MOV_reg (d, scratch); X.SAR_cl d ]
                @ restore
              else if d = X.RCX && shReg <> X.RCX then
                [
                  X.MOV_reg (scratch, s);
                  X.MOV_reg (X.RCX, shReg);
                  X.SAR_cl scratch;
                  X.MOV_reg (X.RCX, scratch);
                ]
              else
                save
                @ (if d <> s then [ X.MOV_reg (d, s) ] else [])
                @ (if shReg <> X.RCX then [ X.MOV_reg (X.RCX, shReg) ] else [])
                @ [ X.SAR_cl d ] @ restore)))

let emitUxth (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg) =
  resolveReg dest
  |> bind (fun d ->
      resolveReg src |> Result.map (fun s -> [ X.MOVZX_word (d, s) ]))

let emitUxtw (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) (src : LIR.reg) =
  (*  32-bit MOV zero-extends to 64-bit on x86_64 *)
  resolveReg dest
  |> bind (fun d ->
      resolveReg src |> Result.map (fun s -> [ X.MOV_reg32 (d, s) ]))

let emitClosureAlloc (ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg)
    (funcId : AST.functionId) (captures : LIR.operand list) =
  let funcName =
    functionName ctx funcId
    (*  Allocate closure on heap: [func_ptr, cap1, cap2, ...][refcount] *)
  in
  resolveReg dest
  |> bind (fun destReg ->
      let numSlots = add32 1 (List.length captures) in
      let sizeBytes = mul32 numSlots 8 in
      let totalSize =
        add32 sizeBytes 15 land -8
        (*  + refcount, aligned *)
      in
      let alloc =
        [
          X.MOV_reg (destReg, heapPtr);
          X.ADD_imm (heapPtr, Int32.of_int totalSize);
        ]
        (*  Store refcount = 1 *)
      in
      let storeRC =
        loadImm64 scratch 1L
        @ [ X.MOV_store (destReg, Int32.of_int sizeBytes, scratch) ]
        (*  Store function address at offset 0 *)
      in
      let storeFunc =
        [ X.LEA_rip (scratch, funcName); X.MOV_store (destReg, 0l, scratch) ]
        (*  Store captures *)
      in
      let storeCaptures =
        captures
        |> List.mapi (fun i cap -> (i, cap))
        |> List.concat_map (fun (i, cap) ->
            let offset = mul32 (add32 i 1) 8 in
            match cap with
            | LIR.Imm value ->
                loadImm64 scratch value
                @ [ X.MOV_store (destReg, Int32.of_int offset, scratch) ]
            | LIR.Reg reg -> (
                match resolveReg reg with
                | Ok srcReg ->
                    [ X.MOV_store (destReg, Int32.of_int offset, srcReg) ]
                | Error _ -> [])
            | LIR.StackSlot stackOffset ->
                let adjOff =
                  X64InstructionContext.adjustStackOffset ctx stackOffset
                in
                [
                  X.MOV_load (scratch, X.RBP, Int32.of_int adjOff);
                  X.MOV_store (destReg, Int32.of_int offset, scratch);
                ]
            | _ -> [])
      in
      Ok (alloc @ storeRC @ storeFunc @ storeCaptures @ genLeakCounterInc ctx))

let emitMadd (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg)
    (mulLeft : LIR.reg) (mulRight : LIR.reg) (add : LIR.reg) =
  (*  dest = add + mulLeft * mulRight *)
  resolveReg dest
  |> bind (fun d ->
      resolveReg mulLeft
      |> bind (fun ml ->
          resolveReg mulRight
          |> bind (fun mr ->
              resolveReg add
              |> Result.map (fun addReg ->
                  let temp = arithmeticTempExcluding [ d; ml; mr; addReg ] in
                  [ X.PUSH temp; X.MOV_reg (temp, ml); X.IMUL_reg (temp, mr) ]
                  @ (if d <> addReg then [ X.MOV_reg (d, addReg) ] else [])
                  @ [ X.ADD_reg (d, temp); X.POP temp ]))))
