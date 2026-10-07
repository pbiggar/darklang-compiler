(*
   X64Frames.ml - Generate aligned frames and callee-saved register handling.
*)
[@@@warning "-4"]

let add a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b = Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
let mul a b = Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))

(*
   LIR Instruction Translation
   StackSize is already in bytes from regalloc
*)
let alignedStackSize stackSlots numCalleeSaved =
  let returnAddr = 8 in
  let pushes = mul numCalleeSaved 8 in
  let total = add (add returnAddr pushes) stackSlots in
  let aligned = mul (add total 15 / 16) 16 in
  sub (sub aligned returnAddr) pushes

(*
   Push RBP and set up frame pointer for stack slot access.
   Stack slots use [RBP - offset] which is stable across SaveRegs PUSHes.
   +1 for RBP push
*)
let genPrologue stackSize usedCalleeSaved =
  let setupFP =
    [ X86_64.PUSH X86_64.RBP; X86_64.MOV_reg (X86_64.RBP, X86_64.RSP) ]
  in
  let saves =
    List.map
      (fun reg -> X86_64.PUSH (X64Operands.lirRegToX86 reg))
      usedCalleeSaved
  in
  let alignedSize =
    alignedStackSize stackSize (add (List.length usedCalleeSaved) 1)
  in
  let stackAlloc =
    if alignedSize > 0 then
      [ X86_64.SUB_imm (X86_64.RSP, Int32.of_int alignedSize) ]
    else []
  in
  setupFP @ saves @ stackAlloc

let genEpilogue stackSize usedCalleeSaved =
  let alignedSize =
    alignedStackSize stackSize (add (List.length usedCalleeSaved) 1)
  in
  let stackDealloc =
    if alignedSize > 0 then
      [ X86_64.ADD_imm (X86_64.RSP, Int32.of_int alignedSize) ]
    else []
  in
  let restores =
    List.map
      (fun reg -> X86_64.POP (X64Operands.lirRegToX86 reg))
      (List.rev usedCalleeSaved)
  in
  let restoreFP = [ X86_64.POP X86_64.RBP ] in
  stackDealloc @ restores @ restoreFP
