(* X64EmitBuffers.ml - Emit x64 instructions for buffers operations. *)
[@@@warning "-4"]
open X64Operands
open X64CodeGenTypes
module X=X86_64
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
let operandText operand=
 let open StructuralValue in
 let reg=function LIR.Physical p -> Union ("Physical",[Scalar (physicalName p)]) | LIR.Virtual n -> Union ("Virtual",[Scalar (string_of_int n)]) in
 StructuralFormat.format (match operand with
 | LIR.Imm n -> Union ("Imm",[Scalar (Int64.to_string n ^ "L")])
 | LIR.FloatImm value -> Union ("FloatImm",[Scalar (FloatFormat.structural value)])
 | LIR.Reg r -> Union ("Reg",[reg r])
 | LIR.StackSlot n -> Union ("StackSlot",[Scalar (string_of_int n)])
 | LIR.StringSymbol text -> Union ("StringSymbol",[Text text])
 | LIR.FloatSymbol value -> Union ("FloatSymbol",[Scalar (FloatFormat.structural value)])
 | LIR.FuncAddr id -> Union ("FuncAddr",[AST.DiagnosticFormatting.func id]))

let utf8Len = String.length
(*
   Canonical buffers share [refcount:8][length:8][data:N]. Compare the
   representation directly without allocating or calling stdlib code.
*)
let emitCanonicalBufferEq (_ctx:funcCtx) kind dest left right =
 Result.bind (resolveReg dest) (fun destReg->
  let leftReg=X.RDI and rightReg=X.RSI and remainingReg=X.RCX and leftWordReg=X.R8 and rightWordReg=X.R9 and byteReg=X.R10 in
  let savedRegs=[leftReg;rightReg;remainingReg;leftWordReg;rightWordReg;byteReg] in
  let saveInstrs=List.map (fun r->X.PUSH r) savedRegs in
  let restoreInstrs=List.map (fun r->X.POP r) (List.rev savedRegs) in
  let prepareOperands=match left,right with
  | LIR.Reg left,LIR.Reg right -> Result.bind (resolveReg left) (fun sourceLeft->Result.map (fun sourceRight->
    [X.PUSH sourceLeft;X.PUSH sourceRight;X.POP rightReg;X.POP leftReg]) (resolveReg right))
  | LIR.Reg left,LIR.StringSymbol right -> Result.map (fun sourceLeft->[X.PUSH sourceLeft]@emitStringLiteralNoRefCount rightReg right@[X.POP leftReg]) (resolveReg left)
  | LIR.StringSymbol left,LIR.Reg right -> Result.map (fun sourceRight->[X.PUSH sourceRight]@emitStringLiteralNoRefCount leftReg left@[X.POP rightReg]) (resolveReg right)
  | LIR.StringSymbol left,LIR.StringSymbol right ->
    let left=emitStringLiteralNoRefCount leftReg left in let right=emitStringLiteralNoRefCount rightReg right in Ok (left@right)
  | _ -> Error "CanonicalBufferEq requires StringSymbol or Reg operands" in
  let wordLoop=freshLabel "canonical_eq_words" in
  let byteLoop=freshLabel "canonical_eq_bytes" in
  let equalLabel=freshLabel "canonical_eq_equal" in
  let unequalLabel=freshLabel "canonical_eq_unequal" in
  let doneLabel=freshLabel "canonical_eq_done" in
  Result.map (fun operandInstrs->saveInstrs@operandInstrs@[X.CMP_reg (leftReg,rightReg);X.Jcc (X.EQ,equalLabel)]@
   (if kind=MemoryModel.NullableUtf8String || kind=MemoryModel.NullableGraphemeCluster then
    [X.TEST_reg (leftReg,leftReg);X.Jcc (X.EQ,unequalLabel);X.TEST_reg (rightReg,rightReg);X.Jcc (X.EQ,unequalLabel)] else [])@
            [X.MOV_load (remainingReg, leftReg, 8l);
               X.MOV_load (rightWordReg, rightReg, 8l);
               X.CMP_reg (remainingReg, rightWordReg);
               X.Jcc (X.NE, unequalLabel);
               X.ADD_imm (leftReg, 16l);
               X.ADD_imm (rightReg, 16l);
               X.Label wordLoop;
               X.CMP_imm (remainingReg, 8l);
               X.Jcc (X.LT, byteLoop);
               X.MOV_load (leftWordReg, leftReg, 0l);
               X.MOV_load (rightWordReg, rightReg, 0l);
               X.CMP_reg (leftWordReg, rightWordReg);
               X.Jcc (X.NE, unequalLabel);
               X.ADD_imm (leftReg, 8l);
               X.ADD_imm (rightReg, 8l);
               X.SUB_imm (remainingReg, 8l);
               X.JMP wordLoop;
               X.Label byteLoop;
               X.CMP_imm (remainingReg, 0l);
               X.Jcc (X.EQ, equalLabel);
               X.MOV_load_byte (leftWordReg, leftReg, 0l);
               X.MOV_load_byte (byteReg, rightReg, 0l);
               X.CMP_reg (leftWordReg, byteReg);
               X.Jcc (X.NE, unequalLabel);
               X.ADD_imm (leftReg, 1l);
               X.ADD_imm (rightReg, 1l);
               X.SUB_imm (remainingReg, 1l);
               X.JMP byteLoop;
               X.Label equalLabel;
               X.MOV_imm32 (scratch, 1l);
               X.JMP doneLabel;
               X.Label unequalLabel;
               X.MOV_imm32 (scratch, 0l);
               X.Label doneLabel]
 @restoreInstrs@(if destReg=scratch then [] else [X.MOV_reg (destReg,scratch)])) prepareOperands)
(*
   String concat: dest = left ++ right
   Dynamic and literal strings share [refcount:8][length:8][data:N].
   Strategy: load both strings' info, allocate result, copy bytes with loops.
   Register plan (no PUSH/POP in loops):
   RDI = left data ptr, RSI = left len
   R8  = right data ptr, R9 = right len
   R10 = loop counter, R11(scratch) = temp byte
   destReg = result ptr, RCX = dest write ptr
   IMPORTANT: This operation clobbers RDI, RSI, RCX, R8, R9, R10.
   Save/restore all caller-saved registers except those used as operands,
   since the register allocator doesn't model these clobbers.
   Clobbered registers (RDI=X1, RSI=X2, RCX=X3, R8=X4, R9=X5, R10=X6)
   Save all except the dest reg (caller may still need operand regs after this)
   srcReg == lenDest: LEA first so MOV_load doesn't clobber pointer
   srcReg == addrDest: save pointer in scratch before LEA clobbers it
   Load RIGHT first (if Reg, no allocation needed), then LEFT
   (which might allocate for StringSymbol). This avoids clobbering
   the right source register during left's heap allocation.
   Loading the right operand owns R8/R9 and may also use scratch for
   a stack slot, literal, or aliased R8 source. Preserve a left
   pointer held in any of those registers before that setup.
   Save right info before loading left (left might clobber R8/R9)
   R8 and R9 were pushed after the preserved left pointer.
*)
let emitStringConcatBinary ctx dest left right =
 Result.bind (resolveReg dest) (fun destReg->
  let clobbered=[X.RDI;X.RSI;X.RCX;X.R8;X.R9;X.R10] in
  let toSave=List.filter (fun r->r<>destReg) clobbered in
  let saveInstrs=List.map (fun r->X.PUSH r) toSave in
  let restoreInstrs=List.map (fun r->X.POP r) (List.rev toSave) in
  let loadInfo op addrDest lenDest = match op with
  | LIR.Reg reg -> Result.map (fun srcReg->if srcReg=lenDest then
    [X.LEA (addrDest,srcReg,16l);X.MOV_load (lenDest,srcReg,8l)]
    else if srcReg=addrDest then [X.MOV_reg (scratch,srcReg);X.MOV_load (lenDest,srcReg,8l);X.LEA (addrDest,scratch,16l)]
    else [X.MOV_load (lenDest,srcReg,8l);X.LEA (addrDest,srcReg,16l)]) (resolveReg reg)
  | LIR.StringSymbol value ->
    let len=utf8Len value in let instrs=emitStringLiteralNoRefCount addrDest value in
    let setResults=loadImm64 lenDest (Int64.of_int len)@[X.LEA (addrDest,addrDest,16l)] in Ok (instrs@setResults)
  | LIR.StackSlot stackOffset -> Ok [X.MOV_load (scratch,X.RBP,Int32.of_int (X64InstructionContext.adjustStackOffset ctx stackOffset));X.MOV_load (lenDest,scratch,8l);X.LEA (addrDest,scratch,16l)]
  | _ -> let len=loadImm64 lenDest 0L in let addr=loadImm64 addrDest 0L in Ok (len@addr) in
  let copy1=freshLabel "strcat_c1" in let done1=freshLabel "strcat_d1" in
  let copy2=freshLabel "strcat_c2" in let done2=freshLabel "strcat_d2" in
  let doneAllocation=freshLabel "strcat_alloc_ok" in
  let leftConflictReg=match left with
  | LIR.Reg reg -> (match resolveReg reg with Ok r when r=X.R8 || r=X.R9 || r=scratch -> Some r | _->None)
  | _ -> None in
  Result.bind (loadInfo right X.R8 X.R9) (fun rightInstrs->
   let saveRight=[X.PUSH X.R8;X.PUSH X.R9] in
   let preserveLeft=match leftConflictReg with Some r->[X.PUSH r] | None->[] in
   let loadLeft=match leftConflictReg with Some _->Ok [X.MOV_load (scratch,X.RSP,16l);X.MOV_load (X.RSI,scratch,8l);X.LEA (X.RDI,scratch,16l)] | None->loadInfo left X.RDI X.RSI in
   let discardPreservedLeft=match leftConflictReg with Some _->[X.ADD_imm (X.RSP,8l)] | None->[] in
   Result.map (fun leftInstrs->
                saveInstrs
                @ preserveLeft
                @ rightInstrs @ saveRight @ leftInstrs
                 (* Restore right info *)
                @ [X.POP X.R9; X.POP X.R8]
                @ discardPreservedLeft

                 (* Total length in RCX *)
                @ [X.MOV_reg (X.RCX, X.RSI);
                   X.ADD_reg (X.RCX, X.R9)]

                 (* Allocate: use RBX to hold result ptr (callee-saved, safe across loops) *)
                 (* Save RBX first *)
                @ [X.PUSH X.RBX]
                @ [X.MOV_reg (X.RBX, heapPtr);
                   X.MOV_reg (X.R10, X.RCX);
                   X.ADD_imm (X.R10, 23l);
                   X.AND_imm (X.R10, -8l);
                   X.ADD_reg (heapPtr, X.R10);
                   X.MOV_reg (scratch, heapPtr);
                   X.SUB_reg (scratch, freeListBase);
                   X.CMP_imm (scratch, Int64.to_int32 heapMmapSizeBytes);
                   X.Jcc (X.LE, doneAllocation)]
                @ genOomJump ()
                @ [X.Label doneAllocation]

                 (* Store the fixed header. *)
                @ loadImm64 scratch 1L
                @ [X.MOV_store (X.RBX, 0l, scratch);
                   X.MOV_store (X.RBX, 8l, X.RCX)]

                 (* Copy left bytes: RBX[8+i] = left[i] *)
                @ loadImm64 X.R10 0L
                @ [X.Label copy1;
                   X.CMP_reg (X.R10, X.RSI);
                   X.Jcc (X.GE, done1);
                   X.MOV_reg (scratch, X.RDI);
                   X.ADD_reg (scratch, X.R10);
                   X.MOV_load_byte (scratch, scratch, 0l);
                   X.LEA (X.RCX, X.RBX, 16l);
                   X.ADD_reg (X.RCX, X.R10);
                   X.MOV_store_byte (X.RCX, 0l, scratch);
                   X.ADD_imm (X.R10, 1l);
                   X.JMP copy1;
                   X.Label done1]

                 (* Copy right bytes: RBX[8+leftLen+i] = right[i] *)
                @ [X.LEA (X.RCX, X.RBX, 16l);
                   X.ADD_reg (X.RCX, X.RSI)]
                @ loadImm64 X.R10 0L
                @ [X.Label copy2;
                   X.CMP_reg (X.R10, X.R9);
                   X.Jcc (X.GE, done2);
                   X.MOV_reg (scratch, X.R8);
                   X.ADD_reg (scratch, X.R10);
                   X.MOV_load_byte (scratch, scratch, 0l);
                   X.MOV_reg (X.RDI, X.RCX);
                   X.ADD_reg (X.RDI, X.R10);
                   X.MOV_store_byte (X.RDI, 0l, scratch);
                   X.ADD_imm (X.R10, 1l);
                   X.JMP copy2;
                   X.Label done2]

                 (* Leak counter increment for string allocation *)
                @ genLeakCounterInc ctx
                 (* Move result to destReg, restore RBX *)
                 (* If destReg IS RBX, we need to save result elsewhere first *)
                @ (if destReg = X.RBX then
                        (* Result is already in RBX. Pop saved RBX to scratch, keep result. *)
                       [X.ADD_imm (X.RSP, 8l)]   (* discard saved RBX *)
                   else
                       [X.MOV_reg (destReg, X.RBX);
                        X.POP X.RBX])
 @restoreInstrs) loadLeft))
(*
   Lower a concat tree as one length pass, one allocation, and one ordered copy pass.
*)
let emitStringConcatMany ctx dest first second remaining =
 let operands=first::second::remaining in
 let savedRegs=[X.RAX;X.RDI;X.RSI;X.RCX;X.R8;X.R9;X.R10;X.RBX] in
 let snapshotOperand=function
 | LIR.Reg reg -> Result.map (fun source->[X.PUSH source;X.MOV_load (scratch,source,8l);X.PUSH scratch]) (resolveReg reg)
 | LIR.StringSymbol value -> Ok (emitStringLiteralNoRefCount scratch value@[X.PUSH scratch]@loadImm64 scratch (Int64.of_int (utf8Len value))@[X.PUSH scratch])
 | LIR.StackSlot stackOffset -> Ok [X.MOV_load (scratch,X.RBP,Int32.of_int (X64InstructionContext.adjustStackOffset ctx stackOffset));X.PUSH scratch;X.MOV_load (scratch,scratch,8l);X.PUSH scratch]
 | other -> Error ("StringConcat requires string operands, got: "^operandText other) in
 Result.bind (resolveReg dest) (fun destReg->Result.map (fun snapshots->
  let save=List.map (fun r->X.PUSH r) savedRegs in
  let restore=List.map (fun r->X.POP r) (List.rev savedRegs) in
  let operandCount=List.length operands in
  let stackOffset index fieldOffset=Int32.mul (Int32.add (Int32.mul 2l (Int32.sub (Int32.of_int operandCount) (Int32.add (Int32.of_int index) 1l))) (Int32.of_int fieldOffset)) 8l in
  let measure=loadImm64 X.RCX 0L@List.concat_map (fun index->[X.MOV_load (scratch,X.RSP,stackOffset index 0);X.ADD_reg (X.RCX,scratch)]) (List.init operandCount Fun.id) in
  let allocationDone=freshLabel "strcat_many_alloc_ok" in
  let allocate=
                [ X.MOV_reg (X.RBX, heapPtr);
                  X.MOV_reg (X.R10, X.RCX);
                  X.ADD_imm (X.R10, 23l);
                  X.AND_imm (X.R10, -8l);
                  X.ADD_reg (heapPtr, X.R10);
                  X.MOV_reg (scratch, heapPtr);
                  X.SUB_reg (scratch, freeListBase);
                  X.CMP_imm (scratch, Int64.to_int32 heapMmapSizeBytes);
                  X.Jcc (X.LE, allocationDone) ]
                @ genOomJump ()
                @ [ X.Label allocationDone ]
                @ loadImm64 scratch 1L
                @ [ X.MOV_store (X.RBX, 0l, scratch);
                    X.MOV_store (X.RBX, 8l, X.RCX);
                    X.LEA (X.RDI, X.RBX, 16l) ]

 in
  let copy index =
   let loop=freshLabel ("strcat_many_copy_"^string_of_int index) in
   let doneLabel=freshLabel ("strcat_many_done_"^string_of_int index) in
                [ X.MOV_load (X.RSI, X.RSP, stackOffset index 1);
                  X.ADD_imm (X.RSI, 16l);
                  X.MOV_load (X.R10, X.RSP, stackOffset index 0);
                  X.Label loop;
                  X.CMP_imm (X.R10, 0l);
                  X.Jcc (X.LE, doneLabel);
                  X.MOV_load_byte (scratch, X.RSI, 0l);
                  X.MOV_store_byte (X.RDI, 0l, scratch);
                  X.ADD_imm (X.RSI, 1l);
                  X.ADD_imm (X.RDI, 1l);
                  X.SUB_imm (X.R10, 1l);
                  X.JMP loop;
                  X.Label doneLabel ]

 in
  let copies=List.concat_map copy (List.init operandCount Fun.id) in
  save@List.concat snapshots@measure@allocate@copies@genLeakCounterInc ctx@
  [X.MOV_reg (scratch,X.RBX);X.ADD_imm (X.RSP,Int32.mul (Int32.of_int operandCount) 16l)]@restore@
  (if destReg=scratch then [] else [X.MOV_reg (destReg,scratch)])) (ResultList.mapResults snapshotOperand operands))
let emitStringConcat ctx dest first second remaining=match remaining with
 | [] -> emitStringConcatBinary ctx dest first second
 | _ -> emitStringConcatMany ctx dest first second remaining
