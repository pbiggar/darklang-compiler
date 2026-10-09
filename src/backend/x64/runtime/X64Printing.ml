(* X64Printing.ml - Generate x64 scalar printing and heap initialization support. *)
open X64Operands
module X = X86_64

(*
   Generate x86-64 instructions to print a signed 64-bit integer to stdout.
   Value is in the given register. Includes newline. Does NOT exit.
   Algorithm: itoa by repeated division by 10, writing digits backwards
   into a stack buffer, then write(1, buf, len).
   Save value, allocate 32-byte buffer on stack
   RCX = write pointer (end of buffer, work backwards)
   Store newline at end
   R8 = value to print; R9 = negative flag
   R9 = 0 (positive)
   Check if negative
   Check if zero
   convert_loop: extract digits by dividing by 10
   RAX = value
   Clear RDX for unsigned div
   But we made it positive, so use unsigned division
   RAX = quotient, RDX = remainder
   Convert remainder to ASCII
   Store digit
   Move pointer back
   value = quotient
   Loop if not zero
   store_minus_if_needed
   '-' = 45
   write_output
   print_zero: special case
   '0' = 48
   handle_negative: negate and set flag
   negative flag
   RCX was one past first char
   length = (RSP + 32) - RCX
   RDX = length
   fd = stdout
   buf
   Deallocate buffer
*)
let genPrintInt64 srcReg addNewline =
  let loopLabel = freshLabel "itoa_loop" in
  let doneLabel = freshLabel "itoa_done" in
  let zeroLabel = freshLabel "itoa_zero" in
  let negLabel = freshLabel "itoa_neg" in
  let writeLabel = freshLabel "itoa_write" in
  let skipMinusLabel = freshLabel "itoa_skipminus" in
  [ X.SUB_imm (X.RSP, 32l); X.LEA (X.RCX, X.RSP, 31l) ]
  @ (if addNewline then
       [
         X.MOV_imm32 (scratch, 10l);
         X.MOV_store_byte (X.RCX, 0l, scratch);
         X.SUB_imm (X.RCX, 1l);
       ]
     else [])
  @ [
      X.MOV_reg (X.R8, srcReg);
      X.XOR_reg (X.R9, X.R9);
      X.TEST_reg (X.R8, X.R8);
      X.Jcc (X.LT, negLabel);
      X.TEST_reg (X.R8, X.R8);
      X.Jcc (X.EQ, zeroLabel);
      X.Label loopLabel;
      X.MOV_reg (X.RAX, X.R8);
      X.XOR_reg (X.RDX, X.RDX);
      X.MOV_imm32 (X.RSI, 10l);
      X.DIV X.RSI;
      X.ADD_imm (X.RDX, 48l);
      X.MOV_store_byte (X.RCX, 0l, X.RDX);
      X.SUB_imm (X.RCX, 1l);
      X.MOV_reg (X.R8, X.RAX);
      X.TEST_reg (X.R8, X.R8);
      X.Jcc (X.NE, loopLabel);
      X.TEST_reg (X.R9, X.R9);
      X.Jcc (X.EQ, skipMinusLabel);
      X.MOV_imm32 (scratch, 45l);
      X.MOV_store_byte (X.RCX, 0l, scratch);
      X.SUB_imm (X.RCX, 1l);
      X.Label skipMinusLabel;
      X.JMP writeLabel;
      X.Label zeroLabel;
      X.MOV_imm32 (scratch, 48l);
      X.MOV_store_byte (X.RCX, 0l, scratch);
      X.SUB_imm (X.RCX, 1l);
      X.JMP writeLabel;
      X.Label negLabel;
      X.NEG X.R8;
      X.MOV_imm32 (X.R9, 1l);
      X.TEST_reg (X.R8, X.R8);
      X.Jcc (X.EQ, zeroLabel);
      X.JMP loopLabel;
      X.Label writeLabel;
      X.ADD_imm (X.RCX, 1l);
      X.LEA (X.RDX, X.RSP, 32l);
      X.SUB_reg (X.RDX, X.RCX);
      X.MOV_imm32 (X.RDI, 1l);
      X.MOV_reg (X.RSI, X.RCX);
    ]
  @ genWriteSyscall
  @ [ X.ADD_imm (X.RSP, 32l); X.Label doneLabel ]

(*
   Generate x86-64 instructions to print an unsigned 64-bit integer to stdout.
*)
let genPrintUInt64 srcReg addNewline =
  let loopLabel = freshLabel "utoa_loop" in
  let zeroLabel = freshLabel "utoa_zero" in
  let writeLabel = freshLabel "utoa_write" in
  [ X.SUB_imm (X.RSP, 32l); X.LEA (X.RCX, X.RSP, 31l) ]
  @ (if addNewline then
       [
         X.MOV_imm32 (scratch, 10l);
         X.MOV_store_byte (X.RCX, 0l, scratch);
         X.SUB_imm (X.RCX, 1l);
       ]
     else [])
  @ [
      X.MOV_reg (X.R8, srcReg);
      X.TEST_reg (X.R8, X.R8);
      X.Jcc (X.EQ, zeroLabel);
      X.Label loopLabel;
      X.MOV_reg (X.RAX, X.R8);
      X.XOR_reg (X.RDX, X.RDX);
      X.MOV_imm32 (X.RSI, 10l);
      X.DIV X.RSI;
      X.ADD_imm (X.RDX, 48l);
      X.MOV_store_byte (X.RCX, 0l, X.RDX);
      X.SUB_imm (X.RCX, 1l);
      X.MOV_reg (X.R8, X.RAX);
      X.TEST_reg (X.R8, X.R8);
      X.Jcc (X.NE, loopLabel);
      X.JMP writeLabel;
      X.Label zeroLabel;
      X.MOV_imm32 (scratch, 48l);
      X.MOV_store_byte (X.RCX, 0l, scratch);
      X.SUB_imm (X.RCX, 1l);
      X.Label writeLabel;
      X.ADD_imm (X.RCX, 1l);
      X.LEA (X.RDX, X.RSP, 32l);
      X.SUB_reg (X.RDX, X.RCX);
      X.MOV_imm32 (X.RDI, 1l);
      X.MOV_reg (X.RSI, X.RCX);
    ]
  @ genWriteSyscall
  @ [ X.ADD_imm (X.RSP, 32l) ]

(*
   Generate PrintInt64 + exit(0)
*)
let genPrintInt64AndExit srcReg =
  genPrintInt64 srcReg true @ loadImm64 X.RDI 0L @ genExitSyscall
[@@warning "-32"]

(*
   Generate heap initialization via mmap (only for _start).
   mmap(NULL, 512MB, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
   x86_64 Linux: rax=9, rdi=addr, rsi=length, rdx=prot, r10=flags, r8=fd, r9=offset
   PROT_READ | PROT_WRITE
   MAP_PRIVATE | MAP_ANONYMOUS
   fd = -1
   offset = 0
*)
let genHeapInit () =
  let failLabel = freshLabel "mmap_fail" in
  let okLabel = freshLabel "mmap_ok" in
  loadImm64 X.RDI 0L
  @ loadImm64 X.RSI heapMmapSizeBytes
  @ loadImm64 X.RDX 3L @ loadImm64 X.R10 0x22L
  @ [ X.MOV_imm32 (X.R8, -1l) ]
  @ loadImm64 X.R9 0L
  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.mmap)
  @ [
      X.SYSCALL;
      X.CMP_imm (X.RAX, -1l);
      X.Jcc (X.NE, okLabel);
      X.Label failLabel;
    ]
  @ loadImm64 X.RDI 1L @ genExitSyscall
  @ [
      X.Label okLabel;
      X.MOV_reg (freeListBase, X.RAX);
      X.LEA
        (heapPtr, freeListBase, Int32.of_int (freeListSize + processTableSize + 512));
    ]

(*
   Generate PrintBool + exit(0)
   false\n = 6 bytes
   "false\n" in little-endian (6 bytes)
   "true\n" in little-endian (5 bytes)
   fd = stdout
*)
let genPrintBoolAndExit srcReg =
  let trueLabel = freshLabel "bool_true" in
  let writeLabel = freshLabel "bool_write" in
  [
    X.TEST_reg (srcReg, srcReg); X.Jcc (X.NE, trueLabel); X.SUB_imm (X.RSP, 8l);
  ]
  @ loadImm64 scratch 0x0a65736c6166L
  @ [
      X.MOV_store (X.RSP, 0l, scratch);
      X.MOV_reg (X.RSI, X.RSP);
      X.MOV_imm32 (X.RDX, 6l);
      X.JMP writeLabel;
      X.Label trueLabel;
      X.SUB_imm (X.RSP, 8l);
    ]
  @ loadImm64 scratch 0x0a65757274L
  @ [
      X.MOV_store (X.RSP, 0l, scratch);
      X.MOV_reg (X.RSI, X.RSP);
      X.MOV_imm32 (X.RDX, 5l);
      X.Label writeLabel;
      X.MOV_imm32 (X.RDI, 1l);
    ]
  @ genWriteSyscall
  @ [ X.ADD_imm (X.RSP, 8l) ]
  @ loadImm64 X.RDI 0L @ genExitSyscall
[@@warning "-32"]
