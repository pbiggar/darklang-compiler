(*
   X86_64_Encoding.ml - x86-64 Instruction Encoding (Pass 7, x86_64 variant)
   Encodes x86-64 instructions to variable-length machine code bytes.
   x86-64 encoding format (variable length, 1-15 bytes):
   [REX prefix] [Opcode] [ModR/M] [SIB] [Displacement] [Immediate]
   REX prefix (0x40-0x4F): Required when using 64-bit operands or R8-R15.
   REX.W (bit 3): 64-bit operand size
   REX.R (bit 2): Extension of ModR/M reg field (for R8-R15)
   REX.X (bit 1): Extension of SIB index field
   REX.B (bit 0): Extension of ModR/M r/m field or SIB base (for R8-R15)
   ModR/M byte: [mod:2][reg:3][r/m:3]
   mod=11: register direct
   mod=00: [r/m] (register indirect)
   mod=01: [r/m + disp8]
   mod=10: [r/m + disp32]
   See Intel SDM Vol 2, Chapter 2 for complete encoding reference.
   Check if a value fits in a signed 32-bit immediate
*)
open X86_64

let byte value = value land 255
let concat values = Array.concat (Array.to_list values)

(*
   Get the 3-bit register encoding and whether REX.B/REX.R extension is needed
*)
let regEncoding = function
  | RAX -> (0, false)
  | RCX -> (1, false)
  | RDX -> (2, false)
  | RBX -> (3, false)
  | RSP -> (4, false)
  | RBP -> (5, false)
  | RSI -> (6, false)
  | RDI -> (7, false)
  | R8 -> (0, true)
  | R9 -> (1, true)
  | R10 -> (2, true)
  | R11 -> (3, true)
  | R12 -> (4, true)
  | R13 -> (5, true)
  | R14 -> (6, true)
  | R15 -> (7, true)

(*
   Get the 3-bit XMM register encoding and whether REX extension is needed
*)
let fregEncoding = function
  | XMM0 -> (0, false)
  | XMM1 -> (1, false)
  | XMM2 -> (2, false)
  | XMM3 -> (3, false)
  | XMM4 -> (4, false)
  | XMM5 -> (5, false)
  | XMM6 -> (6, false)
  | XMM7 -> (7, false)
  | XMM8 -> (0, true)
  | XMM9 -> (1, true)
  | XMM10 -> (2, true)
  | XMM11 -> (3, true)
  | XMM12 -> (4, true)
  | XMM13 -> (5, true)
  | XMM14 -> (6, true)
  | XMM15 -> (7, true)

(*
   Build a REX prefix byte. Returns empty array if no REX needed.
   REX.W: 64-bit operand
   REX.R: reg field extension
   REX.X: SIB index extension
   REX.B: r/m field extension
*)
let rex w r x b =
  if w || r || x || b then
    let wBit = if w then 0x08 else 0 in
    let rBit = if r then 0x04 else 0 in
    let xBit = if x then 0x02 else 0 in
    let bBit = if b then 0x01 else 0 in
    [| 0x40 lor wBit lor rBit lor xBit lor bBit |]
  else [||]

(*
   Build a ModR/M byte
*)
let modRM modBits reg rm = byte ((modBits lsl 6) lor (reg lsl 3) lor rm)

type memoryOperandEncoding = { baseExt : bool; blob : int array }

(*
   Encode a 32-bit signed immediate as little-endian bytes
*)
let imm32Bytes v =
  Array.init 4 (fun i ->
      Int32.to_int (Int32.logand (Int32.shift_right_logical v (i * 8)) 0xffl))

(*
   Encode an 8-bit signed immediate
*)
let imm8Bytes v = [| byte v |]

(*
   Encode a 64-bit immediate as little-endian bytes
*)
let imm64Bytes v =
  Array.init 8 (fun i ->
      Int64.to_int (Int64.logand (Int64.shift_right_logical v (i * 8)) 0xffL))

(*
   Check if a value fits in a signed 8-bit immediate
*)
let fitsInt8 v = Int32.compare v (-128l) >= 0 && Int32.compare v 127l <= 0

let[@warning "-32"] fitsInt32 v =
  Int64.compare v (Int64.of_int32 Int32.min_int) >= 0
  && Int64.compare v (Int64.of_int32 Int32.max_int) <= 0

(*
   Encode opcode, ModR/M, optional SIB, and displacement for [base + offset] operands.
*)
let encodeMemoryOperand opcodeBytes regField baseAddr offset =
  let baseEnc, baseExt = regEncoding baseAddr in
  let needsSIB = baseEnc = 4 in
  let modBits =
    if offset = 0l && baseEnc <> 5 then 0 else if fitsInt8 offset then 1 else 2
  in
  let modrm = modRM modBits regField baseEnc in
  let disp =
    if modBits = 0 then [||]
    else if modBits = 1 then imm8Bytes (Int32.to_int offset)
    else imm32Bytes offset
  in
  let operandBytes =
    if needsSIB then
      let sib = byte ((0 lsl 6) lor (4 lsl 3) lor baseEnc) in
      concat [| opcodeBytes; [| modrm; sib |]; disp |]
    else concat [| opcodeBytes; [| modrm |]; disp |]
  in
  { baseExt; blob = operandBytes }

(*
   Encode register-to-register operation with REX.W and a 2-byte opcode of form [opcode] [ModR/M]
   ModR/M: mod=11 (register direct), reg=src, r/m=dest
*)
let encodeRegReg opcode dest src =
  let destEnc, destExt = regEncoding dest in
  let srcEnc, srcExt = regEncoding src in
  concat
    [| rex true srcExt false destExt; [| opcode; modRM 3 srcEnc destEnc |] |]

(*
   Encode a condition code to its 4-bit value (for Jcc, SETcc)
   ZF=1
   ZF=0
   SF!=OF (signed)
   SF=OF (signed)
   ZF=1 or SF!=OF (signed)
   ZF=0 and SF=OF (signed)
   CF=1 (unsigned/float below)
   CF=0 (unsigned/float above or equal)
   CF=1 or ZF=1 (unsigned/float below or equal)
   CF=0 and ZF=0 (unsigned/float above)
   PF=1 (parity/unordered - NaN)
   PF=0 (no parity/ordered - not NaN)
*)
let condCode = function
  | EQ -> 0x04
  | NE -> 0x05
  | LT -> 0x0c
  | GE -> 0x0d
  | LE -> 0x0e
  | GT -> 0x0f
  | B -> 0x02
  | AE -> 0x03
  | BE -> 0x06
  | A -> 0x07
  | P -> 0x0a
  | NP -> 0x0b

let encodeLeaIndex dest baseAddr index scale offset =
  let destEnc, destExt = regEncoding dest in
  let baseEnc, baseExt = regEncoding baseAddr in
  let indexEnc, indexExt = regEncoding index in
  if indexEnc = 4 then Crash.crash "x64 LEA index cannot use RSP/R12";
  let scaleBits =
    match scale with
    | 1 -> 0
    | 2 -> 1
    | 4 -> 2
    | 8 -> 3
    | _ ->
        Crash.crash
          (Printf.sprintf "x64 LEA received unsupported scale %d" scale)
  in
  let modBits =
    if offset = 0l && baseEnc <> 5 then 0 else if fitsInt8 offset then 1 else 2
  in
  let displacement =
    if modBits = 0 then [||]
    else if modBits = 1 then imm8Bytes (Int32.to_int offset)
    else imm32Bytes offset
  in
  let sib = byte ((scaleBits lsl 6) lor (indexEnc lsl 3) lor baseEnc) in
  concat
    [|
      rex true destExt indexExt baseExt;
      [| 0x8d; modRM modBits destEnc 4; sib |];
      displacement;
    |]

let encodeInstructionBytes instr =
  match instr with
  | MOV_imm (dest, imm) ->
      let destEnc, destExt = regEncoding dest in
      concat
        [|
          rex true false false destExt;
          [| 0xB8 + byte destEnc |];
          imm64Bytes imm;
        |]
  | MOV_imm32 (dest, imm) ->
      let destEnc, destExt = regEncoding dest in
      concat
        [|
          rex true false false destExt;
          [| 0xC7; modRM 3 0 destEnc |];
          imm32Bytes imm;
        |]
  | MOV_reg (dest, src) -> encodeRegReg 0x89 dest src
  | MOV_reg32 (dest, src) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      let needsRex = destExt || srcExt in
      let rexByte =
        if needsRex then
          [|
            (0x40
            lor (if srcExt then 0x04 else 0x00)
            lor if destExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| rexByte; [| 0x89; modRM 3 srcEnc destEnc |] |]
  | MOV_load (dest, baseAddr, offset) ->
      let destEnc, destExt = regEncoding dest in
      let mem = encodeMemoryOperand [| 0x8B |] destEnc baseAddr offset in
      concat [| rex true destExt false mem.baseExt; mem.blob |]
  | MOV_store (baseAddr, offset, src) ->
      let srcEnc, srcExt = regEncoding src in
      let mem = encodeMemoryOperand [| 0x89 |] srcEnc baseAddr offset in
      concat [| rex true srcExt false mem.baseExt; mem.blob |]
  | LEA (dest, baseAddr, offset) ->
      let destEnc, destExt = regEncoding dest in
      let mem = encodeMemoryOperand [| 0x8D |] destEnc baseAddr offset in
      concat [| rex true destExt false mem.baseExt; mem.blob |]
  | LEA_index (dest, baseAddr, index, scale, offset) ->
      encodeLeaIndex dest baseAddr index scale offset
  | PUSH reg ->
      let enc, ext = regEncoding reg in
      if ext then [| 0x41; 0x50 + byte enc |] else [| 0x50 + byte enc |]
  | POP reg ->
      let enc, ext = regEncoding reg in
      if ext then [| 0x41; 0x58 + byte enc |] else [| 0x58 + byte enc |]
  | ADD_imm (dest, imm) ->
      let destEnc, destExt = regEncoding dest in
      if fitsInt8 imm then
        concat
          [|
            rex true false false destExt;
            [| 0x83; modRM 3 0 destEnc |];
            imm8Bytes (Int32.to_int imm);
          |]
      else
        concat
          [|
            rex true false false destExt;
            [| 0x81; modRM 3 0 destEnc |];
            imm32Bytes imm;
          |]
  | ADD_reg (dest, src) -> encodeRegReg 0x01 dest src
  | ADD_load (dest, baseAddr, offset) ->
      let destEnc, destExt = regEncoding dest in
      let mem = encodeMemoryOperand [| 0x03 |] destEnc baseAddr offset in
      concat [| rex true destExt false mem.baseExt; mem.blob |]
  | SUB_imm (dest, imm) ->
      let destEnc, destExt = regEncoding dest in
      if fitsInt8 imm then
        concat
          [|
            rex true false false destExt;
            [| 0x83; modRM 3 5 destEnc |];
            imm8Bytes (Int32.to_int imm);
          |]
      else
        concat
          [|
            rex true false false destExt;
            [| 0x81; modRM 3 5 destEnc |];
            imm32Bytes imm;
          |]
  | SUB_reg (dest, src) -> encodeRegReg 0x29 dest src
  | SUB_load (dest, baseAddr, offset) ->
      let destEnc, destExt = regEncoding dest in
      let mem = encodeMemoryOperand [| 0x2B |] destEnc baseAddr offset in
      concat [| rex true destExt false mem.baseExt; mem.blob |]
  | IMUL_reg (dest, src) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      concat
        [|
          rex true destExt false srcExt;
          [| 0x0F; 0xAF; modRM 3 destEnc srcEnc |];
        |]
  | IMUL_imm (dest, src, imm) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      if fitsInt8 imm then
        concat
          [|
            rex true destExt false srcExt;
            [| 0x6B; modRM 3 destEnc srcEnc |];
            imm8Bytes (Int32.to_int imm);
          |]
      else
        concat
          [|
            rex true destExt false srcExt;
            [| 0x69; modRM 3 destEnc srcEnc |];
            imm32Bytes imm;
          |]
  | IDIV src ->
      let srcEnc, srcExt = regEncoding src in
      concat [| rex true false false srcExt; [| 0xF7; modRM 3 7 srcEnc |] |]
  | DIV src ->
      let srcEnc, srcExt = regEncoding src in
      concat [| rex true false false srcExt; [| 0xF7; modRM 3 6 srcEnc |] |]
  | NEG dest ->
      let destEnc, destExt = regEncoding dest in
      concat [| rex true false false destExt; [| 0xF7; modRM 3 3 destEnc |] |]
  | NOT dest ->
      let destEnc, destExt = regEncoding dest in
      concat [| rex true false false destExt; [| 0xF7; modRM 3 2 destEnc |] |]
  | CQO -> [| 0x48; 0x99 |]
  | XOR_reg (dest, src) -> encodeRegReg 0x31 dest src
  | CMP_imm (src, imm) ->
      let srcEnc, srcExt = regEncoding src in
      if fitsInt8 imm then
        concat
          [|
            rex true false false srcExt;
            [| 0x83; modRM 3 7 srcEnc |];
            imm8Bytes (Int32.to_int imm);
          |]
      else
        concat
          [|
            rex true false false srcExt;
            [| 0x81; modRM 3 7 srcEnc |];
            imm32Bytes imm;
          |]
  | CMP_reg (src1, src2) -> encodeRegReg 0x39 src1 src2
  | TEST_reg (src1, src2) -> encodeRegReg 0x85 src1 src2
  | SETcc (cond, dest) ->
      let cc = condCode cond in
      let destEnc, destExt = regEncoding dest in
      let needsRex = destExt || destEnc >= 4 in
      let rexByte =
        if needsRex then [| (0x40 lor if destExt then 0x01 else 0x00) |]
        else [||]
      in
      concat [| rexByte; [| 0x0F; 0x90 + cc; modRM 3 0 destEnc |] |]
  | CMOVcc (cond, dest, src) ->
      let cc = condCode cond in
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      concat
        [|
          rex true destExt false srcExt;
          [| 0x0F; 0x40 + cc; modRM 3 destEnc srcEnc |];
        |]
  | AND_imm (dest, imm) ->
      let destEnc, destExt = regEncoding dest in
      if fitsInt8 imm then
        concat
          [|
            rex true false false destExt;
            [| 0x83; modRM 3 4 destEnc |];
            imm8Bytes (Int32.to_int imm);
          |]
      else
        concat
          [|
            rex true false false destExt;
            [| 0x81; modRM 3 4 destEnc |];
            imm32Bytes imm;
          |]
  | AND_reg (dest, src) -> encodeRegReg 0x21 dest src
  | OR_reg (dest, src) -> encodeRegReg 0x09 dest src
  | SHL_imm (dest, shift) ->
      let destEnc, destExt = regEncoding dest in
      concat
        [|
          rex true false false destExt;
          [| 0xC1; modRM 3 4 destEnc |];
          imm8Bytes shift;
        |]
  | SHR_imm (dest, shift) ->
      let destEnc, destExt = regEncoding dest in
      concat
        [|
          rex true false false destExt;
          [| 0xC1; modRM 3 5 destEnc |];
          imm8Bytes shift;
        |]
  | SAR_imm (dest, shift) ->
      let destEnc, destExt = regEncoding dest in
      concat
        [|
          rex true false false destExt;
          [| 0xC1; modRM 3 7 destEnc |];
          imm8Bytes shift;
        |]
  | SHL_cl dest ->
      let destEnc, destExt = regEncoding dest in
      concat [| rex true false false destExt; [| 0xD3; modRM 3 4 destEnc |] |]
  | SHR_cl dest ->
      let destEnc, destExt = regEncoding dest in
      concat [| rex true false false destExt; [| 0xD3; modRM 3 5 destEnc |] |]
  | SAR_cl dest ->
      let destEnc, destExt = regEncoding dest in
      concat [| rex true false false destExt; [| 0xD3; modRM 3 7 destEnc |] |]
  | MOV_store_byte (baseAddr, offset, src) ->
      let srcEnc, srcExt = regEncoding src in
      let mem = encodeMemoryOperand [| 0x88 |] srcEnc baseAddr offset in
      let needsRex = srcExt || mem.baseExt || srcEnc >= 4 in
      let rexByte =
        if needsRex then
          [|
            (0x40
            lor (if srcExt then 0x04 else 0x00)
            lor if mem.baseExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| rexByte; mem.blob |]
  | MOV_load_byte (dest, baseAddr, offset) ->
      let destEnc, destExt = regEncoding dest in
      let mem = encodeMemoryOperand [| 0x0F; 0xB6 |] destEnc baseAddr offset in
      let rexByte =
        if destExt || mem.baseExt then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if mem.baseExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| rexByte; mem.blob |]
  | MOVZX_byte (dest, src) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      let needsRex = destExt || srcExt || srcEnc >= 4 in
      let rexByte =
        if needsRex then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if srcExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| rexByte; [| 0x0F; 0xB6; modRM 3 destEnc srcEnc |] |]
  | MOVZX_word (dest, src) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      let needsRex = destExt || srcExt in
      let rexByte =
        if needsRex then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if srcExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| rexByte; [| 0x0F; 0xB7; modRM 3 destEnc srcEnc |] |]
  | MOVSX_byte (dest, src) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      concat
        [|
          rex true destExt false srcExt;
          [| 0x0F; 0xBE; modRM 3 destEnc srcEnc |];
        |]
  | MOVSX_word (dest, src) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      concat
        [|
          rex true destExt false srcExt;
          [| 0x0F; 0xBF; modRM 3 destEnc srcEnc |];
        |]
  | MOVSXD (dest, src) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      concat
        [| rex true destExt false srcExt; [| 0x63; modRM 3 destEnc srcEnc |] |]
  | LEA_rip (dest, _label) ->
      let destEnc, destExt = regEncoding dest in
      concat
        [|
          rex true destExt false false;
          [| 0x8D; modRM 0 destEnc 5 |];
          imm32Bytes 0l;
        |]
  | CALL _label -> concat [| [| 0xE8 |]; imm32Bytes 0l |]
  | CALL_reg reg ->
      let enc, ext = regEncoding reg in
      let rexByte = if ext then [| 0x41 |] else [||] in
      concat [| rexByte; [| 0xFF; modRM 3 2 enc |] |]
  | JMP _label -> concat [| [| 0xE9 |]; imm32Bytes 0l |]
  | JMP_reg reg ->
      let enc, ext = regEncoding reg in
      let rexByte = if ext then [| 0x41 |] else [||] in
      concat [| rexByte; [| 0xFF; modRM 3 4 enc |] |]
  | Jcc (_cond, _label) ->
      let cc = condCode _cond in
      concat [| [| 0x0F; 0x80 + cc |]; imm32Bytes 0l |]
  | RET -> [| 0xC3 |]
  | SYSCALL -> [| 0x0F; 0x05 |]
  | Label _ -> [||]
  | MOVSD_load (dest, baseAddr, offset) ->
      let destEnc, destExt = fregEncoding dest in
      let mem = encodeMemoryOperand [| 0x0F; 0x10 |] destEnc baseAddr offset in
      let rexByte =
        if destExt || mem.baseExt then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if mem.baseExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| [| 0xF2 |]; rexByte; mem.blob |]
  | MOVSD_store (baseAddr, offset, src) ->
      let srcEnc, srcExt = fregEncoding src in
      let mem = encodeMemoryOperand [| 0x0F; 0x11 |] srcEnc baseAddr offset in
      let rexByte =
        if srcExt || mem.baseExt then
          [|
            (0x40
            lor (if srcExt then 0x04 else 0x00)
            lor if mem.baseExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| [| 0xF2 |]; rexByte; mem.blob |]
  | MOVSD_reg (dest, src) ->
      let destEnc, destExt = fregEncoding dest in
      let srcEnc, srcExt = fregEncoding src in
      let rexByte =
        if destExt || srcExt then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if srcExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| [| 0xF2 |]; rexByte; [| 0x0F; 0x10; modRM 3 destEnc srcEnc |] |]
  | ADDSD (dest, src) ->
      let destEnc, destExt = fregEncoding dest in
      let srcEnc, srcExt = fregEncoding src in
      let rexByte =
        if destExt || srcExt then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if srcExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| [| 0xF2 |]; rexByte; [| 0x0F; 0x58; modRM 3 destEnc srcEnc |] |]
  | SUBSD (dest, src) ->
      let destEnc, destExt = fregEncoding dest in
      let srcEnc, srcExt = fregEncoding src in
      let rexByte =
        if destExt || srcExt then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if srcExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| [| 0xF2 |]; rexByte; [| 0x0F; 0x5C; modRM 3 destEnc srcEnc |] |]
  | MULSD (dest, src) ->
      let destEnc, destExt = fregEncoding dest in
      let srcEnc, srcExt = fregEncoding src in
      let rexByte =
        if destExt || srcExt then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if srcExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| [| 0xF2 |]; rexByte; [| 0x0F; 0x59; modRM 3 destEnc srcEnc |] |]
  | DIVSD (dest, src) ->
      let destEnc, destExt = fregEncoding dest in
      let srcEnc, srcExt = fregEncoding src in
      let rexByte =
        if destExt || srcExt then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if srcExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| [| 0xF2 |]; rexByte; [| 0x0F; 0x5E; modRM 3 destEnc srcEnc |] |]
  | XORPD (dest, src) ->
      let destEnc, destExt = fregEncoding dest in
      let srcEnc, srcExt = fregEncoding src in
      let rexByte =
        if destExt || srcExt then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if srcExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| [| 0x66 |]; rexByte; [| 0x0F; 0x57; modRM 3 destEnc srcEnc |] |]
  | SQRTSD (dest, src) ->
      let destEnc, destExt = fregEncoding dest in
      let srcEnc, srcExt = fregEncoding src in
      let rexByte =
        if destExt || srcExt then
          [|
            (0x40
            lor (if destExt then 0x04 else 0x00)
            lor if srcExt then 0x01 else 0x00);
          |]
        else [||]
      in
      concat [| [| 0xF2 |]; rexByte; [| 0x0F; 0x51; modRM 3 destEnc srcEnc |] |]
  | UCOMISD (src1, src2) ->
      let src1Enc, src1Ext = fregEncoding src1 in
      let src2Enc, src2Ext = fregEncoding src2 in
      let rexByte =
        if src1Ext || src2Ext then
          [|
            (0x40
            lor (if src1Ext then 0x04 else 0x00)
            lor if src2Ext then 0x01 else 0x00);
          |]
        else [||]
      in
      concat
        [| [| 0x66 |]; rexByte; [| 0x0F; 0x2E; modRM 3 src1Enc src2Enc |] |]
  | CVTSI2SD (dest, src) ->
      let destEnc, destExt = fregEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      concat
        [|
          [| 0xF2 |];
          rex true destExt false srcExt;
          [| 0x0F; 0x2A; modRM 3 destEnc srcEnc |];
        |]
  | CVTTSD2SI (dest, src) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = fregEncoding src in
      concat
        [|
          [| 0xF2 |];
          rex true destExt false srcExt;
          [| 0x0F; 0x2C; modRM 3 destEnc srcEnc |];
        |]
  | MOVQ_to_gp (dest, src) ->
      let destEnc, destExt = regEncoding dest in
      let srcEnc, srcExt = fregEncoding src in
      concat
        [|
          [| 0x66 |];
          rex true srcExt false destExt;
          [| 0x0F; 0x7E; modRM 3 srcEnc destEnc |];
        |]
  | MOVQ_from_gp (dest, src) ->
      let destEnc, destExt = fregEncoding dest in
      let srcEnc, srcExt = regEncoding src in
      concat
        [|
          [| 0x66 |];
          rex true destExt false srcExt;
          [| 0x0F; 0x6E; modRM 3 destEnc srcEnc |];
        |]

(*
   Encode a single x86-64 instruction to bytes
   --- Data movement ---
   REX.W + B8+rd io (MOV r64, imm64) — "movabs"
   REX.W + C7 /0 id (MOV r/m64, imm32) — sign-extended
   REX.W + 89 /r (MOV r/m64, r64)
   89 /r (MOV r/m32, r32) — no REX.W, 32-bit write zero-extends to 64-bit
   REX.W + 8B /r (MOV r64, r/m64)
   REX.W + 8D /r (LEA r64, m)
   50+rd (PUSH r64) — REX.B if R8-R15
   58+rd (POP r64) — REX.B if R8-R15
   --- Arithmetic ---
   REX.W + 83 /0 ib (ADD r/m64, imm8)
   REX.W + 81 /0 id (ADD r/m64, imm32)
   REX.W + 01 /r (ADD r/m64, r64)
   REX.W + 83 /5 ib (SUB r/m64, imm8)
   REX.W + 81 /5 id (SUB r/m64, imm32)
   REX.W + 29 /r (SUB r/m64, r64)
   REX.W + 0F AF /r (IMUL r64, r/m64)
   REX.W + 6B /r ib (IMUL r64, r/m64, imm8)
   REX.W + 69 /r id (IMUL r64, r/m64, imm32)
   REX.W + F7 /7 (IDIV r/m64)
   REX.W + F7 /6 (DIV r/m64)
   REX.W + F7 /3 (NEG r/m64)
   REX.W + F7 /2 (NOT r/m64)
   REX.W + 99 (CQO)
   REX.W + 31 /r (XOR r/m64, r64)
   --- Comparison and conditional ---
   REX.W + 83 /7 ib (CMP r/m64, imm8)
   REX.W + 81 /7 id (CMP r/m64, imm32)
   REX.W + 39 /r (CMP r/m64, r64)
   REX.W + 85 /r (TEST r/m64, r64)
   0F 90+cc /0 (SETcc r/m8) — then MOVZX to clear upper bits
   Need REX prefix if dest is SPL/BPL/SIL/DIL (enc 4-7 with no extension)
   or if dest needs REX.B extension
   --- Bitwise ---
   REX.W + 21 /r (AND r/m64, r64)
   REX.W + 09 /r (OR r/m64, r64)
   REX.W + C1 /4 ib (SHL r/m64, imm8)
   REX.W + C1 /5 ib (SHR r/m64, imm8)
   REX.W + D3 /4 (SHL r/m64, CL)
   REX.W + D3 /5 (SHR r/m64, CL)
   --- Byte-level memory ---
   88 /r (MOV r/m8, r8)
   Need REX for SPL/BPL/SIL/DIL
   0F B6 /r (MOVZX r32, r/m8) — zero-extends to 64-bit
   --- Sign/zero extension ---
   0F B6 /r (MOVZX r32, r/m8) — implicitly zero-extends to 64-bit
   0F B7 /r (MOVZX r32, r/m16) — implicitly zero-extends to 64-bit
   REX.W + 0F BE /r (MOVSX r64, r/m8)
   REX.W + 0F BF /r (MOVSX r64, r/m16)
   REX.W + 63 /r (MOVSXD r64, r/m32)
   REX.W + 8D /r — [RIP + disp32] — displacement will be fixed up later
   --- Control flow ---
   E8 cd (CALL rel32) — offset will be fixed up later
   FF /2 (CALL r/m64)
   E9 cd (JMP rel32) — offset will be fixed up later
   FF /4 (JMP r/m64)
   0F 80+cc cd (Jcc rel32) — offset will be fixed up later
   C3 (RET)
   0F 05 (SYSCALL)
   Pseudo-instruction, no bytes emitted
   --- Floating-point (SSE2) ---
   F2 0F 10 /r (MOVSD xmm, m64)
   F2 0F 11 /r (MOVSD m64, xmm)
   F2 0F 10 /r (MOVSD xmm1, xmm2)
   F2 0F 58 /r (ADDSD xmm1, xmm2)
   F2 0F 5C /r
   F2 0F 59 /r
   F2 0F 5E /r
   66 0F 57 /r
   F2 0F 51 /r
   66 0F 2E /r
   F2 REX.W 0F 2A /r (CVTSI2SD xmm, r64)
   F2 REX.W 0F 2C /r (CVTTSD2SI r64, xmm)
   66 REX.W 0F 7E /r (MOVQ r64, xmm)
   66 REX.W 0F 6E /r (MOVQ xmm, r64)
*)
let encodeInstruction instr =
  let encoded = encodeInstructionBytes instr in
  Bytes.init (Array.length encoded) (fun i -> Char.chr encoded.(i))
