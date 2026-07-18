// X86_64EncodingTests.fs - Unit tests for x86-64 instruction encoding
//
// Verifies that x86-64 instructions encode to correct machine code bytes.

module X86_64EncodingTests

open X86_64
open X86_64_Encoding

/// Assert that an instruction encodes to the expected bytes
let private assertEncoding (label: string) (instr: Instr) (expected: byte array) : Result<unit, string> =
    let actual = encodeInstruction instr
    if actual = expected then
        Ok ()
    else
        let expectedHex = expected |> Array.map (fun b -> sprintf "%02X" b) |> String.concat " "
        let actualHex = actual |> Array.map (fun b -> sprintf "%02X" b) |> String.concat " "
        Error $"{label}: expected [{expectedHex}], got [{actualHex}]"

let private assertEncodings (cases: (string * Instr * byte array) list) : Result<unit, string> =
    cases
    |> List.fold
        (fun result (label, instr, expected) ->
            result |> Result.bind (fun () -> assertEncoding label instr expected))
        (Ok ())

let testMovRegReg () : Result<unit, string> =
    assertEncodings [
        // MOV RAX, RBX -> REX.W 89 /r (mod=11, reg=RBX(3), r/m=RAX(0))
        ("MOV RAX,RBX", MOV_reg (RAX, RBX), [| 0x48uy; 0x89uy; 0xD8uy |])
        // MOV R8, RAX -> REX.WB 89 /r
        ("MOV R8,RAX", MOV_reg (R8, RAX), [| 0x49uy; 0x89uy; 0xC0uy |])
        // MOV RAX, R8 -> REX.WR 89 /r
        ("MOV RAX,R8", MOV_reg (RAX, R8), [| 0x4Cuy; 0x89uy; 0xC0uy |])
    ]

let testMovImm32 () : Result<unit, string> =
    // MOV RAX, 42 -> REX.W C7 /0 imm32
    assertEncoding "MOV RAX,42" (MOV_imm32 (RAX, 42)) [| 0x48uy; 0xC7uy; 0xC0uy; 0x2Auy; 0x00uy; 0x00uy; 0x00uy |]

let testMemoryAddressing () : Result<unit, string> =
    // MOV RAX, [RSP] → REX.W 8B /r with SIB for RSP base
    assertEncoding "MOV RAX,[RSP]" (MOV_load (RAX, RSP, 0)) [| 0x48uy; 0x8Buy; 0x04uy; 0x24uy |]
    |> Result.bind (fun () ->
        // MOV [R12], RAX → REX.WB 89 /r with SIB for R12 base
        assertEncoding "MOV [R12],RAX" (MOV_store (R12, 0, RAX)) [| 0x49uy; 0x89uy; 0x04uy; 0x24uy |])
    |> Result.bind (fun () ->
        // LEA RAX, [RBP] must use disp8=0 because mod=00 r/m=101 means RIP-relative
        assertEncoding "LEA RAX,[RBP]" (LEA (RAX, RBP, 0)) [| 0x48uy; 0x8Duy; 0x45uy; 0x00uy |])
    |> Result.bind (fun () ->
        // MOV RAX, [R13] has the same disp8=0 rule as RBP plus REX.B
        assertEncoding "MOV RAX,[R13]" (MOV_load (RAX, R13, 0)) [| 0x49uy; 0x8Buy; 0x45uy; 0x00uy |])

let testAddSubImm () : Result<unit, string> =
    assertEncodings [
        // ADD RSP, 8 -> REX.W 83 /0 ib
        ("ADD RSP,8", ADD_imm (RSP, 8), [| 0x48uy; 0x83uy; 0xC4uy; 0x08uy |])
        // SUB RSP, 16 -> REX.W 83 /5 ib
        ("SUB RSP,16", SUB_imm (RSP, 16), [| 0x48uy; 0x83uy; 0xECuy; 0x10uy |])
    ]

let testPushPop () : Result<unit, string> =
    assertEncodings [
        ("PUSH RBP", PUSH RBP, [| 0x55uy |])
        ("POP RBP", POP RBP, [| 0x5Duy |])
        ("PUSH R12", PUSH R12, [| 0x41uy; 0x54uy |])
        ("POP R12", POP R12, [| 0x41uy; 0x5Cuy |])
    ]

let testSimpleInstructions () : Result<unit, string> =
    assertEncodings [
        ("RET", RET, [| 0xC3uy |])
        ("SYSCALL", SYSCALL, [| 0x0Fuy; 0x05uy |])
        ("CQO", CQO, [| 0x48uy; 0x99uy |])
    ]

let testXorZeroing () : Result<unit, string> =
    // XOR RAX, RAX -> REX.W 31 /r
    assertEncoding "XOR RAX,RAX" (XOR_reg (RAX, RAX)) [| 0x48uy; 0x31uy; 0xC0uy |]

let testNeg () : Result<unit, string> =
    // NEG RAX -> REX.W F7 /3
    assertEncoding "NEG RAX" (NEG RAX) [| 0x48uy; 0xF7uy; 0xD8uy |]

let testAddReg () : Result<unit, string> =
    // ADD RAX, RBX -> REX.W 01 /r
    assertEncoding "ADD RAX,RBX" (ADD_reg (RAX, RBX)) [| 0x48uy; 0x01uy; 0xD8uy |]

let testCmpImm () : Result<unit, string> =
    // CMP RDI, 0 -> REX.W 83 /7 ib
    assertEncoding "CMP RDI,0" (CMP_imm (RDI, 0)) [| 0x48uy; 0x83uy; 0xFFuy; 0x00uy |]

let testImulReg () : Result<unit, string> =
    // IMUL RAX, RBX -> REX.W 0F AF /r
    assertEncoding "IMUL RAX,RBX" (IMUL_reg (RAX, RBX)) [| 0x48uy; 0x0Fuy; 0xAFuy; 0xC3uy |]

let testShlImm () : Result<unit, string> =
    // SHL RAX, 3 -> REX.W C1 /4 ib
    assertEncoding "SHL RAX,3" (SHL_imm (RAX, 3)) [| 0x48uy; 0xC1uy; 0xE0uy; 0x03uy |]

let testMemoryOperands () : Result<unit, string> =
    assertEncoding "MOV RAX,[RSP]" (MOV_load (RAX, RSP, 0)) [| 0x48uy; 0x8Buy; 0x04uy; 0x24uy |]
    |> Result.bind (fun () ->
        assertEncoding "MOV [R13],RAX" (MOV_store (R13, 0, RAX)) [| 0x49uy; 0x89uy; 0x45uy; 0x00uy |])
    |> Result.bind (fun () ->
        assertEncoding "LEA R8,[RBP+128]" (LEA (R8, RBP, 128)) [| 0x4Cuy; 0x8Duy; 0x85uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |])
    |> Result.bind (fun () ->
        assertEncoding "MOVZX R9,[R12-1]" (MOV_load_byte (R9, R12, -1)) [| 0x45uy; 0x0Fuy; 0xB6uy; 0x4Cuy; 0x24uy; 0xFFuy |])
    |> Result.bind (fun () ->
        assertEncoding "MOVSD [RSP+16],XMM8" (MOVSD_store (RSP, 16, XMM8)) [| 0xF2uy; 0x44uy; 0x0Fuy; 0x11uy; 0x44uy; 0x24uy; 0x10uy |])

let tests : (string * (unit -> Result<unit, string>)) list = [
    ("MOV reg,reg", testMovRegReg)
    ("MOV reg,imm32", testMovImm32)
    ("Memory addressing", testMemoryAddressing)
    ("ADD/SUB imm", testAddSubImm)
    ("PUSH/POP", testPushPop)
    ("Simple instructions", testSimpleInstructions)
    ("XOR zeroing", testXorZeroing)
    ("NEG", testNeg)
    ("ADD reg", testAddReg)
    ("CMP imm", testCmpImm)
    ("IMUL reg", testImulReg)
    ("SHL imm", testShlImm)
    ("Memory operands", testMemoryOperands)
]
