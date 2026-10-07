(*
   ARM64EncodingTests.fs - Unit tests for ARM64 encoding utilities
   Tests utility functions like encodeReg that are used by the
   ARM64 instruction encoder.
*)
[@@@warning "-4-42"]
open Dark_compiler
open ARM64
open ARM64_Encoding
(*
   Test result type
*)
type testResult=(unit,string) result
(*
   Test that encodeReg produces correct register numbers
*)
let testEncodeReg () =
 let tests=[X0,0l,"X0";X1,1l,"X1";X15,15l,"X15";X30,30l,"X30";SP,31l,"SP"] in
 let rec checkTests=function [] -> Ok () | (reg,expected,name)::rest -> let actual=encodeReg reg in if actual<>expected then Error (Printf.sprintf "encodeReg %s: expected %lu, got %lu" name expected actual) else checkTests rest in checkTests tests
(*
   Test MOVK encoding with various shift values
   The shift parameter should be in bit positions (0, 16, 32, 48)
   and gets converted to hw values (0, 1, 2, 3) by dividing by 16
   MOVK X0, #0xFFFF, shift
   Expected encoding: sf=1 opc=11 100101 hw imm16 Rd
   sf=1 (bit 31), opc=11 (bits 30-29), 100101 (bits 28-23), hw (bits 22-21), imm16 (bits 20-5), Rd (bits 4-0)
   (shift, expected_hw, test_name)
   Extract hw field: bits 22-21
*)
let testMOVKShiftEncoding () =
 let tests=[0,0l,"shift=0 → hw=0";16,1l,"shift=16 → hw=1";32,2l,"shift=32 → hw=2";48,3l,"shift=48 → hw=3"] in
 let rec checkTests=function [] -> Ok () | (shift,expectedHw,name)::rest -> let encoded=encode (MOVK (X0,0xffff,shift)) in match encoded with
 | [word] -> let actualHw=Int32.logand (Int32.shift_right_logical word 21) 3l in if actualHw<>expectedHw then Error (Printf.sprintf "MOVK %s: expected hw=%lu, got hw=%lu (encoded=0x%08lX)" name expectedHw actualHw word) else checkTests rest
 | _ -> Error (Printf.sprintf "MOVK %s: expected 1 word, got %d" name (List.length encoded)) in checkTests tests
(*
   Test that MOVZ + MOVK sequence correctly builds 64-bit values
   This tests the common pattern for loading large immediates
   Build 0x0001_0000 (65536) using MOVZ + MOVK
   MOVZ X0, #0, 0     -> X0 = 0
   MOVK X0, #1, 16    -> X0[31:16] = 1, so X0 = 0x10000 = 65536
   Verify MOVZ has hw=0
   Verify MOVK has hw=1 (for shift=16)
*)
let testMOVZMOVKSequence () =
 let movz=encode (MOVZ (X0,0,0)) in let movk=encode (MOVK (X0,1,16)) in
 match movz,movk with
 | [movzWord],[movkWord] -> let movzHw=Int32.logand (Int32.shift_right_logical movzWord 21) 3l in if movzHw<>0l then Error (Printf.sprintf "MOVZ: expected hw=0, got hw=%lu" movzHw) else let movkHw=Int32.logand (Int32.shift_right_logical movkWord 21) 3l in if movkHw<>1l then Error (Printf.sprintf "MOVK: expected hw=1, got hw=%lu" movkHw) else Ok ()
 | _ -> Error "MOVZ/MOVK sequence: unexpected encoding length"
let checkWords cases =
 let rec check=function [] -> Ok () | (instr,expected,name)::rest -> match encode instr with
 | [actual] when actual=expected -> check rest
 | [actual] -> Error (Printf.sprintf "%s: expected 0x%08lX, got 0x%08lX" name expected actual)
 | words -> Error (Printf.sprintf "%s: expected one word, got %d" name (List.length words)) in check cases
let testCombinedInstructionEncoding ()=checkWords [CSEL (X0,X1,X2,EQ),0x9a820020l,"CSEL";FMADD (D0,D1,D2,D3),0x1f420c20l,"FMADD";ADD_extended (X0,X1,X2,ExtendSXTW),0x8b22c020l,"ADD extended"]
let expectCrash name f=try f ();Error (name^": expected encoder to reject invalid immediate offset") with _ -> Ok ()
let checkCrashes cases =
 let rec checkCases=function [] -> Ok () | (name,f)::rest -> match expectCrash name f with Ok () -> checkCases rest | Error msg -> Error msg in checkCases cases
let testUnsignedMemoryOffsetsRejectInvalidValues ()=checkCrashes ["STR negative offset",(fun () -> ignore (encode (STR (X0,SP,-8))));"LDR unaligned offset",(fun () -> ignore (encode (LDR (X0,SP,2))));"STR out-of-range offset",(fun () -> ignore (encode (STR (X0,SP,32761))));"LDR_fp negative offset",(fun () -> ignore (encode (LDR_fp (D0,SP,-8))))]
let testSignedPairOffsetsRejectInvalidValues ()=checkCrashes ["STP unaligned offset",(fun () -> ignore (encode (STP (X0,X1,SP,7))));"LDP out-of-range offset",(fun () -> ignore (encode (LDP (X0,X1,SP,512))));"STP_pre negative out-of-range offset",(fun () -> ignore (encode (STP_pre (X0,X1,SP,-520))));"LDP_post unaligned offset",(fun () -> ignore (encode (LDP_post (X0,X1,SP,-7))));"STP_fp out-of-range offset",(fun () -> ignore (encode (STP_fp (D0,D1,SP,512))));"LDP_fp unaligned offset",(fun () -> ignore (encode (LDP_fp (D0,D1,SP,6))))]
let testArithmeticImmediatesRejectOutOfRangeValues ()=checkCrashes ["ADD_imm 4096",(fun () -> ignore (encode (ADD_imm (X0,X1,4096))));"SUB_imm 4096",(fun () -> ignore (encode (SUB_imm (X0,X1,4096))));"SUB_imm12 4096",(fun () -> ignore (encode (SUB_imm12 (X0,X1,4096))));"SUBS_imm 4096",(fun () -> ignore (encode (SUBS_imm (X0,X1,4096))));"CMP_imm 4096",(fun () -> ignore (encode (CMP_imm (X1,4096))))]
let testMoveWideShiftsRejectInvalidValues ()=checkCrashes ["MOVZ shift 8",(fun () -> ignore (encode (MOVZ (X0,1,8))));"MOVN shift 24",(fun () -> ignore (encode (MOVN (X0,1,24))));"MOVK shift 64",(fun () -> ignore (encode (MOVK (X0,1,64))))]
let testFMOVImmediateEncoding () =
 let cases=["1.0",FMOV_imm (D2,1.),0x1e6e1002l;"4.0",FMOV_imm (D3,4.),0x1e621003l] in
 let rec check=function [] -> Ok () | (name,instr,expected)::rest -> match encode instr with [word] when word=expected -> check rest | [word] -> Error (Printf.sprintf "FMOV_imm %s: expected 0x%08lX, got 0x%08lX" name expected word) | words -> Error (Printf.sprintf "FMOV_imm %s: expected 1 word, got %d" name (List.length words)) in check cases
let testBICRegisterEncoding ()=match encode (BIC_reg (X3,X1,X2)) with [word] when word=0x8a220023l -> Ok () | [word] -> Error (Printf.sprintf "BIC_reg: expected 0x8A220023, got 0x%08lX" word) | words -> Error (Printf.sprintf "BIC_reg: expected 1 word, got %d" (List.length words))
let testPreparedChunksPreserveWholeProgramEncoding () =
 let literal=Symbolic.DataLabel (Symbolic.StringLiteral "chunked") in
 let chunks=[[Symbolic.Label "_start";Symbolic.B_label "local_continue";Symbolic.MOVZ (X0,0,0);Symbolic.Label "local_continue";Symbolic.MOVZ (X0,42,0);Symbolic.BL "callee";Symbolic.ADRP (X1,literal);Symbolic.ADD_label (X1,X1,literal)];[Symbolic.Label "callee";Symbolic.RET]] in
 let flattened=List.concat chunks in let expectedStrings,expectedFloats=ARM64_Resolve.collectPools flattened in
 let expected=encodeSymbolicWithPools flattened expectedStrings expectedFloats Platform.Linux false in
 let prepared=List.map prepareSymbolicChunk chunks in
 let actualStrings,actualFloats=ARM64_Resolve.collectPoolsFromLabelRefs (Seq.flat_map (fun (chunk:preparedChunk) -> Array.to_seq chunk.poolLabelRefs) (List.to_seq prepared)) in
 let actual=encodePreparedChunksWithPools prepared actualStrings actualFloats Platform.Linux false in
 let combined=combinePreparedChunks prepared in let combinedActual=encodePreparedChunksWithPools [combined] actualStrings actualFloats Platform.Linux false in
 let localRelocationWasPrepared=Array.length (List.hd prepared).relocations=3 in let crossChunkRelocationWasPrepared=Array.length combined.relocations=2 in
 if actual=expected && combinedActual=expected && localRelocationWasPrepared && crossChunkRelocationWasPrepared then Ok () else Error "Prepared chunk composition changed whole-program encoding or retained a group-local relocation"
let testRotatedLogicalImmediateEncoding ()=match encode (AND_imm (X2,X0,0xfffffffffffffff8L)) with [word] when word=0x927df002l -> Ok () | [word] -> Error (Printf.sprintf "AND_imm #~7: expected 0x927DF002, got 0x%08lX" word) | words -> Error (Printf.sprintf "AND_imm #~7: expected 1 word, got %d" (List.length words))
let testBytePopcountSequenceEncoding () =
 let instrs=[Symbolic.FMOV_from_gp (D16,X6);Symbolic.CNT_8B (D16,D16);Symbolic.ADDV_8B (D16,D16);Symbolic.UMOV_byte (X5,D16)] in
 let expected=[|0x9e6700d0l;0x0e205a10l;0x0e31ba10l;0x0e013e05l|] in
 let actual=encodeSymbolicWithPools instrs LiteralPool.emptyStringPool LiteralPool.emptyFloatPool Platform.Linux false in if actual=expected then Ok () else Error "Byte popcount encoding mismatch"
let testInvalidAssertDifferentValueIsRejected () =
 let content="---INPUT-ARM64---\nRET\n\n---OUTPUT-HEX---\n0xD65F03C0\n\n---ASSERT-DIFFERENT---\nmaybe\n" in
 match ARM64EncodingFormat.parseARM64EncodingTest content with Ok _ -> Error "expected invalid ASSERT-DIFFERENT value to be rejected" | Error msg when HostText.contains msg "ASSERT-DIFFERENT" -> Ok () | Error msg -> Error ("expected ASSERT-DIFFERENT parse error, got: "^msg)
let tests=["encodeReg",testEncodeReg;"MOVK shift encoding",testMOVKShiftEncoding;"MOVZ+MOVK sequence",testMOVZMOVKSequence;"combined instruction encoding",testCombinedInstructionEncoding;"unsigned memory offsets reject invalid values",testUnsignedMemoryOffsetsRejectInvalidValues;"signed pair offsets reject invalid values",testSignedPairOffsetsRejectInvalidValues;"arithmetic immediates reject out-of-range values",testArithmeticImmediatesRejectOutOfRangeValues;"move-wide shifts reject invalid values",testMoveWideShiftsRejectInvalidValues;"FMOV immediate encoding",testFMOVImmediateEncoding;"BIC register encoding",testBICRegisterEncoding;"prepared chunks preserve whole-program encoding",testPreparedChunksPreserveWholeProgramEncoding;"rotated logical immediate encoding",testRotatedLogicalImmediateEncoding;"byte popcount sequence encoding",testBytePopcountSequenceEncoding;"invalid ASSERT-DIFFERENT value is rejected",testInvalidAssertDifferentValueIsRejected]
(*
   Run all encoding unit tests
   Returns Ok () if all pass, Error with first failure message if any fail
*)
let runAll () =
 let rec runTests=function [] -> Ok () | (name,test)::rest -> match test () with Ok () -> runTests rest | Error msg -> Error (name^" test failed: "^msg) in runTests tests
