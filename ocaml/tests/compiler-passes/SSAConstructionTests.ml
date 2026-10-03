(*
   These tests cover internal CFG invariants that cannot be exercised cleanly
   through source-level end-to-end programs.
*)
(* SSAConstructionTests.fs - Unit tests for MIR SSA construction invariants. *)
[@@@warning "-4-42"]
open Dark_compiler
open MIR
open SSA_Construction
type testResult = (unit, string) result
let label name = Label name
let vreg id = VReg id
let makeBlock label instrs terminator : basicBlock = {label; instrs; terminator}
let format value = HostStructuralFormat.format value
let uses values = format (StructuralValue.Union ("set", [StructuralValue.Sequence (List.map MIRTestFormatting.vReg (VRegSet.elements values))]))
let instructions values = format (StructuralValue.Sequence (List.map MIRTestFormatting.instr values))
let labels values = format (StructuralValue.Union ("map", [StructuralValue.Sequence (List.map (fun (left, right) -> StructuralValue.Tuple [MIRTestFormatting.label left; MIRTestFormatting.label right]) (LabelMap.bindings values))]))
let testGetBlockUsesCoversEveryOperandPosition () =
 let destination = vreg 0 and first = vreg 1 and second = vreg 2 and third = vreg 3 in
 let register reg = Register reg in let expected regs = VRegSet.of_list regs in let target = label "target" in
 let instructionCases = [
("Mov", Mov (destination, register first, Some AST.TInt64), expected [first]);
("BinOp", BinOp (destination, Add, register first, register second, AST.TInt64), expected [first; second]);
("UnaryOp", UnaryOp (destination, Neg, register first), expected [first]);
("Call", Call (destination, TestIds.functionIdForName "callee", [register first; Int64Const 1L; register second], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second]);
("TailCall", TailCall (TestIds.functionIdForName "callee", [register first; Int64Const 1L; register second], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second]);
("IndirectCall", IndirectCall (destination, register first, [register second; Int64Const 1L; register third], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second; third]);
("IndirectTailCall", IndirectTailCall (register first, [register second; Int64Const 1L; register third], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second; third]);
("ClosureAlloc", ClosureAlloc (destination, TestIds.functionIdForName "callee", [register first; Int64Const 1L; register second]), expected [first; second]);
("ClosureCall", ClosureCall (destination, register first, [register second; Int64Const 1L; register third], [AST.TInt64; AST.TInt64; AST.TInt64], AST.TInt64), expected [first; second; third]);
("ClosureTailCall", ClosureTailCall (register first, [register second; Int64Const 1L; register third], [AST.TInt64; AST.TInt64; AST.TInt64]), expected [first; second; third]);
("HeapStore", HeapStore (first, 0, register second, Some AST.TInt64), expected [first; second]);
("HeapLoad", HeapLoad (destination, first, 0, Some AST.TInt64), expected [first]);
("StringConcat", StringConcat (destination, register first, register second, []), expected [first; second]);
("RefCountInc", RefCountInc (first, 8, GenericHeap, None), expected [first]);
("RefCountDec", RefCountDec (first, 8, GenericHeap, None), expected [first]);
("Print", Print (register first, AST.TInt64), expected [first]);
("FileReadBlob", FileReadBlob (destination, register first), expected [first]);
("FileExists", FileExists (destination, register first), expected [first]);
("FileWriteBlob", FileWriteBlob (destination, register first, register second), expected [first; second]);
("FileAppendText", FileAppendText (destination, register first, register second), expected [first; second]);
("FileDelete", FileDelete (destination, register first), expected [first]);
("FileSetExecutable", FileSetExecutable (destination, register first), expected [first]);
("FileWriteFromPtr", FileWriteFromPtr (destination, register first, register second, register third), expected [first; second; third]);
("Phi", Phi (destination, [(register first, target); (Int64Const 1L, target); (register second, target)], Some AST.TInt64), expected [first; second]);
("RawAlloc", RawAlloc (destination, register first), expected [first]);
("RawFree", RawFree (register first), expected [first]);
("RawGet", RawGet (destination, register first, register second, Some AST.TInt64), expected [first; second]);
("RawGetByte", RawGetByte (destination, register first, register second), expected [first; second]);
("RawWriteWord", RawWriteWord (register first, register second, register third), expected [first; second; third]);
("RawWriteByte", RawWriteByte (register first, register second, register third), expected [first; second; third]);
("RawSlotInit", RawSlotInit (register first, register second, register third, AST.TInt64), expected [first; second; third]);
("StringToRawPtr", StringToRawPtr (destination, register first), expected [first]);
("RawPtrToString", RawPtrToString (destination, register first), expected [first]);
("BlobToRawPtr", BlobToRawPtr (destination, register first), expected [first]);
("RawPtrToBlob", RawPtrToBlob (destination, register first), expected [first]);
("DictToRawPtr", DictToRawPtr (destination, register first), expected [first]);
("RawPtrToDict", RawPtrToDict (destination, register first, register second), expected [first; second]);
("ListToRawPtr", ListToRawPtr (destination, register first), expected [first]);
("RawPtrToList", RawPtrToList (destination, register first, register second), expected [first; second]);
("FloatSqrt", FloatSqrt (destination, register first), expected [first]);
("FloatAbs", FloatAbs (destination, register first), expected [first]);
("FloatNeg", FloatNeg (destination, register first), expected [first]);
("Int64ToFloat", Int64ToFloat (destination, register first), expected [first]);
("FloatToInt64", FloatToInt64 (destination, register first), expected [first]);
("FloatToBits", FloatToBits (destination, register first), expected [first]);
("RefCountIncString", RefCountIncString (register first), expected [first]);
("RefCountDecString", RefCountDecString (register first), expected [first]);
("RefCountIncBlob", RefCountIncBlob (register first), expected [first]);
("RefCountDecBlob", RefCountDecBlob (register first), expected [first]);
("FloatToString", FloatToString (destination, register first), expected [first]) ] in
 let terminators = ["Ret", Ret (register first), expected [first]; "Branch", Branch (register first, target, target), expected [first]; "Jump", Jump target, VRegSet.empty] in
 let check name block expected = let actual = getBlockUses block in if VRegSet.equal actual expected then None else Some (name ^ ": expected uses " ^ uses expected ^ ", got " ^ uses actual) in
 match List.find_map (fun (name, instr, expected) -> check name (makeBlock (label name) [instr] (Jump target)) expected) instructionCases with Some error -> Error error | None -> (match List.find_map (fun (name, term, expected) -> check name (makeBlock (label name) [] term) expected) terminators with Some error -> Error error | None -> Ok ())
let testComputeLivenessReportsMissingSuccessorBlock () =
 let entry = label "entry" and missing = label "missing" in let cfg = {entry; blocks = LabelMap.singleton entry (makeBlock entry [Mov (vreg 0, Int64Const 1L, Some AST.TInt64)] (Jump missing))} in
 try ignore (computeLiveness cfg); Error "Expected computeLiveness to report the missing successor block" with Failure message -> if HostText.contains "SSA: Missing CFG block missing while computing liveness successor" message then Ok () else Error ("Expected contextual SSA missing-block message, got: " ^ message)
let testComputeDominatorsHandlesJoinLoopAndUnreachableBlock () =
 let entry = label "entry" and left = label "left" and right = label "right" and join = label "join" and header = label "header" and body = label "body" and exit = label "exit" and unreachable = label "unreachable" in
 let blocks = [makeBlock entry [] (Branch (Register (vreg 0), left, right)); makeBlock left [] (Jump join); makeBlock right [] (Jump join); makeBlock join [] (Jump header); makeBlock header [] (Branch (Register (vreg 1), body, exit)); makeBlock body [] (Jump header); makeBlock exit [] (Ret (Int64Const 0L)); makeBlock unreachable [] (Ret (Int64Const 1L))] in
 let cfg = {entry; blocks = LabelMap.of_list (List.map (fun block -> block.label, block) blocks)} in
 let expected = LabelMap.of_list [left, entry; right, entry; join, entry; header, join; body, header; exit, header] in
 let actual = computeDominators cfg (buildPredecessors cfg) in if LabelMap.equal (=) actual expected then Ok () else Error ("Expected immediate dominators " ^ labels expected ^ ", got " ^ labels actual)
let testSSAVersionsStartAboveParameterRegisters () =
 let entry = label "entry" in let parameter : typedMIRParam = {reg = vreg 10000; typ = AST.TInt64} in
 let func : functionDef = {id = TestIds.functionIdForName "test"; name = "test"; typedParams = [parameter]; returnType = AST.TInt64; cfg = {entry; blocks = LabelMap.singleton entry (makeBlock entry [RawAlloc (vreg 0, Int64Const 8L)] (Ret (Register parameter.reg)))}; floatRegs = IntSet.empty} in
 let converted = convertFunctionToSSA func in match LabelMap.find_opt converted.cfg.entry converted.cfg.blocks with
 | Some {instrs = RawAlloc (VReg destination, _) :: _; _} when destination <> 10000 -> Ok ()
 | Some {instrs = RawAlloc (VReg destination, _) :: _; _} -> Error ("SSA construction reused parameter VReg 10000 for instruction destination " ^ string_of_int destination)
 | Some block -> Error ("Expected converted entry block to start with RawAlloc, got " ^ instructions block.instrs)
 | None -> Error "Expected converted CFG to contain its entry block"
let testDeferredPhiUpdatesPreserveInstructionAndSourceOrder () =
 let left = label "left" and right = label "right" and join = label "join" and first = vreg 1 and second = vreg 2 in
 let block = makeBlock join [Phi (vreg 3, [Register first, right; Register first, left], Some AST.TInt64); Phi (vreg 4, [Register second, left; Register second, right], Some AST.TInt64); BinOp (vreg 5, Add, Register (vreg 3), Register (vreg 4), AST.TInt64)] (Ret (Register (vreg 5))) in
 let updates = PhiUpdateMap.of_list [(join, left, first), Register (vreg 101); (join, right, first), Register (vreg 102); (join, left, second), Register (vreg 201); (join, right, second), Register (vreg 202)] in
 let expected = [Phi (vreg 3, [Register (vreg 102), right; Register (vreg 101), left], Some AST.TInt64); Phi (vreg 4, [Register (vreg 201), left; Register (vreg 202), right], Some AST.TInt64); BinOp (vreg 5, Add, Register (vreg 3), Register (vreg 4), AST.TInt64)] in
 let updated = applyPhiSourceUpdates updates block in if updated.instrs = expected then Ok () else Error ("Expected deferred phi updates to preserve order, got " ^ instructions updated.instrs)
let tests = ["getBlockUses covers every operand position", testGetBlockUsesCoversEveryOperandPosition; "computeLiveness reports missing successor block", testComputeLivenessReportsMissingSuccessorBlock; "computeDominators handles join, loop, and unreachable block", testComputeDominatorsHandlesJoinLoopAndUnreachableBlock; "SSA versions start above parameter registers", testSSAVersionsStartAboveParameterRegisters; "deferred phi updates preserve instruction and source order", testDeferredPhiUpdatesPreserveInstructionAndSourceOrder]
