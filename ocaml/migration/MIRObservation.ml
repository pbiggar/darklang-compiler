(* Complete constructor, arithmetic, CFG, purity, scheduling and literal observations. *)
[@@@warning "-4"]
open Dark_compiler
module J = ProductionMIR
module F = MIROptimizationFacts
module C = MIRCopyPropagation
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let attempt encode action = result encode (try Ok (action ()) with Failure message | Invalid_argument message -> Error message)
let fid index = AST.functionId (Int64.of_int index)
let reg value = MIR.VReg value
let v value = MIR.Register (reg value)
let regMap values = `Assoc ["map", list (fun (reg, value) -> tuple [J.vReg reg; J.operand value]) (MIR.VRegMap.bindings values)]
let cfg label instructions terminator : MIR.cfg = {MIR.entry = MIR.Label label; blocks = MIR.LabelMap.singleton (MIR.Label label) {MIR.label = MIR.Label label; instrs = instructions; terminator}}
let functionDef index name graph : MIR.functionDef = {MIR.id = fid index; name; typedParams = [{MIR.reg = reg 2; typ = AST.TInt64}]; returnType = AST.TInt64; cfg = graph; floatRegs = MIR.IntSet.empty}
let purity (value : F.puritySummary) = SemanticJson.record "PuritySummary" ["ObservableEffects", `Bool value.F.observableEffects; "ReadsMutableState", `Bool value.F.readsMutableState; "MayTrap", `Bool value.F.mayTrap; "MayDiverge", `Bool value.F.mayDiverge]
let observe source =
 let variants = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TFloat64; AST.TBool; AST.TString; AST.TUnit; AST.TInt128; AST.TUInt128] in
 let operands = [v 2; MIR.Int64Const 0L; MIR.FloatSymbol (Int64.float_of_bits 0x7ff8000000000001L); MIR.StringSymbol source] in
 let operations typ operand = [
MIR.Mov (MIR.VReg 1, operand, Some typ);
MIR.BinOp (MIR.VReg 1, MIR.Div, operand, operand, typ);
MIR.UnaryOp (MIR.VReg 1, MIR.Not, operand);
MIR.Call (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
MIR.TailCall (fid 200, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
MIR.IndirectCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
MIR.IndirectTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
MIR.ClosureAlloc (MIR.VReg 1, fid 200, [operand; MIR.Register (MIR.VReg 3)]);
MIR.ClosureCall (MIR.VReg 1, operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ], typ);
MIR.ClosureTailCall (operand, [operand; MIR.Register (MIR.VReg 3)], [typ; typ]);
MIR.HeapAlloc (MIR.VReg 1, 3);
MIR.HeapStore (MIR.VReg 1, 3, operand, Some typ);
MIR.HeapLoad (MIR.VReg 1, MIR.VReg 2, 3, Some typ);
MIR.StringConcat (MIR.VReg 1, operand, operand, [operand; MIR.Register (MIR.VReg 3)]);
MIR.CanonicalBufferEq (MIR.VReg 1, MemoryModel.Utf8String, operand, operand);
MIR.RefCountInc (MIR.VReg 1, 3, MIR.GenericHeap, None);
MIR.RefCountDec (MIR.VReg 1, 3, MIR.GenericHeap, None);
MIR.Print (operand, typ);
MIR.StdoutWrite (3, operand, true);
MIR.StdinReadLine (MIR.VReg 1);
MIR.RuntimeError (source);
MIR.RuntimeErrorString (operand);
MIR.FileReadBlob (MIR.VReg 1, operand);
MIR.FileExists (MIR.VReg 1, operand);
MIR.FileWriteBlob (MIR.VReg 1, operand, operand);
MIR.FileAppendText (MIR.VReg 1, operand, operand);
MIR.FileDelete (MIR.VReg 1, operand);
MIR.FileCreateDirectory (MIR.VReg 1, operand);
MIR.FileSetExecutable (MIR.VReg 1, operand);
MIR.FileWriteFromPtr (MIR.VReg 1, operand, operand, operand);
MIR.FloatSqrt (MIR.VReg 1, operand);
MIR.FloatAbs (MIR.VReg 1, operand);
MIR.FloatNeg (MIR.VReg 1, operand);
MIR.Int64ToFloat (MIR.VReg 1, operand);
MIR.FloatToInt64 (MIR.VReg 1, operand);
MIR.FloatToBits (MIR.VReg 1, operand);
MIR.RawAlloc (MIR.VReg 1, operand);
MIR.MappedAlloc (MIR.VReg 1, operand);
MIR.RawFree (operand);
MIR.MappedFree (operand);
MIR.RawGet (MIR.VReg 1, operand, operand, Some typ);
MIR.RawGetByte (MIR.VReg 1, operand, operand);
MIR.RawWriteWord (operand, operand, operand);
MIR.RawWriteByte (operand, operand, operand);
MIR.RawSlotInit (operand, operand, operand, typ);
MIR.StringToRawPtr (MIR.VReg 1, operand);
MIR.RawPtrToString (MIR.VReg 1, operand);
MIR.BlobToRawPtr (MIR.VReg 1, operand);
MIR.RawPtrToBlob (MIR.VReg 1, operand);
MIR.DictToRawPtr (MIR.VReg 1, operand);
MIR.RawPtrToDict (MIR.VReg 1, operand, operand);
MIR.ListToRawPtr (MIR.VReg 1, operand);
MIR.RawPtrToList (MIR.VReg 1, operand, operand);
MIR.RefCountIncString (operand);
MIR.RefCountDecString (operand);
MIR.RefCountIncBlob (operand);
MIR.RefCountDecBlob (operand);
MIR.RefCountIncInt (operand);
MIR.RefCountDecInt (operand);
MIR.RandomInt64 (MIR.VReg 1);
MIR.DateTimeNow (MIR.VReg 1);
MIR.Sleep (3, MIR.VReg 2, operand);
MIR.CliNative (MIR.VReg 1, MIR.HostOS, [operand; MIR.Register (MIR.VReg 3)]);
MIR.FloatToString (MIR.VReg 1, operand);
MIR.Phi (MIR.VReg 1, [operand, MIR.Label source; MIR.Register (MIR.VReg 3), MIR.Label "other"], Some typ);
MIR.CoverageHit (3) ] in
 let copyCases = [MIR.VRegMap.empty; MIR.VRegMap.of_list [reg 2, v 5; reg 3, MIR.Int64Const 0x4000000000000000L]; MIR.VRegMap.of_list [reg 2, v 3; reg 3, v 2]; MIR.VRegMap.singleton (reg 2) (MIR.BoolConst true)] in
 let instructionFacts = list (fun typ -> list (fun operand -> list (fun instruction -> tuple [J.instr instruction; option J.vReg (F.getInstrDest instruction); list J.vReg (F.foldInstrUses (fun values reg -> values @ [reg]) [] instruction); `Assoc ["set", list J.vReg (MIR.VRegSet.elements (F.getInstrUses instruction))]; `Bool (F.hasSideEffects instruction); list (fun copies -> attempt J.instr (fun () -> C.propagateCopyInstr copies instruction)) copyCases]) (operations typ operand)) operands) [AST.TInt64; AST.TFloat64] in
 let binary = [MIR.Add; MIR.Sub; MIR.Mul; MIR.Div; MIR.Mod; MIR.Shl; MIR.Shr; MIR.BitAnd; MIR.BitOr; MIR.BitXor; MIR.Eq; MIR.Neq; MIR.Lt; MIR.Gt; MIR.Lte; MIR.Gte; MIR.And; MIR.Or] in
 let scalarOperands = [MIR.Int64Const Int64.min_int; MIR.Int64Const (-1L); MIR.Int64Const 0L; MIR.Int64Const 1L; MIR.Int64Const 2L; MIR.Int64Const 256L; MIR.Int64Const Int64.max_int; MIR.BoolConst false; MIR.BoolConst true; v 2; MIR.FloatSymbol (Int64.float_of_bits 0xfff8000000000000L); MIR.FloatSymbol (-0.)] in
 let constants = list (fun typ -> list (fun operation -> list (fun left -> list (fun right -> option J.operand (MIRConstants.tryFoldBinOp operation left right typ)) scalarOperands) scalarOperands) binary) variants in
 let copies = list (fun values -> tuple [regMap values; list (fun operand -> J.operand (C.resolveCopy values operand)) operands; regMap (C.resolveCopyMap values)]) copyCases in
 let graphCases = [cfg source [MIR.Mov (reg 1, v 2, Some AST.TInt64); MIR.Mov (reg 3, v 1, Some AST.TInt64); MIR.BinOp (reg 4, MIR.Add, v 3, MIR.Int64Const 1L, AST.TInt64)] (MIR.Ret (v 4)); cfg source [MIR.Phi (reg 1, [v 1, MIR.Label source], Some AST.TInt64); MIR.BinOp (reg 3, MIR.Add, v 1, v 2, AST.TInt64)] (MIR.Ret (v 2)); cfg source [MIR.Mov (reg 1, MIR.Int64Const 0L, Some AST.TString); MIR.Mov (reg 3, MIR.Int64Const 0L, None); MIR.Print (v 1, AST.TString)] (MIR.Ret (v 3)); cfg source [MIR.Mov (reg 1, v 2, Some AST.TInt64); MIR.Phi (reg 1, [v 3, MIR.Label source], Some AST.TInt64)] (MIR.Ret (v 1)); cfg source [MIR.BinOp (reg 1, MIR.Div, v 2, MIR.Int64Const 0L, AST.TInt64)] (MIR.Ret (MIR.Int64Const 0L))] in
 let graphs = list (fun graph -> let copies = C.buildCopyMap graph in let optimized, changed = MIRDeadCode.eliminateDeadCode graph in tuple [regMap copies; regMap (C.resolveCopyMap copies); J.cfg optimized; `Bool changed]) graphCases in
 let make index name instructions term = functionDef index name (cfg name instructions term) in
 let leaf = make 200 "leaf" [] (MIR.Ret (v 2)) in
 let caller = make 400 "caller" [MIR.Call (reg 1, fid 200, [v 2], [AST.TInt64], AST.TInt64)] (MIR.Ret (v 1)) in
 let cycle = make 500 "cycle" [] (MIR.Jump (MIR.Label "cycle")) in
 let recursive = make 600 "recursive" [MIR.Call (reg 1, fid 600, [v 2], [AST.TInt64], AST.TInt64)] (MIR.Ret (v 1)) in
 let unknown = make 700 "unknown" [MIR.Call (reg 1, fid 900, [v 2], [AST.TInt64], AST.TInt64)] (MIR.Ret (v 1)) in
 let reader = make 800 "reader" [MIR.RawGet (reg 1, v 2, MIR.Int64Const 0L, Some AST.TInt64)] (MIR.Ret (v 1)) in
 let programs = [[]; [caller; leaf]; [leaf; caller; leaf]; [cycle; recursive; unknown; reader; caller; leaf]; [caller; recursive; {recursive with MIR.id = fid 200}; leaf]] in
 let analyses = list (fun functions -> tuple [list (fun component -> SemanticJson.record "Component" ["NodeIndices", list SemanticJson.int32 component.CallGraphSchedule.nodeIndices; "SCCs", list (list J.functionDef) component.CallGraphSchedule.sccs; "Functions", list J.functionDef component.CallGraphSchedule.functions]) (CallGraphSchedule.calleeFirst functions); `Assoc ["set", list J.functionId (SpecializationIdentity.FunctionSet.elements (F.analyzeEffectFreeFunctions functions))]; list (fun (id, summary) -> tuple [J.operand (MIR.FuncAddr id); purity summary]) (FunctionIdMap.toList (F.analyzePurityWithKnown FunctionIdMap.empty functions))]) programs in
 let moveCases = [[1, v 2]; [1, v 2; 2, v 1]; [1, MIR.Int64Const 0L; 2, v 1]; [1, v 1]; [1, v 2; 2, v 3; 3, v 1]; [1, v 2; 1, v 3; 2, v 1]; [1, v 2; 3, v 2; 2, v 1]; [1, MIR.Int64Const 0L; 2, MIR.StringSymbol source]] in
 let moves = list (fun values -> list (function ParallelMoves.SaveToTemp reg -> SemanticJson.union "MoveAction" "SaveToTemp" [SemanticJson.int32 reg] | ParallelMoves.Move (reg, source) -> SemanticJson.union "MoveAction" "Move" [SemanticJson.int32 reg; J.operand source] | ParallelMoves.MoveFromTemp reg -> SemanticJson.union "MoveAction" "MoveFromTemp" [SemanticJson.int32 reg]) (ParallelMoves.resolve values (function MIR.Register (MIR.VReg reg) -> Some reg | _ -> None))) moveCases in
 let strings = [source; "😀"; "é"; "é"; "\000"; HostText.ofUtf16Units [|0xd800|]; HostText.ofUtf16Units [|0xdfff|]; HostText.ofUtf16Units [|0xd800; 0xdfff|]; source] in
 let stringPool = LiteralPool.createStringPool (List.to_seq strings) in
 let floats = [0.; -0.; Int64.float_of_bits 0x7ff8000000000001L; Int64.float_of_bits 0x7ff8000000000002L; Float.infinity; Float.neg_infinity; Int64.float_of_bits 0x7ff8000000000001L] in
 let floatPool = LiteralPool.createFloatPool (List.to_seq floats) in
 let pools = tuple [SemanticJson.record "StringPool" ["Strings", list (fun (text, length) -> tuple [SemanticJson.string text; SemanticJson.int32 length]) (Array.to_list stringPool.LiteralPool.strings); "StringToId", `Assoc ["map", list (fun (text, index) -> tuple [SemanticJson.string text; SemanticJson.int32 index]) (StringOrder.Map.bindings stringPool.LiteralPool.stringToId)]]; SemanticJson.record "FloatPool" ["Floats", list (fun value -> `Assoc ["kind", `String "float64"; "value", `String (Printf.sprintf "%016Lx" (Int64.bits_of_float value))]) (Array.to_list floatPool.LiteralPool.floats); "FloatBitsToId", `Assoc ["map", list (fun (bits, index) -> tuple [`Assoc ["kind", `String "int64"; "value", `String (Int64.to_string bits)]; SemanticJson.int32 index]) (LiteralPool.FloatBitsMap.bindings floatPool.LiteralPool.floatBitsToId)]]] in
 tuple [instructionFacts; constants; copies; graphs; analyses; moves; pools]
