(* Full MIR common-expression and path-completion differential observations. *)
[@@@warning "-4"]
open Dark_compiler
module J = ProductionMIR
module C = MIRCommonExpressions
module M = MIR
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let reg n = M.VReg n
let v n = M.Register (reg n)
let label text = M.Label text
let block name instrs terminator : M.basicBlock = {M.label = label name; instrs; terminator}
let graph entry blocks : M.cfg = {M.entry = label entry; blocks = M.LabelMap.of_list (List.map (fun (block : M.basicBlock) -> block.M.label, block) blocks)}
let key = function
 | C.BinExpr (op, a, b, typ) -> SemanticJson.union "ExprKey" "BinExpr" [J.binOp op; J.operand a; J.operand b; SemanticAST.semanticType typ]
 | C.UnaryExpr (op, src) -> SemanticJson.union "ExprKey" "UnaryExpr" [J.unaryOp op; J.operand src]
 | C.ScalarHeapLoadExpr (addr, offset, typ) -> SemanticJson.union "ExprKey" "ScalarHeapLoadExpr" [J.vReg addr; SemanticJson.int32 offset; SemanticAST.semanticType typ]
 | C.DirectCallExpr (fn, args, typ) -> SemanticJson.union "ExprKey" "DirectCallExpr" [J.functionId fn; list J.operand args; SemanticAST.semanticType typ]
let observe source =
 let fid index = AST.functionId (Int64.of_int index) in
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
 let types = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TFloat64; AST.TBool; AST.TChar; AST.TDateTime; AST.TString; AST.TUnit; AST.TInt128; AST.TUInt128; AST.TTuple [AST.TInt64]] in
 let binary = [M.Add; M.Sub; M.Mul; M.Div; M.Mod; M.Shl; M.Shr; M.BitAnd; M.BitOr; M.BitXor; M.Eq; M.Neq; M.Lt; M.Gt; M.Lte; M.Gte; M.And; M.Or] in
 let operands = [v 2; M.Int64Const (-1L); M.BoolConst true; M.FloatSymbol (-0.); M.FloatSymbol 0.; M.FloatSymbol (Int64.float_of_bits 0x7ff8000000000001L); M.StringSymbol "😀"; M.StringSymbol "\xee\x80\x80"; M.FuncAddr (AST.functionId Int64.min_int); M.FuncAddr (AST.functionId 1L)] in
 let keys = list (fun op -> list (fun a -> list (fun b -> let a', b' = C.normalizeOperands op a b in tuple [`Bool (C.isCommutative op); J.operand a'; J.operand b'; key (C.makeBinExprKey op a b AST.TInt64)]) operands) operands) binary in
 let optimize graph = list (fun functions -> attempt (fun (once, changed, twice, changedAgain) -> tuple [J.cfg once; `Bool changed; J.cfg twice; `Bool changedAgain]) (fun () -> let once, changed = C.applyCSEWithEffectFreeCalls functions graph in let twice, changedAgain = C.applyCSEWithEffectFreeCalls functions once in once, changed, twice, changedAgain)) [SpecializationIdentity.FunctionSet.empty; SpecializationIdentity.FunctionSet.singleton (fid 200)] in
 let barriers = list (fun typ -> list (fun instruction ->
  let expressions dest = [M.BinOp (reg dest, M.Add, v 2, v 3, typ); M.UnaryOp (reg (dest+1), M.Not, v 2); M.HeapLoad (reg (dest+2), reg 2, 3, Some typ); M.Call (reg (dest+3), fid 200, [v 2], [typ], typ)] in
  list optimize [graph source [block source (expressions 10 @ [instruction] @ expressions 20) (M.Ret (v 20))]; graph source [block source (expressions 10 @ [instruction]) (M.Jump (label "child")); block "child" (expressions 20) (M.Ret (v 20))]]) (operations typ (v 2))) types in
 let joins = list (fun typ -> list (fun op ->
  let expression dest = M.BinOp (reg dest, op, v 2, v 3, typ) in
  let join = block "join" [expression 20; M.UnaryOp (reg 21, M.Not, v 2)] (M.Ret (v 20)) in
  let entry = block source [] (M.Branch (v 2, label "left", label "right")) in
  let left = block "left" [expression 10; M.UnaryOp (reg 11, M.Not, v 2)] (M.Jump (label "join")) in
  let right = block "right" [] (M.Jump (label "join")) in
  list optimize [graph source [entry;left;right;join]; graph source [entry;left;{right with M.terminator = M.Branch (v 2, label "join", label "exit")};join;block "exit" [] (M.Ret (v 2))]; graph source [entry;left;right;{join with M.instrs = [M.Mov (reg 2, v 3, Some typ); expression 20]}]; graph source [entry;left;{right with M.instrs = [expression 12]};join]; graph source [block source [] (M.Ret (v 2));left;right;join]]) binary) types in
 tuple [keys; barriers; joins]
