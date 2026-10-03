(* Full SCCP, path/heap facts, and fixed-point scheduler observations. *)
[@@@warning "-4"]
open Dark_compiler
module M = MIR
module J = ProductionMIR
module C = MIRSparseConditionalConstants
module F = MIROptimizationFacts
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let reg n = M.VReg n
let v n = M.Register (reg n)
let fid n = AST.functionId (Int64.of_int n)
let label text = M.Label text
let block name instrs terminator : M.basicBlock = {M.label = label name; instrs; terminator}
let graph entry blocks : M.cfg = {M.entry = label entry; blocks = M.LabelMap.of_list (List.map (fun (block : M.basicBlock) -> block.M.label, block) blocks)}
let cfgChange (cfg, changed) = tuple [J.cfg cfg; `Bool changed]
let observe source =
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
 let transforms = [C.applySparseConditionalConstantPropagation; C.applySparseConditionalConstantPropagationWithCallResults (fun fn -> if fn = fid 200 then Some (M.BoolConst true) else None); C.applySparseConditionalSimplification] in
 let observe cfg = list (fun transform -> attempt cfgChange (fun () -> transform cfg)) transforms in
 let types = [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TFloat64;AST.TBool;AST.TString;AST.TChar;AST.TDateTime;AST.TUnit;AST.TInt128;AST.TSum ("Option",[AST.TInt64]);AST.TList AST.TInt64;AST.TDict (AST.TInt64, AST.TInt64);AST.TTuple [AST.TInt64]] in
 let instructionCases = list (fun typ -> list (fun operand -> list (fun instr ->
  let instructions = [M.Mov (reg 2, operand, Some typ); M.Mov (reg 3, v 2, Some typ); instr] in
  list observe [graph source [block source instructions (M.Ret (v 1))]; graph source [block source instructions (M.Branch (M.BoolConst true, label "child", label "dead")); block "child" [] (M.Ret (v 1)); block "dead" [] (M.Ret (v 2))]]) (operations typ (v 3))) [M.Int64Const 0L; M.FloatSymbol (Int64.float_of_bits 0x7ff8000000000001L); M.StringSymbol source; v 4]) [AST.TInt64;AST.TInt8;AST.TUInt32;AST.TFloat64;AST.TString;AST.TBool] in
 let comparisons = [M.Eq; M.Neq; M.Lt; M.Gt; M.Lte; M.Gte] in
 let paths = list (fun typ -> list (fun op -> list (fun bound ->
  let entry = block source [M.BinOp (reg 1, op, v 4, M.Int64Const bound, typ); M.UnaryOp (reg 2, M.Not, v 1); M.Mov (reg 3, v 2, Some AST.TBool)] (M.Branch (v 3, label "yes", label "no")) in
  let yes = block "yes" [M.BinOp (reg 5, op, v 4, M.Int64Const bound, typ); M.BinOp (reg 6, M.And, v 5, v 1, AST.TBool)] (M.Branch (v 6, label "a", label "b")) in
  let no = block "no" [M.BinOp (reg 7, M.Or, v 1, M.BoolConst false, AST.TBool)] (M.Branch (v 7, label "a", label "b")) in
  observe (graph source [entry;yes;no;block "a" [] (M.Ret (M.Int64Const 1L));block "b" [] (M.Ret (M.Int64Const 2L))])) [Int64.min_int;-1L;0L;255L;Int64.max_int]) comparisons) types in
 let heaps = list (fun typ -> list (fun count ->
  let arms = List.init count (fun i -> let name = "arm" ^ string_of_int i in block name [M.HeapAlloc (reg (10+i), 16); M.HeapStore (reg (10+i), 8, M.Int64Const (if i = count-1 && count > 1 then 1L else 0L), Some AST.TInt64)] (M.Jump (label "join"))) in
  let branches = List.init (max 0 (count-1)) (fun i -> block (if i = 0 then source else "branch" ^ string_of_int i) [] (M.Branch (v 4, label ("arm" ^ string_of_int i), label (if i = count-2 then "arm" ^ string_of_int (count-1) else "branch" ^ string_of_int (i+1))))) in
  let sources = List.mapi (fun i (block : M.basicBlock) -> v (10+i), block.M.label) arms in
  let entry = if count = 1 then [block source [] (M.Jump (label "arm0"))] else branches in
  observe (graph source (entry @ arms @ [block "join" [M.Phi (reg 1, sources, Some typ); M.HeapLoad (reg 2, reg 1, 8, Some AST.TInt64); M.BinOp (reg 3, M.Eq, v 2, M.Int64Const 0L, AST.TInt64)] (M.Branch (v 3, label "yes", label "no"));block "yes" [] (M.Ret (v 2));block "no" [] (M.Ret (v 2))]))) [1;2;16;17]) [AST.TTuple [AST.TInt64];AST.TSum ("Option",[AST.TInt64]);AST.TList AST.TInt64;AST.TDict (AST.TInt64, AST.TInt64)] in
 let floats = [0.;-0.;1.;-1.;infinity;neg_infinity;Int64.float_of_bits 0x7ff8000000000001L;Int64.float_of_bits 0xfff8000000000002L] in
 let arithmetic = list (fun op -> list (fun a -> list (fun b -> observe (graph source [block source [M.Mov (reg 2, M.FloatSymbol a, Some AST.TFloat64); M.Mov (reg 3, M.FloatSymbol b, Some AST.TFloat64); M.BinOp (reg 1, op, v 2, v 3, AST.TFloat64); M.FloatAbs (reg 5, v 1); M.FloatToInt64 (reg 6, v 5); M.FloatToBits (reg 7, v 5)] (M.Branch (v 1, label "yes", label "no"));block "yes" [] (M.Ret (v 7));block "no" [] (M.Ret (v 6))])) floats) floats) [M.Add;M.Sub;M.Mul;M.Div;M.Mod;M.Eq;M.Neq;M.Lt;M.Gt;M.Lte;M.Gte] in
 let cfgs = [graph source [block source [M.Mov (reg 1, M.Int64Const 0L, Some AST.TInt64); M.Mov (reg 2, v 1, Some AST.TInt64)] (M.Ret (v 2))]; graph source [block source [M.Call (reg 1, fid 200, [], [], AST.TBool)] (M.Branch (v 1, label "yes", label "no"));block "yes" [] (M.Ret (M.Int64Const 1L));block "no" [] (M.Ret (M.Int64Const 2L))]; graph source [block source [] (M.Jump (label "missing"))];graph source []] in
 let functionDef id name cfg : M.functionDef = {M.id = fid id;name;typedParams = [];returnType = AST.TInt64;cfg;floatRegs = M.IntSet.empty} in
 let leaf = functionDef 200 "leaf" (graph "leaf" [block "leaf" [] (M.Ret (M.BoolConst true))]) in
 let programs = list (fun cfg -> list (fun bits ->
  let options : F.optimizeOptions = {F.enableSCCP = bits land 1 <> 0;enableCSE = bits land 2 <> 0;enableDCE = bits land 4 <> 0;enableLICM = bits land 8 <> 0} in
  let func = functionDef 400 source cfg in let program = M.Program ([leaf;func], StringOrder.Map.empty, StringOrder.Map.empty) in
  tuple [attempt cfgChange (fun () -> MIR_Optimize.optimizeCFGOnce options cfg);attempt J.cfg (fun () -> MIR_Optimize.optimizeCFGWithOptions options cfg);attempt J.functionDef (fun () -> MIR_Optimize.optimizeFunctionWithOptions options func);option J.operand (MIR_Optimize.constantReturnOperand func);attempt J.program (fun () -> MIR_Optimize.optimizeProgramWithOptions options program);
   attempt (fun (program, timings) -> tuple [J.program program;list (fun (name, valid) -> tuple [SemanticJson.string name;`Bool valid]) timings]) (fun () -> let timings = ref [] in let program = MIR_Optimize.optimizeProgramWithOptionsAndTrace (Some (fun name elapsed -> timings := (name, elapsed >= 0.) :: !timings)) options program in program, List.rev !timings)]) (List.init 16 Fun.id)) cfgs in
 tuple [instructionCases;paths;heaps;arithmetic;list observe cfgs;programs]
