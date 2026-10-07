(* IRSymbolTests.fs - Unit tests for symbolic IR pool references
   Validates conversion between pooled refs and symbolic refs used for late pool resolution. *)
[@@@warning "-4-42"]
open Dark_compiler
module M=MIR
module L=LIR
(* Test result type *)
type testResult=(unit,string) result
(* Sequential allocation distinguishes names that collided under the former
   31-bit FNV representation. *)
let testFunctionIdentitiesDoNotHashCollide ()=
 let generated=TestIds.functionIdForName "e2eBatchde340de328b830e8_Check5" in let stdlib=TestIds.functionIdForName "Darklang.Stdlib.Int64.__digitToString" in
 if generated<>stdlib then Ok () else Error "Distinct function names received the same semantic identity"
(* A threaded catalog reuses a name's identity and allocates the next
   declaration immediately after it. *)
let testFunctionIdentitiesComposeAcrossUnits ()=
 let name="User.Module.generated<Int64>" in let first,symbols=CheckedAST.internFunction name (CheckedAST.emptySymbols ()) in
 let second,symbols=CheckedAST.internFunction name symbols in let next,_=CheckedAST.internFunction "User.Module.next" symbols in
 if first=second && AST.functionIdValue next=Int64.add (AST.functionIdValue first) 1L then Ok () else Error "Threaded function allocation did not preserve and advance identities"
let instructions func=L.LabelMap.bindings func.L.cfg.L.blocks |> List.concat_map (fun (_,block)->block.L.instrs)
let formatInstructions values=StructuralFormat.format (StructuralValue.Sequence (List.map LIRTestFormatting.instr values))
let testMirToLirSymbolicOperands ()=
 let label=M.Label "entry" in
 let instrs=[M.Mov (M.VReg 0,M.StringSymbol "mir_symbolic",Some AST.TString);M.Mov (M.VReg 1,M.FloatSymbol 4.5,Some AST.TFloat64)] in
 let block:M.basicBlock={M.label;instrs;terminator=M.Ret (M.Register (M.VReg 0))} in
 let cfg:M.cfg={M.entry=label;blocks=M.LabelMap.singleton label block} in
 let func:M.functionDef={M.id=TestIds.functionIdForName "mir_symbolic_operands";name="mir_symbolic_operands";typedParams=[];returnType=AST.TString;cfg;floatRegs=M.IntSet.singleton 1} in
 match MIR_to_LIR.toLIR (M.Program ([func],StringOrder.Map.empty,StringOrder.Map.empty)) with
 |Error error->Error ("MIR→LIR failed: "^error)
 |Ok (L.Program (funcs,_,_))->match funcs with
 |[lirFunc]->if List.exists (function L.Mov (_,L.StringSymbol value)->value="mir_symbolic"|L.Mov (_,L.FloatSymbol value)->value=4.5|_->false) (instructions lirFunc) then Ok () else Error "Expected MIR→LIR to preserve symbolic operands"
 |_->Error "Expected a single LIR function"
let testMirToLirReportsMissingEntryBlock ()=
 let entry=M.Label "entry" in let actual=M.Label "actual" in
 let block:M.basicBlock={M.label=actual;instrs=[M.Mov (M.VReg 0,M.Int64Const 42L,Some AST.TInt64)];terminator=M.Ret (M.Register (M.VReg 0))} in
 let cfg:M.cfg={M.entry;blocks=M.LabelMap.singleton actual block} in
 let func:M.functionDef={M.id=TestIds.functionIdForName "missing_entry";name="missing_entry";typedParams=[];returnType=AST.TInt64;cfg;floatRegs=M.IntSet.empty} in
 match MIR_to_LIR.toLIR (M.Program ([func],StringOrder.Map.empty,StringOrder.Map.empty)) with
 |Error error when Text.contains error "missing entry block"->Ok ()
 |Error error->Error ("Expected missing entry block error, got '"^error^"'")
 |Ok _->Error "Expected MIR→LIR to reject a CFG whose entry block is absent"
(* Native 64-bit variable shifts already mask their count in both supported
   instruction sets. Ensure lowering does not emit a redundant explicit mask. *)
let testMirToLirUsesNativeInt64ShiftMask ()=
 let label=M.Label "entry" in
 let block:M.basicBlock={M.label;instrs=[M.BinOp (M.VReg 2,M.Shl,M.Register (M.VReg 0),M.Register (M.VReg 1),AST.TInt64);M.BinOp (M.VReg 3,M.Shr,M.Register (M.VReg 0),M.Register (M.VReg 1),AST.TInt64)];terminator=M.Ret (M.Register (M.VReg 3))} in
 let cfg:M.cfg={M.entry=label;blocks=M.LabelMap.singleton label block} in
 let func:M.functionDef={M.id=TestIds.functionIdForName "native_int64_shift_mask";name="native_int64_shift_mask";typedParams=[{M.reg=M.VReg 0;typ=AST.TInt64};{M.reg=M.VReg 1;typ=AST.TInt64}];returnType=AST.TInt64;cfg;floatRegs=M.IntSet.empty} in
 match MIR_to_LIR.toLIR (M.Program ([func],StringOrder.Map.empty,StringOrder.Map.empty)) with
 |Error error->Error ("MIR→LIR failed: "^error)
 |Ok (L.Program ([lirFunc],_,_))->let instrs=instructions lirFunc in
 if List.exists (function L.And_imm (_,_,63L)->true|_->false) instrs then Error "Expected native Int64 variable shift lowering to omit AND #63"
 else if List.exists (function L.Lsl _->true|_->false) instrs && List.exists (function L.Asr _->true|_->false) instrs then Ok () else Error "Expected native Int64 variable shift lowering to emit Lsl and Asr"
 |Ok _->Error "Expected a single LIR function"
(* Float field stores need an ordinary virtual GP temporary. A fixed scratch
   register can alias the scratch used to reload a spilled record address on x64. *)
let testMirToLirAllocatesFloatHeapStoreTemporary ()=
 let label=M.Label "entry" in
 let block:M.basicBlock={M.label;instrs=[M.HeapAlloc (M.VReg 0,8);M.Mov (M.VReg 1,M.FloatSymbol 42.5,Some AST.TFloat64);M.HeapStore (M.VReg 0,0,M.Register (M.VReg 1),Some AST.TFloat64)];terminator=M.Ret (M.Register (M.VReg 0))} in
 let cfg:M.cfg={M.entry=label;blocks=M.LabelMap.singleton label block} in
 let func:M.functionDef={M.id=TestIds.functionIdForName "float_heap_store_temp";name="float_heap_store_temp";typedParams=[];returnType=AST.TInt64;cfg;floatRegs=M.IntSet.singleton 1} in
 match MIR_to_LIR.toLIRFor Platform.X86_64 (M.Program ([func],StringOrder.Map.empty,StringOrder.Map.empty)) with
 |Error error->Error ("MIR→LIR failed: "^error)
 |Ok (L.Program ([lirFunc],_,_))->let instrs=instructions lirFunc in
 let rec pairs=function left::(right::_ as rest)->(left,right)::pairs rest|[]|[_]->[] in
 if List.exists (function L.FpToGp (L.Virtual tempId,L.FVirtual 1),L.HeapStore (L.Virtual 0,0,L.Reg (L.Virtual storedId),None)->tempId=storedId && tempId>1|_->false) (pairs instrs) then Ok () else Error ("Expected an allocated virtual float-store temporary, got "^formatInstructions instrs)
 |Ok _->Error "Expected a single LIR function"
let testMirToLirUsesImmediateMaskForListToRawPtr ()=
 let label=M.Label "entry" in let listReg=M.VReg 0 in let rawPtrReg=M.VReg 1 in
 let block:M.basicBlock={M.label;instrs=[M.ListToRawPtr (rawPtrReg,M.Register listReg)];terminator=M.Ret (M.Register rawPtrReg)} in
 let cfg:M.cfg={M.entry=label;blocks=M.LabelMap.singleton label block} in
 let func:M.functionDef={M.id=TestIds.functionIdForName "list_to_raw_ptr";name="list_to_raw_ptr";typedParams=[{M.reg=listReg;typ=AST.TList AST.TInt64}];returnType=AST.TInternalRawPtr;cfg;floatRegs=M.IntSet.empty} in
 match MIR_to_LIR.toLIR (M.Program ([func],StringOrder.Map.empty,StringOrder.Map.empty)) with
 |Error error->Error ("MIR→LIR failed: "^error)
 |Ok (L.Program ([lirFunc],_,_))->let instrs=instructions lirFunc in if List.mem (L.And_imm (L.Virtual 1,L.Virtual 0,-8L)) instrs then Ok () else Error ("Expected ListToRawPtr to use an immediate tag mask, got: "^formatInstructions instrs)
 |Ok _->Error "Expected a single LIR function"
let tests=["function identities preserve colliding names",testFunctionIdentitiesDoNotHashCollide;"function identities compose across units",testFunctionIdentitiesComposeAcrossUnits;"mir → lir symbolic operands",testMirToLirSymbolicOperands;"mir → lir reports missing entry block",testMirToLirReportsMissingEntryBlock;"mir → lir uses native Int64 shift mask",testMirToLirUsesNativeInt64ShiftMask;"mir → lir allocates float heap-store temporary",testMirToLirAllocatesFloatHeapStoreTemporary;"mir → lir uses immediate list tag mask",testMirToLirUsesImmediateMaskForListToRawPtr]
(* Run all symbolic LIR unit tests *)
let runAll ()=List.fold_left (fun acc (name,test)->match acc with Error _->acc|Ok ()->match test () with Ok ()->Ok ()|Error error->Error ("IRSymbolTests - "^name^" failed: "^error)) (Ok ()) tests
