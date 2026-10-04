(* Full operand materialization, scratch sequences, frames and leak-report emission. *)
[@@@warning "-4"]
open Dark_compiler
module O=InstrumentedX64Operands
module C=X64CodeGenTypes
module F=X64Frames
module J=MachineISAObservation
let tuple values=`Assoc ["tuple",`List values]
let list f xs=`List (List.map f xs)
let attempt f action=try SemanticJson.union "FSharpResult" "Ok" [f (action ())] with Failure msg | Invalid_argument msg -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string msg]
let instructions value=tuple [list J.x64Instr value;X64EncodingObservation.bytes (Bytes.concat Bytes.empty (List.map X86_64_Encoding.encodeInstruction value))]
let sums registry=`Assoc ["map",list (fun (name,(info:MemoryModel.rcSumShapeInfo)) -> tuple [SemanticJson.string name;SemanticJson.record "RcSumShapeInfo" ["TypeParams",list SemanticJson.string info.MemoryModel.typeParams;"Payloads",list (fun (tag,payload) -> tuple [SemanticJson.int32 tag;SemanticJson.union "FSharpOption" (match payload with None -> "None" | Some _ -> "Some") (match payload with None -> [] | Some t -> [SemanticAST.semanticType t])]) info.MemoryModel.payloads;"UnaryPayloadTags",`Assoc ["set",list SemanticJson.int32 (MemoryModel.IntSet.elements info.MemoryModel.unaryPayloadTags)]]]) (StringOrder.Map.bindings registry)]
let observe source =
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let fps=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15] in
 let regs=Array.to_list X64EncodingFixtures.regValues in let fregs=Array.to_list X64EncodingFixtures.fRegValues in
 let result f= function Ok value -> SemanticJson.union "FSharpResult" "Ok" [f value] | Error msg -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string msg] in
 let mappings=tuple [list (fun phys -> tuple [attempt (fun r -> J.x64Instr (X86_64.PUSH r)) (fun () -> O.lirRegToX86 phys);result (fun r -> J.x64Instr (X86_64.PUSH r)) (O.resolveReg (LIR.Physical phys))]) physical;list (fun fp -> let r=O.lirFRegToX86 fp in tuple [J.x64Instr (X86_64.MOVSD_reg (r,r));result (fun f -> J.x64Instr (X86_64.MOVSD_reg (f,f))) (O.resolveFreg (LIR.FPhysical fp))]) fps;list (fun id -> tuple [result (fun r -> J.x64Instr (X86_64.PUSH r)) (O.resolveReg (LIR.Virtual id));result (fun f -> J.x64Instr (X86_64.MOVSD_reg (f,f))) (O.resolveFreg (LIR.FVirtual id))]) [-1;0;1;Int32.to_int Int32.min_int;Int32.to_int Int32.max_int]] in
 let immediates=list (fun dest -> list (fun value -> instructions (O.loadImm64 dest value)) [0L;1L;-1L;-2147483649L;-2147483648L;2147483647L;2147483648L;0x1122334455667788L;Int64.min_int;Int64.max_int]) regs in
 let subsets values count=list (fun mask -> let excluded=List.filteri (fun n _ -> mask land (1 lsl n)<>0) values in
 tuple [attempt (fun r -> J.x64Instr (X86_64.PUSH r)) (fun () -> O.arithmeticTempExcluding excluded)]) (List.init count Fun.id) in
 let arithmetic=subsets regs 65536 in
 let floatScratch=list (fun mask -> let excluded=List.filteri (fun n _ -> mask land (1 lsl n)<>0) fregs in attempt instructions (fun () -> O.withPreservedFloatScratch excluded (fun temp -> [X86_64.XORPD (temp,temp)]))) (List.init 65536 Fun.id) in
 let strings=list (fun reg -> list (fun value -> tuple [instructions (O.emitStringLiteral reg value);instructions (O.emitStringLiteralNoRefCount reg value)]) [source;"";"é";"😀"]) regs in
 let copies=list (fun valueReg -> list (fun destReg -> list (fun length -> let bytes=Bytes.init length (fun n -> Char.chr ((n*73+255) land 255)) in instructions (O.emitStringByteCopy valueReg destReg bytes)) (List.init 34 Fun.id@[63;64;65])) regs) regs in
 let printChars=list (fun length -> let bytes=List.init length (fun n -> Char.chr ((n*73+255) land 255)) in instructions (O.genPrintChars bytes)) (List.init 66 Fun.id) in
 let savedSets=[]::List.map (fun p -> [p]) physical@[[LIR.X19;LIR.X20;LIR.X21];[LIR.X21;LIR.X19;LIR.X20];[LIR.X19;LIR.X19]] in
 let frames=list (fun stack -> list (fun saved -> tuple [attempt instructions (fun () -> F.genPrologue stack saved);attempt instructions (fun () -> F.genEpilogue stack saved)]) savedSets) [-16;-1;0;1;7;8;9;15;16;24;32;512;Int32.to_int Int32.min_int;Int32.to_int Int32.max_int] in
 let variants : LIR.typeVariants={LIR.typeParams=[source];variants=[{LIR.name="c";tag=3;payload=Some AST.TInt64;fieldCount=1};{LIR.name="a";tag=1;payload=Some AST.TFloat64;fieldCount=2};{LIR.name="b";tag=1;payload=None;fieldCount=0};{LIR.name="z";tag=(-1);payload=Some AST.TBool;fieldCount=1}]} in
 let sumRegistries=list (fun registry -> sums (C.rcSumShapeRegistryFromVariantRegistry registry)) [StringOrder.Map.empty;StringOrder.Map.singleton source variants;StringOrder.Map.of_seq (List.to_seq [source,variants;"A",variants])] in
 let ctx : C.funcCtx={C.functionName=source;stackSize=32;usedCalleeSaved=[LIR.X19];enableLeakCheck=false;recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.ofList [AST.functionId 0L,source;AST.functionId (-1L),"largest"]} in
 let offsets=list (fun offset -> list (fun saved -> SemanticJson.int32 (X64InstructionContext.adjustStackOffset {ctx with C.usedCalleeSaved=saved} offset)) savedSets) [-16;-1;0;1;7;8;9;15;16;24;32;512;Int32.to_int Int32.min_int;Int32.to_int Int32.max_int] in
 let names=list (fun id -> attempt SemanticJson.string (fun () -> C.functionName ctx (AST.functionId id))) [0L;1L;Int64.min_int;Int64.max_int;-1L] in
 let leaks=list (fun enabled -> let ctx={ctx with C.enableLeakCheck=enabled} in tuple [instructions (C.genLeakCounterInc ctx);instructions (C.genLeakCounterDec ctx)]) [false;true] in
 let report1=C.genLeakCheckReport () in let report2=C.genLeakCheckReport () in
 let fresh1=X64Operands.freshLabel source in let fresh2=X64Operands.freshLabel "again" in
 let runtime=tuple [instructions O.genWriteSyscall;instructions O.genExitSyscall;instructions (O.genOomJump ());instructions (O.genOomHandler ());instructions (O.genRuntimeErrorHandler ());instructions report1;instructions report2;SemanticJson.string fresh1;SemanticJson.string fresh2] in
 tuple [mappings;immediates;arithmetic;floatScratch;strings;copies;printChars;frames;sumRegistries;names;leaks;runtime;offsets]
