(* Complete x64 calls, floating emission, register aliases and diagnostic results. *)
open Dark_compiler
module F=X64EmitFloatingPoint
module C=X64EmitCalls
module X=X64CodeGenTypes
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let result=function Ok code->SemanticJson.union "FSharpResult" "Ok" [list MachineISAObservation.x64Instr code]|Error error->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let call f=try tuple [`Bool false;result (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let observe source=
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let fps=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15] in
 let regs=List.map (fun reg -> LIR.Physical reg) physical@[LIR.Virtual (-1);LIR.Virtual 0] in
 let fregs=List.map (fun reg -> LIR.FPhysical reg) fps@[LIR.FVirtual (-1);LIR.FVirtual (-2000);LIR.FVirtual 0] in
 let ctx={X.functionName=source;stackSize=32;usedCalleeSaved=[LIR.X19;LIR.X20];enableLeakCheck=false;recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.ofList [AST.functionId 0L,source;AST.functionId 1L,"fn";AST.functionId (-1L),"largest"]} in
 let calls=list (fun stack -> let ctx={ctx with X.stackSize=stack} in
  tuple [list (fun dest -> list (fun func -> tuple [call (fun () -> C.emitIndirectCall ctx dest func []);call (fun () -> C.emitClosureCall ctx dest func []);call (fun () -> C.emitIndirectTailCall ctx func []);call (fun () -> C.emitClosureTailCall ctx func [])]) regs) regs;
   list (fun id -> list (fun dest -> tuple [call (fun () -> C.emitCall ctx dest (AST.functionId id) []);call (fun () -> C.emitTailCall ctx (AST.functionId id) []);call (fun () -> C.emitLoadFuncAddr ctx dest (AST.functionId id))]) regs) [0L;1L;2L;Int64.min_int;-1L]]) [-1;0;32;Int32.to_int Int32.max_int] in
 let saves=list (fun mask -> let ints=List.filteri (fun index _ -> mask land (1 lsl index)<>0) [LIR.X0;LIR.X19;LIR.X20;LIR.SP] in let floats=List.filteri (fun index _ -> mask land (1 lsl (index+4))<>0) [LIR.D0;LIR.D14;LIR.D15] in tuple [call (fun () -> C.emitSaveRegs ctx ints floats);call (fun () -> C.emitRestoreRegs ctx ints floats)]) (List.init 128 Fun.id) in
 let binary=list (fun dest -> list (fun left -> list (fun right -> tuple [call (fun () -> F.emitFAdd ctx dest left right);call (fun () -> F.emitFSub ctx dest left right);call (fun () -> F.emitFMul ctx dest left right);call (fun () -> F.emitFDiv ctx dest left right)]) fregs) fregs) fregs in
 let unary=list (fun dest -> list (fun src -> tuple [call (fun () -> F.emitFMov ctx dest src);call (fun () -> F.emitFNeg ctx dest src);call (fun () -> F.emitFAbs ctx dest src);call (fun () -> F.emitFSqrt ctx dest src);call (fun () -> F.emitFCmp ctx dest src)]) fregs) fregs in
 let floats=[0.;-0.;1.;-1.;1.5;Int64.float_of_bits 1L;Int64.float_of_bits 0x7ff8000000000001L;infinity;neg_infinity;Float.max_float;Float.min_float] in
 let loads=list (fun dest -> list (fun value -> call (fun () -> F.emitFLoad ctx dest value)) floats) fregs in
 let casts=list (fun dest -> list (fun src -> tuple [call (fun () -> F.emitFloatToInt64 ctx dest src);call (fun () -> F.emitFpToGp ctx dest src);call (fun () -> F.emitFloatToBits ctx dest src);call (fun () -> F.emitFloatToString ctx dest src)]) fregs) regs in
 let spills=list (fun saved -> list (fun offset -> list (fun reg -> let ctx={ctx with X.usedCalleeSaved=saved} in tuple [call (fun () -> F.emitFSpillLoad ctx reg offset);call (fun () -> F.emitFSpillStore ctx offset reg)]) fregs) [Int32.to_int Int32.min_int;-32769;-1;0;8;32768;Int32.to_int Int32.max_int]) [[];[LIR.X19];[LIR.X19;LIR.X19;LIR.X20]] in
 let moves=[[];[LIR.D0,LIR.FPhysical LIR.D0];[LIR.D0,LIR.FPhysical LIR.D1;LIR.D1,LIR.FPhysical LIR.D0];[LIR.D0,LIR.FPhysical LIR.D1;LIR.D1,LIR.FPhysical LIR.D2;LIR.D2,LIR.FPhysical LIR.D0];[LIR.D15,LIR.FPhysical LIR.D14;LIR.D14,LIR.FPhysical LIR.D15];[LIR.D0,LIR.FVirtual (-1)];[LIR.D0,LIR.FVirtual (-2000)];[LIR.D0,LIR.FPhysical LIR.D1;LIR.D0,LIR.FPhysical LIR.D2]] in
 tuple [calls;saves;binary;unary;loads;casts;spills;list (fun moves -> call (fun () -> F.emitFArgMoves ctx moves)) moves;call (fun () -> F.emitFPhi ctx)]
