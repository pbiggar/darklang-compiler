(* Complete instruction selection, register banks, modulo guards and CFG observations. *)
[@@@warning "-4"]
open Dark_compiler
module R=MIR_to_LIR
module M=MIR
module L=LIR
let tuple values=`Assoc ["tuple",`List values]
let list fn values=`List (List.map fn values)
let i=SemanticJson.int32
let text=SemanticJson.string
let result fn=function Ok value -> SemanticJson.union "FSharpResult" "Ok" [fn value] | Error message -> SemanticJson.union "FSharpResult" "Error" [text message]
let attempt fn action=try SemanticJson.union "FSharpResult" "Ok" [fn (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [text message]
let state (s:R.tempState)=SemanticJson.record "TempState" ["NextRegId",i s.R.nextRegId;"NextFRegId",i s.R.nextFRegId]
let selected (instrs,s)=tuple [list ProductionLIR.instr instrs;state s]
let regs (instrs,reg,s)=tuple [list ProductionLIR.instr instrs;ProductionLIR.reg reg;state s]
let fregs (instrs,reg,s)=tuple [list ProductionLIR.instr instrs;ProductionLIR.fReg reg;state s]
let reg n=M.VReg n
let v n=M.Register (reg n)
let fid n=AST.functionId (Int64.of_int n)
let block label instrs terminator : M.basicBlock={M.label=M.Label label;instrs;terminator}
let graph entry blocks : M.cfg={M.entry=M.Label entry;blocks=M.LabelMap.of_list (List.map (fun (b:M.basicBlock) -> b.M.label,b) blocks)}
let observe source=
 let types=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TChar;AST.TBlob;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TTuple [AST.TInt64;AST.TFloat64;AST.TString];AST.TList AST.TInt64;AST.TDict (AST.TInt64,AST.TString);AST.TRecord (source,[]);AST.TSum (source,[]);AST.TVar "a";AST.TInferenceVar ("1","a");AST.TDateTime;AST.TFunction ([AST.TInt64],AST.TFloat64);AST.TStream AST.TString] in
 let operands=[v 2;v 3;M.Int64Const 0L;M.Int64Const (-1L);M.Int64Const Int64.max_int;M.BoolConst false;M.FloatSymbol (-0.);M.FloatSymbol (Int64.float_of_bits 0x7ff8000000000001L);M.StringSymbol source;M.FuncAddr (fid 200)] in
 let variants=StringOrder.Map.singleton source ({M.typeParams=[];variants=[{M.name="Only";tag=0;payload=Some AST.TInt64;fieldCount=1}]} : M.typeVariants) in
 let records=StringOrder.Map.singleton source [{M.name="first";typ=AST.TInt64};{M.name="second";typ=AST.TFloat64}] in
 let sums=StringOrder.Map.singleton source ({MemoryModel.typeParams=[];payloads=[0,Some AST.TInt64];unaryPayloadTags=MemoryModel.IntSet.singleton 0} : MemoryModel.rcSumShapeInfo) in
 let ctx : R.printRcContext={R.recordFields=StringOrder.Map.singleton source ["first",AST.TInt64;"second",AST.TFloat64];recordTypeParams=StringOrder.Map.singleton source [];sumShapes=sums} in
 let initial : R.tempState={R.nextRegId=4000;nextFRegId=5000} in
 let architectures=[Platform.ARM64;Platform.X86_64] in
 let floats=[M.IntSet.empty;M.IntSet.of_list [1;2;3]] in
 let instructionCases=list (fun typ -> list (fun operand -> list (fun instr -> tuple [i (R.maxVRegIdFromInstr instr (-1));list (fun arch -> list (fun floatRegs -> attempt (result selected) (fun () -> R.selectInstr arch instr variants records ctx floatRegs initial)) floats) architectures]) (MIRFixtures.instructions source typ operand)) operands) types in
 let binOps=[M.Add;M.Sub;M.Mul;M.Div;M.Mod;M.Shl;M.Shr;M.BitAnd;M.BitOr;M.BitXor;M.Eq;M.Neq;M.Lt;M.Gt;M.Lte;M.Gte;M.And;M.Or] in
 let arithmeticOperands=[v 2;M.Int64Const Int64.min_int;M.Int64Const 0L;M.Int64Const 7L;M.Int64Const 64L;M.Int64Const Int64.max_int;M.BoolConst true;M.FloatSymbol (-0.);M.StringSymbol source] in
 let arithmetic=list (fun typ -> list (fun op -> list (fun left -> list (fun right -> list (fun arch -> attempt (result selected) (fun () -> R.selectInstr arch (M.BinOp (reg 1,op,left,right,typ)) variants records ctx M.IntSet.empty initial)) architectures) arithmeticOperands) arithmeticOperands) binOps) [AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TFloat64;AST.TBool;AST.TBlob] in
 let helperCases=list (fun s ->
 let gp,gpNext=R.freshTempReg s in let fp,fpNext=R.freshTempFReg s in
 tuple [list (fun operand -> tuple [ProductionLIR.operand (R.convertOperand operand);result regs (R.ensureInRegister operand s);result regs (R.ensureBlobInRegister operand s);result fregs (R.ensureInFRegister operand s)]) operands;tuple [ProductionLIR.reg gp;state gpNext];tuple [ProductionLIR.fReg fp;state fpNext]]) [initial;{R.nextRegId=Int32.to_int Int32.max_int;nextFRegId=Int32.to_int Int32.max_int};{R.nextRegId=(-1);nextFRegId=(-2)}] in
 let terminators=list (fun typ -> list (fun operand -> list (fun term -> tuple [i (R.maxVRegIdFromTerminator term (-1));result (fun (instrs,term,s) -> tuple [list ProductionLIR.instr instrs;ProductionLIR.terminator term;state s]) (R.selectTerminator term typ initial)]) [M.Ret operand;M.Branch (operand,M.Label "yes",M.Label "no");M.Jump (M.Label source)]) operands) types in
 let typeHelpers=list (fun typ -> tuple [list ProductionLIR.instr (R.truncateForType (L.Virtual 1) typ);`Bool (R.shouldCheckNegativeDivisor typ);`Bool (R.isUnsignedIntegerType typ);`Assoc ["kind",`String "int64";"value",`String (Int64.to_string (R.shiftCountMask typ))];`Bool (R.usesNativeVariableShiftMask typ);list (fun op -> attempt ProductionLIR.condition (fun () -> R.comparisonCondition typ op)) binOps;list (fun (params,args) -> attempt SemanticAST.semanticType (fun () -> R.applyTypeSubst params args typ)) [[],[];["a"],[AST.TFloat64];["a";"a"],[AST.TInt64;AST.TString];["a"],[]]]) (types@[AST.TFunction ([AST.TVar "a"],AST.TList (AST.TVar "a"));AST.TRecord (source,[AST.TVar "a"]);AST.TSum (source,[AST.TVar "a"])]) in
 let errors : R.integerErrorLabels={R.divideByZero=L.Label "division-error";moduloByZero=L.Label "modulo-zero";moduloNegativeDivisor=L.Label "modulo-negative"} in
 let moduloGraphs typ=
 let operation op left right=M.BinOp (reg 1,op,left,right,typ) in
 let b instrs term=block source instrs term in
 [graph source [b [] (M.Ret (v 2))];graph source [b [operation M.Div (v 2) (M.Int64Const 0L)] (M.Ret (v 1))];
 graph source [b [operation M.Div (M.Int64Const 11L) (v 3)] (M.Ret (v 1))];graph source [b [operation M.Mod (v 2) (M.Int64Const (-3L))] (M.Ret (v 1))];
 graph source [b [operation M.Mod (v 2) (M.Int64Const Int64.max_int)] (M.Ret (v 1))];graph source [b [operation M.Div (v 2) (v 3);operation M.Mod (v 1) (v 2);operation M.Mod (v 3) (M.Int64Const 0L)] (M.Ret (v 1))];
 graph source [b [M.RuntimeError source;M.Print (v 2,AST.TInt128)] (M.Ret (v 2))];graph source [b [] (M.Branch (v 2,M.Label "left",M.Label "right"));block "left" [operation M.Div (v 2) (v 3)] (M.Jump (M.Label "join"));block "right" [operation M.Mod (v 2) (v 3)] (M.Jump (M.Label "join"));block "join" [M.Phi (reg 4,[v 1,M.Label "left";v 1,M.Label "right"],Some typ)] (M.Ret (v 4))];
 graph source [b [] (M.Jump (M.Label "division-error"));block "division-error" [] (M.Ret (v 2))];graph source [];graph source [b [M.Phi (reg 1,[M.FloatSymbol (-0.),M.Label source],Some AST.TFloat64)] (M.Ret (v 1))]] in
 let cfgCases=list (fun typ -> list (fun cfg -> list (fun arch -> tuple [attempt (result ProductionLIR.cfg) (fun () -> R.selectCFG arch source cfg variants records ctx typ M.IntSet.empty errors initial);list (fun (_,b) -> attempt (result (fun (blocks,final,s) -> tuple [list ProductionLIR.basicBlock blocks;ProductionLIR.label final;state s])) (fun () -> R.selectBlocksWithModuloChecks arch source b variants records ctx typ M.IntSet.empty errors initial)) (M.LabelMap.bindings cfg.M.blocks)]) architectures) (moduloGraphs typ)) [AST.TInt8;AST.TInt64;AST.TUInt64;AST.TFloat64;AST.TBool] in
 let make parameters typ cfg : M.functionDef={M.id=fid 1;name=source;typedParams=parameters;returnType=typ;cfg;floatRegs=(if typ=AST.TFloat64 then M.IntSet.of_list [1;2;3;4] else M.IntSet.empty)} in
 let parameter n typ : M.typedMIRParam={M.reg=reg n;typ} in
 let params=[[];List.init 9 (fun n -> parameter n AST.TInt64);List.init 9 (fun n -> parameter n AST.TFloat64);List.init 16 (fun n -> parameter n (if n<8 then AST.TInt64 else AST.TFloat64))] in
 let observeProgram arch func=
 let phases=ref [] in let recorder name duration=phases:= !phases@[tuple [text name;`Bool (Float.is_finite duration && duration>=0.)]] in
 let program=M.Program ([func],variants,records) in
 let full=attempt (result ProductionLIR.program) (fun () -> R.toLIRForWithTrace (Some recorder) arch program) in
 let onlyPhases=ref [] in let onlyRecord name duration=onlyPhases:= !onlyPhases@[tuple [text name;`Bool (Float.is_finite duration && duration>=0.)]] in
 let only=attempt (result (list ProductionLIR.functionDef)) (fun () -> R.toLIRFunctionsForWithTrace (Some onlyRecord) arch program) in
 let rcPhases=ref [] in let rcRecord name duration=rcPhases:= !rcPhases@[tuple [text name;`Bool (Float.is_finite duration && duration>=0.)]] in
 let rc=attempt (result (list ProductionLIR.functionDef)) (fun () -> R.toLIRFunctionsForWithTraceAndRcRegistries (Some rcRecord) arch ctx.R.recordFields ctx.R.recordTypeParams ctx.R.sumShapes program) in
 tuple [state (R.initTempState func);full;`List !phases;only;`List !onlyPhases;rc;`List !rcPhases;attempt (result ProductionLIR.program) (fun () -> R.toLIRFor arch program);attempt (result ProductionLIR.program) (fun () -> R.toLIR program)] in
 let programCases=list (fun typ -> list (fun cfg -> list (fun parameters -> list (fun arch -> observeProgram arch (make parameters typ cfg)) architectures) params) (moduloGraphs typ)) [AST.TInt64;AST.TUInt64;AST.TFloat64] in
 let callCases=list (fun intCount -> list (fun floatCount ->
 let argTypes=List.init intCount (fun _ -> AST.TInt64)@List.init floatCount (fun _ -> AST.TFloat64) in
 let args=List.mapi (fun n typ -> if typ=AST.TFloat64 then M.FloatSymbol (-0.) else M.Register (reg n)) argTypes in
 let calls=[M.Call (reg 1,fid 200,args,argTypes,AST.TFloat64);M.TailCall (fid 200,args,argTypes,AST.TFloat64);M.IndirectCall (reg 1,v 2,args,argTypes,AST.TFloat64);M.IndirectTailCall (v 2,args,argTypes,AST.TFloat64);M.ClosureCall (reg 1,v 2,args,argTypes,AST.TFloat64);M.ClosureTailCall (v 2,args,argTypes)] in
 list (fun instr -> list (fun arch -> observeProgram arch (make [] AST.TFloat64 (graph source [block source [instr] (M.Ret (v 1))]))) architectures) calls) [0;8;9]) [0;7;8;9] in
 let floatArgCases=list (fun count -> list (fun available -> list (fun operand -> result selected (R.buildFloatArgMoves (List.init count (fun _ -> operand)) (List.init available (fun n -> List.nth [L.D0;L.D1;L.D2;L.D3;L.D4;L.D5;L.D6;L.D7] (n mod 8))) initial)) operands) [0;1;7;8;9]) [0;1;7;8;9] in
 let printCases=list (fun typ -> list (fun operand -> attempt (result selected) (fun () -> R.selectInstr Platform.ARM64 (M.Print (operand,typ)) variants records ctx M.IntSet.empty initial)) operands) [AST.TTuple [];AST.TTuple [AST.TUInt64;AST.TBool;AST.TChar;AST.TInt128;AST.TUInt128];AST.TTuple [AST.TList AST.TInt64];AST.TTuple [AST.TUnit];AST.TList AST.TInt128;AST.TList AST.TUInt128;AST.TSum ("missing",[]);AST.TRecord ("missing",[]);AST.TRecord (source,[AST.TInt64]);AST.TSum (source,[AST.TInt64])] in
 tuple [instructionCases;arithmetic;helperCases;terminators;typeHelpers;cfgCases;programCases;callCases;floatArgCases;printCases]
