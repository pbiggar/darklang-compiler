(* Full ANF-to-MIR lowering, lexical exits, type/coverage state and ownership observations. *)
[@@@warning "-4"]
open Dark_compiler
module R=ANF_to_MIR
module A=ANF
module M=MIR
module S=SSAANF
module T=R.TempMap
let ( let* ) = Result.bind
let tuple values=`Assoc ["tuple",`List values]
let list fn values=`List (List.map fn values)
let option fn = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [fn value]
let i=SemanticJson.int32
let text=SemanticJson.string
let attempt fn action=try SemanticJson.union "FSharpResult" "Ok" [fn (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [text message]
let result fn = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [fn value] | Error message -> SemanticJson.union "FSharpResult" "Error" [text message]
let tempMap fn values=`Assoc ["map",list (fun (id,value) -> tuple [ProductionANF.aNF_tempId id;fn value]) (T.bindings values)]
let labelMap fn values=`Assoc ["map",list (fun (id,value) -> tuple [ProductionMIR.label id;fn value]) (M.LabelMap.bindings values)]
let state (b:R.cfgBuilder)=tuple [labelMap ProductionMIR.basicBlock b.R.blocks;tempMap (fun (label,typ) -> tuple [ProductionMIR.label label;SemanticAST.semanticType typ]) b.R.joins;tempMap (list (fun (op,label) -> tuple [ProductionMIR.operand op;ProductionMIR.label label])) b.R.joinIncoming;list (fun (label,ops) -> tuple [ProductionMIR.label label;list ProductionMIR.operand ops]) b.R.selfTailIncoming;ProductionMIR.labelGen b.R.labelGen;ProductionMIR.regGen b.R.regGen;i b.R.sourceTempIdMax;tempMap SemanticAST.semanticType b.R.extraTypeMap;`Assoc ["set",list i (M.IntSet.elements b.R.floatRegs)];tempMap ProductionMIR.functionId b.R.closureFuncs;ProductionANF.aNF_exprIdGen b.R.exprIdGen;ProductionANF.aNF_coverageMapping b.R.coverageMapping]
let exit=function R.Terminated -> SemanticJson.union "ExprExit" "Terminated" [] | R.Returned (op,label) -> SemanticJson.union "ExprExit" "Returned" [ProductionMIR.operand op;ProductionMIR.label label]
let id n=A.TempId n
let v n=A.Var (id n)
let fid n=AST.functionId (Int64.of_int n)
let observe source=
 let types=[AST.TUnit;AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TList AST.TFloat64;AST.TTuple [AST.TInt64;AST.TFloat64];AST.TFunction ([AST.TInt64],AST.TFloat64);AST.TSum (source,[AST.TFloat64]);AST.TRecord (source,[])] in
 let atoms=[A.UnitLiteral;A.IntLiteral (A.UInt64 (-1L));A.BoolLiteral false;A.StringLiteral source;A.FloatLiteral (-0.);v 3;A.FuncRef (fid 7)] in
 let typeReg=StringOrder.Map.singleton source ["a",AST.TInt64;"b",AST.TFloat64] in
 let names=FunctionIdMap.ofList [fid 0,"_start";fid 1,source;fid 7,"callee"] in
 let typeMap typ=A.TypeMap.ofSeq (List.to_seq [id 0,typ;id 1,typ;id 2,typ;id 3,typ;id 4,typ;id 5,typ;id 6,typ]) in
 let builder typ coverage : R.cfgBuilder={R.blocks=M.LabelMap.empty;joins=T.empty;joinIncoming=T.empty;selfTailIncoming=[];labelGen=M.LabelGen 0;regGen=M.RegGen 4000;typeById=typeMap typ;sourceTempIdMax=6;extraTypeMap=T.empty;typeReg;returnTypeReg=FunctionIdMap.ofList [fid 1,typ;fid 7,typ];functionNames=names;funcId=fid 1;funcName=source;paramRegs=[M.VReg 0;M.VReg 1];floatRegs=(if typ=AST.TFloat64 then M.IntSet.of_list [0;1;3] else M.IntSet.empty);closureFuncs=T.empty;enableCoverage=coverage;exprIdGen=A.initialExprIdGen;coverageMapping=A.emptyCoverageMapping} in
 let lower b typ expr prefix=attempt (result (fun (ex,after) -> tuple [exit ex;state after])) (fun () -> R.convertExpr typ expr (M.Label (source^"_input")) prefix b) in
 let operations=list (fun typ -> list (fun atom -> list (fun operation ->
  let expr=A.Let (id 6,operation,A.Return (v 6)) in
  tuple [i (R.maxTempIdInCExpr operation);text (R.cexprDescription operation);`Bool (R.cexprProducesFloat (builder typ false).R.floatRegs (builder typ false).R.returnTypeReg operation);list (fun coverage -> lower (builder typ coverage) typ expr [M.Mov (M.VReg 5,M.Int64Const 17L,Some AST.TInt64)]) [false;true]]) (ANFFixtures.operations source atom typ)) atoms) types in
 let flows=list (fun typ ->
 let param : A.typedParam={A.id=id 4;typ} in
 let leaf n=A.Return (v n) in
 let jump n=A.Jump (id 4,v n) in
 let tail args rest=A.Let (id 5,A.TailCall (fid 1,args),rest) in
 let dec kind rest=A.Let (id 6,A.RefCountDec (v 0,16,kind,None),rest) in
 let flows=[A.If (v 3,leaf 0,leaf 1);A.If (v 3,A.If (v 3,leaf 0,leaf 1),leaf 3);A.Join (param,leaf 4,A.If (v 3,jump 0,jump 1));
  A.Join (param,A.Join ({A.id=id 5;typ},A.Return (v 5),A.Jump (id 5,v 4)),A.If (v 3,jump 0,jump 1));A.Join (param,leaf 4,leaf 0);A.Jump (id 99,v 0);
  tail [v 1;v 0] (A.Return (v 5));tail [A.FloatLiteral (-0.);A.IntLiteral (A.Int64 7L)] (A.Return (v 5));
  dec MemoryModel.GenericHeap (tail [v 0;v 0] (dec MemoryModel.GenericHeap (A.Return (v 5))));
  A.If (v 3,tail [v 1;v 0] (A.Return (v 5)),leaf 0);A.If (v 3,tail [v 1;v 0] (A.Return (v 5)),tail [v 0;v 1] (A.Return (v 5)));
  tail [v 0;v 1] (A.Let (id 6,A.RefCountDecString (v 0),A.Let (id 6,A.RefCountDecBlob (v 1),A.Let (id 6,A.RefCountDecInt (v 3),A.Return (v 5)))));
  tail [v 0;v 1] (A.Let (id 6,A.RefCountDec (A.UnitLiteral,16,MemoryModel.GenericHeap,None),A.Return (v 5)));tail [v 0;v 1] (A.Return A.UnitLiteral)] in
 list (fun expr -> tuple [i (R.maxTempIdInAExpr expr);list (fun coverage -> lower (builder typ coverage) typ expr []) [false;true];list (fun enabled ->
 let func : A.functionDef={A.id=fid 1;name=source;typedParams=[{A.id=id 0;typ};{A.id=id 1;typ}];returnType=typ;returnOwnership=A.OwnedReturn;body=expr} in
 attempt (result ProductionMIR.functionDef) (fun () -> let ssa=SSAANF.convertFunction (R.maxTempIdInFunction func) (typeMap typ) func in Result.bind ssa (fun ssa -> R.convertSSAANFFunction (if enabled then SSATailCallDetection.detect FunctionIdMap.empty ssa else ssa) (typeMap typ) typeReg (builder typ true).R.returnTypeReg names true))) [false;true]]) flows) types in
 let helpers=list (fun typ -> let b=builder typ true in
 let missing={b with R.typeById=A.TypeMap.empty} in
 let extra={missing with R.extraTypeMap=T.of_list [id 3,typ;id 99,AST.TFloat64];floatRegs=M.IntSet.empty} in
 list (fun b -> tuple [list (fun atom -> attempt SemanticAST.semanticType (fun () -> R.atomType b atom)) (atoms@[v (-1);v 99]);list (fun operand -> attempt SemanticAST.semanticType (fun () -> R.operandType b operand)) [M.Int64Const 0L;M.BoolConst true;M.FloatSymbol (Int64.float_of_bits 0xfff8000000000000L);M.StringSymbol source;M.FuncAddr (fid 7);M.Register (M.VReg 3);M.Register (M.VReg 99)];list (fun atom -> result ProductionMIR.operand (R.atomToOperand b atom)) atoms]) [b;missing;extra]) types in
 let ownership=list (fun aliasMode ->
 let aliases=match aliasMode with 0 -> [] | 1 -> [M.Mov (M.VReg 2,M.Register (M.VReg 0),None)] | 2 -> [M.Mov (M.VReg 3,M.Register (M.VReg 2),None);M.Mov (M.VReg 2,M.Register (M.VReg 0),None)] | _ -> [M.Mov (M.VReg 2,M.Register (M.VReg 3),None);M.Mov (M.VReg 3,M.Register (M.VReg 2),None)] in
 list (fun mask -> let cleanup=List.filter_map (fun n -> if mask land (1 lsl n)=0 then None else Some (M.RefCountDec (M.VReg n,16,M.GenericHeap,None))) [0;1;2;3] @ [M.RefCountDecString (M.Register (M.VReg 0));M.RefCountDecBlob (M.Register (M.VReg 1));M.RefCountDecInt (M.Register (M.VReg 2))] in
 list (fun argCode -> let args=List.init 4 (fun n -> match (argCode lsr (n*2)) land 3 with 0 -> M.Int64Const 0L | k -> M.Register (M.VReg k)) in
 let incs,decs=R.transferOverlappingArgOwnership args cleanup aliases in tuple [list ProductionMIR.instr incs;list ProductionMIR.instr decs;let before,rest=R.collectPreSelfTailCallCleanup (List.rev cleanup@aliases) in tuple [list ProductionMIR.instr before;list ProductionMIR.instr rest]]) (List.init 256 Fun.id)) (List.init 16 Fun.id)) [0;1;2;3] in
 let registryInputs=[[];["Some",("Option",["a"],1,[AST.TVar "a"]);"None",("Option",["a"],0,[])];
  ["X.Some",("X",[],3,[AST.TInt64]);"Some",("X",["wrong"],9,[]);"Y.Some",("Y",[],2,[AST.TString;AST.TFloat64]);"Other",("Z",[],0,[])];
  ["A.One",("A",["a"],0,[]);"A.Two",("A",["b"],1,[])];["One",("A",["a"],0,[]);"Two",("A",[],1,[])];
  [source^".V",(source,[],Int32.to_int Int32.min_int,[AST.TInt64]);source^".W",(source,[],Int32.to_int Int32.max_int,[AST.TInt64;AST.TString])]] in
 let registries=list (fun entries -> attempt ProductionMIR.variantRegistry (fun () -> R.buildVariantRegistry (StringOrder.Map.of_list entries))) registryInputs in
 let program typ expr : A.functionDef={A.id=fid 1;name=source;typedParams=[{A.id=id 0;typ};{A.id=id 1;typ}];returnType=typ;returnOwnership=A.OwnedReturn;body=expr} in
 let programEncoder (functions,variants,records)=tuple [list ProductionMIR.functionDef functions;ProductionMIR.variantRegistry variants;ProductionMIR.recordRegistry records] in
 let programs=list (fun typ -> list (fun shape ->
 let body=match shape with 0 -> A.Return (v 0) | 1 -> A.If (A.BoolLiteral true,A.Return (A.FloatLiteral (-0.)),A.Return (v 0)) | 2 -> A.Let (id 5,A.Call (fid 7,[v 0;v 1]),A.Return (v 5)) | _ -> A.Let (id 5,A.TailCall (fid 1,[v 1;v 0]),A.Return (v 5)) in
 let funcs=[program typ body] in let prog=A.Program (funcs,A.Return (A.IntLiteral (A.Int64 9L))) in
 let externalTypes=FunctionIdMap.ofList [fid 7,("callee",typ);fid 1,(source,AST.TString)] in
 let returns=R.buildReturnTypeReg funcs externalTypes in
 let variants=StringOrder.Map.of_list (List.nth registryInputs 1) in
 list (fun coverage ->
 let whole=attempt (result ProductionMIR.program) (fun () -> R.toMIR prog (typeMap typ) typeReg AST.TInt64 variants typeReg coverage externalTypes names) in
 let only=attempt (result programEncoder) (fun () -> R.toMIRFunctionsOnly prog (typeMap typ) typeReg variants typeReg coverage externalTypes names) in
 let traced=list (fun projected -> list (fun enabled ->
 let phases=ref [] in let recorder name duration=phases:= !phases@[tuple [text name;`Bool (Float.is_finite duration && duration>=0.)]] in
 let projection=if projected then Some (StringOrder.Map.empty,StringOrder.Map.empty) else None in
 let allocationResult=attempt (result programEncoder) (fun () -> R.toMIRFunctionsOnlyWithTrace (Some recorder) projection FunctionIdMap.empty enabled prog (typeMap typ) typeReg variants typeReg coverage returns names) in
 let directPhases=ref [] in let directRecorder name duration=directPhases:= !directPhases@[tuple [text name;`Bool (duration=0.)]] in
 let direct=attempt (result programEncoder) (fun () -> let* ssa=ResultList.mapResults (fun func -> S.convertFunction (R.maxTempIdInFunction func) (typeMap typ) func) funcs in R.toMIRSSAFunctionsOnlyWithTrace (Some directRecorder) projection FunctionIdMap.empty enabled ssa (typeMap typ) typeReg variants typeReg coverage returns names) in
 tuple [allocationResult;`List !phases;direct;`List !directPhases]) [false;true]) [false;true] in
 tuple [whole;only;traced]) [false;true]) [0;1;2;3]) types in
 let ssaGraphs=list (fun typ -> list (fun style -> list (fun entry ->
 let label n=S.Label n in let param n : A.typedParam={A.id=id n;typ} in
 let block n parameters operations terminator : S.block={S.label=label n;parameters;operations;terminator} in
 let blocks=match style with
 | 0 -> [label 0,block 0 [] [] (S.Return (v 0))]
 | 1 -> [label 0,block 0 [] [] (S.Branch (A.BoolLiteral true,label 1,label 2));label 1,block 1 [] [] (S.Jump (label 3,[A.FloatLiteral (-0.);v 0]));label 2,block 2 [] [] (S.Jump (label 3,[A.FloatLiteral (Int64.float_of_bits 0xfff8000000000000L);v 1]));label 3,block 3 [param 4;param 5] [] (S.Return (v 4))]
 | 2 -> [label 0,block 0 [] [] (S.Jump (label 1,[v 0]));label 1,block 1 [param 4] [id 5,A.Atom (v 4)] (S.Branch (A.BoolLiteral true,label 2,label 3));label 2,block 2 [] [] (S.Jump (label 1,[v 5]));label 3,block 3 [] [] (S.Return (v 4))]
 | 3 -> [label 0,block 0 [] [] (S.Jump (label 1,[]));label 1,block 1 [param 4] [] (S.Return (v 4))]
 | 4 -> [label 0,block 0 [] [] (S.Jump (label 99,[]))]
 | 5 -> [label 0,block 0 [] [id 6,A.RefCountInc (v 0,16,MemoryModel.GenericHeap,None);id 6,A.RefCountIncString (v 0);id 6,A.RefCountIncBlob (v 1);id 6,A.RefCountIncInt (v 0);id 5,A.TailCall (fid 1,[v 1;v 0])] (S.Return (v 5))]
 | _ -> [] in
 let func : S.functionDef={S.id=fid 1;name=source;typedParams=[param 0;param 1];returnType=typ;returnOwnership=A.OwnedReturn;entry=label entry;blocks=S.LabelMap.of_list blocks;freshValueTypes=T.of_list [id 5,typ;id 6,AST.TUnit]} in
 list (fun coverage -> attempt (result ProductionMIR.functionDef) (fun () -> R.convertSSAANFFunction func (typeMap typ) typeReg (builder typ false).R.returnTypeReg names coverage)) [false;true]) [0;1]) [0;1;2;3;4;5;6]) [AST.TInt64;AST.TFloat64;AST.TString] in
 let intrinsics=list (fun name -> attempt (option SemanticAST.semanticType) (fun () -> R.tryGetIntrinsicReturnType name)) ["Builtin.pmFindValuesByValueType";"Builtin.pmGetLocationsByValue";"__raw_get_str";"__raw_take_str";"__stream_to_rawptr_x";"__raw_slot_init_x";"__hash_x";"__key_eq_x";"__empty_dict_x";"__dict_is_null_x";"__dict_get_tag_x";"__dict_to_rawptr_x";"__rawptr_to_dict_x";"__list_is_null_x";"__list_get_tag_x";"__list_to_rawptr_x";"__rawptr_to_list_x";source] in
 let binOps=[A.Add;A.Sub;A.Mul;A.Div;A.Mod;A.Shl;A.Shr;A.BitAnd;A.BitOr;A.BitXor;A.Eq;A.Neq;A.Lt;A.Gt;A.Lte;A.Gte;A.And;A.Or] in
 let unaryOps=[A.Neg;A.Not;A.BitNot] in
 let cliOps=[A.Execute;A.RunProcess;A.HostOS;A.HostArchitecture;A.Hostname;A.GetEnv;A.GetEnvironmentPacked;A.SetEnv;A.UnsetEnv;A.DirectoryCurrent;A.DirectoryListPacked;A.FileIsDirectory;A.FileCreateExclusive;A.GetArgv;A.Kill;A.GetPid;A.GetUid;A.CpuCount;A.SpawnProcess;A.ProcessIO;A.TerminateProcess;A.SocketTcp4;A.SocketTcp6;A.SocketUdp4;A.SocketUdp6;A.SocketConnect4;A.SocketConnect6;A.SocketSend;A.SocketReceive;A.SocketReceiveTimeout;A.SocketSendTimeout;A.SocketClose;A.SecureRandomFill] in
 let operatorCases=list (fun typ -> list (fun atom ->
 let operations=List.map (fun op -> A.Prim (op,atom,v 0)) binOps@List.map (fun op -> A.Prim (op,v 0,atom)) binOps@List.map (fun op -> A.UnaryPrim (op,atom)) unaryOps@List.map (fun op -> A.CliNative (op,[atom;v 0])) cliOps in
 list (fun op -> tuple [text (R.cexprDescription op);i (R.maxTempIdInCExpr op);`Bool (R.cexprProducesFloat (builder typ false).R.floatRegs (builder typ false).R.returnTypeReg op);lower (builder typ true) typ (A.Let (id 6,op,A.Return (v 6))) []]) operations) atoms) types in
 tuple [operations;flows;helpers;ownership;registries;programs;ssaGraphs;intrinsics;operatorCases;list ProductionMIR.binOp (List.map R.convertBinOp binOps);list ProductionMIR.unaryOp (List.map R.convertUnaryOp unaryOps);list ProductionMIR.cliOperation (List.map R.convertCliOperation cliOps)]

