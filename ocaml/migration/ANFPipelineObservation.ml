[@@@warning "-4"]
(* Compare complete SSA pipeline outputs, type lookups and pass ordering. *)
open Dark_compiler
module A=ANF
module P=ANFPipeline
module M=StringOrder.Map
module F=SpecializationIdentity.FunctionSet
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let option f=function None->SemanticJson.union "FSharpOption" "None" []|Some value->SemanticJson.union "FSharpOption" "Some" [f value]
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let attempt f action=result f (try action () with Failure message|Invalid_argument message->Error message)
let observe input=
 let open Yojson.Basic.Util in let request=Yojson.Basic.from_string input in let source=request |> member "text" |> to_string in let bucket=request |> member "bucket" |> to_int in
 let caseIndex=ref 0 in let select action=let index= !caseIndex in incr caseIndex;if index mod 64=bucket then [tuple [SemanticJson.int32 index;action ()]] else [] in
 let fid n=AST.functionId (Int64.of_int n) in let id n=A.TempId n in let var n=A.Var (id n) in let int n=A.IntLiteral (A.Int64 n) in
 let func n name params returnType body={A.id=fid n;name;typedParams=List.map (fun (n,typ)->{A.id=id n;typ}) params;returnType;returnOwnership=A.OwnedReturn;body} in
 let helper=func 200 "external" [30,AST.TInt64] AST.TInt64 (A.Let (id 31,A.Call (fid 300,[var 30]),A.Return (var 31))) in
 let deeper=func 300 "deeper" [40,AST.TInt64] AST.TInt64 (A.Let (id 41,A.Prim (A.Add,var 40,int 1L),A.Return (var 41))) in
 let bad=func 400 "unreachable_bad" [] AST.TInt64 (A.Return (var 99)) in
 let empty=AST_to_ANF.buildRegistries (CheckedAST.emptySymbols ()) M.empty [] M.empty [] in
 let config value=SemanticJson.record "InliningConfig" ["MaxFunctionSize",SemanticJson.int32 value.InliningCommon.maxFunctionSize;"MaxInlineDepth",SemanticJson.int32 value.InliningCommon.maxInlineDepth;"MaxExternalInlineSites",SemanticJson.int32 value.InliningCommon.maxExternalInlineSites;"MaxBoundedLoopIterations",SemanticJson.int32 value.InliningCommon.maxBoundedLoopIterations;"MaxBoundedLoopExpansion",SemanticJson.int32 value.InliningCommon.maxBoundedLoopExpansion;"MaxProjectedTupleInlineSize",SemanticJson.int32 value.InliningCommon.maxProjectedTupleInlineSize;"MaxProjectedTupleInlineSites",SemanticJson.int32 value.InliningCommon.maxProjectedTupleInlineSites] in
 let options=[CompilerOptions.defaultOptions;{CompilerOptions.defaultOptions with CompilerOptions.disableANFOpt=true};{CompilerOptions.defaultOptions with CompilerOptions.disableInlining=true};{CompilerOptions.defaultOptions with CompilerOptions.disableANFOpt=true;disableInlining=true};{CompilerOptions.defaultOptions with CompilerOptions.disableANFConstFolding=true};{CompilerOptions.defaultOptions with CompilerOptions.disableANFConstProp=true};{CompilerOptions.defaultOptions with CompilerOptions.disableANFCopyProp=true};{CompilerOptions.defaultOptions with CompilerOptions.disableANFDCE=true};{CompilerOptions.defaultOptions with CompilerOptions.disableTCO=true};{CompilerOptions.defaultOptions with CompilerOptions.disableANFStrengthReduction=true};{CompilerOptions.defaultOptions with CompilerOptions.enableCoverage=true}] in
 let run functions externals config options specialize excluded contracts=
  let all=functions @ externals in let signatures=FunctionIdMap.ofList (List.map (fun (func:A.functionDef)->func.A.id,(func.A.name,AST.TFunction (List.map (fun (param:A.typedParam)->param.A.typ) func.A.typedParams,func.A.returnType))) all) in
  let names=FunctionIdMap.map (fun _ (name,_)->name) signatures in
  let registries={empty with AST_to_ANF.funcReg=signatures;functionNames=names;functionIds=TypeRegistries.functionIdsFromNames names;recordFieldsReg=M.singleton "R" [source,AST.TString];recordTypeParamsReg=M.singleton "R" []} in
  let program=A.Program (functions,A.Return A.UnitLiteral) in let converted=P.buildConversionResult program registries contracts in
  let conversion=tuple [ProductionANF.aNF_program converted.AST_to_ANF.program;list (fun (key,(name,typ))->tuple [ProductionMIR.functionId key;str name;SemanticAST.semanticType typ]) (FunctionIdMap.toList converted.AST_to_ANF.funcReg);`Bool (converted.AST_to_ANF.ownershipContracts=contracts);`Bool (converted.AST_to_ANF.recursiveMembers=registries.AST_to_ANF.recursiveMembers);`Bool (converted.AST_to_ANF.typeReg=registries.AST_to_ANF.typeReg);`Bool (converted.AST_to_ANF.recordFieldsReg=registries.AST_to_ANF.recordFieldsReg);`Bool (converted.AST_to_ANF.recordTypeParamsReg=registries.AST_to_ANF.recordTypeParamsReg);`Bool (converted.AST_to_ANF.variantLookup=registries.AST_to_ANF.variantLookup);`Bool (converted.AST_to_ANF.rcSumShapeReg=registries.AST_to_ANF.rcSumShapeReg);`Bool (converted.AST_to_ANF.funcParams=registries.AST_to_ANF.funcParams);`Bool (converted.AST_to_ANF.moduleRegistry=registries.AST_to_ANF.moduleRegistry)] in
  let phases=ref [] in let record (value:CompilerOptions.passTiming)=phases:=tuple [str value.CompilerOptions.pass;`Bool (value.CompilerOptions.elapsed>=0L)]:: !phases in
  let clock=ref 0. in let elapsed ()=clock:= !clock+.0.125;!clock in
  let output=attempt (fun (pre,ssa,typeMap)->let highest=List.fold_left (fun highest (func:SSAANF.functionDef)->RcTypeFacts.TempMap.fold (fun (A.TempId n) _ highest->max n highest) func.SSAANF.freshValueTypes highest) 0 ssa in
   let lookups=List.init (highest+7) (fun n->n-3) @ [Int32.to_int Int32.min_int;Int32.to_int Int32.max_int] in
   tuple [list ProductionANF.aNF_functionDef pre;list RcObservation.ssaFunction ssa;list (fun n->tuple [SemanticJson.int32 n;option SemanticAST.semanticType (A.TypeMap.tryFind (id n) typeMap)]) lookups]) (fun ()->P.buildAnf 0 options elapsed registries 1000L config (InliningCommon.buildExternalCandidateInfoMap config externals) M.empty excluded functions contracts specialize (Some record)) in
  tuple [conversion;output;list Fun.id (List.rev !phases)] in
 let cases typ=let make=A.Call (fid 500,[]) in
  let bodies=[A.Return (var 10);A.Let (id 20,A.TypedAtom (var 10,typ),A.Return (var 20));A.Let (id 20,make,A.Return (var 20));A.Let (id 20,make,A.Return (var 10));A.If (var 11,A.Return (var 10),A.Return (var 10));A.Join ({A.id=id 30;typ},A.Return (var 30),A.Jump (id 30,var 10))] in
  List.map (func 100 ("caller_"^source) [10,typ;11,AST.TBool] typ) bodies in
 let observed=list (fun typ->let body=match typ with AST.TInt64->A.Return (int 42L)|AST.TString->A.Return (A.StringLiteral source)|AST.TTuple _->A.Let (id 900,A.TupleAlloc [A.StringLiteral source;int 42L],A.Return (var 900))|AST.TList _->A.Let (id 900,A.RawPtrToList (int 0L,int 0L,typ),A.Return (var 900))|AST.TDict _->A.Let (id 900,A.RawPtrToDict (int 0L,int 0L,typ),A.Return (var 900))|_->assert false in let make=func 500 "make" [] typ body in list (fun main->list (fun options->list (fun specialize->list Fun.id (select (fun ()->run [main] [make] P.stdlibInliningConfig options specialize F.empty FunctionIdMap.empty))) [false;true]) options) (cases typ)) [AST.TInt64;AST.TString;AST.TList AST.TString;AST.TTuple [AST.TString;AST.TInt64];AST.TDict (AST.TString,AST.TString)] in
 let caller=func 100 "caller" [10,AST.TInt64] AST.TInt64 (A.Let (id 20,A.Call (fid 200,[var 10]),A.Return (var 20))) in
 let externalCases=list (fun hasExternal->list (fun excluded->list (fun config->list (fun options->list (fun specialize->list Fun.id (select (fun ()->run (if hasExternal then [caller] else [helper;deeper;caller]) (if hasExternal then [helper;deeper;bad] else []) config options specialize (if excluded then F.singleton (fid 200) else F.empty) FunctionIdMap.empty))) [false;true]) options) [P.stdlibInliningConfig;InliningCommon.defaultConfig]) [false;true]) [false;true] in
 let ownershipCases=list (fun parameter->let main=List.hd (cases AST.TString) in let contract:OwnedIR.callSignature={OwnedIR.parameters=[parameter;OwnedIR.UnmanagedCallParameter];result=OwnedIR.ProducedCallResult} in list Fun.id (select (fun ()->run [main] [] P.stdlibInliningConfig CompilerOptions.defaultOptions true F.empty (FunctionIdMap.ofList [main.A.id,contract])))) [OwnedIR.UnmanagedCallParameter;OwnedIR.BorrowedCallParameter;OwnedIR.ConsumedCallParameter;OwnedIR.UniqueCallParameter] in
 tuple [config P.stdlibInliningConfig;run [] [] P.stdlibInliningConfig CompilerOptions.defaultOptions true F.empty FunctionIdMap.empty;observed;externalCases;ownershipCases]
