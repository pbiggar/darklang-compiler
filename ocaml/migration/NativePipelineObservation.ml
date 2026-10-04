(* Compare scheduled native output, published summaries and cache/timing contracts. *)
open Dark_compiler
module A=ANF
module R=AST_to_ANF
module C=CompilationCacheIdentity
module P=NativePipeline
module M=StringOrder.Map
module F=SpecializationIdentity.FunctionSet
let tuple=SemanticJson.tuple
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let attempt f action=result f (try action () with Failure message|Invalid_argument message->Error message)
let observe input=
 let open Yojson.Basic.Util in let request=Yojson.Basic.from_string input in let source=request |> member "text" |> to_string in let bucket=request |> member "bucket" |> to_int in
 let caseIndex=ref 0 in let select action=let index= !caseIndex in incr caseIndex;if index mod 16=bucket then [tuple [SemanticJson.int32 index;action ()]] else [] in
 let fid n=AST.functionId (Int64.of_int n) in let id n=A.TempId n in let var n=A.Var (id n) in let int n=A.IntLiteral (A.Int64 n) in
 let func n name params returnType body={A.id=fid n;name;typedParams=List.map (fun (n,typ)->{A.id=id n;typ}) params;returnType;returnOwnership=A.OwnedReturn;body} in
 let callee=func 100 "constant" [] AST.TInt64 (A.Return (int 42L)) in let identity=func 200 "identity" [20,AST.TInt64] AST.TInt64 (A.Return (var 20)) in
 let caller=func 300 ("caller_"^source) [] AST.TInt64 (A.Let (id 30,A.Call (fid 100,[]),A.Let (id 31,A.Call (fid 200,[var 30]),A.Return (var 31)))) in
 let recursive n other=func n "recursive" [10,AST.TInt64;11,AST.TBool] AST.TInt64 (A.If (var 11,A.Return (var 10),A.Let (id 12,A.Call (fid other,[var 10;A.BoolLiteral true]),A.Return (var 12)))) in
 let duplicate n value=func n "duplicate" [] AST.TInt64 (A.Return (int value)) in
 let stringFunc=func 400 "string" [] AST.TString (A.Return (A.StringLiteral source)) in
 let floatFunc=func 500 "float" [50,AST.TFloat64] AST.TFloat64 (A.Let (id 51,A.Prim (A.Add,var 50,A.FloatLiteral 1.5),A.Return (var 51))) in
 let externalCaller=func 700 "external_caller" [] AST.TInt64 (A.Let (id 70,A.Call (fid 600,[]),A.Return (var 70))) in
 let optionsBase={CompilerOptions.defaultOptions with CompilerOptions.disableInlining=true} in
 let options=[optionsBase;{optionsBase with CompilerOptions.disableMIROpt=true};{optionsBase with CompilerOptions.disableMIRSCCP=true};{optionsBase with CompilerOptions.disableMIRCSE=true};{optionsBase with CompilerOptions.disableMIRLICM=true};{optionsBase with CompilerOptions.disableMIRDCE=true};{optionsBase with CompilerOptions.disableLIRPeephole=true};{optionsBase with CompilerOptions.disableTCO=true};{optionsBase with CompilerOptions.enableCoverage=true}] in
 let fixtures=[[];[caller;identity;callee];[recursive 800 900;recursive 900 800];[duplicate 401 1L;duplicate 402 2L];[stringFunc;floatFunc];[externalCaller]] in
 let empty=SourcePreparation.emptyRegistries M.empty in
 let run functions target options split cached=
  let signatures=List.map (fun (func:A.functionDef)->func.A.id,(func.A.name,AST.TFunction (List.map (fun (param:A.typedParam)->param.A.typ) func.A.typedParams,func.A.returnType))) functions @ [fid 600,("external",AST.TFunction ([],AST.TInt64))] |> FunctionIdMap.ofList in let names=FunctionIdMap.map (fun _ (name,_)->name) signatures in
  let registries={empty with R.funcReg=signatures;functionNames=names;functionIds=TypeRegistries.functionIdsFromNames names} in
  let phases=ref [] in let record (value:CompilerOptions.passTiming)=phases:=tuple [str value.CompilerOptions.pass;`Bool (value.CompilerOptions.elapsed>=0L)]:: !phases in
  let clock=ref 0. in let elapsed ()=clock:= !clock+.0.125;!clock in
  let session=new CompilationSession.compilationSession () in
  let caches=if cached then Some {C.optimizeMir=(fun key generate->session#optimizeMirFunction key generate);allocateLir=(fun arch func generate->session#allocateLirFunction arch func generate);allocateCallAwareLir=(fun func summaries generate->session#allocateCallAwareLirFunction func summaries generate)} else None in
  let releaseCache=if cached then Some (fun unique key plan generate->session#arm64ReleasePlanSummary unique key plan generate) else None in
  let externalSummary={C.unknownSummary with C.purity={MIROptimizationFacts.observableEffects=false;readsMutableState=false;mayTrap=false;mayDiverge=false};constantReturn=Some (AST.TInt64,MIR.Int64Const 17L)} in
  let externalSummaries=FunctionIdMap.ofList [fid 600,externalSummary] in
  let output=attempt (fun (funcs,summaries)->tuple [list ProductionLIR.functionDef funcs;list (fun (id,summary)->tuple [ProductionMIR.functionId id;CacheIdentityObservation.summary summary]) (FunctionIdMap.toList summaries)]) (fun ()->
   Result.bind (ANFPipeline.buildAnf 0 options elapsed registries 1000L InliningCommon.defaultConfig FunctionIdMap.empty M.empty F.empty functions FunctionIdMap.empty false None) (fun (_,ssa,typeMap)->let groups=if split then List.map (fun func->[func],typeMap) ssa else [ssa,typeMap] in P.lowerToAllocatedLirWithKnownGroups externalSummaries target 0 options elapsed (Some record) caches releaseCache source groups registries None (FunctionIdMap.ofList [fid 600,("external",AST.TInt64)]))) in
  tuple [output;list Fun.id (List.rev !phases)] in
 tuple [list (fun functions->list (fun target->list (fun options->list (fun split->list (fun cached->list Fun.id (select (fun ()->run functions target options split cached))) [false;true]) [false;true]) options) [Platform.LinuxX86_64;Platform.ARM64Backend Platform.LinuxARM64;Platform.ARM64Backend Platform.MacOSARM64]) fixtures]
