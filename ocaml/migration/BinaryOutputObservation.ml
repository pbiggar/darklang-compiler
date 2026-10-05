(* Compare complete driver executable bytes, cache reuse and timing order. *)
open Dark_compiler
module A=ANF
module R=AST_to_ANF
module G=Backend_Arm64_CodeGen
module M=StringOrder.Map
module F=SpecializationIdentity.FunctionSet
let tuple=SemanticJson.tuple
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let attempt f action=result f (try action () with Failure message|Invalid_argument message->Error message)
let capture action=
 let path=Filename.temp_file "binary-output-log" ".txt" in let saved=Unix.dup ~cloexec:true Unix.stdout in
 Fun.protect ~finally:(fun ()->flush stdout;Unix.dup2 saved Unix.stdout;Unix.close saved;Sys.remove path) (fun ()->
 let fd=Unix.openfile path [Unix.O_WRONLY;Unix.O_CLOEXEC;Unix.O_TRUNC] 0 in flush stdout;Unix.dup2 fd Unix.stdout;Unix.close fd;
 let result=action () in flush stdout;let text=In_channel.with_open_bin path In_channel.input_all in
 let text=Str.global_replace (Str.regexp "[0-9]+\\(\\.[0-9]+\\)?ms") "<duration>ms" text in tuple [result;str text])
let observe input=
 let open Yojson.Basic.Util in let request=Yojson.Basic.from_string input in let source=request |> member "text" |> to_string in let bucket=request |> member "bucket" |> to_int in
 let caseIndex=ref 0 in let select action=let index= !caseIndex in incr caseIndex;if index mod 16=bucket then [tuple [SemanticJson.int32 index;action ()]] else [] in
 let fid n=AST.functionId (Int64.of_int n) in let id n=A.TempId n in let int n=A.IntLiteral (A.Int64 n) in
 let fn n name returnType body={A.id=fid n;name;typedParams=[];returnType;returnOwnership=A.OwnedReturn;body} in
 let entry typ atom=fn 10 "_start" typ (A.Return atom) in
 let callee=fn 20 "callee" AST.TInt64 (A.Return (int 17L)) in
 let caller=fn 10 "_start" AST.TInt64 (A.Let (id 1,A.Call (fid 20,[]),A.Return (A.Var (id 1)))) in
 let fixtures=[[];[entry AST.TUnit A.UnitLiteral];[entry AST.TInt64 (int 42L)];[entry AST.TBool (A.BoolLiteral true)];[entry AST.TFloat64 (A.FloatLiteral 1.5)];[entry AST.TString (A.StringLiteral source)];[caller;callee];[callee]] in
 let base={CompilerOptions.defaultOptions with CompilerOptions.disableInlining=true} in
 let options=[base;{base with CompilerOptions.enableLeakCheck=true};{base with CompilerOptions.disableFreeList=true};{base with CompilerOptions.enableCoverage=true}] in
 let run functions target options cached=
  let signatures=List.map (fun (func:A.functionDef)->func.A.id,(func.A.name,AST.TFunction ([],func.A.returnType))) functions |> FunctionIdMap.ofList in
  let names=FunctionIdMap.map (fun _ (name,_)->name) signatures in
  let registries={(SourcePreparation.emptyRegistries M.empty) with R.funcReg=signatures;functionNames=names;functionIds=TypeRegistries.functionIdsFromNames names} in
  attempt Fun.id (fun ()->Result.bind (ANFPipeline.buildAnf 0 options (fun ()->0.) registries 100L InliningCommon.defaultConfig FunctionIdMap.empty M.empty F.empty functions FunctionIdMap.empty false None) (fun (_,ssa,typeMap)->Result.map (fun (functions,summaries)->
   let program=LIR.Program (functions,M.empty,M.empty) in let identity=Obj.repr (ref 0) in
   let groups=[{G.contextIdentity=identity;reusableAcrossCompilations=true;functions}] in let metadata=[{G.contextIdentity=identity;functions}] in
   let session=if cached then Some (new CompilationSession.compilationSession ()) else None in
   let phases=ref [] in let record (value:CompilerOptions.passTiming)=phases:=tuple [str value.CompilerOptions.pass;`Bool (value.CompilerOptions.elapsed>=0L)]:: !phases in
   let verbosity=if not cached && options=base then 3 else 0 in
   let generate ()=capture (fun ()->result X64EncodingObservation.bytes (BinaryOutput.generateBinary target verbosity options (fun ()->0.) (Some record) "codegen" "emit {format}" true true session identity groups metadata registries.R.rcSumShapeReg (FunctionIdMap.toList summaries |> List.filter_map (fun (id,(summary:CompilationCacheIdentity.functionSummary))->Option.map (fun writes->id,writes) summary.CompilationCacheIdentity.arm64Writes) |> FunctionIdMap.ofList) program)) in
   let first=generate () in let firstPhases=List.rev !phases in phases:=[];let second=generate () in let secondPhases=List.rev !phases in
   tuple [ProductionLIR.program program;first;list Fun.id firstPhases;second;list Fun.id secondPhases]) (NativePipeline.lowerToAllocatedLirWithKnown FunctionIdMap.empty target 0 options (fun ()->0.) None None None source ssa typeMap registries None (SourcePreparation.extractReturnTypes signatures)))) in
 tuple [list (fun functions->list (fun target->list (fun options->list (fun cached->list Fun.id (select (fun ()->run functions target options cached))) [false;true]) options) [Platform.LinuxX86_64;Platform.ARM64Backend Platform.LinuxARM64]) fixtures]
