(* Compare complete reusable stdlib and native library compilation boundaries. *)
open Dark_compiler
module X=CompilationContexts
module M=StringOrder.Map
module S=StringOrder.Set
module F=SpecializationIdentity.FunctionSet
let tuple=SemanticJson.tuple
let str=SemanticJson.string
let list f xs=`List (List.map f xs)
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let attempt f action=result f (try action () with Failure message|Invalid_argument message->Error message)
let base=lazy (let phases=ref [] in let record (value:CompilerOptions.passTiming)=phases:=value.CompilerOptions.pass:: !phases in let output=StdlibCompilation.buildStdlibWithTrace Platform.LinuxX86_64 (Some record) in output,List.rev !phases)
let typeMap value=let first,types=ANF.TypeMap.snapshot value in tuple [SemanticJson.int32 first;`List (Array.to_list (Array.map (SemanticJson.option SemanticAST.semanticType) types))]
let observe input=
 let open Yojson.Basic.Util in let request=Yojson.Basic.from_string input in let bucket=request |> member "bucket" |> to_int in
 let selected encoder values=List.mapi (fun index value->index,value) values |> List.filter (fun (index,_)->index mod 64=bucket) |> list (fun (index,value)->tuple [SemanticJson.int32 index;encoder value]) in
 let stdlib,phases=Lazy.force base in
 attempt Fun.id (fun ()->Result.map (fun (stdlib:X.stdlibResult)->
  let graph values=selected (fun (id,calls)->tuple [ProductionMIR.functionId id;SemanticJson.record "Calls" ["Ids",list ProductionMIR.functionId (F.elements calls)]]) (FunctionIdMap.toList values) in
  let anf values=selected (fun (name,func)->tuple [str name;ProductionANF.aNF_functionDef func]) (M.bindings values) in
  let describe (stdlib:X.stdlibResult)=tuple [
   (if bucket=0 then tuple [DriverObservation.program stdlib.X.typedAST;DriverObservation.symbols stdlib.X.context.X.symbols;DriverObservation.registries stdlib.X.context.X.registries;typeMap stdlib.X.stdlibTypeMap;list str (S.elements (CompilerReachability.getAllStdlibFunctionNamesFromStdlib stdlib))] else `Null);
   selected ProductionLIR.functionDef stdlib.X.allocatedFunctions;
   selected (fun (id,summary)->tuple [ProductionMIR.functionId id;CacheIdentityObservation.summary summary]) (FunctionIdMap.toList stdlib.X.callGraphSummaries);
   anf stdlib.X.stdlibAnfFunctions;anf stdlib.X.stdlibAnfOptimizationCandidates;graph stdlib.X.stdlibCallGraph;graph stdlib.X.stdlibAnfCallGraph;
   selected (fun (id,(info:InliningCommon.functionInfo))->tuple [ProductionMIR.functionId id;ProductionANF.aNF_functionDef info.InliningCommon.func;list ProductionMIR.functionId (F.elements info.InliningCommon.calls);SemanticJson.int32 info.InliningCommon.size;`Bool info.InliningCommon.isRecursive;`Bool info.InliningCommon.hasClosures;`Bool info.InliningCommon.hasTailCalls;`Bool info.InliningCommon.isExternal]) (FunctionIdMap.toList stdlib.X.stdlibInlineCandidates)] in
  let texts=["()";"1L";"true";"let f (x: Int64) : Int64 = x + 1L\nf 2L";"let id (x: 'a) : 'a = x\nid 3L";"Darklang.Stdlib.List.length [1L,2L]";"unknown 1L";"let broken ="] in
  let cases=List.concat_map (fun source->List.concat_map (fun mode->List.map (fun cached->source,mode,cached) [false;true]) [CompilerOptions.FullProgram;CompilerOptions.TestExpression]) texts in
  let compiled=selected (fun (source,mode,cached)->let session=if cached then Some (new CompilationSession.compilationSession ()) else None in
   let compile ()=let phases=ref [] in let record (value:CompilerOptions.passTiming)=phases:=value.CompilerOptions.pass:: !phases in
    let request={X.context=X.StdlibOnly stdlib;mode;sources=NonEmptyList.singleton {X.name="input.dark";purpose=NameSyntax.SourceUnitPurpose.Executable;source};allowInternal=false;verbosity=0;options=CompilerOptions.defaultOptions;packageValues=X.emptyPackageValueCatalog;packageManager=None;passTimingRecorder=Some record;session} in
    let report=CompilerLibrary.compile request in tuple [result X64EncodingObservation.bytes report.CompilerOptions.result;`Bool (report.CompilerOptions.compileTime>=0L);list str (List.rev !phases)] in
   let first=compile () in let second=compile () in tuple [first;second;result (fun names->list str (S.elements names)) (CompilerReachability.getReachableStdlibFunctionsFromStdlib stdlib source)]) cases in
  tuple [describe stdlib;(if bucket=0 then list str phases else `Null);compiled]) stdlib)
