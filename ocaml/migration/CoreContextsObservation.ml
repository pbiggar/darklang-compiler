(* Compare reusable preamble compilation and complete stdlib specialization artifacts. *)
open Dark_compiler
module X=CompilationContexts
module C=CheckedAST
module M=StringOrder.Map
module S=StringOrder.Set
module F=SpecializationIdentity.FunctionSet
module K=SpecializationIdentity.SpecMap
let tuple=SemanticJson.tuple
let str=SemanticJson.string
let typ=SemanticAST.semanticType
let list f xs=`List (List.map f xs)
let map f xs=`Assoc ["map",list (fun (key,value)->tuple [str key;f value]) (M.bindings xs)]
let ids f xs=SemanticJson.union "FunctionIdMap" "FunctionIdMap" [`Assoc ["map",list (fun (key,value)->tuple [`Assoc ["kind",`String "uint64";"value",`String (SemanticJson.unsigned64 (AST.functionIdValue key))];f value]) (FunctionIdMap.toList xs)]]
let strings xs=`Assoc ["set",list str (S.elements xs)]
let fields=list (fun (name,value)->tuple [str name;typ value])
let typeReg=map (fun (value:TypeRegistries.recordTypeInfo)->SemanticJson.record "RecordTypeInfo" ["TypeParams",list str value.TypeRegistries.typeParams;"Fields",fields value.TypeRegistries.fields])
let variants=map (fun (name,params,tag,args)->tuple [str name;list str params;SemanticJson.int32 tag;list typ args])
let returns=ids (fun (name,value)->tuple [str name;typ value])
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let typeMap value=let first,types=ANF.TypeMap.snapshot value in tuple [SemanticJson.int32 first;`List (Array.to_list (Array.map (SemanticJson.option typ) types))]
let spec (name,args)=tuple [str name;list typ args]
let registry xs=`Assoc ["map",list (fun (key,value)->tuple [spec key;str value]) (K.bindings xs)]
let env (value:Types.typeCheckEnv)=tuple [
 map fields value.Types.typeReg;
 map (fun (info:Types.recordTypeInfo)->tuple [fields info.Types.fields;map typ info.Types.fieldTypes;list str info.Types.typeParams]) value.Types.indexedTypeReg;
 strings value.Types.recordTypeNames;variants value.Types.variantLookup;
 map (fun (info:Types.sumTypeInfo)->tuple [list str info.Types.typeParams;list (fun (variant:Types.sumVariantInfo)->tuple [str variant.Types.name;SemanticJson.int32 variant.Types.tag;list typ variant.Types.fields]) info.Types.variants]) value.Types.indexedSumTypeReg;
 strings value.Types.sumTypeNames;map typ value.Types.funcEnv;map typ value.Types.values;map (list str) value.Types.funcParamNames;
 tuple [map (list str) value.Types.genericFuncReg.Types.functions;`Bool value.Types.genericFuncReg.Types.requireExplicitTypeArgsForBareCalls];
 map (SemanticAST.observationFunctionDefNode typ) value.Types.genericFuncDefs;map SemanticAST.observationModuleFunc value.Types.moduleRegistry;
 map (fun (params,target)->tuple [list str params;typ target]) value.Types.aliasReg]
let generic values=
 let cached=ref [] in let catalogs=ref [] in
 let catalog symbols=
  match List.find_opt (fun (old,_,_)->old==symbols) !cached with Some (_,index,_)->index|None->
  let encoded=DriverObservation.symbols symbols in
  let index=match List.find_index ((=) encoded) !catalogs with Some index->index|None->let index=List.length !catalogs in catalogs:= !catalogs@[encoded];index in
  cached:=(symbols,index,encoded):: !cached;index in
 let artifacts=map (fun (item:SpecializationIdentity.genericFunctionArtifact)->
  let index=catalog item.SpecializationIdentity.symbols in
  tuple [SemanticJson.int32 index;DriverObservation.program (C.programFromCheckedParts (C.emptySymbols (),[C.FunctionDef item.SpecializationIdentity.func]));list ProductionMIR.functionId (F.elements item.SpecializationIdentity.directDependencies)]) values in
 tuple [`List !catalogs;artifacts]
let context (value:X.pipelineContext)=tuple [
 DriverObservation.symbols value.X.symbols;CacheIdentityObservation.target value.X.target;env value.X.typeCheckEnv;
 map (fun (item:X.checkedValueArtifact)->tuple [SemanticJson.int32 item.X.bindingCursor;typ item.X.typ;str (CheckedStructuralFormat.expr item.X.body)]) value.X.checkedValues;
 generic value.X.genericFuncDefs;
 registry value.X.specRegistry;DriverObservation.registries value.X.registries;strings value.X.baseFuncNames;
 tuple [ids (list typ) value.X.lambdaLiftFunctions.LiftFunctions.params;ids typ value.X.lambdaLiftFunctions.LiftFunctions.returnTypes;ids (fun (names,value)->tuple [list str names;typ value]) value.X.lambdaLiftFunctions.LiftFunctions.genericDefs];
 typeReg value.X.lambdaLiftTypeReg;variants value.X.lambdaLiftVariantLookup;
 tuple [ProductionMIR.variantRegistry (fst value.X.projectedMirRegistries);ProductionMIR.recordRegistry (snd value.X.projectedMirRegistries)];returns value.X.returnTypes;strings value.X.packageCatalogGenericCallers;
 `Bool (value.X.typeCheckEnv.Types.functionCatalog=C.functionCatalog value.X.symbols);`Bool (value.X.typeCheckEnv.Types.typeCatalog=C.typeCatalog value.X.symbols);`Bool (Option.is_some value.X.writtenEnvironment)]
let graph values=list (fun (id,calls)->tuple [ProductionMIR.functionId id;list ProductionMIR.functionId (F.elements calls)]) (FunctionIdMap.toList values)
let preamble (value:X.preambleContext)=tuple [context value.X.context;list ProductionANF.aNF_functionDef value.X.anfFunctions;typeMap value.X.typeMap;list ProductionLIR.functionDef value.X.symbolicFunctions;ids CacheIdentityObservation.summary value.X.callGraphSummaries;graph value.X.symbolicCallGraph]
let preambles case=let stdlib,_=Lazy.force StdlibCompilationObservation.base in Result.map (fun (stdlib:X.stdlibResult)->
 let sources=["";" \n\t";"let add (x: Int64) : Int64 = x + 1L";"let id (x: 'a) : 'a = x";"val saved = 4L\nlet add (x: Int64) : Int64 = x + saved";"type R = { x: Int64 }\nlet same (x: R) : Bool = x == x";"let broken =";"let bad (x: Int64) : String = x"] in
 List.concat_map (fun source->List.concat_map (fun allowInternal->List.map (fun fromAnalysis->fun ()->
 let phases=ref [] in let recorder (value:CompilerOptions.passTiming)=phases:=value.CompilerOptions.pass:: !phases in
 let report=try (if not fromAnalysis then PreambleCompilation.buildPreambleContext allowInternal stdlib source "preamble.dark" M.empty (Some recorder) else
 Result.bind (PreambleAnalysis.analyzePreamble allowInternal stdlib source) (fun (analysis:X.preambleAnalysis)->
 let specs=if M.mem "id" analysis.X.genericFuncDefs then SpecializationIdentity.SpecSet.singleton ("id",[AST.TInt64]) else SpecializationIdentity.SpecSet.empty in
 let specialization=Monomorphization.specializeFromSpecs (C.programSymbols analysis.X.typedAST) (M.fold M.add analysis.X.genericFuncDefs stdlib.X.context.X.genericFuncDefs) specs in
 PreambleCompilation.buildPreambleContextFromAnalysis stdlib analysis specialization "preamble.dark" M.empty (Some recorder))) with Failure message|Invalid_argument message->Error message in
 source,allowInternal,fromAnalysis,report,List.rev !phases) [false;true]) [false;true]) sources |> fun cases->(List.nth cases case) ()) stdlib
let specializationCache=Hashtbl.create 7
let specializations case=match Hashtbl.find_opt specializationCache case with Some value->value|None->
 let result=let stdlib,_=Lazy.force StdlibCompilationObservation.base in Result.map (fun (stdlib:X.stdlibResult)->
 let record="Port.Record" in let sum="Port.Sum" in
 let externalTypes=M.singleton record {TypeRegistries.typeParams=[];fields=["x",AST.TInt64;"s",AST.TString]} in
 let externalVariants=M.of_list [sum^".A",(sum,[],0,[]);sum^".B",(sum,[],1,[AST.TInt64])] in
 let cases=[[];["Darklang.Stdlib.List.length",[AST.TInt64]];["Darklang.Stdlib.List.reverse",[AST.TString]];["Darklang.Stdlib.List.map",[AST.TInt64;AST.TString]];["Darklang.Stdlib.List.reverse",[AST.TRecord (record,[])]];["Darklang.Stdlib.List.reverse",[AST.TSum (sum,[])]];["missing",[AST.TInt64]]] in
 List.map (fun specs->fun ()->let specs=SpecializationIdentity.SpecSet.of_list specs in let phases=ref [] in let record (value:CompilerOptions.passTiming)=phases:=value.CompilerOptions.pass:: !phases in
 let report=try StdlibCompilation.buildStdlibSpecializations stdlib specs externalTypes externalVariants (Some record) with Failure message|Invalid_argument message->Error message in
 let repeated=Result.bind report (fun value->try StdlibCompilation.buildStdlibSpecializations value specs externalTypes externalVariants None with Failure message|Invalid_argument message->Error message) in
 report,repeated,List.rev !phases) cases |> fun cases->(List.nth cases case) ()) stdlib in Hashtbl.add specializationCache case result;result
let observe input=
 let open Yojson.Basic.Util in let request=Yojson.Basic.from_string input in let bucket=request |> member "bucket" |> to_int in let case=request |> member "case" |> to_int in
 let selected encoder values=List.mapi (fun index value->index,value) values |> List.filter (fun (index,_)->index mod 64=bucket) |> list (fun (index,value)->tuple [SemanticJson.int32 index;encoder value]) in
 let described (value:X.stdlibResult)=tuple [
 (if bucket=0 then tuple [DriverObservation.program value.X.typedAST;context value.X.context;typeMap value.X.stdlibTypeMap] else `Null);
 selected ProductionLIR.functionDef value.X.allocatedFunctions;selected (fun (id,summary)->tuple [ProductionMIR.functionId id;CacheIdentityObservation.summary summary]) (FunctionIdMap.toList value.X.callGraphSummaries);
 selected (fun (name,func)->tuple [str name;ProductionANF.aNF_functionDef func]) (M.bindings value.X.stdlibAnfFunctions);
 selected (fun (name,func)->tuple [str name;ProductionANF.aNF_functionDef func]) (M.bindings value.X.stdlibAnfOptimizationCandidates);
 selected (fun (id,calls)->tuple [ProductionMIR.functionId id;list ProductionMIR.functionId (F.elements calls)]) (FunctionIdMap.toList value.X.stdlibCallGraph);
 selected (fun (id,calls)->tuple [ProductionMIR.functionId id;list ProductionMIR.functionId (F.elements calls)]) (FunctionIdMap.toList value.X.stdlibAnfCallGraph);
 selected (fun (id,(info:InliningCommon.functionInfo))->tuple [ProductionMIR.functionId id;ProductionANF.aNF_functionDef info.InliningCommon.func;list ProductionMIR.functionId (F.elements info.InliningCommon.calls);SemanticJson.int32 info.InliningCommon.size;`Bool info.InliningCommon.isRecursive;`Bool info.InliningCommon.hasClosures;`Bool info.InliningCommon.hasTailCalls;`Bool info.InliningCommon.isExternal]) (FunctionIdMap.toList value.X.stdlibInlineCandidates)] in
 if case<32 then result (fun (source,allowInternal,fromAnalysis,report,phases)->tuple [str source;`Bool allowInternal;`Bool fromAnalysis;result (fun (stdlib,value)->
 let compiled source=let request={X.context=X.StdlibWithPreamble (stdlib,value);mode=CompilerOptions.TestExpression;sources=NonEmptyList.singleton {X.name="user.dark";purpose=NameSyntax.SourceUnitPurpose.Executable;source};allowInternal;verbosity=0;options=CompilerOptions.defaultOptions;packageValues=X.emptyPackageValueCatalog;packageManager=None;passTimingRecorder=None;session=None} in
 result X64EncodingObservation.bytes (CompilerLibrary.compile request).CompilerOptions.result in
 tuple [preamble value;list compiled ["()";"add 2L";"id 3L";"saved"];
 result (fun (_,program,_)->DriverObservation.program program) (Result.bind (WrittenParsing.parse Validation.Script "()") (fun parsed->WrittenChecking.checkSourceUnitsWithBase value.X.context.X.writtenEnvironment allowInternal false [parsed]))]) report;list str phases]) (preambles case) else
 result (fun (report,repeated,phases)->tuple [result described (if (case-32) mod 2=0 then report else repeated);(if bucket=0 then list str phases else `Null)]) (specializations ((case-32)/2))
