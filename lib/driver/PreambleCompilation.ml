(* PreambleCompilation.fs - Compile reusable preamble contexts and their dependencies. *)
module X=CompilationContexts
module P=SourcePreparation
module M=StringOrder.Map
module S=StringOrder.Set
module K=SpecializationIdentity.SpecMap
let (let*)=Result.bind
let preambleError error=let prefix="Reference count insertion error: " in if HostText.startsWith error prefix then "Preamble RC insertion error: "^String.sub error (String.length prefix) (String.length error-String.length prefix) else "Preamble "^error
(* Build preamble with stdlib as base, returning extended context for test compilation.
   Preamble functions go through the full pipeline (parse → typecheck → mono → inline → lift → ANF → RC → TCO).
   The result is built once per file and reused for all tests in that file. *)
let buildPreambleContext allowInternal (stdlib:X.stdlibResult) preamble _sourceFile _funcLineMap recorder=
 (* Handle empty preamble - return a context that just wraps stdlib. *)
 if HostText.trim preamble="" then Ok (stdlib,{X.context=stdlib.X.context;anfFunctions=[];typeMap=stdlib.X.stdlibTypeMap;symbolicFunctions=[];callGraphSummaries=FunctionIdMap.empty;symbolicCallGraph=FunctionIdMap.empty}) else
 let* analysis=PreambleAnalysis.analyzePreamble allowInternal stdlib preamble in
 let typed=analysis.X.typedAST in let env=analysis.X.typeCheckEnv in
 (* Extract generic function definitions from preamble. *)
 let generic=SpecializationIdentity.extractGenericFuncDefs typed in
 (* Merge stdlib generics with preamble generics. *)
 let merged=M.fold M.add generic stdlib.X.context.X.genericFuncDefs in
 (* Convert preamble to ANF (mono → inline → lift → ANF). *)
 let* conversion=P.convertTypedDeclarationsWithTrace recorder (Some stdlib.X.context) (P.Monomorphize (Some stdlib.X.context.X.genericFuncDefs)) typed |> Result.map_error (fun error->"Preamble ANF conversion error: "^error) in
 let registries=conversion.P.registries in let options=CompilerOptions.defaultOptions in let start=HostClock.milliseconds () in let elapsed ()=HostClock.milliseconds ()-.start in
 let returns=X.mergeReturnTypes stdlib.X.context.X.returnTypes conversion.P.localReturnTypes in
 let names=List.fold_left (fun names (func:ANF.functionDef)->S.add func.ANF.name names) stdlib.X.context.X.baseFuncNames conversion.P.functions in
 let values=M.fold M.add (X.checkedValueArtifacts typed) stdlib.X.context.X.checkedValues in
 let context=X.buildContext stdlib.X.context.X.target conversion.P.symbols env values merged K.empty registries names returns in
 let context={context with X.writtenEnvironment=Option.map (WrittenChecking.includeAllocatedFunctions conversion.P.symbols) analysis.X.writtenEnvironment} in
 let* functions,ssa,typeMap=ANFPipeline.buildAnf 0 options elapsed registries (CheckedAST.nextFunctionOrdinal conversion.P.symbols) InliningCommon.defaultConfig FunctionIdMap.empty M.empty SpecializationIdentity.FunctionSet.empty conversion.P.functions FunctionIdMap.empty false recorder |> Result.map_error preambleError in
 let context=X.includeCompiledFunctions functions context in
 let* allocated,summaries=NativePipeline.lowerToAllocatedLirWithKnown stdlib.X.callGraphSummaries stdlib.X.context.X.target 0 options elapsed recorder None None "preamble" ssa typeMap registries None returns |> Result.map_error (fun error->"Preamble "^error) in
 let stdlibNames=List.map (fun (func:LIR.functionDef)->func.LIR.name) stdlib.X.allocatedFunctions |> S.of_list in
 let symbolic=List.filter (fun (func:LIR.functionDef)->not (S.mem func.LIR.name stdlibNames)) allocated in
 (* Merge TypeMaps (stdlib + preamble). *)
 let typeMap=ANF.TypeMap.merge stdlib.X.stdlibTypeMap typeMap in
 Ok (stdlib,{X.context;anfFunctions=functions;typeMap;symbolicFunctions=symbolic;callGraphSummaries=summaries;symbolicCallGraph=DeadCodeElimination.buildCallGraph (CheckedAST.functionIds context.X.symbols) symbolic})
(* Build preamble context from a typed preamble analysis and precomputed specializations. *)
let buildPreambleContextFromAnalysis (stdlib:X.stdlibResult) (analysis:X.preambleAnalysis) (specialization:SpecializationIdentity.specializationResult) _sourceFile _funcLineMap recorder=
 let specs=P.mergeSpecRegistries stdlib.X.context.X.specRegistry specialization.SpecializationIdentity.specRegistry in
 let generic=M.fold M.add analysis.X.genericFuncDefs stdlib.X.context.X.genericFuncDefs in
 let _,items=CheckedAST.viewProgram analysis.X.typedAST in
 let symbols,functions=SpecializationIdentity.importSpecializedFunctions specialization.SpecializationIdentity.symbols specialization.SpecializationIdentity.specializedFuncs in
 let tops=List.map (fun func->CheckedAST.FunctionDef func) functions @ items in
 let symbols,tops=CheckedMaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums symbols analysis.X.typeCheckEnv.Types.aliasReg analysis.X.typeCheckEnv.Types.indexedTypeReg analysis.X.typeCheckEnv.Types.variantLookup analysis.X.typeCheckEnv.Types.indexedSumTypeReg tops in
 let program=CheckedAST.programFromCheckedParts (symbols,tops) in
 let* conversion=P.convertTypedDeclarationsWithTrace recorder (Some stdlib.X.context) (P.ReplaceTypeApps specs) program in
 let registries=conversion.P.registries in let options=CompilerOptions.defaultOptions in let start=HostClock.milliseconds () in let elapsed ()=HostClock.milliseconds ()-.start in
 let returns=X.mergeReturnTypes stdlib.X.context.X.returnTypes conversion.P.localReturnTypes in
 let names=List.fold_left (fun names (func:ANF.functionDef)->S.add func.ANF.name names) stdlib.X.context.X.baseFuncNames conversion.P.functions in
 let values=M.fold M.add (X.checkedValueArtifacts analysis.X.typedAST) stdlib.X.context.X.checkedValues in
 let context=X.buildContext stdlib.X.context.X.target conversion.P.symbols analysis.X.typeCheckEnv values generic specs registries names returns in
 let context={context with X.writtenEnvironment=Option.map (WrittenChecking.includeAllocatedFunctions conversion.P.symbols) analysis.X.writtenEnvironment} in
 let* functions,ssa,typeMap=ANFPipeline.buildAnf 0 options elapsed registries (CheckedAST.nextFunctionOrdinal conversion.P.symbols) InliningCommon.defaultConfig FunctionIdMap.empty M.empty SpecializationIdentity.FunctionSet.empty conversion.P.functions FunctionIdMap.empty false recorder |> Result.map_error preambleError in
 let context=X.includeCompiledFunctions functions context in
 let* allocated,summaries=NativePipeline.lowerToAllocatedLirWithKnown stdlib.X.callGraphSummaries stdlib.X.context.X.target 0 options elapsed recorder None None "preamble" ssa typeMap registries None returns |> Result.map_error (fun error->"Preamble "^error) in
 let stdlibNames=List.map (fun (func:LIR.functionDef)->func.LIR.name) stdlib.X.allocatedFunctions |> S.of_list in
 let symbolic=List.filter (fun (func:LIR.functionDef)->not (S.mem func.LIR.name stdlibNames)) allocated in
 let typeMap=ANF.TypeMap.merge stdlib.X.stdlibTypeMap typeMap in
 Ok (stdlib,{X.context;anfFunctions=functions;typeMap;symbolicFunctions=symbolic;callGraphSummaries=summaries;symbolicCallGraph=DeadCodeElimination.buildCallGraph (CheckedAST.functionIds context.X.symbols) symbolic})
