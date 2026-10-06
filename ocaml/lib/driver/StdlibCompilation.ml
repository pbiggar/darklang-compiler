(* StdlibCompilation.fs - Build reusable standard-library functions and concrete specializations. *)
[@@@warning "-4"]
module X=CompilationContexts
module P=SourcePreparation
module M=StringOrder.Map
module S=StringOrder.Set
module K=SpecializationIdentity.SpecMap
module F=SpecializationIdentity.FunctionSet
module Specs=SpecializationIdentity.SpecSet
let (let*)=Result.bind
(* Load the stdlib and unicode_data.dark files.
   Returns validated interpreter source units in declaration order. *)
let loadStdlib ()=
 let files=["stdlib/Types.dark";"stdlib/NoModule.dark";"stdlib/Int8.dark";"stdlib/Int16.dark";"stdlib/Int32.dark";"stdlib/Int64.dark";"stdlib/__Integer.dark";"stdlib/Int.dark";"stdlib/Int128.dark";"stdlib/UInt8.dark";"stdlib/UInt16.dark";"stdlib/UInt32.dark";"stdlib/UInt64.dark";"stdlib/UInt128.dark";"stdlib/Bool.dark";"stdlib/CliFileSystem.dark";"stdlib/CliFileError.dark";"stdlib/Env.dark";"stdlib/Builtin.dark";"stdlib/Tuple2.dark";"stdlib/Tuple3.dark";"stdlib/Result.dark";"stdlib/Option.dark";"stdlib/ListSortByComparatorHelpers.dark";"stdlib/List.dark";"stdlib/Print.dark";"stdlib/Fun.dark";"stdlib/Float.dark";"stdlib/CliPosix.dark";"stdlib/CliPosixMode.dark";"stdlib/CliPosixError.dark";"stdlib/CliPosixStat.dark";"stdlib/Retry.dark";"stdlib/CliPath.dark";"stdlib/CliFile.dark";"unicode_data.dark";"unicode_data_index/00.dark";"unicode_data_index/01.dark";"unicode_data_index/02.dark";"unicode_data_index/03.dark";"unicode_data_index/04.dark";"unicode_data_index/05.dark";"unicode_data_index/06.dark";"unicode_data/00.dark";"unicode_data/01.dark";"unicode_data/02.dark";"unicode_data/03.dark";"unicode_data/04.dark";"unicode_data/05.dark";"unicode_data/06.dark";"unicode_data/07.dark";"unicode_data/08.dark";"unicode_data/09.dark";"unicode_data/10.dark";"unicode_data/11.dark";"unicode_data/12.dark";"unicode_data/13.dark";"unicode_data/14.dark";"unicode_data/15.dark";"unicode_data/16.dark";"unicode_data/17.dark";"unicode_data/18.dark";"unicode_data/19.dark";"unicode_data/20.dark";"unicode_data/21.dark";"unicode_data/22.dark";"unicode_data/23.dark";"unicode_data/24.dark";"unicode_data/25.dark";"unicode_data/26.dark";"unicode_data/27.dark";"unicode_data/28.dark";"unicode_data/29.dark";"unicode_data/30.dark";"unicode_data/31.dark";"unicode_data/32.dark";"unicode_data/33.dark";"unicode_data/34.dark";"unicode_data/35.dark";"unicode_data/36.dark";"unicode_data/37.dark";"unicode_data/38.dark";"unicode_data/39.dark";"unicode_data/40.dark";"unicode_data/41.dark";"unicode_data/42.dark";"unicode_data/43.dark";"unicode_data/44.dark";"unicode_data/45.dark";"unicode_data/46.dark";"unicode_data/47.dark";"unicode_data/48.dark";"unicode_data/49.dark";"unicode_data/50.dark";"unicode_data/51.dark";"unicode_data/52.dark";"unicode_data/53.dark";"unicode_data/54.dark";"unicode_data/55.dark";"unicode_data/56.dark";"unicode_data/57.dark";"unicode_data/58.dark";"unicode_data/59.dark";"unicode_data/60.dark";"unicode_data/61.dark";"unicode_data/62.dark";"unicode_data/63.dark";"stdlib/Unicode.dark";"stdlib/String.dark";"stdlib/__Hash.dark";"stdlib/Dict.dark";"stdlib/__HAMT.dark";"stdlib/Uuid.dark";"stdlib/Diff.dark";"stdlib/ProgramTypes.dark";"stdlib/RuntimeTypes.dark";"stdlib/RuntimeTypesBase.dark";"stdlib/RuntimeFQTypeName.dark";"stdlib/RuntimeFQFnName.dark";"stdlib/RuntimeFQValueName.dark";"stdlib/RuntimeTypeReference.dark";"stdlib/PrettyPrinterRuntimeTypes.dark";"stdlib/RuntimeValueType.dark";"stdlib/RuntimeDval.dark";"stdlib/PrettyPrinterRuntimeError.dark";"stdlib/RuntimeValueTypeSupport.dark";"stdlib/PackageManager.dark";"stdlib/PackageManagerPickContext.dark";"stdlib/SCMBranch.dark";"stdlib/ValueSearch.dark";"stdlib/DateTime.dark";"stdlib/Duration.dark";"stdlib/Blob.dark";"stdlib/Stream.dark";"stdlib/Html.dark";"stdlib/Http.dark";"stdlib/HttpRequest.dark";"stdlib/HttpClient.dark";"stdlib/HttpClientValues.dark";"stdlib/HttpClientSse.dark";"stdlib/HttpServerConfig.dark";"stdlib/HttpServer.dark";"stdlib/Network.dark";"stdlib/HttpWire.dark";"stdlib/DnsWire.dark";"stdlib/HttpConnect.dark";"stdlib/Pretty.dark";"stdlib/Char.dark";"stdlib/Regex.dark";"stdlib/Base64.dark";"stdlib/X509.dark";"stdlib/X509Identity.dark";"stdlib/RsaSpki.dark";"stdlib/Crypto.dark";"stdlib/RsaPss.dark";"stdlib/P256.dark";"stdlib/RsaPkcs1.dark";"stdlib/X509Chain.dark";"stdlib/AesGcm.dark";"stdlib/Chacha20Poly1305.dark";"stdlib/Tls13.dark";"stdlib/Tls13Handshake.dark";"stdlib/Tls13Certificate.dark";"stdlib/Tls13Client.dark";"stdlib/Math.dark";"stdlib/X25519.dark";"stdlib/__SkewList.dark";"stdlib/__ListArray.dark";"stdlib/CliColor.dark";"stdlib/CliTextField.dark";"stdlib/CliTerminalSession.dark";"stdlib/CliTuiTextEscape.dark";"stdlib/CliTuiTextWidth.dark";"stdlib/CliTuiTextClip.dark";"stdlib/CliTuiText.dark";"stdlib/CliLog.dark";"stdlib/CliProgress.dark";"stdlib/CliPrompt.dark";"stdlib/CliSpinner.dark";"stdlib/CliTable.dark";"stdlib/CliExecution.dark";"stdlib/CliOS.dark";"stdlib/CliArchitecture.dark";"stdlib/CliShell.dark";"stdlib/CliHost.dark";"stdlib/CliEnv.dark";"stdlib/CliArgs.dark";"stdlib/CliProcess.dark";"stdlib/CliSys.dark";"stdlib/CliStdin.dark";"stdlib/CliStdinModifiers.dark";"stdlib/CliStdinKeyRead.dark";"stdlib/CliStdinRead.dark";"stdlib/AltJsonParseError.dark";"stdlib/AltJson.dark";"stdlib/AltJsonHelpers.dark";"stdlib/AltJsonBuilder.dark";"stdlib/LanguageTools.dark";"stdlib/JsonPathPart.dark";"stdlib/JsonPath.dark";"stdlib/JsonParseError.dark";"stdlib/Json.dark"] in
 let load filename=
  let executable=if Filename.is_relative Sys.executable_name then Filename.concat (Sys.getcwd ()) Sys.executable_name else Sys.executable_name in
  let directory=Filename.dirname executable in
  let candidates=[Filename.concat directory ("../share/"^filename);Filename.concat directory ("../share/dark_compiler/"^filename);Filename.concat directory filename;Filename.concat directory ("../../../../share/"^filename);Filename.concat (Sys.getcwd ()) ("ocaml/share/"^filename)] in
  match List.find_opt Sys.file_exists candidates with
  |None->Error ("Could not find "^filename^" in any of: "^String.concat ", " candidates)
  |Some path->let text=In_channel.with_open_bin path In_channel.input_all in
   WrittenParsing.parse Validation.Script (HostPackageIO.decodeContent None text) |> Result.map_error (fun error->"Error parsing "^filename^": "^error) in
 ResultList.traverse load files
(* Build stdlib in isolation, returning reusable result.
   This can be called once and the result reused for multiple user program compilations. *)
let buildStdlibWithTrace target recorder=
 let measure name operation=let start=HostClock.milliseconds () in let result=operation () in PipelineDiagnostics.recordPassTiming recorder name (HostClock.milliseconds ()-.start);result in
 let* sources=measure "Stdlib detail: Source loading and parsing" loadStdlib in
 let* _,checked,written=measure "Stdlib detail: Type checking" (fun ()->WrittenChecking.checkSourceUnitsWithBase None true false sources) in
 let env=WrittenChecking.typeCheckEnvironment checked in let symbols,tops=CheckedAST.viewProgram checked in
 let symbols,tops=CheckedMaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums symbols env.Types.aliasReg env.Types.indexedTypeReg env.Types.variantLookup env.Types.indexedSumTypeReg tops in
 let typed=CheckedAST.programFromCheckedParts (symbols,tops) in
 (* Extract generic function definitions for on-demand monomorphization. *)
 let generic=SpecializationIdentity.extractGenericFuncDefs typed in
 (* Build module registry once (reused across all compilations). *)
 let _moduleRegistry=DarkStdlib.buildModuleRegistry () in
 let* conversion=measure "Stdlib detail: Declaration conversion" (fun ()->P.convertTypedDeclarationsWithTrace recorder None (P.Monomorphize None) typed) in
 let start=HostClock.milliseconds () in let elapsed ()=HostClock.milliseconds ()-.start in
 let registries=conversion.P.registries in let returns=P.extractReturnTypes registries.AST_to_ANF.funcReg in let names=X.buildBaseFuncNames registries in
 let context=X.buildContext target conversion.P.symbols env (X.checkedValueArtifacts typed) generic K.empty registries names returns in
 let context={context with X.writtenEnvironment=Some (WrittenChecking.includeAllocatedFunctions conversion.P.symbols written)} in
 let options=CompilerOptions.defaultOptions in
 let* functions,ssa,typeMap=ANFPipeline.buildAnf 0 options elapsed registries (CheckedAST.nextFunctionOrdinal conversion.P.symbols) ANFPipeline.stdlibInliningConfig FunctionIdMap.empty M.empty F.empty conversion.P.functions FunctionIdMap.empty false recorder in
 let context=X.includeCompiledFunctions functions context in
 let anfMap=List.map (fun (func:ANF.functionDef)->func.ANF.name,func) functions |> M.of_list in
 let inline=InliningCommon.buildExternalCandidateInfoMap InliningCommon.defaultConfig conversion.P.functions in
 let anfGraph=ANFDeadCodeElimination.buildCallGraph functions in
 let* allocated,summaries=NativePipeline.lowerToAllocatedLirWithKnown FunctionIdMap.empty target 0 options elapsed recorder None None "stdlib" ssa typeMap registries None returns in
 let callGraph=DeadCodeElimination.buildCallGraph (List.map (fun (func:LIR.functionDef)->func.LIR.name,func.LIR.id) allocated |> M.of_list) allocated in
 Ok {X.typedAST=typed;context;allocatedFunctions=allocated;callGraphSummaries=summaries;stdlibCallGraph=callGraph;stdlibAnfFunctions=anfMap;stdlibAnfOptimizationCandidates=List.map (fun (func:ANF.functionDef)->func.ANF.name,func) conversion.P.functions |> M.of_list;stdlibInlineCandidates=inline;stdlibAnfCallGraph=anfGraph;stdlibTypeMap=typeMap}
let buildStdlib target=buildStdlibWithTrace target None
(* Build stdlib specializations for a spec set and merge them into the stdlib result. *)
let buildStdlibSpecializations (stdlib:X.stdlibResult) specs externalTypes externalVariants recorder=
 if Specs.is_empty specs then Ok stdlib else
 let base=stdlib.X.context in let env=base.X.typeCheckEnv in
 let variants=M.fold M.add externalVariants env.Types.variantLookup in
 let indexed=Types.indexTypeRegistry variants (TypeRegistries.recordTypeParamsRegistry externalTypes) (TypeRegistries.recordFieldsRegistry externalTypes) in
 let types=M.fold M.add indexed env.Types.indexedTypeReg in
 let specialization=Monomorphization.specializeFromSpecs base.X.symbols base.X.genericFuncDefs specs in
 let initialSpecs=P.mergeSpecRegistries base.X.specRegistry specialization.SpecializationIdentity.specRegistry in
 let existing=M.to_seq stdlib.X.stdlibAnfFunctions |> Seq.map fst |> S.of_seq in
 let newFunctions=List.filter (fun (artifact:SpecializationIdentity.genericFunctionArtifact)->not (S.mem artifact.SpecializationIdentity.func.CheckedAST.name existing)) specialization.SpecializationIdentity.specializedFuncs in
 if newFunctions=[] then Ok {stdlib with X.context={base with X.specRegistry=initialSpecs}} else
 let* typeDefs,_=AST_to_ANF.splitDeclarations stdlib.X.typedAST in
 let symbols,functions=SpecializationIdentity.importSpecializedFunctions specialization.SpecializationIdentity.symbols newFunctions in
 let symbols,initialTops=List.fold_left (fun (symbols,all) func->let symbols,tops=CheckedMaterializeHelpers.materializeEqHelpersInTopLevels symbols env.Types.aliasReg types variants [CheckedAST.FunctionDef func] in symbols,tops::all) (symbols,[]) functions in
 let initialFunctions=List.rev initialTops |> List.concat |> List.filter_map (function CheckedAST.FunctionDef func->Some func|_->None) in
 let helperSpecs=List.map (Monomorphization.collectTypeAppsFromFunc symbols) initialFunctions |> List.fold_left Specs.union Specs.empty |> Specs.filter (fun (name,_)->M.mem name base.X.genericFuncDefs) in
 let helper=Monomorphization.specializeFromSpecs symbols base.X.genericFuncDefs helperSpecs in
 let combined=P.mergeSpecRegistries initialSpecs helper.SpecializationIdentity.specRegistry in
 let symbols,helpers=SpecializationIdentity.importSpecializedFunctions helper.SpecializationIdentity.symbols helper.SpecializationIdentity.specializedFuncs in
 let tops=helpers @ initialFunctions |> List.filter (fun (func:CheckedAST.functionDef)->not (S.mem func.CheckedAST.name existing)) |> List.map (fun func->CheckedAST.FunctionDef func) in
 let symbols,tops=CheckedMaterializeHelpers.materializeEqHelpersInTopLevels symbols env.Types.aliasReg types variants tops in
 let _,materialized=List.fold_left (fun (seen,all) top->match top with CheckedAST.FunctionDef func when not (S.mem func.CheckedAST.name seen)->S.add func.CheckedAST.name seen,func::all|_->seen,all) (S.empty,[]) tops in let materialized=List.rev materialized in
 let symbols,checkedDefs=List.fold_left (fun (symbols,defs) typeDef->let name=match typeDef with AST.RecordDef (name,_,_)|AST.SumTypeDef (name,_,_)|AST.TypeAlias (name,_,_)->name in let id,symbols=CheckedAST.internType name symbols in symbols,CheckedAST.TypeDef (id,CheckedAST.checkedTypeDef typeDef)::defs) (symbols,[]) typeDefs in
 let program=CheckedAST.programFromCheckedParts (symbols,List.rev checkedDefs @ List.map (fun func->CheckedAST.FunctionDef func) materialized) |> ValueRendering.rewriteDictionaryKeyRenderers env.Types.indexedTypeReg env.Types.indexedSumTypeReg in
 let* prepared=P.prepareProgramForAnf (P.ReplaceTypeApps combined) base.X.lambdaLiftTypeReg base.X.lambdaLiftVariantLookup base.X.baseFuncNames base.X.lambdaLiftFunctions base.X.checkedValues recorder program in
 let* typeDefs,functions=AST_to_ANF.splitDeclarations prepared in
 let symbols=CheckedAST.programSymbols prepared in
 let registries,local,functions=P.buildRegistriesForProgram recorder (CheckedAST.nextFunctionOrdinal base.X.symbols) symbols true base.X.registries.AST_to_ANF.moduleRegistry base.X.registries typeDefs functions in
 let registries={registries with AST_to_ANF.functionIds=M.fold M.add (CheckedAST.functionIds symbols) registries.AST_to_ANF.functionIds;functionNames=FunctionIdMap.fold (fun values id name->FunctionIdMap.add id name values) registries.AST_to_ANF.functionNames (CheckedAST.functionNames symbols);typeReg=M.fold M.add externalTypes registries.AST_to_ANF.typeReg;recordFieldsReg=M.fold M.add (TypeRegistries.recordFieldsRegistry externalTypes) registries.AST_to_ANF.recordFieldsReg;recordTypeParamsReg=M.fold M.add (TypeRegistries.recordTypeParamsRegistry externalTypes) registries.AST_to_ANF.recordTypeParamsReg;variantLookup=M.fold M.add externalVariants registries.AST_to_ANF.variantLookup;sumMetadata=LoweringPrimitives.mergeSumMetadata registries.AST_to_ANF.sumMetadata (LoweringPrimitives.sumMetadataFromVariantLookup externalVariants);rcSumShapeReg=M.fold M.add (TypeRegistries.rcSumShapeRegistryFromVariantLookup externalVariants) registries.AST_to_ANF.rcSumShapeReg} in
 let localReturns=P.extractReturnTypes local.AST_to_ANF.funcReg in
 let* anf,_=AST_to_ANF.convertFunctions symbols registries (ANF.VarGen 0) functions in
 let options=CompilerOptions.defaultOptions in let start=HostClock.milliseconds () in let elapsed ()=HostClock.milliseconds ()-.start in
 let* optimized,ssa,typeMap=ANFPipeline.buildAnf 0 options elapsed registries (CheckedAST.nextFunctionOrdinal symbols) ANFPipeline.stdlibInliningConfig FunctionIdMap.empty M.empty F.empty anf FunctionIdMap.empty false recorder in
 let newMap=List.map (fun (func:ANF.functionDef)->func.ANF.name,func) optimized |> M.of_list in
 let returns=X.mergeReturnTypes base.X.returnTypes localReturns in
 let* allocated,summaries=NativePipeline.lowerToAllocatedLirWithKnown stdlib.X.callGraphSummaries base.X.target 0 options elapsed recorder None None "stdlib_specializations" ssa typeMap registries None returns in
 let allLir=stdlib.X.allocatedFunctions @ allocated in
 let typeMap=ANF.TypeMap.merge stdlib.X.stdlibTypeMap typeMap in
 let anfMap=M.fold M.add newMap stdlib.X.stdlibAnfFunctions in
 let candidates=List.fold_left (fun values (func:ANF.functionDef)->M.add func.ANF.name func values) stdlib.X.stdlibAnfOptimizationCandidates anf in
 let newInline=InliningCommon.buildExternalCandidateInfoMap InliningCommon.defaultConfig anf in
 let inline=FunctionIdMap.fold (fun values id info->FunctionIdMap.add id info values) stdlib.X.stdlibInlineCandidates newInline in
 let allAnf=M.bindings anfMap |> List.map snd in
 let callGraph=DeadCodeElimination.buildCallGraph (List.map (fun (func:LIR.functionDef)->func.LIR.name,func.LIR.id) allLir |> M.of_list) allLir in
 let anfGraph=ANFDeadCodeElimination.buildCallGraph allAnf in
 let names=List.fold_left (fun names (func:ANF.functionDef)->S.add func.ANF.name names) base.X.baseFuncNames optimized in
 let lambdaTypes,lambdaVariants=LiftFunctions.prepareLambdaLiftBaseTypes registries.AST_to_ANF.typeReg registries.AST_to_ANF.variantLookup in
 let context={base with X.symbols;writtenEnvironment=Option.map (WrittenChecking.includeAllocatedFunctions symbols) base.X.writtenEnvironment;specRegistry=combined;registries;baseFuncNames=names;lambdaLiftFunctions=X.buildLambdaLiftFunctionCatalog registries names returns;lambdaLiftTypeReg=lambdaTypes;lambdaLiftVariantLookup=lambdaVariants;returnTypes=returns} |> X.includeCompiledFunctions optimized in
 Ok {stdlib with X.context;allocatedFunctions=allLir;callGraphSummaries=CompilationCacheIdentity.mergeFunctionSummaries stdlib.X.callGraphSummaries summaries;stdlibCallGraph=callGraph;stdlibAnfFunctions=anfMap;stdlibAnfOptimizationCandidates=candidates;stdlibInlineCandidates=inline;stdlibAnfCallGraph=anfGraph;stdlibTypeMap=typeMap}
