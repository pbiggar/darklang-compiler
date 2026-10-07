(* SourcePreparation.fs - Prepare checked declarations through specialization and closure lowering. *)
[@@@warning "-4"]
module C=CheckedAST
module X=CompilationContexts
module M=StringOrder.Map
module S=StringOrder.Set
module F=SpecializationIdentity.FunctionSet
module K=SpecializationIdentity.SpecSet
module B=C.BindingIdMap
module BS=ClosureAnalysis.BindingSet
let (let*)=Result.bind
(*
   Helper functions for exception-to-Result conversion (Darklang compatibility)
   Extract return types from a FuncReg (FunctionRegistry maps func name -> full type)
   This is needed because buildReturnTypeReg only includes functions in the current program,
   but we need return types for all callable functions (including stdlib)
*)
let extractReturnTypes funcReg=FunctionIdMap.toList funcReg |> List.map (fun (id,(name,typ))->match typ with AST.TFunction (_,returnType)->id,(name,returnType)|other->Crash.crash ("extractReturnTypes: Non-function type '"^StructuralFormat.semanticType other^"' found in FuncReg for '"^name^"'")) |> FunctionIdMap.ofList
let emptyRegistries moduleRegistry={AST_to_ANF.scopeContracts=FunctionIdMap.empty;inertFunctionScopes=F.empty;typeReg=M.empty;typeNames=TypeRegistries.emptyTypeNames;recordFieldsReg=M.empty;recordTypeParamsReg=M.empty;variantLookup=M.empty;sumMetadata={LoweringPrimitives.names=S.empty;cases=M.empty};rcSumShapeReg=M.empty;funcReg=FunctionIdMap.empty;functionIds=M.empty;functionNames=FunctionIdMap.empty;funcParams=M.empty;moduleRegistry;recursiveMembers=FunctionIdMap.empty}
let measure recorder name operation=let start=(Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in let result=operation () in PipelineDiagnostics.recordPassTiming recorder name ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6)-.start);result
let liftLambdasWithBase baseTypeReg baseVariantLookup baseFunctions recorder program=measure recorder "AST -> ANF Preparation: Lambda Lifting" (fun ()->LiftFunctions.liftLambdasInProgram baseTypeReg baseVariantLookup baseFunctions program)
let mergeSpecRegistries base overlay=SpecializationIdentity.SpecMap.fold SpecializationIdentity.SpecMap.add overlay base
let collectLocalSpecs genericDefs program=
 let symbols,topLevels=C.viewProgram program in
 List.map (function C.FunctionDef f when f.C.typeParams=[]->Monomorphization.collectTypeAppsFromFunc symbols f|C.ValueDef value->Monomorphization.collectTypeApps symbols (C.valueDefBody value)|C.Expression e->Monomorphization.collectTypeApps symbols e|_->K.empty) topLevels |> List.fold_left K.union K.empty |> K.filter (fun (name,_)->M.mem name genericDefs)
type monomorphizationMode=Monomorphize of SpecializationIdentity.genericFuncDefs option|ReplaceTypeApps of SpecializationIdentity.specRegistry|SpecializeLocalAndReplace of SpecializationIdentity.specRegistry
(* Import inherited checked values before specialization so their bodies cross
   every preparation boundary together with local declarations. *)
let importInheritedValues recorder inheritedValues program=
 let symbols,topLevels=C.viewProgram program in
 let currentNames=List.filter_map (function C.ValueDef value->Some value.C.name|_->None) topLevels |> S.of_list in
 let inheritedEntries=M.bindings inheritedValues |> List.filter (fun (name,_)->not (S.mem name currentNames)) in
 let inherited,symbols,compositionTicks,definitionTicks=List.fold_left (fun (collected,symbols,compositionTicks,definitionTicks) (name,(artifact:X.checkedValueArtifact))->
  let compositionStart=if Option.is_some recorder then Mtime_clock.elapsed_ns () else 0L in
  (* Checked value bodies carry canonical IDs. Their reusable
     artifacts retain only the cursor needed for later fresh binders. *)
  let symbols=C.includeBindingCursor artifact.X.bindingCursor symbols in
  let compositionTicks=if Option.is_some recorder then Int64.add compositionTicks (Int64.sub (Mtime_clock.elapsed_ns ()) compositionStart) else compositionTicks in
  let definitionStart=if Option.is_some recorder then Mtime_clock.elapsed_ns () else 0L in
  let id,symbols=C.internValue name symbols in let definition=C.ValueDef {C.id;name;typ=C.checkedType artifact.X.typ;body=artifact.X.body} in
  let definitionTicks=if Option.is_some recorder then Int64.add definitionTicks (Int64.sub (Mtime_clock.elapsed_ns ()) definitionStart) else definitionTicks in definition::collected,symbols,compositionTicks,definitionTicks) ([],symbols,0L,0L) inheritedEntries in
 let milliseconds ticks=Int64.to_float ticks*.1000./.1000000000. in
 PipelineDiagnostics.recordPassTiming recorder "AST -> ANF Value Import: Binding Cursor Composition" (milliseconds compositionTicks);
 PipelineDiagnostics.recordPassTiming recorder "AST -> ANF Value Import: Definition Construction" (milliseconds definitionTicks);
 C.programFromCheckedParts (symbols,List.rev inherited @ topLevels)
(* Materialize checked module values as one lexical binding per execution
   scope. This gives every reference ordinary value semantics through the
   existing ANF ownership pipeline and leaves no value-only lowering cases. *)
let materializeProgramValues program=
 let symbols,topLevels=C.viewProgram program in
 let currentValues=List.filter_map (function C.ValueDef value->Some (value.C.name,(value.C.id,value.C.typ,value.C.body))|_->None) topLevels in
 let rawBindings=List.map (fun (name,(id,_,body))->name,id,body) currentValues in
 (* Checked top-level value references already carry canonical BindingIds.
    The old name-based repair walked every body again, including programs
    with no references requiring repair. *)
 let bindings=rawBindings in let valueIds=List.map (fun (_,id,_)->id) bindings |> BS.of_list in
 let dependencies=List.map (fun (_,id,body)->id,BS.inter valueIds (ClosureAnalysis.freeVars body BS.empty)) bindings |> List.to_seq |> B.of_seq in
 (* Outlining a literal replaces one cheap instruction with a call and can
    regress runtime code. Share bodies that have actual lowering work. *)
 let cheapValueIds=List.filter_map (fun (_,id,body)->match body with
  |C.UnitLiteral|C.Int64Literal _|C.Int128Literal _|C.Int8Literal _|C.Int16Literal _|C.Int32Literal _|C.UInt8Literal _|C.UInt16Literal _|C.UInt32Literal _|C.UInt64Literal _|C.UInt128Literal _|C.BigIntLiteral _|C.BoolLiteral _|C.StringLiteral _|C.BlobLiteral _|C.CharLiteral _|C.FloatLiteral _|C.Local _|C.ListLiteral []|C.DictLiteral (_,_,[])|C.Constructor (_,[])->Some id|_->None) bindings |> BS.of_list in
 let valueTypes=List.map (fun (_, (id,typ,_))->id,typ) currentValues |> List.to_seq |> B.of_seq in
 let helperName name="__dark_value_materializer_"^name in
 let symbols=List.fold_left (fun symbols (name,(id,_,_))->if not (BS.mem id cheapValueIds) then snd (C.internFunction (helperName name) symbols) else symbols) symbols currentValues in
 let helperIds=List.filter_map (fun (name,id,_)->Option.map (fun functionId->id,functionId) (C.tryFindFunctionId (helperName name) symbols)) bindings |> List.to_seq |> B.of_seq in
 let bindingOrder=List.mapi (fun index (_,id,_)->id,index) bindings |> List.to_seq |> B.of_seq in
 let dependencyArgs=B.map (fun required->BS.elements required |> List.stable_sort (fun a b->let ordinal id=match B.find_opt id bindingOrder with Some index->index|None->Crash.crash "Missing checked value dependency" in Int.compare (ordinal a) (ordinal b))) dependencies in
 let wrap excluded body=
  let direct=BS.diff (BS.inter valueIds (ClosureAnalysis.freeVars body BS.empty)) excluded in
  if BS.is_empty direct then BS.empty,body else
  let rec required pending needed=match pending with []->needed|id::rest when BS.mem id needed->required rest needed|id::rest->let next=Option.value ~default:BS.empty (B.find_opt id dependencies) |> fun values->BS.diff values excluded |> BS.elements in required (next @ rest) (BS.add id needed) in
  let needed=required (BS.elements direct) BS.empty in let selected=List.filter (fun (_,id,_)->BS.mem id needed) bindings in
  let rec orderByDependencies ordered=function []->ordered|remaining->let remainingNames=List.map (fun (_,id,_)->id) remaining |> BS.of_list in let ready=List.filter (fun (_,id,_)->BS.is_empty (BS.inter (BS.remove id remainingNames) (Option.value ~default:BS.empty (B.find_opt id dependencies)))) remaining in
   match ready with []->Crash.crash "Checked top-level values contain a cyclic materialization dependency"|_->let readyNames=List.map (fun (_,id,_)->id) ready |> BS.of_list in let pending=List.filter (fun (_,id,_)->not (BS.mem id readyNames)) remaining in orderByDependencies (ordered @ ready) pending in
  let ordered=orderByDependencies [] selected in
  let materialized=List.fold_right (fun (_,id,value) result->let initialValue=if BS.mem id cheapValueIds then value else let arguments=Option.value ~default:[] (B.find_opt id dependencyArgs) |> List.map (fun id->C.Local id) in let arguments=match arguments with []->NonEmptyList.singleton C.UnitLiteral|_->NonEmptyList.fromList arguments in let helperId=match B.find_opt id helperIds with Some id->id|None->Crash.crash "Missing checked value materializer" in C.Call (helperId,arguments) in C.Let (C.LPVariable id,initialValue,result)) ordered body in needed,materialized in
 let materialized,usedValues=List.fold_left (fun (items,used) item->match item with
 |C.ValueDef _->items,used
 |C.FunctionDef definition->let parameters=NonEmptyList.toList definition.C.params |> List.map fst |> BS.of_list in let needed,body=wrap parameters definition.C.body in C.FunctionDef {definition with C.body}::items,BS.union used needed
 |C.Expression expr->let needed,body=wrap BS.empty expr in C.Expression body::items,BS.union used needed
 |C.TypeDef _->item::items,used) ([],BS.empty) topLevels in
 (* A helper owns each checked value body once. Calls still bind its result
    in each execution scope, in dependency order, so effects are not cached. *)
 let helpers=List.filter_map (fun (name,(id,typ,body))->if not (BS.mem id usedValues) || BS.mem id cheapValueIds then None else
  let parameters=Option.value ~default:[] (B.find_opt id dependencyArgs) |> List.map (fun id->let typ=match B.find_opt id valueTypes with Some typ->typ|None->Crash.crash "Missing checked value type" in id,C.semanticType typ) in
  let parameters=match parameters with []->NonEmptyList.singleton (AST.topLevelValueId (name^"#unit"),AST.TUnit)|_->NonEmptyList.fromList parameters in
  let functionId=match B.find_opt id helperIds with Some id->id|None->Crash.crash "Missing checked value materializer" in
  Some (C.FunctionDef {C.id=functionId;name=helperName name;typeParams=[];params=C.checkedParams parameters;returnType=typ;body;recursion=None})) currentValues in
 C.programFromCheckedParts (symbols,List.rev materialized @ helpers)
let prepareProgramForAnf mode baseTypeReg baseVariantLookup baseFuncNames baseFunctions inheritedValues recorder program=
 let program=measure recorder "AST -> ANF Preparation: Value Import" (fun ()->importInheritedValues recorder inheritedValues program) in
 let monomorphizedResult=measure recorder "AST -> ANF Preparation: Monomorphization" (fun ()->match mode with
 |Monomorphize None->Ok (PrepareFunctions.monomorphize program)|Monomorphize (Some defs)->Ok (PrepareFunctions.monomorphizeWithExternalDefs defs program)
 |ReplaceTypeApps registry->Monomorphization.replaceTypeAppsInProgramWithRegistry registry program
 |SpecializeLocalAndReplace registry->let genericDefs=SpecializationIdentity.extractGenericFuncDefs program in if M.is_empty genericDefs then Monomorphization.replaceTypeAppsInProgramWithRegistry registry program else
  let localSpecs=collectLocalSpecs genericDefs program in let specialization=Monomorphization.specializeFromSpecs (C.programSymbols program) genericDefs localSpecs in let combined=mergeSpecRegistries registry specialization.SpecializationIdentity.specRegistry in
  let _,items=C.viewProgram program in let symbols,specializedFunctions=SpecializationIdentity.importSpecializedFunctions specialization.SpecializationIdentity.symbols specialization.SpecializationIdentity.specializedFuncs in
  let topLevels=List.map (fun func->C.FunctionDef func) specializedFunctions in Monomorphization.replaceTypeAppsInProgramWithRegistry combined (C.programFromCheckedParts (symbols,topLevels @ items))) in
 let* monomorphized=monomorphizedResult in
 let monomorphized=measure recorder "AST -> ANF Preparation: Value Materialization" (fun ()->materializeProgramValues monomorphized) in
 let needsLowering=measure recorder "AST -> ANF Preparation: Lambda Analysis" (fun ()->let _,topLevels=C.viewProgram monomorphized in let localNames=List.filter_map (function C.FunctionDef f->Some f.C.name|_->None) topLevels |> S.of_list in Monomorphization.programNeedsLambdaLowering (S.union baseFuncNames localNames) monomorphized) in
 if needsLowering then measure recorder "AST -> ANF Preparation: Lambda Lowering" (fun ()->let inlined=measure recorder "AST -> ANF Preparation: Lambda Inlining" (fun ()->InlineLambdas.inlineLambdasInProgram monomorphized) in liftLambdasWithBase baseTypeReg baseVariantLookup baseFunctions recorder inlined) else Ok monomorphized
let buildRegistriesForProgram recorder existingFunctionOrdinal symbols baseProvidesModuleFunctionParams moduleRegistry baseRegistries typeDefs functions=
 let measure name operation=match recorder with None->operation ()|Some _->measure recorder name operation in
 let aliasReg,resolvedFunctions=measure "AST -> ANF Registry: Alias Resolution" (fun ()->let aliasReg=AST_to_ANF.buildAliasRegistry typeDefs in aliasReg,AST_to_ANF.resolveAliasesInFunctions aliasReg functions) in
 let phaseRecorder=Option.map (fun recorder->fun name elapsed->recorder {CompilerOptions.pass=name;elapsed=(Int64.of_float (elapsed *. 1e6))}) recorder in
 let localRegistries=measure "AST -> ANF Registry: Local Construction" (fun ()->if baseProvidesModuleFunctionParams then AST_to_ANF.buildOverlayRegistriesWithTrace phaseRecorder symbols moduleRegistry typeDefs aliasReg resolvedFunctions else AST_to_ANF.buildRegistriesWithTrace phaseRecorder symbols moduleRegistry typeDefs aliasReg resolvedFunctions) in
 let merged=measure "AST -> ANF Registry: Base Overlay Merge" (fun ()->AST_to_ANF.mergeRegistriesWithTrace phaseRecorder baseRegistries localRegistries) in
 let merged=List.fold_left (fun (registries:AST_to_ANF.registries) (id,name)->{registries with AST_to_ANF.functionIds=M.add name id registries.AST_to_ANF.functionIds;functionNames=FunctionIdMap.add id name registries.AST_to_ANF.functionNames}) merged (C.allocatedFunctionNamesSince existingFunctionOrdinal symbols) in merged,localRegistries,resolvedFunctions
type declarationConversion={symbols:C.symbols;functions:ANF.functionDef list;registries:AST_to_ANF.registries;localReturnTypes:(string*AST.semanticType) FunctionIdMap.t}
let splitDeclarations program=let _,topLevels=C.viewProgram program in let expressions=List.filter_map (function C.Expression expr->Some expr|_->None) topLevels in if expressions<>[] then Error ("Declaration-only program must not contain entry expressions; found "^string_of_int (List.length expressions)) else Ok (List.filter_map (function C.TypeDef (_,definition)->Some (C.semanticTypeDef definition)|_->None) topLevels,List.filter_map (function C.FunctionDef definition->Some definition|_->None) topLevels)
let convertTypedDeclarationsWithTrace recorder baseContext mode typedProgram=
 let typedProgram=match baseContext with None->typedProgram|Some (context:X.pipelineContext)->let sourceSymbols=C.programSymbols typedProgram in let symbols,tops=C.composeTopLevels sourceSymbols context.X.symbols (C.programTopLevels typedProgram) in C.programFromCheckedParts (symbols,tops) in
 let moduleRegistry=match baseContext with Some context->context.X.registries.AST_to_ANF.moduleRegistry|None->DarkStdlib.buildModuleRegistry () in
 let baseRegistries=match baseContext with Some context->context.X.registries|None->emptyRegistries moduleRegistry in
 let symbols=C.programSymbols typedProgram in
 let baseRegistries={baseRegistries with AST_to_ANF.functionIds=M.fold M.add (C.functionIds symbols) baseRegistries.AST_to_ANF.functionIds;functionNames=FunctionIdMap.fold (fun names id name->FunctionIdMap.add id name names) baseRegistries.AST_to_ANF.functionNames (C.functionNames symbols)} in
 let baseFuncNames=match baseContext with Some context->context.X.baseFuncNames|None->X.buildBaseFuncNames baseRegistries in
 let baseFunctions=match baseContext with Some context->context.X.lambdaLiftFunctions|None->X.buildLambdaLiftFunctionCatalog baseRegistries baseFuncNames (extractReturnTypes baseRegistries.AST_to_ANF.funcReg) in
 let baseTypeReg,baseVariantLookup=match baseContext with Some context->context.X.lambdaLiftTypeReg,context.X.lambdaLiftVariantLookup|None->LiftFunctions.prepareLambdaLiftBaseTypes baseRegistries.AST_to_ANF.typeReg baseRegistries.AST_to_ANF.variantLookup in
 let inheritedValues=match baseContext with Some context->context.X.checkedValues|None->M.empty in
 let* liftedProgram=prepareProgramForAnf mode baseTypeReg baseVariantLookup baseFuncNames baseFunctions inheritedValues recorder typedProgram in
 let* typeDefs,functions=splitDeclarations liftedProgram in
 let ordinal=match baseContext with Some context->C.nextFunctionOrdinal context.X.symbols|None->0L in
 let registries,localRegistries,resolvedFunctions=buildRegistriesForProgram recorder ordinal (C.programSymbols liftedProgram) (Option.is_some baseContext) moduleRegistry baseRegistries typeDefs functions in
 let ownershipTiming name elapsed=PipelineDiagnostics.recordPassTiming recorder name elapsed in
 let* converted=AST_to_ANF.convertFunctionsWithOwnershipWithTrace (Some ownershipTiming) (C.programSymbols liftedProgram) registries (ANF.VarGen 0) resolvedFunctions in
 let symbols=List.fold_left (fun symbols (func:ANF.functionDef)->C.registerGeneratedFunction func.ANF.name func.ANF.id symbols) (C.programSymbols liftedProgram) converted.AST_to_ANF.functions in
 let registries={registries with AST_to_ANF.functionIds=List.fold_left (fun ids (func:ANF.functionDef)->M.add func.ANF.name func.ANF.id ids) registries.AST_to_ANF.functionIds converted.AST_to_ANF.functions;functionNames=List.fold_left (fun names (func:ANF.functionDef)->FunctionIdMap.add func.ANF.id func.ANF.name names) registries.AST_to_ANF.functionNames converted.AST_to_ANF.functions;funcReg=AST_to_ANF.extendFunctionRegistryWithConverted registries.AST_to_ANF.funcReg converted.AST_to_ANF.functions;funcParams=List.fold_left (fun parameters (func:ANF.functionDef)->let args=List.mapi (fun index (param:ANF.typedParam)->"arg"^string_of_int index,param.ANF.typ) func.ANF.typedParams in M.add func.ANF.name args parameters) registries.AST_to_ANF.funcParams converted.AST_to_ANF.functions} in
 Ok {symbols;functions=converted.AST_to_ANF.functions;registries;localReturnTypes=extractReturnTypes localRegistries.AST_to_ANF.funcReg}
let convertTypedDeclarations baseContext mode program=convertTypedDeclarationsWithTrace None baseContext mode program
let convertTypedProgramToConversionResult moduleRegistry program=
 let baseRegistries=emptyRegistries moduleRegistry in let baseFuncNames=X.buildBaseFuncNames baseRegistries in let baseFunctions=X.buildLambdaLiftFunctionCatalog baseRegistries baseFuncNames FunctionIdMap.empty in
 let* liftedProgram=prepareProgramForAnf (Monomorphize None) M.empty M.empty baseFuncNames baseFunctions M.empty None program in
 let* typeDefs,functions,expr=AST_to_ANF.splitTopLevels liftedProgram in
 let registries,_,resolvedFunctions=buildRegistriesForProgram None 0L (C.programSymbols liftedProgram) false moduleRegistry baseRegistries typeDefs functions in
 let* converted=AST_to_ANF.convertFunctionsWithOwnership (C.programSymbols liftedProgram) registries (ANF.VarGen 0) resolvedFunctions in
 let* anfExpr,_=AST_to_ANF.convertExprToAnf registries converted.AST_to_ANF.varGen expr in
 Ok (ANFPipeline.buildConversionResult (ANF.Program (converted.AST_to_ANF.functions,anfExpr)) registries converted.AST_to_ANF.ownershipContracts)
let convertTypedProgramToUserOnlyWithMode (baseContext:X.pipelineContext) mode (typeCheckEnv:Types.typeCheckEnv) session recorder typedProgram=
 let sourceSymbols=C.programSymbols typedProgram in let symbols,topLevels=measure recorder "AST -> ANF Symbol Import" (fun ()->C.composeDeclaredTopLevels sourceSymbols baseContext.X.symbols (C.programTopLevels typedProgram)) in
 let symbols,topLevels=CheckedMaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums symbols typeCheckEnv.Types.aliasReg typeCheckEnv.Types.indexedTypeReg typeCheckEnv.Types.variantLookup typeCheckEnv.Types.indexedSumTypeReg topLevels in let typedProgram=C.programFromCheckedParts (symbols,topLevels) in
 (* Late AOT plans (notably Json) may introduce concrete calls to generic
    stdlib functions after the suite preamble registry was built. Materialize
    just those missing specializations into the user compilation unit. *)
 let typedProgram,mode,nonInlineableFunctionNames=measure recorder "AST -> ANF Dependency Planning" (fun ()->
  let addMissing baseRegistry rebuildMode=
   (* A local generic function's body may instantiate a stdlib generic
      only once the local one is itself specialized (a fold over
      Parser<a> inside choice<'a>, called as choice<String>), so
      specialize the local generics first and request what their
      specialized bodies reach: specializeFromSpecs reports those as
      ExternalSpecs. The later local specialization repeats this work
      on the same input and lands on the same names. *)
   let localGenericDefs=SpecializationIdentity.extractGenericFuncDefs typedProgram in
   let reachedThroughLocalGenerics=if M.is_empty localGenericDefs then K.empty else let localSpecs=collectLocalSpecs localGenericDefs typedProgram in (Monomorphization.specializeFromSpecs (C.programSymbols typedProgram) localGenericDefs localSpecs).SpecializationIdentity.externalSpecs |> K.filter (fun (name,_)->M.mem name baseContext.X.genericFuncDefs) in
   let requested=K.union (collectLocalSpecs baseContext.X.genericFuncDefs typedProgram) reachedThroughLocalGenerics in
   let initialSymbols,items=C.viewProgram typedProgram in let localNames=List.filter_map (function C.FunctionDef fn->Some fn.C.name|_->None) items |> S.of_list in
   let isKnown localNames name=S.mem name localNames || S.mem name baseContext.X.baseFuncNames in
   let rec materialize symbols registry localNames pending accFunctions=
    let missing=K.filter (fun key->not (SpecializationIdentity.SpecMap.mem key registry)) pending in
    if K.is_empty missing then registry,accFunctions,symbols else
    let specialization=Monomorphization.specializeFromSpecs symbols baseContext.X.genericFuncDefs missing in let combined=mergeSpecRegistries registry specialization.SpecializationIdentity.specRegistry in
    let symbols,specializedFunctions=SpecializationIdentity.importSpecializedFunctions specialization.SpecializationIdentity.symbols specialization.SpecializationIdentity.specializedFuncs in
    let specializedTopLevels=List.filter (fun (fn:C.functionDef)->not (isKnown localNames fn.C.name)) specializedFunctions |> List.map (fun fn->C.FunctionDef fn) in
    let symbols,materializedTopLevels=CheckedMaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums symbols typeCheckEnv.Types.aliasReg typeCheckEnv.Types.indexedTypeReg typeCheckEnv.Types.variantLookup typeCheckEnv.Types.indexedSumTypeReg specializedTopLevels in
    let newFunctions=List.filter_map (function C.FunctionDef fn when not (isKnown localNames fn.C.name)->Some fn|_->None) materializedTopLevels in
    let nextNames=List.fold_left (fun names (fn:C.functionDef)->S.add fn.C.name names) localNames newFunctions in let nextSpecs=C.programFromCheckedParts (symbols,materializedTopLevels) |> collectLocalSpecs baseContext.X.genericFuncDefs in
    materialize symbols combined nextNames nextSpecs (accFunctions @ newFunctions) in
   let combined,newFunctions,symbols=materialize initialSymbols baseRegistry localNames requested [] in
   let program=C.programFromCheckedParts (symbols,List.map (fun fn->C.FunctionDef fn) newFunctions @ items) in let names=List.map (fun (fn:C.functionDef)->fn.C.id) newFunctions |> F.of_list in program,rebuildMode combined,names in
  match mode with ReplaceTypeApps registry->addMissing registry (fun registry->ReplaceTypeApps registry)|SpecializeLocalAndReplace registry->addMissing registry (fun registry->SpecializeLocalAndReplace registry)|Monomorphize _->typedProgram,mode,F.empty) in
 let typedProgram=ValueRendering.rewriteDictionaryKeyRenderers typeCheckEnv.Types.indexedTypeReg typeCheckEnv.Types.indexedSumTypeReg typedProgram in
 let* liftedProgram=measure recorder "AST -> ANF Program Preparation" (fun ()->prepareProgramForAnf mode baseContext.X.lambdaLiftTypeReg baseContext.X.lambdaLiftVariantLookup baseContext.X.baseFuncNames baseContext.X.lambdaLiftFunctions baseContext.X.checkedValues recorder typedProgram) in
 let* symbols,registries,localRegistries,resolvedFunctions,localReturnTypes,expr=measure recorder "AST -> ANF Registry Construction" (fun ()->
  let* typeDefs,functions,expr=AST_to_ANF.splitTopLevels liftedProgram in
  let registries,localRegistries,resolvedFunctions=buildRegistriesForProgram recorder (C.nextFunctionOrdinal baseContext.X.symbols) (C.programSymbols liftedProgram) true baseContext.X.registries.AST_to_ANF.moduleRegistry baseContext.X.registries typeDefs functions in
  Ok (C.programSymbols liftedProgram,registries,localRegistries,resolvedFunctions,extractReturnTypes localRegistries.AST_to_ANF.funcReg,expr)) in
 let conversionKey={CompilationCacheIdentity.functions=resolvedFunctions;localRegistries;nonInlineableFunctionNames} in
 let convert ()=measure recorder "AST -> ANF Dependency Conversion" (fun ()->let ownershipTiming name elapsed=PipelineDiagnostics.recordPassTiming recorder name elapsed in AST_to_ANF.convertFunctionsWithOwnershipWithTrace (Some ownershipTiming) symbols registries (ANF.VarGen 0) resolvedFunctions) in
 let* converted,dependencyIdentity=measure recorder "AST -> ANF Dependency Lookup" (fun ()->match session with Some (current:CompilationSession.compilationSession)->current#convertAnfDependencies (Obj.repr baseContext) conversionKey convert|None->Result.map (fun converted->converted,Obj.repr (ref ())) (convert ())) in
 let* anfExpr,_=measure recorder "AST -> ANF Expression Conversion" (fun ()->AST_to_ANF.convertExprToAnf registries converted.AST_to_ANF.varGen expr) in
 let convertedFuncReg=AST_to_ANF.extendFunctionRegistryWithConverted registries.AST_to_ANF.funcReg converted.AST_to_ANF.functions in
 let convertedReturnTypes=List.fold_left (fun returns (func:ANF.functionDef)->FunctionIdMap.add func.ANF.id (func.ANF.name,func.ANF.returnType) returns) localReturnTypes converted.AST_to_ANF.functions in
 let nonInlineableFunctionNames=F.union nonInlineableFunctionNames (FunctionIdMap.keys converted.AST_to_ANF.ownershipContracts |> List.of_seq |> F.of_list) in
 Ok ({AST_to_ANF.symbols;userFunctions=converted.AST_to_ANF.functions;ownershipContracts=converted.AST_to_ANF.ownershipContracts;scopeContracts=registries.AST_to_ANF.scopeContracts;inertFunctionScopes=registries.AST_to_ANF.inertFunctionScopes;nonInlineableFunctionNames;mainExpr=anfExpr;typeReg=registries.AST_to_ANF.typeReg;typeNames=registries.AST_to_ANF.typeNames;recordFieldsReg=registries.AST_to_ANF.recordFieldsReg;recordTypeParamsReg=registries.AST_to_ANF.recordTypeParamsReg;variantLookup=registries.AST_to_ANF.variantLookup;sumMetadata=registries.AST_to_ANF.sumMetadata;localRecordFieldsReg=localRegistries.AST_to_ANF.recordFieldsReg;localVariantLookup=localRegistries.AST_to_ANF.variantLookup;rcSumShapeReg=registries.AST_to_ANF.rcSumShapeReg;funcReg=convertedFuncReg;functionIds=registries.AST_to_ANF.functionIds;functionNames=registries.AST_to_ANF.functionNames;localReturnTypes=convertedReturnTypes;funcParams=registries.AST_to_ANF.funcParams;moduleRegistry=registries.AST_to_ANF.moduleRegistry;recursiveMembers=registries.AST_to_ANF.recursiveMembers},dependencyIdentity)
let convertTypedProgramToUserOnly context program=Result.map fst (convertTypedProgramToUserOnlyWithMode context (Monomorphize (Some context.X.genericFuncDefs)) context.X.typeCheckEnv None None program)
let convertTypedProgramToUserOnlyWithTrace context recorder program=Result.map fst (convertTypedProgramToUserOnlyWithMode context (Monomorphize (Some context.X.genericFuncDefs)) context.X.typeCheckEnv None recorder program)
(* Try to delete a file, ignoring any errors. *)
let tryDeleteFile path=try Sys.remove path with _->()
(* Try to start a process, returning Result instead of throwing. *)
type processStartInfo={fileName:string;arguments:string list;environment:(string*string) list;stdin:Unix.file_descr;stdout:Unix.file_descr;stderr:Unix.file_descr}
let tryStartProcess info=try
 let env=Array.to_list (Unix.environment ()) |> List.filter_map (fun entry->match String.index_opt entry '=' with Some index->Some (String.sub entry 0 index,String.sub entry (index+1) (String.length entry-index-1))|None->None) |> M.of_list in
 let env=List.fold_left (fun env (name,value)->M.add name value env) env info.environment |> M.bindings |> List.map (fun (name,value)->name^"="^value) |> Array.of_list in
 Ok (Unix.create_process_env info.fileName (Array.of_list (info.fileName::info.arguments)) env info.stdin info.stdout info.stderr)
 with Unix.Unix_error (error,_,_)->Error ("An error occurred trying to start process '"^info.fileName^"' with working directory '"^Sys.getcwd ()^"'. "^Unix.error_message error)|Invalid_argument message|Failure message|Sys_error message->Error message
