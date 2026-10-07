(* CompilationContexts.ml - Define stdlib, preamble, and user-compilation interfaces. *)
[@@@warning "-30"]
module M=StringOrder.Map
module S=StringOrder.Set
module F=SpecializationIdentity.FunctionSet
let buildBaseFuncNames (registries:AST_to_ANF.registries)=M.fold (fun name _ acc->S.add name acc) registries.AST_to_ANF.funcParams S.empty
let buildLambdaLiftFunctionCatalog (registries:AST_to_ANF.registries) baseFuncNames returnTypes=
 let functionId name=match M.find_opt name registries.AST_to_ANF.functionIds with Some id->id|None->Crash.crash ("Lambda-lift function '"^name^"' has no allocated identity") in
 let parameters=M.bindings registries.AST_to_ANF.funcParams |> List.map (fun (name,parameters)->functionId name,List.map snd parameters) |> FunctionIdMap.ofList in
 let parameters=M.fold (fun name (moduleFunc:AST.moduleFunc) current->FunctionIdMap.add (functionId name) moduleFunc.AST.paramTypes current) registries.AST_to_ANF.moduleRegistry parameters in
 let parameters=S.fold (fun name current->let id=functionId name in if FunctionIdMap.containsKey id current then current else FunctionIdMap.add id [] current) baseFuncNames parameters in
 let genericDefs=M.bindings registries.AST_to_ANF.moduleRegistry |> List.filter_map (fun (name,(moduleFunc:AST.moduleFunc))->if moduleFunc.AST.typeParams=[] then None else Some (functionId name,(moduleFunc.AST.typeParams,moduleFunc.AST.returnType))) |> FunctionIdMap.ofList in
 let returnTypes=M.fold (fun name (moduleFunc:AST.moduleFunc) current->FunctionIdMap.add (functionId name) moduleFunc.AST.returnType current) registries.AST_to_ANF.moduleRegistry (FunctionIdMap.map (fun _ (_,typ)->typ) returnTypes) in
 {LiftFunctions.params=parameters;returnTypes;genericDefs}
let mergeReturnTypes baseReturnTypes overlayReturnTypes=FunctionIdMap.fold (fun acc key value->FunctionIdMap.add key value acc) baseReturnTypes overlayReturnTypes
let packageCatalogFunctionNames=S.of_list ["Builtin.pmFindValuesByValueType";"Builtin.pmGetLocationsByValue";"Builtin.pmEvaluateValue"]
(* Generic functions whose call graph can reach a package-catalog intrinsic.
   This lets ordinary programs skip catalog specialization without changing
   the behavior of generic wrappers around the catalog API. *)
let buildPackageCatalogGenericCallers (genericFuncDefs:SpecializationIdentity.genericFuncDefs)=
 let callsByFunction=M.map (fun (definition:SpecializationIdentity.genericFunctionArtifact)->F.elements definition.SpecializationIdentity.directDependencies |> List.filter_map (fun id->CheckedAST.functionName id definition.SpecializationIdentity.symbols) |> S.of_list) genericFuncDefs in
 let rec findFixedPoint callers=let targets=S.union packageCatalogFunctionNames callers in let next=M.fold (fun name calls found->if S.exists (fun called->S.mem called targets) calls then S.add name found else found) callsByFunction callers in if S.cardinal next=S.cardinal callers then callers else findFixedPoint next in
 findFixedPoint S.empty
(* Shared compilation context used across pipeline steps *)
type checkedValueArtifact={bindingCursor:int;typ:AST.semanticType;body:CheckedAST.expr}
let checkedValueArtifacts program=let bindingCursor=CheckedAST.bindingCursor (CheckedAST.programSymbols program) in M.map (fun (typ,body)->{bindingCursor;typ;body}) (CheckedAST.programValues program)
type pipelineContext={symbols:CheckedAST.symbols;target:Platform.target;typeCheckEnv:Types.typeCheckEnv;writtenEnvironment:WrittenChecking.environment option;checkedValues:checkedValueArtifact M.t;genericFuncDefs:SpecializationIdentity.genericFuncDefs;specRegistry:SpecializationIdentity.specRegistry;registries:AST_to_ANF.registries;baseFuncNames:S.t;lambdaLiftFunctions:LiftFunctions.functionCatalog;lambdaLiftTypeReg:TypeRegistries.typeRegistry;lambdaLiftVariantLookup:LoweringPrimitives.variantLookup;projectedMirRegistries:MIR.variantRegistry*MIR.recordRegistry;returnTypes:(string*AST.semanticType) FunctionIdMap.t;packageCatalogGenericCallers:S.t}
let includeCompiledFunctions (functions:ANF.functionDef list) (context:pipelineContext)=
 let symbols=List.fold_left (fun symbols (func:ANF.functionDef)->CheckedAST.registerGeneratedFunction func.ANF.name func.ANF.id symbols) context.symbols functions in
 let functionIds,functionNames,returnTypes,baseFuncNames=List.fold_left (fun (ids,names,returns,baseNames) (func:ANF.functionDef)->M.add func.ANF.name func.ANF.id ids,FunctionIdMap.add func.ANF.id func.ANF.name names,FunctionIdMap.add func.ANF.id (func.ANF.name,func.ANF.returnType) returns,S.add func.ANF.name baseNames) (context.registries.AST_to_ANF.functionIds,context.registries.AST_to_ANF.functionNames,context.returnTypes,context.baseFuncNames) functions in
 let registries={context.registries with AST_to_ANF.functionIds;functionNames;funcReg=AST_to_ANF.extendFunctionRegistryWithConverted context.registries.AST_to_ANF.funcReg functions} in
 let lambdaLiftFunctions=
  (* Registries for declared signatures are unchanged here. Retain
     their parameter and generic metadata; generated names previously
     entered the base-name catalog with an empty parameter list. *)
  List.fold_left (fun (catalog:LiftFunctions.functionCatalog) (func:ANF.functionDef)->let parameters=if FunctionIdMap.containsKey func.ANF.id catalog.LiftFunctions.params then catalog.LiftFunctions.params else FunctionIdMap.add func.ANF.id [] catalog.LiftFunctions.params in let returnType=match M.find_opt func.ANF.name registries.AST_to_ANF.moduleRegistry with Some definition->definition.AST.returnType|None->func.ANF.returnType in {catalog with LiftFunctions.params=parameters;returnTypes=FunctionIdMap.add func.ANF.id returnType catalog.LiftFunctions.returnTypes}) context.lambdaLiftFunctions functions in
 {context with symbols;typeCheckEnv={context.typeCheckEnv with Types.functionCatalog=CheckedAST.functionCatalog symbols};writtenEnvironment=Option.map (WrittenChecking.includeAllocatedFunctions symbols) context.writtenEnvironment;registries;baseFuncNames;lambdaLiftFunctions;returnTypes}
let buildContext target symbols (typeCheckEnv:Types.typeCheckEnv) checkedValues genericFuncDefs specRegistry (registries:AST_to_ANF.registries) baseFuncNames returnTypes=
 let lambdaLiftTypeReg,lambdaLiftVariantLookup=LiftFunctions.prepareLambdaLiftBaseTypes registries.AST_to_ANF.typeReg registries.AST_to_ANF.variantLookup in
 {symbols;target;typeCheckEnv={typeCheckEnv with Types.functionCatalog=CheckedAST.functionCatalog symbols};writtenEnvironment=None;checkedValues;genericFuncDefs;specRegistry;registries;baseFuncNames;lambdaLiftFunctions=buildLambdaLiftFunctionCatalog registries baseFuncNames returnTypes;lambdaLiftTypeReg;lambdaLiftVariantLookup;projectedMirRegistries=(ANF_to_MIR.buildVariantRegistry registries.AST_to_ANF.variantLookup,ANF_to_MIR.buildRecordRegistry registries.AST_to_ANF.recordFieldsReg);returnTypes;packageCatalogGenericCallers=buildPackageCatalogGenericCallers genericFuncDefs}
(* Compiled preamble context - extends stdlib for a test file
   Preamble functions are compiled ONCE per file, then reused for all tests in that file *)
(*
   Extended compilation context (stdlib + preamble)
   Preamble's ANF functions (after mono, inline, lift, ANF, RC, TCO)
   Type map from RC insertion (merged with stdlib's TypeMap)
   Preamble's symbolic LIR functions after register allocation
   Direct-call summary computed once with the reusable preamble unit.
*)
type preambleContext={context:pipelineContext;anfFunctions:ANF.functionDef list;typeMap:ANF.typeMap;symbolicFunctions:LIR.functionDef list;callGraphSummaries:CompilationCacheIdentity.functionSummary FunctionIdMap.t;symbolicCallGraph:F.t FunctionIdMap.t}
(* Parsed and typechecked preamble analysis for suite-level specialization *)
type preambleAnalysis={typedAST:CheckedAST.program;typeCheckEnv:Types.typeCheckEnv;writtenEnvironment:WrittenChecking.environment option;genericFuncDefs:SpecializationIdentity.genericFuncDefs}
(* Result of compiling stdlib - can be reused across compilations *)
(*
   Type-checked stdlib with inferred types
   Shared compilation context (typecheck env + registries)
   Pre-allocated stdlib functions (physical registers assigned, ready for merge)
   Call graph for dead code elimination (which stdlib funcs call which other funcs)
   Stdlib ANF functions indexed by name (for coverage analysis)
   Pre-reference-count bodies available to optimizations that introduce
   calls to already-monomorphized stdlib helpers.
   Pre-reference-count stdlib ANF functions available as user inlining candidates
   Call graph at ANF level (for coverage analysis reachability)
   TypeMap from RC insertion (needed for getReachableStdlibFunctions)
*)
type stdlibResult={typedAST:CheckedAST.program;context:pipelineContext;allocatedFunctions:LIR.functionDef list;callGraphSummaries:CompilationCacheIdentity.functionSummary FunctionIdMap.t;stdlibCallGraph:F.t FunctionIdMap.t;stdlibAnfFunctions:ANF.functionDef M.t;stdlibAnfOptimizationCandidates:ANF.functionDef M.t;stdlibInlineCandidates:InliningCommon.functionInfo FunctionIdMap.t;stdlibAnfCallGraph:F.t FunctionIdMap.t;stdlibTypeMap:ANF.typeMap}
(* Context for compiling user code *)
type compileContext=StdlibOnly of stdlibResult|StdlibWithPreamble of stdlibResult*preambleContext
(* Recursive custom-type identity retained only at the immutable package-value
   catalog boundary. Runtime type arguments use the same exact nested custom
   identities as ValueSearch's ValueType query. *)
type packageCustomType={hash:string;typeArguments:packageCustomType list}
(* A branch-visible package location. Input order is the interpreter package
   manager's branch-prioritized order and remains observable during selection. *)
type catalogPackageLocation={visibleInBranches:string list;owner:string;modules:string list;name:string}
(* Evaluation availability is explicit; missing and failed package evaluation
   both become None at the public primitive, but are distinct catalog states. *)
type packageValueEvaluatorState=Available of AST.expr|Unavailable|EvaluationFailure
(* The evaluator's concrete result type is checked before its expression can
   cross into a monomorphized ValueSearch caller. *)
type typedPackageValueEvaluator={resultType:AST.semanticType;state:packageValueEvaluatorState}
type packageValueCatalogEntry={valueHash:string;runtimeType:packageCustomType;locations:catalogPackageLocation list;evaluator:typedPackageValueEvaluator}
(* Explicit AOT package snapshot. Unlike the interpreter package manager this
   value is immutable and contains no database or live branch traversal. *)
type packageValueCatalog=PackageValueCatalog of packageValueCatalogEntry list
let emptyPackageValueCatalog=PackageValueCatalog []
(* One independently parsed source unit. Ordering is caller-owned and is
   preserved when declaration overlays are composed. *)
type sourceUnit={name:string;purpose:NameSyntax.SourceUnitPurpose.t;source:string}
(* Request for compiling source code *)
(*
   Hosted ProgramTypes package resolver. None explicitly disables package loading.
   Optional caller-owned bounded reuse scope.
*)
type compileRequest={context:compileContext;mode:CompilerOptions.compileMode;sources:sourceUnit AST.nonEmptyList;allowInternal:bool;verbosity:int;options:CompilerOptions.compilerOptions;packageValues:packageValueCatalog;packageManager:PackageManager.config option;passTimingRecorder:CompilerOptions.passTimingRecorder option;session:CompilationSession.compilationSession option}
