(* UserCompilation.ml - Compile a user source unit through the typed pipeline stages. *)
[@@@warning "-4"]

module X = CompilationContexts
module P = PackageCatalog
module O = CompilerOptions
module R = AST_to_ANF
module C = CompilationCacheIdentity
module G = Backend_Arm64_CodeGen
module M = StringOrder.Map
module S = StringOrder.Set
module F = SpecializationIdentity.FunctionSet

let ( let* ) = Result.bind
let log verbosity level text = if verbosity >= level then Output.println text

let roundOne value =
  let scaled = value *. 10. in
  let lower = Float.floor scaled in
  (if scaled -. lower = 0.5 then
     if Float.rem lower 2. = 0. then lower else lower +. 1.
   else Float.round scaled)
  /. 10.

let detail verbosity value =
  log verbosity 2 ("        " ^ FloatFormat.roundTrip (roundOne value) ^ "ms")

let registries (user : R.userOnlyResult) =
  {
    R.scopeContracts = user.R.scopeContracts;
    inertFunctionScopes = user.R.inertFunctionScopes;
    typeReg = user.R.typeReg;
    typeNames = user.R.typeNames;
    recordFieldsReg = user.R.recordFieldsReg;
    recordTypeParamsReg = user.R.recordTypeParamsReg;
    variantLookup = user.R.variantLookup;
    sumMetadata = user.R.sumMetadata;
    rcSumShapeReg = user.R.rcSumShapeReg;
    funcReg = user.R.funcReg;
    functionIds = user.R.functionIds;
    functionNames = user.R.functionNames;
    funcParams = user.R.funcParams;
    moduleRegistry = user.R.moduleRegistry;
    recursiveMembers = user.R.recursiveMembers;
  }

(*
   Compile a user/test program against a prebuilt stdlib/preamble context
   Pass 1: Parse user code only
   Concrete helper and Stdlib specialization names encode their
   complete type arguments. A user unit can request a function already
   supplied by the prebuilt stdlib, with context-specific lowering
   making the allocated bodies differ. Keep the stdlib copy, which is
   first and has the complete stdlib registries. The same holds for a
   user unit that declares a function the stdlib carries under a
   non-Stdlib name (Darklang.LanguageTools.* is in both).
*)
let compileUserWithPlan ?writtenSources (plan : P.userCompilePlan) =
  let start = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6 in
  let elapsed () =
    (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. start
  in
  let record = PipelineDiagnostics.recordPassTiming plan.P.passTimingRecorder in
  let result =
    try
      (* Pass 1: Parse user code. *)
      log plan.P.verbosity 1 plan.P.labels.P.parse;
      let parseResult =
        P.parseWrittenSourceProgram ?writtenSources plan.P.allowInternal true plan.P.sources
        |> fun original ->
        Result.bind original (fun parsed ->
            match plan.P.packageManager with
            | None -> Ok parsed
            | Some config -> (
                let* packages =
                  PackageManager.resolveWritten config
                    plan.P.baseContext.X.typeCheckEnv.Types.resolutionEnv parsed
                in
                let packages =
                  List.map
                    (fun (package : PackageManager.resolvedSource) ->
                      {
                        X.name = package.PackageManager.name;
                        purpose = NameSyntax.SourceUnitPurpose.Package;
                        source = package.PackageManager.source;
                      })
                    packages
                in
                match
                  NonEmptyList.tryFromList
                    (packages @ NonEmptyList.toList plan.P.sources)
                with
                | Some sources ->
                    let writtenSources =
                      Option.map
                        (fun originals -> List.map (fun _ -> None) packages @ originals)
                        writtenSources
                    in
                    P.parseWrittenSourceProgram ?writtenSources plan.P.allowInternal true
                      sources
                | None ->
                    Error "Package resolution produced an empty source program"))
      in
      let parseTime = elapsed () in
      record "Parse" parseTime;
      detail plan.P.verbosity parseTime;
      let* parsed =
        Result.map_error (fun error -> "Parse error: " ^ error) parseResult
      in
      (* Pass 1.5: Type Checking (user code with base TypeCheckEnv). *)
      log plan.P.verbosity 1 plan.P.labels.P.typeCheck;
      let checking =
        WrittenChecking.checkSourceUnitsWithBase
          plan.P.baseContext.X.writtenEnvironment plan.P.allowInternal true
          parsed
        |> Result.map (fun (typ, program, _) ->
            ( typ,
              program,
              Types.mergeTypeCheckEnv plan.P.baseContext.X.typeCheckEnv
                (WrittenChecking.typeCheckEnvironment program) ))
      in
      let typeTime = elapsed () -. parseTime in
      record "Type Checking" typeTime;
      detail plan.P.verbosity typeTime;
      let* programType, typed, env = checking in
      let* () =
        if
          plan.P.mode = O.FullProgram
          && programType <> AST.TUnit && programType <> AST.TInt64
          && programType <> AST.TInt
        then
          Error
            ("File entry expression must return Unit, Int, or Int64; got "
            ^ CheckingDiagnostics.typeToString programType)
        else Ok ()
      in
      let jsonStart = elapsed () in
      let planned =
        JsonPlanning.rewriteProgramWithSession
          (Option.map
             (fun (session : CompilationSession.compilationSession) ->
               session#jsonPlanning)
             plan.P.session)
          env typed
      in
      record "JSON Planning" (elapsed () -. jsonStart);
      let renderStart = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6 in
      let plannedType = Types.resolveType env.Types.aliasReg programType in
      let rendered, boundary =
        if plan.P.mode = O.FullProgram then (planned, plannedType)
        else if plannedType = AST.TUnit then (planned, AST.TUnit)
        else
          ( ValueRendering.rewriteProgram env.Types.indexedTypeReg
              env.Types.indexedSumTypeReg plannedType planned,
            AST.TString )
      in
      record "Value Rendering"
        ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. renderStart);
      if plan.P.verbosity >= 3 then (
        Output.println
          ("Program type: " ^ CheckingDiagnostics.typeToString programType);
        Output.println "");
      (* Pass 2: AST → ANF (user only). *)
      log plan.P.verbosity 1 plan.P.labels.P.anf;
      let catalogStart = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6 in
      let materialized =
        P.materializePackageValueCatalog plan.P.baseContext
          plan.P.options.O.warnings plan.P.packageValues rendered
      in
      record "AST -> ANF Package Catalog"
        ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. catalogStart);
      let converted =
        Result.bind materialized
          (SourcePreparation.convertTypedProgramToUserOnlyWithMode
             plan.P.baseContext plan.P.monomorphization env plan.P.session
             plan.P.passTimingRecorder)
      in
      let anfTime = elapsed () -. parseTime -. typeTime in
      record "AST -> ANF" anfTime;
      detail plan.P.verbosity anfTime;
      let* user, dependencyIdentity =
        Result.map_error
          (fun error -> "ANF conversion error: " ^ error)
          converted
      in
      let functions =
        List.filter
          (fun (func : ANF.functionDef) ->
            not (S.mem func.ANF.name plan.P.skipFunctionNames))
          user.R.userFunctions
      in
      let roots =
        List.filter_map
          (fun (func : ANF.functionDef) ->
            if
              Text.startsWith func.ANF.name "__dark_json_"
              || Text.startsWith func.ANF.name "__dark_eq_"
              || F.mem func.ANF.id user.R.nonInlineableFunctionNames
            then Some func.ANF.id
            else None)
          functions
        |> F.of_list
      in
      let dependencyNames =
        CallGraphReachability.findReachable
          (ANFDeadCodeElimination.buildCallGraph functions)
          roots
      in
      let dependencies, programFunctions =
        List.partition
          (fun (func : ANF.functionDef) -> F.mem func.ANF.id dependencyNames)
          functions
      in
      if plan.P.emitFunctionEvents && plan.P.verbosity >= 3 then (
        Output.println
          ("  [COMPILE] "
          ^ string_of_int (List.length programFunctions)
          ^ " program functions compiled fresh");
        List.iter
          (fun (func : ANF.functionDef) ->
            Output.println ("    - " ^ func.ANF.name))
          functions);
      let entryName = "__dark_compiler_program_entry" in
      let entryId, symbols =
        CheckedAST.internFunction entryName user.R.symbols
      in
      let startId, symbols = CheckedAST.internFunction "_start" symbols in
      let reserved =
        List.exists
          (fun (func : ANF.functionDef) -> func.ANF.name = entryName)
          functions
      in
      let registry = registries user in
      let returns =
        X.mergeReturnTypes plan.P.baseContext.X.returnTypes
          user.R.localReturnTypes
      in
      let releaseCache =
        Option.map
          (fun (session : CompilationSession.compilationSession) includeStatic
               key release generate ->
            session#arm64ReleasePlanSummary includeStatic key release generate)
          plan.P.session
      in
      let caches =
        (if plan.P.options.O.enableCoverage then None else plan.P.session)
        |> Option.map (fun (session : CompilationSession.compilationSession) ->
            {
              C.optimizeMir =
                (fun key generate -> session#optimizeMirFunction key generate);
              allocateLir =
                (fun arch func generate ->
                  session#allocateLirFunction arch func generate);
              allocateCallAwareLir =
                (fun func callees generate ->
                  session#allocateCallAwareLirFunction func callees generate);
            })
      in
      let projectionStart = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6 in
      let projected =
        match plan.P.session with
        | Some session ->
            session#projectMirRegistries dependencyIdentity
              plan.P.baseContext.X.projectedMirRegistries
              user.R.localVariantLookup user.R.localRecordFieldsReg
        | None ->
            C.projectMirRegistryOverlay
              plan.P.baseContext.X.projectedMirRegistries
              user.R.localVariantLookup user.R.localRecordFieldsReg
      in
      record "MIR Registry Projection Preparation"
        ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. projectionStart);
      let merge = C.mergeFunctionSummaries in
      let direct =
        FunctionIdMap.fold
          (fun calls _ callees -> F.union calls callees)
          F.empty
          (ANFDeadCodeElimination.buildCallGraph dependencies)
      in
      let relevant =
        List.fold_left
          (fun ids graph ->
            F.union ids (CallGraphReachability.findReachable graph direct))
          direct
          [ plan.P.stdlib.X.stdlibAnfCallGraph; plan.P.prebuiltCallGraph ]
      in
      let select summaries =
        F.fold
          (fun id values ->
            match FunctionIdMap.tryFind id summaries with
            | Some summary -> FunctionIdMap.add id summary values
            | None -> values)
          relevant FunctionIdMap.empty
      in
      let known =
        merge
          (select plan.P.stdlib.X.callGraphSummaries)
          (select plan.P.prebuiltCallGraphSummaries)
      in
      let build registry candidates optimizations noInline funcs ownership early
          =
        ANFPipeline.buildAnf plan.P.verbosity plan.P.options elapsed registry
          (CheckedAST.nextFunctionOrdinal symbols)
          InliningCommon.defaultConfig candidates optimizations noInline funcs
          ownership early plan.P.passTimingRecorder
      in
      let compileDependencies () =
        let* _, ssa, typeMap =
          build registry plan.P.externalInlineCandidates
            plan.P.stdlib.X.stdlibAnfOptimizationCandidates
            user.R.nonInlineableFunctionNames dependencies
            user.R.ownershipContracts true
        in
        NativePipeline.lowerToAllocatedLirWithKnown known
          plan.P.baseContext.X.target plan.P.verbosity plan.P.options elapsed
          plan.P.passTimingRecorder caches releaseCache
          plan.P.labels.P.stageSuffix ssa typeMap registry (Some projected)
          returns
      in
      let dependencyResult =
        if dependencies = [] then Ok ([], FunctionIdMap.empty)
        else
          match plan.P.session with
          | None -> compileDependencies ()
          | Some session ->
              session#compileDependencies dependencyIdentity
                {
                  C.target = plan.P.baseContext.X.target;
                  options = plan.P.options;
                  nonInlineableFunctionNames = user.R.nonInlineableFunctionNames;
                  knownSummaries =
                    FunctionIdMap.map
                      (fun _ summary -> C.summaryFacts summary)
                      known;
                }
                compileDependencies
      in
      let entry =
        R.synthesizeEntryFunction entryId entryName boundary user.R.mainExpr
      in
      let prune functions =
        if
          plan.P.treeShakeUserFunctions
          && not plan.P.options.O.disableFunctionTreeShaking
        then (
          let before = elapsed () in
          let functions =
            ANFDeadCodeElimination.filterReachableFunctions
              (F.singleton entryId) functions
          in
          record "Early Function Tree Shaking" (elapsed () -. before);
          functions)
        else functions
      in
      let prepare () =
        let functions = entry :: programFunctions in
        match plan.P.mode with
        | O.FullProgram -> Ok functions
        | O.TestExpression ->
            log plan.P.verbosity 1 "  [anf.print-result] Print Insertion...";
            let before = elapsed () in
            let printed =
              PrintInsertion.insertPrintInEntry user.R.functionIds entryName
                boundary functions
              |> fun result ->
              Result.bind result (fun printed ->
                  match plan.P.options.O.nativeLayoutProbe with
                  | O.NoNativeLayoutProbe -> Ok printed
                  | (O.RootWord | O.TupleWords) as probe ->
                      PrintInsertion.insertRootWordProbeInEntry
                        user.R.functionNames entryName (probe = O.TupleWords)
                        printed)
              |> Result.map_error (fun error ->
                  "Print insertion error: " ^ error)
            in
            Result.map
              (fun functions ->
                let duration = elapsed () -. before in
                record "Print Insertion" duration;
                detail plan.P.verbosity duration;
                if
                  PipelineDiagnostics.shouldDumpIR plan.P.verbosity
                    plan.P.options.O.dumpANF
                then
                  PipelineDiagnostics.printANFProgram plan.P.options
                    "=== ANF (after Print insertion) ==="
                    (ANF.Program (functions, ANF.Return ANF.UnitLiteral));
                functions)
              printed
      in
      let programResult =
        if reserved then Error ("Function name '" ^ entryName ^ "' is reserved")
        else
          let* functions = prepare () in
          build registry plan.P.externalInlineCandidates
            plan.P.stdlib.X.stdlibAnfOptimizationCandidates
            user.R.nonInlineableFunctionNames (prune functions)
            user.R.ownershipContracts true
      in
      let* allocatedDependencies, dependencySummaries = dependencyResult in
      let* _, ssa, programTypeMap = programResult in
      let through =
        merge
          (merge plan.P.stdlib.X.callGraphSummaries
             plan.P.prebuiltCallGraphSummaries)
          dependencySummaries
      in
      let ssa =
        SSADirectCallSpecialization.reachableFrom (F.singleton entryId) ssa
      in
      let resultId = ANF.TempId 0 in
      let startFunction =
        R.synthesizeEntryFunction startId "_start" boundary
          (ANF.Let
             (resultId, ANF.Call (entryId, []), ANF.Return (ANF.Var resultId)))
      in
      let startRegistry =
        {
          registry with
          R.funcReg =
            FunctionIdMap.add entryId
              (entryName, AST.TFunction ([], boundary))
              registry.R.funcReg;
          funcParams = M.add entryName [] registry.R.funcParams;
        }
      in
      let* _, startSsa, startTypeMap =
        build startRegistry FunctionIdMap.empty M.empty F.empty
          [ startFunction ] FunctionIdMap.empty false
      in
      let* allocated, summaries =
        NativePipeline.lowerToAllocatedLirWithKnownGroups through
          plan.P.baseContext.X.target plan.P.verbosity plan.P.options elapsed
          plan.P.passTimingRecorder None releaseCache
          plan.P.labels.P.stageSuffix
          [ (ssa, programTypeMap); (startSsa, startTypeMap) ]
          startRegistry (Some projected)
          (FunctionIdMap.add entryId (entryName, boundary) returns)
      in
      let starts, programs =
        List.partition
          (fun (func : LIR.functionDef) -> func.LIR.id = startFunction.ANF.id)
          allocated
      in
      let allocatedUser = starts @ programs @ allocatedDependencies in
      let allUser = plan.P.prebuiltSymbolicFunctions @ allocatedUser in
      let before = elapsed () in
      let callGraph =
        if plan.P.options.O.disableFunctionTreeShaking then FunctionIdMap.empty
        else
          FunctionIdMap.fold
            (fun graph id calls -> FunctionIdMap.add id calls graph)
            plan.P.prebuiltCallGraph
            (DeadCodeElimination.buildCallGraph user.R.functionIds allocatedUser)
      in
      record "Function Tree Shaking" (elapsed () -. before);
      let finalUser =
        if plan.P.treeShakeUserFunctions then (
          log plan.P.verbosity 1 "  [lir.tree-shake] Function Tree Shaking...";
          let before = elapsed () in
          let funcs =
            if plan.P.options.O.disableFunctionTreeShaking then allUser
            else
              FunctionTreeShaking.filterUserFunctionsWithCallGraph
                (Some "_start") callGraph allUser
          in
          record "Function Tree Shaking" (elapsed () -. before);
          funcs)
        else allUser
      in
      let finalUser =
        if
          (not plan.P.treeShakeUserFunctions)
          || plan.P.options.O.disableFunctionTreeShaking
        then finalUser
        else
          let byId =
            List.map
              (fun (func : LIR.functionDef) -> (func.LIR.id, func))
              allUser
            |> FunctionIdMap.ofList
          in
          let rec close reachable pending =
            if F.is_empty pending then reachable
            else
              let found =
                F.elements pending
                |> List.filter_map (fun id -> FunctionIdMap.tryFind id byId)
                |> List.fold_left
                     (fun ids func ->
                       F.union ids
                         (DeadCodeElimination.getCalledFunctions
                            user.R.functionIds func))
                     F.empty
                |> F.filter (fun id ->
                    FunctionIdMap.containsKey id byId
                    && not (F.mem id reachable))
              in
              close (F.union reachable found) found
          in
          let ids funcs =
            List.map (fun (func : LIR.functionDef) -> func.LIR.id) funcs
            |> F.of_list
          in
          let catalog =
            List.filter
              (fun (func : LIR.functionDef) ->
                func.LIR.name = "Builtin.pmFindValuesByValueType"
                || func.LIR.name = "Builtin.pmGetLocationsByValue"
                || Text.startsWith func.LIR.name "Builtin.pmEvaluateValue_")
              allUser
          in
          let initial = F.union (ids finalUser) (ids catalog) in
          let reachable = close initial initial in
          List.filter
            (fun (func : LIR.functionDef) -> F.mem func.LIR.id reachable)
            allUser
      in
      if plan.P.emitFunctionEvents && plan.P.verbosity >= 3 then (
        Output.println
          ("  [COMBINED] fresh: "
          ^ string_of_int (List.length allocatedUser)
          ^ ", total: "
          ^ string_of_int (List.length allUser));
        List.iter
          (fun (func : LIR.functionDef) ->
            Output.println ("    - " ^ func.LIR.name))
          allUser;
        Output.println
          ("  [TreeShaking] user funcs: "
          ^ string_of_int (List.length finalUser)));
      (* Filter stdlib functions to only include reachable ones (dead code elimination). *)
      let stdlib =
        if plan.P.options.O.disableFunctionTreeShaking then
          plan.P.stdlib.X.allocatedFunctions
        else
          let before = elapsed () in
          let funcs =
            match plan.P.session with
            | Some session ->
                session#reachableStdlibFunctions (Obj.repr plan.P.stdlib)
                  callGraph finalUser plan.P.stdlib.X.stdlibCallGraph
                  plan.P.stdlib.X.allocatedFunctions
            | None ->
                FunctionTreeShaking.filterStdlibFunctionsWithUserCallGraph
                  plan.P.stdlib.X.stdlibCallGraph callGraph finalUser
                  plan.P.stdlib.X.allocatedFunctions
          in
          record "Function Tree Shaking" (elapsed () -. before);
          funcs
      in
      let prebuilt =
        List.map (fun (func : LIR.functionDef) -> func.LIR.name) stdlib
        |> S.of_list
      in
      let deduplicate functions =
        let _, retained =
          List.fold_left
            (fun (names, all) (func : LIR.functionDef) ->
              match M.find_opt func.LIR.name names with
              | None -> (M.add func.LIR.name func names, func :: all)
              | Some existing when C.lirFunctionEquals existing func ->
                  (names, all)
              | Some _
                when Text.startsWith func.LIR.name "__dark_eq_"
                     || Text.startsWith func.LIR.name "__dark_compare_"
                     || Text.startsWith func.LIR.name "Darklang.Stdlib."
                     || S.mem func.LIR.name prebuilt ->
                  (names, all)
              | Some _ ->
                  Crash.crash
                    ("Conflicting allocated LIR functions named '"
                   ^ func.LIR.name ^ "'"))
            (M.empty, []) functions
        in
        List.rev retained
      in
      (* x64 emits one ELF symbol per function name, so discard duplicate specializations there.
     ARM64 emission has historically retained the user copies; preserving that selection also preserves specialization. *)
      let all, retained =
        match plan.P.baseContext.X.target with
        | Platform.LinuxX86_64 ->
            ( deduplicate (stdlib @ finalUser),
              List.filter
                (fun (func : LIR.functionDef) ->
                  not (S.mem func.LIR.name prebuilt))
                finalUser )
        | Platform.ARM64Backend _ -> (stdlib @ finalUser, finalUser)
      in
      let dependencyDisplay =
        F.elements dependencyNames
        |> List.filter_map (fun id ->
            FunctionIdMap.tryFind id registry.R.functionNames)
        |> S.of_list
      in
      let lowered =
        List.map
          (fun (func : LIR.functionDef) -> func.LIR.name)
          allocatedDependencies
        |> S.of_list |> S.union dependencyDisplay
      in
      let reachableDependencies, reachablePrograms =
        List.partition
          (fun (func : LIR.functionDef) -> S.mem func.LIR.name lowered)
          retained
      in
      let variants =
        M.map
          (fun (info : Types.sumTypeInfo) ->
            {
              LIR.typeParams = info.Types.typeParams;
              variants =
                List.map
                  (fun (variant : Types.sumVariantInfo) ->
                    {
                      LIR.name = variant.Types.name;
                      tag = variant.Types.tag;
                      payload =
                        (match variant.Types.fields with
                        | [] -> None
                        | [ field ] -> Some field
                        | fields -> Some (AST.TTuple fields));
                      fieldCount = List.length variant.Types.fields;
                    })
                  info.Types.variants;
            })
          env.Types.indexedSumTypeReg
      in
      let allocatedProgram =
        LIR.Program (all, variants, registry.R.recordFieldsReg)
      in
      let writes =
        merge through summaries |> FunctionIdMap.toList
        |> List.filter_map (fun (id, (summary : C.functionSummary)) ->
            Option.map (fun writes -> (id, writes)) summary.C.arm64Writes)
        |> FunctionIdMap.ofList
      in
      let fresh = Obj.repr programs in
      let starts, others =
        List.partition
          (fun (func : LIR.functionDef) -> func.LIR.name = "_start")
          retained
      in
      let runs =
        List.fold_left
          (fun runs (func : LIR.functionDef) ->
            let dep = S.mem func.LIR.name dependencyDisplay in
            match runs with
            | (same, funcs) :: rest when dep = same ->
                (same, func :: funcs) :: rest
            | _ -> (dep, [ func ]) :: runs)
          [] others
        |> List.rev
        |> List.map (fun (dep, funcs) ->
            {
              G.contextIdentity = (if dep then dependencyIdentity else fresh);
              reusableAcrossCompilations = false;
              functions = List.rev funcs;
            })
      in
      let groups =
        [
          {
            G.contextIdentity = fresh;
            reusableAcrossCompilations = false;
            functions = starts;
          };
          {
            G.contextIdentity = Obj.repr plan.P.stdlib;
            reusableAcrossCompilations = true;
            functions = stdlib;
          };
        ]
        @ runs
        |> List.filter (fun (group : G.functionGroup) ->
            group.G.functions <> [])
      in
      if
        PipelineDiagnostics.shouldDumpIR plan.P.verbosity
          plan.P.options.O.dumpLIR
      then
        PipelineDiagnostics.printLIRProgram plan.P.options
          "=== LIR (After Register Allocation) ===" allocatedProgram;
      let metadata =
        [
          {
            G.contextIdentity = Obj.repr plan.P.baseContext;
            functions = stdlib;
          };
          {
            G.contextIdentity = dependencyIdentity;
            functions = reachablePrograms;
          };
          {
            G.contextIdentity = dependencyIdentity;
            functions = reachableDependencies;
          };
        ]
        |> List.filter (fun (group : G.metadataGroup) ->
            group.G.functions <> [])
      in
      BinaryOutput.generateBinary plan.P.baseContext.X.target plan.P.verbosity
        plan.P.options elapsed plan.P.passTimingRecorder
        "  [backend.codegen] Code Generation..."
        "  [backend.emit] ARM64 Emit ({format})..." false false plan.P.session
        dependencyIdentity groups metadata registry.R.rcSumShapeReg writes
        allocatedProgram
    with
    | Failure message | Invalid_argument message ->
        Error ("Compilation failed: " ^ message)
    | ex -> Error ("Compilation failed: " ^ Printexc.to_string ex)
  in
  let duration = elapsed () in
  (match result with
  | Ok _ ->
      log plan.P.verbosity 1
        ("  ✓ Compilation complete ("
        ^ FloatFormat.roundTrip (roundOne duration)
        ^ "ms)")
  | Error _ -> ());
  {
    O.target = plan.P.baseContext.X.target;
    result;
    compileTime = Int64.of_float (duration *. 1e6);
  }
