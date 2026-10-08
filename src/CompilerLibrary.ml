(* CompilerLibrary.ml - Compile a validated request using its explicit source-context plan. *)
module X = CompilationContexts
module P = PackageCatalog
module O = CompilerOptions
module S = StringOrder.Set

let labelsForMode = function
  | O.FullProgram ->
      {
        P.parse = "  [frontend.parse] Parse...";
        typeCheck = "  [frontend.type-check] Type Checking (with stdlib env)...";
        anf = "  [anf.lower] AST → ANF (user only)...";
        stageSuffix = "user only";
      }
  | O.TestExpression ->
      {
        P.parse = "  [frontend.parse] Parse (test expr only)...";
        typeCheck =
          "  [frontend.type-check] Type Checking (with preamble env)...";
        anf = "  [anf.lower] AST → ANF (test expr only)...";
        stageSuffix = "";
      }

let buildCompilePlan (request : X.compileRequest) =
  let stdlib, context, symbolic, graph, summaries, names =
    match request.X.context with
    | X.StdlibOnly stdlib ->
        ( stdlib,
          stdlib.X.context,
          [],
          FunctionIdMap.empty,
          FunctionIdMap.empty,
          S.empty )
    | X.StdlibWithPreamble (stdlib, preamble) ->
        let names =
          List.map
            (fun (func : LIR.functionDef) -> func.LIR.name)
            preamble.X.symbolicFunctions
          |> S.of_list
        in
        ( stdlib,
          preamble.X.context,
          preamble.X.symbolicFunctions,
          preamble.X.symbolicCallGraph,
          preamble.X.callGraphSummaries,
          names )
  in
  let events, treeShake =
    match request.X.mode with
    | O.FullProgram -> (false, false)
    | O.TestExpression -> (true, true)
  in
  let monomorphization =
    match request.X.mode with
    | O.FullProgram ->
        SourcePreparation.Monomorphize (Some context.X.genericFuncDefs)
    | O.TestExpression ->
        SourcePreparation.SpecializeLocalAndReplace context.X.specRegistry
  in
  {
    P.allowInternal = request.X.allowInternal;
    mode = request.X.mode;
    verbosity = request.X.verbosity;
    options = request.X.options;
    packageValues = request.X.packageValues;
    packageManager = request.X.packageManager;
    passTimingRecorder = request.X.passTimingRecorder;
    session = request.X.session;
    stdlib;
    baseContext = context;
    monomorphization;
    externalInlineCandidates = stdlib.X.stdlibInlineCandidates;
    prebuiltSymbolicFunctions = symbolic;
    prebuiltCallGraphSummaries = summaries;
    prebuiltCallGraph = graph;
    skipFunctionNames = names;
    emitFunctionEvents = events;
    treeShakeUserFunctions = treeShake;
    labels = labelsForMode request.X.mode;
    sources = request.X.sources;
  }

(* Compile source code to binary (in-memory, no file I/O). *)
let compile request =
  UserCompilation.compileUserWithPlan (buildCompilePlan request)

(* Generated callers retain parsed expression trees and still cross the
   structural validation, source ownership, and normal checking boundaries. *)
let compileWritten request sources =
  UserCompilation.compileUserWithPlan
    ~writtenSources:(List.map (fun source -> Some source) sources)
    (buildCompilePlan request)
