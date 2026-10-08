(*
   E2ETestRunner.ml - End-to-end test runner

   Compiles source code, executes it, and validates output/exit code.
   Internal identifiers are only allowed for stdlib-internal tests.
   Retain parsed actual and expected expressions inside a checked assertion.
   Older E2E lines place an entry after a function declaration's semicolon.
   The interpreter parser treats that semicolon as part of the function body.
   Preserve evaluation order for legacy parenthesized `;` sequences that the
   copied parser cannot read directly, before synthesizing a value assertion.
   Result of running an E2E test
   A value-equality test whose synthesized checker is a single expression and
   can therefore share one compiler invocation with other compatible checks.
   One physical compile/run with one logical result for every test in it.
   Keep the bound finite so an accidental command-line value cannot synthesize
   an arbitrarily large compiler input. This is larger than the complete E2E
   corpus, allowing a requested batch to contain every compatible test.
   Each result chunk deliberately uses only the low 32 bits of an Int64. This
   keeps every printed mask non-negative and makes the final partial chunk easy
   to validate without relying on signed overflow behavior.
   Only value-equality tests with no process contract can share a process. The
   compiler path and options remain production-identical; only the synthesized
   caller contains several independent checks.
   Map of built preamble contexts and their matching stdlib specialization set,
   keyed by source file + preamble text.
   Keep only top-level definitions and their continuation lines.
   This is used by upstream reduced-preamble fallback when the raw per-test
   preamble includes non-definition noise that cannot be parsed standalone.
   Build suite stdlib specializations and per-file/per-preamble contexts
   Interpreter execution fixtures spell a returned Result.Error as `error=...`.
   Native execution renders the value instead of converting it to a process
   failure, so recognize that canonical Result presentation at this boundary.
   QEMU makes the intentional 500,000-iteration TCO stress case much
   slower than native execution while still completing reliably.
   Execute a compiler-library result on its declared target. Unit integration
   tests use the same explicit QEMU boundary as E2E tests when the target does
   not match the development host.
   Run E2E test using a prebuilt preamble context.
   Individual runs and batches compile the same comparison expression tree.
*)
[@@@warning "-4-42"]

open Dark_compiler
open E2EFormat
module WT = WrittenTypes
module SM = StringOrder.Map
module SS = StringOrder.Set
module SI = SpecializationIdentity
module CC = CompilationContexts

let ( let* ) = Result.bind
let nowTicks () = Mtime_clock.elapsed_ns ()

let replace old replacement source =
  let n = String.length source and m = String.length old in
  let b = Buffer.create n in
  let rec loop i =
    if i < n then
      if i + m <= n && String.sub source i m = old then (
        Buffer.add_string b replacement;
        loop (i + m))
      else (
        Buffer.add_char b source.[i];
        loop (i + 1))
  in
  loop 0;
  Buffer.contents b

let length s = Array.length (Text.scalars s)

let slice s first count =
  Text.ofScalars (Array.sub (Text.scalars s) first count)

let trimEnd s =
  let us = Text.scalars s in
  let rec last i =
    if
      i >= 0
      && Uchar.is_valid us.(i)
      && Uucp.White.is_white_space (Uchar.of_int us.(i))
    then last (i - 1)
    else i
  in
  Text.ofScalars (Array.sub us 0 (last (Array.length us - 1) + 1))

let isInternalTestFile path =
  let path = replace "\\" "/" path in
  Text.contains path "/stdlib-internal/" || Text.contains path "/verification/"

let sourceOffset source (position : Tokenizer.pos) =
  let lines = String.split_on_char '\n' source in
  List.fold_left
    (fun sum line -> sum + length line + 1)
    0
    (List.take position.Tokenizer.row lines)
  + position.Tokenizer.column

let parseWritten source =
  Result.map Validation.ValidatedSourceFile.toWrittenTypes
    (WrittenParsing.parse Validation.Script source)

let normalizeInlineEntry source =
  let normalized = replace "\r\n" "\n" source in
  let hasEntry =
    match parseWritten normalized with
    | Ok p -> p.WT.exprsToEval <> []
    | Error _ -> false
  in
  if hasEntry then normalized
  else
    match String.rindex_opt normalized ';' with
    | None -> normalized
    | Some i -> (
        let candidate =
          String.sub normalized 0 i ^ "\n\n"
          ^ String.sub normalized (i + 1) (String.length normalized - i - 1)
        in
        match parseWritten candidate with
        | Ok p when List.length p.WT.exprsToEval = 1 -> candidate
        | _ -> normalized)

let asSingleWrittenExpression (p : WT.sourceFile) =
  match (p.WT.declarations, p.WT.exprsToEval) with
  | [], [ e ] -> Some e
  | _ -> None

let isFloatExpectedExpr = function
  | WT.EFloat _ -> true
  | WT.EApply (_, WT.EFnName (_, name), _, _) when name.WT.fn.WT.name = "negate"
    ->
      true
  | _ -> false

let rewriteParenthesizedStatements source =
  let us = Text.scalars source in
  let n = Array.length us in
  let character u = Text.ofScalars [| u |] in
  let concat xs = Text.ofScalars (Text.scalars (String.concat "" xs)) in
  let rec quoted i escaped rev =
    if i >= n then (concat (List.rev rev), i)
    else
      let c = us.(i) in
      let rev = character c :: rev in
      if escaped then quoted (i + 1) false rev
      else if c = 92 then quoted (i + 1) true rev
      else if c = 34 then (concat (List.rev rev), i + 1)
      else quoted (i + 1) false rev
  in
  let rec group closing first =
    let rec collect i rev statements =
      if i >= n || Some us.(i) = closing then
        let current = concat (List.rev rev) in
        let rewritten =
          List.fold_right
            (fun statement rest -> "let _ = " ^ statement ^ " in " ^ rest)
            (List.rev statements) current
        in
        (rewritten, if i < n then i + 1 else i)
      else
        match us.(i) with
        | 34 ->
            let s, next = quoted (i + 1) false [ "\"" ] in
            collect next (s :: rev) statements
        | (40 | 91 | 123) as c ->
            let closer = if c = 40 then 41 else if c = 91 then 93 else 125 in
            let inner, next = group (Some closer) (i + 1) in
            collect next
              ((character c ^ inner ^ character closer) :: rev)
              statements
        | 59 when closing = Some 41 ->
            collect (i + 1) [] (concat (List.rev rev) :: statements)
        | c -> collect (i + 1) (character c :: rev) statements
    in
    collect first [] []
  in
  fst (group None 0)

(* Build comparisons from the original trees: adding source parentheses would
   move only the first line and change indentation-sensitive applications. *)
let sourceToExecute _allowInternal (test : e2eTest) =
  let parsed =
    match parseWritten test.source with
    | Ok program when program.WT.exprsToEval <> [] -> Ok program
    | Ok _ -> parseWritten (normalizeInlineEntry test.source)
    | Error _ ->
        parseWritten
          (normalizeInlineEntry (rewriteParenthesizedStatements test.source))
  in
  let* left = parsed in
  match test.expectedValueExpr with
  | None -> Ok left
  | Some rhs -> (
      let* right = parseWritten rhs in
      match (List.rev left.WT.exprsToEval, asSingleWrittenExpression right) with
      | actual :: earlier, Some expected ->
          let range = WT.exprRange actual in
          let infix op a b =
            WT.EInfix (range, (range, WT.InfixFnCall op), a, b)
          in
          let comparison =
            if isFloatExpectedExpr expected then
              let name =
                {
                  WT.range;
                  modules =
                    [
                      ({ WT.range; name = "Stdlib" }, range);
                      ({ WT.range; name = "Float" }, range);
                    ];
                  fn = { WT.range; name = "absoluteValue" };
                }
              in
              let difference = infix WT.ArithmeticMinus actual expected in
              let absolute =
                WT.EApply (range, WT.EFnName (range, name), [], [ difference ])
              in
              infix WT.ComparisonLessThan absolute
                (WT.EFloat (range, false, "0", "00000000001"))
            else infix WT.ComparisonEquals actual expected
          in
          Ok { left with WT.exprsToEval = List.rev earlier @ [ comparison ] }
      | _ ->
          Error
            ("Expected-value test must have an entry expression and a single \
              expected expression: " ^ test.name))

type e2eRun =
  | CompileFailed of int * string * int64
  | Ran of int * string * string * int64 * int64

type e2eFailure = { run : e2eRun; message : string }
type e2eTestResult = (e2eRun, e2eFailure) result
type preparedE2EBatchTest = { test : e2eTest; equalityProgram : WT.sourceFile }

type e2eBatchExecution = {
  aggregateRun : e2eRun;
  results : (e2eTest * e2eTestResult) list;
}

let maxSupportedBatchSize = 8192
let batchResultChunkSize = 32

let tryPrepareBatchTest (test : e2eTest) =
  let eligible =
    Option.is_some test.expectedValueExpr
    && Option.is_none test.expectedStdout
    && Option.is_none test.expectedStderr
    && test.arguments = [] && test.environment = [] && test.stdin = Closed
    && (not test.isolated) && test.expectedExitCode = 0
    && Option.is_none test.errorExpectation
    && Option.is_none test.skipReason
  in
  if not eligible then None
  else
    let allowInternal = isInternalTestFile test.sourceFile in
    match sourceToExecute allowInternal test with
    | Error _ -> None
    | Ok equalityProgram
      when equalityProgram.WT.declarations = []
           && List.length equalityProgram.WT.exprsToEval = 1 ->
        Some { test; equalityProgram }
    | Ok _ -> None

type preambleContextKey = string * string

let preambleContextKeyForTest (test : e2eTest) = (test.sourceFile, test.preamble)

let comparePreambleContextKey (a, b) (c, d) =
  let n = StringOrder.compare a c in
  if n = 0 then StringOrder.compare b d else n

module PreambleContextMap = Map.Make (struct
  type t = preambleContextKey

  let compare = comparePreambleContextKey
end)

type suiteContext = {
  preambleContexts : (CC.stdlibResult * CC.preambleContext) PreambleContextMap.t;
}

type preambleBuildSpec = {
  sourceFile : string;
  preamble : string;
  functionLineMap : int SM.t;
  allowInternal : bool;
}

type preamblePlan = {
  spec : preambleBuildSpec;
  tests : e2eTest list;
  analysis : CC.preambleAnalysis option;
  specialization : SI.specializationResult;
  stdlibSpecs : SI.SpecSet.t;
  externalTypeReg : TypeRegistries.typeRegistry;
  externalVariantLookup : LoweringPrimitives.variantLookup;
}

let buildPreambleBuildSpec sourceFile tests =
  let preambles =
    List.map (fun (t : e2eTest) -> t.preamble) tests
    |> List.sort_uniq StringOrder.compare
  in
  match preambles with
  | [ preamble ] -> (
      let maps =
        List.map
          (fun (t : e2eTest) ->
            SM.of_list (StringMap.bindings t.functionLineMap))
          tests
      in
      match maps with
      | first :: rest when List.for_all (SM.equal Int.equal first) rest ->
          Ok
            {
              sourceFile;
              preamble;
              functionLineMap = first;
              allowInternal = isInternalTestFile sourceFile;
            }
      | _ -> Error ("Multiple function line maps found for " ^ sourceFile))
  | _ -> Error ("Multiple preambles found for " ^ sourceFile)

let collectTypeAppsFromProgram program =
  let symbols, tops = CheckedAST.viewProgram program in
  List.fold_left
    (fun acc top ->
      SI.SpecSet.union acc
        (match top with
        | CheckedAST.FunctionDef f when f.CheckedAST.typeParams = [] ->
            Monomorphization.collectTypeAppsFromFunc symbols f
        | CheckedAST.ValueDef v ->
            Monomorphization.collectTypeApps symbols v.CheckedAST.body
        | CheckedAST.Expression e -> Monomorphization.collectTypeApps symbols e
        | _ -> SI.SpecSet.empty))
    SI.SpecSet.empty tops

let filterSpecsByDefs defs specs =
  SI.SpecSet.filter (fun (name, _) -> SM.mem name defs) specs

let isUpstreamDarkTestFile file =
  let file = replace "\\" "/" file in
  Text.contains file "/e2e/upstream/"
  && String.ends_with ~suffix:".dark" (String.lowercase_ascii file)

let parsePreambleAsProgram _allowInternal preamble = parseWritten preamble

let preambleFunctionDefs (p : WT.sourceFile) =
  List.filter_map
    (function WT.DFunction d -> Some d | _ -> None)
    p.WT.declarations

let referencedPreambleFunctions known expr =
  SS.inter known (SS.of_list (WrittenSource.expressionNames expr))

let collectProgramReferencedPreambleFuncs known (p : WT.sourceFile) =
  let decls =
    List.filter_map
      (function
        | WT.DFunction d -> Some d.WT.body
        | WT.DValue d -> Some d.WT.body
        | _ -> None)
      p.WT.declarations
  in
  List.fold_left
    (fun acc e -> SS.union acc (referencedPreambleFunctions known e))
    SS.empty (decls @ p.WT.exprsToEval)

let buildPreambleFunctionDependencyMap known defs =
  List.map
    (fun (d : WT.fnDecl) ->
      (d.WT.name.WT.name, referencedPreambleFunctions known d.WT.body))
    defs
  |> SM.of_list

let expandRequiredPreambleFunctions dependencies initial =
  let rec loop pending required =
    if SS.is_empty pending then required
    else
      let discovered =
        SS.fold
          (fun name acc ->
            SS.union acc
              (Option.value ~default:SS.empty (SM.find_opt name dependencies)))
          pending SS.empty
        |> SS.filter (fun name -> not (SS.mem name required))
      in
      loop discovered (SS.union required discovered)
  in
  loop initial initial

let declarationRange = function
  | WT.DFunction d -> d.WT.range
  | WT.DValue d -> d.WT.range
  | WT.DType d | WT.DTypeDB d -> d.WT.range
  | WT.DModule d -> d.WT.range
  | WT.DExpr e -> WT.exprRange e
  | WT.DTest d -> d.WT.range

let reducePreambleSource required source (p : WT.sourceFile) =
  let normalized = replace "\r\n" "\n" source in
  let lines = String.split_on_char '\n' normalized in
  List.filter_map
    (fun decl ->
      match decl with
      | WT.DFunction d when not (SS.mem d.WT.name.WT.name required) -> None
      | WT.DExpr _ | WT.DTest _ -> None
      | _ ->
          let range = declarationRange decl in
          let start = sourceOffset normalized range.Tokenizer.start in
          let last =
            min range.Tokenizer.end_.Tokenizer.row (List.length lines - 1)
          in
          let finish =
            min (length normalized)
              (List.fold_left
                 (fun sum line -> sum + length line + 1)
                 0
                 (List.take (last + 1) lines))
          in
          if finish > start then
            Some (trimEnd (slice normalized start (finish - start)))
          else None)
    p.WT.declarations
  |> String.concat "\n\n"

let countLeadingSpaces line =
  let rec loop i =
    if i < String.length line && line.[i] = ' ' then loop (i + 1) else i
  in
  loop 0

let isTopLevelPreambleDefinitionStart s =
  List.exists (Text.startsWith s) [ "let "; "val "; "type "; "def " ]

let sanitizePreambleForReducedFallback preamble =
  let rec loop active rev = function
    | [] -> List.rev rev
    | line :: rest -> (
        let trimmed = Text.trim line and indent = countLeadingSpaces line in
        match active with
        | Some _ when trimmed = "" -> loop active (line :: rev) rest
        | Some n when indent > n -> loop active (line :: rev) rest
        | _ ->
            if indent = 0 && isTopLevelPreambleDefinitionStart trimmed then
              loop (Some indent) (line :: rev) rest
            else loop None rev rest)
  in
  String.concat "\n" (loop None [] (String.split_on_char '\n' preamble))

let analyzePreambleWithReducedFunctionSet stdlib (spec : preambleBuildSpec)
    tests =
  let parseResult =
    match parsePreambleAsProgram spec.allowInternal spec.preamble with
    | Ok p -> Ok p
    | Error primary ->
        let sanitized = sanitizePreambleForReducedFallback spec.preamble in
        if sanitized = spec.preamble then Error primary
        else
          Result.map_error
            (fun e -> primary ^ "\nSanitized preamble parse failed: " ^ e)
            (parsePreambleAsProgram spec.allowInternal sanitized)
  in
  let* program = parseResult in
  let defs = preambleFunctionDefs program in
  let names =
    SS.of_list (List.map (fun (d : WT.fnDecl) -> d.WT.name.WT.name) defs)
  in
  let dependencies = buildPreambleFunctionDependencyMap names defs in
  let runnable =
    List.filter
      (fun (t : e2eTest) ->
        t.errorExpectation <> Some CompileError && Option.is_none t.skipReason)
      tests
  in
  let testProgram t = sourceToExecute spec.allowInternal t in
  let unparsable =
    List.exists (fun t -> Result.is_error (testProgram t)) runnable
  in
  let seeds =
    if unparsable then names
    else
      List.fold_left
        (fun acc t ->
          match testProgram t with
          | Error _ -> acc
          | Ok p -> SS.union acc (collectProgramReferencedPreambleFuncs names p))
        SS.empty runnable
  in
  let required =
    SS.filter (fun name -> SS.mem name names) seeds
    |> expandRequiredPreambleFunctions dependencies
  in
  PreambleAnalysis.analyzePreamble spec.allowInternal stdlib
    (reducePreambleSource required spec.preamble program)

let analyzePreambleForPlan stdlib (spec : preambleBuildSpec) tests =
  if Text.trim spec.preamble = "" then Ok None
  else
    match
      PreambleAnalysis.analyzePreamble spec.allowInternal stdlib spec.preamble
    with
    | Ok analysis -> Ok (Some analysis)
    | Error primary when isUpstreamDarkTestFile spec.sourceFile ->
        Result.map_error
          (fun e ->
            "Preamble parse error in " ^ spec.sourceFile ^ ": " ^ primary
            ^ "\nReduced preamble fallback failed: " ^ e)
          (Result.map Option.some
             (analyzePreambleWithReducedFunctionSet stdlib spec tests))
    | Error primary ->
        Error ("Preamble parse error in " ^ spec.sourceFile ^ ": " ^ primary)

let buildPreamblePlan (stdlib : CC.stdlibResult) (spec : preambleBuildSpec)
    tests =
  Result.map
    (fun (analysis : CC.preambleAnalysis option) ->
      let preambleSpecs =
        match analysis with
        | None -> SI.SpecSet.empty
        | Some a -> collectTypeAppsFromProgram a.CC.typedAST
      in
      let externalTypeReg, externalVariantLookup =
        match analysis with
        | None -> (SM.empty, SM.empty)
        | Some a -> (
            match AST_to_ANF.splitDeclarations a.CC.typedAST with
            | Error _ -> (SM.empty, SM.empty)
            | Ok (types, funcs) ->
                let aliases = AST_to_ANF.buildAliasRegistry types in
                let funcs =
                  AST_to_ANF.resolveAliasesInFunctions aliases funcs
                in
                let regs =
                  AST_to_ANF.buildRegistries
                    (CheckedAST.programSymbols a.CC.typedAST)
                    SM.empty types aliases funcs
                in
                (regs.AST_to_ANF.typeReg, regs.AST_to_ANF.variantLookup))
      in
      let generic =
        match analysis with None -> SM.empty | Some a -> a.CC.genericFuncDefs
      in
      let ownSpecs = filterSpecsByDefs generic preambleSpecs in
      let stdlibGeneric = stdlib.CC.context.CC.genericFuncDefs in
      let stdlibSpecsFromPreamble =
        filterSpecsByDefs stdlibGeneric preambleSpecs
      in
      let symbols =
        match analysis with
        | Some a -> CheckedAST.programSymbols a.CC.typedAST
        | None -> stdlib.CC.context.CC.symbols
      in
      let specialization =
        if SM.is_empty generic then
          {
            SI.specializedFuncs = [];
            specRegistry = SI.SpecMap.empty;
            externalSpecs = SI.SpecSet.empty;
            symbols;
          }
        else Monomorphization.specializeFromSpecs symbols generic ownSpecs
      in
      let stdlibSpecs =
        SI.SpecSet.union stdlibSpecsFromPreamble
          (filterSpecsByDefs stdlibGeneric specialization.SI.externalSpecs)
      in
      {
        spec;
        tests;
        analysis;
        specialization;
        stdlibSpecs;
        externalTypeReg;
        externalVariantLookup;
      })
    (analyzePreambleForPlan stdlib spec tests)

let buildSuiteContexts stdlib tests passTimingRecorder =
  let recordTiming name elapsed =
    Option.iter
      (fun record -> record { CompilerOptions.pass = name; elapsed })
      passTimingRecorder
  in
  let overlapping =
    SS.of_list
      [
        "Start Function Compilation";
        "JSON Planning";
        "ARM64 Codegen Metadata";
        "ARM64 Codegen Functions";
        "ARM64 Codegen Helpers";
        "ARM64 Codegen Assembly";
        "ARM64 Codegen Peephole";
      ]
  in
  let measure name operation =
    let nested = ref 0L in
    let recorder =
      Option.map
        (fun outer (timing : CompilerOptions.passTiming) ->
          if
            (not (SS.mem timing.CompilerOptions.pass overlapping))
            && not
                 (List.exists
                    (Text.startsWith timing.CompilerOptions.pass)
                    [
                      "TypeCheck: ";
                      "AST -> ANF Preparation: ";
                      "SSA: ";
                      "RegAlloc: ";
                    ])
          then nested := Int64.add !nested timing.CompilerOptions.elapsed;
          outer timing)
        passTimingRecorder
    in
    let start = nowTicks () in
    let result = operation recorder in
    let overhead = Int64.sub (Int64.sub (nowTicks ()) start) !nested in
    if overhead > 0L then recordTiming name overhead;
    result
  in
  let start = nowTicks () in
  let groups =
    Array.fold_left
      (fun groups test ->
        let key = preambleContextKeyForTest test in
        let rec add = function
          | [] -> [ (key, [ test ]) ]
          | (existing, group) :: rest ->
              if comparePreambleContextKey key existing = 0 then
                (existing, group @ [ test ]) :: rest
              else (existing, group) :: add rest
        in
        add groups)
      [] tests
  in
  let plansResult =
    List.fold_left
      (fun result (key, group) ->
        let* plans = result in
        let file, _ = key in
        let* spec = buildPreambleBuildSpec file group in
        Result.map
          (fun plan -> (key, plan) :: plans)
          (buildPreamblePlan stdlib spec group))
      (Ok []) groups
  in
  recordTiming "Suite Context Planning" (Int64.sub (nowTicks ()) start);
  let* plans = plansResult in
  let* specializedPlans =
    measure "Suite Context Stdlib Specialization Overhead" (fun recorder ->
        List.fold_left
          (fun result (key, plan) ->
            let* acc = result in
            Result.map
              (fun specialized -> (key, plan, specialized) :: acc)
              (StdlibCompilation.buildStdlibSpecializations stdlib
                 plan.stdlibSpecs plan.externalTypeReg
                 plan.externalVariantLookup recorder))
          (Ok []) plans)
  in
  measure "Suite Context Preamble Build Overhead" (fun recorder ->
      let result =
        List.fold_left
          (fun result (key, plan, (specialized : CC.stdlibResult)) ->
            let* contexts = result in
            let contextResult =
              match plan.analysis with
              | None ->
                  Ok
                    {
                      CC.context = specialized.CC.context;
                      anfFunctions = [];
                      typeMap = specialized.CC.stdlibTypeMap;
                      symbolicFunctions = [];
                      callGraphSummaries = FunctionIdMap.empty;
                      symbolicCallGraph = FunctionIdMap.empty;
                    }
              | Some analysis when SI.SpecSet.is_empty plan.stdlibSpecs ->
                  PreambleCompilation.buildPreambleContextFromAnalysis
                    specialized analysis plan.specialization
                    plan.spec.sourceFile plan.spec.functionLineMap recorder
                  |> Result.map snd
                  |> Result.map_error (fun e ->
                      "Preamble build error (" ^ plan.spec.sourceFile ^ "): "
                      ^ e)
              | Some _ ->
                  (let* analysis =
                     analyzePreambleForPlan specialized plan.spec plan.tests
                   in
                   let analysis =
                     match analysis with
                     | Some a -> a
                     | None ->
                         Crash.crash
                           "A nonempty preamble disappeared during \
                            specialization"
                   in
                   let specs =
                     SI.SpecMap.fold
                       (fun key _ acc -> SI.SpecSet.add key acc)
                       plan.specialization.SI.specRegistry SI.SpecSet.empty
                   in
                   let specialization =
                     Monomorphization.specializeFromSpecs
                       (CheckedAST.programSymbols analysis.CC.typedAST)
                       analysis.CC.genericFuncDefs specs
                   in
                   PreambleCompilation.buildPreambleContextFromAnalysis
                     specialized analysis specialization plan.spec.sourceFile
                     plan.spec.functionLineMap recorder)
                  |> Result.map snd
                  |> Result.map_error (fun e ->
                      "Preamble build error (" ^ plan.spec.sourceFile ^ "): "
                      ^ e)
            in
            Result.map
              (fun ctx ->
                PreambleContextMap.add key (specialized, ctx) contexts)
              contextResult)
          (Ok PreambleContextMap.empty) specializedPlans
      in
      Result.map (fun preambleContexts -> { preambleContexts }) result)

let exitCodeFromRun = function
  | CompileFailed (code, _, _) | Ran (code, _, _, _, _) -> code

let stdoutFromRun = function
  | CompileFailed _ -> ""
  | Ran (_, stdout, _, _, _) -> stdout

let stderrFromRun = function
  | CompileFailed (_, error, _) | Ran (_, _, error, _, _) -> error

let failRun run message = Error { run; message }

let visibleOutput s =
  replace "\\" "\\\\" s |> replace "\r" "\\r" |> replace "\n" "\\n"

let didValueEqualityPass run =
  exitCodeFromRun run = 0
  &&
  match
    String.split_on_char '\n' (stdoutFromRun run)
    |> List.filter (fun s -> s <> "")
    |> List.rev
  with
  | "true" :: _ -> true
  | _ -> false

let isRenderedResultError expected run =
  exitCodeFromRun run = 0
  && Text.contains (stdoutFromRun run) ".Error("
  &&
  match expected with
  | None -> true
  | Some message -> Text.contains (stdoutFromRun run) message

let evaluateExpectations (test : e2eTest) run =
  let unexpectedLeak =
    match run with
    | Ran (_, _, stderr, _, _) when not test.disableLeakCheck ->
        let expected =
          Option.fold ~none:false
            ~some:(fun e -> Text.contains e "leaks:")
            test.expectedStderr
        in
        if expected then None
        else
          List.find_map
            (fun line ->
              if not (String.starts_with ~prefix:"leaks: " line) then None
              else
                let number = String.sub line 7 (String.length line - 7) in
                if
                  number <> ""
                  && String.for_all (fun c -> c >= '0' && c <= '9') number
                then Some number
                else None)
            (String.split_on_char '\n' stderr)
    | _ -> None
  in
  match unexpectedLeak with
  | Some leak -> failRun run ("Compiled program leaked: leaks: " ^ leak)
  | None ->
      if test.errorExpectation = Some CompileError then
        match run with
        | Ran _ ->
            failRun run "Expected compilation error but compilation succeeded"
        | CompileFailed (_, error, _) -> (
            match test.expectedErrorMessage with
            | Some expected when not (Text.contains error expected) ->
                failRun run
                  ("Expected compile error message '" ^ expected
                 ^ "' not found in stderr. Actual stderr: " ^ error)
            | _ -> Ok run)
      else if test.errorExpectation = Some AnyError then
        match run with
        | Ran (code, _, _, _, _) when code >= 128 ->
            failRun run
              ("Expected a compiler or language error, but the generated \
                program terminated by signal (exit " ^ string_of_int code ^ ")"
              )
        | _
          when exitCodeFromRun run = 0
               && not (isRenderedResultError test.expectedErrorMessage run) ->
            failRun run "Expected compilation error but compilation succeeded"
        | _ -> (
            match test.expectedErrorMessage with
            | None -> Ok run
            | Some expected ->
                let output =
                  if exitCodeFromRun run = 0 then stdoutFromRun run
                  else stderrFromRun run
                in
                if Text.contains output expected then Ok run
                else
                  failRun run
                    ("Expected error message '" ^ expected
                   ^ "' not found in stderr. Actual stderr: " ^ output))
      else if Option.is_some test.expectedValueExpr then
        if didValueEqualityPass run then Ok run
        else
          let stderr = Text.trim (stderrFromRun run) in
          failRun run
            ("Value mismatch" ^ if stderr = "" then "" else "\n" ^ stderr)
      else
        let matches expected actual =
          match expected with
          | None -> true
          | Some expected -> (
              match test.outputMatch with
              | ExactBytes -> actual = expected
              | NormalizedText -> Text.trim actual = Text.trim expected)
        in
        if
          matches test.expectedStdout (stdoutFromRun run)
          && matches test.expectedStderr (stderrFromRun run)
          && exitCodeFromRun run = test.expectedExitCode
        then Ok run
        else
          failRun run
            ("Output mismatch. stdout expected '"
            ^ visibleOutput
                (Option.value ~default:"<not asserted>" test.expectedStdout)
            ^ "', actual '"
            ^ visibleOutput (stdoutFromRun run)
            ^ "'; stderr expected '"
            ^ visibleOutput
                (Option.value ~default:"<not asserted>" test.expectedStderr)
            ^ "', actual '"
            ^ visibleOutput (stderrFromRun run)
            ^ "'")

let buildCompilerOptions (test : e2eTest) =
  {
    CompilerOptions.defaultOptions with
    CompilerOptions.disableFreeList = test.disableFreeList;
    disableANFOpt = test.disableANFOpt;
    disableANFConstFolding = test.disableANFConstFolding;
    disableANFConstProp = test.disableANFConstProp;
    disableANFCopyProp = test.disableANFCopyProp;
    disableANFDCE = test.disableANFDCE;
    disableANFStrengthReduction = test.disableANFStrengthReduction;
    disableInlining = test.disableInlining;
    disableTCO = test.disableTCO;
    disableMIROpt = test.disableMIROpt;
    disableMIRSCCP = test.disableMIRSCCP;
    disableMIRCSE = test.disableMIRCSE;
    disableMIRDCE = test.disableMIRDCE;
    disableMIRLICM = test.disableMIRLICM;
    disableLIROpt = test.disableLIROpt;
    disableLIRPeephole = test.disableLIRPeephole;
    disableFunctionTreeShaking = test.disableFunctionTreeShaking;
    enableCoverage = false;
    enableLeakCheck = not test.disableLeakCheck;
    nativeLayoutProbe = CompilerOptions.NoNativeLayoutProbe;
    warnings = CompilerOptions.defaultWarningSettings;
    dumpANF = false;
    dumpMIR = false;
    dumpLIR = false;
  }

let stdinBytes s = Bytes.of_string (Utf8.utf8 s)

let tryExecuteBinary target arguments environment stdin binary =
  let input =
    match stdin with
    | Closed -> CompilerOptions.Closed
    | Bytes s -> CompilerOptions.Bytes (stdinBytes s)
  in
  match Platform.detectHostTarget () with
  | Error error -> Error error
  | Ok host when Platform.archFor host = Platform.archFor target -> (
      try
        Ok
          (CompilerExecution.executeCapturedWithArgumentsAndEnvironment target 0
             arguments environment input binary)
      with exn ->
        Error
          (match exn with
          | Failure message | Sys_error message | Invalid_argument message ->
              message
          | _ -> Printexc.to_string exn))
  | Ok _ -> (
      match target with
      | Platform.ARM64Backend arm ->
          Error
            ("Cross-target execution is unavailable for ARM64Backend "
            ^
            match arm with
            | Platform.LinuxARM64 -> "LinuxARM64"
            | Platform.MacOSARM64 -> "MacOSARM64")
      | Platform.LinuxX86_64 ->
          let qemu = "/opt/dcb/qemu/qemu-x86_64" in
          if not (FileIO.exists qemu) then
            Error ("Pinned x86_64 QEMU is unavailable at " ^ qemu)
          else
            let path = Filename.temp_file "dark-e2e-cross-" ".elf" in
            Fun.protect
              ~finally:(fun () -> SourcePreparation.tryDeleteFile path)
              (fun () ->
                try
                  let fd =
                    Unix.openfile path
                      [ Unix.O_WRONLY; Unix.O_CLOEXEC; Unix.O_TRUNC ]
                      0o600
                  in
                  Fun.protect
                    ~finally:(fun () -> Unix.close fd)
                    (fun () ->
                      let rec write offset =
                        if offset < Bytes.length binary then
                          let n =
                            Unix.write fd binary offset
                              (Bytes.length binary - offset)
                          in
                          write (offset + n)
                      in
                      write 0;
                      Unix.fsync fd);
                  let stat = Unix.stat path in
                  Unix.chmod path (stat.Unix.st_perm lor 0o100);
                  let start = nowTicks () in
                  let bytes =
                    match input with
                    | CompilerOptions.Closed -> Bytes.empty
                    | CompilerOptions.Bytes b -> b
                  in
                  match
                    ProcessCapture.captureWithInputAndEnvironment qemu
                      (path :: arguments) environment bytes 120000
                  with
                  | Ok (exitCode, stdout, stderr) ->
                      Ok
                        {
                          CompilerOptions.exitCode;
                          stdout;
                          stderr;
                          runtimeTime = Int64.sub (nowTicks ()) start;
                        }
                  | Error "Execution timed out after 120000ms" ->
                      Error "Cross-target execution exceeded 120s"
                  | Error error -> Error error
                with exn ->
                  Error
                    (match exn with
                    | Failure message
                    | Sys_error message
                    | Invalid_argument message ->
                        message
                    | _ -> Printexc.to_string exn)))

let executeBinaryForTarget target binary =
  tryExecuteBinary target [] [] Closed binary

let compileAndRun ?writtenSources arguments environment stdin request =
  let report =
    match writtenSources with
    | None -> CompilerLibrary.compile request
    | Some sources -> CompilerLibrary.compileWritten request sources
  in
  match report.CompilerOptions.result with
  | Error error -> CompileFailed (1, error, report.CompilerOptions.compileTime)
  | Ok binary -> (
      match
        tryExecuteBinary report.CompilerOptions.target arguments environment
          stdin binary
      with
      | Ok output ->
          Ran
            ( output.CompilerOptions.exitCode,
              output.CompilerOptions.stdout,
              output.CompilerOptions.stderr,
              report.CompilerOptions.compileTime,
              output.CompilerOptions.runtimeTime )
      | Error error ->
          Ran
            ( -1,
              "",
              "Execution failed: " ^ error,
              report.CompilerOptions.compileTime,
              0L ))

let canBatchTogether left right =
  comparePreambleContextKey
    (preambleContextKeyForTest left.test)
    (preambleContextKeyForTest right.test)
  = 0
  && isInternalTestFile left.test.sourceFile
     = isInternalTestFile right.test.sourceFile
  && buildCompilerOptions left.test = buildCompilerOptions right.test

let batchBindingPrefix tests =
  let parts =
    List.concat_map
      (fun prepared ->
        [
          prepared.test.sourceFile;
          prepared.test.name;
          prepared.test.source;
          Option.value ~default:"" prepared.test.expectedValueExpr;
        ])
      tests
  in
  let hash =
    List.fold_left
      (fun hash part ->
        Array.fold_left
          (fun current unit ->
            Int64.mul (Int64.logxor current (Int64.of_int unit)) 1099511628211L)
          (Int64.mul (Int64.logxor hash 255L) 1099511628211L)
          (Text.scalars part))
      (Int64.of_string "0xcbf29ce484222325")
      parts
  in
  let existing =
    List.concat_map
      (fun prepared ->
        StringMap.bindings prepared.test.functionLineMap |> List.map fst)
      tests
    |> SS.of_list
  in
  let rec pick attempt =
    let prefix =
      "e2eBatch"
      ^ Printf.sprintf "%016Lx" hash
      ^ "_"
      ^ if attempt = 0 then "" else string_of_int attempt ^ "_"
    in
    if
      List.exists
        (fun (i, _) -> SS.mem (prefix ^ "Check" ^ string_of_int i) existing)
        (List.mapi (fun i t -> (i, t)) tests)
    then pick (attempt + 1)
    else prefix
  in
  pick 0

(* Only fixed harness scaffolding is generated as text. User comparison trees
   are inserted by buildBatchProgram after this scaffolding is parsed. *)
let buildBatchSource tests =
  let prefix = batchBindingPrefix tests in
  let checkFunctions =
    List.mapi
      (fun i _prepared ->
        let index = string_of_int i in
        "let " ^ prefix ^ "Check" ^ index ^ " (seed: Int64) : Bool =\n  let "
        ^ prefix ^ "CheckResult" ^ index ^ " =\n" ^ "    false" ^ " in\n  let "
        ^ prefix ^ "Fence" ^ index
        ^ " = fun value -> if seed == 0L then value else false in\n  " ^ prefix
        ^ "Fence" ^ index ^ " (" ^ prefix ^ "CheckResult" ^ index ^ ")")
      tests
    |> String.concat "\n\n"
  in
  let resultBindings =
    List.mapi
      (fun i _ ->
        let index = string_of_int i in
        "let " ^ prefix ^ "Result" ^ index ^ " = " ^ prefix ^ "Check" ^ index
        ^ " (0L) in")
      tests
    |> String.concat "\n"
  in
  let rec chunks xs =
    if xs = [] then []
    else
      List.take (min batchResultChunkSize (List.length xs)) xs
      :: chunks (List.drop (min batchResultChunkSize (List.length xs)) xs)
  in
  let masks =
    List.mapi (fun i t -> (i, t)) tests
    |> chunks
    |> List.map (fun chunk ->
        List.map
          (fun (i, _) ->
            let bit = Int64.shift_left 1L (i mod batchResultChunkSize) in
            "(if " ^ prefix ^ "Result" ^ string_of_int i ^ " then "
            ^ Int64.to_string bit ^ "L else 0L)")
          chunk
        |> String.concat "\n+ ")
  in
  let vector =
    match masks with
    | [ mask ] -> mask
    | _ ->
        "("
        ^ String.concat ",\n" (List.map (fun mask -> "(" ^ mask ^ ")") masks)
        ^ ")"
  in
  checkFunctions ^ "\n\n" ^ resultBindings ^ "\n" ^ vector

(* Parse the harness-only scaffolding, then insert each comparison tree into its
   result binding. User expressions and literal contents are never reprinted. *)
let buildBatchProgram tests =
  let* scaffold = parseWritten (buildBatchSource tests) in
  let comparisons =
    List.map
      (fun prepared ->
        match prepared.equalityProgram.WT.exprsToEval with
        | [ expression ] -> expression
        | _ -> Crash.crash "Prepared assertion must have one expression")
      tests
  in
  let rec replace declarations comparisons =
    match (declarations, comparisons) with
    | WT.DFunction fn :: rest, comparison :: remaining ->
        let body =
          match fn.WT.body with
          | WT.ELet (range, pattern, _, body, keyword, equals) ->
              WT.ELet (range, pattern, comparison, body, keyword, equals)
          | _ -> Crash.crash "Batch scaffold must start with a result binding"
        in
        WT.DFunction { fn with WT.body } :: replace rest remaining
    | rest, [] -> rest
    | _ -> Crash.crash "Batch scaffold and assertion counts differ"
  in
  Ok
    {
      scaffold with
      WT.declarations = replace scaffold.WT.declarations comparisons;
    }

let tryParseInteger64 source =
  let n = String.length source in
  let whitespace = function
    | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true
    | _ -> false
  in
  let rec nulEnd n =
    if n > 0 && source.[n - 1] = '\000' then nulEnd (n - 1) else n
  in
  let n = nulEnd n in
  let rec first i =
    if i < n && whitespace source.[i] then first (i + 1) else i
  in
  let rec last i =
    if i >= 0 && whitespace source.[i] then last (i - 1) else i
  in
  let first = first 0 and last = last (n - 1) in
  let digit =
    if first <= last && (source.[first] = '+' || source.[first] = '-') then
      first + 1
    else first
  in
  let rec valid i =
    i > last || (source.[i] >= '0' && source.[i] <= '9' && valid (i + 1))
  in
  if digit > last || not (valid digit) then None
  else Int64.of_string_opt (String.sub source first (last - first + 1))

let tryParseBatchBoolResults expectedCount stdout =
  let last =
    String.split_on_char '\n' stdout
    |> List.filter (fun s -> s <> "")
    |> List.rev
  in
  let expectedChunks =
    (expectedCount + batchResultChunkSize - 1) / batchResultChunkSize
  in
  match last with
  | raw :: _ when expectedCount > 0 && expectedCount <= maxSupportedBatchSize ->
      let line = Text.trim raw in
      let parts =
        if expectedChunks = 1 then [ line ]
        else if Text.startsWith line "(" && Text.endsWith line ")" then
          slice line 1 (length line - 2)
          |> String.split_on_char ',' |> List.map Text.trim
        else []
      in
      let parsed = List.map tryParseInteger64 parts in
      if
        List.length parsed <> expectedChunks
        || List.exists Option.is_none parsed
      then None
      else
        let masks = List.filter_map Fun.id parsed in
        let finalBits =
          let remainder = expectedCount mod batchResultChunkSize in
          if remainder = 0 then batchResultChunkSize else remainder
        in
        let valid =
          List.mapi
            (fun i mask ->
              let bits =
                if i = expectedChunks - 1 then finalBits
                else batchResultChunkSize
              in
              let allowed = Int64.sub (Int64.shift_left 1L bits) 1L in
              mask >= 0L && Int64.logand mask (Int64.lognot allowed) = 0L)
            masks
          |> List.for_all Fun.id
        in
        if not valid then None
        else
          Some
            (List.concat_map
               (fun mask ->
                 List.init batchResultChunkSize (fun bit ->
                     Int64.logand mask (Int64.shift_left 1L bit) <> 0L))
               masks
            |> List.take expectedCount)
  | _ -> None

let splitDuration count index duration =
  let count = Int64.of_int count in
  let quotient = Int64.div duration count
  and remainder = Int64.rem duration count in
  Int64.add quotient (if Int64.of_int index < remainder then 1L else 0L)

let splitRun count index stdout = function
  | CompileFailed (code, error, compile) ->
      CompileFailed (code, error, splitDuration count index compile)
  | Ran (code, _, stderr, compile, runtime) ->
      Ran
        ( code,
          stdout,
          stderr,
          splitDuration count index compile,
          splitDuration count index runtime )

let packageManagerFixture =
  lazy
    (let server = "http://127.0.0.1:1/" in
     let cachePath =
       Filename.concat
         (Filename.get_temp_dir_name ())
         ("dark-compiler-package-e2e-"
         ^ string_of_int (Unix.getpid ())
         ^ ".sqlite3")
     in
     let responses =
       [
         ("function/find/Darklang.Test", 404, "");
         ( "function/find/Darklang.Test.returnsInt",
           200,
           {|{"Hash":["0f116690f2572bcfff9e18effd2589ad4fc7672fc46088c219229a7821a122f4"]}|}
         );
         ( "function/get/with-location/0f116690f2572bcfff9e18effd2589ad4fc7672fc46088c219229a7821a122f4",
           200,
           {|{"entity":{"body":{"EInt":[5376903518395640452,5]},"description":"","hash":{"Hash":["0f116690f2572bcfff9e18effd2589ad4fc7672fc46088c219229a7821a122f4"]},"parameters":[{"description":"","name":"_","typ":{"TUnit":[]}}],"permissionCeiling":{"None":[]},"returnType":{"TInt":[]},"typeParams":[]},"location":{"modules":["Test"],"name":"returnsInt","owner":"Darklang"}}|}
         );
         ("type/find/Darklang.Test", 404, "");
         ("type/find/Darklang.Test.returnsInt", 404, "");
         ("value/find/Darklang.Test", 404, "");
         ("value/find/Darklang.Test.returnsInt", 404, "");
       ]
     in
     List.iter
       (fun (path, status, body) ->
         PackageIO.cacheWrite cachePath (server ^ path) status body)
       responses;
     { PackageManager.server; cachePath })

let packageManagerForFile path =
  if String.ends_with ~suffix:"/package_manager.e2e" path then
    Some (Lazy.force packageManagerFixture)
  else None

let runE2ETestBatchWithPreambleContext stdlib preambleCtx session tests
    passTimingRecorder =
  match tests with
  | [] ->
      {
        aggregateRun = CompileFailed (1, "Cannot execute an empty E2E batch", 0L);
        results = [];
      }
  | first :: _ ->
      let count = List.length tests in
      let source = buildBatchSource tests in
      let request =
        {
          CC.context = CC.StdlibWithPreamble (stdlib, preambleCtx);
          mode = CompilerOptions.TestExpression;
          sources =
            AST.NonEmptyList.singleton
              {
                CC.name = first.test.sourceFile;
                purpose = NameSyntax.SourceUnitPurpose.Executable;
                source;
              };
          allowInternal = isInternalTestFile first.test.sourceFile;
          verbosity = 0;
          options = buildCompilerOptions first.test;
          packageValues = CC.emptyPackageValueCatalog;
          packageManager = packageManagerForFile first.test.sourceFile;
          passTimingRecorder;
          session;
        }
      in
      let aggregateRun =
        match buildBatchProgram tests with
        | Error error -> CompileFailed (1, error, 0L)
        | Ok program ->
            compileAndRun ~writtenSources:[ program ] [] [] Closed request
      in
      let results =
        match aggregateRun with
        | Ran (0, stdout, _, _, _) -> (
            match tryParseBatchBoolResults count stdout with
            | Some values ->
                List.mapi
                  (fun i prepared ->
                    let passed = List.nth values i in
                    let caseStdout = if passed then "true\n" else "false\n" in
                    let run = splitRun count i caseStdout aggregateRun in
                    (prepared.test, evaluateExpectations prepared.test run))
                  tests
            | None ->
                List.mapi
                  (fun i prepared ->
                    let run = splitRun count i stdout aggregateRun in
                    ( prepared.test,
                      failRun run
                        ("Batch returned an invalid result vector for "
                       ^ string_of_int count ^ " tests. Last stdout: "
                       ^ visibleOutput stdout) ))
                  tests)
        | _ ->
            let message =
              match aggregateRun with
              | CompileFailed (_, error, _) ->
                  "Batch compilation failed: " ^ error
              | Ran (code, _, stderr, _, _) ->
                  let detail = Text.trim stderr in
                  "Batch execution failed with exit code " ^ string_of_int code
                  ^ if detail = "" then "" else ": " ^ detail
            in
            List.mapi
              (fun i prepared ->
                let run = splitRun count i "" aggregateRun in
                (prepared.test, failRun run message))
              tests
      in
      { aggregateRun; results }

let tryBuildReducedPreambleForTest allowInternal preamble testSource =
  match parsePreambleAsProgram allowInternal preamble with
  | Ok preambleProgram ->
      let testProgram = testSource in
      let defs = preambleFunctionDefs preambleProgram in
      let names =
        SS.of_list (List.map (fun (d : WT.fnDecl) -> d.WT.name.WT.name) defs)
      in
      let dependencies = buildPreambleFunctionDependencyMap names defs in
      let seeds = collectProgramReferencedPreambleFuncs names testProgram in
      let required =
        SS.filter (fun name -> SS.mem name names) seeds
        |> expandRequiredPreambleFunctions dependencies
      in
      Some (reducePreambleSource required preamble preambleProgram)
  | _ -> None

let runE2ETestSourceWithPreambleContext stdlib preambleCtx session
    (test : e2eTest) program passTimingRecorder =
  let source = test.source in
  let allowInternal = isInternalTestFile test.sourceFile in
  let options = buildCompilerOptions test in
  let request =
    {
      CC.context = CC.StdlibWithPreamble (stdlib, preambleCtx);
      mode = CompilerOptions.TestExpression;
      sources =
        AST.NonEmptyList.singleton
          {
            CC.name = test.sourceFile;
            purpose = NameSyntax.SourceUnitPurpose.Executable;
            source;
          };
      allowInternal;
      verbosity = 0;
      options;
      packageValues = CC.emptyPackageValueCatalog;
      packageManager = packageManagerForFile test.sourceFile;
      passTimingRecorder;
      session;
    }
  in
  let run =
    compileAndRun ~writtenSources:[ program ] test.arguments test.environment
      test.stdin request
  in
  let primary = evaluateExpectations test run in
  let fallback =
    match primary with
    | Ok _ -> false
    | Error failure ->
        Option.is_some test.errorExpectation
        && Option.is_some test.expectedErrorMessage
        && isUpstreamDarkTestFile test.sourceFile
        && String.starts_with ~prefix:"Expected error message" failure.message
  in
  if not fallback then primary
  else
    let fallbackPreamble =
      Option.value ~default:test.preamble
        (tryBuildReducedPreambleForTest allowInternal test.preamble program)
    in
    let request =
      {
        CC.context = CC.StdlibOnly stdlib;
        mode = CompilerOptions.FullProgram;
        sources =
          AST.NonEmptyList.fromList
            [
              {
                CC.name = test.sourceFile ^ ":preamble";
                purpose = NameSyntax.SourceUnitPurpose.Library;
                source = fallbackPreamble;
              };
              {
                CC.name = test.sourceFile;
                purpose = NameSyntax.SourceUnitPurpose.Executable;
                source;
              };
            ];
        allowInternal;
        verbosity = 0;
        options;
        packageValues = CC.emptyPackageValueCatalog;
        packageManager = packageManagerForFile test.sourceFile;
        passTimingRecorder;
        session;
      }
    in
    let run =
      match parsePreambleAsProgram allowInternal fallbackPreamble with
      | Error error -> CompileFailed (1, error, 0L)
      | Ok preamble ->
          compileAndRun ~writtenSources:[ preamble; program ] test.arguments
            test.environment test.stdin request
    in
    match evaluateExpectations test run with
    | Ok _ as success -> success
    | Error _ -> primary

let runE2ETestWithPreambleContext stdlib preambleCtx session (test : e2eTest)
    passTimingRecorder =
  let allowInternal = isInternalTestFile test.sourceFile in
  match sourceToExecute allowInternal test with
  | Error message ->
      let run = CompileFailed (1, message, 0L) in
      failRun run message
  | Ok source ->
      runE2ETestSourceWithPreambleContext stdlib preambleCtx session test source
        passTimingRecorder

let runPreparedE2ETestWithPreambleContext stdlib preambleCtx session prepared
    passTimingRecorder =
  runE2ETestSourceWithPreambleContext stdlib preambleCtx session prepared.test
    prepared.equalityProgram passTimingRecorder
