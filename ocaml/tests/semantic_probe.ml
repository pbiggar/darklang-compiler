(* semantic_probe.ml - Observe native semantic results for migration requests. *)
let rec requests () =
  match input_line stdin with
  | line ->
      let open Yojson.Basic.Util in
      let request = Yojson.Basic.from_string line in
      let stage = request |> member "stage" |> to_string in
      let source = request |> member "source" |> to_string in
      let value = match stage with
        | "tokens" -> Semantic_observation.SemanticJson.tokens (Dark_compiler.Lexer.tokenize source)
        | "parser-support" -> Semantic_observation.SemanticJson.parserSupport source
        | "parameters" | "effects" -> Semantic_observation.SemanticJson.declarationSupport stage source
        | "validated" -> Semantic_observation.SemanticJson.validated source
        | "rendered" -> Semantic_observation.SemanticJson.rendered source
        | "written-source" -> Semantic_observation.SemanticJson.writtenSource source
        | "formatter" -> Semantic_observation.SemanticJson.formatter source
        | "ast-helpers" -> Semantic_observation.SemanticJson.astHelpers source
        | "macho-images" -> Semantic_observation.MachOObservation.observe source
        | "elf-images" -> Semantic_observation.ELFObservation.observe source
        | "x64-resolve" -> Semantic_observation.X64ResolveObservation.observe source
        | "arm64-encoding" -> Semantic_observation.ARMEncodingObservation.observe source
        | "x64-encoding" -> Semantic_observation.X64EncodingObservation.observe source
        | "machine-isa" -> Semantic_observation.MachineISAObservation.observe source
        | "mir-lir" -> Semantic_observation.MIRLIRObservation.observe source
        | "anf-mir" -> Semantic_observation.ANFMIRObservation.observe source
        | "register-allocation" -> Semantic_observation.RegisterAllocationObservation.observe source
        | "lir-peephole" -> Semantic_observation.PeepholeObservation.observe source
        | "callee-clobbers" -> Semantic_observation.CalleeClobberObservation.observe source
        | "block-allocation" -> Semantic_observation.BlockAllocationObservation.observe source
        | "instruction-allocation" -> Semantic_observation.InstructionAllocationObservation.observe source
        | "phi-resolution" -> Semantic_observation.PhiObservation.observe source
        | "spill-operands" -> Semantic_observation.SpillObservation.observe source
        | "float-allocation" -> Semantic_observation.FloatAllocationObservation.observe source
        | "register-coloring" -> Semantic_observation.ColoringObservation.observe source
        | "allocation-foundations" -> Semantic_observation.AllocationObservation.observe source
        | "lir-tree" -> Semantic_observation.LIRTreeObservation.observe source
        | "lir-foundations" -> Semantic_observation.LIRObservation.observe source
        | "ir-printers" -> Semantic_observation.IRPrinterObservation.observe source
        | "mir-sccp" -> Semantic_observation.MIRSCCPObservation.observe source
        | "mir-loops" -> Semantic_observation.MIRLoopObservation.observe source
        | "mir-cse" -> Semantic_observation.MIRCSEObservation.observe source
        | "mir-ssa" -> Semantic_observation.MIRSSAObservation.observe source
        | "mir-foundations" -> Semantic_observation.MIRObservation.observe source
        | "ssa-inlining" -> Semantic_observation.InliningObservation.observe source
        | "ssa-specialization" -> Semantic_observation.SpecializationObservation.observe source
        | "rc-insertion" -> Semantic_observation.RcObservation.observe source
        | "expression-lowering" -> Semantic_observation.ExpressionLoweringObservation.observe source
        | "atom-lowering" -> Semantic_observation.AtomLoweringObservation.observe source
        | "lowering-types" -> Semantic_observation.LoweringAnalysisObservation.observeTypes source
        | "lowering-aggregates" -> Semantic_observation.LoweringAnalysisObservation.observeAggregates source
        | "lowering-operators" -> Semantic_observation.LoweringAnalysisObservation.observeOperators source
        | "monomorphization" -> Semantic_observation.MonomorphizationObservation.observe source
        | "lift-functions" -> Semantic_observation.ClosureAnalysisObservation.observeLiftFunctions source
        | "lift-expressions" -> Semantic_observation.ClosureAnalysisObservation.observeLiftExpressions source
        | "closure-comparisons" -> Semantic_observation.ClosureAnalysisObservation.observeComparisons source
        | "closure-analysis" -> Semantic_observation.ClosureAnalysisObservation.observe source
        | "checked-display" -> Semantic_observation.CheckedFormatObservation.observeDisplay source
        | "checked-structural-format" -> Semantic_observation.CheckedFormatObservation.observe source
        | "inline-lambdas" -> Semantic_observation.InlineLambdasObservation.observe source
        | "type-substitution" -> Semantic_observation.TypeSubstitutionObservation.observe source
        | "lowering-primitives" -> Semantic_observation.LoweringPrimitivesObservation.observe source
        | "memory-planning" -> Semantic_observation.MemoryPlanningObservation.observe source
        | "preparation-registries" -> Semantic_observation.PreparationRegistryObservation.observe source
        | "anf-output-planning" -> Semantic_observation.ANFOutputPlanningObservation.observe source
        | "anf-scalar-optimization" -> Semantic_observation.ANFScalarObservation.observe source
        | "anf" -> Semantic_observation.ANFObservation.observe source
        | "checked-preparation" -> Semantic_observation.CheckedPreparationObservation.observe source
        | "written-checking" -> Semantic_observation.WrittenCheckingObservation.observe source
        | "written-patterns" -> Semantic_observation.WrittenPatternObservation.observe source
        | "written-types" -> Semantic_observation.WrittenTypeObservation.observe source
        | "program-checking" -> Semantic_observation.ProgramObservation.observe source
        | "function-checking" -> Semantic_observation.FunctionObservation.observe source
        | "expression-checking" -> Semantic_observation.ExpressionObservation.observe source
        | "match-checking" -> Semantic_observation.MatchObservation.observe source
        | "call-checking" -> Semantic_observation.CallObservation.observe source
        | "lambda-checking" -> Semantic_observation.LambdaObservation.observe source
        | "stdlib-catalog" -> Semantic_observation.StdlibObservation.observe source
        | "binary-checking" -> Semantic_observation.BinaryObservation.observe source
        | "record-checking" -> Semantic_observation.RecordObservation.observe source
        | "declarations" -> Semantic_observation.DeclarationObservation.observe source
        | "materialize-helpers" -> Semantic_observation.ComparisonObservation.observeMaterialization source
        | "helper-dependencies" -> Semantic_observation.ComparisonObservation.observeDependencies source
        | "structural-helpers" -> Semantic_observation.ComparisonObservation.observeHelpers source
        | "comparison-planning" -> Semantic_observation.ComparisonObservation.observe source
        | "structural-format" -> Semantic_observation.StructuralFormatObservation.observe source
        | "unification" -> Semantic_observation.UnificationObservation.observe source
        | "checking-types" -> Semantic_observation.CheckingTypesObservation.observe source
        | "checked-ast" -> Semantic_observation.InstrumentedCheckedAST.observe source
        | "function-map" -> Semantic_observation.SemanticJson.functionIdMap source
        | "free-variables" -> Semantic_observation.SemanticJson.freeVariables source
        | "checking-diagnostics" -> Semantic_observation.SemanticJson.checkingDiagnostics source
        | "resolution" -> Semantic_observation.SemanticJson.resolution source
        | "names" -> Semantic_observation.SemanticJson.names source
        | "ast" -> Semantic_observation.SemanticJson.ast source
        | "bindings" -> Semantic_observation.SemanticJson.bindings source
        | "types" -> Semantic_observation.SemanticJson.types source
        | "patterns" -> Semantic_observation.SemanticJson.patterns source
        | stage -> failwith ("Unsupported native observation stage: " ^ stage)
      in
      Semantic_observation.StreamingJson.to_channel stdout (`Assoc ["schema", `Int 1; "stage", `String stage; "value", value]);
      output_char stdout '\n'; flush stdout; Gc.full_major ();
      requests ()
  | exception End_of_file -> ()
let () = requests ()
