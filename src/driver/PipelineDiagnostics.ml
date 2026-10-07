(* PipelineDiagnostics.ml - Record pass timing and format scoped IR diagnostics. *)
open ANFPrinter
open MIRPrinter
open LIRPrinter
open Output
open CompilerOptions

let recordPassTiming (recorder : CompilerOptions.passTimingRecorder option)
    (pass : string) (elapsedMs : float) : unit =
  match recorder with
  | None -> ()
  | Some record -> record { pass; elapsed = Int64.of_float (elapsedMs *. 1e6) }

(*  Determine whether to dump a specific IR, based on verbosity or explicit option *)
let shouldDumpIR (verbosity : int) (enabled : bool) : bool =
  verbosity >= 3 || enabled

let buildANFOptimizeOptions (options : CompilerOptions.compilerOptions) :
    ANFConstants.optimizeOptions =
  let enabled = not options.disableANFOpt in
  {
    ANFConstants.enableConstFolding =
      enabled && not options.disableANFConstFolding;
    enableConstProp = enabled && not options.disableANFConstProp;
    enableCopyProp = enabled && not options.disableANFCopyProp;
    enableDCE = enabled && not options.disableANFDCE;
    enableCSE = enabled;
    enableStrengthReduction = enabled && not options.disableANFStrengthReduction;
    enableTailRecursionModuloOperation = enabled && not options.disableTCO;
  }

let shouldRunANFOptimize (anfOptions : ANFConstants.optimizeOptions) : bool =
  anfOptions.ANFConstants.enableConstFolding
  || anfOptions.ANFConstants.enableConstProp
  || anfOptions.ANFConstants.enableCopyProp || anfOptions.ANFConstants.enableDCE
  || anfOptions.ANFConstants.enableCSE
  || anfOptions.ANFConstants.enableStrengthReduction
  || anfOptions.ANFConstants.enableTailRecursionModuloOperation

let buildMIROptimizeOptions (options : CompilerOptions.compilerOptions) :
    MIROptimizationFacts.optimizeOptions =
  let enabled = not options.disableMIROpt in
  {
    MIROptimizationFacts.enableSCCP = enabled && not options.disableMIRSCCP;
    enableCSE = enabled && not options.disableMIRCSE;
    enableDCE = enabled && not options.disableMIRDCE;
    enableLICM = enabled && not options.disableMIRLICM;
  }

let shouldRunMIROptimize (mirOptions : MIROptimizationFacts.optimizeOptions) :
    bool =
  mirOptions.MIROptimizationFacts.enableSCCP
  || mirOptions.MIROptimizationFacts.enableCSE
  || mirOptions.MIROptimizationFacts.enableDCE
  || mirOptions.MIROptimizationFacts.enableLICM

let formatPassGroup (label : string) (passes : (string * bool) list) : string =
  let enabled =
    passes
    |> List.filter_map (fun (name, isEnabled) ->
        if isEnabled then Some name else None)
  in
  let enabledNames = String.concat ", " enabled in
  match enabled with
  | [] -> label ^ " (disabled)"
  | _ -> label ^ " (" ^ enabledNames ^ ")"

(*  Print ANF program in a consistent, human-readable format *)
let printANFProgram (options : CompilerOptions.compilerOptions) (title : string)
    (program : ANF.program) : unit =
  println title;
  println (formatANFDump options.dumpFunction options.dumpIRSummary program);
  println ""

(*  Print MIR program (with CFG) in a consistent format *)
let printMIRProgram (options : CompilerOptions.compilerOptions) (title : string)
    (program : MIR.program) : unit =
  println title;
  println (formatMIRDump options.dumpFunction options.dumpIRSummary program);
  println ""

(*  Print symbolic LIR program (with CFG) in a consistent format *)
let printLIRProgram (options : CompilerOptions.compilerOptions) (title : string)
    (program : LIR.program) : unit =
  println title;
  println (formatLIRDump options.dumpFunction options.dumpIRSummary program);
  println ""
