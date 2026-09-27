// Diagnostics.fs - Record pass timing and format scoped IR diagnostics.

module PipelineDiagnostics

open ANFPrinter
open MIRPrinter
open LIRPrinter
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open Output
open CompilerOptions
open CompilationSession

let internal recordPassTiming
    (recorder: PassTimingRecorder option)
    (pass: string)
    (elapsedMs: float)
    : unit =
    match recorder with
    | None -> ()
    | Some record ->
        record { Pass = pass; Elapsed = TimeSpan.FromMilliseconds(elapsedMs) }

/// Determine whether to dump a specific IR, based on verbosity or explicit option
let internal shouldDumpIR (verbosity: int) (enabled: bool) : bool =
    verbosity >= 3 || enabled

let internal buildANFOptimizeOptions (options: CompilerOptions) : ANFConstants.OptimizeOptions =
    let enabled = not options.DisableANFOpt
    {
        EnableConstFolding = enabled && not options.DisableANFConstFolding
        EnableConstProp = enabled && not options.DisableANFConstProp
        EnableCopyProp = enabled && not options.DisableANFCopyProp
        EnableDCE = enabled && not options.DisableANFDCE
        EnableCSE = enabled
        EnableStrengthReduction = enabled && not options.DisableANFStrengthReduction
        EnableTailRecursionModuloOperation = enabled && not options.DisableTCO
    }

let internal shouldRunANFOptimize (anfOptions: ANFConstants.OptimizeOptions) : bool =
    anfOptions.EnableConstFolding
    || anfOptions.EnableConstProp
    || anfOptions.EnableCopyProp
    || anfOptions.EnableDCE
    || anfOptions.EnableCSE
    || anfOptions.EnableStrengthReduction
    || anfOptions.EnableTailRecursionModuloOperation

let internal buildMIROptimizeOptions (options: CompilerOptions) : MIROptimizationFacts.OptimizeOptions =
    let enabled = not options.DisableMIROpt
    {
        EnableSCCP = enabled && not options.DisableMIRSCCP
        EnableCSE = enabled && not options.DisableMIRCSE
        EnableDCE = enabled && not options.DisableMIRDCE
        EnableLICM = enabled && not options.DisableMIRLICM
    }

let internal shouldRunMIROptimize (mirOptions: MIROptimizationFacts.OptimizeOptions) : bool =
    mirOptions.EnableSCCP
    || mirOptions.EnableCSE
    || mirOptions.EnableDCE
    || mirOptions.EnableLICM

let internal formatPassGroup (label: string) (passes: (string * bool) list) : string =
    let enabled =
        passes
        |> List.choose (fun (name, isEnabled) -> if isEnabled then Some name else None)
    let enabledNames = String.concat ", " enabled
    match enabled with
    | [] -> $"{label} (disabled)"
    | _ -> $"{label} ({enabledNames})"

/// Print ANF program in a consistent, human-readable format
let internal printANFProgram (options: CompilerOptions) (title: string) (program: ANF.Program) : unit =
    println title
    println (formatANFDump options.DumpFunction options.DumpIRSummary program)
    println ""

/// Print MIR program (with CFG) in a consistent format
let internal printMIRProgram (options: CompilerOptions) (title: string) (program: MIR.Program) : unit =
    println title
    println (formatMIRDump options.DumpFunction options.DumpIRSummary program)
    println ""

/// Print symbolic LIR program (with CFG) in a consistent format
let internal printLIRProgram (options: CompilerOptions) (title: string) (program: LIR.Program) : unit =
    println title
    println (formatLIRDump options.DumpFunction options.DumpIRSummary program)
    println ""
