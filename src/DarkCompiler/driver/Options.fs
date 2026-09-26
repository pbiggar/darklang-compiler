// Options.fs - Define compilation options, reports, and timing records independently of backend implementation.

module CompilerOptions

open System

type PassTiming = {
    Pass: string
    Elapsed: TimeSpan
}

/// Recorder for compiler pass timings
type PassTimingRecorder = PassTiming -> unit

/// Result of execution with timing
type ExecutionOutput = {
    ExitCode: int
    Stdout: string
    Stderr: string
    RuntimeTime: TimeSpan
}

/// Finite stdin supplied to a captured native execution.
type ExecutionInput =
    | Closed
    | Bytes of byte array

/// Compilation mode for labeling and test behavior
type CompileMode =
    | FullProgram
    | TestExpression

/// Shared compiler warning settings.
let defaultWarningSettings : AST.WarningSettings = AST.defaultWarningSettings

/// Result of compilation with timing
type CompileReport = {
    Target: Platform.Target
    Result: Result<byte array, string>
    CompileTime: TimeSpan
}

/// Compiler options for controlling optimization behavior
type CompilerOptions = {
    /// Disable free list memory reuse (always bump allocate)
    DisableFreeList: bool
    /// Disable ANF-level optimizations (constant folding, propagation, etc.)
    DisableANFOpt: bool
    /// Disable ANF constant folding (includes algebraic identities and constant branches)
    DisableANFConstFolding: bool
    /// Disable ANF constant propagation
    DisableANFConstProp: bool
    /// Disable ANF copy propagation
    DisableANFCopyProp: bool
    /// Disable ANF dead code elimination
    DisableANFDCE: bool
    /// Disable ANF strength reduction (pow2 mul/div/mod)
    DisableANFStrengthReduction: bool
    /// Disable ANF function inlining
    DisableInlining: bool
    /// Disable tail call optimization
    DisableTCO: bool
    /// Disable MIR-level optimizations (DCE, copy/constant propagation on SSA)
    DisableMIROpt: bool
    /// Disable MIR constant folding
    DisableMIRConstFolding: bool
    /// Disable MIR common subexpression elimination
    DisableMIRCSE: bool
    /// Disable MIR copy propagation
    DisableMIRCopyProp: bool
    /// Disable MIR dead code elimination
    DisableMIRDCE: bool
    /// Disable MIR CFG simplification
    DisableMIRCFGSimplify: bool
    /// Disable MIR loop-invariant code motion
    DisableMIRLICM: bool
    /// Disable LIR-level optimizations (peephole optimizations)
    DisableLIROpt: bool
    /// Disable LIR peephole optimizations
    DisableLIRPeephole: bool
    /// Disable function tree shaking (pruning unused stdlib/user functions)
    DisableFunctionTreeShaking: bool
    /// Enable runtime expression coverage tracking
    EnableCoverage: bool
    /// Enable leak checking (debug only)
    EnableLeakCheck: bool
    /// Test-only observation of the final machine word before semantic printing.
    ProbeRootWord: bool
    /// Warning compatibility settings passed into type checking
    Warnings: AST.WarningSettings
    /// Dump ANF representations to stdout
    DumpANF: bool
    /// Dump MIR representations to stdout
    DumpMIR: bool
    /// Dump LIR representations to stdout (before and after register allocation)
    DumpLIR: bool
    /// Restrict IR dumps to function names containing this text.
    DumpFunction: string option
    /// Emit only function and instruction counts for selected IR dumps.
    DumpIRSummary: bool
}

/// Default compiler options
let defaultOptions : CompilerOptions = {
    DisableFreeList = false
    DisableANFOpt = false
    DisableANFConstFolding = false
    DisableANFConstProp = false
    DisableANFCopyProp = false
    DisableANFDCE = false
    DisableANFStrengthReduction = false
    DisableInlining = false
    DisableTCO = false
    DisableMIROpt = false
    DisableMIRConstFolding = false
    DisableMIRCSE = false
    DisableMIRCopyProp = false
    DisableMIRDCE = false
    DisableMIRCFGSimplify = false
    DisableMIRLICM = false
    DisableLIROpt = false
    DisableLIRPeephole = false
    DisableFunctionTreeShaking = false
    EnableCoverage = false
    EnableLeakCheck = false
    ProbeRootWord = false
    Warnings = AST.defaultWarningSettings
    DumpANF = false
    DumpMIR = false
    DumpLIR = false
    DumpFunction = None
    DumpIRSummary = false
}

/// Explicit lifetime for reuse across a bounded group of compilations (the E2E
/// runner owns one per suite). No compiler-global cache is retained.
type CodegenFunctionMetric = {
    FunctionName: string
    Elapsed: TimeSpan
    LirInstructionCount: int
    SymbolicInstructionCount: int
}

/// Aggregate cost of expanding one LIR opcode across freshly generated ARM64
/// functions. Symbolic instructions are counted before function peepholing.
type CodegenLirOpMetric = {
    FunctionName: string
    Opcode: string
    Detail: string
    Occurrences: int
    SymbolicInstructionCount: int
    Elapsed: TimeSpan
}
