(*  Options.fs - Define compilation options, reports, and timing records independently of backend implementation. *)
type passTiming = {
    pass: string;
    elapsed: HostTimeSpan.t;
}
(*  Recorder for compiler pass timings *)
type passTimingRecorder = passTiming -> unit
(*  Result of execution with timing *)
type executionOutput = {
    exitCode: int;
    stdout: string;
    stderr: string;
    runtimeTime: HostTimeSpan.t;
}
(*  Finite stdin supplied to a captured native execution. *)
type executionInput =
    | Closed
    | Bytes of bytes
(*  Compilation mode for labeling and test behavior *)
type compileMode =
    | FullProgram
    | TestExpression
type nativeLayoutProbe =
    | NoNativeLayoutProbe
    | RootWord
    | TupleWords
(*  Shared compiler warning settings. *)
let defaultWarningSettings : AST.warningSettings = AST.defaultWarningSettings
(*  Result of compilation with timing *)
type compileReport = {
    target: Platform.target;
    result: (bytes,string) result;
    compileTime: HostTimeSpan.t;
}
(*  Compiler options for controlling optimization behavior *)
type compilerOptions = {
(*  Disable free list memory reuse (always bump allocate) *)
    disableFreeList: bool;
(*  Disable ANF-level optimizations (constant folding, propagation, etc.) *)
    disableANFOpt: bool;
(*  Disable ANF constant folding (includes algebraic identities and constant branches) *)
    disableANFConstFolding: bool;
(*  Disable ANF constant propagation *)
    disableANFConstProp: bool;
(*  Disable ANF copy propagation *)
    disableANFCopyProp: bool;
(*  Disable ANF dead code elimination *)
    disableANFDCE: bool;
(*  Disable ANF strength reduction (pow2 mul/div/mod) *)
    disableANFStrengthReduction: bool;
(*  Disable ANF function inlining *)
    disableInlining: bool;
(*  Disable tail call optimization *)
    disableTCO: bool;
(*  Disable MIR-level optimizations (DCE, copy/constant propagation on SSA) *)
    disableMIROpt: bool;
(*  Disable MIR sparse conditional simplification *)
    disableMIRSCCP: bool;
(*  Disable MIR common subexpression elimination *)
    disableMIRCSE: bool;
(*  Disable MIR dead code elimination *)
    disableMIRDCE: bool;
(*  Disable MIR loop-invariant code motion *)
    disableMIRLICM: bool;
(*  Disable LIR-level optimizations (peephole optimizations) *)
    disableLIROpt: bool;
(*  Disable LIR peephole optimizations *)
    disableLIRPeephole: bool;
(*  Disable function tree shaking (pruning unused stdlib/user functions) *)
    disableFunctionTreeShaking: bool;
(*  Enable runtime expression coverage tracking *)
    enableCoverage: bool;
(*  Enable leak checking (debug only) *)
    enableLeakCheck: bool;
(*  Test-only observation of the source value before semantic printing. *)
    nativeLayoutProbe: nativeLayoutProbe;
(*  Warning compatibility settings passed into type checking *)
    warnings: AST.warningSettings;
(*  Dump ANF representations to stdout *)
    dumpANF: bool;
(*  Dump MIR representations to stdout *)
    dumpMIR: bool;
(*  Dump LIR representations to stdout (before and after register allocation) *)
    dumpLIR: bool;
(*  Restrict IR dumps to function names containing this text. *)
    dumpFunction: string option;
(*  Emit only function and instruction counts for selected IR dumps. *)
    dumpIRSummary: bool;
}
(*  Default compiler options *)
let defaultOptions : compilerOptions = {
    disableFreeList = false;
    disableANFOpt = false;
    disableANFConstFolding = false;
    disableANFConstProp = false;
    disableANFCopyProp = false;
    disableANFDCE = false;
    disableANFStrengthReduction = false;
    disableInlining = false;
    disableTCO = false;
    disableMIROpt = false;
    disableMIRSCCP = false;
    disableMIRCSE = false;
    disableMIRDCE = false;
    disableMIRLICM = false;
    disableLIROpt = false;
    disableLIRPeephole = false;
    disableFunctionTreeShaking = false;
    enableCoverage = false;
    enableLeakCheck = false;
    nativeLayoutProbe = NoNativeLayoutProbe;
    warnings = AST.defaultWarningSettings;
    dumpANF = false;
    dumpMIR = false;
    dumpLIR = false;
    dumpFunction = None;
    dumpIRSummary = false;
}
(*  Explicit lifetime for reuse across a bounded group of compilations (the E2E *)
(*  runner owns one per suite). No compiler-global cache is retained. *)
type codegenFunctionMetric = {
    functionName: string;
    elapsed: HostTimeSpan.t;
    lirInstructionCount: int;
    symbolicInstructionCount: int;
}
(*  Aggregate cost of expanding one LIR opcode across freshly generated ARM64 *)
(*  functions. Symbolic instructions are counted before function peepholing. *)
type codegenLirOpMetric = {
    functionName: string;
    opcode: string;
    detail: string;
    occurrences: int;
    symbolicInstructionCount: int;
    elapsed: HostTimeSpan.t;
}
