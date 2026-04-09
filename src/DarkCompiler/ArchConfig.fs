// ArchConfig.fs - Architecture Configuration
//
// Defines the calling convention and register configuration for each
// target architecture. Used by MIR→LIR and the register allocator
// to generate architecture-appropriate code.
//
// The compiler uses LIR.PhysReg as abstract register identifiers.
// Each architecture maps a subset of these to physical hardware registers
// in its codegen pass.

module ArchConfig

/// Architecture-specific register and calling convention configuration
type Config = {
    /// Registers used for integer function arguments, in order.
    IntArgRegs: LIR.PhysReg list

    /// Registers used for floating-point function arguments, in order.
    FPArgRegs: LIR.PhysFPReg list

    /// The register that holds the integer return value.
    ReturnReg: LIR.PhysReg

    /// Caller-saved registers available for allocation (excluding reserved/scratch).
    CallerSavedRegs: LIR.PhysReg list

    /// Callee-saved registers available for allocation.
    CalleeSavedRegs: LIR.PhysReg list

    /// Scratch registers reserved for the compiler (not allocatable).
    ScratchRegs: LIR.PhysReg list
}

/// ARM64 (AArch64) calling convention
let arm64 : Config = {
    IntArgRegs = [LIR.X0; LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
    FPArgRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]
    ReturnReg = LIR.X0
    CallerSavedRegs = [LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
    CalleeSavedRegs = [LIR.X19; LIR.X20; LIR.X21; LIR.X22; LIR.X23; LIR.X24; LIR.X25; LIR.X26]
    ScratchRegs = [LIR.X9; LIR.X10]
}

/// x86_64 System V AMD64 ABI calling convention.
/// Uses a subset of LIR.PhysReg identifiers — the x86_64 codegen maps
/// these to actual hardware registers:
///   X0→RAX, X1→RDI, X2→RSI, X3→RDX, X4→RCX, X5→R8, X6→R9,
///   X7→R10, X9→R11(scratch),
///   X19→RBX, X20→R12, X21→R13, X22→R14, X23→R15 (callee-saved)
let x86_64 : Config = {
    IntArgRegs = [LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6]
    FPArgRegs = [LIR.D0; LIR.D1; LIR.D2; LIR.D3; LIR.D4; LIR.D5; LIR.D6; LIR.D7]
    ReturnReg = LIR.X0
    CallerSavedRegs = [LIR.X1; LIR.X2; LIR.X3; LIR.X4; LIR.X5; LIR.X6; LIR.X7]
    CalleeSavedRegs = [LIR.X19; LIR.X20; LIR.X21]
    ScratchRegs = [LIR.X9; LIR.X10]
    // Note: X22→R14 and X23→R15 are reserved for heap pointer and free list base
}

/// Get the architecture config for the given architecture
let forArch (arch: Platform.Arch) : Config =
    match arch with
    | Platform.ARM64 -> arm64
    | Platform.X86_64 -> x86_64
