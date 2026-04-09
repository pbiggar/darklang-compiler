// Platform.fs - Platform Detection and Configuration
//
// Detects the current operating system and CPU architecture, and provides
// platform-specific constants for binary generation and syscalls.
//
// Supports:
// - macOS ARM64 (Mach-O binaries, BSD syscalls)
// - Linux ARM64 (ELF binaries, Linux syscalls)
// - Linux x86_64 (ELF binaries, Linux syscalls)

module Platform

/// Supported target platforms
type OS =
    | MacOS
    | Linux

/// Supported CPU architectures
type Arch =
    | ARM64
    | X86_64

/// Get the current operating system
let detectOS () : Result<OS, string> =
    if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.OSX) then
        Ok MacOS
    elif System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Linux) then
        Ok Linux
    else
        Error "Unsupported operating system. Only macOS and Linux are supported."

/// Get the current CPU architecture
let detectArch () : Result<Arch, string> =
    match System.Runtime.InteropServices.RuntimeInformation.OSArchitecture with
    | System.Runtime.InteropServices.Architecture.Arm64 -> Ok ARM64
    | System.Runtime.InteropServices.Architecture.X64 -> Ok X86_64
    | arch -> Error $"Unsupported architecture: {arch}. Only ARM64 and x86_64 are supported."

/// OS-specific syscall numbers (independent of CPU architecture).
/// On Linux, ARM64 and x86_64 use different numbering schemes.
type SyscallNumbers = {
    Write: uint16
    Exit: uint16
    Mmap: uint16
    Open: uint16
    Read: uint16
    Close: uint16
    Fstat: uint16
    Access: uint16
    Unlink: uint16
    Chmod: uint16
    Getrandom: uint16
    Gettimeofday: uint16
}

/// ARM64-specific syscall invocation details (layered on top of SyscallNumbers)
type ARM64SyscallConfig = {
    Numbers: SyscallNumbers
    SvcImmediate: uint16         // SVC instruction immediate value
    SyscallRegister: ARM64.Reg   // Register to hold syscall number (X16 macOS, X8 Linux)
}

/// Get syscall numbers for macOS ARM64
let private macOSSyscallNumbers : SyscallNumbers = {
    Write = 4us
    Exit = 1us
    Mmap = 197us
    Open = 5us
    Read = 3us
    Close = 6us
    Fstat = 339us
    Access = 33us
    Unlink = 10us
    Chmod = 15us
    Getrandom = 439us
    Gettimeofday = 116us
}

/// Get syscall numbers for Linux ARM64
let private linuxARM64SyscallNumbers : SyscallNumbers = {
    Write = 64us
    Exit = 93us
    Mmap = 222us
    Open = 56us
    Read = 63us
    Close = 57us
    Fstat = 80us
    Access = 48us
    Unlink = 35us
    Chmod = 53us
    Getrandom = 278us
    Gettimeofday = 113us
}

/// Get syscall numbers for Linux x86_64
let linuxX86_64SyscallNumbers : SyscallNumbers = {
    Write = 1us
    Exit = 60us
    Mmap = 9us
    Open = 2us      // open (not openat)
    Read = 0us
    Close = 3us
    Fstat = 5us
    Access = 21us
    Unlink = 87us
    Chmod = 90us
    Getrandom = 318us
    Gettimeofday = 228us  // clock_gettime
}

/// Get ARM64 syscall configuration for the given OS.
/// Used by the ARM64 runtime code generator.
let getARM64SyscallConfig (os: OS) : ARM64SyscallConfig =
    match os with
    | MacOS ->
        { Numbers = macOSSyscallNumbers
          SvcImmediate = 0x80us
          SyscallRegister = ARM64.X16 }
    | Linux ->
        { Numbers = linuxARM64SyscallNumbers
          SvcImmediate = 0us
          SyscallRegister = ARM64.X8 }

/// Get syscall numbers for the given OS (legacy API, returns ARM64 config for compatibility).
/// New code should use getARM64SyscallConfig or linuxX86_64SyscallNumbers directly.
let getSyscallNumbers (os: OS) : ARM64SyscallConfig =
    getARM64SyscallConfig os

/// Check if code signing is required for this platform
let requiresCodeSigning (os: OS) : bool =
    match os with
    | MacOS -> true
    | Linux -> false
