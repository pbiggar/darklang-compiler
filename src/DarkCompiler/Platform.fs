// Platform.fs - Platform Detection and Configuration
//
// Defines OS and CPU architecture types, detection helpers, and
// per-(OS, Arch) syscall number tables.
//
// Supports:
// - macOS ARM64 (Mach-O binaries, BSD syscalls)
// - Linux ARM64 (ELF binaries, Linux syscalls)
// - Linux x86_64 (ELF binaries, Linux syscalls)

module Platform

open System.Runtime.InteropServices

/// Supported target platforms
type OS =
    | MacOS
    | Linux

/// Supported CPU architectures
type Arch =
    | ARM64
    | X86_64

/// Supported compiler targets. Unsupported OS/architecture pairs cannot be
/// represented after host detection succeeds.
type ARM64Target =
    | MacOSARM64
    | LinuxARM64

type Target =
    | ARM64Backend of ARM64Target
    | LinuxX86_64

/// Get the current operating system
let detectOS () : Result<OS, string> =
    if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then Ok MacOS
    elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then Ok Linux
    else Error "Unsupported operating system. Only macOS and Linux are supported."

/// Get the current CPU architecture
let detectArch () : Result<Arch, string> =
    match RuntimeInformation.OSArchitecture with
    | Architecture.Arm64 -> Ok ARM64
    | Architecture.X64 -> Ok X86_64
    | arch -> Error $"Unsupported architecture: {arch}. Only ARM64 and x86_64 are supported."

/// Validate an OS/architecture pair as one of the compiler's supported targets.
let targetFor (os: OS) (arch: Arch) : Result<Target, string> =
    match os, arch with
    | MacOS, ARM64 -> Ok (ARM64Backend MacOSARM64)
    | Linux, ARM64 -> Ok (ARM64Backend LinuxARM64)
    | Linux, X86_64 -> Ok LinuxX86_64
    | MacOS, X86_64 -> Error "Unsupported target: macOS x86_64"

/// Detect and validate the host target once at compiler initialization.
let detectHostTarget () : Result<Target, string> =
    match detectOS (), detectArch () with
    | Error err, _ -> Error err
    | _, Error err -> Error err
    | Ok os, Ok arch -> targetFor os arch

let osFor (target: Target) : OS =
    match target with
    | ARM64Backend MacOSARM64 -> MacOS
    | ARM64Backend LinuxARM64
    | LinuxX86_64 -> Linux

let archFor (target: Target) : Arch =
    match target with
    | ARM64Backend _ -> ARM64
    | LinuxX86_64 -> X86_64

/// Syscall numbers for a specific (OS, Arch) pair.
/// On Linux, ARM64 and x86_64 use different numbering schemes.
type SyscallNumbers = {
    Write: uint16
    Exit: uint16
    Mmap: uint16  // Memory map syscall for heap allocation
    // File I/O syscalls
    Open: uint16      // Open file (or openat on Linux with AT_FDCWD)
    Read: uint16      // Read from file descriptor
    Close: uint16     // Close file descriptor
    Fstat: uint16     // Get file status (for file size)
    Access: uint16    // Check file accessibility (for exists)
    Unlink: uint16    // Delete file (or unlinkat on Linux with AT_FDCWD)
    Chmod: uint16     // Change file mode (or fchmodat on Linux with AT_FDCWD)
    Getrandom: uint16 // Get random bytes (getentropy on macOS, getrandom on Linux)
    Gettimeofday: uint16 // Get current time (gettimeofday on macOS, clock_gettime on Linux)
}

let macOSARM64SyscallNumbers : SyscallNumbers = {
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

let linuxARM64SyscallNumbers : SyscallNumbers = {
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

/// Get syscall numbers for the given (OS, Arch) pair.
let syscallNumbersFor (target: Target) : SyscallNumbers =
    match target with
    | ARM64Backend MacOSARM64 -> macOSARM64SyscallNumbers
    | ARM64Backend LinuxARM64 -> linuxARM64SyscallNumbers
    | LinuxX86_64 -> linuxX86_64SyscallNumbers

/// Check if code signing is required for this platform
let requiresCodeSigning (os: OS) : bool =
    match os with
    | MacOS -> true
    | Linux -> false
