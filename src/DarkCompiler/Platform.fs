// Platform.fs - Platform Detection and Configuration
//
// Detects the current operating system and provides platform-specific
// constants and configurations for binary generation and syscalls.
//
// Supports:
// - macOS ARM64 (Mach-O binaries, BSD syscalls)
// - Linux ARM64 (ELF binaries, Linux syscalls)

module Platform

/// Supported target platforms
type OS =
    | MacOS
    | Linux

/// Platform-specific syscall numbers
type SyscallNumbers = {
    Write: uint16
    Exit: uint16
    SvcImmediate: uint16  // SVC instruction immediate value
}

/// Get the current operating system
let detectOS () : OS =
    if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.OSX) then
        MacOS
    elif System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Linux) then
        Linux
    else
        failwith "Unsupported operating system. Only macOS and Linux ARM64 are supported."

/// Get syscall numbers for the current platform
let getSyscallNumbers (os: OS) : SyscallNumbers =
    match os with
    | MacOS ->
        { Write = 4us
          Exit = 1us
          SvcImmediate = 0x80us }
    | Linux ->
        { Write = 64us
          Exit = 93us
          SvcImmediate = 0us }

/// Check if code signing is required for this platform
let requiresCodeSigning (os: OS) : bool =
    match os with
    | MacOS -> true
    | Linux -> false

/// Get a human-readable platform name
let platformName (os: OS) : string =
    match os with
    | MacOS -> "macOS ARM64"
    | Linux -> "Linux ARM64"
