// PlatformTypes.fs - Platform Type Definitions
//
// Defines OS and Arch discriminated unions and detection functions.
// Separated from Platform.fs so these types are available early in the
// compilation order (before the register allocator).

module PlatformTypes

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
