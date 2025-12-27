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

/// Platform-specific syscall numbers and register conventions
type SyscallNumbers = {
    Write: uint16
    Exit: uint16
    Mmap: uint16  // Memory map syscall for heap allocation
    // File I/O syscalls
    Open: uint16   // Open file (or openat on Linux with AT_FDCWD)
    Read: uint16   // Read from file descriptor
    Close: uint16  // Close file descriptor
    Fstat: uint16  // Get file status (for file size)
    Access: uint16 // Check file accessibility (for exists)
    SvcImmediate: uint16  // SVC instruction immediate value
    SyscallRegister: ARM64.Reg  // Register to hold syscall number (X16 for macOS, X8 for Linux)
}

/// Get the current operating system
let detectOS () : Result<OS, string> =
    if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.OSX) then
        Ok MacOS
    elif System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Linux) then
        Ok Linux
    else
        Error "Unsupported operating system. Only macOS and Linux ARM64 are supported."

/// Get syscall numbers for the current platform
let getSyscallNumbers (os: OS) : SyscallNumbers =
    match os with
    | MacOS ->
        // macOS ARM64 BSD syscall numbers
        { Write = 4us
          Exit = 1us
          Mmap = 197us
          Open = 5us       // open(path, flags, mode)
          Read = 3us       // read(fd, buf, count)
          Close = 6us      // close(fd)
          Fstat = 339us    // fstat(fd, statbuf) - uses fstat64 on macOS
          Access = 33us    // access(path, mode)
          SvcImmediate = 0x80us
          SyscallRegister = ARM64.X16 }
    | Linux ->
        // Linux ARM64 syscall numbers
        { Write = 64us
          Exit = 93us
          Mmap = 222us
          Open = 56us      // openat(dirfd, path, flags, mode) - use AT_FDCWD=-100 for dirfd
          Read = 63us      // read(fd, buf, count)
          Close = 57us     // close(fd)
          Fstat = 80us     // fstat(fd, statbuf)
          Access = 48us    // faccessat(dirfd, path, mode, flags) - use AT_FDCWD=-100
          SvcImmediate = 0us
          SyscallRegister = ARM64.X8 }

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
