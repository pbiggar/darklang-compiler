(*
   Platform.ml - Platform Detection and Configuration
   Defines OS and CPU architecture types, detection helpers, and
   per-(OS, Arch) syscall number tables.
   Supports:
   - macOS ARM64 (Mach-O binaries, BSD syscalls)
   - Linux ARM64 (ELF binaries, Linux syscalls)
   - Linux x86_64 (ELF binaries, Linux syscalls)
   Supported target platforms
   Supported compiler targets. Unsupported OS/architecture pairs cannot be
   represented after host detection succeeds.
*)
(* Platform.ml - Validate native targets and select unchanged ABI constants. *)
type os = MacOS | Linux

(*
   Supported CPU architectures
*)
type arch = ARM64 | X86_64
type arm64Target = MacOSARM64 | LinuxARM64
type target = ARM64Backend of arm64Target | LinuxX86_64

(*
   Get the current operating system
*)
let detectOS () =
  match BuildPlatform.system with
  | "macosx" -> Ok MacOS
  | "linux" -> Ok Linux
  | _ ->
      Error "Unsupported operating system. Only macOS and Linux are supported."

(*
   Get the current CPU architecture
*)
let detectArch () =
  match BuildPlatform.architecture with
  | "aarch64" | "arm64" -> Ok ARM64
  | "x86_64" | "amd64" -> Ok X86_64
  | arch ->
      Error
        ("Unsupported architecture: " ^ arch
       ^ ". Only ARM64 and x86_64 are supported.")

(*
   Validate an OS/architecture pair as one of the compiler's supported targets.
*)
let targetFor os arch =
  match (os, arch) with
  | MacOS, ARM64 -> Ok (ARM64Backend MacOSARM64)
  | Linux, ARM64 -> Ok (ARM64Backend LinuxARM64)
  | Linux, X86_64 -> Ok LinuxX86_64
  | MacOS, X86_64 -> Error "Unsupported target: macOS x86_64"

(*
   Detect and validate the host target once at compiler initialization.
*)
let detectHostTarget () =
  let os = detectOS () in
  let arch = detectArch () in
  match (os, arch) with
  | Error error, _ | _, Error error -> Error error
  | Ok os, Ok arch -> targetFor os arch

let osFor = function
  | ARM64Backend MacOSARM64 -> MacOS
  | ARM64Backend LinuxARM64 | LinuxX86_64 -> Linux

let archFor = function ARM64Backend _ -> ARM64 | LinuxX86_64 -> X86_64

(*
   Syscall numbers for a specific (OS, Arch) pair.
   On Linux, ARM64 and x86_64 use different numbering schemes.
   Memory map syscall for heap allocation
   Release an independently mapped buffer
   File I/O syscalls
   Open file (or openat on Linux with AT_FDCWD)
   Read from file descriptor
   Close file descriptor
   Get file status (for file size)
   Check file accessibility (for exists)
   Delete file (or unlinkat on Linux with AT_FDCWD)
   Change file mode (or fchmodat on Linux with AT_FDCWD)
   Get random bytes (getentropy on macOS, getrandom on Linux)
   Get current time (gettimeofday on macOS, clock_gettime on Linux)
   Blocking sleep with a normalized timespec
   Create a socket
   Connect to a checked address
   Set socket I/O timeouts
*)
type syscallNumbers = {
  write : int;
  exit : int;
  mmap : int;
  munmap : int;
  open_ : int;
  read : int;
  close : int;
  fstat : int;
  access : int;
  unlink : int;
  chmod : int;
  getrandom : int;
  gettimeofday : int;
  nanosleep : int;
  socket : int;
  connect : int;
  setSockOpt : int;
  bind : int;
  listen : int;
  accept : int;
  fcntl : int;
  poll : int;
  signalMask : int;
    signalPending : int;
  signalWait : int;
  sendTo : int;
  recvFrom : int;
}
let macOSARM64SyscallNumbers : syscallNumbers = {
  write = 4;
    exit = 1;
    mmap = 197;
    munmap = 73;
    open_ = 5;
    read = 3;
    close = 6;
    fstat = 339;
    access = 33;
    unlink = 10;
    chmod = 15;
    getrandom = 439;
    gettimeofday = 116;
    nanosleep = 240;
    socket = 97;
    connect = 98;
    setSockOpt = 105;
    bind = 104;
    listen = 106;
    accept = 30;
    fcntl = 92;
    poll = 230;
    signalMask = 48;
      signalPending = 52;
  signalWait = 330;
  sendTo = 133;
  recvFrom = 29;
}
let linuxARM64SyscallNumbers : syscallNumbers = {
  write = 64;
    exit = 93;
    mmap = 222;
    munmap = 215;
    open_ = 56;
    read = 63;
    close = 57;
    fstat = 80;
    access = 48;
    unlink = 35;
    chmod = 53;
    getrandom = 278;
    gettimeofday = 113;
    nanosleep = 101;
    socket = 198;
    connect = 203;
    setSockOpt = 208;
    bind = 200;
    listen = 201;
    accept = 202;
    fcntl = 25;
    poll = 73;
    signalMask = 135;
      signalPending = 136;
  signalWait = 137;
  sendTo = 206;
  recvFrom = 207;
}
(*
   open (not openat)
   clock_gettime
*)
let linuxX86_64SyscallNumbers : syscallNumbers =
  {
    write = 1;
    exit = 60;
    mmap = 9;
    munmap = 11;
    open_ = 2;
    read = 0;
    close = 3;
    fstat = 5;
    access = 21;
    unlink = 87;
    chmod = 90;
    getrandom = 318;
    gettimeofday = 228;
    nanosleep = 35;
    socket = 41;
    connect = 42;
    setSockOpt = 54;
    bind = 49;
    listen = 50;
    accept = 43;
    fcntl = 72;
    poll = 271;
    signalMask = 14;
      signalPending = 127;
  signalWait = 128;
  sendTo = 44;
  recvFrom = 45;
}
(*
   Get syscall numbers for the given (OS, Arch) pair.
*)
let syscallNumbersFor = function
  | ARM64Backend MacOSARM64 -> macOSARM64SyscallNumbers
  | ARM64Backend LinuxARM64 -> linuxARM64SyscallNumbers
  | LinuxX86_64 -> linuxX86_64SyscallNumbers

(* Mach trap numbers use the negative namespace on Darwin ARM64. *)
let macOSMachTimebaseInfoTrap = -89L

type socketConstants = {
  addressFamily4 : int;
  addressFamily6 : int;
  streamType : int64;
  datagramType : int64;
  socketLevel : int;
  receiveTimeout : int;
  sendTimeout : int;
  reuseAddress : int;
  noSignal : int64;
  blockSignal : int;
  restoreSignal : int;
}

let socketConstantsFor = function
  | MacOS ->
      {
        addressFamily4 = 2;
        addressFamily6 = 30;
        (* Darwin socket() has no SOCK_CLOEXEC flag; emission sets FD_CLOEXEC. *)
        streamType = 1L;
        datagramType = 2L;
        socketLevel = 65535;
        receiveTimeout = 4102;
        sendTimeout = 4101;
        reuseAddress = 4;
        noSignal = 524288L;
        blockSignal = 1;
        restoreSignal = 3;
      }
  | Linux ->
      {
        addressFamily4 = 2;
        addressFamily6 = 10;
        streamType = 524289L;
        datagramType = 524290L;
        socketLevel = 1;
        receiveTimeout = 20;
        sendTimeout = 21;
        reuseAddress = 2;
        noSignal = 16384L;
        blockSignal = 0;
        restoreSignal = 2;
      }

(*
   Check if code signing is required for this platform
*)
let requiresCodeSigning = function MacOS -> true | Linux -> false
