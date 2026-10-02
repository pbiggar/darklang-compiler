(* Platform.ml - Validate native targets and select unchanged ABI constants. *)
type os = MacOS | Linux
type arch = ARM64 | X86_64
type arm64Target = MacOSARM64 | LinuxARM64
type target = ARM64Backend of arm64Target | LinuxX86_64
external hostIdentity : unit -> string * string = "dark_compiler_host_identity"
let detectOS () =
  match fst (hostIdentity ()) with
  | "Darwin" -> Ok MacOS
  | "Linux" -> Ok Linux
  | _ -> Error "Unsupported operating system. Only macOS and Linux are supported."
let detectArch () =
  match snd (hostIdentity ()) with
  | "aarch64" | "arm64" -> Ok ARM64
  | "x86_64" | "amd64" -> Ok X86_64
  | arch -> Error ("Unsupported architecture: " ^ arch ^ ". Only ARM64 and x86_64 are supported.")
let targetFor os arch =
  match os, arch with
  | MacOS, ARM64 -> Ok (ARM64Backend MacOSARM64)
  | Linux, ARM64 -> Ok (ARM64Backend LinuxARM64)
  | Linux, X86_64 -> Ok LinuxX86_64
  | MacOS, X86_64 -> Error "Unsupported target: macOS x86_64"
let detectHostTarget () =
  let os = detectOS () in
  let arch = detectArch () in
  match os, arch with
  | Error error, _ | _, Error error -> Error error
  | Ok os, Ok arch -> targetFor os arch
let osFor = function ARM64Backend MacOSARM64 -> MacOS | ARM64Backend LinuxARM64 | LinuxX86_64 -> Linux
let archFor = function ARM64Backend _ -> ARM64 | LinuxX86_64 -> X86_64
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
}
let linuxX86_64SyscallNumbers : syscallNumbers = {
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
}
let syscallNumbersFor = function
  | ARM64Backend MacOSARM64 -> macOSARM64SyscallNumbers
  | ARM64Backend LinuxARM64 -> linuxARM64SyscallNumbers
  | LinuxX86_64 -> linuxX86_64SyscallNumbers
type socketConstants = {
  addressFamily4 : int;
  addressFamily6 : int;
  streamCloexec : int64;
  datagramCloexec : int64;
  socketLevel : int;
  receiveTimeout : int;
  sendTimeout : int;
}
let socketConstantsFor = function
  | MacOS -> {
      addressFamily4 = 2;
      addressFamily6 = 30;
      streamCloexec = 268435457L;
      datagramCloexec = 268435458L;
      socketLevel = 65535;
      receiveTimeout = 4102;
      sendTimeout = 4101;
    }
  | Linux -> {
      addressFamily4 = 2;
      addressFamily6 = 10;
      streamCloexec = 524289L;
      datagramCloexec = 524290L;
      socketLevel = 1;
      receiveTimeout = 20;
      sendTimeout = 21;
    }
let requiresCodeSigning = function MacOS -> true | Linux -> false
