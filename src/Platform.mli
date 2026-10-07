(** Validated native targets, host detection, and exact syscall tables. *)
type os = MacOS | Linux

type arch = ARM64 | X86_64
type arm64Target = MacOSARM64 | LinuxARM64
type target = ARM64Backend of arm64Target | LinuxX86_64

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
}

type socketConstants = {
  addressFamily4 : int;
  addressFamily6 : int;
  streamCloexec : int64;
  datagramCloexec : int64;
  socketLevel : int;
  receiveTimeout : int;
  sendTimeout : int;
  reuseAddress : int;
  noSignal : int64;
  blockSignal : int;
  restoreSignal : int;
}

val detectOS : unit -> (os, string) result
val detectArch : unit -> (arch, string) result
val targetFor : os -> arch -> (target, string) result
val detectHostTarget : unit -> (target, string) result
val osFor : target -> os
val archFor : target -> arch
val macOSARM64SyscallNumbers : syscallNumbers
val linuxARM64SyscallNumbers : syscallNumbers
val linuxX86_64SyscallNumbers : syscallNumbers
val syscallNumbersFor : target -> syscallNumbers
val socketConstantsFor : os -> socketConstants
val requiresCodeSigning : os -> bool
