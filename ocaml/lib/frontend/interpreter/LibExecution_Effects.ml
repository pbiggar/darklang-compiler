(* LibExecution_Effects.ml - Preserve declaration order and scoped/ambient distinctions. *)
type effect_ = Http | HttpServer | FileRead | FileWrite | EnvRead | EnvWrite
  | DbRead | DbWrite | Stdin | Stdout | Clock | Random | Process
  | PackageRead | PackageWrite | TraceRead | TraceWrite | Native
let name = function
  | Http -> "http" | HttpServer -> "http-server" | FileRead -> "file-read"
  | FileWrite -> "file-write" | EnvRead -> "env-read" | EnvWrite -> "env-write"
  | DbRead -> "db-read" | DbWrite -> "db-write" | Stdin -> "stdin" | Stdout -> "stdout"
  | Clock -> "clock" | Random -> "random" | Process -> "process"
  | PackageRead -> "package-read" | PackageWrite -> "package-write"
  | TraceRead -> "trace-read" | TraceWrite -> "trace-write" | Native -> "native"
let all = [Http; HttpServer; FileRead; FileWrite; EnvRead; EnvWrite; DbRead; DbWrite;
  Stdin; Stdout; Clock; Random; Process; PackageRead; PackageWrite; TraceRead; TraceWrite; Native]
let fromName wanted = List.find_opt (fun effect_ -> name effect_ = wanted) all
let isScoped = function
  | Http | HttpServer | FileRead | FileWrite | EnvRead | EnvWrite | DbRead | DbWrite | Process -> true
  | Stdin | Stdout | Clock | Random | PackageRead | PackageWrite | TraceRead | TraceWrite | Native -> false
(* The parser refers to F# union case spellings in declared effect_ rows. *)
let caseName = function
  | Http -> "Http" | HttpServer -> "HttpServer" | FileRead -> "FileRead" | FileWrite -> "FileWrite"
  | EnvRead -> "EnvRead" | EnvWrite -> "EnvWrite" | DbRead -> "DbRead" | DbWrite -> "DbWrite"
  | Stdin -> "Stdin" | Stdout -> "Stdout" | Clock -> "Clock" | Random -> "Random" | Process -> "Process"
  | PackageRead -> "PackageRead" | PackageWrite -> "PackageWrite" | TraceRead -> "TraceRead"
  | TraceWrite -> "TraceWrite" | Native -> "Native"
