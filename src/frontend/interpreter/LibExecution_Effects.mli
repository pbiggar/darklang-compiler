(* LibExecution_Effects.mli - Static effect_ vocabulary independent of runtime types. *)
type effect_ =
  | Http
  | HttpServer
  | FileRead
  | FileWrite
  | EnvRead
  | EnvWrite
  | DbRead
  | DbWrite
  | Stdin
  | Stdout
  | Clock
  | Random
  | Process
  | PackageRead
  | PackageWrite
  | TraceRead
  | TraceWrite
  | Native

val name : effect_ -> string
val all : effect_ list
val fromName : string -> effect_ option
val isScoped : effect_ -> bool
val caseName : effect_ -> string
