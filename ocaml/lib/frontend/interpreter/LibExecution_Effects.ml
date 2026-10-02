(*
   Static effects attached to functions.
   Effects describe behavior; they do not grant runtime permission. Keep this
   module independent of RuntimeTypes so it can be used by both ProgramTypes
   and RuntimeTypes without introducing a compile-order cycle.
   A deliberately small initial vocabulary. Add a case only when callers need
   to distinguish it for typechecking, preview, replay, or scheduling.
   The effect for a builtin nobody can scope: it can reach anything on the
   host, and no rule could honestly say otherwise. `Sqlite.query` is the
   canonical case: it is given one database path, but the SQL it runs can
   say `ATTACH '/home/you/.ssh/id_rsa' AS x` and open any file on the
   machine. Checking the database path alone would pretend to confine
   something the runtime cannot see, so the builtin declares `Native`
   instead, which means "granting this hands over the keys". The same
   applies to raw descriptors and process handles (the operation names a
   number, not a resource) and to plain host facts such as `uname`, which
   have nothing to scope. There is deliberately no scoped form: a policy
   grants it whole, with `allow native`, or not at all.
*)
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
(*
   Every effect, in declaration order.
*)
let all = [Http; HttpServer; FileRead; FileWrite; EnvRead; EnvWrite; DbRead; DbWrite;
  Stdin; Stdout; Clock; Random; Process; PackageRead; PackageWrite; TraceRead; TraceWrite; Native]
let fromName wanted = List.find_opt (fun effect_ -> name effect_ = wanted) all
(*
   A scoped effect names a resource (a path, a URL, a table, an executable),
   so its exact request can only be built by the builtin body — or, for the
   OS-facing ones, by the checked host boundary from the `Operation`. An
   ambient effect has no resource and is checked once, from the builtin's
   declared effects, before the body runs.
*)
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
