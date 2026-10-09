(* Oracle.mli - Compare accepted interpreter results with native compiler execution. *)
type outcome =
  | Passed
  | Unsupported of string
  | OracleFailed of string
  | CompilerRejected of string * string
  | CompilerCrashed of string * string
  | NativeFailed of string * int * string * string
  | ResultMismatch of string * string

val check :
  string ->
  int ->
  Dark_compiler.CompilationContexts.stdlibResult ->
  string ->
  outcome

val describe : outcome -> string
val sameFailure : outcome -> outcome -> bool
