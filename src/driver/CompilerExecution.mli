(* CompilerExecution.mli - Run generated binaries through the host process boundary. *)
val executeCapturedWithArgumentsAndEnvironment : Platform.target -> int -> string list -> (string*string) list -> CompilerOptions.executionInput -> bytes -> CompilerOptions.executionOutput
val executeCapturedWithArguments : Platform.target -> int -> string list -> CompilerOptions.executionInput -> bytes -> CompilerOptions.executionOutput
val executeCaptured : Platform.target -> int -> CompilerOptions.executionInput -> bytes -> CompilerOptions.executionOutput
val execute : Platform.target -> int -> bytes -> CompilerOptions.executionOutput
val executeAttached : Platform.target -> int -> bytes -> CompilerOptions.executionOutput
