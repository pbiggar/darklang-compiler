(* Native file boundaries with frozen .NET path, decoding and I/O diagnostics. *)
val absolutePath : string -> string
val errorMessage : string -> exn -> string
val exists : string -> bool
val readText : string -> string
val writeBytes : string -> bytes -> (unit,string) result
