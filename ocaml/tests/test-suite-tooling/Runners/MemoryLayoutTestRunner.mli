(*
   MemoryLayoutTestRunner.fs - Execute source fixtures and observe final native value words.
   The x64 integer printer emits a newline before the separator.
*)
open Dark_compiler
val tests : CompilationContexts.stdlibResult -> string array -> (string * (unit -> (unit,string) result)) list
