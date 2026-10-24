(* CompilerAnnotations.mli - Per-item compiler decisions kept in imported source comments. *)
type phase = Compile | Runtime
type failureClass = AotRejection | MissingSupport

type disposition =
  | Run
  | Blocked of string
  | Excluded of string
  | Replace of { target : string; reason : string }
  | Xfail of {
      phase : phase;
      diagnostic : string;
      reason : string;
      classification : failureClass;
    }

type item = { position : Tokenizer.pos; isTest : bool }
type annotation = { position : Tokenizer.pos; disposition : disposition }

val read :
  reasons:string list ->
  items:item list ->
  string ->
  (annotation list, string) result

val at : annotation list -> Tokenizer.pos -> disposition
val parseReasons : string -> (string list, string) result
val readReasons : string -> (string list, string) result
