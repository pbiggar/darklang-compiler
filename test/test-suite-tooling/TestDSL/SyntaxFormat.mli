(* SyntaxFormat.mli - Canonical Dark syntax fixture representation. *)
type syntaxTest = {name : string; source : string; expectedError : string option; expectedFormat : string option; roundtrip : bool; sourceFile : string}
val parseSyntaxFileContent : string -> string -> (syntaxTest list, string) result
