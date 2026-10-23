(* SourceInventory.mli - Discover source items independently of compilation and execution. *)
type kind =
  | Function
  | Value
  | Type
  | Trait
  | Implementation
  | TraitMethod
  | NamedTest
  | Assertion
  | Expression

type entry = {
  name : string;
  kind : kind;
  position : Tokenizer.pos;
  disposition : CompilerAnnotations.disposition;
}

type source = { tree : WrittenTypes.sourceFile; entries : entry list }

val kindName : kind -> string
val isTest : kind -> bool
val parse : reasons:string list -> string -> (source, string) result
