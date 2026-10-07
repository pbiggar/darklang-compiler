(*
   ParserDependencies.ml - Small interfaces used by the copied interpreter parser.
   These definitions preserve the interpreter parser's existing calls without
   changing the copied source files. They are limited to symbols used by the
   lexer, parser, WrittenTypes, and validation modules.
*)
(* ParserDependencies.ml - Preserve shared frontend definitions. *)
type 'a neList = { head : 'a; tail : 'a list }
let ofList head tail = {head; tail}
let toList items = items.head :: items.tail
let ofListWithDefault fallback = function head :: tail -> ofList head tail | [] -> ofList fallback []
