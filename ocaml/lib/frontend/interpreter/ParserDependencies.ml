(* ParserDependencies.ml - Preserve shared frontend definitions. *)
type 'a neList = { head : 'a; tail : 'a list }
let ofList head tail = {head; tail}
let toList items = items.head :: items.tail
let ofListWithDefault fallback = function head :: tail -> ofList head tail | [] -> ofList fallback []
