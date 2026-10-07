(* ParserDependencies.ml - Share nonempty lists while preserving the interpreter parser API. *)
type 'a neList = 'a NonEmptyList.t = { head : 'a; tail : 'a list }

let ofList head tail = { head; tail }
let toList = NonEmptyList.toList

let ofListWithDefault fallback = function
  | head :: tail -> ofList head tail
  | [] -> NonEmptyList.singleton fallback
