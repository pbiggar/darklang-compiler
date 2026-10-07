(* NonEmptyList.ml - Preserve semantic collection order and impossible empty failures. *)
type 'a t = { head : 'a; tail : 'a list }

let singleton head = { head; tail = [] }
let cons head value = { head; tail = value.head :: value.tail }
let toList value = value.head :: value.tail
let map fn value = { head = fn value.head; tail = List.map fn value.tail }
let length value = 1 + List.length value.tail
let appendList value items = { value with tail = value.tail @ items }
let snoc value item = appendList value [ item ]
let head value = value.head
let tryFromList = function [] -> None | head :: tail -> Some { head; tail }

let fromList = function
  | [] -> Crash.crash "NonEmptyList.fromList: empty list"
  | head :: tail -> { head; tail }
