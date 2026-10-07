val mapResults : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
(** Sequential, order-preserving list/result transformations. *)

val collectResults :
  ('a -> ('b list, 'e) result) -> 'a list -> ('b list, 'e) result

val sequenceResults : ('a, 'e) result list -> ('a list, 'e) result
val traverse : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
val sequenceOption : ('a, 'e) result option -> ('a option, 'e) result
