(** Raw 64-bit words, with the explicit mutable operations. *)
type bitset = int64 array
val wordCount : int -> int
val empty : int -> bitset
val all : int -> bitset
val singleton : int -> int -> bitset
val clone : bitset -> bitset
val isEmpty : bitset -> bool
val equal : bitset -> bitset -> bool
val union : bitset -> bitset -> bitset
val diff : bitset -> bitset -> bitset
val intersectMany : bitset -> bitset list -> bitset
val containsIndex : int -> bitset -> bool
val addIndexInPlace : int -> bitset -> unit
val add : int -> bitset -> bitset
val removeIndexInPlace : int -> bitset -> unit
val unionInPlace : bitset -> bitset -> unit
val intersectInPlace : bitset -> bitset -> unit
val diffInPlace : bitset -> bitset -> unit
val intersects : bitset -> bitset -> bool
val iterIndices : bitset -> (int -> unit) -> unit
val count : bitset -> int
val indicesToList : bitset -> int list
