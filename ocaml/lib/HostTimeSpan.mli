(* Exact .NET TimeSpan tick values at compiler timing boundaries. *)
type t = int64
val zero : t
val fromTicks : int64 -> t
val ticks : t -> int64
val fromMilliseconds : float -> t
val fromSeconds : float -> t
val totalMilliseconds : t -> float
val totalSeconds : t -> float
