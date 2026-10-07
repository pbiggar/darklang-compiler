(* Exact .NET TimeSpan tick values at compiler timing boundaries. *)
type t = int64
let zero=0L
let fromTicks ticks=ticks
let ticks value=value
let fromDoubleTicks value=
 if Float.is_nan value then invalid_arg "TimeSpan does not accept floating point Not-a-Number values."
 else if value > Int64.to_float Int64.max_int || value < Int64.to_float Int64.min_int then
  failwith "TimeSpan overflowed because the duration is too long."
 else if value = Int64.to_float Int64.max_int then Int64.max_int
 else Int64.of_float value
let fromMilliseconds value=fromDoubleTicks (value*.10000.)
let fromSeconds value=fromDoubleTicks (value*.10000000.)
let totalMilliseconds value=Int64.to_float value/.10000.
let totalSeconds value=Int64.to_float value/.10000000.
