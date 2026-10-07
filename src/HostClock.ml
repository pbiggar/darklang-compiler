(* Monotonic host timestamps for System.Diagnostics.Stopwatch-compatible phase timing. *)
external milliseconds : unit -> float = "dark_monotonic_milliseconds"
external ticks : unit -> int64 = "dark_monotonic_ticks"
