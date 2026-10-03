(* Monotonic host timestamps for System.Diagnostics.Stopwatch-compatible phase timing. *)
external milliseconds : unit -> float = "dark_monotonic_milliseconds"
