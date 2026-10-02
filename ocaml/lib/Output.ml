(*
   Output.fs - Output helper functions
   Provides simple print functions for stdout and stderr.
   These functions use string interpolation and handle newlines explicitly.
*)
(* Output.ml - Preserve console output and newline boundaries. *)
(*
   Print to stdout without newline
*)
let print text = output_string stdout text; flush stdout
(*
   Print to stdout with newline
*)
let println text = output_string stdout text; output_char stdout '\n'; flush stdout
(*
   Print to stderr without newline
*)
let eprint text = output_string stderr text; flush stderr
(*
   Print to stderr with newline
*)
let eprintln text = output_string stderr text; output_char stderr '\n'; flush stderr
