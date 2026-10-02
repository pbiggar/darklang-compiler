(* Output.ml - Preserve console output and newline boundaries. *)
let print text = output_string stdout text; flush stdout
let println text = output_string stdout text; output_char stdout '\n'; flush stdout
let eprint text = output_string stderr text; flush stderr
let eprintln text = output_string stderr text; output_char stderr '\n'; flush stderr
