(* main.ml - Parameterized OCaml benchmark implementation. *)
let argument index = int_of_string Sys.argv.(index + 1)
let argument64 index = Int64.of_string Sys.argv.(index + 1)

let rec sum_to n acc =
  if n <= 0 then acc
  else sum_to (n - 1) (acc + n)

let rec repeat n sum_input acc =
  if n <= 0 then acc
  else repeat (n - 1) sum_input (sum_to sum_input 0)

let () = Printf.printf "%d\n" (repeat (argument 0) (argument 1) 0)
