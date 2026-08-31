(* main.ml - Parameterized OCaml benchmark implementation. *)
let argument index = int_of_string Sys.argv.(index + 1)
let argument64 index = Int64.of_string Sys.argv.(index + 1)

(* Tak (Takeuchi) Benchmark *)
(* Tests recursion and function call overhead *)

let rec tak x y z =
  if x <= y then z
  else tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)

(* Repeat multiple times for meaningful measurement *)
let rec repeat n x y z acc =
  if n <= 0 then acc
  else repeat (n - 1) x y z (tak x y z)

let () = Printf.printf "%d\n" (repeat (argument 0) (argument 1) (argument 2) (argument 3) 0)
