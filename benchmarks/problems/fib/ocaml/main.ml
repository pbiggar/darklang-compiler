(* main.ml - Parameterized OCaml benchmark implementation. *)
let argument index = int_of_string Sys.argv.(index + 1)
let argument64 index = Int64.of_string Sys.argv.(index + 1)

let rec fib n =
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)

let () = Printf.printf "%d\n" (fib (argument 0))
