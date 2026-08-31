(* main.ml - Parameterized OCaml benchmark implementation. *)
let argument index = int_of_string Sys.argv.(index + 1)
let argument64 index = Int64.of_string Sys.argv.(index + 1)

let rec factorial n =
  if n <= 1L then 1L
  else Int64.mul n (factorial (Int64.sub n 1L))

let rec repeat n acc =
  if n <= 0 then acc
  else repeat (n - 1) (factorial (argument64 1))

let () = Printf.printf "%Ld\n" (repeat (argument 0) 0L)
