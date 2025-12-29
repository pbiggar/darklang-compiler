(* Collatz Benchmark *)
(* Counts steps in Collatz sequences for numbers 1 to n *)

let rec collatz_steps n steps =
  if n = 1L then steps
  else if Int64.rem n 2L = 0L then collatz_steps (Int64.div n 2L) (steps + 1)
  else collatz_steps (Int64.add (Int64.mul 3L n) 1L) (steps + 1)

let rec sum_collatz_range i limit total =
  if i > limit then total
  else sum_collatz_range (Int64.add i 1L) limit (total + collatz_steps i 0)

(* Sum steps for numbers 1 to 100000 *)
let () = Printf.printf "%d\n" (sum_collatz_range 1L 100000L 0)
