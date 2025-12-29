let rec ackermann m n =
  if m = 0 then n + 1
  else if n = 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))

(* A(3, 12) = 32765 *)
let () = Printf.printf "%d\n" (ackermann 3 12)
