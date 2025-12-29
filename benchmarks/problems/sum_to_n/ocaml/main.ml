let rec sum_to n acc =
  if n <= 0 then acc
  else sum_to (n - 1) (acc + n)

let rec repeat n acc =
  if n <= 0 then acc
  else repeat (n - 1) (sum_to 10000 0)

let () = Printf.printf "%d\n" (repeat 100 0)
