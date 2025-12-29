let rec factorial n =
  if n <= 1L then 1L
  else Int64.mul n (factorial (Int64.sub n 1L))

let rec repeat n acc =
  if n <= 0 then acc
  else repeat (n - 1) (factorial 20L)

let () = Printf.printf "%Ld\n" (repeat 10000 0L)
