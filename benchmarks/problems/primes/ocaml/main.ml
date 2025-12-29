(* Prime Counting Benchmark *)
(* Counts primes up to N using trial division *)

let rec isqrt n guess =
  if guess * guess > n then guess - 1
  else if (guess + 1) * (guess + 1) > n then guess
  else isqrt n (guess + 1)

let rec is_divisible n d limit =
  if d > limit then false
  else if n mod d = 0 then true
  else is_divisible n (d + 1) limit

let is_prime n =
  if n < 2 then false
  else if n = 2 then true
  else if n mod 2 = 0 then false
  else
    let limit = isqrt n 1 in
    not (is_divisible n 3 limit)

let rec count_primes n count =
  if n <= 1 then count
  else if is_prime n then count_primes (n - 1) (count + 1)
  else count_primes (n - 1) count

(* Count primes up to 10000 *)
(* Expected: 1229 primes *)
let () = Printf.printf "%d\n" (count_primes 10000 0)
