(* main.ml - Parameterized OCaml benchmark implementation. *)
let argument index = int_of_string Sys.argv.(index + 1)
let argument64 index = Int64.of_string Sys.argv.(index + 1)

(* Nsieve Benchmark - Sieve of Eratosthenes *)
(* Counts primes up to n using sieve algorithm *)

let nsieve n =
  let is_prime = Array.make (n + 1) true in
  let count = ref 0 in

  for i = 2 to n do
    if is_prime.(i) then begin
      incr count;
      (* Mark multiples as not prime *)
      let j = ref (i + i) in
      while !j <= n do
        is_prime.(!j) <- false;
        j := !j + i
      done
    end
  done;
  !count

(* Run sieve multiple times for meaningful benchmark *)
let () =
  let total = ref 0 in
  for _ = 1 to argument 1 do
    total := nsieve (argument 0)
  done;
  Printf.printf "%d\n" !total
