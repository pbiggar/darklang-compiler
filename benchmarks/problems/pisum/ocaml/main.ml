(* Pi Summation Benchmark *)
(* Computes partial sum of 1/k^2 series (converges to pi^2/6) *)

let pisum n =
  let s = ref 0.0 in
  for _ = 0 to 499 do
    s := 0.0;
    for k = 1 to n do
      s := !s +. 1.0 /. float_of_int (k * k)
    done
  done;
  !s

(* n=10000 gives reasonable runtime *)
let () =
  let result = pisum 10000 in
  (* Output as integer (multiply by large factor for precision) *)
  Printf.printf "%Ld\n" (Int64.of_float (result *. 1000000000000.0))
