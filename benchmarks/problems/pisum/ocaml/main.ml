(* main.ml - Parameterized OCaml benchmark implementation. *)
let argument index = int_of_string Sys.argv.(index + 1)
let argument64 index = Int64.of_string Sys.argv.(index + 1)

(* Pi Summation Benchmark *)
(* Computes partial sum of 1/k^2 series (converges to pi^2/6) *)

let pisum rounds n =
  let s = ref 0.0 in
  for _ = 1 to rounds do
    s := 0.0;
    for k = 1 to n do
      s := !s +. 1.0 /. float_of_int (k * k)
    done
  done;
  !s

(* n=10000 gives reasonable runtime *)
let () =
  let result = pisum (argument 0) (argument 1) in
  (* Output as integer (multiply by large factor for precision) *)
  Printf.printf "%Ld\n" (Int64.of_float (result *. 1000000000000.0))
