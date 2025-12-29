(* Leibniz Pi Benchmark *)
(* Computes pi using Leibniz formula: pi/4 = 1 - 1/3 + 1/5 - 1/7 + ... *)

let rec leibniz_loop i n sum sign =
  if i >= n then sum *. 4.0
  else
    let term = sign /. float_of_int (2 * i + 1) in
    leibniz_loop (i + 1) n (sum +. term) (-.sign)

let leibniz_pi n =
  leibniz_loop 0 n 0.0 1.0

(* Use 100 million iterations for timing *)
(* Output as integer (multiply by large factor for precision) *)
let () = Printf.printf "%Ld\n" (Int64.of_float (leibniz_pi 100000000 *. 100000000.0))
