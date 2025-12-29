(* Edigits Benchmark - Computing digits of e *)
(* Uses series expansion: e = sum(1/n!) for n=0 to infinity *)
(* Computes checksum of first N digits *)

let compute_e_digits num_digits =
  (* We need extra precision for computation *)
  let precision = num_digits + 10 in
  let base_val = 10 in

  (* Work with scaled integers *)
  let e_scaled = Array.make (precision + 1) 0 in
  e_scaled.(0) <- 1; (* represents 1.0 *)

  (* Add 1/n! for n = 1, 2, 3, ... *)
  let term = Array.make (precision + 1) 0 in
  term.(0) <- 1; (* start with 1 *)

  for n = 1 to 50 do
    (* term = term / n *)
    let carry = ref 0 in
    for i = 0 to precision do
      let current = !carry * base_val + term.(i) in
      term.(i) <- current / n;
      carry := current mod n
    done;

    (* e_scaled += term *)
    let carry2 = ref 0 in
    for i = precision downto 0 do
      let sum = e_scaled.(i) + term.(i) + !carry2 in
      e_scaled.(i) <- sum mod base_val;
      carry2 := sum / base_val
    done
  done;

  (* Extract digits *)
  Array.sub e_scaled 0 num_digits

(* Compute first 1000 digits of e multiple times *)
let () =
  let checksum = ref 0L in
  for _ = 1 to 10 do
    let digits = compute_e_digits 1000 in
    checksum := 0L;
    for i = 0 to Array.length digits - 1 do
      checksum := Int64.rem (Int64.add !checksum (Int64.mul (Int64.of_int digits.(i)) (Int64.of_int (i + 1)))) 1000000007L
    done
  done;
  Printf.printf "%Ld\n" !checksum
