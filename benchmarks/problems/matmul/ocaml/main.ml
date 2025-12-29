(* Matrix Multiplication Benchmark *)
(* Multiplies two NxN matrices using naive O(n^3) algorithm *)

let matmul a b n =
  let c = Array.make_matrix n n 0L in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let s = ref 0L in
      for k = 0 to n - 1 do
        s := Int64.add !s (Int64.mul a.(i).(k) b.(k).(j))
      done;
      c.(i).(j) <- !s
    done
  done;
  c

let generate_matrix n seed =
  let matrix = Array.make_matrix n n 0L in
  let x = ref seed in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      x := Int64.rem (Int64.add (Int64.mul !x 1103515245L) 12345L) (Int64.shift_left 1L 31);
      matrix.(i).(j) <- Int64.rem !x 100L
    done
  done;
  matrix

let checksum m n =
  let result = ref 0L in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      result := Int64.rem (Int64.add !result (Int64.mul m.(i).(j) (Int64.of_int (i * n + j + 1)))) 1000000007L
    done
  done;
  !result

(* Use 100x100 matrices for reasonable runtime *)
let () =
  let n = 100 in
  let a = generate_matrix n 42L in
  let b = generate_matrix n 123L in
  let c = matmul a b n in
  Printf.printf "%Ld\n" (checksum c n)
