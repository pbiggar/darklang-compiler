(* main.ml - Parameterized OCaml benchmark implementation. *)
let argument index = int_of_string Sys.argv.(index + 1)
let argument64 index = Int64.of_string Sys.argv.(index + 1)

(* Spectral Norm Benchmark *)
(* From: Computer Language Benchmarks Game *)

let a i j =
  1.0 /. float_of_int ((i + j) * (i + j + 1) / 2 + i + 1)

let av n v out =
  for i = 0 to n - 1 do
    let s = ref 0.0 in
    for j = 0 to n - 1 do
      s := !s +. a i j *. v.(j)
    done;
    out.(i) <- !s
  done

let atv n v out =
  for i = 0 to n - 1 do
    let s = ref 0.0 in
    for j = 0 to n - 1 do
      s := !s +. a j i *. v.(j)
    done;
    out.(i) <- !s
  done

let atav n v out tmp =
  av n v tmp;
  atv n tmp out

let spectral_norm n iterations =
  let u = Array.make n 1.0 in
  let v = Array.make n 0.0 in
  let tmp = Array.make n 0.0 in

  for _ = 1 to iterations do
    atav n u v tmp;
    atav n v u tmp
  done;

  let vbv = ref 0.0 in
  let vv = ref 0.0 in
  for i = 0 to n - 1 do
    vbv := !vbv +. u.(i) *. v.(i);
    vv := !vv +. v.(i) *. v.(i)
  done;

  sqrt (!vbv /. !vv)

(* n = 100 is a reasonable size *)
let () =
  let result = spectral_norm (argument 0) (argument 1) in
  Printf.printf "%Ld\n" (Int64.of_float (result *. 1000000000.0))
