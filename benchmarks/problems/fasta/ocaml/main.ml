(* Fasta Benchmark - DNA Sequence Generation *)
(* Generates pseudo-random DNA sequences and computes checksum *)

let im = 139968
let ia = 3877
let ic = 29573

let random_state = ref 42

let random_next max =
  random_state := (!random_state * ia + ic) mod im;
  max *. float_of_int !random_state /. float_of_int im

let alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

let make_cumulative table =
  let cp = ref 0.0 in
  Array.iter (fun (c, p) ->
    cp := !cp +. p.(0);
    p.(0) <- !cp
  ) table

let select_random table r =
  let result = ref (fst table.(Array.length table - 1)) in
  Array.iter (fun (c, p) ->
    if r < p.(0) && !result = fst table.(Array.length table - 1) then
      result := c
  ) table;
  !result

let make_random_fasta n table =
  let checksum = ref 0L in
  for i = 0 to n - 1 do
    let r = random_next 1.0 in
    let c = select_random table r in
    checksum := Int64.rem (Int64.add !checksum (Int64.mul (Int64.of_int (Char.code c)) (Int64.of_int (i + 1)))) 1000000007L
  done;
  !checksum

let make_repeat_fasta n seq =
  let checksum = ref 0L in
  let len = String.length seq in
  for i = 0 to n - 1 do
    let c = seq.[i mod len] in
    checksum := Int64.rem (Int64.add !checksum (Int64.mul (Int64.of_int (Char.code c)) (Int64.of_int (i + 1)))) 1000000007L
  done;
  !checksum

let () =
  let n = 100000 in

  let iub = [|
    ('a', [|0.27|]); ('c', [|0.12|]); ('g', [|0.12|]); ('t', [|0.27|]);
    ('B', [|0.02|]); ('D', [|0.02|]); ('H', [|0.02|]); ('K', [|0.02|]);
    ('M', [|0.02|]); ('N', [|0.02|]); ('R', [|0.02|]); ('S', [|0.02|]);
    ('V', [|0.02|]); ('W', [|0.02|]); ('Y', [|0.02|])
  |] in

  let homosapiens = [|
    ('a', [|0.3029549426680|]); ('c', [|0.1979883004921|]);
    ('g', [|0.1975473066391|]); ('t', [|0.3015094502008|])
  |] in

  make_cumulative iub;
  make_cumulative homosapiens;

  (* Generate sequences and compute combined checksum *)
  let c1 = make_repeat_fasta (n * 2) alu in
  let c2 = make_random_fasta (n * 3) iub in
  let c3 = make_random_fasta (n * 5) homosapiens in

  let total = Int64.rem (Int64.add (Int64.add c1 c2) c3) 1000000007L in
  Printf.printf "%Ld\n" total
