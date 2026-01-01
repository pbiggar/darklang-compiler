(* Quicksort Benchmark *)
(* Sorts a list and returns a checksum *)

let rec quicksort arr =
  match arr with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let len = List.length arr in
    let pivot = List.nth arr (len / 2) in
    let left = List.filter (fun x -> x < pivot) arr in
    let middle = List.filter (fun x -> x = pivot) arr in
    let right = List.filter (fun x -> x > pivot) arr in
    quicksort left @ middle @ quicksort right

let generate_list n seed =
  let x = ref seed in
  let result = ref [] in
  for _ = 1 to n do
    x := Int64.rem (Int64.add (Int64.mul !x 1103515245L) 12345L) (Int64.shift_left 1L 31);
    result := Int64.rem !x 10000L :: !result
  done;
  List.rev !result

let checksum arr =
  let result = ref 0L in
  List.iteri (fun i x ->
    result := Int64.rem (Int64.add !result (Int64.rem (Int64.mul x (Int64.of_int (i + 1))) 1000000007L)) 1000000007L
  ) arr;
  !result

let () =
  let arr = generate_list 5000 42L in
  let sorted_arr = quicksort arr in
  Printf.printf "%Ld\n" (checksum sorted_arr)
