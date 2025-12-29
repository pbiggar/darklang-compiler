(* Merkle Trees Benchmark *)
(* Builds binary Merkle trees and computes root hashes *)
(* Uses a simple hash function for portability *)

(* Simple hash function (FNV-1a variant) *)
let hash_val data =
  let h = ref 14695981039346656037L in
  for _ = 0 to 7 do
    h := Int64.logxor !h (Int64.logand data 0xFFL);
    h := Int64.mul !h 1099511628211L
  done;
  !h

let hash_pair left right =
  let combined = Int64.add left (Int64.mul right 31L) in
  hash_val combined

(* Build a complete binary Merkle tree of given depth *)
(* Returns the root hash *)
let rec build_tree depth leaf_start =
  if depth = 0 then hash_val leaf_start
  else
    let left_size = Int64.shift_left 1L (depth - 1) in
    let left = build_tree (depth - 1) leaf_start in
    let right = build_tree (depth - 1) (Int64.add leaf_start left_size) in
    hash_pair left right

(* Verify a tree by rebuilding and comparing *)
let verify_tree depth leaf_start expected_root =
  build_tree depth leaf_start = expected_root

let () =
  let depth = 15 in
  let iterations = 50 in

  let checksum = ref 0L in

  for i = 0 to iterations - 1 do
    (* Build tree *)
    let root = build_tree depth (Int64.of_int i) in

    (* Verify tree *)
    let verified = verify_tree depth (Int64.of_int i) root in

    (* Accumulate checksum *)
    checksum := Int64.rem (Int64.add !checksum root) 1000000007L;
    if verified then
      checksum := Int64.rem (Int64.add !checksum 1L) 1000000007L
  done;

  Printf.printf "%Ld\n" !checksum
