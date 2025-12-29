(* Binary Trees Benchmark *)
(* Counts nodes in complete binary trees *)

let rec count_tree depth =
  if depth <= 0 then 1
  else 1 + count_tree (depth - 1) + count_tree (depth - 1)

let rec stress_test depth iterations acc =
  if iterations <= 0 then acc
  else
    let count = count_tree depth in
    stress_test depth (iterations - 1) (acc + count)

(* Run stress test: create many trees of depth 15 *)
(* Each complete binary tree of depth 15 has 2^16 - 1 = 65535 nodes *)
(* Do 100 iterations *)
let () = Printf.printf "%d\n" (stress_test 15 100 0)
