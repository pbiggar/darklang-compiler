(* N-Queens Benchmark *)
(* Counts solutions to the N-queens problem using bit manipulation *)

let nqueen n =
  let all_ones = (1 lsl n) - 1 in

  let rec solve cols diag1 diag2 =
    if cols = all_ones then 1L
    else begin
      let count = ref 0L in
      let avail = ref (all_ones land (lnot (cols lor diag1 lor diag2))) in
      while !avail <> 0 do
        let pos = !avail land (-(!avail)) in
        avail := !avail - pos;
        count := Int64.add !count (solve (cols lor pos) ((diag1 lor pos) lsl 1) ((diag2 lor pos) lsr 1))
      done;
      !count
    end
  in

  solve 0 0 0

(* N=13 gives reasonable runtime *)
let () = Printf.printf "%Ld\n" (nqueen 13)
