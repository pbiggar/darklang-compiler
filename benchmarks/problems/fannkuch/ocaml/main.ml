(* Fannkuch Benchmark *)
(* From: Computer Language Benchmarks Game *)

let fannkuch n =
  let perm = Array.init n (fun i -> i) in
  let count = Array.init n (fun i -> i) in
  let max_flips = ref 0 in

  let running = ref true in
  while !running do
    (* Count flips for current permutation *)
    if perm.(0) <> 0 then begin
      let p = Array.copy perm in
      let flips = ref 0 in
      while p.(0) <> 0 do
        let k = p.(0) in
        (* Reverse first k+1 elements *)
        for i = 0 to k / 2 do
          let tmp = p.(i) in
          p.(i) <- p.(k - i);
          p.(k - i) <- tmp
        done;
        incr flips
      done;
      if !flips > !max_flips then
        max_flips := !flips
    end;

    (* Generate next permutation *)
    let i = ref 1 in
    let found = ref false in
    while !i < n && not !found do
      (* Rotate first i+1 elements left by 1 *)
      let t = perm.(0) in
      for j = 0 to !i - 1 do
        perm.(j) <- perm.(j + 1)
      done;
      perm.(!i) <- t;
      count.(!i) <- count.(!i) - 1;
      if count.(!i) > 0 then
        found := true
      else begin
        count.(!i) <- !i;
        incr i
      end
    done;
    if !i >= n then
      running := false
  done;

  !max_flips

(* n=9 gives reasonable runtime *)
let () = Printf.printf "%d\n" (fannkuch 9)
