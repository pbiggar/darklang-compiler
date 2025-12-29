(* Mandelbrot Benchmark *)

let mandelbrot cr ci max_iter =
  let zr = ref 0.0 in
  let zi = ref 0.0 in
  let i = ref 0 in
  let escaped = ref false in
  while !i < max_iter && not !escaped do
    if !zr *. !zr +. !zi *. !zi > 4.0 then
      escaped := true
    else begin
      let new_zr = !zr *. !zr -. !zi *. !zi +. cr in
      let new_zi = 2.0 *. !zr *. !zi +. ci in
      zr := new_zr;
      zi := new_zi;
      incr i
    end
  done;
  if !escaped then 0 else 1

let count_mandelbrot size max_iter =
  let count = ref 0 in
  for y = 0 to size - 1 do
    let ci = 2.0 *. float_of_int y /. float_of_int size -. 1.0 in
    for x = 0 to size - 1 do
      let cr = 2.0 *. float_of_int x /. float_of_int size -. 1.5 in
      count := !count + mandelbrot cr ci max_iter
    done
  done;
  !count

(* Count points in 200x200 grid with 50 iterations *)
let () = Printf.printf "%d\n" (count_mandelbrot 200 50)
