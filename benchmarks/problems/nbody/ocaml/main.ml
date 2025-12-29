(* N-Body Benchmark *)
(* From: Computer Language Benchmarks Game *)

let pi = 4.0 *. atan 1.0
let solar_mass = 4.0 *. pi *. pi
let days_per_year = 365.24

type body = {
  mutable x: float; mutable y: float; mutable z: float;
  mutable vx: float; mutable vy: float; mutable vz: float;
  mass: float
}

let advance bodies dt n =
  for _ = 1 to n do
    let len = Array.length bodies in
    for i = 0 to len - 1 do
      for j = i + 1 to len - 1 do
        let dx = bodies.(i).x -. bodies.(j).x in
        let dy = bodies.(i).y -. bodies.(j).y in
        let dz = bodies.(i).z -. bodies.(j).z in

        let dist = sqrt (dx *. dx +. dy *. dy +. dz *. dz) in
        let mag = dt /. (dist *. dist *. dist) in

        bodies.(i).vx <- bodies.(i).vx -. dx *. bodies.(j).mass *. mag;
        bodies.(i).vy <- bodies.(i).vy -. dy *. bodies.(j).mass *. mag;
        bodies.(i).vz <- bodies.(i).vz -. dz *. bodies.(j).mass *. mag;

        bodies.(j).vx <- bodies.(j).vx +. dx *. bodies.(i).mass *. mag;
        bodies.(j).vy <- bodies.(j).vy +. dy *. bodies.(i).mass *. mag;
        bodies.(j).vz <- bodies.(j).vz +. dz *. bodies.(i).mass *. mag
      done
    done;
    Array.iter (fun body ->
      body.x <- body.x +. dt *. body.vx;
      body.y <- body.y +. dt *. body.vy;
      body.z <- body.z +. dt *. body.vz
    ) bodies
  done

let energy bodies =
  let e = ref 0.0 in
  let len = Array.length bodies in
  for i = 0 to len - 1 do
    e := !e +. 0.5 *. bodies.(i).mass *. (
      bodies.(i).vx *. bodies.(i).vx +.
      bodies.(i).vy *. bodies.(i).vy +.
      bodies.(i).vz *. bodies.(i).vz);
    for j = i + 1 to len - 1 do
      let dx = bodies.(i).x -. bodies.(j).x in
      let dy = bodies.(i).y -. bodies.(j).y in
      let dz = bodies.(i).z -. bodies.(j).z in
      let dist = sqrt (dx *. dx +. dy *. dy +. dz *. dz) in
      e := !e -. (bodies.(i).mass *. bodies.(j).mass) /. dist
    done
  done;
  !e

let offset_momentum bodies =
  let px = ref 0.0 in
  let py = ref 0.0 in
  let pz = ref 0.0 in
  Array.iter (fun body ->
    px := !px -. body.vx *. body.mass;
    py := !py -. body.vy *. body.mass;
    pz := !pz -. body.vz *. body.mass
  ) bodies;
  bodies.(0).vx <- !px /. solar_mass;
  bodies.(0).vy <- !py /. solar_mass;
  bodies.(0).vz <- !pz /. solar_mass

let () =
  let bodies = [|
    (* Sun *)
    { x = 0.0; y = 0.0; z = 0.0; vx = 0.0; vy = 0.0; vz = 0.0; mass = solar_mass };
    (* Jupiter *)
    { x = 4.84143144246472090e+00; y = -1.16032004402742839e+00; z = -1.03622044471123109e-01;
      vx = 1.66007664274403694e-03 *. days_per_year; vy = 7.69901118419740425e-03 *. days_per_year;
      vz = -6.90460016972063023e-05 *. days_per_year; mass = 9.54791938424326609e-04 *. solar_mass };
    (* Saturn *)
    { x = 8.34336671824457987e+00; y = 4.12479856412430479e+00; z = -4.03523417114321381e-01;
      vx = -2.76742510726862411e-03 *. days_per_year; vy = 4.99852801234917238e-03 *. days_per_year;
      vz = 2.30417297573763929e-05 *. days_per_year; mass = 2.85885980666130812e-04 *. solar_mass };
    (* Uranus *)
    { x = 1.28943695621391310e+01; y = -1.51111514016986312e+01; z = -2.23307578892655734e-01;
      vx = 2.96460137564761618e-03 *. days_per_year; vy = 2.37847173959480950e-03 *. days_per_year;
      vz = -2.96589568540237556e-05 *. days_per_year; mass = 4.36624404335156298e-05 *. solar_mass };
    (* Neptune *)
    { x = 1.53796971148509165e+01; y = -2.59193146099879641e+01; z = 1.79258772950371181e-01;
      vx = 2.68067772490389322e-03 *. days_per_year; vy = 1.62824170038242295e-03 *. days_per_year;
      vz = -9.51592254519715870e-05 *. days_per_year; mass = 5.15138902046611451e-05 *. solar_mass }
  |] in

  offset_momentum bodies;
  advance bodies 0.01 500000;
  Printf.printf "%Ld\n" (Int64.of_float (energy bodies *. 1000000.0))
