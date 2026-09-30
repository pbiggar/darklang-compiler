(* Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md. *)
let argument i = int_of_string Sys.argv.(i+1)
let modulus = 1_000_000_007
let modulo n = ((n mod modulus) + modulus) mod modulus
let rec fft values =
  let n=Array.length values in
  if n<=1 then values else
  let even=fft (Array.init (n/2) (fun i -> values.(i*2))) in
  let odd=fft (Array.init (n/2) (fun i -> values.(i*2+1))) in
  let result=Array.make n Complex.zero in
  for i=0 to n/2-1 do
    let angle= -.2. *. Float.pi *. float i /. float n in
    let twiddle=Complex.mul {Complex.re=cos angle;im=sin angle} odd.(i) in
    result.(i)<-Complex.add even.(i) twiddle;
    result.(i+n/2)<-Complex.sub even.(i) twiddle
  done;result
let () =
  let n=argument 0 and runs=argument 1 in
  let values=Array.init n (fun i -> let x=float i in
    {Complex.re=sin (x*.0.017)+.cos (x*.0.031);im=cos (x*.0.013)-.sin (x*.0.007)}) in
  let total=ref 0 in
  for _=1 to runs do
    Array.iteri (fun i z -> total:=modulo (!total+int_of_float ((z.Complex.re*.3.+.z.Complex.im*.5.)*.1e6)*(i+1))) (fft values)
  done;Printf.printf "%d\n" !total
