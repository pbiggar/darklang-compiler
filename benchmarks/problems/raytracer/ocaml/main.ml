(* Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md. *)
let argument i = int_of_string Sys.argv.(i+1)
let modulus = 1_000_000_007
let modulo n = ((n mod modulus) + modulus) mod modulus
type vec={x:float;y:float;z:float}
type sphere={center:vec;radius:float;color:vec}
let v x y z={x;y;z}
let add a b=v (a.x+.b.x) (a.y+.b.y) (a.z+.b.z)
let sub a b=v (a.x-.b.x) (a.y-.b.y) (a.z-.b.z)
let scale a s=v (a.x*.s) (a.y*.s) (a.z*.s)
let dot a b=a.x*.b.x+.a.y*.b.y+.a.z*.b.z
let length a=sqrt (dot a a)
let normalize a=scale a (1. /. length a)
let intersect origin direction sphere =
  let offset=sub origin sphere.center in
  let b=dot offset direction and c=dot offset offset-.sphere.radius*.sphere.radius in
  let d=b*.b-.c in
  if d<0. then None else
  let root=sqrt d in let near= -.b-.root and far= -.b+.root in
  if near>0.001 then Some near else if far>0.001 then Some far else None
let spheres=[{center=v 0. 0. 0.;radius=1.;color=v 0.9 0.22 0.18};{center=v (-1.45) (-0.35) 1.3;radius=0.65;color=v 0.18 0.72 0.30};{center=v 1.35 0.15 1.;radius=0.8;color=v 0.18 0.38 0.92};{center=v 0. (-101.) 1.5;radius=100.;color=v 0.72 0.70 0.62}]
let closest origin direction maximum =
  snd (List.fold_left (fun (limit,hit) sphere -> match intersect origin direction sphere with
    | Some distance when distance<limit -> distance,Some(distance,sphere) | _ -> limit,hit) (maximum,None) spheres)
let light=v (-4.) 5. (-3.)
let trace origin direction = match closest origin direction 1e6 with
  | None -> let blend=0.5*.(direction.y+.1.) in v (0.08+.0.12*.blend) (0.10+.0.18*.blend) (0.16+.0.30*.blend)
  | Some(distance,sphere) ->
    let point=add origin (scale direction distance) in
    let normal=normalize (sub point sphere.center) in
    let surface=add point (scale normal 0.001) in
    let toward=sub light surface in let ld=normalize toward in
    let diffuse=max 0. (dot normal ld) in
    let intensity=match closest surface ld (length toward) with Some _ -> 0.12 | None -> 0.12+.0.88*.diffuse in
    scale sphere.color intensity
let render size =
  let total=ref 0 in
  for y=0 to size-1 do for x=0 to size-1 do
    let denominator=float (size-1) in
    let direction=normalize (v (2.*.float x/.denominator-.1.) (1.-.2.*.float y/.denominator) 1.5) in
    let color=trace (v 0. 0. (-5.)) direction in
    let value=int_of_float (color.x*.1e6)*3+int_of_float (color.y*.1e6)*5+int_of_float (color.z*.1e6)*7 in
    total:=modulo (!total+value*(y*size+x+1))
  done done; !total
let () = let total=ref 0 in for _=1 to argument 1 do total:=modulo (!total+render (argument 0)) done;Printf.printf "%d\n" !total
