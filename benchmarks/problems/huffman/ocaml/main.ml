(* Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md. *)
let argument i = int_of_string Sys.argv.(i+1)
let modulus = 1_000_000_007
let modulo n = ((n mod modulus) + modulus) mod modulus
type tree = Leaf of int | Branch of tree * tree
let generate n seed =
  let state=ref seed in
  Array.init n (fun _ ->
    state:=(!state*1103515245+12345) mod 2147483648;
    let thresholds=[300;480;610;710;790;850;900;940] in
    let rec choose i=function [] -> 8+ !state mod 24 | t::rest -> if !state mod 1000<t then i else choose (i+1) rest in
    choose 0 thresholds)
let codec data =
  let counts=Array.make 32 0 in
  Array.iter (fun s -> counts.(s)<-counts.(s)+1) data;
  let order (w,s,_) (v,t,_)=let c=compare w v in if c=0 then compare s t else c in
  let queue=List.filter_map (fun s -> if counts.(s)=0 then None else Some (counts.(s),s,Leaf s)) (List.init 32 Fun.id) in
  let rec combine=function
    | [(_,_,tree)] -> tree
    | (w,s,a)::(v,t,b)::rest -> combine (List.sort order ((w+v,min s t,Branch(a,b))::rest))
    | _ -> failwith "empty codec" in
  let tree=combine (List.sort order queue) in
  let codes=Array.make 32 (0,0) in
  let rec visit tree bits length=match tree with
    | Leaf s -> codes.(s)<-bits,max 1 length
    | Branch(a,b) -> visit a (bits*2) (length+1);visit b (bits*2+1) (length+1) in
  visit tree 0 0;tree,codes
let encode codes data =
  Array.to_list data |> List.concat_map (fun s -> let bits,length=codes.(s) in List.init length (fun i -> (bits lsr (length-i-1)) land 1)) |> Array.of_list
let decode tree bits = match tree with
  | Leaf s -> Array.make (Array.length bits) s
  | _ ->
    let current=ref tree and output=ref [] in
    Array.iter (fun bit -> match !current with
      | Leaf _ -> failwith "invalid codec"
      | Branch(a,b) -> current:=(if bit=0 then a else b);
        match !current with Leaf s -> output:=s:: !output;current:=tree | _ -> ()) bits;
    Array.of_list (List.rev !output)
let checksum xs=let s=ref 0 in Array.iteri (fun i v -> s:=modulo (!s+v*(i+1))) xs; !s
let () =
  let data=generate (argument 0) (argument 1) in
  let tree,codes=codec data in
  let total=ref 0 in
  for _=1 to argument 2 do
    let encoded=encode codes data in let decoded=decode tree encoded in
    if decoded<>data then failwith "Huffman roundtrip failed";
    total:=modulo (!total+Array.length encoded*17+checksum encoded+checksum decoded)
  done;Printf.printf "%d\n" !total
