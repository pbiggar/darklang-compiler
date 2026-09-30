(* Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md. *)
let argument i = int_of_string Sys.argv.(i+1)
let modulus = 1_000_000_007
let modulo n = ((n mod modulus) + modulus) mod modulus
module Frontier = Map.Make(Int)
let diff left right =
  let n=String.length left and m=String.length right in
  let rec snake x y=if x<n && y<m && left.[x]=right.[y] then snake (x+1) (y+1) else x,y in
  let x,y=snake 0 0 in
  let work=(x+1)*(y+3) in
  let rec layer d previous work =
    let rec diagonals k frontier work reached =
      if k>d then frontier,work,reached else
      let start=if k= -d || (k<>d && Frontier.find (k-1) previous<Frontier.find (k+1) previous)
        then Frontier.find (k+1) previous else Frontier.find (k-1) previous+1 in
      let x,y=snake start (start-k) in
      diagonals (k+2) (Frontier.add k x frontier) (modulo (work+(x+1)*(y+3)+(k+d+1)*17)) (reached || (x>=n && y>=m)) in
    let next,work,reached=diagonals (-d) Frontier.empty work false in
    if reached then d,work else layer (d+1) next work in
  if x=n && y=m then 0,work else layer 1 (Frontier.singleton 0 x) work
let repeat s n=String.concat "" (List.init n (fun _ -> s))
let () =
  let blocks=argument 0 and insertions=argument 1 and runs=argument 2 in
  let unit="darklang compiler benchmark: persistent values and recursive paths.\n" in
  let prefix=repeat unit blocks and suffix=repeat unit (blocks+1) in
  let left=prefix^suffix and right=prefix^repeat "<changed-block>" insertions^suffix in
  let total=ref 0 in
  for _=1 to runs do let d,w=diff left right in total:=modulo (!total+d*1000003+w) done;
  Printf.printf "%d\n" !total
