(* Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md. *)
let argument i = int_of_string Sys.argv.(i+1)
let modulus = 1_000_000_007
let modulo n = ((n mod modulus) + modulus) mod modulus
let () =
  let rounds=argument 0 and token=Sys.argv.(2) in
  let middle=String.concat "" (List.init 2 (fun _ -> "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")) in
  let short="ab"^token^"cd" and long="prefix:"^token^":"^middle^":suffix" in
  let short_cases=[short;String.concat "" ["a";"b";token;"cd"];short^"x";"xb"^token^"cd";"ab"^token^"ce"] in
  let long_cases=[long;String.concat "" ["pre";"fix:";token;":";middle;":suffix"];long^"!";"xrefix:"^token^":"^middle^":suffix";"prefix:"^token^":"^middle^":suffiy"] in
  let total=ref 0 in
  for _=1 to rounds do
    List.iteri (fun i other -> if String.equal short other then total:= !total+(1 lsl i)) short_cases;
    List.iteri (fun i other -> if String.equal long other then total:= !total+(32 lsl i)) long_cases
  done;Printf.printf "%d\n" !total
