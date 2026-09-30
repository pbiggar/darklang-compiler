(* Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md. *)
let argument i = int_of_string Sys.argv.(i+1)
let modulus = 1_000_000_007
let modulo n = ((n mod modulus) + modulus) mod modulus
type token = Number of int | Variable of char | Symbol of char
let lex source =
  let digit c=c>='0' && c<='9' and alpha c=(c>='a' && c<='z') || (c>='A' && c<='Z') in
  let rec scan i output =
    if i=String.length source then Array.of_list (List.rev output) else
    let c=source.[i] in
    if List.mem c [' ';'\n';'\r';'\t'] then scan (i+1) output else
    let rec end_of predicate j=if j<String.length source && predicate source.[j] then end_of predicate (j+1) else j in
    if digit c then let j=end_of digit i in scan j (Number(int_of_string (String.sub source i (j-i)))::output)
    else if alpha c then let j=end_of alpha i in
      let name=String.sub source i (j-i) in
      if name="x" || name="y" then scan j (Variable c::output) else failwith "unknown identifier"
    else if String.contains "+-*/<();" c then scan (i+1) (Symbol c::output) else failwith "invalid token" in
  scan 0 []
let precedence=function '<' -> 1 | '+'|'-' -> 2 | '*'|'/' -> 3 | _ -> 0
let apply op a b=match op with '+' -> a+b | '-' -> a-b | '*' -> a*b | '/' -> a/b | '<' -> if a<b then 1 else 0 | _ -> failwith "bad operator"
let evaluate tokens iteration =
  let index=ref 0 and x=ref 0 and y=ref 0 in
  let rec primary () =
    let token=tokens.(!index) in incr index;
    match token with
    | Number n -> n | Variable c -> if c='x' then !x else !y
    | Symbol '(' -> let v=expression 0 in if tokens.(!index)<>Symbol ')' then failwith "expected )";incr index;v
    | _ -> failwith "invalid primary"
  and expression minimum =
    let rec loop left =
      if !index=Array.length tokens then left else match tokens.(!index) with
      | Symbol op when precedence op>0 && precedence op>=minimum ->
        incr index;let right=expression (precedence op+1) in loop (apply op left right)
      | _ -> left in loop (primary ()) in
  let total=ref 0 and statement=ref 0 in
  while !index<Array.length tokens do
    x:=(iteration*17+ !statement*13) mod 97+3;y:=(iteration*29+ !statement*7) mod 89+5;
    let value=expression 0 in
    if tokens.(!index)<>Symbol ';' then failwith "expected ;";
    incr index;total:=modulo (!total+value*(!statement+1));incr statement
  done; !total
let () =
  let tokens=lex (String.concat "" (List.init (argument 0) (fun _ -> "x * x + y * 3 + (x + y) * (x - y) + x / 2;\n"))) in
  let total=ref 0 in
  for i=0 to argument 1-1 do total:=modulo (!total+evaluate tokens i) done;
  Printf.printf "%d\n" !total
