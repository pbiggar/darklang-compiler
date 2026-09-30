(* Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md. *)
let argument i = int_of_string Sys.argv.(i+1)
let modulus = 1_000_000_007
let modulo n = ((n mod modulus) + modulus) mod modulus
type atom = Literal of char | Any | Range of char * char
type term = atom * int * int option
let parse_pattern pattern =
  String.split_on_char '|' pattern |> List.map (fun s ->
    let rec parse i = if i=String.length s then [] else
      let atom,next=match s.[i] with
        | '[' -> if i+4>=String.length s || s.[i+2]<>'-' || s.[i+4]<>']' then failwith "bad range" else Range(s.[i+1],s.[i+3]),i+5
        | '.' -> Any,i+1 | c -> Literal c,i+1 in
      let minimum,maximum,next=if next=String.length s then 1,Some 1,next else match s.[next] with
        | '*' -> 0,None,next+1 | '+' -> 1,None,next+1 | '?' -> 0,Some 1,next+1 | _ -> 1,Some 1,next in
      (atom,minimum,maximum)::parse next in parse 0)
let accepts atom c=match atom with Literal a -> a=c | Any -> true | Range(a,b) -> c>=a && c<=b
let rec matches terms text position=match terms with
  | [] -> true
  | (atom,minimum,maximum)::rest ->
    let rec consume count =
      if position+count<String.length text && accepts atom text.[position+count]
         && (match maximum with None -> true | Some m -> count<m) then consume (count+1) else count in
    let count=consume 0 in
    let rec try_count i=i<=count && (matches rest text (position+i) || try_count (i+1)) in try_count minimum
let () =
  let text=String.concat "" (List.init (argument 0) (fun _ -> "darklang darkxxlang compiler42 compiler ab ab7 nope DARKlang compilerx\n")) in
  let pattern=parse_pattern "dark[a-z]*lang|compiler[0-9]+|ab[0-9]?" in
  let total=ref 0 in
  for _=1 to argument 1 do
    let count=ref 0 and checksum=ref 0 in
    for position=0 to String.length text-1 do
      if List.exists (fun terms -> matches terms text position) pattern then
        (checksum:=modulo (!checksum+(position+1)*(!count+3));incr count)
    done;total:=modulo (!total+ !count*1000003+ !checksum)
  done;Printf.printf "%d\n" !total
