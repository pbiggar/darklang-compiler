(* Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md. *)
let argument i = int_of_string Sys.argv.(i+1)
let modulus = 1_000_000_007
let modulo n = ((n mod modulus) + modulus) mod modulus
type value = Text of string | Number of int | Boolean of bool | Object of (string * value) list | Array of value list
type node = Literal of string | Print of string | If of bool * string * node list * node list | For of string * string * node list | With of string * string * node list | Call of string * string
type token = Raw of string | Field of string | Control of string
let trim_end s=let rec end_at i=if i>=0 && String.contains " \n\r\t" s.[i] then end_at (i-1) else i+1 in String.sub s 0 (end_at (String.length s-1))
let words s=String.split_on_char ' ' s |> List.filter (fun s -> s<>"")
let scan source =
  let n=String.length source in
  let starts i prefix=i+String.length prefix<=n && String.sub source i (String.length prefix)=prefix in
  let rec until i delimiter=if i>=n then failwith "unclosed token" else if starts i delimiter then i else until (i+1) delimiter in
  let literal=Buffer.create n in
  let flush trim output =
    let text=Buffer.contents literal in
    let text=if trim then trim_end text else text in
    Buffer.clear literal;
    if text="" then output else Raw text::output in
  let rec go i trim_next output =
    if i=n then List.rev (flush false output) else
    if starts i "{#" then go (until (i+2) "#}"+2) trim_next output else
    if starts i "{{" then
      let j=until (i+2) "}}" in let body=String.sub source (i+2) (j-i-2) in
      let left=String.length body>0 && body.[0]='-' and right=String.length body>0 && body.[String.length body-1]='-' in
      let output=flush left output in
      let body=String.trim body in
      let body=if String.starts_with ~prefix:"-" body then String.sub body 1 (String.length body-1) else body in
      let body=if String.ends_with ~suffix:"-" body then String.sub body 0 (String.length body-1) else body in
      go (j+2) right (Control(String.trim body)::output)
    else if starts i "{" then
      let j=until (i+1) "}" in
      let output=flush false output in
      go (j+1) false (Field(String.trim (String.sub source (i+1) (j-i-1)))::output)
    else if trim_next && String.contains " \n\r\t" source.[i] then go (i+1) true output
    else (Buffer.add_char literal source.[i];go (i+1) false output) in
  go 0 false []

let parse source =
  let rec block stops tokens = match tokens with
    | [] -> if stops=[] then [],[] else failwith "unclosed directive"
    | Control c::_ when List.mem c stops -> [],tokens
    | token::rest ->
      let node,tail=match token with
        | Raw s -> Literal s,rest | Field s -> Print s,rest
        | Control c -> match words c with
          | "if"::args ->
            let body,done_tokens=block ["else";"endif"] rest in
            let other,tail=match done_tokens with
              | Control "else"::more -> let other,done_tokens=block ["endif"] more in other,List.tl done_tokens
              | Control "endif"::more -> [],more | _ -> failwith "unclosed if" in
            If(List.hd args="not",List.hd (List.rev args),body,other),tail
          | ["for";alias;"in";path] -> let body,done_tokens=block ["endfor"] rest in For(alias,path,body),List.tl done_tokens
          | ["with";path;"as";alias] -> let body,done_tokens=block ["endwith"] rest in With(path,alias,body),List.tl done_tokens
          | ["call";name;"with";path] -> Call(name,path),rest
          | _ -> failwith ("unknown directive "^c) in
      let more,tail=block stops tail in node::more,tail in
  fst (block [] (scan source))
let field value name=match value with Object fields -> List.assoc name fields | _ -> failwith "field on scalar"
let lookup path root scope=match String.split_on_char '.' path with
  | head::tail ->
    let initial=if head="@root" then root else match List.assoc_opt head scope with Some v -> v | None -> field root head in
    List.fold_left field initial tail
  | [] -> failwith "empty path"
let truth=function Boolean b -> b | Text s -> s<>"" | Number n -> n<>0 | Array xs -> xs<>[] | Object xs -> xs<>[]
let string=function Text s -> s | Number n -> string_of_int n | Boolean b -> string_of_bool b | _ -> failwith "cannot format container"
let escape text =
  let b=Buffer.create (String.length text) in
  String.iter (fun c -> Buffer.add_string b (match c with '&' -> "&amp;" | '<' -> "&lt;" | '>' -> "&gt;" | '"' -> "&quot;" | '\'' -> "&#39;" | _ -> String.make 1 c)) text;
  Buffer.contents b
let rec render engine formatters name root =
  let output=Buffer.create 1024 in
  let rec nodes ast scope=List.iter (node scope) ast
  and node scope = function
    | Literal s -> Buffer.add_string output s
    | Print path ->
      let text=match List.map String.trim (String.split_on_char '|' path) with
        | [p] -> escape (string (lookup p root scope))
        | [p;f] -> (Hashtbl.find formatters f) (lookup p root scope)
        | _ -> failwith "invalid formatter" in Buffer.add_string output text
    | If(neg,path,body,other) -> nodes (if truth (lookup path root scope)<>neg then body else other) scope
    | For(alias,path,body) -> (match lookup path root scope with
      | Array xs -> List.iteri (fun i value -> nodes body ((alias,value)::("@index",Number i)::("@first",Boolean(i=0))::("@last",Boolean(i=List.length xs-1))::scope)) xs
      | _ -> failwith "for requires array")
    | With(path,alias,body) -> nodes body ((alias,lookup path root scope)::scope)
    | Call(name,path) -> Buffer.add_string output (render engine formatters name (lookup path root scope)) in
  nodes (Hashtbl.find engine name) [];Buffer.contents output
let report n=Object ["title",Text "Inventory <nightly>";"empty",Boolean(n=0);"rows",Array(List.init n (fun i -> Object ["name",Text(Printf.sprintf "Item <%d>" i);"featured",Boolean(i mod 3=0);"details",Object ["category",Text(if i mod 2=0 then "hardware" else "software");"price",Number((i+1)*7)];"tags",Array(List.map (fun s -> Text s) ["stable";Printf.sprintf "batch-%d" (i mod 4);"ready & tested"]);"raw_html",Text(Printf.sprintf "<span>SKU-%03d</span>" i)]));"footer",Text "Generated & checked"]
let checksum text=String.fold_left (fun s c -> modulo (s*31+Char.code c)) 0 text
let page="{# TinyTemplate application benchmark #}<main>\n<h1>{ title }</h1>\n{{ if not empty }}<section>{{ for row in rows -}}\n{{ call row with row }}\n{{- endfor }}</section>{{ else }}<p>No inventory.</p>{{ endif }}\n{{ call footer with footer }}\n</main>"
let row_template="<article class=\"{{ if featured }}featured{{ else }}standard{{ endif }}\">\n<h2>{ name }</h2>\n{{ with details as detail }}<p>{ detail.category }: { detail.price | currency }</p>{{ endwith }}\n<ul>{{ for tag in tags }}<li data-first=\"{ @first }\" data-last=\"{ @last }\">{ @index }:{ tag }</li>{{ endfor }}</ul>\n<div>{ raw_html | unescaped }</div>\n</article>"
let () =
  let engine=Hashtbl.create 3 and formatters=Hashtbl.create 2 in
  List.iter (fun (name,text) -> Hashtbl.add engine name (parse text)) ["page",page;"row",row_template;"footer","<footer>{ @root }</footer>"];
  Hashtbl.add formatters "unescaped" string;
  Hashtbl.add formatters "currency" (fun v -> "$"^string v^".00");
  let data=report (argument 0) in
  let total=ref 0 in
  for _=1 to argument 1 do total:=modulo (!total+checksum (render engine formatters "page" data)) done;
  Printf.printf "%d\n" !total
