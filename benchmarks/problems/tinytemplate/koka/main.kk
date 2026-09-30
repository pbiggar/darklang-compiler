// Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
import std/os/env
import std/core/vector
fun argument(i : int) : <ndet,exn> int
  match get-args()[i]
    Just(s) -> match s.parse-int
      Just(n) -> n
      Nothing -> throw("invalid argument")
    Nothing -> throw("missing argument")
val modulus = 1000000007
fun modulo(n : int) : int
  ((n % modulus) + modulus) % modulus
type value
  VText(text : string)
  VNumber(number : int)
  VBool(boolean : bool)
  VObject(fields : list<(string,value)>)
  VArray(items : list<value>)
type node
  Literal(text : string)
  Print(path : string)
  If(negated : bool, path : string, body : list<node>, other : list<node>)
  For(binding : string, path : string, body : list<node>)
  With(path : string, binding : string, body : list<node>)
  Call(name : string, path : string)
type token
  Raw(text : string)
  Field(path : string)
  Control(command : string)
fun part(chars : vector<char>, start : int, length : int) : <div,exn> string
  vector-init(length, fn(i) chars[start + i]).string
fun starts(chars : vector<char>, i : int, text : string) : <div,exn> bool
  val pattern = text.vector
  if i + pattern.length > chars.length then False else
    pattern.list.zip(vector-init(pattern.length, fn(j) chars[i + j]).list).all(fn(pair) pair.fst == pair.snd)
fun scan(source : string) : <div,exn> vector<token>
  val chars = source.vector
  fun until(i : int, delimiter : string) : <div,exn> int
    if i >= chars.length then throw("unclosed token") elif starts(chars,i,delimiter) then i else until(i + 1,delimiter)
  fun go(i : int, literal : string, trim-next : bool, reversed : list<token>) : <div,exn> list<token>
    if i >= chars.length then (if literal == "" then reversed else Cons(Raw(literal),reversed)).reverse else
      if starts(chars,i,"{#") then go(until(i + 2,"#}") + 2,literal,trim-next,reversed)
      elif starts(chars,i,"{{") then
        val j = until(i + 2,"}}")
        val body = part(chars,i + 2,j - i - 2)
        val left = body.vector[0] == '-'
        val right = body.vector[body.vector.length - 1] == '-'
        val text = if left then literal.trim-right else literal
        val output = if text == "" then reversed else Cons(Raw(text),reversed)
        val command = part(chars,i + 2 + (if left then 1 else 0),j - i - 2 - (if left then 1 else 0) - (if right then 1 else 0)).trim
        go(j + 2,"",right,Cons(Control(command),output))
      elif starts(chars,i,"{") then
        val j = until(i + 1,"}")
        val output = if literal == "" then reversed else Cons(Raw(literal),reversed)
        go(j + 1,"",False,Cons(Field(part(chars,i + 1,j - i - 1).trim),output))
      elif trim-next && chars[i].is-white then go(i + 1,literal,True,reversed)
      else go(i + 1,literal ++ chars[i].string,False,reversed)
  go(0,"",False,[]).vector
fun parse(source : string) : <div,exn> list<node>
  val tokens = scan(source)
  fun block(i : int, stops : list<string>, reversed : list<node>) : <div,exn> (list<node>,int,string)
    if i >= tokens.length then
      if !stops.is-empty then throw("unclosed directive")
      (reversed.reverse,i,"")
    else match tokens[i]
      Raw(s) -> block(i + 1,stops,Cons(Literal(s),reversed))
      Field(s) -> block(i + 1,stops,Cons(Print(s),reversed))
      Control(c) ->
        if stops.any(fn(s) s == c) then (reversed.reverse,i,c) else
          val words = c.split(" ").filter(fn(w) w != "").vector
          val (node,next) = match words[0]
            "if" ->
              val (body,j,stop) = block(i + 1,["else","endif"],[])
              val (other,next,_) = if stop == "else" then block(j + 1,["endif"],[]) else ([],j,"")
              (If(words[1] == "not",words[words.length - 1],body,other),next + 1)
            "for" ->
              val (body,j,_) = block(i + 1,["endfor"],[])
              (For(words[1],words[3],body),j + 1)
            "with" ->
              val (body,j,_) = block(i + 1,["endwith"],[])
              (With(words[1],words[3],body),j + 1)
            "call" -> (Call(words[1],words[3]),i + 1)
            _ -> throw("unknown directive")
          block(next,stops,Cons(node,reversed))
  val (nodes,_,_) = block(0,[],[])
  nodes
fun find-value(fields : list<(string,value)>, key : string) : <div,exn> value
  match fields
    Nil -> throw("missing path: " ++ key)
    Cons((name,value),rest) -> if name == key then value else find-value(rest,key)
fun field(value : value, key : string) : <div,exn> value
  match value
    VObject(fields) -> find-value(fields,key)
    _ -> throw("field on scalar")
fun lookup(path : string, root : value, scope : list<(string,value)>) : <div,exn> value
  match path.split(".")
    Nil -> throw("empty path")
    Cons(head,tail) ->
      val scoped = scope.find(fn(pair) pair.fst == head)
      val initial = if head == "@root" then root else match scoped
        Just((_,v)) -> v
        Nothing -> field(root,head)
      tail.foldl(initial,fn(v,key) field(v,key))
fun truth(value : value) : bool
  match value
    VBool(b) -> b
    VText(s) -> s != ""
    VNumber(n) -> n != 0
    VArray(xs) -> !xs.is-empty
    VObject(xs) -> !xs.is-empty
fun text(value : value) : exn string
  match value
    VText(s) -> s
    VNumber(n) -> n.show
    VBool(b) -> if b then "true" else "false"
    _ -> throw("cannot format container")
fun escape(s : string) : string
  s.list.map(fn(c) match c
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '\"' -> "&quot;"
    '\'' -> "&#39;"
    _ -> c.string
  ).join
alias engine = list<(string,list<node>)>
alias formatters = list<(string,value -> exn string)>
fun render(engine : engine, formatters : formatters, name : string, root : value) : <div,exn> string
  fun nodes(ast : list<node>, scope : list<(string,value)>) : <div,exn> string
    ast.map(fn(node) match node
      Literal(s) -> s
      Print(path) -> match path.split("|").map(fn(s) s.trim)
        Cons(p,Nil) -> escape(text(lookup(p,root,scope)))
        Cons(p,Cons(f,Nil)) ->
          match formatters.find(fn(pair) pair.fst == f)
            Just((_,format)) -> format(lookup(p,root,scope))
            Nothing -> throw("unknown formatter")
        _ -> throw("invalid formatter")
      If(neg,path,body,other) -> nodes(if truth(lookup(path,root,scope)) != neg then body else other,scope)
      For(binding,path,body) -> match lookup(path,root,scope)
        VArray(xs) -> xs.map-indexed(fn(i,v) nodes(body,[(binding,v),("@index",VNumber(i)),("@first",VBool(i == 0)),("@last",VBool(i == xs.length - 1))] ++ scope)).join
        _ -> throw("for requires array")
      With(path,binding,body) -> nodes(body,Cons((binding,lookup(path,root,scope)),scope))
      Call(callee,path) -> render(engine,formatters,callee,lookup(path,root,scope))
    ).join
  match engine.find(fn(pair) pair.fst == name)
    Just((_,ast)) -> nodes(ast,[])
    Nothing -> throw("unknown template")
fun report(n : int) : value
  val rows = vector-init(n,fn(i)
    val digits = i.show
    VObject([("name",VText("Item <" ++ digits ++ ">")),("featured",VBool(i % 3 == 0)),("details",VObject([("category",VText(if i % 2 == 0 then "hardware" else "software")),("price",VNumber((i + 1) * 7))])),("tags",VArray([VText("stable"),VText("batch-" ++ (i % 4).show),VText("ready & tested")])),("raw_html",VText("<span>SKU-" ++ "0".repeat(max(0,3 - digits.count)) ++ digits ++ "</span>"))])
  ).list
  VObject([("title",VText("Inventory <nightly>")),("empty",VBool(n == 0)),("rows",VArray(rows)),("footer",VText("Generated & checked"))])
fun checksum(s : string) : int
  s.list.foldl(0,fn(sum,c) (sum * 31 + c.int) % modulus)
val page = "{# TinyTemplate application benchmark #}<main>\n<h1>{ title }</h1>\n{{ if not empty }}<section>{{ for row in rows -}}\n{{ call row with row }}\n{{- endfor }}</section>{{ else }}<p>No inventory.</p>{{ endif }}\n{{ call footer with footer }}\n</main>"
val row-template = "<article class=\"{{ if featured }}featured{{ else }}standard{{ endif }}\">\n<h2>{ name }</h2>\n{{ with details as detail }}<p>{ detail.category }: { detail.price | currency }</p>{{ endwith }}\n<ul>{{ for tag in tags }}<li data-first=\"{ @first }\" data-last=\"{ @last }\">{ @index }:{ tag }</li>{{ endfor }}</ul>\n<div>{ raw_html | unescaped }</div>\n</article>"
pub fun main()
  val engine = [("page",parse(page)),("row",parse(row-template)),("footer",parse("<footer>{ @root }</footer>"))]
  val formatters = [("unescaped",text),("currency",fn(v) "$" ++ text(v) ++ ".00")]
  val data = report(argument(0))
  var total := 0
  for(1,argument(1)) fn(_)
    total := modulo(total + checksum(render(engine,formatters,"page",data)))
  total.println
