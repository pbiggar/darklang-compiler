# Parameterized reference workload; provenance in benchmarks/IMPLEMENTATIONS.md.
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br" }
import pf.Arg exposing [Arg]
import pf.Stdout


argument : List Arg, U64 -> I64
argument = \args, index ->
    when List.get(args, index + 1) is
        Ok(arg) ->
            when Str.to_i64(Arg.display(arg)) is
                Ok(n) -> n
                Err(_) -> crash("invalid benchmark argument")
        Err(_) -> crash("missing benchmark argument")

at : List a, U64 -> a
at = \xs, index ->
    when List.get(xs, index) is
        Ok(x) -> x
        Err(_) -> crash("benchmark index out of bounds")

range : I64 -> List I64
range = \n -> List.range({ start: At(0), end: Before(n) })

Value : [Text Str, Number I64, Boolean Bool, Object (Dict Str Value), Array (List Value)]
Node : [Literal Str, Print Str, If Bool Str (List Node) (List Node), For Str Str (List Node), With Str Str (List Node), Call Str Str]
Token : [Raw Str, Field Str, Control Str]

bytes_text = \bytes -> when Str.from_utf8(bytes) is
    Ok(s) -> s
    Err(_) -> crash("invalid UTF8")
part = \chars, start, length -> bytes_text(List.take_first(List.drop_first(chars, start), length))
starts = \chars, i, text ->
    pattern = Str.to_utf8(text)
    i + List.len(pattern) <= List.len(chars) && List.take_first(List.drop_first(chars, i), List.len(pattern)) == pattern
white = \c -> c == 32 || c == 10 || c == 9 || c == 13
trim = \s -> Str.trim(s)
trim_end = \s ->
    reversed = List.reverse(Str.to_utf8(s))
    bytes_text(List.reverse(drop_white(reversed)))
drop_white = \xs -> when xs is
    [c, .. as rest] -> if white(c) then drop_white(rest) else xs
    [] -> []
until_end = \chars, i, delimiter ->
    if i >= List.len(chars) then crash("unclosed token") else if starts(chars, i, delimiter) then i else until_end(chars, i + 1, delimiter)
scan_loop : List U8, U64, Str, Bool, List Token -> List Token
scan_loop = \chars, i, literal, trim_next, reversed ->
    if i >= List.len(chars) then List.reverse(if literal == "" then reversed else List.prepend(reversed, Raw(literal))) else
        if starts(chars, i, "{#") then scan_loop(chars, until_end(chars, i + 2, "#}") + 2, literal, trim_next, reversed)
        else if starts(chars, i, "{{") then
            j = until_end(chars, i + 2, "}}")
            left = at(chars, i + 2) == 45
            right = at(chars, j - 1) == 45
            text = if left then trim_end(literal) else literal
            output = if text == "" then reversed else List.prepend(reversed, Raw(text))
            a = if left then 1 else 0
            b = if right then 1 else 0
            command = trim(part(chars, i + 2 + a, j - i - 2 - a - b))
            scan_loop(chars, j + 2, "", right, List.prepend(output, Control(command)))
        else if starts(chars, i, "{") then
            j = until_end(chars, i + 1, "}")
            output = if literal == "" then reversed else List.prepend(reversed, Raw(literal))
            scan_loop(chars, j + 1, "", Bool.false, List.prepend(output, Field(trim(part(chars, i + 1, j - i - 1)))))
        else if trim_next && white(at(chars, i)) then scan_loop(chars, i + 1, literal, Bool.true, reversed)
        else scan_loop(chars, i + 1, Str.concat(literal, part(chars, i, 1)), Bool.false, reversed)
block : List Token, U64, List Str, List Node -> (List Node, U64, Str)
block = \tokens, i, stops, reversed ->
    if i >= List.len(tokens) then
        if stops != [] then crash("unclosed directive") else (List.reverse(reversed), i, "")
    else when at(tokens, i) is
        Raw(s) -> block(tokens, i + 1, stops, List.prepend(reversed, Literal(s)))
        Field(s) -> block(tokens, i + 1, stops, List.prepend(reversed, Print(s)))
        Control(c) ->
            if List.contains(stops, c) then (List.reverse(reversed), i, c) else
                words = List.keep_if(Str.split_on(c, " "), \word -> word != "")
                (node, next) = when at(words, 0) is
                    "if" ->
                        (body, j, stop) = block(tokens, i + 1, ["else", "endif"], [])
                        (other, end, _) = if stop == "else" then block(tokens, j + 1, ["endif"], []) else ([], j, "")
                        (If(at(words, 1) == "not", at(words, List.len(words) - 1), body, other), end + 1)
                    "for" ->
                        (body, j, _) = block(tokens, i + 1, ["endfor"], [])
                        (For(at(words, 1), at(words, 3), body), j + 1)
                    "with" ->
                        (body, j, _) = block(tokens, i + 1, ["endwith"], [])
                        (With(at(words, 1), at(words, 3), body), j + 1)
                    "call" -> (Call(at(words, 1), at(words, 3)), i + 1)
                    _ -> crash("unknown directive")
                block(tokens, next, stops, List.prepend(reversed, node))
parse = \source ->
    (ast, _, _) = block(scan_loop(Str.to_utf8(source), 0, "", Bool.false, []), 0, [], [])
    ast
field : Value, Str -> Value
field = \value, key -> when value is
    Object(fields) -> when Dict.get(fields, key) is
        Ok(v) -> v
        Err(_) -> crash("missing path")
    _ -> crash("field on scalar")
lookup = \path, root, scope -> when Str.split_on(path, ".") is
    [] -> crash("empty path")
    [head, .. as tail] ->
        initial = if head == "@root" then root else when Dict.get(scope, head) is
            Ok(v) -> v
            Err(_) -> field(root, head)
        List.walk(tail, initial, \value, key -> field(value, key))
truth : Value -> Bool
truth = \value -> when value is
    Boolean(b) -> b
    Text(s) -> s != ""
    Number(n) -> n != 0
    Array(xs) -> !List.is_empty(xs)
    Object(xs) -> !Dict.is_empty(xs)
value_text : Value -> Str
value_text = \value -> when value is
    Text(s) -> s
    Number(n) -> Num.to_str(n)
    Boolean(b) -> if b then "true" else "false"
    _ -> crash("cannot format container")
escape = \s -> Str.join_with(List.map(Str.to_utf8(s), \c -> when c is
    38 -> "&amp;"
    60 -> "&lt;"
    62 -> "&gt;"
    34 -> "&quot;"
    39 -> "&#39;"
    _ -> bytes_text([c])), "")
render : Dict Str (List Node), Dict Str (Value -> Str), Str, Value -> Str
render = \engine, formatters, name, root -> when Dict.get(engine, name) is
    Ok(ast) -> nodes(engine, formatters, ast, root, Dict.empty({}))
    Err(_) -> crash("unknown template")
nodes : Dict Str (List Node), Dict Str (Value -> Str), List Node, Value, Dict Str Value -> Str
nodes = \engine, formatters, ast, root, scope -> Str.join_with(List.map(ast, \node -> when node is
    Literal(s) -> s
    Print(path) -> when List.map(Str.split_on(path, "|"), trim) is
        [p] -> escape(value_text(lookup(p, root, scope)))
        [p, f] -> when Dict.get(formatters, f) is
            Ok(format) -> format(lookup(p, root, scope))
            Err(_) -> crash("unknown formatter")
        _ -> crash("invalid formatter")
    If(neg, path, body, other) -> nodes(engine, formatters, if truth(lookup(path, root, scope)) != neg then body else other, root, scope)
    For(alias, path, body) -> when lookup(path, root, scope) is
        Array(xs) -> Str.join_with(List.map_with_index(xs, \v, i ->
            local = Dict.insert(Dict.insert(Dict.insert(Dict.insert(scope, alias, v), "@index", Number(Num.to_i64(i))), "@first", Boolean(i == 0)), "@last", Boolean(i == List.len(xs) - 1))
            nodes(engine, formatters, body, root, local)), "")
        _ -> crash("for requires array")
    With(path, alias, body) -> nodes(engine, formatters, body, root, Dict.insert(scope, alias, lookup(path, root, scope)))
    Call(name, path) -> render(engine, formatters, name, lookup(path, root, scope))), "")
object = \pairs -> Object(Dict.from_list(pairs))
report = \n ->
    rows = List.map(range(n), \i ->
        digits = Num.to_str(i)
        padding = Str.join_with(List.repeat("0", Num.to_u64(Num.max(0, 3 - Num.to_i64(Str.count_utf8_bytes(digits))))), "")
        object([("name", Text(Str.join_with(["Item <", digits, ">"], ""))), ("featured", Boolean(Num.rem(i, 3) == 0)), ("details", object([("category", Text(if Num.rem(i, 2) == 0 then "hardware" else "software")), ("price", Number((i + 1) * 7))])), ("tags", Array([Text("stable"), Text(Str.concat("batch-", Num.to_str(Num.rem(i, 4)))), Text("ready & tested")])), ("raw_html", Text(Str.join_with(["<span>SKU-", padding, digits, "</span>"], "")))]))
    object([("title", Text("Inventory <nightly>")), ("empty", Boolean(n == 0)), ("rows", Array(rows)), ("footer", Text("Generated & checked"))])
checksum = \s -> List.walk(Str.to_utf8(s), 0, \sum, byte -> Num.rem(sum * 31 + Num.to_i64(byte), 1000000007))
page = "{# TinyTemplate application benchmark #}<main>\n<h1>{ title }</h1>\n{{ if not empty }}<section>{{ for row in rows -}}\n{{ call row with row }}\n{{- endfor }}</section>{{ else }}<p>No inventory.</p>{{ endif }}\n{{ call footer with footer }}\n</main>"
row_template = "<article class=\"{{ if featured }}featured{{ else }}standard{{ endif }}\">\n<h2>{ name }</h2>\n{{ with details as detail }}<p>{ detail.category }: { detail.price | currency }</p>{{ endwith }}\n<ul>{{ for tag in tags }}<li data-first=\"{ @first }\" data-last=\"{ @last }\">{ @index }:{ tag }</li>{{ endfor }}</ul>\n<div>{ raw_html | unescaped }</div>\n</article>"
main! = \args ->
    engine = Dict.from_list([("page", parse(page)), ("row", parse(row_template)), ("footer", parse("<footer>{ @root }</footer>"))])
    formatters = Dict.from_list([("unescaped", value_text), ("currency", \v -> Str.join_with(["$", value_text(v), ".00"], ""))])
    data = report(argument(args, 0))
    total = List.walk(range(argument(args, 1)), 0, \sum, _ -> Num.rem(sum + checksum(render(engine, formatters, "page", data)), 1000000007))
    Stdout.line!(Num.to_str(total))
