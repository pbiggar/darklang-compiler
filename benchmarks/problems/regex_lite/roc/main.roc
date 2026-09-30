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

Atom : [Literal U8, Any, Range U8 U8]
Term : { atom: Atom, minimum: I64, maximum: I64 }

parse_branch : List U8, U64 -> List Term
parse_branch = \source, i ->
    if i >= List.len(source) then [] else
        (atom, next) = when at(source, i) is
            91 -> (Range(at(source, i + 1), at(source, i + 3)), i + 5)
            46 -> (Any, i + 1)
            c -> (Literal(c), i + 1)
        (minimum, maximum, j) = if next >= List.len(source) then (1, 1, next) else when at(source, next) is
            42 -> (0, 2147483647, next + 1)
            43 -> (1, 2147483647, next + 1)
            63 -> (0, 1, next + 1)
            _ -> (1, 1, next)
        List.prepend(parse_branch(source, j), { atom, minimum, maximum })
accepts = \atom, c -> when atom is
    Literal(a) -> a == c
    Any -> Bool.true
    Range(a, b) -> c >= a && c <= b
consume = \term, text, position, count ->
    if count < term.maximum && position + count < Num.to_i64(List.len(text)) && accepts(term.atom, at(text, Num.to_u64(position + count))) then consume(term, text, position, count + 1) else count
matches = \terms, text, position -> when terms is
    [] -> Bool.true
    [term, .. as rest] ->
        count = consume(term, text, position, 0)
        List.any(range(count - term.minimum + 1), \i -> matches(rest, text, position + term.minimum + i))
main! = \args ->
    pattern = List.map(Str.split_on("dark[a-z]*lang|compiler[0-9]+|ab[0-9]?", "|"), \s -> parse_branch(Str.to_utf8(s), 0))
    text = Str.to_utf8(Str.join_with(List.repeat("darklang darkxxlang compiler42 compiler ab ab7 nope DARKlang compilerx\n", Num.to_u64(argument(args, 0))), ""))
    total = List.walk(range(argument(args, 1)), 0, \sum, _ ->
        result = List.walk(range(Num.to_i64(List.len(text))), { count: 0, checksum: 0 }, \s, i ->
            if List.any(pattern, \terms -> matches(terms, text, i)) then { count: s.count + 1, checksum: Num.rem(s.checksum + (i + 1) * (s.count + 3), 1000000007) } else s)
        Num.rem(sum + result.count * 1000003 + result.checksum, 1000000007))
    Stdout.line!(Num.to_str(total))
