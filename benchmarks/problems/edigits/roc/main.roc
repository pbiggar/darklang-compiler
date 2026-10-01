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

divide : List I64, I64 -> List I64
divide = \xs, k ->
    state = List.walk(xs, { carry: 0, digits: [] }, \s, d ->
        current = s.carry * 10 + d
        { carry: Num.rem(current, k), digits: List.append(s.digits, current // k) })
    state.digits
add : List I64, List I64 -> List I64
add = \xs, ys ->
    state = List.walk(List.reverse(List.map_with_index(xs, \x, i -> x + at(ys, i))), { carry: 0, digits: [] }, \s, d ->
        current = d + s.carry
        { carry: current // 10, digits: List.append(s.digits, Num.rem(current, 10)) })
    List.reverse(state.digits)
checksum : I64 -> I64
checksum = \n ->
    initial = List.prepend(List.repeat(0, Num.to_u64(n + 10)), 1)
    final = List.walk(range(50), { term: initial, total: initial }, \s, i ->
        term = divide(s.term, i + 1)
        { term, total: add(s.total, term) })
    List.walk(List.map_with_index(List.take_first(final.total, Num.to_u64(n)), \d, i -> d * (Num.to_i64(i) + 1)), 0, \s, x -> Num.rem(s + x, 1000000007))

main! = \args ->
    result = List.walk(range(argument(args, 0)), 0, \_, _ -> checksum(argument(args, 1)))
    Stdout.line!(Num.to_str(result))
