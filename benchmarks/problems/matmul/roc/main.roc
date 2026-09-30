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

generate : I64, I64, List I64 -> List I64
generate = \n, seed, xs ->
    if n <= 0 then List.reverse(xs)
    else
        next = Num.rem(seed * 1103515245 + 12345, 2147483648)
        generate(n - 1, next, List.prepend(xs, Num.rem(next, 100)))
solve : I64 -> I64
solve = \n ->
    a = generate(n * n, 42, [])
    b = generate(n * n, 123, [])
    c = List.map(range(n * n), \index ->
        i = index // n
        j = Num.rem(index, n)
        List.walk(range(n), 0, \s, k -> s + at(a, Num.to_u64(i * n + k)) * at(b, Num.to_u64(k * n + j))))
    List.walk(List.map_with_index(c, \x, i -> x * (Num.to_i64(i) + 1)), 0, \s, x -> Num.rem(s + x, 1000000007))

main! = \args ->
    result = solve(argument(args, 0))
    Stdout.line!(Num.to_str(result))
