# Parameterized reference workload; provenance in benchmarks/IMPLEMENTATIONS.md.
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br" }
import pf.Arg exposing [Arg]
import pf.Stdout
import Quicksort

argument : List Arg, U64 -> I64
argument = \args, index ->
    when List.get(args, index + 1) is
        Ok(arg) ->
            when Str.to_i64(Arg.display(arg)) is
                Ok(n) -> n
                Err(_) -> crash("invalid benchmark argument")
        Err(_) -> crash("missing benchmark argument")

generate : I64, I64, List I64 -> List I64
generate = \n, seed, xs ->
    if n <= 0 then List.reverse(xs)
    else
        next = Num.rem(seed * 1103515245 + 12345, 2147483648)
        generate(n - 1, next, List.prepend(xs, Num.rem(next, 10000)))
checksum : List I64 -> I64
checksum = \xs ->
    List.walk(List.map_with_index(xs, \x, i -> x * (Num.to_i64(i) + 1)), 0, \s, x -> Num.rem(s + x, 1000000007))

main! = \args ->
    result = checksum(Quicksort.sort_by(generate(argument(args, 0), argument(args, 1), []), \x -> x))
    Stdout.line!(Num.to_str(result))
