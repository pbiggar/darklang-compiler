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

a : I64, I64 -> F64
a = \i, j -> 1 / Num.to_f64((i + j) * (i + j + 1) // 2 + i + 1)
product : I64, List F64, Bool -> List F64
product = \n, v, transpose -> List.map(range(n), \i ->
    List.walk(range(n), 0, \s, j -> s + (if transpose then a(j, i) else a(i, j)) * at(v, Num.to_u64(j))))
atav = \n, v -> product(n, product(n, v, Bool.false), Bool.true)
solve : I64, I64 -> F64
solve = \n, rounds ->
    final = List.walk(range(rounds), { u: List.repeat(1, Num.to_u64(n)), v: List.repeat(0, Num.to_u64(n)) }, \s, _ ->
        v = atav(n, s.u)
        { u: atav(n, v), v })
    vbv = List.walk(List.map_with_index(final.u, \x, i -> x * at(final.v, i)), 0, \s, x -> s + x)
    vv = List.walk(final.v, 0, \s, x -> s + x * x)
    Num.sqrt(vbv / vv)

main! = \args ->
    result = Num.floor(solve(argument(args, 0), argument(args, 1)) * 1000000000)
    Stdout.line!(Num.to_str(result))
