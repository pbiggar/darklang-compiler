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

flips : List I64, I64 -> I64
flips = \p, count ->
    k = at(p, 0)
    if k == 0 then count
    else
        next = List.concat(List.reverse(List.take_first(p, Num.to_u64(k + 1))), List.drop_first(p, Num.to_u64(k + 1)))
        flips(next, count + 1)
next_perm : List I64, List I64, U64 -> [Done, Next (List I64) (List I64)]
next_perm = \p, counts, i ->
    if i >= List.len(p) then Done
    else
        rotated = List.map_with_index(p, \x, j -> if j < i then at(p, j + 1) else if j == i then at(p, 0) else x)
        c = at(counts, i) - 1
        updated = List.set(counts, i, (if c > 0 then c else Num.to_i64(i) + 1))
        if c > 0 then Next(rotated, updated) else next_perm(rotated, updated, i + 1)
run : List I64, List I64, I64 -> I64
run = \p, counts, best ->
    current = Num.max(best, flips(p, 0))
    when next_perm(p, counts, 1) is
        Done -> current
        Next(next, c) -> run(next, c, current)

main! = \args ->
    result = run(range(argument(args, 0)), List.map(range(argument(args, 0)), \i -> i + 1), 0)
    Stdout.line!(Num.to_str(result))
