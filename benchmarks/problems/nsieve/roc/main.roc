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

mark : List Bool, U64, U64 -> List Bool
mark = \flags, i, j ->
    if j >= List.len(flags) then flags else mark(List.set(flags, j, Bool.false), i, j + i)
sieve_loop : List Bool, U64, I64 -> I64
sieve_loop = \flags, i, count ->
    if i >= List.len(flags) then count
    else if at(flags, i) then sieve_loop(mark(flags, i, 2 * i), i + 1, count + 1)
    else sieve_loop(flags, i + 1, count)
sieve : I64 -> I64
sieve = \n -> sieve_loop(List.repeat(Bool.true, Num.to_u64(n + 1)), 2, 0)

main! = \args ->
    result = List.walk(range(argument(args, 1)), 0, \_, _ -> sieve(argument(args, 0)))
    Stdout.line!(Num.to_str(result))
