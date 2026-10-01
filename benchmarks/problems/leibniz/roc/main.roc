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

range : I64 -> List I64
range = \n -> List.range({ start: At(0), end: Before(n) })

series : I64 -> F64
series = \n -> List.walk(range(n), 0, \s, i -> s + (if Num.rem(i, 2) == 0 then 1 else -1) / Num.to_f64(2 * i + 1))

main! = \args ->
    result = Num.floor(4 * series(argument(args, 0)) * 100000000)
    Stdout.line!(Num.to_str(result))
