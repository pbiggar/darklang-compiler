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

ack : I64, I64 -> I64
ack = \m, n ->
    if m == 0 then n + 1
    else if n == 0 then ack(m - 1, 1)
    else ack(m - 1, ack(m, n - 1))

main! = \args ->
    result = ack(argument(args, 0), argument(args, 1))
    Stdout.line!(Num.to_str(result))
