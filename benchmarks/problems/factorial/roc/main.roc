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

factorial : I64 -> I64
factorial = \n -> if n <= 1 then 1 else n * factorial(n - 1)

main! = \args ->
    result = List.walk(range(argument(args, 0)), 0, \_, _ -> factorial(argument(args, 1)))
    Stdout.line!(Num.to_str(result))
