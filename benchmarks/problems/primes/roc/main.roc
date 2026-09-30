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

prime : I64 -> Bool
prime = \n ->
    if n < 2 then Bool.false
    else if n == 2 then Bool.true
    else if Num.rem(n, 2) == 0 then Bool.false
    else divisors(n, 3, Num.floor(Num.sqrt(Num.to_f64(n))))
divisors : I64, I64, I64 -> Bool
divisors = \n, d, limit ->
    if d > limit then Bool.true
    else if Num.rem(n, d) == 0 then Bool.false
    else divisors(n, d + 1, limit)

main! = \args ->
    result = List.walk(range(argument(args, 0) - 1), 0, \s, i -> s + (if prime(i + 2) then 1 else 0))
    Stdout.line!(Num.to_str(result))
