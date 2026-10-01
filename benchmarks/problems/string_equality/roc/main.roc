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

main! = \args ->
    token = Arg.display(at(args, 2))
    middle = Str.concat("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
    short = Str.join_with(["ab", token, "cd"], "")
    long = Str.join_with(["prefix:", token, ":", middle, ":suffix"], "")
    short_cases = [short, Str.join_with(["a", "b", token, "cd"], ""), Str.concat(short, "x"), Str.join_with(["xb", token, "cd"], ""), Str.join_with(["ab", token, "ce"], "")]
    long_cases = [long, Str.join_with(["pre", "fix:", token, ":", middle, ":suffix"], ""), Str.concat(long, "!"), Str.join_with(["xrefix:", token, ":", middle, ":suffix"], ""), Str.join_with(["prefix:", token, ":", middle, ":suffiy"], "")]
    total = List.walk(range(argument(args, 0)), 0, \sum, _ ->
        a = List.walk(List.map_with_index(short_cases, \other, i -> (other, i)), sum, \s, pair -> if short == pair.0 then s + Num.pow_int(2, Num.to_u32(pair.1)) else s)
        List.walk(List.map_with_index(long_cases, \other, i -> (other, i)), a, \s, pair -> if long == pair.0 then s + 32 * Num.pow_int(2, Num.to_u32(pair.1)) else s))
    Stdout.line!(Num.to_str(total))
