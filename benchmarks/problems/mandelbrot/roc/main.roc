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

inside : I64, F64, F64, I64, F64, F64 -> I64
inside = \limit, cr, ci, i, zr, zi ->
    if i >= limit then 1
    else if zr * zr + zi * zi > 4 then 0
    else inside(limit, cr, ci, i + 1, zr * zr - zi * zi + cr, 2 * zr * zi + ci)
solve : I64, I64 -> I64
solve = \n, limit ->
    List.walk(range(n), 0, \s, y ->
        List.walk(range(n), s, \t, x -> t + inside(limit, 2 * Num.to_f64(x) / Num.to_f64(n) - 1.5, 2 * Num.to_f64(y) / Num.to_f64(n) - 1, 0, 0, 0)))

main! = \args ->
    result = solve(argument(args, 0), argument(args, 1))
    Stdout.line!(Num.to_str(result))
