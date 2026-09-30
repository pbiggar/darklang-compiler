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

Complex : { re: F64, im: F64 }
fft : List Complex -> List Complex
fft = \xs ->
    n = List.len(xs)
    if n <= 1 then xs else
        even = fft(List.keep_if(List.map_with_index(xs, \z, i -> (z, i)), \pair -> Num.rem(pair.1, 2) == 0) |> List.map(\pair -> pair.0))
        odd = fft(List.keep_if(List.map_with_index(xs, \z, i -> (z, i)), \pair -> Num.rem(pair.1, 2) == 1) |> List.map(\pair -> pair.0))
        twiddled = List.map_with_index(odd, \z, i ->
            angle = -6.283185307179586 * Num.to_f64(i) / Num.to_f64(n)
            c = Num.cos(angle)
            s = Num.sin(angle)
            { re: c * z.re - s * z.im, im: c * z.im + s * z.re })
        lower = List.map_with_index(even, \a, i ->
            b = at(twiddled, i)
            { re: a.re + b.re, im: a.im + b.im })
        upper = List.map_with_index(even, \a, i ->
            b = at(twiddled, i)
            { re: a.re - b.re, im: a.im - b.im })
        List.concat(lower, upper)
modulo = \n -> Num.rem(Num.rem(n, 1000000007) + 1000000007, 1000000007)
truncate : F64 -> I64
truncate = \n -> if n < 0 then Num.ceiling(n) else Num.floor(n)
main! = \args ->
    input = List.map(range(argument(args, 0)), \i ->
        x = Num.to_f64(i)
        { re: Num.sin(x * 0.017) + Num.cos(x * 0.031), im: Num.cos(x * 0.013) - Num.sin(x * 0.007) })
    total = List.walk(range(argument(args, 1)), 0, \sum, _ ->
        result = fft(input)
        List.walk(List.map_with_index(result, \z, i -> (z, i)), sum, \s, pair ->
            modulo(s + truncate((pair.0.re * 3 + pair.0.im * 5) * 1000000) * (Num.to_i64(pair.1) + 1))))
    Stdout.line!(Num.to_str(total))
