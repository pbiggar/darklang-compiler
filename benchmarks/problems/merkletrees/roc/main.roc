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

hash_value : U64 -> U64
hash_value = \value -> List.walk(range(8), 14695981039346656037, \h, _ -> Num.mul_wrap(Num.bitwise_xor(h, Num.bitwise_and(value, 255)), 1099511628211))
build : I64, U64 -> U64
build = \depth, start ->
    if depth == 0 then hash_value(start)
    else hash_value(Num.add_wrap(build(depth - 1, start), Num.mul_wrap(31, build(depth - 1, start + Num.shift_left_by(1, Num.to_u8(depth - 1))))))
solve : List Arg -> U64
solve = \args -> List.walk(range(argument(args, 1)), 0, \s, i ->
    root = build(argument(args, 0), Num.to_u64(i))
    verified = build(argument(args, 0), Num.to_u64(i)) == root
    count = Num.rem(Num.add_wrap(s, root), 1000000007)
    if verified then Num.rem(count + 1, 1000000007) else count)

main! = \args ->
    result = solve(args)
    Stdout.line!(Num.to_str(result))
