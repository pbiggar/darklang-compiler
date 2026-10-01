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

modulo = \n -> Num.rem(n, 1000000007)
snake = \left, right, x, y ->
    if x < Num.to_i64(List.len(left)) && y < Num.to_i64(List.len(right)) && at(left, Num.to_u64(x)) == at(right, Num.to_u64(y)) then snake(left, right, x + 1, y + 1) else (x, y)
layer = \left, right, d, previous, work ->
    offset = Num.to_i64(List.len(left) + List.len(right))
    state = List.walk(range(d + 1), { frontier: List.repeat(0, Num.to_u64(offset * 2 + 1)), work, reached: Bool.false }, \s, i ->
        k = -d + i * 2
        start = if k == -d || (k != d && at(previous, Num.to_u64(offset + k - 1)) < at(previous, Num.to_u64(offset + k + 1))) then at(previous, Num.to_u64(offset + k + 1)) else at(previous, Num.to_u64(offset + k - 1)) + 1
        (x, y) = snake(left, right, start, start - k)
        { frontier: List.set(s.frontier, Num.to_u64(offset + k), x), work: modulo(s.work + (x + 1) * (y + 3) + (k + d + 1) * 17), reached: s.reached || (x >= Num.to_i64(List.len(left)) && y >= Num.to_i64(List.len(right))) })
    if state.reached then modulo(d * 1000003 + state.work) else layer(left, right, d + 1, state.frontier, state.work)
diff = \left, right ->
    (x, y) = snake(left, right, 0, 0)
    work = (x + 1) * (y + 3)
    offset = List.len(left) + List.len(right)
    if x == Num.to_i64(List.len(left)) && y == Num.to_i64(List.len(right)) then work else
        previous = List.set(List.repeat(0, offset * 2 + 1), offset, x)
        layer(left, right, 1, previous, work)
repeat = \s, n -> Str.join_with(List.repeat(s, Num.to_u64(n)), "")
main! = \args ->
    unit = "darklang compiler benchmark: persistent values and recursive paths.\n"
    prefix = repeat(unit, argument(args, 0))
    suffix = repeat(unit, argument(args, 0) + 1)
    left = Str.to_utf8(Str.concat(prefix, suffix))
    right = Str.to_utf8(Str.join_with([prefix, repeat("<changed-block>", argument(args, 1)), suffix], ""))
    total = List.walk(range(argument(args, 2)), 0, \s, _ -> modulo(s + diff(left, right)))
    Stdout.line!(Num.to_str(total))
