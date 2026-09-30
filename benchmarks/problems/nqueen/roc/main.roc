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

# Adapted from Roc compiler n_queens.roc (UPL-1.0).
ConsList a : [Nil, Cons a (ConsList a)]

queens = \n -> length(find_solutions(n, n))

find_solutions = \n, k ->
    if k <= 0 then
        # should we use U64 as input type here instead?
        Cons(Nil, Nil)
    else
        extend(n, Nil, find_solutions(n, (k - 1)))

extend = \n, acc, solutions ->
    when solutions is
        Nil -> acc
        Cons(soln, rest) -> extend(n, append_safe(n, soln, acc), rest)

append_safe : I64, ConsList I64, ConsList (ConsList I64) -> ConsList (ConsList I64)
append_safe = \k, soln, solns ->
    if k <= 0 then
        solns
    else if safe(k, 1, soln) then
        append_safe((k - 1), soln, Cons(Cons(k, soln), solns))
    else
        append_safe((k - 1), soln, solns)

safe : I64, I64, ConsList I64 -> Bool
safe = \queen, diagonal, xs ->
    when xs is
        Nil -> Bool.true
        Cons(q, t) ->
            if queen != q and queen != q + diagonal and queen != q - diagonal then
                safe(queen, (diagonal + 1), t)
            else
                Bool.false

length : ConsList a -> I64
length = \xs ->
    length_help(xs, 0)

length_help : ConsList a, I64 -> I64
length_help = \foobar, acc ->
    when foobar is
        Cons(_, lrest) -> length_help(lrest, (1 + acc))
        Nil -> acc

main! = \args ->
    result = queens(argument(args, 0))
    Stdout.line!(Num.to_str(result))
