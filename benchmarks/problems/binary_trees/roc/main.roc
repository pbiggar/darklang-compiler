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

# Adapted from Koka's recursive Benchmarks Game make/check.
Tree : [Tip, Node Tree Tree]
make : I64 -> Tree
make = \depth -> if depth > 0 then Node(make(depth - 1), make(depth - 1)) else Node(Tip, Tip)
check : Tree -> I64
check = \tree ->
    when tree is
        Tip -> 0
        Node(l, r) -> check(l) + check(r) + 1

main! = \args ->
    result = List.walk(range(argument(args, 1)), 0, \s, _ -> s + check(make(argument(args, 0))))
    Stdout.line!(Num.to_str(result))
