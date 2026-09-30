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

Token : [Number I64, Variable U8, Symbol U8]

digit = \c -> c >= 48 && c <= 57
alpha = \c -> (c >= 97 && c <= 122) || (c >= 65 && c <= 90)
digits = \source, i, value ->
    if i < List.len(source) && digit(at(source, i)) then digits(source, i + 1, value * 10 + Num.to_i64(at(source, i)) - 48) else (value, i)
identifier = \source, i -> if i < List.len(source) && alpha(at(source, i)) then identifier(source, i + 1) else i
lex : List U8, U64, List Token -> List Token
lex = \source, i, reversed ->
    if i >= List.len(source) then List.reverse(reversed) else
        c = at(source, i)
        if c == 32 || c == 10 || c == 9 || c == 13 then lex(source, i + 1, reversed) else if digit(c) then
            (n, j) = digits(source, i, 0)
            lex(source, j, List.prepend(reversed, Number(n)))
        else if alpha(c) then
            j = identifier(source, i)
            if j != i + 1 || (c != 120 && c != 121) then crash("unknown variable") else lex(source, j, List.prepend(reversed, Variable(c)))
        else lex(source, i + 1, List.prepend(reversed, Symbol(c)))
precedence = \op -> when op is
    60 -> 1
    43 | 45 -> 2
    42 | 47 -> 3
    _ -> 0
apply = \op, a, b -> when op is
    43 -> a + b
    45 -> a - b
    42 -> a * b
    47 -> Num.div_trunc(a, b)
    60 -> if a < b then 1 else 0
    _ -> crash("unknown operator")
expression = \tokens, index, x, y, minimum ->
    (left, start) = when at(tokens, index) is
        Number(n) -> (n, index + 1)
        Variable(c) -> (if c == 120 then x else y, index + 1)
        Symbol(40) ->
            (v, j) = expression(tokens, index + 1, x, y, 0)
            when at(tokens, j) is
                Symbol(41) -> (v, j + 1)
                _ -> crash("expected )")
        _ -> crash("invalid primary")
    infix_loop(tokens, start, x, y, minimum, left)
infix_loop = \tokens, i, x, y, minimum, left ->
    if i >= List.len(tokens) then (left, i) else when at(tokens, i) is
        Symbol(op) ->
            p = precedence(op)
            if p == 0 || p < minimum then (left, i) else
                (right, j) = expression(tokens, i + 1, x, y, p + 1)
                infix_loop(tokens, j, x, y, minimum, apply(op, left, right))
        _ -> (left, i)
statements = \tokens, iteration, i, index, sum ->
    if i >= List.len(tokens) then sum else
        x = Num.rem(iteration * 17 + index * 13, 97) + 3
        y = Num.rem(iteration * 29 + index * 7, 89) + 5
        (v, j) = expression(tokens, i, x, y, 0)
        when at(tokens, j) is
            Symbol(59) -> statements(tokens, iteration, j + 1, index + 1, Num.rem(Num.rem(sum + v * (index + 1), 1000000007) + 1000000007, 1000000007))
            _ -> crash("expected ;")
main! = \args ->
    source = Str.join_with(List.repeat("x * x + y * 3 + (x + y) * (x - y) + x / 2;\n", Num.to_u64(argument(args, 0))), "")
    tokens = lex(Str.to_utf8(source), 0, [])
    total = List.walk(range(argument(args, 1)), 0, \s, i -> Num.rem(s + statements(tokens, i, 0, 0, 0), 1000000007))
    Stdout.line!(Num.to_str(total))
