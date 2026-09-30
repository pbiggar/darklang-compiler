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

Tree : [Leaf I64, Branch Tree Tree]
Entry : { weight: I64, minimum: I64, tree: Tree }

modulo = \n -> Num.rem(n, 1000000007)
generate = \n, seed ->
    state = List.walk(range(n), { seed, reversed: [] }, \s, _ ->
        next = Num.rem(s.seed * 1103515245 + 12345, 2147483648)
        r = Num.rem(next, 1000)
        symbol = if r < 300 then 0 else if r < 480 then 1 else if r < 610 then 2 else if r < 710 then 3 else if r < 790 then 4 else if r < 850 then 5 else if r < 900 then 6 else if r < 940 then 7 else 8 + Num.rem(next, 24)
        { seed: next, reversed: List.prepend(s.reversed, symbol) })
    List.reverse(state.reversed)
insert : List Entry, Entry -> List Entry
insert = \queue, entry -> when queue is
    [] -> [entry]
    [x, .. as rest] -> if entry.weight < x.weight || (entry.weight == x.weight && entry.minimum < x.minimum) then List.prepend(queue, entry) else List.prepend(insert(rest, entry), x)
combine = \queue -> when queue is
    [a] -> a.tree
    [a, b, .. as rest] -> combine(insert(rest, { weight: a.weight + b.weight, minimum: Num.min(a.minimum, b.minimum), tree: Branch(a.tree, b.tree) }))
    _ -> crash("empty codec")
codec = \data ->
    counts = List.walk(data, List.repeat(0, 32), \xs, s -> List.set(xs, Num.to_u64(s), at(xs, Num.to_u64(s)) + 1))
    queue = List.walk(range(32), [], \q, s ->
        weight = at(counts, Num.to_u64(s))
        if weight == 0 then q else insert(q, { weight, minimum: s, tree: Leaf(s) }))
    combine(queue)
codes : Tree, I64, I64, List (I64, I64) -> List (I64, I64)
codes = \tree, bits, length, table -> when tree is
    Leaf(s) -> List.set(table, Num.to_u64(s), (bits, Num.max(1, length)))
    Branch(a, b) -> codes(b, bits * 2 + 1, length + 1, codes(a, bits * 2, length + 1, table))
encode = \data, table -> List.join(List.map(data, \s ->
    (bits, length) = at(table, Num.to_u64(s))
    List.map(range(length), \i -> Num.rem(Num.div_trunc(bits, Num.pow_int(2, length - i - 1)), 2))))
decode = \tree, bits -> when tree is
    Leaf(s) -> List.repeat(s, List.len(bits))
    _ ->
        state = List.walk(bits, { current: tree, reversed: [] }, \s, bit ->
            next = when s.current is
                Branch(a, b) -> if bit == 0 then a else b
                _ -> crash("invalid codec")
            when next is
                Leaf(symbol) -> { current: tree, reversed: List.prepend(s.reversed, symbol) }
                _ -> { current: next, reversed: s.reversed })
        List.reverse(state.reversed)
checksum = \xs -> List.walk(List.map_with_index(xs, \v, i -> (v, i)), 0, \sum, pair -> modulo(sum + pair.0 * (Num.to_i64(pair.1) + 1)))
main! = \args ->
    data = generate(argument(args, 0), argument(args, 1))
    tree = codec(data)
    table = codes(tree, 0, 0, List.repeat((0, 0), 32))
    total = List.walk(range(argument(args, 2)), 0, \sum, _ ->
        encoded = encode(data, table)
        decoded = decode(tree, encoded)
        if decoded != data then crash("Huffman roundtrip failed") else modulo(sum + Num.to_i64(List.len(encoded)) * 17 + checksum(encoded) + checksum(decoded)))
    Stdout.line!(Num.to_str(total))
