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

alu = Str.to_utf8("GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA")
cumulative : List F64 -> List F64
cumulative = \xs ->
    state = List.walk(xs, { sum: 0, values: [] }, \s, x -> { sum: s.sum + x, values: List.append(s.values, s.sum + x) })
    state.values
select : F64, List U8, List F64, U64 -> U8
select = \r, chars, probs, i -> if r < at(probs, i) or i + 1 == List.len(chars) then at(chars, i) else select(r, chars, probs, i + 1)
random_fasta : I64, List U8, List F64, I64 -> { checksum: I64, seed: I64 }
random_fasta = \n, chars, probs, seed -> List.walk(range(n), { checksum: 0, seed }, \s, i ->
    next = Num.rem(s.seed * 3877 + 29573, 139968)
    char = select(Num.to_f64(next) / 139968, chars, probs, 0)
    { checksum: Num.rem(s.checksum + Num.to_i64(char) * (i + 1), 1000000007), seed: next })
solve : I64 -> I64
solve = \n ->
    ip = cumulative(List.concat([0.27, 0.12, 0.12, 0.27], List.repeat(0.02, 11)))
    hp = cumulative([0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008])
    c1 = List.walk(range(2 * n), 0, \s, i -> Num.rem(s + Num.to_i64(at(alu, Num.rem(Num.to_u64(i), List.len(alu)))) * (i + 1), 1000000007))
    c2 = random_fasta(3 * n, Str.to_utf8("acgtBDHKMNRSVWY"), ip, 42)
    c3 = random_fasta(5 * n, Str.to_utf8("acgt"), hp, c2.seed)
    Num.rem(c1 + c2.checksum + c3.checksum, 1000000007)

main! = \args ->
    result = solve(argument(args, 0))
    Stdout.line!(Num.to_str(result))
