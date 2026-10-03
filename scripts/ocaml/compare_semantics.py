#!/usr/bin/env python3
"""Compare complete stage observations across frozen source inputs and probes."""
import argparse
import hashlib
from contextlib import ExitStack
import json
import random
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

def inputs():
    manifest = json.loads((ROOT / "ocaml/inventory.json").read_text())
    for entry in manifest["entries"]:
        path = ROOT / entry["source"]
        if entry["kind"] in ("stdlib", "test-input") and path.suffix in (".dark", ".e2e"):
            yield entry["source"], path.read_text()
    for path in sorted((ROOT / "benchmarks").rglob("*.dark")):
        yield str(path.relative_to(ROOT)), path.read_text()
    probes = [
        "", "/// doc\n//// ordinary\n// tail", "(* outer (* inner *) end *) let x = 1", "(* never closed",
        "(*) (**) => ... ** ++ :: |> && || << >>", "``name with spaces`` ``half\nlet x = 1",
        "let x: 'a = 'x' ''' '\\U0001F600' 'e\u0301' '👨‍👩‍👧‍👦'",
        '"é" "e\u0301" "\\uD800" "\\U00110000" "\\UFFFFFFFF" "\\q"',
        '"half\nnext', '"""raw\nquoted " \\ stuff"""', '"""unfinished',
        '$"hello {f { x = 1 }} {{ }} \\}"', '$"{ \"}\" (* } *) // }\n x }"',
        '$"unclosed { 1', '$"""raw { \"text\" } " single"""',
        "λ 中文 é 𐐀 😀 \u1c89 \ua7cb \u0661 \uff11 \u00a0", "1.0abc 12l3 123abc 1e 1e+ 1.5e-3 1e9999",
    ]
    for suffix, bits, signed in [("",128,False),("y",8,True),("uy",8,False),("s",16,True),("us",16,False),
                                 ("l",32,True),("ul",32,False),("L",64,True),("UL",64,False),("Q",128,True),("Z",128,False)]:
        bound = 2 ** (bits - int(signed))
        for value in [0, 1, bound-1, bound, bound+1, 2**256]:
            probes.extend([str(value)+suffix, "0"+str(value)+suffix, "-"+str(value)+suffix])
    probes.extend([
        "1, 2 | 3, 4", "a :: b :: tail", "Ok -128y", "Pair(a, b)", "Pair((a, b))", "Case()",
        "Result.Ok value", "Case a b", "Case a\n  b", "Case a\nb", "(a | b)", "(a, b)", "()",
        "[head, tail]", "[a; b]", "[a\nb]", "[a b]", "[a,", "(a,", "Case(a,", "...", "->", "'क्'", "'क्क'",
    ])
    probes.extend([
        "List<List<Int>> * Bool", "Option<Result<Int, String>, Bool>", "Dict<String, List<Int>>",
        "Int -> Bool -> String", "Int * Bool -> String", "(Int * Bool)", "(Int)", "'TModel", "'Int", "a", "",
        "List <Int>", "Mod.Type <Int>", "Dict<Int Bool>", "List<List<Int>>, Bool", "List<Int", "()", "Int ->",
        "Option<>", "Option<Int,>", "((Int * Bool) -> List<Int>)", "A<B<C<D>>> * E", "A<B<C>, D>",
    ])
    probes.extend([
        "()", "(x: Int)", "(_: Unit)", "(x: List<List<Int>>)", "(x Int)", "(x: Int", "(1: Int)",
        "/// param docs\n(x: Int)", "(/// name docs\nx: Int)", "{}", "{Http, Clock}", "{Http,}",
        "{Imaginary}", "{Http Clock}", "{Http, Http}", "{", "{1}", "{Http", "{http}",
    ])
    probes.extend([
        "if x then y else z", "if a then\n  if b then c\nelse d", "if a then b elif c then d else e",
        "let x = 1L in x", "let f (x: Int) = x in f 1", "let x: Int = 1 in x", "fun (a, b) _ -> a",
        "match x with | A -> 1 | B when p -> 2", "match x, y with | a, b -> a", "match x with | a | b -> a",
        "type X = | A of (Int * Bool) * named: String | B", "type R = { /// field\n x: Int; y: Bool }",
        "module A =\n  val x = 1\n  let f () : Int = x\nval y = 2", "module A.B\nlet x = 1",
        "let f (___: Int): Int = 1", "val f (): Int = 1", "[<DB>] type X = { x: Int }",
        "[f a\nf b]", "(f a\nf b)", "Ctor((a, b))", "Ctor (a, b)", "f None (g)", "parse<Int> x",
        "Type<Int>.Case(a, b)", "Mod.R<Int> {x = f a; y = g b}", "{r with x = f a; y = g b}", "{r with}",
        "Dict {1: f a; 2: g b}", "a & b == c || d && e", "2 ** 3 ** 2", "a @ b @ c", "1L\n-8L", "1L\n -8L",
        '$"é😀 {x + 1} text"', '$"{{{{ \\{\\{ {x} }}}}"', '$"{val x = 1}"', '$"{}"', '$"{1 2}"',
        '$"""é {{raw}} {f x}\n😀"""', "match x with | (a, a) -> a", "fun a a -> a",
    ])
    probes.extend([
        "let", "val", "in", "if", "elif", "then", "else", "type", "of", "match", "with", "fun", "when", "true", "false", "_", "___",
        "ordinary", "'a", "a'", "α.é", "𐐀", "A.B.C", "A.", ".A", "A..B", "``a.b``.C", "````", "``half", "``line\nend``",
        "module A.B\n  val x = 1", "// lead\nmodule A.B\nval x = 1", "module A =\n  val x = 1\n  val y = 2",
        "module A =\nval x = 1", "module ``a.b``\r\nval x = 1", "module A =\r\n\tval x = 1", "module A =", "module A. =\n  1",
    ])
    probes.extend([
        "fun z z aa aa -> z", "fun (z, z, aa, aa) -> z", "match x with | (z, z, aa, aa) -> z",
        "fun 𐀀 𐀀   -> 𐀀", "Darklang.Stdlib.Option.Option", "Darklang.Stdlib.Result.Result",
        "x", "_x", "", "x.x", "A.Case", "𝒜.漢字", "😀", "Case", "A.B", "A.B.C",
    ])
    probes.extend([
        "---NAME---\na\n---SOURCE---\n1L\n---ROUNDTRIP---\n",
        "---NAME---\r\na\r\n---SOURCE---\r\n1L\r\n",
        "---SOURCE---\n1L", "---NAME---\na", "---NAME---\na\n---OTHER---\nx\n---SOURCE---\n1L",
        "prefix\n---NAME---\na\n---SOURCE---\n1L\n---EXPECT-ERROR---\nx\n---ROUNDTRIP---",
        "---NAME---\na\n---SOURCE---\n1L\n---EXPECTED---\n1L", "---NAME---\na\n---SOURCE---\n1L\n---SOURCE---\n2L",
        "---NAME---\na\n---SOURCE---\n1L\n---NAME---\nb\n---SOURCE---\n2L", "---NÄME---\nx", "---NAME--- \nx",
        "foo // tail\n bar\r\n// comment", "\\n\\r\\t\\\\\\\"", "\\q", "x\\", "\\😀",
    ])
    rng = random.Random(12864)
    atoms = ["let", "val", "___", "x'", "'a", "α", "é", "😀", "0L", "9223372036854775808L", "1e+", "12abc",
             "(", ")", "(*)", "(*", "*)", '"', '"""', '$"', "\\u0041", "\\U00110000", "'", "``", "//", "///", "\n", "\r", "\t", "{", "}", ";"]
    probes.extend(" ".join(rng.choices(atoms, k=rng.randrange(1,30))) for _ in range(1000))
    for index, source in enumerate(probes):
        yield f"probe-{index}", source

def first_difference(expected, actual, path="value"):
    if type(expected) is not type(actual):
        return path, expected, actual
    if isinstance(expected, dict):
        if expected.keys() != actual.keys(): return path, list(expected), list(actual)
        for key in expected:
            if expected[key] != actual[key]: return first_difference(expected[key], actual[key], f"{path}.{key}")
    elif isinstance(expected, list):
        if len(expected) != len(actual): return path + ".length", len(expected), len(actual)
        for index, (left, right) in enumerate(zip(expected, actual)):
            if left != right: return first_difference(left, right, f"{path}[{index}]")
    return path, expected, actual

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--stage", default="tokens", choices=["tokens", "parser-support", "patterns", "types", "bindings", "parameters", "effects", "ast", "validated", "rendered", "written-source", "names", "ast-helpers", "formatter", "dsl", "resolution", "checking-diagnostics", "free-variables", "function-map", "checked-ast", "checking-types", "unification"])
    parser.add_argument("--probes-only", action="store_true")
    args = parser.parse_args()
    corpus = list(inputs())
    if args.stage == "dsl" and not args.probes_only:
        corpus.extend((str(path.relative_to(ROOT)), path.read_text()) for path in sorted((ROOT / "src/Tests").rglob("*.syntax")))
    if args.probes_only:
        corpus = [(label, source) for label, source in corpus if label.startswith("probe-")]
    output = ROOT / "TestResults/ocaml-migration" / args.stage
    output.mkdir(parents=True, exist_ok=True)
    requests = "".join(json.dumps({"stage":args.stage,"source":source},ensure_ascii=True)+"\n" for _,source in corpus)
    request_file = output / "requests.jsonl"
    request_file.write_text(requests)
    commands = [
        ("fsharp", ["dotnet", "fsi", "--exec", "scripts/ocaml/semantic_reference.fsx", str(request_file)]),
        ("ocaml", ["ocaml/_build/default/tests/foundations_main.exe", "--dsl-probe"] if args.stage == "dsl" else ["ocaml/_build/default/tests/semantic_probe.exe"]),
    ]
    # Compare complete rows immediately. Large resolver inventories repeat source
    # evidence many times; retain canonical audit hashes rather than gigabytes of
    # identical successful trees. A mismatch retains both complete observations.
    processes = []
    count = 0
    with ExitStack() as stack:
        audits = []
        try:
            for name, command in commands:
                stdin = stack.enter_context(request_file.open())
                stderr = stack.enter_context((output / f"{name}.stderr").open("w"))
                audits.append(stack.enter_context((output / f"{name}.jsonl").open("w")))
                processes.append(subprocess.Popen(command, cwd=ROOT, stdin=stdin, stdout=subprocess.PIPE, text=True, stderr=stderr))
            for label, source in corpus:
                rows = [process.stdout.readline() for process in processes]
                for (name, _), row in zip(commands, rows, strict=True):
                    if not row:
                        print(f"{name} stopped before {label}; see {output / (name + '.stderr')}")
                        return 1
                expected, actual = [json.loads(row) for row in rows]
                for audit, observation in zip(audits, [expected, actual], strict=True):
                    canonical = json.dumps(observation, sort_keys=True, ensure_ascii=True, separators=(",", ":")).encode()
                    audit.write(json.dumps({"input": label, "sha256": hashlib.sha256(canonical).hexdigest(), "bytes": len(canonical)}) + "\n")
                if expected != actual:
                    difference = first_difference(expected, actual)
                    (output / "mismatch.json").write_text(json.dumps({"input":label,"source":source,"difference":difference},ensure_ascii=True,indent=2))
                    (output / "expected.json").write_text(rows[0])
                    (output / "actual.json").write_text(rows[1])
                    print(f"{args.stage} mismatch in {label}: {difference[0]}; see {output / 'mismatch.json'}")
                    return 1
                count += 1
            for (name, _), process in zip(commands, processes, strict=True):
                if process.stdout.readline():
                    print(f"{name} emitted extra observations")
                    return 1
                if process.wait(timeout=1200):
                    print(f"{name} failed; see {output / (name + '.stderr')}")
                    return 1
        finally:
            for process in processes:
                if process.poll() is None:
                    process.terminate()
                    try:
                        process.wait(timeout=10)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        process.wait()
                process.stdout.close()
    print(f"Complete {args.stage} parity: {count}/{len(corpus)} source observations match")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
