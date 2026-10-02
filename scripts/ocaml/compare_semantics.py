#!/usr/bin/env python3
"""Compare complete stage observations across frozen source inputs and probes."""
import argparse
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
    parser.add_argument("--stage", default="tokens", choices=["tokens", "parser-support", "patterns", "types", "bindings", "parameters", "effects"])
    parser.add_argument("--probes-only", action="store_true")
    args = parser.parse_args()
    corpus = list(inputs())
    if args.probes_only:
        corpus = [(label, source) for label, source in corpus if label.startswith("probe-")]
    output = ROOT / "TestResults/ocaml-migration" / args.stage
    output.mkdir(parents=True, exist_ok=True)
    requests = "".join(json.dumps({"stage":args.stage,"source":source},ensure_ascii=True)+"\n" for _,source in corpus)
    request_file = output / "requests.jsonl"
    request_file.write_text(requests)
    observations = []
    for name, command in [
        ("fsharp", ["dotnet", "fsi", "--exec", "scripts/ocaml/semantic_reference.fsx", str(request_file)]),
        ("ocaml", ["ocaml/_build/default/tests/semantic_probe.exe"]),
    ]:
        with request_file.open() as stdin, (output / f"{name}.jsonl").open("w") as stdout, (output / f"{name}.stderr").open("w") as stderr:
            run = subprocess.run(command, cwd=ROOT, stdin=stdin, text=True, stdout=stdout, stderr=stderr, timeout=1200)
        if run.returncode:
            print(f"{name} failed; see {output / (name + '.stderr')}")
            return 1
        observations.append(output / f"{name}.jsonl")
    count = 0
    with observations[0].open() as left, observations[1].open() as right:
        for (label, source), expected, actual in zip(corpus, left, right, strict=True):
            expected, actual = json.loads(expected), json.loads(actual)
            if expected != actual:
                difference = first_difference(expected, actual)
                (output / "mismatch.json").write_text(json.dumps({"input":label,"source":source,"difference":difference},ensure_ascii=True,indent=2))
                print(f"{args.stage} mismatch in {label}: {difference[0]}; see {output / 'mismatch.json'}")
                return 1
            count += 1
    print(f"Complete {args.stage} parity: {count}/{len(corpus)} source observations match")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
