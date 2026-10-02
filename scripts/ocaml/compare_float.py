#!/usr/bin/env python3
"""Compare complete deterministic IEEE binary64 R-format observations."""
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

def main():
    output = ROOT / "TestResults/ocaml-migration/float"
    output.mkdir(parents=True, exist_ok=True)
    observations = []
    for name, command in [
        ("fsharp", ["dotnet", "fsi", "--exec", "scripts/ocaml/float_reference.fsx"]),
        ("ocaml", ["ocaml/_build/default/tests/float_probe.exe"]),
    ]:
        result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=120)
        (output / f"{name}.jsonl").write_text(result.stdout)
        (output / f"{name}.stderr").write_text(result.stderr)
        if result.returncode:
            print(f"{name} failed; see {output / (name + '.stderr')}")
            return 1
        observations.append([json.loads(line) for line in result.stdout.split("\n") if line])
    if observations[0] != observations[1]:
        print(f"Float format mismatch; complete observations: {output}")
        return 1
    print(f"Binary64 roundtrip format parity: {len(observations[0])}/{len(observations[0])} observations match")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
