#!/usr/bin/env python3
"""Compare all Unicode scalar casing/whitespace and UTF-16 roundtrips."""
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

def main():
    output = ROOT / "TestResults/ocaml-migration/text"
    output.mkdir(parents=True, exist_ok=True)
    observations = []
    for name, command in [
        ("fsharp", ["dotnet", "fsi", "--exec", "scripts/ocaml/text_reference.fsx"]),
        ("ocaml", ["ocaml/_build/default/tests/text_probe.exe"]),
    ]:
        result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=120)
        (output / f"{name}.jsonl").write_text(result.stdout)
        (output / f"{name}.stderr").write_text(result.stderr)
        if result.returncode:
            print(f"{name} failed: {output / (name + '.stderr')}")
            return 1
        # Only LF delimits JSONL. Unicode line separators are valid string data.
        observations.append([json.loads(line) for line in result.stdout.split("\n") if line])
    if observations[0] != observations[1]:
        print(f"Host text mismatch; complete observations: {output}")
        return 1
    print(f"Host text parity: all 1,112,064 Unicode scalars checked; {len(observations[0])} nonidentity/integer observations match")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
