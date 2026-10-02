#!/usr/bin/env python3
"""Compare complete deterministic foundation observations against frozen F#."""

import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def main():
    commands = [
        ["dotnet", "fsi", "--exec", "scripts/ocaml/foundations_reference.fsx"],
        ["ocaml/_build/default/tests/foundations_main.exe", "--probe"],
    ]
    output = ROOT / "TestResults/ocaml-migration/foundations"
    output.mkdir(parents=True, exist_ok=True)
    runs = []
    for name, command in zip(("fsharp", "ocaml"), commands):
        result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=120)
        (output / f"{name}.jsonl").write_text(result.stdout)
        (output / f"{name}.stderr").write_text(result.stderr)
        if result.returncode:
            print(f"{name} failed; see {output / (name + '.stderr')}")
            return 1
        runs.append([json.loads(line) for line in result.stdout.splitlines()])
    if len(runs[0]) != len(runs[1]):
        print(f"Observation counts differ: {len(runs[0])} vs {len(runs[1])}")
        return 1
    for index, (expected, actual) in enumerate(zip(*runs)):
        if expected != actual:
            print(f"Foundation mismatch at observation {index}; artifacts: {output}")
            return 1
    print(f"Foundation parity: {len(runs[0])}/{len(runs[0])} complete observations match")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
