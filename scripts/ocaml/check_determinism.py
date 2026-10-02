#!/usr/bin/env python3
"""Compare complete F#-generated benchmark executables from independent runs."""

import hashlib
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def main():
    profiles = json.loads((ROOT / "benchmarks/profiles.json").read_text())
    names = profiles["profiles"]["full"]
    artifacts = ROOT / "TestResults/ocaml-migration/determinism"
    results = []
    for options_name, options in (("default", []), ("leaks", ["--leak-check"])):
        runs = []
        for index in (1, 2):
            output = artifacts / options_name / str(index)
            output.mkdir(parents=True, exist_ok=True)
            command = [str(ROOT / "dark"), "--batch", "--quiet", *options, "--"]
            for name in names:
                command.extend([str(ROOT / "benchmarks/problems" / name / "dark/main.dark"),
                                str(output / name)])
            run = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=600)
            (output / "compile.log").write_text(run.stdout + run.stderr)
            if run.returncode:
                print(f"Determinism compilation failed: {output / 'compile.log'}")
                return 1
            runs.append(output)
        for name in names:
            first = (runs[0] / name).read_bytes()
            second = (runs[1] / name).read_bytes()
            same = first == second
            offset = next((i for i, (a, b) in enumerate(zip(first, second)) if a != b),
                          None if same else min(len(first), len(second)))
            results.append({"name": name, "options": options, "identical": same,
                            "sha256": hashlib.sha256(first).hexdigest(),
                            "first_difference": offset, "size": len(first)})
    report = artifacts / "results.json"
    report.write_text(json.dumps(results, indent=2) + "\n")
    passed = sum(result["identical"] for result in results)
    print(f"Whole-file determinism: {passed}/{len(results)} identical; {report}")
    return 0 if passed == len(results) else 1


if __name__ == "__main__":
    raise SystemExit(main())
