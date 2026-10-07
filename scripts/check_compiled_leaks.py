#!/usr/bin/env python3
"""Compile canonical Dark benchmarks with leak accounting and check both workloads."""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path


def main() -> int:
    root = Path(__file__).resolve().parents[1]
    profiles = json.loads((root / "benchmarks/profiles.json").read_text())
    names = profiles["profiles"]["full"]
    artifacts = root / "TestResults/ai/compiled-leak-gate"
    binaries = artifacts / "binaries"
    binaries.mkdir(parents=True, exist_ok=True)
    command = [str(root / "dark"), "--batch", "--allow-internal", "--leak-check", "--quiet", "--"]
    for name in names:
        command.extend(
            [
                str(root / "benchmarks/problems" / name / "dark/main.dark"),
                str(binaries / name),
            ]
        )
    compiled = subprocess.run(command, cwd=root, capture_output=True, text=True, timeout=600)
    if compiled.returncode:
        (artifacts / "compile.log").write_text(compiled.stdout + compiled.stderr)
        print(f"leak gate: compilation failed; see {artifacts / 'compile.log'}", file=sys.stderr)
        return 1

    results = []
    for name in names:
        for profile in ("quick", "full"):
            workload = profiles["workloads"][name][profile]
            try:
                run = subprocess.run(
                    [str(binaries / name), *workload["args"]],
                    cwd=root,
                    capture_output=True,
                    text=True,
                    timeout=120,
                )
                result = {
                    "name": name,
                    "profile": profile,
                    "exit_code": run.returncode,
                    "stdout": run.stdout,
                    "stderr": run.stderr,
                    "passed": run.returncode == 0
                    and run.stdout == workload["expected_stdout"]
                    and run.stderr == "",
                }
            except subprocess.TimeoutExpired:
                result = {"name": name, "profile": profile, "passed": False, "error": "timeout"}
            results.append(result)

    report = artifacts / "results.json"
    report.write_text(json.dumps(results, indent=2) + "\n")
    failures = [item for item in results if not item["passed"]]
    print(f"leak gate: {len(results) - len(failures)}/{len(results)} clean; report: {report}")
    for item in failures:
        detail = item.get("stderr", item.get("error", ""))
        print(f"  {item['name']} {item['profile']}: {detail.strip()[:160]}", file=sys.stderr)
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
