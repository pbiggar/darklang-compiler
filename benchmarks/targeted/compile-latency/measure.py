#!/usr/bin/env python3
"""Measure fresh-process standard-mode compiler latency."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import statistics
import subprocess
import tempfile
import time
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument("--warmups", type=int, default=1)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.runs <= 0 or args.warmups < 0:
        raise SystemExit("--runs must be positive and --warmups must be non-negative")

    root = Path(__file__).resolve().parents[3]
    compiler = root / "dark"
    compiler_dll = root / "bin/DarkCompiler/Debug/net11.0/DarkCompiler.dll"
    source = Path(__file__).with_name("program.dark")
    if not compiler_dll.is_file():
        raise SystemExit("compiler is not built; run ./build --ai first")

    wall_samples_ms: list[float] = []
    pipeline_samples_ms: list[float] = []
    with tempfile.TemporaryDirectory(prefix="dark-compile-latency-") as temp:
        temp_dir = Path(temp)
        total = args.warmups + args.runs
        for index in range(total):
            output = temp_dir / f"program-{index}"
            started = time.perf_counter_ns()
            completed = subprocess.run(
                [str(compiler), "-vv", str(source), "-o", str(output)],
                cwd=root,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=False,
            )
            elapsed_ms = (time.perf_counter_ns() - started) / 1_000_000
            if completed.returncode != 0:
                raise SystemExit(
                    f"compilation {index + 1}/{total} failed:\n{completed.stderr}{completed.stdout}"
                )
            timing = re.search(
                r"Compilation complete \(([0-9]+(?:\.[0-9]+)?)ms\)",
                completed.stdout + completed.stderr,
            )
            if timing is None:
                raise SystemExit("compiler did not report its pipeline latency")
            if index >= args.warmups:
                wall_samples_ms.append(elapsed_ms)
                pipeline_samples_ms.append(float(timing.group(1)))

    commit = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        cwd=root,
        check=True,
        capture_output=True,
        text=True,
    ).stdout.strip()
    payload = {
        "schema_version": 1,
        "measurement": "standard_mode_fresh_process_compile_latency",
        "compiler_cache": "fresh process per sample",
        "source": str(source.relative_to(root)),
        "source_sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
        "commit": commit,
        "warmups": args.warmups,
        "runs": args.runs,
        "wall_samples_ms": [round(sample, 3) for sample in wall_samples_ms],
        "wall_median_ms": round(statistics.median(wall_samples_ms), 3),
        "wall_minimum_ms": round(min(wall_samples_ms), 3),
        "wall_maximum_ms": round(max(wall_samples_ms), 3),
        "compiler_pipeline_samples_ms": pipeline_samples_ms,
        "compiler_pipeline_median_ms": round(statistics.median(pipeline_samples_ms), 3),
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(payload, indent=2) + "\n")
    print(json.dumps(payload, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
