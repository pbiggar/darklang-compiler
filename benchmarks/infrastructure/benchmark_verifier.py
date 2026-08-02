#!/usr/bin/env python3
"""Verify full Dark benchmark results without modifying tracked result files."""

import sys
from pathlib import Path

from history_updater import (
    is_reduced_size_benchmark,
    load_json_results,
    load_results_file,
)


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: python3 benchmark_verifier.py <results_dir>")
        return 1

    results_dir = Path(sys.argv[1])
    if not results_dir.is_dir():
        print(f"Error: Results directory not found: {results_dir}")
        return 1

    benchmarks_dir = results_dir.parent.parent
    expected = load_results_file(benchmarks_dir)
    actual = load_json_results(results_dir)
    failures = []
    verified = 0

    for benchmark, results in sorted(actual.items()):
        if is_reduced_size_benchmark(benchmarks_dir, benchmark):
            print(f"Skipping {benchmark}: reduced-size Dark benchmark")
            continue

        dark_results = [
            result
            for result in results
            if result.get("language", "").lower() == "dark"
        ]
        if len(dark_results) != 1:
            failures.append(
                f"{benchmark}: expected one Dark result, found {len(dark_results)}"
            )
            continue

        instructions = dark_results[0].get("instructions", 0)
        expected_instructions = expected.get(benchmark, {}).get("dark")
        if expected_instructions is None:
            failures.append(f"{benchmark}: missing Dark result in RESULTS.md")
            continue

        verified += 1
        if instructions != expected_instructions:
            difference = instructions - expected_instructions
            failures.append(
                f"{benchmark}: expected {expected_instructions:,}, "
                f"measured {instructions:,} ({difference:+,})"
            )

    if verified == 0:
        failures.append("no full-size Dark benchmark results were available to verify")

    if failures:
        print("Benchmark verification failed:")
        for failure in failures:
            print(f"  {failure}")
        return 1

    print(f"Verified {verified} benchmark result(s) against RESULTS.md")
    return 0


if __name__ == "__main__":
    sys.exit(main())
