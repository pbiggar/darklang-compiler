#!/usr/bin/env python3
"""Verify a named Dark benchmark profile without modifying tracked files."""

import sys
from pathlib import Path

from benchmark_profiles import load_profile
from history_updater import (
    is_reduced_size_benchmark,
    load_json_results,
    load_results_file,
)


def main() -> int:
    if len(sys.argv) != 3:
        print("Usage: python3 benchmark_verifier.py <results_dir> <profile>")
        return 1

    results_dir = Path(sys.argv[1])
    if not results_dir.is_dir():
        print(f"Error: Results directory not found: {results_dir}")
        return 1

    benchmarks_dir = results_dir.parent.parent
    try:
        profile = load_profile(benchmarks_dir, sys.argv[2])
    except ValueError as error:
        print(f"Error: {error}")
        return 1
    expected = load_results_file(benchmarks_dir)
    actual = load_json_results(results_dir)
    failures = []
    verified = 0

    profile_names = set(profile)
    actual_names = set(actual)
    expected_names = set(expected)
    for benchmark in sorted(profile_names - actual_names):
        failures.append(f"{benchmark}: missing result from profile run")
    for benchmark in sorted(actual_names - profile_names):
        failures.append(f"{benchmark}: unexpected result outside profile")
    for benchmark in sorted(profile_names - expected_names):
        failures.append(f"{benchmark}: missing row in RESULTS.md")
    for benchmark in sorted(expected_names - profile_names):
        failures.append(f"{benchmark}: unexpected RESULTS.md row outside profile")

    for benchmark in profile:
        if benchmark not in actual:
            continue
        results = actual[benchmark]
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

    print(
        f"Verified {verified} full-size result(s) across the "
        f"{len(profile)}-benchmark {sys.argv[2]} profile against RESULTS.md"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
