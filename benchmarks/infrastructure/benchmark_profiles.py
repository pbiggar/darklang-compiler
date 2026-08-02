#!/usr/bin/env python3
"""Load named benchmark profiles shared by the benchmark tools."""

import sys
from pathlib import Path


def load_profile(benchmarks_dir: Path, name: str) -> list[str]:
    profile_path = benchmarks_dir / "profiles" / f"{name}.txt"
    if not profile_path.is_file():
        raise ValueError(f"unknown benchmark profile: {name}")

    benchmarks = [
        line.strip()
        for line in profile_path.read_text().splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    if not benchmarks:
        raise ValueError(f"benchmark profile is empty: {name}")
    if len(benchmarks) != len(set(benchmarks)):
        raise ValueError(f"benchmark profile contains duplicates: {name}")

    problems_dir = benchmarks_dir / "problems"
    missing = [benchmark for benchmark in benchmarks if not (problems_dir / benchmark).is_dir()]
    if missing:
        raise ValueError(
            f"benchmark profile {name} contains unknown benchmarks: {', '.join(missing)}"
        )
    return benchmarks


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: python3 benchmark_profiles.py <profile>")
        return 1
    benchmarks_dir = Path(__file__).resolve().parent.parent
    try:
        print("\n".join(load_profile(benchmarks_dir, sys.argv[1])))
    except ValueError as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
