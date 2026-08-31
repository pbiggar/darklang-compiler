#!/usr/bin/env python3
"""Load benchmark membership, argv workloads, and expected stdout."""

import json
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class BenchmarkInvocation:
    name: str
    args: tuple[str, ...]
    expected_stdout: str


def _load_contract(benchmarks_dir: Path) -> dict[str, object]:
    path = benchmarks_dir / "profiles.json"
    data = json.loads(path.read_text())
    if data.get("schema") != 1:
        raise ValueError("profiles.json must use schema 1")
    if not isinstance(data.get("profiles"), dict) or not isinstance(data.get("workloads"), dict):
        raise ValueError("profiles.json must contain profiles and workloads objects")
    return data


def workload_profile_name(name: str) -> str:
    return "quick" if name in {"quick", "quick-fast"} else name


def load_invocation(benchmarks_dir: Path, profile: str, benchmark: str) -> BenchmarkInvocation:
    data = _load_contract(benchmarks_dir)
    workloads = data["workloads"]
    mode = workload_profile_name(profile)
    workload = workloads.get(benchmark) if isinstance(workloads, dict) else None
    invocation = workload.get(mode) if isinstance(workload, dict) else None
    if not isinstance(invocation, dict):
        raise ValueError(f"no {mode} workload declared for {benchmark}")
    args = invocation.get("args")
    expected = invocation.get("expected_stdout")
    if not isinstance(args, list) or not args or not all(isinstance(arg, str) and arg for arg in args):
        raise ValueError(f"{benchmark} {mode} args must be non-empty strings")
    if not isinstance(expected, str) or not expected.endswith("\n"):
        raise ValueError(f"{benchmark} {mode} expected_stdout must end in a newline")
    return BenchmarkInvocation(benchmark, tuple(args), expected)


def load_profile(benchmarks_dir: Path, name: str) -> list[str]:
    data = _load_contract(benchmarks_dir)
    profiles = data["profiles"]
    benchmarks = profiles.get(name) if isinstance(profiles, dict) else None
    if not isinstance(benchmarks, list):
        raise ValueError(f"unknown benchmark profile: {name}")
    if not benchmarks or not all(isinstance(benchmark, str) and benchmark for benchmark in benchmarks):
        raise ValueError(f"benchmark profile is empty: {name}")
    if len(benchmarks) != len(set(benchmarks)):
        raise ValueError(f"benchmark profile contains duplicates: {name}")

    problems_dir = benchmarks_dir / "problems"
    missing = [benchmark for benchmark in benchmarks if not (problems_dir / benchmark).is_dir()]
    if missing:
        raise ValueError(
            f"benchmark profile {name} contains unknown benchmarks: {', '.join(missing)}"
        )
    for benchmark in benchmarks:
        load_invocation(benchmarks_dir, name, benchmark)
    return benchmarks


def main() -> int:
    if len(sys.argv) not in {2, 4}:
        print("Usage: python3 benchmark_profiles.py <profile> | arguments|expected <profile> <benchmark>")
        return 1
    benchmarks_dir = Path(__file__).resolve().parent.parent
    try:
        if len(sys.argv) == 2:
            print("\n".join(load_profile(benchmarks_dir, sys.argv[1])))
        else:
            invocation = load_invocation(benchmarks_dir, sys.argv[2], sys.argv[3])
            if sys.argv[1] == "arguments":
                print("\n".join(invocation.args))
            elif sys.argv[1] == "expected":
                print(invocation.expected_stdout, end="")
            else:
                raise ValueError(f"unknown operation: {sys.argv[1]}")
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
