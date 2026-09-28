#!/usr/bin/env python3
"""Keep measured deployed-head counts separate from the canonical best snapshot."""

from __future__ import annotations

import argparse
import subprocess
from pathlib import Path

from benchmark_baseline import (
    BaselineError, CompilerAttribution, TRACKS, atomic_write_json,
    compare_dark_performance, comparison_dict, create_snapshot,
    load_dark_counts, load_snapshot, machine_architecture, print_comparison,
    write_snapshot,
)
from benchmark_profiles import load_profile


def git(repo: Path, *args: str) -> str:
    result = subprocess.run(["git", "-C", str(repo), *args], capture_output=True, text=True)
    if result.returncode:
        raise BaselineError(result.stderr.strip() or "Git command failed")
    return result.stdout.strip()


def state_dir(repo: Path) -> Path:
    return Path(git(repo, "rev-parse", "--path-format=absolute", "--git-common-dir")) / "mergetrain-deployed-benchmarks"


def measure(results: Path):
    repo = results.parent.parent.parent
    benchmarks = repo / "benchmarks"
    track = TRACKS[f"{machine_architecture()}-full-cachegrind"]
    commit = git(repo, "rev-parse", "HEAD")
    recorded = (results / "compiler_version.txt").read_text().splitlines()[0]
    if recorded != commit:
        raise BaselineError("benchmark result commit differs from worktree HEAD")
    snapshot = create_snapshot(
        benchmarks, "dark", track,
        load_dark_counts(results, load_profile(benchmarks, "full")),
        (results / "run_timestamp.txt").read_text().strip(),
        CompilerAttribution(commit, git(repo, "log", "-1", "--format=%s")),
    )
    return repo, benchmarks, track, snapshot


def verify(results: Path) -> int:
    repo, benchmarks, track, current = measure(results)
    base = git(repo, "rev-parse", "refs/remotes/mergetrain-local/main")
    baseline = load_snapshot(state_dir(repo) / f"{track.id}.json", benchmarks, "dark", track)
    if baseline.compiler.commit != base:
        raise BaselineError(
            f"deployed benchmark baseline {baseline.compiler.commit} differs from integration head {base}"
        )
    comparison = compare_dark_performance(current.benchmarks, baseline.benchmarks)
    print_comparison(comparison, baseline, details=False)
    decision = comparison_dict(comparison, "full", baseline, "deployed-head-comparison")
    atomic_write_json(results / "dark_suite_decision.json", decision)
    # A failed benchmark gate may later receive an exact human waiver. Retain
    # its measurement, but promote it only after the job is deployed.
    write_snapshot(state_dir(repo) / "pending" / f"{current.compiler.commit}.json", current)
    return 1 if comparison.decision == "regressed" else 0


def seed(results: Path) -> None:
    repo, benchmarks, track, snapshot = measure(results)
    head = git(repo, "rev-parse", "refs/remotes/mergetrain-local/main")
    if snapshot.compiler.commit != head:
        raise BaselineError("seed measurement is not the deployed integration head")
    target = state_dir(repo) / f"{track.id}.json"
    if target.exists():
        raise BaselineError("deployed baseline already exists")
    write_snapshot(target, snapshot)


def promote(repo: Path, commit: str) -> None:
    head = git(repo, "rev-parse", "refs/remotes/mergetrain-local/main")
    if head != commit:
        raise BaselineError("deployed head differs from candidate; cannot promote measurement")
    track = TRACKS[f"{machine_architecture()}-full-cachegrind"]
    benchmarks = repo / "benchmarks"
    target = state_dir(repo) / f"{track.id}.json"
    if target.exists() and load_snapshot(target, benchmarks, "dark", track).compiler.commit == commit:
        return
    pending = state_dir(repo) / "pending" / f"{commit}.json"
    snapshot = load_snapshot(pending, benchmarks, "dark", track)
    if snapshot.compiler.commit != commit:
        raise BaselineError("pending measurement has a different compiler commit")
    write_snapshot(target, snapshot)
    pending.unlink()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("verify", "seed", "promote"))
    parser.add_argument("target")
    parser.add_argument("commit", nargs="?")
    args = parser.parse_args()
    try:
        if args.action == "verify":
            return verify(Path(args.target).resolve())
        if args.action == "seed":
            seed(Path(args.target).resolve())
        else:
            if not args.commit:
                parser.error("promote requires the deployed commit")
            promote(Path(args.target).resolve(), args.commit)
        return 0
    except (BaselineError, OSError, ValueError) as error:
        print(f"Deployed benchmark baseline error: {error}")
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
