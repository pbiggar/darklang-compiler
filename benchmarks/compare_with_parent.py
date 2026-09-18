#!/usr/bin/env python3
"""Compare retained Dark benchmark measurements with the task parent's snapshot."""

from __future__ import annotations

import argparse
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent / "infrastructure"))

from benchmark_baseline import (  # noqa: E402
    BaselineError,
    TRACKS,
    compare_suites,
    load_dark_counts,
    load_snapshot,
    machine_architecture,
)
from benchmark_profiles import load_profile  # noqa: E402


def git(project_root: Path, *args: str) -> str:
    completed = subprocess.run(
        ("git", "-C", str(project_root), *args),
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        detail = completed.stderr.strip() or completed.stdout.strip()
        raise BaselineError(f"git {' '.join(args)} failed: {detail}")
    return completed.stdout.strip()


def resolve_parent(project_root: Path, revision: str | None) -> str:
    if revision is None:
        revision = git(project_root, "merge-base", "HEAD", "@{upstream}")
    parent = git(project_root, "rev-parse", "--verify", f"{revision}^{{commit}}")
    git(project_root, "merge-base", "--is-ancestor", parent, "HEAD")
    return parent


def compare_with_parent(
    project_root: Path,
    results_dir: Path,
    parent: str,
    quiet: bool,
) -> int:
    benchmarks_dir = project_root / "benchmarks"
    profile = load_profile(benchmarks_dir, "full")
    architecture = machine_architecture()
    track = TRACKS[f"{architecture}-full-cachegrind"]
    snapshot_relative = f"benchmarks/baselines/dark-{track.id}.json"
    snapshot_document = git(project_root, "show", f"{parent}:{snapshot_relative}")

    with tempfile.NamedTemporaryFile(mode="w", suffix=".json") as snapshot_file:
        snapshot_file.write(snapshot_document)
        snapshot_file.flush()
        snapshot = load_snapshot(
            Path(snapshot_file.name), benchmarks_dir, "dark", track
        )

    current = load_dark_counts(results_dir, profile)
    comparison = compare_suites(current, snapshot.benchmarks)
    if not quiet:
        print(f"Task parent: {parent}")
        print(
            "Parent snapshot: compiler "
            f"{snapshot.compiler.commit}, contract {snapshot.contract_sha256}"
        )
        for row in comparison.rows:
            if row.absolute_delta != 0:
                print(
                    f"  {row.name}: current {row.current:,}, parent {row.baseline:,}, "
                    f"delta {row.absolute_delta:+,} ({row.percentage_delta:+.3f}%)"
                )
    print(
        f"Dark candidate/parent: {comparison.decision}; current/parent geometric "
        f"ratio {comparison.ratio:.6f}"
    )
    return 1 if comparison.decision == "regressed" else 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results_dir", type=Path)
    parser.add_argument(
        "--parent",
        help="parent revision (default: merge-base of HEAD and the branch upstream)",
    )
    parser.add_argument(
        "--quiet", action="store_true", help="print only the aggregate comparison"
    )
    args = parser.parse_args()

    project_root = Path(__file__).resolve().parent.parent
    results_dir = args.results_dir.resolve()
    if not results_dir.is_dir():
        print(f"Parent comparison failed: results directory not found: {results_dir}")
        return 1
    try:
        parent = resolve_parent(project_root, args.parent)
        return compare_with_parent(project_root, results_dir, parent, args.quiet)
    except (BaselineError, OSError, ValueError) as error:
        print(f"Parent comparison failed: {error}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
