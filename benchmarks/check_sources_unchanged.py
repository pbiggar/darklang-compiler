#!/usr/bin/env python3
"""Reject integration candidates that alter Darklang benchmark problem sources."""

from __future__ import annotations

import argparse
import subprocess
from pathlib import Path


class SourceGateError(ValueError):
    """The requested Git comparison could not establish source integrity."""


def git(repo: Path, *args: str) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        ("git", "-C", str(repo), *args),
        check=False,
        capture_output=True,
    )


def changed_benchmark_sources(repo: Path, base: str) -> tuple[str, ...]:
    ancestor = git(repo, "merge-base", "--is-ancestor", base, "HEAD")
    if ancestor.returncode != 0:
        detail = ancestor.stderr.decode(errors="replace").strip()
        raise SourceGateError(
            f"benchmark source base is not an ancestor of HEAD: {base}"
            + (f" ({detail})" if detail else "")
        )

    comparison = git(
        repo,
        "diff",
        "--name-only",
        "--no-renames",
        "-z",
        f"{base}..HEAD",
        "--",
        "benchmarks/problems",
    )
    if comparison.returncode != 0:
        detail = comparison.stderr.decode(errors="replace").strip()
        raise SourceGateError(
            "could not compare benchmark problem sources"
            + (f": {detail}" if detail else "")
        )
    paths = (
        path.decode(errors="surrogateescape")
        for path in comparison.stdout.split(b"\0")
        if path
    )
    # Protect all inputs in each Darklang implementation directory. Reference
    # implementations may evolve without weakening the Darklang workload.
    return tuple(sorted(
        path for path in paths
        if len(Path(path).parts) >= 4 and Path(path).parts[3] == "dark"
    ))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", required=True, help="integration base revision")
    parser.add_argument(
        "--repo",
        type=Path,
        default=Path(__file__).resolve().parent.parent,
        help="repository root (default: inferred from this script)",
    )
    args = parser.parse_args()

    try:
        changed = changed_benchmark_sources(args.repo.resolve(), args.base)
    except (OSError, SourceGateError) as error:
        print(f"Benchmark source integrity gate failed: {error}")
        return 1

    if not changed:
        print("Benchmark source integrity gate passed")
        return 0

    print(
        "Benchmark source integrity gate failed: "
        "candidate changes Darklang benchmark problem sources"
    )
    for path in changed[:20]:
        print(f"  - {path}")
    if len(changed) > 20:
        print(f"  - ... and {len(changed) - 20} more")
    print(
        "Remove Darklang benchmark problem source changes from the task branch before integration."
    )
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
