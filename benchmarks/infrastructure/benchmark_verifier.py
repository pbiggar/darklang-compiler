#!/usr/bin/env python3
"""Read-only exact-product verification of a canonical Dark benchmark snapshot."""

import argparse
import sys
from pathlib import Path

from benchmark_baseline import (
    BaselineError,
    TRACKS,
    atomic_write_json,
    compare_suites,
    comparison_dict,
    load_dark_counts,
    load_snapshot,
    machine_architecture,
    print_comparison,
    snapshot_path,
)
from benchmark_profiles import load_profile


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results_dir")
    parser.add_argument("profile")
    parser.add_argument(
        "--require-recorded",
        action="store_true",
        help="fail when the run is improved and therefore must be recorded",
    )
    args = parser.parse_args()
    results_dir = Path(args.results_dir)
    if not results_dir.is_dir():
        print(f"Error: Results directory not found: {results_dir}")
        return 1
    benchmarks_dir = results_dir.parent.parent
    try:
        profile = load_profile(benchmarks_dir, args.profile)
        architecture = machine_architecture()
        track = TRACKS[f"{architecture}-{args.profile}-cachegrind"]
        baseline = load_snapshot(
            snapshot_path(benchmarks_dir, "dark", track),
            benchmarks_dir,
            "dark",
            track,
        )
        current = load_dark_counts(results_dir, profile)
        comparison = compare_suites(current, baseline.benchmarks)
    except (BaselineError, OSError, ValueError) as error:
        print(f"Dark benchmark verification requires a baseline reset: {error}")
        print(
            "Run a complete successful routine suite with "
            "--reset-dark-baseline. No tracked benchmark files were changed."
        )
        return 1

    if comparison.decision == "improved":
        action = "requires-recording"
    elif comparison.decision == "equal":
        action = "unchanged-equal"
    else:
        action = "preserved-stronger-baseline"
    print_comparison(comparison, baseline)
    print(f"Dark routine snapshot: {action}")
    atomic_write_json(
        results_dir / "dark_suite_decision.json",
        comparison_dict(comparison, args.profile, baseline, action),
    )

    if comparison.decision == "regressed":
        return 1
    if comparison.decision == "improved":
        print("Routine improvement must be recorded before integration.")
    if comparison.decision == "improved" and args.require_recorded:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
