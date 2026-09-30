#!/usr/bin/env python3
"""Record current benchmark state and regenerate its current reference tables."""

import argparse
import json
import sys
from datetime import datetime
from pathlib import Path

from benchmark_baseline import (
    BaselineError, TRACKS, CompilerAttribution, atomic_write_json,
    atomic_write_text, compare_dark_performance, comparison_dict, create_snapshot,
    load_dark_counts, load_snapshot, machine_architecture, print_comparison,
    snapshot_path, write_snapshot,
)
from benchmark_profiles import load_profile


def format_number(value: int) -> str:
    return f"{value:,}"


def format_ratio(value: float) -> str:
    if value >= 100:
        return f"{value:.0f}x"
    if value >= 10:
        return f"{value:.1f}x"
    return f"{value:.2f}x"


def load_json_results(results_dir: Path) -> dict:
    return {
        path.stem.replace("_cachegrind", ""): json.loads(path.read_text()).get("results", [])
        for path in results_dir.glob("*_cachegrind.json")
    }


def run_metadata(results_dir: Path) -> tuple[str, CompilerAttribution]:
    timestamp_path = results_dir / "run_timestamp.txt"
    identity_path = results_dir / "run_identity.txt"
    version_path = results_dir / "compiler_version.txt"
    if not timestamp_path.is_file() or not identity_path.is_file() or not version_path.is_file():
        raise BaselineError("run timestamp, identity, or compiler version is missing")
    timestamp = timestamp_path.read_text().strip()
    if not identity_path.read_text().strip():
        raise BaselineError("run identity is empty")
    try:
        parsed = datetime.fromisoformat(timestamp.replace("Z", "+00:00"))
    except ValueError as error:
        raise BaselineError("run timestamp must be ISO-8601") from error
    if parsed.tzinfo is None:
        raise BaselineError("run timestamp must include a UTC offset")
    version = version_path.read_text().strip().splitlines()
    if not version or len(version[0]) != 40:
        raise BaselineError("compiler_version.txt must contain a full Git commit")
    return timestamp, CompilerAttribution(version[0], version[1] if len(version) > 1 else "")


def update_baselines(benchmarks_dir: Path, json_results: dict, profile: str, timestamp: str) -> None:
    """Persist a measured Rust reference independently of Darklang's decision."""
    from diagnostic_references import command_version
    from reference_snapshots import measured_reference, save_reference

    rows = []
    for name in load_profile(benchmarks_dir, profile):
        rust = [row for row in json_results[name] if row.get("language", "").lower() == "rust"]
        if len(rust) != 1:
            raise BaselineError(f"{name}: expected one validated Rust row")
        rows.append({"name": name, "instructions": rust[0]["instructions"], "output_valid": True})
    document = measured_reference(
        benchmarks_dir, "rust", profile, machine_architecture(),
        command_version(["rustc", "--version"]), timestamp, rows,
        ["rustc -C opt-level=3; Cargo --release"], [],
        {"valgrind": command_version(["valgrind", "--version"])},
    )
    save_reference(benchmarks_dir, document)


def update_results(benchmarks_dir: Path, snapshot) -> None:
    """Regenerate reports from stored snapshots after a successful recording."""
    from benchmark_reports import generate_reports

    generate_reports(benchmarks_dir)


def validate_rust_refresh(json_results: dict, profile: list[str]) -> None:
    for name in profile:
        rust = [row for row in json_results[name] if row.get("language", "").lower() == "rust"]
        if len(rust) != 1 or not isinstance(rust[0].get("instructions"), int) or rust[0]["instructions"] <= 0:
            raise BaselineError(f"{name}: audited Rust refresh requires one positive instruction count")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results_dir")
    parser.add_argument("--profile", required=True)
    parser.add_argument("--refresh-baseline", action="store_true")
    parser.add_argument("--reset-dark-baseline", action="store_true")
    parser.add_argument("--snapshot-override")
    args = parser.parse_args()
    results_dir = Path(args.results_dir)
    try:
        profile = load_profile(results_dir.parent.parent, args.profile)
        json_results = load_json_results(results_dir)
        if set(json_results) != set(profile):
            raise BaselineError("profile result set is incomplete")
        timestamp, compiler = run_metadata(results_dir)
        benchmarks_dir = results_dir.parent.parent
        architecture = machine_architecture()
        track = TRACKS[f"{architecture}-{args.profile}-cachegrind"]
        canonical = snapshot_path(benchmarks_dir, "dark", track)
        current = load_dark_counts(results_dir, profile)
        if args.refresh_baseline:
            validate_rust_refresh(json_results, profile)
            update_baselines(benchmarks_dir, json_results, args.profile, timestamp)
        if args.reset_dark_baseline:
            active = create_snapshot(benchmarks_dir, "dark", track, current, timestamp, compiler)
            write_snapshot(canonical, active)
            decision, action, document = "reset", "reset", {"decision": "reset", "snapshot_action": "reset", "benchmarks": []}
        else:
            snapshot_source = Path(args.snapshot_override) if args.snapshot_override else canonical
            previous = load_snapshot(snapshot_source, benchmarks_dir, "dark", track)
            comparison = compare_dark_performance(current, previous.benchmarks)
            print_comparison(comparison, previous)
            decision = comparison.decision
            action = "advanced" if decision == "improved" else "unchanged-equal" if decision == "equal" else "preserved-stronger-baseline"
            active = create_snapshot(benchmarks_dir, "dark", track, current, timestamp, compiler) if decision == "improved" else previous
            if decision == "improved":
                write_snapshot(canonical, active)
            document = comparison_dict(comparison, args.profile, previous, action)
        if decision in {"improved", "reset"} or args.refresh_baseline:
            update_results(benchmarks_dir, active)
        atomic_write_json(results_dir / "dark_suite_decision.json", document)
        print(f"Dark full snapshot: {action}")
        return 1 if decision == "regressed" else 0
    except (BaselineError, OSError, ValueError, json.JSONDecodeError) as error:
        print(f"Dark full recording failed: {error}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
