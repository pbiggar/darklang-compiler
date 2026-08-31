#!/usr/bin/env python3
"""Record current benchmark state and regenerate its current reference tables."""

import argparse
import json
import sys
from datetime import datetime
from pathlib import Path

from benchmark_baseline import (
    BaselineError, TRACKS, CompilerAttribution, atomic_write_json,
    atomic_write_text, compare_suites, comparison_dict, create_snapshot,
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


def load_baselines(benchmarks_dir: Path) -> dict[str, int]:
    rows = {}
    for line in (benchmarks_dir / "BASELINES.md").read_text().splitlines():
        if not line.startswith("|") or line.startswith("| Benchmark") or line.startswith("|---"):
            continue
        cells = [cell.strip() for cell in line.split("|")[1:-1]]
        if len(cells) >= 3 and cells[1] == "rust":
            rows[cells[0]] = int(cells[2].replace(",", ""))
    return rows


def update_baselines(benchmarks_dir: Path, json_results: dict) -> None:
    existing = load_baselines(benchmarks_dir)
    details = {}
    for name, results in json_results.items():
        rust = [row for row in results if row.get("language", "").lower() == "rust"]
        if len(rust) == 1:
            details[name] = rust[0]
            existing[name] = rust[0]["instructions"]
    lines = [
        "# Benchmark Baselines", "", "Reference metrics for the human-audited Rust benchmark pairs.", "",
        "| Benchmark     | Language | Instructions     | Data Refs        | L1 Miss     | LL Miss     | Branches        | Mispred |",
        "|---------------|----------|------------------|------------------|-------------|-------------|-----------------|---------|",
    ]
    old_rows = {name: {"instructions": count} for name, count in existing.items()}
    old_rows.update(details)
    for name in sorted(old_rows):
        row = old_rows[name]
        branches = row.get("branches", 0)
        mispred = row.get("branch_mispredicts", 0)
        rate = mispred / branches * 100 if branches else 0
        lines.append(
            f"| {name:<13} | rust     | {format_number(row['instructions']):>16} | {format_number(row.get('data_refs', 0)):>16} | {format_number(row.get('d1_misses', 0)):>11} | {format_number(row.get('ll_misses', 0)):>11} | {format_number(branches):>15} | {rate:>6.1f}% |"
        )
    atomic_write_text(benchmarks_dir / "BASELINES.md", "\n".join(lines) + "\n")


def update_results(benchmarks_dir: Path, snapshot) -> None:
    baselines = load_baselines(benchmarks_dir)
    rows = [(row.name, row.instructions, baselines.get(row.name)) for row in snapshot.benchmarks]
    ratios = [dark / rust for _, dark, rust in rows if rust]
    geometric = __import__("math").prod(ratios) ** (1 / len(ratios))
    lines = [
        "# Benchmark Results", "", "Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).", "",
        f"**Snapshot timestamp:** {snapshot.generated_at}", f"**Architecture:** `{snapshot.architecture}`",
        f"**Profile:** `{snapshot.profile}` (schema {snapshot.schema_version})",
        f"**Measurement policy:** `{snapshot.measurement_policy}`", f"**Workload contract:** `{snapshot.contract_sha256}`",
        f"**Compiler commit:** `{snapshot.compiler.commit}`" + (f" - {snapshot.compiler.subject}" if snapshot.compiler.subject else ""), "",
        f"| Benchmark | Dark ({format_ratio(geometric)}) | Rust |", "|---|---:|---:|",
    ]
    for name, dark, rust in rows:
        dark_cell = format_number(dark) if rust is None else f"{format_number(dark)} ({format_ratio(dark / rust)})"
        lines.append(f"| {name} | {dark_cell} | {format_number(rust) if rust else '-'} |")
    atomic_write_text(benchmarks_dir / "RESULTS.md", "\n".join(lines) + "\n")


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
            update_baselines(benchmarks_dir, json_results)
        if args.reset_dark_baseline:
            active = create_snapshot(benchmarks_dir, "dark", track, current, timestamp, compiler)
            write_snapshot(canonical, active)
            decision, action, document = "reset", "reset", {"decision": "reset", "snapshot_action": "reset", "benchmarks": []}
        else:
            previous = load_snapshot(canonical, benchmarks_dir, "dark", track)
            comparison = compare_suites(current, previous.benchmarks)
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
        print(f"Dark routine snapshot: {action}")
        return 1 if decision == "regressed" else 0
    except (BaselineError, OSError, ValueError, json.JSONDecodeError) as error:
        print(f"Dark routine recording failed: {error}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
