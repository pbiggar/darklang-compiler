#!/usr/bin/env python3
"""Record current benchmark state and regenerate its current reference tables."""

import argparse
import json
import math
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


DIAGNOSTIC_LANGUAGES = (
    ("darklang-interpreter", "Darklang interpreter"),
    ("node", "Node"),
    ("ocaml", "OCaml"),
    ("python", "Python"),
)


def load_diagnostic_references(
    benchmarks_dir: Path, snapshot
) -> dict[str, dict[str, int]]:
    path = (
        benchmarks_dir
        / "baselines"
        / f"diagnostic-{snapshot.architecture}-{snapshot.profile}-cachegrind.json"
    )
    if not path.is_file():
        return {}
    document = json.loads(path.read_text())
    expected = {
        "schema_version": 1,
        "architecture": snapshot.architecture,
        "profile": snapshot.profile,
        "measurement_policy": snapshot.measurement_policy,
        "contract_sha256": snapshot.contract_sha256,
    }
    for field, value in expected.items():
        if document.get(field) != value:
            # Diagnostic runtimes are never a canonical recording gate. A stale
            # snapshot simply disappears from the generated comparison until it
            # is refreshed for the active workload contract.
            return {}
    implementations = document.get("implementations")
    if not isinstance(implementations, dict):
        raise BaselineError("diagnostic reference snapshot implementations must be an object")
    counts: dict[str, dict[str, int]] = {}
    for language, _ in DIAGNOSTIC_LANGUAGES:
        implementation = implementations.get(language)
        if implementation is None:
            continue
        if not isinstance(implementation, dict) or not isinstance(
            implementation.get("benchmarks"), list
        ):
            raise BaselineError(f"diagnostic {language} benchmark rows must be a list")
        language_counts: dict[str, int] = {}
        seen_names: set[str] = set()
        for row in implementation["benchmarks"]:
            if not isinstance(row, dict) or not isinstance(row.get("name"), str):
                raise BaselineError(f"diagnostic {language} row is malformed")
            instructions = row.get("instructions")
            if not isinstance(instructions, int) or instructions <= 0:
                raise BaselineError(f"diagnostic {language} instruction count must be positive")
            if row["name"] in seen_names:
                raise BaselineError(f"diagnostic {language} repeats {row['name']}")
            seen_names.add(row["name"])
            output_valid = row.get("output_valid")
            if not isinstance(output_valid, bool):
                raise BaselineError(
                    f"diagnostic {language} output-valid marker must be boolean"
                )
            if output_valid:
                language_counts[row["name"]] = instructions
        counts[language] = language_counts
    return counts


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
        "# Benchmark Baselines", "", "Reference instruction counts for the human-audited Rust benchmark pairs.", "",
        "| Benchmark     | Language | Instructions     |",
        "|---------------|----------|------------------|",
    ]
    old_rows = {name: {"instructions": count} for name, count in existing.items()}
    old_rows.update(details)
    for name in sorted(old_rows):
        row = old_rows[name]
        lines.append(
            f"| {name:<13} | rust     | {format_number(row['instructions']):>16} |"
        )
    atomic_write_text(benchmarks_dir / "BASELINES.md", "\n".join(lines) + "\n")


def update_results(benchmarks_dir: Path, snapshot) -> None:
    baselines = load_baselines(benchmarks_dir)
    diagnostics = load_diagnostic_references(benchmarks_dir, snapshot)
    rows = [(row.name, row.instructions, baselines.get(row.name)) for row in snapshot.benchmarks]
    ratios = [dark / rust for _, dark, rust in rows if rust]
    geometric = math.prod(ratios) ** (1 / len(ratios))
    diagnostic_geometric = {}
    for language, _ in DIAGNOSTIC_LANGUAGES:
        language_ratios = [
            diagnostics[language][name] / rust
            for name, _, rust in rows
            if rust and name in diagnostics.get(language, {})
        ]
        if language_ratios:
            diagnostic_geometric[language] = math.prod(language_ratios) ** (
                1 / len(language_ratios)
            )
    lines = [
        "# Benchmark Results",
        "",
        "Best-known compatible full-profile Dark performance vs audited Rust "
        "references, with diagnostic reference runtimes (instruction counts).",
        "",
        f"**Snapshot timestamp:** {snapshot.generated_at}",
        f"**Architecture:** `{snapshot.architecture}`",
        f"**Profile:** `{snapshot.profile}` (schema {snapshot.schema_version})",
        f"**Measurement policy:** `{snapshot.measurement_policy}`",
        f"**Workload contract:** `{snapshot.contract_sha256}`",
        f"**Compiler commit:** `{snapshot.compiler.commit}`"
        + (f" - {snapshot.compiler.subject}" if snapshot.compiler.subject else ""),
        "**Diagnostic references:** informational only; multipliers are instructions "
        "divided by Rust for the same workload.",
        "Every displayed diagnostic row matched the profile's expected stdout.",
        "",
    ]
    headers = ["Benchmark", f"Dark ({format_ratio(geometric)})", "Rust"]
    headers.extend(
        f"{label} ({format_ratio(diagnostic_geometric[language])})"
        if language in diagnostic_geometric
        else label
        for language, label in DIAGNOSTIC_LANGUAGES
    )
    lines.extend(["| " + " | ".join(headers) + " |", "|---|" + "---:|" * (len(headers) - 1)])
    for name, dark, rust in rows:
        dark_cell = (
            format_number(dark)
            if rust is None
            else f"{format_number(dark)} ({format_ratio(dark / rust)})"
        )
        cells = [name, dark_cell, format_number(rust) if rust else "-"]
        for language, _ in DIAGNOSTIC_LANGUAGES:
            diagnostic = diagnostics.get(language, {}).get(name)
            cells.append(
                f"{format_number(diagnostic)} ({format_ratio(diagnostic / rust)})"
                if diagnostic and rust
                else "-"
            )
        lines.append("| " + " | ".join(cells) + " |")
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
            update_baselines(benchmarks_dir, json_results)
        if args.reset_dark_baseline:
            active = create_snapshot(benchmarks_dir, "dark", track, current, timestamp, compiler)
            write_snapshot(canonical, active)
            decision, action, document = "reset", "reset", {"decision": "reset", "snapshot_action": "reset", "benchmarks": []}
        else:
            snapshot_source = Path(args.snapshot_override) if args.snapshot_override else canonical
            previous = load_snapshot(snapshot_source, benchmarks_dir, "dark", track)
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
        print(f"Dark full snapshot: {action}")
        return 1 if decision == "regressed" else 0
    except (BaselineError, OSError, ValueError, json.JSONDecodeError) as error:
        print(f"Dark full recording failed: {error}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
