#!/usr/bin/env python3
"""
Update benchmark result files after a benchmark run.

Manages three files plus the canonical Dark snapshot:
- RESULTS.md: Best compatible routine profile vs audited Rust
- BASELINES.md: Detailed baseline metrics for reference languages (no Dark)
- HISTORY.md: Append-only log of all Dark benchmark runs

Usage: python3 history_updater.py <results_dir> --profile <profile> [--machine <id>] [--refresh-baseline] [--reset-dark-baseline]
"""

import argparse
import json
import sys
from datetime import datetime
from pathlib import Path

from benchmark_baseline import (
    BaselineError,
    CompilerAttribution,
    Snapshot,
    atomic_write_json,
    atomic_write_text,
    compare_suites,
    comparison_dict,
    create_snapshot,
    load_dark_counts,
    load_snapshot,
    machine_architecture,
    print_comparison,
    snapshot_path,
    write_snapshot,
)
from benchmark_profiles import load_profile

RESULTS_FILE = "RESULTS.md"
BASELINES_FILE = "BASELINES.md"
HISTORY_FILE = "HISTORY.md"


def format_number(n: int) -> str:
    """Format large numbers with commas."""
    return f"{n:,}"


def format_speedup(ratio: float | None) -> str:
    """Format speedup ratio as 'Nx' string."""
    if ratio is None:
        return "-"
    if ratio >= 100:
        return f"{ratio:.0f}x"
    if ratio >= 10:
        return f"{ratio:.1f}x"
    return f"{ratio:.2f}x"


def geometric_mean(values: list[float]) -> float | None:
    """Calculate geometric mean of positive values."""
    if not values:
        return None
    product = 1.0
    for v in values:
        product *= v
    return product ** (1 / len(values))


def load_json_results(results_dir: Path) -> dict:
    """Load all cachegrind JSON results from the results directory."""
    results = {}
    for json_file in results_dir.glob("*_cachegrind.json"):
        benchmark_name = json_file.stem.replace("_cachegrind", "")
        with open(json_file) as f:
            data = json.load(f)
            results[benchmark_name] = data.get("results", [])
    return results


def get_run_metadata(results_dir: Path) -> dict:
    """Extract timestamp and commit info from results directory."""
    timestamp_file = results_dir / "run_timestamp.txt"
    identity_file = results_dir / "run_identity.txt"
    if not timestamp_file.is_file() or not identity_file.is_file():
        raise BaselineError("run timestamp or unique run identity is missing")
    timestamp = timestamp_file.read_text().strip()
    run_identity = identity_file.read_text().strip()
    if not timestamp or not run_identity:
        raise BaselineError("run timestamp and identity must be non-empty")
    try:
        parsed_timestamp = datetime.fromisoformat(timestamp.replace("Z", "+00:00"))
    except ValueError as error:
        raise BaselineError("run timestamp must be ISO-8601") from error
    if parsed_timestamp.tzinfo is None:
        raise BaselineError("run timestamp must include a UTC offset")

    version_file = results_dir / "compiler_version.txt"
    commit_hash = ""
    commit_message = ""
    if version_file.exists():
        lines = version_file.read_text().strip().split("\n")
        commit_hash = lines[0] if lines else ""
        commit_message = lines[1] if len(lines) > 1 else ""
    if len(commit_hash) != 40:
        raise BaselineError("compiler_version.txt must contain a full Git commit")

    return {
        "timestamp": timestamp,
        "run_identity": run_identity,
        "commit_hash": commit_hash,
        "commit_message": commit_message,
    }


# ============================================================================
# RESULTS.md - Quick overview table
# ============================================================================

def update_results_file(benchmarks_dir: Path, snapshot: Snapshot, baselines: dict):
    """Regenerate RESULTS.md from the canonical Dark snapshot and audited Rust rows."""
    results_path = benchmarks_dir / RESULTS_FILE
    existing = {
        row.name: {"dark": row.instructions}
        for row in snapshot.benchmarks
    }
    for benchmark, baseline_list in baselines.items():
        if benchmark not in existing:
            continue
        rust_rows = [
            row for row in baseline_list
            if row.get("language", "").lower() == "rust"
            and isinstance(row.get("instructions"), int)
            and row["instructions"] > 0
        ]
        if len(rust_rows) == 1:
            existing[benchmark]["rust"] = rust_rows[0]["instructions"]

    # Only the human-audited Dark/Rust pairs are canonical comparisons. Other
    # language implementations remain useful diagnostics but are not covered by
    # PARITY.json and must not silently contribute stale-workload ratios.
    languages = ["dark", "rust"]
    langs_with_speedup = ["dark"]  # Rust is baseline

    # Calculate speedups for each benchmark
    speedups = {}  # {benchmark: {lang: ratio}}
    avg_speedups = {lang: [] for lang in langs_with_speedup}

    for benchmark in existing:
        speedups[benchmark] = {}
        rust_instrs = existing[benchmark].get("rust", 0)
        for lang in langs_with_speedup:
            if lang in existing[benchmark] and rust_instrs > 0:
                ratio = existing[benchmark][lang] / rust_instrs
                speedups[benchmark][lang] = ratio
                avg_speedups[lang].append(ratio)

    # Calculate geometric mean of speedups for header
    avg_speedup_values = {}
    for lang in langs_with_speedup:
        avg_speedup_values[lang] = geometric_mean(avg_speedups[lang])

    # Build header names with averages
    header_names = {}
    for lang in languages:
        if lang in langs_with_speedup and avg_speedup_values.get(lang) is not None:
            header_names[lang] = f"{lang.capitalize()} ({format_speedup(avg_speedup_values[lang])})"
        else:
            header_names[lang] = lang.capitalize()

    # Calculate column widths (including inline speedups)
    col_widths = {"benchmark": len("Benchmark")}
    for lang in languages:
        col_widths[lang] = len(header_names[lang])

    for benchmark in existing:
        col_widths["benchmark"] = max(col_widths["benchmark"], len(benchmark))
        for lang in languages:
            if lang in existing[benchmark]:
                val = format_number(existing[benchmark][lang])
                if lang in langs_with_speedup and benchmark in speedups and lang in speedups[benchmark]:
                    val += f" ({format_speedup(speedups[benchmark][lang])})"
                col_widths[lang] = max(col_widths[lang], len(val))
            else:
                col_widths[lang] = max(col_widths[lang], len("-"))

    # Build table
    lines = [
        "# Benchmark Results",
        "",
        "Best-known compatible routine-profile Dark performance vs audited Rust references (instruction counts).",
        "",
        f"**Snapshot timestamp:** {snapshot.generated_at}",
        f"**Architecture:** `{snapshot.architecture}`",
        f"**Profile:** `{snapshot.profile}` (schema {snapshot.schema_version})",
        f"**Measurement policy:** `{snapshot.measurement_policy}`",
        f"**Workload contract:** `{snapshot.contract_sha256}`",
        f"**Compiler commit:** `{snapshot.compiler.commit}`"
        + (f" - {snapshot.compiler.subject}" if snapshot.compiler.subject else ""),
        "",
    ]

    # Header row
    header = f"| {'Benchmark':<{col_widths['benchmark']}} |"
    separator = f"|{'-' * (col_widths['benchmark'] + 2)}|"
    for lang in languages:
        header += f" {header_names[lang]:>{col_widths[lang]}} |"
        separator += f"{'-' * (col_widths[lang] + 2)}|"
    lines.append(header)
    lines.append(separator)

    # Data rows
    for benchmark in (row.name for row in snapshot.benchmarks):
        row = f"| {benchmark:<{col_widths['benchmark']}} |"
        for lang in languages:
            if lang in existing[benchmark]:
                val = format_number(existing[benchmark][lang])
                if lang in langs_with_speedup and benchmark in speedups and lang in speedups[benchmark]:
                    val += f" ({format_speedup(speedups[benchmark][lang])})"
            else:
                val = "-"
            row += f" {val:>{col_widths[lang]}} |"
        lines.append(row)

    lines.append("")
    atomic_write_text(results_path, "\n".join(lines))
    print(f"Results updated: {results_path}")


# ============================================================================
# BASELINES.md - Detailed baselines for reference languages (no Dark)
# ============================================================================

def load_baselines_file(benchmarks_dir: Path) -> dict:
    """Load audited Rust baselines from BASELINES.md.

    Returns: {benchmark: [{language, instructions, data_refs, ...}]}
    """
    baselines_path = benchmarks_dir / BASELINES_FILE
    if not baselines_path.exists():
        return {}

    baselines = {}
    content = baselines_path.read_text()
    parity_data = json.loads((benchmarks_dir / "PARITY.json").read_text())
    comparable = {
        benchmark
        for benchmark, entry in parity_data.get("benchmarks", {}).items()
        if entry.get("status") == "comparable"
    }

    in_table = False
    for line in content.split("\n"):
        if line.startswith("| Benchmark"):
            in_table = True
            continue
        if line.startswith("|---"):
            continue
        if in_table and line.startswith("|"):
            cols = [c.strip() for c in line.split("|")]
            if len(cols) >= 9:
                benchmark = cols[1]
                lang = cols[2].lower()
                if benchmark not in comparable or lang != "rust":
                    continue
                try:
                    entry = {
                        "language": lang,
                        "instructions": int(cols[3].replace(",", "")),
                        "data_refs": int(cols[4].replace(",", "")),
                        "d1_misses": int(cols[5].replace(",", "")),
                        "ll_misses": int(cols[6].replace(",", "")),
                        "branches": int(cols[7].replace(",", "")),
                        "branch_mispredicts": int(float(cols[8].replace("%", "")) * int(cols[7].replace(",", "")) / 100),
                    }
                    if benchmark not in baselines:
                        baselines[benchmark] = []
                    # Replace existing entry for this language
                    baselines[benchmark] = [b for b in baselines[benchmark] if b["language"] != lang]
                    baselines[benchmark].append(entry)
                except (ValueError, IndexError):
                    pass

    return baselines


def update_baselines_file(benchmarks_dir: Path, json_results: dict):
    """Update BASELINES.md with audited Rust reference results."""
    baselines_path = benchmarks_dir / BASELINES_FILE

    # Load existing baselines
    existing = load_baselines_file(benchmarks_dir)

    # Merge with new results (excluding Dark)
    for benchmark, benchmark_results in json_results.items():
        if benchmark not in existing:
            existing[benchmark] = []
        for r in benchmark_results:
            lang = r.get("language", "").lower()
            if lang != "rust":
                continue
            # Replace existing entry for this language
            existing[benchmark] = [b for b in existing[benchmark] if b["language"] != lang]
            existing[benchmark].append({
                "language": lang,
                "instructions": r.get("instructions", 0),
                "data_refs": r.get("data_refs", 0),
                "d1_misses": r.get("d1_misses", 0),
                "ll_misses": r.get("ll_misses", 0),
                "branches": r.get("branches", 0),
                "branch_mispredicts": r.get("branch_mispredicts", 0),
            })

    # Generate table
    lines = [
        "# Benchmark Baselines",
        "",
        "Reference metrics for the human-audited Rust benchmark pairs.",
        "",
        "| Benchmark     | Language | Instructions     | Data Refs        | L1 Miss     | LL Miss     | Branches        | Mispred |",
        "|---------------|----------|------------------|------------------|-------------|-------------|-----------------|---------|",
    ]

    for benchmark in sorted(existing.keys()):
        for entry in sorted(existing[benchmark], key=lambda x: x["language"]):
            lang = entry["language"]
            instrs = format_number(entry["instructions"])
            data_refs = format_number(entry["data_refs"])
            d1_misses = format_number(entry["d1_misses"])
            ll_misses = format_number(entry["ll_misses"])
            branches = format_number(entry["branches"])
            mispred_rate = (entry["branch_mispredicts"] / entry["branches"] * 100) if entry["branches"] > 0 else 0

            lines.append(
                f"| {benchmark:<13} | {lang:<8} | {instrs:>16} | {data_refs:>16} | {d1_misses:>11} | {ll_misses:>11} | {branches:>15} | {mispred_rate:>6.1f}% |"
            )

    lines.append("")
    atomic_write_text(baselines_path, "\n".join(lines))
    print(f"Baselines updated: {baselines_path}")


# ============================================================================
# HISTORY.md - Append-only Dark results log
# ============================================================================

SUITE_RUNS_HEADER = """| Timestamp                 | Run                                   | Decision  | Machine | Commit   | Profile | Architecture | Current/Baseline |
|---------------------------|---------------------------------------|-----------|---------|----------|---------|--------------|------------------|"""

HISTORY_TABLE_HEADER = """| Date       | Machine | Commit   | Benchmark     | Instructions     | Data Refs        | L1 Miss     | LL Miss     | Branches        | Mispred |
|------------|---------|----------|---------------|------------------|------------------|-------------|-------------|-----------------|---------|"""


def history_row_cells(line: str) -> list[str]:
    return [cell.strip() for cell in line.split("|")[1:-1]]


def registered_machine_ids(history_path: Path) -> set[str]:
    if not history_path.exists():
        return set()
    lines = history_path.read_text().splitlines()
    registry_start = next(
        (index for index, line in enumerate(lines) if line.strip() == "## Machine Registry"),
        None,
    )
    if registry_start is None:
        return set()
    registry_end = next(
        (
            index
            for index, line in enumerate(lines[registry_start + 1 :], registry_start + 1)
            if line.startswith("## ")
        ),
        len(lines),
    )
    return {
        cells[0]
        for line in lines[registry_start + 1 : registry_end]
        if line.startswith("|")
        and (cells := history_row_cells(line))
        and cells[0] not in {"Machine", "---------"}
    }


def validate_history_log(history_path: Path) -> None:
    if not history_path.is_file():
        raise BaselineError(f"{HISTORY_FILE} is missing")
    lines = history_path.read_text().splitlines()
    log_index = next((i for i, line in enumerate(lines) if line == "## Log"), None)
    if log_index is None or not any(
        line.startswith("|---") for line in lines[log_index + 1 :]
    ):
        raise BaselineError(f"{HISTORY_FILE} has no valid Log table")


def validate_new_run_identity(history_path: Path, run_identity: str) -> None:
    if any(
        len(cells := history_row_cells(line)) >= 2 and cells[1] == run_identity
        for line in history_path.read_text().splitlines()
        if line.startswith("|")
    ):
        raise BaselineError(f"run identity already exists in {HISTORY_FILE}: {run_identity}")


def append_to_history(
    benchmarks_dir: Path,
    json_results: dict,
    metadata: dict,
    decision: str,
    ratio: float,
    profile_name: str,
    architecture: str,
    profile: list[str],
):
    """Add a complete Dark run without replacing any prior run identity."""
    history_path = benchmarks_dir / HISTORY_FILE
    new_rows = []
    for benchmark in profile:
        dark_rows = [
            row for row in json_results[benchmark]
            if row.get("language", "").lower() == "dark"
        ]
        row = dark_rows[0]
        branch_count = row.get("branches", 0)
        mispreds = row.get("branch_mispredicts", 0)
        mispred_rate = (mispreds / branch_count * 100) if branch_count > 0 else 0
        new_rows.append(
            f"| {metadata['timestamp'][:10]} | {metadata['machine']} | "
            f"{metadata['commit_hash'][:8]} | {benchmark:<13} | "
            f"{format_number(row['instructions']):>16} | "
            f"{format_number(row.get('data_refs', 0)):>16} | "
            f"{format_number(row.get('d1_misses', 0)):>11} | "
            f"{format_number(row.get('ll_misses', 0)):>11} | "
            f"{format_number(branch_count):>15} | {mispred_rate:>6.1f}% |"
        )

    if not history_path.is_file():
        raise BaselineError(f"{HISTORY_FILE} is missing")
    lines = history_path.read_text().splitlines()
    suite_index = next(
        (i for i, line in enumerate(lines) if line == "## Suite Runs"), None
    )
    suite_row = (
        f"| {metadata['timestamp']} | {metadata['run_identity']} | {decision:<9} | "
        f"{metadata['machine']} | {metadata['commit_hash'][:8]} | {profile_name} | "
        f"{architecture} | {ratio:.6f} |"
    )
    if suite_index is None:
        log_section = next(
            (i for i, line in enumerate(lines) if line == "## Log"), None
        )
        if log_section is None:
            raise BaselineError(f"{HISTORY_FILE} has no Log section")
        suite_section = [
            "## Suite Runs",
            "",
            *SUITE_RUNS_HEADER.splitlines(),
            suite_row,
            "",
        ]
        lines[log_section:log_section] = suite_section
    else:
        suite_separator = next(
            (
                i
                for i in range(suite_index + 1, len(lines))
                if lines[i].startswith("|---")
            ),
            None,
        )
        if suite_separator is None:
            raise BaselineError(f"{HISTORY_FILE} has no Suite Runs table")
        lines[suite_separator + 1 : suite_separator + 1] = [suite_row]
    log_index = next((i for i, line in enumerate(lines) if line == "## Log"), None)
    if log_index is None:
        raise BaselineError(f"{HISTORY_FILE} has no Log section")
    separator_index = next(
        (i for i in range(log_index + 1, len(lines)) if lines[i].startswith("|---")),
        None,
    )
    if separator_index is None:
        raise BaselineError(f"{HISTORY_FILE} has no Log table")
    lines[separator_index - 1 : separator_index + 1] = HISTORY_TABLE_HEADER.splitlines()
    insertion_index = separator_index + 1
    lines[insertion_index:insertion_index] = new_rows
    atomic_write_text(history_path, "\n".join(lines) + "\n")
    print(f"History updated: {history_path}")


# ============================================================================
# Main entry point
# ============================================================================

def validate_rust_refresh(json_results: dict, profile: list[str]) -> None:
    for benchmark in profile:
        rust_rows = [
            row for row in json_results[benchmark]
            if row.get("language", "").lower() == "rust"
        ]
        if len(rust_rows) != 1:
            raise BaselineError(
                f"{benchmark}: audited Rust refresh expected one result, found {len(rust_rows)}"
            )
        instructions = rust_rows[0].get("instructions")
        if (
            isinstance(instructions, bool)
            or not isinstance(instructions, int)
            or instructions <= 0
        ):
            raise BaselineError(f"{benchmark}: audited Rust instruction count is invalid")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results_dir")
    parser.add_argument("--profile", required=True)
    parser.add_argument("--machine")
    parser.add_argument("--refresh-baseline", action="store_true")
    parser.add_argument("--reset-dark-baseline", action="store_true")
    args = parser.parse_args()
    results_dir = Path(args.results_dir)
    if not results_dir.is_dir():
        print(f"Error: Results directory not found: {results_dir}")
        return 1

    benchmarks_dir = results_dir.parent.parent
    registered_machines = registered_machine_ids(benchmarks_dir / HISTORY_FILE)
    if (
        args.machine is not None
        and registered_machines
        and args.machine not in registered_machines
    ):
        print(f"Error: machine ID {args.machine!r} is not registered in {HISTORY_FILE}")
        return 1

    try:
        profile = load_profile(benchmarks_dir, args.profile)
        json_results = load_json_results(results_dir)
        actual_names = set(json_results)
        expected_names = set(profile)
        if actual_names != expected_names:
            missing = sorted(expected_names - actual_names)
            unexpected = sorted(actual_names - expected_names)
            details = []
            if missing:
                details.append(f"missing {', '.join(missing)}")
            if unexpected:
                details.append(f"unexpected {', '.join(unexpected)}")
            raise BaselineError(
                f"profile result set is incomplete ({'; '.join(details)})"
            )
        current = load_dark_counts(results_dir, profile)
        metadata = get_run_metadata(results_dir)
        metadata["machine"] = "" if args.machine is None else args.machine
        architecture = machine_architecture()
        canonical_path = snapshot_path(benchmarks_dir, args.profile, architecture)
        validate_history_log(benchmarks_dir / HISTORY_FILE)
        validate_new_run_identity(
            benchmarks_dir / HISTORY_FILE, metadata["run_identity"]
        )

        # A requested audited Rust refresh is independent of the Dark decision,
        # but only a complete successful reference run may reach this recorder.
        if args.refresh_baseline:
            validate_rust_refresh(json_results, profile)
            update_baselines_file(benchmarks_dir, json_results)

        compiler = CompilerAttribution(
            metadata["commit_hash"], metadata["commit_message"]
        )
        if args.reset_dark_baseline:
            new_snapshot = create_snapshot(
                benchmarks_dir,
                args.profile,
                architecture,
                current,
                metadata["timestamp"],
                compiler,
            )
            write_snapshot(canonical_path, new_snapshot)
            decision = "reset"
            ratio = 1.0
            action = "reset"
            decision_document = {
                "schema_version": 1,
                "suite": "dark-compiler",
                "profile": args.profile,
                "architecture": architecture,
                "baseline": {
                    "profile": args.profile,
                    "commit": compiler.commit,
                    "contract_sha256": new_snapshot.contract_sha256,
                    "measurement_policy": new_snapshot.measurement_policy,
                },
                "decision": decision,
                "current_baseline_ratio": 1.0,
                "snapshot_action": action,
                "benchmarks": [],
            }
            active_snapshot = new_snapshot
            print(f"Dark routine baseline reset atomically: {canonical_path}")
        else:
            old_snapshot = load_snapshot(
                canonical_path, benchmarks_dir, args.profile, architecture
            )
            comparison = compare_suites(current, old_snapshot.benchmarks)
            print_comparison(comparison, old_snapshot)
            decision = comparison.decision
            ratio = comparison.ratio
            if decision == "improved":
                active_snapshot = create_snapshot(
                    benchmarks_dir,
                    args.profile,
                    architecture,
                    current,
                    metadata["timestamp"],
                    compiler,
                )
                write_snapshot(canonical_path, active_snapshot)
                action = "advanced"
            elif decision == "equal":
                active_snapshot = old_snapshot
                action = "unchanged-equal"
            else:
                active_snapshot = old_snapshot
                action = "preserved-stronger-baseline"
            decision_document = comparison_dict(
                comparison, args.profile, old_snapshot, action
            )
            print(f"Dark routine snapshot: {action}")

        baselines = load_baselines_file(benchmarks_dir)
        if decision in {"improved", "reset"} or args.refresh_baseline:
            update_results_file(benchmarks_dir, active_snapshot, baselines)
        append_to_history(
            benchmarks_dir,
            json_results,
            metadata,
            decision,
            ratio,
            args.profile,
            architecture,
            profile,
        )
        atomic_write_json(results_dir / "dark_suite_decision.json", decision_document)
        return 1 if decision == "regressed" else 0
    except (BaselineError, OSError, ValueError, json.JSONDecodeError) as error:
        print(f"Dark routine recording failed: {error}")
        print(
            "No baseline repair was attempted. Contract incompatibility requires "
            "a complete successful --reset-dark-baseline run."
        )
        return 1


if __name__ == "__main__":
    sys.exit(main())
