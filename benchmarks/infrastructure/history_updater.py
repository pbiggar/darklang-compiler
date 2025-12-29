#!/usr/bin/env python3
"""
Update benchmark result files after a benchmark run.

Manages three files:
- RESULTS.md: Quick overview of latest Dark results vs other languages
- BASELINES.md: Detailed baseline metrics for reference languages (no Dark)
- HISTORY.md: Append-only log of all Dark benchmark runs

Usage: python3 history_updater.py <results_dir> [--refresh-baseline]
"""

import json
import os
import re
import sys
from datetime import datetime
from pathlib import Path

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
    dir_name = results_dir.name
    try:
        dt = datetime.strptime(dir_name, "%Y-%m-%d_%H%M%S")
        timestamp = dt.strftime("%Y-%m-%d %H:%M:%S")
        date_only = dt.strftime("%Y-%m-%d")
    except ValueError:
        timestamp = dir_name
        date_only = dir_name

    version_file = results_dir / "compiler_version.txt"
    commit_hash = "unknown"
    commit_message = ""
    if version_file.exists():
        lines = version_file.read_text().strip().split("\n")
        commit_hash = lines[0][:8] if lines else "unknown"
        commit_message = lines[1] if len(lines) > 1 else ""

    return {
        "timestamp": timestamp,
        "date": date_only,
        "commit_hash": commit_hash,
        "commit_message": commit_message,
    }


# ============================================================================
# RESULTS.md - Quick overview table
# ============================================================================

def load_results_file(benchmarks_dir: Path) -> dict:
    """Load existing results from RESULTS.md.

    Returns: {benchmark: {language: instructions}}
    """
    results_path = benchmarks_dir / RESULTS_FILE
    if not results_path.exists():
        return {}

    results = {}
    content = results_path.read_text()

    in_table = False
    for line in content.split("\n"):
        if line.startswith("| Benchmark"):
            in_table = True
            # Parse header to get language columns (strip speedup suffix like " (5.30x)")
            cols = [c.strip() for c in line.split("|")]
            languages = []
            for col in cols[2:]:  # Skip empty and Benchmark columns
                lang = col.split(" (")[0] if " (" in col else col
                languages.append(lang)
            continue
        if line.startswith("|---"):
            continue
        if in_table and line.startswith("|"):
            cols = [c.strip() for c in line.split("|")]
            if len(cols) >= 3:
                benchmark = cols[1].strip()
                if not benchmark:  # Skip empty rows (like averages row)
                    continue
                results[benchmark] = {}
                for i, lang in enumerate(languages):
                    if i + 2 < len(cols) and cols[i + 2]:
                        val = cols[i + 2].replace(",", "").strip()
                        # Strip inline speedup suffix like " (4.14x)"
                        if " (" in val:
                            val = val.split(" (")[0]
                        if val and val != "-" and val != "(timeout)":
                            try:
                                results[benchmark][lang.lower()] = int(val)
                            except ValueError:
                                pass

    return results


def update_results_file(benchmarks_dir: Path, json_results: dict, baselines: dict, metadata: dict):
    """Update RESULTS.md with latest results."""
    results_path = benchmarks_dir / RESULTS_FILE

    # Load existing results
    existing = load_results_file(benchmarks_dir)

    # Merge with new results
    for benchmark, benchmark_results in json_results.items():
        if benchmark not in existing:
            existing[benchmark] = {}
        for r in benchmark_results:
            lang = r.get("language", "").lower()
            instrs = r.get("instructions", 0)
            if instrs > 0:
                existing[benchmark][lang] = instrs

    # Merge with baselines
    for benchmark, baseline_list in baselines.items():
        if benchmark not in existing:
            existing[benchmark] = {}
        for b in baseline_list:
            lang = b.get("language", "").lower()
            instrs = b.get("instructions", 0)
            if instrs > 0 and lang != "dark":
                existing[benchmark][lang] = instrs

    # Languages (rust is baseline, no speedup for it)
    # Note: F# removed - .NET doesn't work with valgrind/cachegrind
    # Note: Bun removed - JIT code not properly instrumented by valgrind
    languages = ["dark", "rust", "go", "ocaml", "python", "node"]
    langs_with_speedup = ["dark", "go", "ocaml", "python", "node"]  # Rust is baseline

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
        "Latest Dark compiler performance vs other languages (instruction counts).",
        "",
        f"**Last Updated:** {metadata['timestamp']}",
        f"**Commit:** `{metadata['commit_hash']}`" + (f" - {metadata['commit_message']}" if metadata['commit_message'] else ""),
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
    for benchmark in sorted(existing.keys()):
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
    results_path.write_text("\n".join(lines))
    print(f"Results updated: {results_path}")


# ============================================================================
# BASELINES.md - Detailed baselines for reference languages (no Dark)
# ============================================================================

def load_baselines_file(benchmarks_dir: Path) -> dict:
    """Load existing baselines from BASELINES.md.

    Returns: {benchmark: [{language, instructions, data_refs, ...}]}
    """
    baselines_path = benchmarks_dir / BASELINES_FILE
    if not baselines_path.exists():
        return {}

    baselines = {}
    content = baselines_path.read_text()

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
                if lang == "dark":
                    continue  # Skip Dark in baselines
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
    """Update BASELINES.md with new baseline results (all languages except Dark)."""
    baselines_path = benchmarks_dir / BASELINES_FILE

    # Load existing baselines
    existing = load_baselines_file(benchmarks_dir)

    # Merge with new results (excluding Dark)
    for benchmark, benchmark_results in json_results.items():
        if benchmark not in existing:
            existing[benchmark] = []
        for r in benchmark_results:
            lang = r.get("language", "").lower()
            if lang == "dark":
                continue  # Skip Dark in baselines
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
        "Reference metrics for all languages except Dark.",
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
    baselines_path.write_text("\n".join(lines))
    print(f"Baselines updated: {baselines_path}")


# ============================================================================
# HISTORY.md - Append-only Dark results log
# ============================================================================

HISTORY_HEADER = """# Benchmark History

Dark compiler performance over time (append-only log).

| Date       | Commit   | Benchmark     | Instructions     | Data Refs        | L1 Miss     | LL Miss     | Branches        | Mispred |
|------------|----------|---------------|------------------|------------------|-------------|-------------|-----------------|---------|
"""


def append_to_history(benchmarks_dir: Path, json_results: dict, metadata: dict):
    """Append Dark results to HISTORY.md."""
    history_path = benchmarks_dir / HISTORY_FILE

    # Generate new rows for Dark results only
    new_rows = []
    for benchmark, benchmark_results in sorted(json_results.items()):
        for r in benchmark_results:
            lang = r.get("language", "").lower()
            if lang != "dark":
                continue  # Only log Dark results

            instrs = format_number(r.get("instructions", 0))
            data_refs = format_number(r.get("data_refs", 0))
            d1_misses = format_number(r.get("d1_misses", 0))
            ll_misses = format_number(r.get("ll_misses", 0))
            branches = format_number(r.get("branches", 0))
            branch_count = r.get("branches", 0)
            mispreds = r.get("branch_mispredicts", 0)
            mispred_rate = (mispreds / branch_count * 100) if branch_count > 0 else 0

            new_rows.append(
                f"| {metadata['date']} | {metadata['commit_hash']} | {benchmark:<13} | {instrs:>16} | {data_refs:>16} | {d1_misses:>11} | {ll_misses:>11} | {branches:>15} | {mispred_rate:>6.1f}% |"
            )

    if not new_rows:
        return

    # Read existing or create new
    if history_path.exists():
        content = history_path.read_text()
        # Find end of header (after the separator line)
        lines = content.split("\n")
        header_end = 0
        for i, line in enumerate(lines):
            if line.startswith("|---"):
                header_end = i + 1
                break

        header = "\n".join(lines[:header_end])
        existing_rows = "\n".join(lines[header_end:])
    else:
        header = HISTORY_HEADER.rstrip()
        existing_rows = ""

    # Prepend new rows
    new_content = header + "\n" + "\n".join(new_rows)
    if existing_rows.strip():
        new_content += "\n" + existing_rows

    history_path.write_text(new_content)
    print(f"History updated: {history_path}")


# ============================================================================
# Main entry point
# ============================================================================

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 history_updater.py <results_dir> [--refresh-baseline]")
        sys.exit(1)

    results_dir = Path(sys.argv[1])
    refresh_baseline = "--refresh-baseline" in sys.argv or os.environ.get("REFRESH_BASELINE") == "true"

    if not results_dir.exists():
        print(f"Error: Results directory not found: {results_dir}")
        sys.exit(1)

    # Determine benchmarks directory (parent of results/)
    benchmarks_dir = results_dir.parent.parent

    json_results = load_json_results(results_dir)
    if not json_results:
        print("No cachegrind results found, skipping updates.")
        sys.exit(0)

    metadata = get_run_metadata(results_dir)

    # Load existing baselines
    baselines = load_baselines_file(benchmarks_dir)

    # Always update RESULTS.md with latest Dark + cached baselines
    update_results_file(benchmarks_dir, json_results, baselines, metadata)

    # Always append Dark results to HISTORY.md
    append_to_history(benchmarks_dir, json_results, metadata)

    # Only update BASELINES.md when refreshing baselines
    if refresh_baseline:
        update_baselines_file(benchmarks_dir, json_results)


if __name__ == "__main__":
    main()
