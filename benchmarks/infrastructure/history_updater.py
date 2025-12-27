#!/usr/bin/env python3
"""
Append benchmark results to the persistent history log.
Usage: python3 history_updater.py <results_dir>
"""

import json
import sys
from datetime import datetime
from pathlib import Path

HISTORY_FILE = "HISTORY.md"

DEFAULT_HEADER = """# Benchmark History

Performance history of Darklang compiler across versions (instruction counts via Cachegrind).

"""


def format_number(n: int) -> str:
    """Format large numbers with commas."""
    return f"{n:,}"


def format_ratio(value: float) -> str:
    """Format ratio for display."""
    if value == 1.0:
        return "baseline"
    else:
        return f"{value:.2f}x"


def load_results(results_dir: Path) -> dict:
    """Load all cachegrind JSON results."""
    results = {}
    for json_file in results_dir.glob("*_cachegrind.json"):
        benchmark_name = json_file.stem.replace("_cachegrind", "")
        with open(json_file) as f:
            data = json.load(f)
            results[benchmark_name] = data.get("results", [])
    return results


def get_run_metadata(results_dir: Path) -> dict:
    """Extract timestamp and commit info from results directory."""
    # Parse timestamp from directory name (format: YYYY-MM-DD_HHMMSS)
    dir_name = results_dir.name
    try:
        dt = datetime.strptime(dir_name, "%Y-%m-%d_%H%M%S")
        timestamp = dt.strftime("%Y-%m-%d %H:%M:%S")
    except ValueError:
        timestamp = dir_name

    # Read commit info
    version_file = results_dir / "compiler_version.txt"
    commit_hash = "unknown"
    commit_message = ""
    if version_file.exists():
        lines = version_file.read_text().strip().split("\n")
        commit_hash = lines[0][:8] if lines else "unknown"
        commit_message = lines[1] if len(lines) > 1 else ""

    return {
        "timestamp": timestamp,
        "commit_hash": commit_hash,
        "commit_message": commit_message,
    }


def generate_table(benchmark_results: list, baseline_instrs: int) -> list[str]:
    """Generate aligned markdown table for benchmark results."""
    # Headers
    headers = ["Language", "Instructions", "vs Rust", "Data Refs", "L1 Miss", "LL Miss", "Branches", "Mispred"]

    # Build rows data
    rows = []
    for r in benchmark_results:
        lang = r.get("language", "unknown").capitalize()
        instrs = r.get("instructions", 0)
        data_refs = r.get("data_refs", 0)
        d1_misses = r.get("d1_misses", 0)
        ll_misses = r.get("ll_misses", 0)
        branches = r.get("branches", 0)
        mispreds = r.get("branch_mispredicts", 0)

        ratio = instrs / baseline_instrs if baseline_instrs > 0 else 0
        mispred_rate = (mispreds / branches * 100) if branches > 0 else 0

        rows.append([
            lang,
            format_number(instrs),
            format_ratio(ratio),
            format_number(data_refs),
            format_number(d1_misses),
            format_number(ll_misses),
            format_number(branches),
            f"{mispred_rate:.1f}%",
        ])

    # Calculate column widths
    widths = [len(h) for h in headers]
    for row in rows:
        for i, cell in enumerate(row):
            widths[i] = max(widths[i], len(cell))

    # Format header
    header_line = "|"
    separator_line = "|"
    for i, h in enumerate(headers):
        header_line += f" {h:<{widths[i]}} |"
        separator_line += "-" * (widths[i] + 2) + "|"

    # Format rows
    table_lines = [header_line, separator_line]
    for row in rows:
        row_line = "|"
        for i, cell in enumerate(row):
            row_line += f" {cell:>{widths[i]}} |"
        table_lines.append(row_line)

    return table_lines


def generate_entry(results: dict, metadata: dict) -> str:
    """Generate markdown entry for this benchmark run."""
    lines = [
        f"## {metadata['timestamp']}",
        "",
        f"**Commit:** `{metadata['commit_hash']}`"
        + (f" - {metadata['commit_message']}" if metadata['commit_message'] else ""),
        "",
    ]

    for benchmark_name, benchmark_results in sorted(results.items()):
        lines.append(f"### {benchmark_name}")
        lines.append("")

        if not benchmark_results:
            lines.append("No results available.")
            lines.append("")
            continue

        # Sort by instruction count
        sorted_results = sorted(benchmark_results, key=lambda x: x.get("instructions", 0))

        # Find Rust baseline
        baseline = None
        for r in sorted_results:
            if r.get("language", "").lower() == "rust":
                baseline = r
                break
        if baseline is None:
            baseline = sorted_results[0]

        baseline_instrs = baseline.get("instructions", 1)

        table_lines = generate_table(sorted_results, baseline_instrs)
        lines.extend(table_lines)
        lines.append("")

    return "\n".join(lines)


def update_history(benchmarks_dir: Path, entry: str):
    """Prepend new entry to history file."""
    history_path = benchmarks_dir / HISTORY_FILE

    if history_path.exists():
        existing = history_path.read_text()
        # Split at first horizontal rule to separate header from entries
        if "\n---\n" in existing:
            parts = existing.split("\n---\n", 1)
            header = parts[0] + "\n"
            rest = parts[1] if len(parts) > 1 else ""
        else:
            header = existing.rstrip() + "\n\n"
            rest = ""
    else:
        header = DEFAULT_HEADER
        rest = ""

    # Compose new content: header + new entry + separator + existing entries
    new_content = header + "---\n\n" + entry + "---\n"
    if rest.strip():
        new_content += "\n" + rest

    history_path.write_text(new_content)
    print(f"History updated: {history_path}")


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 history_updater.py <results_dir>")
        sys.exit(1)

    results_dir = Path(sys.argv[1])
    if not results_dir.exists():
        print(f"Error: Results directory not found: {results_dir}")
        sys.exit(1)

    # Determine benchmarks directory (parent of results/)
    benchmarks_dir = results_dir.parent.parent

    results = load_results(results_dir)
    if not results:
        print("No cachegrind results found, skipping history update.")
        sys.exit(0)

    metadata = get_run_metadata(results_dir)
    entry = generate_entry(results, metadata)
    update_history(benchmarks_dir, entry)


if __name__ == "__main__":
    main()
