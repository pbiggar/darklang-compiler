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

Performance history of Darklang compiler across versions.

"""


def format_time(seconds: float) -> str:
    """Format time in human-readable format."""
    if seconds < 0.001:
        return f"{seconds * 1_000_000:.1f} us"
    elif seconds < 1:
        return f"{seconds * 1000:.1f} ms"
    else:
        return f"{seconds:.2f} s"


def load_results(results_dir: Path) -> dict:
    """Load all hyperfine JSON results."""
    results = {}
    for json_file in results_dir.glob("*_hyperfine.json"):
        benchmark_name = json_file.stem.replace("_hyperfine", "")
        with open(json_file) as f:
            results[benchmark_name] = json.load(f)
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


def generate_entry(results: dict, metadata: dict) -> str:
    """Generate markdown entry for this benchmark run."""
    lines = [
        f"## {metadata['timestamp']}",
        "",
        f"**Commit:** `{metadata['commit_hash']}`"
        + (f" - {metadata['commit_message']}" if metadata['commit_message'] else ""),
        "",
    ]

    for benchmark_name, benchmark_result in sorted(results.items()):
        lines.append(f"### {benchmark_name}")
        lines.append("")

        times = []
        for result in benchmark_result.get("results", []):
            times.append({
                "name": result["command"],
                "mean": result["mean"],
                "stddev": result["stddev"],
                "min": result["min"],
                "max": result["max"],
                "median": result["median"],
            })
        times = sorted(times, key=lambda x: x["mean"])

        if not times:
            lines.append("No results available.")
            lines.append("")
            continue

        # Find Rust baseline
        baseline = None
        for t in times:
            if "Rust" in t["name"] or "rust" in t["name"]:
                baseline = t
                break
        if baseline is None:
            baseline = times[0]

        lines.append("| Language | Mean | Stddev | Min | Max | Median | vs Rust |")
        lines.append("|----------|------|--------|-----|-----|--------|---------|")

        for t in times:
            name = t["name"]
            if "Dark" in name or "dark" in name:
                lang = "Dark"
            elif "Rust" in name or "rust" in name:
                lang = "Rust"
            elif "Python" in name or "python" in name:
                lang = "Python"
            else:
                lang = name

            ratio = t["mean"] / baseline["mean"]
            if ratio == 1.0:
                ratio_str = "baseline"
            else:
                ratio_str = f"{ratio:.1f}x"

            lines.append(
                f"| {lang} | {format_time(t['mean'])} | "
                f"{format_time(t['stddev'])} | {format_time(t['min'])} | "
                f"{format_time(t['max'])} | {format_time(t['median'])} | "
                f"{ratio_str} |"
            )

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
        print("No benchmark results found, skipping history update.")
        sys.exit(0)

    metadata = get_run_metadata(results_dir)
    entry = generate_entry(results, metadata)
    update_history(benchmarks_dir, entry)


if __name__ == "__main__":
    main()
