#!/usr/bin/env python3
"""
Process hyperfine benchmark results and generate summary reports.
Usage: python3 result_processor.py <results_dir>
"""

import json
import os
import sys
from pathlib import Path


def load_results(results_dir: Path) -> dict:
    """Load all hyperfine JSON results from the results directory."""
    results = {}
    for json_file in results_dir.glob("*_hyperfine.json"):
        benchmark_name = json_file.stem.replace("_hyperfine", "")
        with open(json_file) as f:
            results[benchmark_name] = json.load(f)
    return results


def extract_times(benchmark_result: dict) -> list[dict]:
    """Extract timing information from a benchmark result."""
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
    return sorted(times, key=lambda x: x["mean"])


def format_time(seconds: float) -> str:
    """Format time in human-readable format."""
    if seconds < 0.001:
        return f"{seconds * 1_000_000:.1f} us"
    elif seconds < 1:
        return f"{seconds * 1000:.1f} ms"
    else:
        return f"{seconds:.2f} s"


def generate_summary(results: dict, output_dir: Path):
    """Generate a markdown summary of all benchmark results."""
    lines = [
        "# Benchmark Results",
        "",
        f"Generated: {output_dir.name}",
        "",
    ]

    # Read compiler version if available
    version_file = output_dir / "compiler_version.txt"
    if version_file.exists():
        version_info = version_file.read_text().strip().split("\n")
        lines.append(f"Compiler commit: `{version_info[0][:8]}`")
        if len(version_info) > 1:
            lines.append(f"Commit message: {version_info[1]}")
        lines.append("")

    for benchmark_name, benchmark_result in sorted(results.items()):
        lines.append(f"## {benchmark_name}")
        lines.append("")

        times = extract_times(benchmark_result)
        if not times:
            lines.append("No results available.")
            lines.append("")
            continue

        # Find baseline (Rust if available, otherwise fastest)
        baseline = None
        for t in times:
            if "Rust" in t["name"] or "rust" in t["name"]:
                baseline = t
                break
        if baseline is None:
            baseline = times[0]

        lines.append("| Language | Mean | Stddev | vs Baseline |")
        lines.append("|----------|------|--------|-------------|")

        for t in times:
            name = t["name"]
            # Clean up command to just language name
            if "Dark" in name or "dark" in name:
                lang = "Dark"
            elif "Rust" in name or "rust" in name:
                lang = "Rust"
            elif "Python" in name or "python" in name:
                lang = "Python"
            else:
                lang = name

            ratio = t["mean"] / baseline["mean"]
            if ratio < 1:
                ratio_str = f"{ratio:.2f}x (faster)"
            elif ratio > 1:
                ratio_str = f"{ratio:.1f}x slower"
            else:
                ratio_str = "baseline"

            lines.append(
                f"| {lang} | {format_time(t['mean'])} | "
                f"+/- {format_time(t['stddev'])} | {ratio_str} |"
            )

        lines.append("")

    # Write summary
    summary_path = output_dir / "summary.md"
    summary_path.write_text("\n".join(lines))
    print(f"Summary written to: {summary_path}")

    # Also print to stdout
    print("")
    print("=" * 60)
    for line in lines:
        print(line)


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 result_processor.py <results_dir>")
        sys.exit(1)

    results_dir = Path(sys.argv[1])
    if not results_dir.exists():
        print(f"Error: Results directory not found: {results_dir}")
        sys.exit(1)

    results = load_results(results_dir)
    if not results:
        print("No benchmark results found.")
        sys.exit(0)

    generate_summary(results, results_dir)


if __name__ == "__main__":
    main()
