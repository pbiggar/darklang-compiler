#!/usr/bin/env python3
"""
Process cachegrind benchmark results and generate summary reports.
Usage: python3 cachegrind_processor.py <results_dir> [--use-baseline]

When --use-baseline is passed, reads Rust/Python/Node baselines from BASELINES.md
instead of requiring them in the results directory.
"""

import json
import re
import sys
from pathlib import Path


def parse_baselines_file(baselines_path: Path) -> dict:
    """Parse BASELINES.md to extract language baselines per benchmark.

    Returns: {benchmark: [{language, instructions, data_refs, ...}]}
    """
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
                benchmark = cols[1].strip()
                lang = cols[2].strip().lower()
                try:
                    branches = int(cols[7].replace(",", ""))
                    mispred_pct = float(cols[8].replace("%", ""))
                    mispreds = int(branches * mispred_pct / 100)

                    entry = {
                        "language": lang,
                        "instructions": int(cols[3].replace(",", "")),
                        "data_refs": int(cols[4].replace(",", "")),
                        "d1_misses": int(cols[5].replace(",", "")),
                        "ll_misses": int(cols[6].replace(",", "")),
                        "branches": branches,
                        "branch_mispredicts": mispreds,
                    }
                    if benchmark not in baselines:
                        baselines[benchmark] = []
                    baselines[benchmark].append(entry)
                except (ValueError, IndexError):
                    pass

    return baselines


def format_number(n: int) -> str:
    """Format large numbers with commas."""
    return f"{n:,}"


def format_ratio(value: float) -> str:
    """Format ratio for display."""
    if value == 1.0:
        return "baseline"
    elif value < 1.0:
        return f"{value:.2f}x"
    else:
        return f"{value:.1f}x"


def load_results(results_dir: Path) -> dict:
    """Load all cachegrind JSON results from the results directory."""
    results = {}
    for json_file in results_dir.glob("*_cachegrind.json"):
        benchmark_name = json_file.stem.replace("_cachegrind", "")
        with open(json_file) as f:
            data = json.load(f)
            results[benchmark_name] = data.get("results", [])
    return results


def generate_summary(results: dict, output_dir: Path):
    """Generate a markdown summary of cachegrind results."""
    lines = [
        "# Cachegrind Results (Instruction Counts)",
        "",
        "Deterministic instruction counts via Valgrind Cachegrind.",
        "",
    ]

    # Read compiler version if available
    version_file = output_dir / "compiler_version.txt"
    if version_file.exists():
        version_info = version_file.read_text().strip().split("\n")
        lines.append(f"**Commit:** `{version_info[0][:8]}`")
        if len(version_info) > 1:
            lines.append(f"**Message:** {version_info[1]}")
        lines.append("")

    for benchmark_name, benchmark_results in sorted(results.items()):
        lines.append(f"## {benchmark_name}")
        lines.append("")

        if not benchmark_results:
            lines.append("No results available.")
            lines.append("")
            continue

        # Sort by instruction count
        sorted_results = sorted(benchmark_results, key=lambda x: x.get("instructions", 0))

        # Find baseline (Rust, or first if no Rust)
        baseline = None
        for r in sorted_results:
            if r.get("language", "").lower() == "rust":
                baseline = r
                break
        if baseline is None:
            baseline = sorted_results[0]

        baseline_instrs = baseline.get("instructions", 1)

        lines.append("| Language | Instructions | vs Rust | Data Refs | L1 Miss | LL Miss | Branches | Mispred |")
        lines.append("|----------|-------------|---------|-----------|---------|---------|----------|---------|")

        for r in sorted_results:
            lang = r.get("language", "unknown").capitalize()
            instrs = r.get("instructions", 0)
            data_refs = r.get("data_refs", 0)
            d1_misses = r.get("d1_misses", 0)
            ll_misses = r.get("ll_misses", 0)
            branches = r.get("branches", 0)
            mispreds = r.get("branch_mispredicts", 0)

            ratio = instrs / baseline_instrs if baseline_instrs > 0 else 0
            mispred_rate = (mispreds / branches * 100) if branches > 0 else 0

            lines.append(
                f"| {lang} | {format_number(instrs)} | "
                f"{format_ratio(ratio)} | {format_number(data_refs)} | "
                f"{format_number(d1_misses)} | {format_number(ll_misses)} | "
                f"{format_number(branches)} | {mispred_rate:.1f}% |"
            )

        lines.append("")

    # Write summary
    summary_path = output_dir / "cachegrind_summary.md"
    summary_path.write_text("\n".join(lines))
    print(f"Cachegrind summary written to: {summary_path}")

    # Print to stdout
    print("")
    print("=" * 60)
    for line in lines:
        print(line)


def merge_with_baselines(results: dict, baselines: dict) -> dict:
    """Merge fresh Dark results with cached Rust/Python/Node baselines."""
    merged = {}
    for benchmark, dark_results in results.items():
        merged[benchmark] = list(dark_results)  # Copy Dark results
        if benchmark in baselines:
            # Add Rust/Python/Node from baselines
            merged[benchmark].extend(baselines[benchmark])
    return merged


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 cachegrind_processor.py <results_dir> [--use-baseline]")
        sys.exit(1)

    results_dir = Path(sys.argv[1])
    use_baseline = "--use-baseline" in sys.argv

    if not results_dir.exists():
        print(f"Error: Results directory not found: {results_dir}")
        sys.exit(1)

    # Determine benchmarks directory
    benchmarks_dir = results_dir.parent.parent

    results = load_results(results_dir)
    if not results:
        print("No cachegrind results found.")
        sys.exit(0)

    # If using baseline, merge with cached Rust/Python/Node from BASELINES.md
    if use_baseline:
        baselines_path = benchmarks_dir / "BASELINES.md"
        baselines = parse_baselines_file(baselines_path)
        if baselines:
            print(f"  Using cached baselines from BASELINES.md")
            results = merge_with_baselines(results, baselines)

    generate_summary(results, results_dir)
    # Note: history_updater.py now handles updating RESULTS.md, BASELINES.md, and HISTORY.md


if __name__ == "__main__":
    main()
