#!/usr/bin/env python3
"""
Process cachegrind benchmark results and generate summary reports.
Usage: python3 cachegrind_processor.py <results_dir> [--use-baseline] [--quiet]

When --use-baseline is passed, reads the audited Rust reference snapshot
instead of requiring it in the results directory.
"""

import argparse
import json
import re
import sys
from pathlib import Path


def load_reference_counts(benchmarks_dir: Path, profile: str) -> dict:
    """Read compatible Rust counts from structured snapshots, never Markdown."""
    from benchmark_baseline import machine_architecture
    from benchmark_reports import documents
    from reference_snapshots import row_status

    track = f"{machine_architecture()}-{profile}-cachegrind"
    reference = documents(benchmarks_dir, track).get("rust")
    if reference is None:
        return {}
    return {
        row["name"]: [{"language": "rust", "instructions": row["instructions"]}]
        for row in reference["benchmarks"]
        if row_status(benchmarks_dir, reference, row, profile) == "current"
    }


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


def generate_summary(results: dict, output_dir: Path, quiet: bool = False):
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

        # Only an audited Rust row is a comparison baseline. Reduced and
        # Dark-only diagnostics intentionally have no relative ratio.
        baseline = None
        for r in sorted_results:
            if r.get("language", "").lower() == "rust":
                baseline = r
                break
        baseline_instrs = baseline.get("instructions", 1) if baseline else None

        headers = ["Language", "Instructions", "vs Rust"]
        rows = []

        for r in sorted_results:
            lang = r.get("language", "unknown").capitalize()
            instrs = r.get("instructions", 0)
            ratio = (
                instrs / baseline_instrs
                if baseline_instrs is not None and baseline_instrs > 0
                else None
            )
            rows.append(
                [
                    lang,
                    format_number(instrs),
                    format_ratio(ratio) if ratio is not None else "-",
                ]
            )

        widths = [len(h) for h in headers]
        for row in rows:
            for idx, cell in enumerate(row):
                if len(cell) > widths[idx]:
                    widths[idx] = len(cell)

        def format_row(cells):
            padded = [cell.ljust(widths[idx]) for idx, cell in enumerate(cells)]
            return "| " + " | ".join(padded) + " |"

        lines.append(format_row(headers))
        lines.append("| " + " | ".join("-" * w for w in widths) + " |")
        for row in rows:
            lines.append(format_row(row))

        lines.append("")

    # Write summary
    summary_path = output_dir / "cachegrind_summary.md"
    summary_path.write_text("\n".join(lines))
    print(f"Cachegrind summary written to: {summary_path}")

    if not quiet:
        print("")
        print("=" * 60)
        for line in lines:
            print(line)


def load_parity_statuses(benchmarks_dir: Path) -> dict[str, str]:
    """Load audited comparison status for filtering cached baselines."""
    contract = json.loads((benchmarks_dir / "PARITY.json").read_text())
    return {
        benchmark: entry.get("status", "invalid")
        for benchmark, entry in contract.get("benchmarks", {}).items()
    }


def merge_with_baselines(
    results: dict, baselines: dict, parity_statuses: dict[str, str]
) -> dict:
    """Merge fresh Dark results with cached Rust/Python/Node baselines."""
    merged = {}
    for benchmark, dark_results in results.items():
        merged[benchmark] = list(dark_results)  # Copy Dark results
        if benchmark in baselines and parity_statuses.get(benchmark) == "comparable":
            # Only Rust is covered by the source-parity contract.
            merged[benchmark].extend(
                entry
                for entry in baselines[benchmark]
                if entry.get("language", "").lower() == "rust"
            )
    return merged


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results_dir")
    parser.add_argument("--use-baseline", action="store_true")
    parser.add_argument("--profile", choices=("full", "quick"), default="full")
    parser.add_argument(
        "--quiet",
        action="store_true",
        help="write the complete report without echoing its markdown to stdout",
    )
    args = parser.parse_args()
    results_dir = Path(args.results_dir)

    if not results_dir.exists():
        print(f"Error: Results directory not found: {results_dir}")
        sys.exit(1)

    # Determine benchmarks directory
    benchmarks_dir = results_dir.parent.parent

    results = load_results(results_dir)
    if not results:
        print("No cachegrind results found.")
        sys.exit(0)

    # If using a reference, merge only compatible structured Rust rows.
    if args.use_baseline:
        baselines = load_reference_counts(benchmarks_dir, args.profile)
        if baselines:
            if not args.quiet:
                print("  Using compatible stored Rust reference counts")
            results = merge_with_baselines(
                results, baselines, load_parity_statuses(benchmarks_dir)
            )

    generate_summary(results, results_dir, quiet=args.quiet)
    # bench report owns current-state reports; this file writes run-local summaries.


if __name__ == "__main__":
    main()
