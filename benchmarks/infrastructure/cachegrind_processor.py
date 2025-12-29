#!/usr/bin/env python3
"""
Process cachegrind benchmark results and generate summary reports.
Usage: python3 cachegrind_processor.py <results_dir> [--use-baseline]

When --use-baseline is passed, reads Rust/Python baselines from HISTORY.md
instead of requiring them in the results directory.
"""

import json
import re
import sys
from pathlib import Path


def parse_history_baselines(history_path: Path) -> dict:
    """Parse HISTORY.md to extract the most recent Rust/Python baselines per benchmark.

    Searches through history entries to find the most recent one that has Rust/Python data.
    """
    if not history_path.exists():
        return {}

    content = history_path.read_text()

    # Split by --- to get entries (first part is header)
    parts = content.split("\n---\n")
    if len(parts) < 2:
        return {}

    baselines = {}

    # Search through entries to find ones with Rust/Python data
    for entry in parts[1:]:  # Skip header
        current_benchmark = None
        for line in entry.split("\n"):
            # Match benchmark header (### factorial, ### fib, etc.)
            header_match = re.match(r"^###\s+(\w+)", line)
            if header_match:
                current_benchmark = header_match.group(1)
                if current_benchmark not in baselines:
                    baselines[current_benchmark] = []
                continue

            if current_benchmark and line.startswith("|"):
                # Parse table row
                cols = [p.strip() for p in line.split("|")]
                if len(cols) >= 9 and cols[1].lower() in ("rust", "python", "node"):
                    lang = cols[1].lower()
                    # Skip if we already have this language for this benchmark
                    if any(b["language"] == lang for b in baselines.get(current_benchmark, [])):
                        continue
                    try:
                        instrs = int(cols[2].replace(",", ""))
                        data_refs = int(cols[4].replace(",", ""))
                        d1_misses = int(cols[5].replace(",", ""))
                        ll_misses = int(cols[6].replace(",", ""))
                        branches = int(cols[7].replace(",", ""))
                        mispred_pct = float(cols[8].replace("%", ""))
                        mispreds = int(branches * mispred_pct / 100)

                        baselines[current_benchmark].append({
                            "language": lang,
                            "instructions": instrs,
                            "data_refs": data_refs,
                            "d1_misses": d1_misses,
                            "ll_misses": ll_misses,
                            "branches": branches,
                            "branch_mispredicts": mispreds,
                        })
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


def update_history(results: dict, output_dir: Path, benchmarks_dir: Path):
    """Append cachegrind results to HISTORY.md."""
    from datetime import datetime

    history_path = benchmarks_dir / "HISTORY.md"
    if not history_path.exists():
        return  # Let the timing history_updater create it first

    # Get metadata
    dir_name = output_dir.name
    try:
        dt = datetime.strptime(dir_name, "%Y-%m-%d_%H%M%S")
        timestamp = dt.strftime("%Y-%m-%d %H:%M:%S")
    except ValueError:
        timestamp = dir_name

    version_file = output_dir / "compiler_version.txt"
    commit_hash = "unknown"
    if version_file.exists():
        lines_v = version_file.read_text().strip().split("\n")
        commit_hash = lines_v[0][:8] if lines_v else "unknown"

    # Generate cachegrind section
    lines = [
        "",
        "#### Instruction Counts (Cachegrind)",
        "",
    ]

    for benchmark_name, benchmark_results in sorted(results.items()):
        if not benchmark_results:
            continue

        sorted_results = sorted(benchmark_results, key=lambda x: x.get("instructions", 0))

        # Find Dark baseline
        baseline = None
        for r in sorted_results:
            if r.get("language", "").lower() == "dark":
                baseline = r
                break
        if baseline is None:
            baseline = sorted_results[0]

        baseline_instrs = baseline.get("instructions", 1)

        lines.append(f"**{benchmark_name}:**")
        for r in sorted_results:
            lang = r.get("language", "unknown").capitalize()
            instrs = r.get("instructions", 0)
            ratio = instrs / baseline_instrs if baseline_instrs > 0 else 0
            ratio_str = format_ratio(ratio)
            lines.append(f"- {lang}: {format_number(instrs)} ({ratio_str})")
        lines.append("")

    # Read existing history and find the most recent entry to append to
    existing = history_path.read_text()

    # Find the first "---" after the header and insert after it
    # The format is: header, ---, entry, ---, entry, ...
    # We want to add cachegrind data to the most recent entry
    parts = existing.split("\n---\n")
    if len(parts) >= 2:
        # Insert cachegrind section into the first entry (most recent)
        header = parts[0]
        first_entry = parts[1]
        rest = parts[2:] if len(parts) > 2 else []

        # Append cachegrind lines before the end of the first entry
        new_first_entry = first_entry.rstrip() + "\n".join(lines)

        # Reconstruct
        new_parts = [header, new_first_entry] + rest
        new_content = "\n---\n".join(new_parts)

        history_path.write_text(new_content)
        print(f"Cachegrind results appended to: {history_path}")


def merge_with_baselines(results: dict, baselines: dict) -> dict:
    """Merge fresh Dark results with cached Rust/Python baselines."""
    merged = {}
    for benchmark, dark_results in results.items():
        merged[benchmark] = list(dark_results)  # Copy Dark results
        if benchmark in baselines:
            # Add Rust/Python from baselines
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

    # If using baseline, merge with cached Rust/Python from history
    if use_baseline:
        history_path = benchmarks_dir / "HISTORY.md"
        baselines = parse_history_baselines(history_path)
        if baselines:
            print(f"  Using cached baselines from HISTORY.md")
            results = merge_with_baselines(results, baselines)

    generate_summary(results, results_dir)
    update_history(results, results_dir, benchmarks_dir)


if __name__ == "__main__":
    main()
