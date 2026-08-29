#!/usr/bin/env python3
"""Enforce human-audited full and quick benchmark parity contracts."""

import hashlib
import json
import sys
from pathlib import Path

from benchmark_profiles import load_profile


ALLOWED_STATUSES = {"comparable", "incomparable", "reduced", "dark-only"}
ALLOWED_QUICK_STATUSES = {"comparable", "incomparable"}


def source_hash(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def source_tree_hash(path: Path) -> str:
    """Hash a Cargo benchmark's complete audited source tree deterministically."""
    digest = hashlib.sha256()
    files = sorted(
        candidate
        for candidate in path.rglob("*")
        if candidate.is_file()
        and "target" not in candidate.relative_to(path).parts
        and candidate.name not in {"main", "quick"}
    )
    for candidate in files:
        relative = candidate.relative_to(path).as_posix().encode()
        digest.update(relative)
        digest.update(b"\0")
        digest.update(candidate.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def load_contract(benchmarks_dir: Path) -> dict[str, dict[str, object]]:
    contract_path = benchmarks_dir / "PARITY.json"
    data = json.loads(contract_path.read_text())
    if data.get("schema") != 2 or not isinstance(data.get("benchmarks"), dict):
        raise ValueError("PARITY.json must use schema 2 and contain a benchmarks object")
    return data["benchmarks"]


def validate_entry(
    benchmarks_dir: Path, benchmark: str, entry: dict[str, object]
) -> list[str]:
    failures: list[str] = []
    status = entry.get("status")
    if status not in ALLOWED_STATUSES:
        return [f"{benchmark}: invalid parity status {status!r}"]

    problem_dir = benchmarks_dir / "problems" / benchmark
    dark_source = problem_dir / "dark" / "main.dark"
    rust_source = problem_dir / "rust" / "main.rs"

    if not dark_source.is_file():
        failures.append(f"{benchmark}: missing Dark source")
    if status != "dark-only" and not rust_source.is_file():
        failures.append(f"{benchmark}: missing Rust source")
    if status != "comparable" and not entry.get("reason"):
        failures.append(f"{benchmark}: {status} status requires a reason")

    sources = (("dark", dark_source), ("rust", rust_source))
    for language, path in sources:
        if not path.is_file():
            continue
        expected_hash = entry.get(f"{language}_sha256")
        actual_hash = source_hash(path)
        if expected_hash != actual_hash:
            failures.append(
                f"{benchmark}: {language} source changed since its parity audit "
                f"(expected {expected_hash or 'no hash'}, got {actual_hash})"
            )

    expected_tree_hash = entry.get("rust_tree_sha256")
    if expected_tree_hash is not None:
        actual_tree_hash = source_tree_hash(problem_dir / "rust")
        if expected_tree_hash != actual_tree_hash:
            failures.append(
                f"{benchmark}: Rust source tree changed since its parity audit "
                f"(expected {expected_tree_hash}, got {actual_tree_hash})"
            )

    shared_expected = problem_dir / "expected_output.txt"
    if not shared_expected.is_file():
        failures.append(f"{benchmark}: full pair has no shared expected output")
    else:
        expected_hash = entry.get("expected_sha256")
        actual_hash = source_hash(shared_expected)
        if expected_hash != actual_hash:
            failures.append(
                f"{benchmark}: full expected output changed since its parity audit "
                f"(expected {expected_hash or 'no hash'}, got {actual_hash})"
            )

    dark_expected = problem_dir / "dark" / "expected_output.txt"
    if dark_expected.is_file():
        expected_hash = entry.get("dark_expected_sha256")
        actual_hash = source_hash(dark_expected)
        if expected_hash != actual_hash:
            failures.append(
                f"{benchmark}: Dark full expected output changed since its parity audit "
                f"(expected {expected_hash or 'no hash'}, got {actual_hash})"
            )

    if status == "comparable":
        if shared_expected.is_file() and dark_expected.is_file() and (
            dark_expected.read_text().strip() != shared_expected.read_text().strip()
        ):
            failures.append(
                f"{benchmark}: comparable pair has a different Dark expected output"
            )

    quick = entry.get("quick")
    if not isinstance(quick, dict):
        failures.append(f"{benchmark}: missing quick parity contract")
        return failures

    quick_status = quick.get("status")
    if quick_status not in ALLOWED_QUICK_STATUSES:
        failures.append(f"{benchmark}: invalid quick parity status {quick_status!r}")
    if quick_status != "comparable" and not quick.get("reason"):
        failures.append(f"{benchmark}: incomparable quick status requires a reason")

    quick_sources = (
        ("dark", problem_dir / "dark" / "quick.dark"),
        ("rust", problem_dir / "rust" / "quick.rs"),
    )
    for language, path in quick_sources:
        if not path.is_file():
            failures.append(f"{benchmark}: missing {language} quick source")
            continue
        expected_hash = quick.get(f"{language}_sha256")
        actual_hash = source_hash(path)
        if expected_hash != actual_hash:
            failures.append(
                f"{benchmark}: {language} quick source changed since its parity audit "
                f"(expected {expected_hash or 'no hash'}, got {actual_hash})"
            )

    expected_quick_tree_hash = quick.get("rust_tree_sha256")
    if expected_quick_tree_hash is not None:
        actual_quick_tree_hash = source_tree_hash(problem_dir / "rust")
        if expected_quick_tree_hash != actual_quick_tree_hash:
            failures.append(
                f"{benchmark}: Rust quick source tree changed since its parity audit "
                f"(expected {expected_quick_tree_hash}, got {actual_quick_tree_hash})"
            )

    quick_expected = problem_dir / "quick_expected_output.txt"
    if not quick_expected.is_file():
        failures.append(f"{benchmark}: quick pair has no shared expected output")
    else:
        expected_hash = quick.get("expected_sha256")
        actual_hash = source_hash(quick_expected)
        if expected_hash != actual_hash:
            failures.append(
                f"{benchmark}: quick expected output changed since its parity audit "
                f"(expected {expected_hash or 'no hash'}, got {actual_hash})"
            )

    return failures


def main() -> int:
    valid_invocation = (
        (len(sys.argv) == 2 and sys.argv[1] == "check")
        or (len(sys.argv) == 3 and sys.argv[1] in {"check-profile", "status"})
        or (len(sys.argv) == 4 and sys.argv[1] == "status" and sys.argv[3] == "quick")
    )
    if not valid_invocation:
        print(
            "Usage: python3 benchmark_parity.py "
            "check | check-profile <profile> | status <benchmark> [quick]"
        )
        return 1

    benchmarks_dir = Path(__file__).resolve().parent.parent
    try:
        contract = load_contract(benchmarks_dir)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(f"Benchmark parity check failed: {error}")
        return 1

    if sys.argv[1] == "status":
        benchmark = sys.argv[2]
        entry = contract.get(benchmark)
        if entry is None:
            print(f"Error: no parity contract for {benchmark}")
            return 1
        selected_entry = entry.get("quick") if len(sys.argv) == 4 else entry
        if not isinstance(selected_entry, dict):
            print(f"Error: no quick parity contract for {benchmark}")
            return 1
        print(selected_entry.get("status", "invalid"))
        return 0

    problem_names = {
        path.name for path in (benchmarks_dir / "problems").iterdir() if path.is_dir()
    }
    failures = [
        f"{benchmark}: missing parity contract"
        for benchmark in sorted(problem_names - set(contract))
    ]
    failures.extend(
        f"{benchmark}: parity contract has no benchmark directory"
        for benchmark in sorted(set(contract) - problem_names)
    )

    selected = sorted(problem_names)
    if sys.argv[1] == "check-profile":
        try:
            selected = load_profile(benchmarks_dir, sys.argv[2])
        except ValueError as error:
            failures.append(str(error))
            selected = []
        if sys.argv[2] not in {"quick", "quick-fast"}:
            for benchmark in selected:
                status = contract.get(benchmark, {}).get("status")
                if status != "comparable":
                    failures.append(
                        f"{benchmark}: profile requires comparable status, got {status or 'missing'}"
                    )

    for benchmark in selected:
        entry = contract.get(benchmark)
        if entry is not None:
            failures.extend(validate_entry(benchmarks_dir, benchmark, entry))

    if failures:
        print("Benchmark parity check failed:")
        for failure in failures:
            print(f"  {failure}")
        return 1

    print(f"Verified parity contracts for {len(selected)} benchmark(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
