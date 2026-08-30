#!/usr/bin/env python3
"""Canonical output-validated x86_64 quick benchmark measurement under QEMU."""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import math
import re
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent / "infrastructure"))

from benchmark_baseline import (  # noqa: E402
    BenchmarkCount,
    CompilerAttribution,
    TRACKS,
    atomic_write_json,
    atomic_write_text,
    compare_suites,
    contract_digest,
    create_snapshot,
    load_snapshot,
    snapshot_names,
    snapshot_path,
    track_dict,
    write_snapshot,
)
from benchmark_profiles import load_profile  # noqa: E402


SCHEMA_VERSION = 1
TRACK = TRACKS["x86_64-quick-qemu"]
TOTAL_PATTERN = re.compile(r"^total insns: ([1-9][0-9]*)$", re.MULTILINE)


def command_result(arguments: list[str], cwd: Path, timeout: int = 60):
    return subprocess.run(
        arguments,
        cwd=cwd,
        text=True,
        capture_output=True,
        timeout=timeout,
    )


def git_value(repository: Path, arguments: list[str]) -> str:
    result = command_result(["git", *arguments], repository)
    if result.returncode != 0:
        raise ValueError(result.stderr.strip() or "git command failed")
    return result.stdout.strip()


def tool_version(repository: Path, arguments: list[str]) -> str:
    result = command_result(arguments, repository)
    if result.returncode != 0:
        raise ValueError(f"required tool is unavailable: {' '.join(arguments)}")
    lines = (result.stdout + result.stderr).strip().splitlines()
    if not lines:
        raise ValueError(f"required tool reported no version: {' '.join(arguments)}")
    return lines[0]


def toolchains(repository: Path) -> dict[str, str]:
    versions = {
        "dotnet": tool_version(repository, ["dotnet", "--version"]),
        "rustc": tool_version(repository, ["rustc", "--version"]),
        "x86_64_linker": tool_version(repository, ["x86_64-linux-gnu-gcc", "--version"]),
        "qemu": tool_version(repository, ["/opt/dcb/qemu/qemu-x86_64", "--version"]),
    }
    if not versions["rustc"].startswith("rustc 1.89.0 "):
        raise ValueError(f"expected rustc 1.89.0, found {versions['rustc']}")
    if versions["qemu"] != "qemu-x86_64 version 11.1.1":
        raise ValueError(f"expected QEMU 11.1.1, found {versions['qemu']}")
    return versions


def build_dark(repository: Path, name: str, output: Path) -> str | None:
    compiler = repository / "bin" / "DarkCompiler" / "Debug" / "net10.0" / "DarkCompiler.dll"
    source = repository / "benchmarks" / "problems" / name / "dark" / "quick.dark"
    if not compiler.is_file():
        return "compiler output is missing"
    if not source.is_file():
        return "quick Dark source is missing"
    output.parent.mkdir(parents=True, exist_ok=True)
    result = command_result(
        [
            "dotnet",
            str(compiler),
            "--allow-internal",
            "--emit-result",
            "--target=linux-x86_64",
            str(source),
            "-o",
            str(output),
            "-q",
        ],
        repository,
    )
    if result.returncode == 0:
        return None
    detail = (result.stdout + result.stderr).strip().splitlines()
    return detail[0] if detail else f"compiler exited {result.returncode}"


def build_rust(repository: Path, name: str, output: Path) -> str | None:
    rust_root = repository / "benchmarks" / "problems" / name / "rust"
    source = rust_root / "quick.rs"
    if not source.is_file():
        return "quick Rust source is missing"
    output.parent.mkdir(parents=True, exist_ok=True)
    manifest = rust_root / "Cargo.toml"
    if manifest.is_file():
        target_dir = output.parent / f"{name}-cargo"
        result = command_result(
            [
                "cargo",
                "--config=target.x86_64-unknown-linux-gnu.linker=\"x86_64-linux-gnu-gcc\"",
                "build",
                "--release",
                "--target=x86_64-unknown-linux-gnu",
                f"--target-dir={target_dir}",
                f"--manifest-path={manifest}",
                "--bin=benchmark-quick",
            ],
            repository,
            timeout=180,
        )
        built = target_dir / "x86_64-unknown-linux-gnu" / "release" / "benchmark-quick"
        if result.returncode == 0 and built.is_file():
            output.write_bytes(built.read_bytes())
            output.chmod(0o755)
            return None
    else:
        result = command_result(
            [
                "rustc",
                "-C",
                "opt-level=3",
                "--target=x86_64-unknown-linux-gnu",
                "-C",
                "linker=x86_64-linux-gnu-gcc",
                str(source),
                "-o",
                str(output),
            ],
            repository,
        )
        if result.returncode == 0:
            return None
    detail = (result.stdout + result.stderr).strip().splitlines()
    return detail[0] if detail else f"Rust compiler exited {result.returncode}"


def measure_binary(repository: Path, name: str, binary: Path) -> tuple[int | None, str | None]:
    expected_path = repository / "benchmarks" / "problems" / name / "quick_expected_output.txt"
    if not expected_path.is_file():
        return None, "quick expected output is missing"
    counter = repository / "benchmarks" / "infrastructure" / "qemu_instruction_count.sh"
    try:
        result = command_result([str(counter), str(binary)], repository, timeout=30)
    except subprocess.TimeoutExpired:
        return None, "execution exceeded 30 seconds"
    if result.returncode != 0:
        return None, f"execution exited {result.returncode}"
    if result.stdout != expected_path.read_text():
        return None, "program output did not match quick_expected_output.txt"
    matches = TOTAL_PATTERN.findall(result.stderr)
    if len(matches) != 1:
        return None, "QEMU did not report exactly one positive guest instruction count"
    return int(matches[0]), None


def measure_language(
    repository: Path, language: str, names: tuple[str, ...], output_root: Path
) -> tuple[list[dict[str, object]], list[dict[str, str]]]:
    builder = build_dark if language == "dark" else build_rust
    binaries = {name: output_root / language / name for name in names}
    with concurrent.futures.ThreadPoolExecutor(max_workers=min(4, len(names))) as executor:
        failures = dict(
            zip(
                names,
                executor.map(lambda name: builder(repository, name, binaries[name]), names),
            )
        )
    rows: list[dict[str, object]] = []
    unavailable: list[dict[str, str]] = []
    for name in names:
        if failures[name] is not None:
            unavailable.append(
                {"language": language, "name": name, "phase": "build", "reason": failures[name]}
            )
            continue
        instructions, error = measure_binary(repository, name, binaries[name])
        if error is not None:
            unavailable.append(
                {"language": language, "name": name, "phase": "execute", "reason": error}
            )
            continue
        rows.append({"name": name, "instructions": instructions})
    return rows, unavailable


def measurement(
    repository: Path, output: Path, allow_partial: bool, languages: tuple[str, ...]
) -> int:
    versions = toolchains(repository)
    dark_names = tuple(load_profile(repository / "benchmarks", "quick"))
    rust_names = snapshot_names(repository / "benchmarks", "rust", "quick")
    output_root = output.parent / f".{output.stem}-binaries"
    dark_rows, dark_unavailable = (
        measure_language(repository, "dark", dark_names, output_root)
        if "dark" in languages
        else ([], [])
    )
    rust_rows, rust_unavailable = (
        measure_language(repository, "rust", rust_names, output_root)
        if "rust" in languages
        else ([], [])
    )
    payload = {
        "schema_version": SCHEMA_VERSION,
        "suite": "dark-compiler",
        "track": track_dict(TRACK),
        "requested_languages": list(languages),
        "contract_sha256": contract_digest(repository / "benchmarks", "quick"),
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "compiler": {
            "commit": git_value(repository, ["rev-parse", "HEAD"]),
            "subject": git_value(repository, ["log", "-1", "--format=%s"]),
        },
        "toolchains": versions,
        "coverage": {
            "dark": {"measured": len(dark_rows), "total": len(dark_names)},
            "rust": {"measured": len(rust_rows), "total": len(rust_names)},
        },
        "measurements": {"dark": dark_rows, "rust": rust_rows},
        "unavailable": [*dark_unavailable, *rust_unavailable],
    }
    atomic_write_json(output, payload)
    requested_coverage = [payload["coverage"][language] for language in languages]
    complete = not payload["unavailable"] and all(
        coverage["measured"] == coverage["total"] for coverage in requested_coverage
    )
    print(
        "X86_64_MEASUREMENT "
        f"dark={len(dark_rows)}/{len(dark_names)} rust={len(rust_rows)}/{len(rust_names)} "
        f"status={'complete' if complete else 'partial'}"
    )
    for row in payload["unavailable"]:
        print(f"  {row['language']}/{row['name']}: {row['phase']}: {row['reason']}")
    return 0 if complete or allow_partial else 1


def load_measurement(path: Path) -> dict[str, object]:
    try:
        payload = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError(f"cannot read measurement {path}: {error}") from error
    if not isinstance(payload, dict) or payload.get("schema_version") != SCHEMA_VERSION:
        raise ValueError(f"measurement {path} has an incompatible schema")
    if payload.get("track") != track_dict(TRACK):
        raise ValueError(f"measurement {path} has an incompatible track")
    if not isinstance(payload.get("toolchains"), dict):
        raise ValueError(f"measurement {path} has no toolchain attribution")
    return payload


def rows(payload: dict[str, object], language: str) -> tuple[BenchmarkCount, ...]:
    measurements = payload.get("measurements")
    values = measurements.get(language) if isinstance(measurements, dict) else None
    if not isinstance(values, list):
        raise ValueError(f"measurement has no {language} rows")
    return tuple(BenchmarkCount(str(row["name"]), int(row["instructions"])) for row in values)


def compare(base_path: Path, candidate_path: Path, decision_path: Path) -> int:
    base = load_measurement(base_path)
    candidate = load_measurement(candidate_path)
    if base.get("contract_sha256") != candidate.get("contract_sha256"):
        raise ValueError("base and candidate workload contracts differ")
    if base.get("toolchains") != candidate.get("toolchains"):
        raise ValueError("base and candidate toolchains differ")
    base_dark = rows(base, "dark")
    candidate_dark = rows(candidate, "dark")
    base_by_name = {row.name: row for row in base_dark}
    candidate_by_name = {row.name: row for row in candidate_dark}
    comparable_names = tuple(name for name in base_by_name if name in candidate_by_name)
    if not comparable_names:
        raise ValueError("base and candidate have no comparable Dark measurements")
    comparison = compare_suites(
        (candidate_by_name[name] for name in comparable_names),
        (base_by_name[name] for name in comparable_names),
    )
    base_coverage = base.get("coverage")
    candidate_coverage = candidate.get("coverage")
    complete = all(
        isinstance(coverage, dict)
        and isinstance(coverage.get(language), dict)
        and coverage[language].get("measured") == coverage[language].get("total")
        for coverage in (base_coverage, candidate_coverage)
        for language in ("dark",)
    )
    rust_by_name = {row.name: row for row in rows(base, "rust")}
    rust_names = tuple(rust_by_name)
    rust_complete = (
        isinstance(base_coverage, dict)
        and isinstance(base_coverage.get("rust"), dict)
        and base_coverage["rust"].get("measured") == base_coverage["rust"].get("total")
        and all(name in candidate_by_name for name in rust_names)
    )
    dark_rust_ratio = None
    if rust_complete and rust_names:
        dark_rust_ratio = math.exp(
            math.fsum(
                math.log(candidate_by_name[name].instructions)
                - math.log(rust_by_name[name].instructions)
                for name in rust_names
            )
            / len(rust_names)
        )
    decision = comparison.decision if complete and rust_complete else f"partial-{comparison.decision}"
    payload = {
        "schema_version": 2,
        "suite": "dark-compiler",
        "track": track_dict(TRACK),
        "baseline": {"commit": base["compiler"]["commit"], "contract_sha256": base["contract_sha256"]},
        "candidate": {"commit": candidate["compiler"]["commit"]},
        "toolchains": base["toolchains"],
        "coverage": {"base": base_coverage, "candidate": candidate_coverage},
        "decision": decision,
        "current_baseline_ratio": comparison.ratio,
        "candidate_dark_rust_ratio": dark_rust_ratio,
        "benchmarks": [
            {
                "name": row.name,
                "current": row.current,
                "baseline": row.baseline,
                "absolute_delta": row.absolute_delta,
                "percentage_delta": row.percentage_delta,
            }
            for row in comparison.rows
        ],
    }
    atomic_write_json(decision_path, payload)
    changed = [row for row in comparison.rows if row.absolute_delta != 0]
    rust_text = "unavailable" if dark_rust_ratio is None else f"{dark_rust_ratio:.6f}"
    print(
        f"X86_64_COMPARISON decision={decision} ratio={comparison.ratio:.6f} "
        f"dark_rust={rust_text} changed={len(changed)}/{len(comparison.rows)}"
    )
    for row in changed:
        print(
            f"  {row.name}: {row.baseline} -> {row.current} "
            f"({row.percentage_delta:+.3f}%)"
        )
    return 1 if comparison.decision == "regressed" else 0


def render_results(dark, rust) -> tuple[dict[str, object], str]:
    rust_by_name = {row.name: row for row in rust.benchmarks}
    comparable_dark = tuple(row for row in dark.benchmarks if row.name in rust_by_name)
    comparison = compare_suites(
        comparable_dark, (rust_by_name[row.name] for row in comparable_dark)
    )
    rows_json = []
    rows_markdown = []
    for dark_row in dark.benchmarks:
        rust_row = rust_by_name.get(dark_row.name)
        ratio = dark_row.instructions / rust_row.instructions if rust_row else None
        rows_json.append(
            {
                "name": dark_row.name,
                "dark": dark_row.instructions,
                "rust": rust_row.instructions if rust_row else None,
                "dark_rust_ratio": ratio,
            }
        )
        rust_text = "—" if rust_row is None else f"{rust_row.instructions:,}"
        ratio_text = "—" if ratio is None else f"{ratio:.3f}×"
        rows_markdown.append(
            f"| {dark_row.name} | {dark_row.instructions:,} | {rust_text} | {ratio_text} |"
        )
    payload = {
        "schema_version": 1,
        "suite": "dark-compiler",
        "track": track_dict(TRACK),
        "contract_sha256": dark.contract_sha256,
        "compiler": {"commit": dark.compiler.commit, "subject": dark.compiler.subject},
        "generated_at": dark.generated_at,
        "overall_dark_rust_ratio": comparison.ratio,
        "benchmarks": rows_json,
    }
    markdown = "\n".join(
        [
            "# x86_64 QEMU Benchmark Results",
            "",
            "Canonical quick-profile guest instruction counts under pinned QEMU.",
            "",
            f"**Compiler:** `{dark.compiler.commit}` - {dark.compiler.subject}",
            f"**Generated:** {dark.generated_at}",
            f"**Track:** `{TRACK.id}`",
            f"**Measurement policy:** `{TRACK.measurement_policy}`",
            f"**Overall Dark/Rust:** `{comparison.ratio:.6f}×`",
            "",
            "| Benchmark | Dark instructions | Rust instructions | Dark/Rust |",
            "| --- | ---: | ---: | ---: |",
            *rows_markdown,
            "",
        ]
    )
    return payload, markdown


def append_history(repository: Path, payload: dict[str, object], decision: str, ratio: float) -> None:
    path = repository / "benchmarks" / "HISTORY.x86_64.md"
    header = "\n".join(
        [
            "# x86_64 QEMU Benchmark History",
            "",
            "| Timestamp | Commit | Decision | Current/baseline | Dark/Rust | Track |",
            "| --- | --- | --- | ---: | ---: | --- |",
        ]
    )
    existing = path.read_text().rstrip() if path.is_file() else header
    compiler = payload["compiler"]
    row = (
        f"| {payload['generated_at']} | `{compiler['commit'][:8]}` | {decision} | "
        f"{ratio:.6f}× | {payload['overall_dark_rust_ratio']:.6f}× | `{TRACK.id}` |"
    )
    atomic_write_text(path, existing + "\n" + row + "\n")


def record(
    repository: Path, measurement_path: Path, initialize: bool, refresh_rust: bool
) -> int:
    payload = load_measurement(measurement_path)
    coverage = payload["coverage"]
    if any(coverage[language]["measured"] != coverage[language]["total"] for language in ("dark", "rust")):
        raise ValueError("only complete Dark and audited Rust coverage can be recorded")
    compiler = CompilerAttribution(payload["compiler"]["commit"], payload["compiler"]["subject"])
    timestamp = payload["generated_at"]
    benchmarks_dir = repository / "benchmarks"
    measured_dark = create_snapshot(
        benchmarks_dir, "dark", TRACK, rows(payload, "dark"), timestamp, compiler
    )
    measured_rust = create_snapshot(
        benchmarks_dir, "rust", TRACK, rows(payload, "rust"), timestamp, compiler
    )
    dark_path = snapshot_path(benchmarks_dir, "dark", TRACK)
    rust_path = snapshot_path(benchmarks_dir, "rust", TRACK)
    if initialize:
        if dark_path.exists() or rust_path.exists():
            raise ValueError("initialization requires both canonical x86_64 snapshots to be absent")
        active_dark = measured_dark
        active_rust = measured_rust
        decision = "initialized"
        ratio = 1.0
        write_snapshot(dark_path, active_dark)
        write_snapshot(rust_path, active_rust)
    else:
        if not dark_path.is_file() or not rust_path.is_file():
            raise ValueError("canonical x86_64 snapshots are missing; use --initialize")
        old_dark = load_snapshot(dark_path, benchmarks_dir, "dark", TRACK)
        old_rust = load_snapshot(rust_path, benchmarks_dir, "rust", TRACK)
        rust_comparison = compare_suites(measured_rust.benchmarks, old_rust.benchmarks)
        if rust_comparison.decision != "equal" and not refresh_rust:
            raise ValueError("audited Rust reference changed; use --refresh-rust explicitly")
        if refresh_rust:
            write_snapshot(rust_path, measured_rust)
            active_rust = measured_rust
        else:
            active_rust = old_rust
        dark_comparison = compare_suites(measured_dark.benchmarks, old_dark.benchmarks)
        decision = dark_comparison.decision
        ratio = dark_comparison.ratio
        if decision == "improved":
            write_snapshot(dark_path, measured_dark)
            active_dark = measured_dark
        else:
            active_dark = old_dark
    result_payload, result_markdown = render_results(active_dark, active_rust)
    atomic_write_json(benchmarks_dir / "RESULTS.x86_64.json", result_payload)
    atomic_write_text(benchmarks_dir / "RESULTS.x86_64.md", result_markdown)
    append_history(repository, result_payload, decision, ratio)
    print(
        f"X86_64_RECORD decision={decision} ratio={ratio:.6f} "
        f"dark_rust={result_payload['overall_dark_rust_ratio']:.6f}"
    )
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="action", required=True)
    measure_parser = subparsers.add_parser("measure")
    measure_parser.add_argument("--output", type=Path, required=True)
    measure_parser.add_argument("--allow-partial", action="store_true")
    measure_parser.add_argument(
        "--languages", choices=("dark", "dark,rust"), required=True
    )
    compare_parser = subparsers.add_parser("compare")
    compare_parser.add_argument("--baseline", type=Path, required=True)
    compare_parser.add_argument("--candidate", type=Path, required=True)
    compare_parser.add_argument("--decision-json", type=Path, required=True)
    record_parser = subparsers.add_parser("record")
    record_parser.add_argument("--measurement", type=Path, required=True)
    record_mode = record_parser.add_mutually_exclusive_group(required=True)
    record_mode.add_argument("--initialize", action="store_true")
    record_mode.add_argument("--advance", action="store_true")
    record_parser.add_argument("--refresh-rust", action="store_true")
    args = parser.parse_args()
    repository = Path.cwd().resolve()
    if not (repository / "src" / "DarkCompiler" / "DarkCompiler.fsproj").is_file():
        parser.error("run from the C4D repository root")
    try:
        if args.action == "record" and args.initialize and args.refresh_rust:
            raise ValueError("initialization already records Rust; remove --refresh-rust")
        if args.action == "measure":
            return measurement(
                repository,
                args.output.resolve(),
                args.allow_partial,
                tuple(args.languages.split(",")),
            )
        if args.action == "compare":
            return compare(args.baseline.resolve(), args.candidate.resolve(), args.decision_json.resolve())
        return record(
            repository,
            args.measurement.resolve(),
            args.initialize,
            args.refresh_rust,
        )
    except (OSError, ValueError, subprocess.TimeoutExpired) as error:
        print(f"X86_64_ERROR {error}")
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
