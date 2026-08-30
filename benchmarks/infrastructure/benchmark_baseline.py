#!/usr/bin/env python3
"""Canonical Dark benchmark snapshots and exact monotonic suite comparison."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import platform
import tempfile
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Iterable

from benchmark_profiles import load_profile


SCHEMA_VERSION = 2
SUITE_ID = "dark-compiler"
DECISION_SCHEMA_VERSION = 2


@dataclass(frozen=True)
class BenchmarkTrack:
    id: str
    profile: str
    architecture: str
    backend: str
    measurement_policy: str


def _track(
    architecture: str, profile: str, backend: str, measurement_policy: str
) -> BenchmarkTrack:
    return BenchmarkTrack(
        f"{architecture}-{profile}-{backend}",
        profile,
        architecture,
        backend,
        measurement_policy,
    )


CACHEGRIND_POLICIES = {
    "quick": "cachegrind-ir-v1:cache-sim=no,branch-sim=no,extract=summary-I-refs",
    "routine": "cachegrind-ir-v1:cache-sim=yes,branch-sim=yes,extract=summary-I-refs",
}
QEMU_QUICK_POLICY = "qemu-tcg-plugin-guest-insns-v1:qemu-11.1.1:rustc-1.89.0"
TRACKS = {
    track.id: track
    for track in (
        *(
            _track(architecture, profile, "cachegrind", policy)
            for architecture in ("arm64", "x86_64")
            for profile, policy in CACHEGRIND_POLICIES.items()
        ),
        _track("x86_64", "quick", "qemu", QEMU_QUICK_POLICY),
    )
}


class BaselineError(ValueError):
    """A malformed or contract-incompatible benchmark artifact."""


@dataclass(frozen=True)
class BenchmarkCount:
    name: str
    instructions: int


@dataclass(frozen=True)
class CompilerAttribution:
    commit: str
    subject: str


@dataclass(frozen=True)
class Snapshot:
    schema_version: int
    suite: str
    language: str
    track: BenchmarkTrack
    contract_sha256: str
    generated_at: str
    compiler: CompilerAttribution
    benchmarks: tuple[BenchmarkCount, ...]

    @property
    def profile(self) -> str:
        return self.track.profile

    @property
    def architecture(self) -> str:
        return self.track.architecture

    @property
    def measurement_policy(self) -> str:
        return self.track.measurement_policy


@dataclass(frozen=True)
class BenchmarkDelta:
    name: str
    current: int
    baseline: int
    absolute_delta: int
    percentage_delta: float


@dataclass(frozen=True)
class SuiteComparison:
    decision: str
    ratio: float
    rows: tuple[BenchmarkDelta, ...]


def normalize_architecture(value: str) -> str:
    normalized = value.strip().lower()
    aliases = {
        "aarch64": "arm64",
        "arm64": "arm64",
        "amd64": "x86_64",
        "x64": "x86_64",
        "x86_64": "x86_64",
    }
    if normalized not in aliases:
        raise BaselineError(f"unsupported architecture: {value!r}")
    return aliases[normalized]


def machine_architecture() -> str:
    return normalize_architecture(platform.machine())


def _reject_duplicate_object_keys(pairs: list[tuple[str, object]]) -> dict[str, object]:
    result: dict[str, object] = {}
    for key, value in pairs:
        if key in result:
            raise BaselineError(f"duplicate JSON field: {key}")
        result[key] = value
    return result


def _load_json(path: Path) -> object:
    try:
        return json.loads(path.read_text(), object_pairs_hook=_reject_duplicate_object_keys)
    except (OSError, json.JSONDecodeError) as error:
        raise BaselineError(f"cannot parse {path}: {error}") from error


def _exact_fields(value: dict[str, object], expected: set[str], context: str) -> None:
    actual = set(value)
    if actual != expected:
        missing = ", ".join(sorted(expected - actual))
        unexpected = ", ".join(sorted(actual - expected))
        details = []
        if missing:
            details.append(f"missing {missing}")
        if unexpected:
            details.append(f"unexpected {unexpected}")
        raise BaselineError(f"{context} fields are invalid ({'; '.join(details)})")


def _positive_integer(value: object, context: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise BaselineError(f"{context} must be a positive integer")
    return value


def validate_counts(
    counts: Iterable[BenchmarkCount], expected_names: Iterable[str]
) -> tuple[BenchmarkCount, ...]:
    values = tuple(counts)
    expected = tuple(expected_names)
    names = tuple(value.name for value in values)
    duplicates = sorted({name for name in names if names.count(name) > 1})
    if duplicates:
        raise BaselineError(f"duplicate benchmark counts: {', '.join(duplicates)}")
    if names != expected:
        missing = [name for name in expected if name not in names]
        unexpected = [name for name in names if name not in expected]
        details = []
        if missing:
            details.append(f"missing {', '.join(missing)}")
        if unexpected:
            details.append(f"unexpected {', '.join(unexpected)}")
        if not details:
            details.append("benchmark order differs from the declared profile")
        raise BaselineError(
            f"benchmark counts do not match profile ({'; '.join(details)})"
        )
    for value in values:
        _positive_integer(value.instructions, f"{value.name} instruction count")
    return values


def load_count_rows(path: Path, expected_names: Iterable[str]) -> tuple[BenchmarkCount, ...]:
    rows: list[BenchmarkCount] = []
    try:
        lines = path.read_text().splitlines()
    except OSError as error:
        raise BaselineError(f"cannot read counts from {path}: {error}") from error
    for line_number, line in enumerate(lines, 1):
        parts = line.split("\t")
        if len(parts) != 2 or not parts[0] or not parts[1].isdigit():
            raise BaselineError(f"malformed count row at {path}:{line_number}")
        rows.append(BenchmarkCount(parts[0], int(parts[1])))
    return validate_counts(rows, expected_names)


def contract_digest(benchmarks_dir: Path, profile_name: str) -> str:
    names = load_profile(benchmarks_dir, profile_name)
    parity = _load_json(benchmarks_dir / "PARITY.json")
    if not isinstance(parity, dict) or parity.get("schema") != 2:
        raise BaselineError("PARITY.json must use schema 2")
    entries = parity.get("benchmarks")
    if not isinstance(entries, dict):
        raise BaselineError("PARITY.json must contain a benchmarks object")

    contract_rows = []
    use_quick = profile_name in {"quick", "quick-fast"}
    for name in names:
        entry = entries.get(name)
        if not isinstance(entry, dict):
            raise BaselineError(f"PARITY.json has no object for {name}")
        selected = entry.get("quick") if use_quick else entry
        if not isinstance(selected, dict):
            qualifier = "quick " if use_quick else ""
            raise BaselineError(f"PARITY.json has no {qualifier}contract for {name}")
        relevant = {
            key: selected[key]
            for key in sorted(selected)
            if key == "status" or key.endswith("_sha256")
        }
        if "status" not in relevant:
            raise BaselineError(f"PARITY.json has no status for {name}")
        contract_rows.append({"name": name, "parity": relevant})

    payload = {
        "profile": profile_name,
        "benchmarks": contract_rows,
    }
    encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def snapshot_names(
    benchmarks_dir: Path, language: str, profile_name: str
) -> tuple[str, ...]:
    names = tuple(load_profile(benchmarks_dir, profile_name))
    if language == "dark":
        return names
    if language != "rust":
        raise BaselineError(f"unsupported benchmark language: {language!r}")
    parity = _load_json(benchmarks_dir / "PARITY.json")
    entries = parity.get("benchmarks") if isinstance(parity, dict) else None
    if not isinstance(entries, dict):
        raise BaselineError("PARITY.json must contain a benchmarks object")
    use_quick = profile_name in {"quick", "quick-fast"}
    comparable: list[str] = []
    for name in names:
        entry = entries.get(name)
        selected = entry.get("quick") if use_quick and isinstance(entry, dict) else entry
        if not isinstance(selected, dict) or not isinstance(selected.get("status"), str):
            raise BaselineError(f"PARITY.json has no valid status for {name}")
        if selected["status"] == "comparable":
            comparable.append(name)
    return tuple(comparable)


def snapshot_path(
    benchmarks_dir: Path, language: str, track: BenchmarkTrack
) -> Path:
    if language not in {"dark", "rust"}:
        raise BaselineError(f"unsupported benchmark language: {language!r}")
    return benchmarks_dir / "baselines" / f"{language}-{track.id}.json"


def create_snapshot(
    benchmarks_dir: Path,
    language: str,
    track: BenchmarkTrack,
    counts: Iterable[BenchmarkCount],
    generated_at: str,
    compiler: CompilerAttribution,
) -> Snapshot:
    if language not in {"dark", "rust"}:
        raise BaselineError(f"unsupported benchmark language: {language!r}")
    if TRACKS.get(track.id) != track:
        raise BaselineError(f"unsupported benchmark track: {track.id!r}")
    names = snapshot_names(benchmarks_dir, language, track.profile)
    normalized_counts = validate_counts(counts, names)
    if len(compiler.commit) != 40 or any(
        c not in "0123456789abcdef" for c in compiler.commit
    ):
        raise BaselineError("compiler attribution must contain a full lowercase Git commit")
    try:
        parsed_timestamp = datetime.fromisoformat(generated_at.replace("Z", "+00:00"))
    except ValueError as error:
        raise BaselineError("snapshot timestamp must be ISO-8601") from error
    if parsed_timestamp.tzinfo is None:
        raise BaselineError("snapshot timestamp must include a UTC offset")
    return Snapshot(
        schema_version=SCHEMA_VERSION,
        suite=SUITE_ID,
        language=language,
        track=track,
        contract_sha256=contract_digest(benchmarks_dir, track.profile),
        generated_at=generated_at,
        compiler=compiler,
        benchmarks=normalized_counts,
    )


def track_dict(track: BenchmarkTrack) -> dict[str, str]:
    return {
        "id": track.id,
        "profile": track.profile,
        "architecture": track.architecture,
        "backend": track.backend,
        "measurement_policy": track.measurement_policy,
    }


def _snapshot_dict(snapshot: Snapshot) -> dict[str, object]:
    return {
        "schema_version": snapshot.schema_version,
        "suite": snapshot.suite,
        "language": snapshot.language,
        "track": track_dict(snapshot.track),
        "contract_sha256": snapshot.contract_sha256,
        "generated_at": snapshot.generated_at,
        "compiler": {
            "commit": snapshot.compiler.commit,
            "subject": snapshot.compiler.subject,
        },
        "benchmarks": [
            {"name": row.name, "instructions": row.instructions}
            for row in snapshot.benchmarks
        ],
    }


def atomic_write_json(path: Path, value: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    encoded = json.dumps(value, indent=2, ensure_ascii=False) + "\n"
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.", dir=path.parent
    )
    temporary_path = Path(temporary_name)
    try:
        mode = path.stat().st_mode & 0o777 if path.exists() else 0o644
        os.fchmod(descriptor, mode)
        with os.fdopen(descriptor, "w") as temporary_file:
            temporary_file.write(encoded)
            temporary_file.flush()
            os.fsync(temporary_file.fileno())
        os.replace(temporary_path, path)
    except BaseException:
        temporary_path.unlink(missing_ok=True)
        raise


def atomic_write_text(path: Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.", dir=path.parent
    )
    temporary_path = Path(temporary_name)
    try:
        mode = path.stat().st_mode & 0o777 if path.exists() else 0o644
        os.fchmod(descriptor, mode)
        with os.fdopen(descriptor, "w") as temporary_file:
            temporary_file.write(value)
            temporary_file.flush()
            os.fsync(temporary_file.fileno())
        os.replace(temporary_path, path)
    except BaseException:
        temporary_path.unlink(missing_ok=True)
        raise


def write_snapshot(path: Path, snapshot: Snapshot) -> None:
    atomic_write_json(path, _snapshot_dict(snapshot))


def load_snapshot(
    path: Path,
    benchmarks_dir: Path,
    expected_language: str,
    expected_track: BenchmarkTrack,
) -> Snapshot:
    raw = _load_json(path)
    if not isinstance(raw, dict):
        raise BaselineError(f"snapshot {path} must be a JSON object")
    _exact_fields(
        raw,
        {
            "schema_version",
            "suite",
            "language",
            "track",
            "contract_sha256",
            "generated_at",
            "compiler",
            "benchmarks",
        },
        "snapshot",
    )
    track_raw = raw["track"]
    if not isinstance(track_raw, dict):
        raise BaselineError("snapshot track must be an object")
    _exact_fields(
        track_raw,
        {"id", "profile", "architecture", "backend", "measurement_policy"},
        "snapshot track",
    )
    compiler_raw = raw["compiler"]
    if not isinstance(compiler_raw, dict):
        raise BaselineError("snapshot compiler must be an object")
    _exact_fields(compiler_raw, {"commit", "subject"}, "snapshot compiler")
    rows_raw = raw["benchmarks"]
    if not isinstance(rows_raw, list):
        raise BaselineError("snapshot benchmarks must be an array")
    rows: list[BenchmarkCount] = []
    for index, row in enumerate(rows_raw):
        if not isinstance(row, dict):
            raise BaselineError(f"snapshot benchmark {index} must be an object")
        _exact_fields(row, {"name", "instructions"}, f"snapshot benchmark {index}")
        if not isinstance(row["name"], str) or not row["name"]:
            raise BaselineError(f"snapshot benchmark {index} has an invalid name")
        rows.append(
            BenchmarkCount(
                row["name"],
                _positive_integer(
                    row["instructions"], f"{row['name']} instruction count"
                ),
            )
        )

    if raw["schema_version"] != SCHEMA_VERSION:
        raise BaselineError(
            f"snapshot schema is {raw['schema_version']!r}, expected {SCHEMA_VERSION}"
        )
    if raw["suite"] != SUITE_ID:
        raise BaselineError(
            f"snapshot suite is {raw['suite']!r}, expected {SUITE_ID!r}"
        )
    if raw["language"] != expected_language:
        raise BaselineError(
            f"snapshot language is {raw['language']!r}, expected {expected_language!r}"
        )
    actual_track = TRACKS.get(str(track_raw["id"]))
    if actual_track is None or actual_track != expected_track:
        raise BaselineError(
            f"snapshot track is {track_raw.get('id')!r}, expected {expected_track.id!r}"
        )
    expected_track_dict = track_dict(expected_track)
    if track_raw != expected_track_dict:
        raise BaselineError("snapshot track metadata is incompatible")
    expected_digest = contract_digest(benchmarks_dir, expected_track.profile)
    if raw["contract_sha256"] != expected_digest:
        raise BaselineError("snapshot workload contract digest is incompatible")
    if (
        not isinstance(compiler_raw["commit"], str)
        or len(compiler_raw["commit"]) != 40
        or any(c not in "0123456789abcdef" for c in compiler_raw["commit"])
    ):
        raise BaselineError("snapshot compiler commit must be a full Git commit")
    if not isinstance(compiler_raw["subject"], str):
        raise BaselineError("snapshot compiler subject must be a string")
    if not isinstance(raw["generated_at"], str):
        raise BaselineError("snapshot generated_at must be a string")
    try:
        parsed_timestamp = datetime.fromisoformat(raw["generated_at"].replace("Z", "+00:00"))
    except ValueError as error:
        raise BaselineError("snapshot generated_at must be ISO-8601") from error
    if parsed_timestamp.tzinfo is None:
        raise BaselineError("snapshot generated_at must include a UTC offset")
    expected_names = snapshot_names(
        benchmarks_dir, expected_language, expected_track.profile
    )
    validated_rows = validate_counts(rows, expected_names)
    return Snapshot(
        SCHEMA_VERSION,
        SUITE_ID,
        expected_language,
        expected_track,
        expected_digest,
        raw["generated_at"],
        CompilerAttribution(compiler_raw["commit"], compiler_raw["subject"]),
        validated_rows,
    )


def project_counts(
    counts: Iterable[BenchmarkCount], names: Iterable[str]
) -> tuple[BenchmarkCount, ...]:
    by_name = {row.name: row for row in counts}
    expected = tuple(names)
    missing = [name for name in expected if name not in by_name]
    if missing:
        raise BaselineError(f"baseline projection is missing: {', '.join(missing)}")
    return tuple(by_name[name] for name in expected)


def parse_targeted_selection(
    selection: str, canonical_names: Iterable[str]
) -> tuple[str, ...]:
    canonical = tuple(canonical_names)
    selected = tuple(name.strip() for name in selection.split(",") if name.strip())
    if not selected:
        raise BaselineError("targeted quick selection must not be empty")
    duplicates = sorted({name for name in selected if selected.count(name) > 1})
    if duplicates:
        raise BaselineError(
            f"duplicate targeted quick benchmarks: {', '.join(duplicates)}"
        )
    unknown = [name for name in selected if name not in canonical]
    if unknown:
        raise BaselineError(
            f"unknown targeted quick benchmarks: {', '.join(unknown)}"
        )
    return selected


def compare_suites(
    current: Iterable[BenchmarkCount], baseline: Iterable[BenchmarkCount]
) -> SuiteComparison:
    current_rows = tuple(current)
    baseline_rows = tuple(baseline)
    if not current_rows or not baseline_rows:
        raise BaselineError("benchmark suites must not be empty")
    current_names = tuple(row.name for row in current_rows)
    baseline_names = tuple(row.name for row in baseline_rows)
    current_rows = validate_counts(current_rows, current_names)
    baseline_rows = validate_counts(baseline_rows, baseline_names)
    if current_names != baseline_names:
        raise BaselineError("current and baseline benchmark name sets/order differ")

    current_product = math.prod(row.instructions for row in current_rows)
    baseline_product = math.prod(row.instructions for row in baseline_rows)
    if current_product < baseline_product:
        decision = "improved"
    elif current_product > baseline_product:
        decision = "regressed"
    else:
        decision = "equal"
    ratio = math.exp(
        math.fsum(
            math.log(current_row.instructions) - math.log(baseline_row.instructions)
            for current_row, baseline_row in zip(current_rows, baseline_rows)
        ) / len(current_rows)
    )
    deltas = tuple(
        BenchmarkDelta(
            current_row.name,
            current_row.instructions,
            baseline_row.instructions,
            current_row.instructions - baseline_row.instructions,
            (current_row.instructions - baseline_row.instructions)
            / baseline_row.instructions
            * 100.0,
        )
        for current_row, baseline_row in zip(current_rows, baseline_rows)
    )
    return SuiteComparison(decision, ratio, deltas)


def compare_implementations(dark: Snapshot, rust: Snapshot) -> SuiteComparison:
    if dark.language != "dark" or rust.language != "rust":
        raise BaselineError("implementation comparison requires Dark and Rust snapshots")
    if dark.track != rust.track:
        raise BaselineError("implementation snapshots use different benchmark tracks")
    if dark.contract_sha256 != rust.contract_sha256:
        raise BaselineError("implementation snapshots use different workload contracts")
    return compare_suites(dark.benchmarks, rust.benchmarks)


def comparison_dict(
    comparison: SuiteComparison,
    profile: str,
    baseline: Snapshot,
    snapshot_action: str,
) -> dict[str, object]:
    return {
        "schema_version": DECISION_SCHEMA_VERSION,
        "suite": SUITE_ID,
        "track": track_dict(baseline.track),
        "selection_profile": profile,
        "baseline": {
            "commit": baseline.compiler.commit,
            "contract_sha256": baseline.contract_sha256,
        },
        "decision": comparison.decision,
        "current_baseline_ratio": comparison.ratio,
        "snapshot_action": snapshot_action,
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


def print_comparison(
    comparison: SuiteComparison, baseline: Snapshot, summary_name: str = "suite"
) -> None:
    print(
        f"Dark baseline: commit {baseline.compiler.commit}, contract "
        f"{baseline.contract_sha256}, track {baseline.track.id}"
    )
    for row in comparison.rows:
        print(
            f"  {row.name}: current {row.current:,}, baseline {row.baseline:,}, "
            f"delta {row.absolute_delta:+,} ({row.percentage_delta:+.3f}%)"
        )
    print(
        f"Dark {summary_name}: {comparison.decision}; current/baseline geometric ratio "
        f"{comparison.ratio:.6f}"
    )


def load_dark_counts(
    results_dir: Path, expected_names: Iterable[str]
) -> tuple[BenchmarkCount, ...]:
    expected = tuple(expected_names)
    found: dict[str, BenchmarkCount] = {}
    actual_files: set[str] = set()
    for result_path in sorted(results_dir.glob("*_cachegrind.json")):
        name = result_path.name.removesuffix("_cachegrind.json")
        if name in actual_files:
            raise BaselineError(f"duplicate result file for {name}")
        actual_files.add(name)
        raw = _load_json(result_path)
        if not isinstance(raw, dict) or not isinstance(raw.get("results"), list):
            raise BaselineError(f"malformed cachegrind result: {result_path}")
        dark_rows = [
            row
            for row in raw["results"]
            if isinstance(row, dict)
            and row.get("language", "").lower() == "dark"
        ]
        if len(dark_rows) != 1:
            raise BaselineError(f"{name}: expected exactly one Dark result, found {len(dark_rows)}")
        instructions = _positive_integer(
            dark_rows[0].get("instructions"), f"{name} Dark instruction count"
        )
        found[name] = BenchmarkCount(name, instructions)
    actual_names = set(found)
    expected_name_set = set(expected)
    if actual_names != expected_name_set:
        missing = sorted(expected_name_set - actual_names)
        unexpected = sorted(actual_names - expected_name_set)
        details = []
        if missing:
            details.append(f"missing {', '.join(missing)}")
        if unexpected:
            details.append(f"unexpected {', '.join(unexpected)}")
        raise BaselineError(
            f"Dark result set does not match profile ({'; '.join(details)})"
        )
    return validate_counts((found[name] for name in expected if name in found), expected)


def _quick_command(args: argparse.Namespace) -> int:
    benchmarks_dir = Path(args.benchmarks_dir).resolve()
    track = TRACKS[args.track]
    if track.profile != "quick":
        raise BaselineError(f"quick comparison requires a quick track, got {track.id}")
    canonical_names = load_profile(benchmarks_dir, "quick")
    selection = getattr(args, "selection", None)
    if selection and args.fast:
        raise BaselineError("targeted quick selection cannot be combined with quick-fast")
    if selection and args.reset:
        raise BaselineError("targeted quick runs cannot reset the canonical snapshot")
    selected_profile = "targeted" if selection else "quick-fast" if args.fast else "quick"
    selected_names = (
        parse_targeted_selection(selection, canonical_names)
        if selection
        else load_profile(benchmarks_dir, selected_profile)
    )
    counts = load_count_rows(Path(args.counts), selected_names)
    baseline_path = snapshot_path(benchmarks_dir, "dark", track)

    if args.reset:
        if args.fast:
            raise BaselineError("quick-fast runs cannot reset the complete quick baseline")
        snapshot = create_snapshot(
            benchmarks_dir,
            "dark",
            track,
            counts,
            args.timestamp,
            CompilerAttribution(args.commit, args.subject),
        )
        write_snapshot(baseline_path, snapshot)
        decision = {
            "schema_version": DECISION_SCHEMA_VERSION,
            "suite": SUITE_ID,
            "track": track_dict(snapshot.track),
            "decision": "reset",
            "current_baseline_ratio": 1.0,
            "snapshot_action": "reset",
            "baseline": {
                "commit": snapshot.compiler.commit,
                "contract_sha256": snapshot.contract_sha256,
            },
            "benchmarks": [],
        }
        atomic_write_json(Path(args.decision_json), decision)
        print(f"Dark quick baseline reset atomically: {baseline_path}")
        return 0

    baseline = load_snapshot(baseline_path, benchmarks_dir, "dark", track)
    baseline_counts = project_counts(baseline.benchmarks, selected_names)
    comparison = compare_suites(counts, baseline_counts)
    if selection:
        action = "targeted-only"
    elif args.fast:
        action = "projection-only"
    elif comparison.decision == "improved":
        updated = create_snapshot(
            benchmarks_dir,
            "dark",
            track,
            counts,
            args.timestamp,
            CompilerAttribution(args.commit, args.subject),
        )
        write_snapshot(baseline_path, updated)
        action = "advanced"
    elif comparison.decision == "equal":
        action = "unchanged-equal"
    else:
        action = "preserved-stronger-baseline"
    summary_name = "targeted selection" if selection else "suite"
    if args.quiet:
        print(
            f"Dark {summary_name}: {comparison.decision}; current/baseline geometric ratio "
            f"{comparison.ratio:.6f}"
        )
    else:
        print_comparison(comparison, baseline, summary_name)
    if selection:
        print("Dark quick snapshot: preserved (targeted-only)")
    else:
        print(f"Dark quick snapshot: {action}")
    decision = comparison_dict(comparison, selected_profile, baseline, action)
    if selection:
        decision["selected_benchmarks"] = list(selected_names)
        decision["promotion_eligible"] = False
        decision["candidate"] = {
            "commit": args.commit,
            "subject": args.subject,
            "measured_at": args.timestamp,
        }
    atomic_write_json(Path(args.decision_json), decision)
    return 1 if comparison.decision == "regressed" else 0


def _validate_command(args: argparse.Namespace) -> int:
    benchmarks_dir = Path(args.benchmarks_dir).resolve()
    track = TRACKS[args.track]
    path = snapshot_path(benchmarks_dir, args.language, track)
    snapshot = load_snapshot(path, benchmarks_dir, args.language, track)
    print(
        f"Valid {args.language} baseline: {path} (commit {snapshot.compiler.commit}, "
        f"contract {snapshot.contract_sha256})"
    )
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    quick = subparsers.add_parser(
        "quick", help="compare a complete or explicitly targeted quick run"
    )
    quick.add_argument("--benchmarks-dir", required=True)
    quick.add_argument("--track", choices=sorted(TRACKS), required=True)
    quick.add_argument("--counts", required=True)
    quick.add_argument("--commit", required=True)
    quick.add_argument("--subject", default="")
    quick.add_argument("--timestamp", required=True)
    quick.add_argument("--decision-json", required=True)
    quick.add_argument("--fast", action="store_true")
    quick.add_argument(
        "--selection",
        help="comma-separated quick benchmarks to compare without changing the snapshot",
    )
    quick.add_argument("--reset", action="store_true")
    quick.add_argument("--quiet", action="store_true")
    quick.set_defaults(handler=_quick_command)

    validate = subparsers.add_parser("validate", help="validate a canonical snapshot")
    validate.add_argument("--benchmarks-dir", required=True)
    validate.add_argument("--language", choices=("dark", "rust"), required=True)
    validate.add_argument("--track", choices=sorted(TRACKS), required=True)
    validate.set_defaults(handler=_validate_command)
    args = parser.parse_args()
    try:
        return args.handler(args)
    except (BaselineError, OSError, KeyError, ValueError) as error:
        print(f"Dark baseline error: {error}")
        print("Run a complete successful full suite with --reset-dark-baseline.")
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
