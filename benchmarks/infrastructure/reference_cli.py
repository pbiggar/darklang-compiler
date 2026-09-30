"""Unified CLI for independent reference refreshes and measurement-free reports."""

from __future__ import annotations

import argparse
import json
import platform
import shutil
import subprocess
import sys
import tempfile
from datetime import datetime, timezone
from pathlib import Path

from benchmark_baseline import CACHEGRIND_POLICY, machine_architecture
from benchmark_parity import load_contract, validate_entry
from benchmark_profiles import load_invocation, load_profile
from benchmark_reports import documents, generate_reports
from diagnostic_references import command_version, measure_one, parse_instruction_count
from reference_snapshots import (
    LANGUAGES, REFERENCE_LANGUAGES, measured_reference, read_json, reference_path,
    row_status, save_reference, source_path, source_digest, workload_digest,
)


SUPPORTED = ("rust", "python", "node", "ocaml")
BUILD_FLAGS = {"rust": ["rustc -C opt-level=3; Cargo --release --locked --offline"],
               "python": [], "node": [], "ocaml": ["-O3"]}
RUNTIME_FLAGS = {"rust": [], "python": [], "node": ["--stack-size=400000"], "ocaml": []}
VERSIONS = {"rust": ["rustc", "--version"], "python": ["python3", "--version"],
            "node": ["node", "--version"], "ocaml": ["ocamlopt", "-version"]}


def measure_rust(root: Path, temporary: Path, name: str, profile: str, timeout: int) -> dict:
    source = source_path(root, name, "rust")
    prepared = temporary / name
    shutil.copytree(source.parent, prepared, ignore=shutil.ignore_patterns("target", "main", "*.o"))
    binary = prepared / "main"
    if (prepared / "Cargo.toml").is_file():
        command = ["cargo", "build", "--release", "--locked", "--offline", "--bin", "benchmark"]
        subprocess.run(command, cwd=prepared, check=True, capture_output=True, text=True, timeout=timeout)
        binary = prepared / "target" / "release" / "benchmark"
    else:
        subprocess.run(["rustc", "-C", "opt-level=3", str(prepared / "main.rs"), "-o", str(binary)],
                       check=True, capture_output=True, text=True, timeout=timeout)
    invocation = load_invocation(root, profile, name)
    measured = subprocess.run([
        "valgrind", "--tool=cachegrind", "--cache-sim=no", "--branch-sim=no",
        "--main-stacksize=536870912", "--cachegrind-out-file=/dev/null", str(binary), *invocation.args,
    ], check=True, capture_output=True, text=True, timeout=timeout)
    if measured.stdout != invocation.expected_stdout:
        raise ValueError(f"{name} Rust output mismatch: {measured.stdout!r}")
    return {"name": name, "instructions": parse_instruction_count(measured.stderr), "output_valid": True}


def preflight(root: Path, language: str, profile: str) -> tuple[str, dict[str, str]]:
    if language not in SUPPORTED:
        raise ValueError(f"{LANGUAGES[language]} execution adapter is not implemented")
    executable = VERSIONS[language][0]
    for tool in ("valgrind", executable):
        if shutil.which(tool) is None:
            raise ValueError(f"required executable is unavailable: {tool}")
    names = load_profile(root, profile)
    if not any(source_path(root, name, language).is_file() for name in names):
        raise ValueError(f"{LANGUAGES[language]} has no implementations in {profile}")
    if language == "rust":
        contract = load_contract(root)
        failures = [failure for name in names for failure in validate_entry(root, name, contract[name])]
        if failures or any(contract[name].get("status") != "comparable" for name in names):
            raise ValueError("Rust refresh requires current audited parity: " + "; ".join(failures))
        if any((source_path(root, name, language).parent / "Cargo.toml").is_file() for name in names):
            if shutil.which("cargo") is None:
                raise ValueError("Cargo is required for application references")
    version = command_version(VERSIONS[language])
    if language == "ocaml" and version.split(".", 1)[0] != "5":
        raise ValueError(f"OCaml 5 is required; installed version is {version}")
    tools = {"valgrind": command_version(["valgrind", "--version"]),
             "machine": platform.platform()}
    if language == "rust" and shutil.which("cargo"):
        tools["cargo"] = command_version(["cargo", "--version"])
    return version, tools


def refresh(root: Path, args) -> None:
    if args.language == "dark":
        runner = root / ("quick_check.sh" if args.profile == "quick" else "run_benchmarks.sh")
        arguments = ["--build"] if args.profile == "quick" else ["full"]
        subprocess.run([str(runner), *arguments], cwd=root.parent, check=True)
        generate_reports(root)
        return
    languages = [args.language]
    if args.language == "all":
        names = load_profile(root, args.profile)
        languages = [language for language in SUPPORTED
                     if any(source_path(root, name, language).is_file() for name in names)]
        for language in REFERENCE_LANGUAGES:
            if language not in languages:
                print(f"{LANGUAGES[language]}: skipped (no supported implementations)")
    # All requested toolchains are checked before any build or measurement.
    prepared = {language: preflight(root, language, args.profile) for language in languages}
    for language in languages:
        version, tools = prepared[language]
        names = load_profile(root, args.profile)
        identity = {name: (source_digest(root, name, language), workload_digest(root, args.profile, name))
                    for name in names}
        rows = []
        with tempfile.TemporaryDirectory(prefix="benchmark-reference-") as temporary:
            for name in names:
                if not source_path(root, name, language).is_file():
                    continue
                if language == "rust":
                    row = measure_rust(root, Path(temporary), name, args.profile, args.timeout)
                else:
                    row = measure_one(root, Path(temporary), name, language, args.profile,
                                      None, None, args.timeout)
                if row is None:
                    raise ValueError(f"{name} {language}: implementation disappeared during refresh")
                rows.append(row)
                print(f'{language} {name}: {row["instructions"]:,}', flush=True)
        after = {name: (source_digest(root, name, language), workload_digest(root, args.profile, name))
                 for name in load_profile(root, args.profile)}
        if identity != after or version != command_version(VERSIONS[language]):
            raise ValueError(f"{language}: sources, workloads, or toolchain changed during measurement; snapshot preserved")
        document = measured_reference(
            root, language, args.profile, machine_architecture(), version,
            datetime.now(timezone.utc).isoformat(), rows,
            BUILD_FLAGS[language], RUNTIME_FLAGS[language], tools,
        )
        save_reference(root, document)
        # Each language commits independently; a later language failure cannot erase it.
        generate_reports(root)
        print(f"Updated {reference_path(root, document['track']['id'], language)}")


def import_legacy(root: Path) -> None:
    """Split historical files without inventing source, version, or workload provenance."""
    for path in sorted((root / "baselines").glob("diagnostic-*.json")):
        legacy = read_json(path)
        track = {"id": f'{legacy["architecture"]}-{legacy["profile"]}-cachegrind',
                 "architecture": legacy["architecture"], "profile": legacy["profile"],
                 "backend": "cachegrind", "measurement_policy": legacy["measurement_policy"]}
        for language, implementation in legacy["implementations"].items():
            if language not in LANGUAGES:
                raise ValueError(f"{path}: unsupported legacy language {language}")
            destination = reference_path(root, track["id"], language)
            if destination.exists():
                continue
            save_reference(root, {
                "schema_version": 1, "language": language, "track": track,
                "version": implementation["version"], "generated_at": legacy["generated_at"],
                "contract_sha256": legacy["contract_sha256"], "build_flags": [], "runtime_flags": [],
                "benchmarks": implementation["benchmarks"],
                "provenance": f"Imported from {path.relative_to(root)}; original suite contract retained. "
                              "Per-source hashes and build flags were not recorded.",
            })
    # The legacy full Rust table has no version/timestamp attribution. Associate
    # it only with the track and contract explicitly named in its original report.
    baseline = root / "BASELINES.md"
    report = root / "RESULTS.md"
    if baseline.is_file() and report.is_file():
        metadata = {}
        for line in report.read_text().splitlines():
            for key in ("Architecture", "Workload contract", "Profile"):
                if line.startswith(f"**{key}:**") and "`" in line:
                    metadata[key] = line.split("`", 2)[1]
        if metadata.get("Profile") == "full" and "Architecture" in metadata and "Workload contract" in metadata:
            track = {"id": f'{metadata["Architecture"]}-full-cachegrind', "architecture": metadata["Architecture"],
                     "profile": "full", "backend": "cachegrind", "measurement_policy": CACHEGRIND_POLICY}
            if not reference_path(root, track["id"], "rust").exists():
                rows = []
                for line in baseline.read_text().splitlines():
                    cells = [cell.strip() for cell in line.split("|")[1:-1]]
                    if len(cells) == 3 and cells[1] == "rust":
                        rows.append({"name": cells[0], "instructions": int(cells[2].replace(",", "")),
                                     "output_valid": True})
                if rows:
                    save_reference(root, {"schema_version": 1, "language": "rust", "track": track,
                                          "version": "not recorded", "generated_at": "not recorded",
                                          "contract_sha256": metadata["Workload contract"], "benchmarks": rows,
                                          "build_flags": [], "runtime_flags": [],
                                          "provenance": "Imported audited counts from BASELINES.md using the track and "
                                                        "contract named by the original RESULTS.md. Rust version, "
                                                        "measurement date and per-source hashes were not recorded."})


def status(root: Path, profile: str) -> None:
    track = f"{machine_architecture()}-{profile}-cachegrind"
    stored = documents(root, track)
    names = load_profile(root, profile)
    print(f"Track: {track}")
    for language in LANGUAGES:
        if language == "darklang-interpreter" and language not in stored:
            continue
        document = stored.get(language)
        rows = document["benchmarks"] if document else []
        current = sum(row["name"] in names and row_status(root, document, row, profile) == "current" for row in rows)
        coverage = sum(source_path(root, name, language).is_file() for name in names)
        executable = VERSIONS.get(language, ["dark" if language == "dark" else language])[0]
        installed = "installed" if shutil.which(executable) else "unavailable"
        version = document["version"] if document else "not recorded"
        print(f"{LANGUAGES[language]}: sources {coverage}/{len(names)}, current {current}/{len(names)}, "
              f"stored version {version}; {executable} {installed}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    refresh_parser = commands.add_parser("refresh", help="build and measure a language, then regenerate reports")
    refresh_parser.add_argument("language", choices=["dark", *REFERENCE_LANGUAGES, "all"])
    refresh_parser.add_argument("--profile", choices=("full", "quick"), default="full")
    refresh_parser.add_argument("--metric", choices=("instructions",), default="instructions")
    refresh_parser.add_argument("--timeout", type=int, default=3600)
    report_parser = commands.add_parser("report", help="regenerate reports without running benchmarks")
    report_parser.add_argument("--check", action="store_true", help="check consistency without writing files")
    status_parser = commands.add_parser("status", help="show stored versions and coverage without executing toolchains")
    status_parser.add_argument("--profile", choices=("full", "quick"), default="full")
    commands.add_parser("import-legacy", help="import historical reference files without measuring")
    verify_parser = commands.add_parser("verify", help="run the existing Darklang regression gate")
    verify_parser.add_argument("--against", choices=("parent", "deployed"), required=True)
    verify_parser.add_argument("--profile", choices=("full",), default="full")
    args = parser.parse_args()
    root = Path(__file__).resolve().parent.parent
    try:
        if args.command == "refresh":
            if args.timeout <= 0:
                parser.error("--timeout must be positive")
            refresh(root, args)
        elif args.command == "report":
            changed = generate_reports(root, check=args.check)
            for path in changed:
                print(f"{'Out of date' if args.check else 'Generated'}: {path.relative_to(root)}")
            return 1 if args.check and changed else 0
        elif args.command == "status":
            status(root, args.profile)
        elif args.command == "import-legacy":
            import_legacy(root)
            generate_reports(root)
        elif args.command == "verify":
            return subprocess.run([str(root / "run_benchmarks.sh"), f"--verify-{args.against}", args.profile],
                                  cwd=root.parent).returncode
        return 0
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        detail = error.stderr[-2000:] if isinstance(error, subprocess.CalledProcessError) and error.stderr else str(error)
        print(f"Benchmark maintenance failed: {detail}", file=sys.stderr)
        return 1
