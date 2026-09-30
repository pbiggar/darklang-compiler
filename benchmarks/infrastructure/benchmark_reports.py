"""Deterministic comparison reports generated only from stored measurements."""

from __future__ import annotations

import math
from pathlib import Path

from benchmark_baseline import TRACKS, atomic_write_text, contract_digest
from benchmark_profiles import load_profile
from reference_snapshots import (
    LANGUAGES, audited, read_json, reference_path, row_status, source_path, validate_reference,
)


def markdown(value: object) -> str:
    return str(value).replace("|", "\\|").replace("\n", " ").replace("\r", " ")


def table(headers: list[str], rows: list[list[object]]) -> list[str]:
    return ["| " + " | ".join(headers) + " |", "| " + " | ".join(["---"] * len(headers)) + " |",
            *("| " + " | ".join(markdown(cell) for cell in row) + " |" for row in rows)]


def canonical_document(root: Path, path: Path) -> dict:
    document = read_json(path)
    if document.get("schema_version") != 2 or document.get("language") not in {"dark", "rust"}:
        raise ValueError(f"{path}: unsupported canonical snapshot")
    rows = document.get("benchmarks")
    if not isinstance(rows, list) or any(not isinstance(row, dict) for row in rows):
        raise ValueError(f"{path}: malformed canonical rows")
    converted = {**document, "schema_version": 1,
                 "version": document.get("compiler", {}).get("commit", "unknown"),
                 "benchmarks": [{**row, "output_valid": True} for row in rows]}
    validate_reference(converted)
    return converted


def documents(root: Path, track: str) -> dict[str, dict]:
    result = {}
    for language in LANGUAGES:
        reference = reference_path(root, track, language)
        canonical = root / "baselines" / f"{language}-{track}.json"
        if language == "dark" and canonical.is_file():
            result[language] = canonical_document(root, canonical)
        elif reference.is_file():
            document = read_json(reference)
            validate_reference(document)
            if document["language"] != language or document["track"]["id"] != track:
                raise ValueError(f"{reference}: snapshot identity disagrees with its path")
            result[language] = document
        elif language == "rust" and canonical.is_file():
            result[language] = canonical_document(root, canonical)
    return result


def report_for_track(root: Path, track_id: str) -> str:
    track = TRACKS[track_id]
    names = load_profile(root, track.profile)
    stored = documents(root, track_id)
    languages = list(LANGUAGES)
    if "darklang-interpreter" not in stored:
        languages.remove("darklang-interpreter")
    values: dict[str, dict[str, int]] = {}
    statuses: dict[str, dict[str, str]] = {}
    rows_by_language = {}
    for language in languages:
        document = stored.get(language)
        rows = {row["name"]: row for row in document["benchmarks"]} if document else {}
        rows_by_language[language] = rows
        values[language] = {}
        statuses[language] = {}
        for name in names:
            row = rows.get(name)
            if row is None:
                status = ("not measured" if source_path(root, name, language).is_file()
                          else "missing implementation")
            else:
                status = row_status(root, document, row, track.profile)
            statuses[language][name] = status
            if status == "current":
                values[language][name] = row["instructions"]
    lines = [f"# {track_id} comparison", "",
             f"Architecture: `{track.architecture}`. Profile: `{track.profile}`.", "",
             f"Measurement policy: `{track.measurement_policy}`.", "",
             f"Current workload contract: `{contract_digest(root, track.profile)}`.", "",
             "Instruction counts include runtime and startup work; they are not elapsed-time speed ratios.", "",
             "Snapshots are independent. Stale values are retained for provenance and excluded from ratios.", "",
             "## Stored versions and coverage", ""]
    metadata = []
    for language in languages:
        document = stored.get(language, {})
        valid = values[language]
        reviewed = sum(audited(root, name, language) for name in valid)
        metadata.append([LANGUAGES[language], document.get("version", "not recorded"),
                         document.get("generated_at", "not recorded"),
                         f"{len(valid)}/{len(names)}", f"{reviewed}/{len(valid)}",
                         ", ".join(document.get("build_flags", [])) or "not recorded",
                         ", ".join(document.get("runtime_flags", [])) or "not recorded"])
    lines.extend(table(["Language", "Version / compiler commit", "Measured", "Current rows",
                        "Audited current rows", "Build flags", "Runtime flags"], metadata))
    for language, document in stored.items():
        if document.get("provenance"):
            lines.extend(["", f'{LANGUAGES[language]} provenance: {markdown(document["provenance"])}'])
    lines.extend(["", "Audited means the implementation's algorithm and source have a current reviewed parity contract.",
                  "Output validation alone does not establish algorithm parity.", "", "## Absolute instructions", ""])
    absolute = []
    ratios = []
    for name in names:
        cells: list[object] = [name]
        ratio_cells: list[object] = [name]
        for language in languages:
            row = rows_by_language[language].get(name)
            status = statuses[language][name]
            cells.append(f'{row["instructions"]:,}' if status == "current" else
                         f'{row["instructions"]:,} ({status})' if row else status)
            rust = values["rust"].get(name)
            count = values[language].get(name)
            ratio_cells.append(f"{count / rust:.3f}×" if count and rust else "unavailable")
        absolute.append(cells)
        ratios.append(ratio_cells)
    headers = ["Benchmark", *(LANGUAGES[language] for language in languages)]
    lines.extend(table(headers, absolute))
    lines.extend(["", "## Instructions relative to Rust", "",
                  "Ratios for unaudited implementations are informational comparisons of validated output.", ""])
    lines.extend(table(headers, ratios))
    eligible = [language for language in languages if values[language]]
    common = [name for name in names if eligible and all(name in values[language] for language in eligible)]
    lines.extend(["", "## Common-workload aggregate", ""])
    if "rust" in eligible and common:
        lines.extend(["All included languages use this exact workload set: " + ", ".join(f"`{name}`" for name in common) + ".", "",
                      "Geometric means below are informational unless every included row is audited.", ""])
        summary = []
        for language in eligible:
            ratio = math.exp(math.fsum(math.log(values[language][name] / values["rust"][name])
                                      for name in common) / len(common))
            summary.append([LANGUAGES[language], f"{ratio:.3f}×",
                            "audited" if all(audited(root, name, language) for name in common) else "unaudited"])
        lines.extend(table(["Language", "Instructions / Rust", "Parity"], summary))
        excluded = [LANGUAGES[language] for language in languages if language not in eligible]
        if excluded:
            lines.extend(["", "Excluded because no current measurements exist: " + ", ".join(excluded) + "."])
    else:
        lines.append("Unavailable: no shared current workload set with a Rust reference.")
    return "\n".join(lines) + "\n"


def report_outputs(root: Path) -> dict[Path, str]:
    tracks = {path.parent.name for path in (root / "references").glob("*/*.json")}
    for path in (root / "baselines").glob("dark-*.json"):
        tracks.add(path.stem.removeprefix("dark-"))
    unknown = tracks - TRACKS.keys()
    if unknown:
        raise ValueError(f"unsupported report tracks: {', '.join(sorted(unknown))}")
    output = {}
    index = ["# Benchmark Results", "", "Generated by `./benchmarks/bench report` from stored measurements; no benchmarks are run.", "",
             "Each report compares Darklang with Rust, Haskell, Python, Node, Roc, OCaml 5, and Koka.", "",
             "Reference upgrades do not change Darklang's regression baselines.", ""]
    baseline_index = ["# Benchmark References", "", "Independent stored reference measurements, including version and workload provenance.", ""]
    for track in sorted(tracks):
        path = root / "reports" / f"{track}.md"
        output[path] = report_for_track(root, track)
        index.append(f"- [{track}](reports/{track}.md)")
        for language in LANGUAGES:
            reference = reference_path(root, track, language)
            if reference.is_file():
                baseline_index.append(f"- [{LANGUAGES[language]} / {track}]({reference.relative_to(root).as_posix()})")
    index.extend(["", "Run `./benchmarks/bench status` for coverage and stale measurements."])
    output[root / "RESULTS.md"] = "\n".join(index) + "\n"
    output[root / "BASELINES.md"] = "\n".join(baseline_index) + "\n"
    # Keep the existing x86_64 report and JSON consumers current as well.
    qemu = "x86_64-quick-qemu"
    dark = root / "baselines" / f"dark-{qemu}.json"
    rust = root / "baselines" / f"rust-{qemu}.json"
    if (dark.is_file() and rust.is_file()
            and read_json(dark).get("contract_sha256") == contract_digest(root, "quick")
            and read_json(rust).get("contract_sha256") == contract_digest(root, "quick")):
        from benchmark_baseline import load_snapshot
        from x86_64_reports import render_results
        import json

        dark_snapshot = load_snapshot(dark, root, "dark", TRACKS[qemu])
        rust_snapshot = load_snapshot(rust, root, "rust", TRACKS[qemu])
        payload, rendered = render_results(dark_snapshot, rust_snapshot)
        output[root / "RESULTS.x86_64.md"] = rendered
        output[root / "RESULTS.x86_64.json"] = json.dumps(payload, indent=2) + "\n"
    return output


def generate_reports(root: Path, check: bool = False) -> list[Path]:
    output = report_outputs(root)
    obsolete = set((root / "reports").glob("*.md")) - output.keys()
    changed = [path for path, contents in output.items()
               if not path.is_file() or path.read_text() != contents] + sorted(obsolete)
    if not check:
        for path in changed:
            if path in output:
                atomic_write_text(path, output[path])
            else:
                path.unlink()
    return changed
