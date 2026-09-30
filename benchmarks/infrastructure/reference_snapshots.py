"""Independent reference snapshots, workload compatibility, and reviewed parity."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

from benchmark_baseline import CACHEGRIND_POLICY, TRACKS, atomic_write_json, contract_digest
from benchmark_parity import load_contract, source_hash, source_tree_hash, validate_entry
from benchmark_profiles import load_invocation, load_profile


LANGUAGES = {
    "dark": "Darklang",
    "rust": "Rust",
    "haskell": "Haskell",
    "python": "Python",
    "node": "Node",
    "roc": "Roc",
    "ocaml": "OCaml 5",
    "koka": "Koka",
    "darklang-interpreter": "Darklang interpreter",
}
REFERENCE_LANGUAGES = tuple(language for language in LANGUAGES if language not in {
    "dark", "darklang-interpreter",
})
EXTENSIONS = {
    "dark": "dark", "rust": "rs", "haskell": "hs", "python": "py",
    "node": "js", "roc": "roc", "ocaml": "ml", "koka": "kk",
    "darklang-interpreter": "dark",
}


def read_json(path: Path) -> dict:
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError(f"{path}: duplicate field {key}")
            result[key] = value
        return result

    document = json.loads(path.read_text(), object_pairs_hook=unique)
    if not isinstance(document, dict):
        raise ValueError(f"{path}: expected a JSON object")
    return document


def source_path(root: Path, name: str, language: str) -> Path:
    directory = "dark" if language == "darklang-interpreter" else language
    return root / "problems" / name / directory / f"main.{EXTENSIONS[language]}"


def source_digest(root: Path, name: str, language: str) -> str | None:
    source = source_path(root, name, language)
    if not source.is_file():
        return None
    if language == "rust" and (source.parent / "Cargo.toml").is_file():
        return source_tree_hash(source.parent)
    return source_hash(source)


def workload_digest(root: Path, profile: str, name: str) -> str:
    invocation = load_invocation(root, profile, name)
    payload = {"name": name, "args": invocation.args,
               "expected_stdout": invocation.expected_stdout}
    return hashlib.sha256(json.dumps(payload, sort_keys=True).encode()).hexdigest()


def reference_path(root: Path, track: str, language: str) -> Path:
    return root / "references" / track / f"{language}.json"


def validate_reference(document: dict) -> None:
    if document.get("schema_version") != 1 or document.get("language") not in LANGUAGES:
        raise ValueError("invalid reference snapshot schema or language")
    track = document.get("track", {})
    if not isinstance(track, dict) or any(not isinstance(track.get(key), str) for key in (
        "id", "architecture", "profile", "backend", "measurement_policy",
    )):
        raise ValueError("reference snapshot has no complete track identity")
    if track["id"] != f'{track["architecture"]}-{track["profile"]}-{track["backend"]}':
        raise ValueError("reference track id disagrees with its metadata")
    if track["id"] not in TRACKS:
        raise ValueError(f'unsupported reference track: {track["id"]}')
    if not isinstance(document.get("version"), str) or not isinstance(document.get("generated_at"), str):
        raise ValueError("reference snapshot lacks version or timestamp provenance")
    rows = document.get("benchmarks")
    if not isinstance(rows, list):
        raise ValueError("reference benchmarks must be a list")
    seen = set()
    for row in rows:
        if not isinstance(row, dict) or not isinstance(row.get("name"), str):
            raise ValueError("malformed reference row")
        if row["name"] in seen:
            raise ValueError(f'duplicate reference row: {row["name"]}')
        seen.add(row["name"])
        if type(row.get("instructions")) is not int or row["instructions"] <= 0:
            raise ValueError(f'{row["name"]}: instruction count must be a positive integer')
        if type(row.get("output_valid")) is not bool:
            raise ValueError(f'{row["name"]}: output validation marker must be boolean')


def save_reference(root: Path, document: dict) -> None:
    validate_reference(document)
    atomic_write_json(reference_path(root, document["track"]["id"], document["language"]), document)


def row_status(root: Path, document: dict, row: dict, profile: str) -> str:
    if row.get("output_valid") is not True:
        return "output mismatch"
    language = document["language"]
    track = TRACKS.get(document["track"]["id"])
    if track is None or document["track"]["measurement_policy"] != track.measurement_policy:
        return "incompatible measurement policy"
    if "workload_sha256" in row:
        if row["workload_sha256"] != workload_digest(root, profile, row["name"]):
            return "stale workload"
        if row.get("source_sha256") != source_digest(root, row["name"], language):
            return "stale source"
    elif document.get("contract_sha256") != contract_digest(root, profile):
        return "stale workload contract"
    return "current"


def audited(root: Path, name: str, language: str) -> bool:
    if language in {"dark", "rust"}:
        entry = load_contract(root).get(name)
        return bool(entry and entry.get("status") == "comparable"
                    and not validate_entry(root, name, entry))
    path = root / "REFERENCE-PARITY.json"
    if not path.is_file():
        return False
    audit = read_json(path)
    if audit.get("schema_version") != 1:
        raise ValueError("REFERENCE-PARITY.json must use schema_version 1")
    entry = audit.get("benchmarks", {}).get(name, {}).get(language, {})
    return bool(entry.get("status") == "comparable"
                and entry.get("source_sha256") == source_digest(root, name, language)
                and entry.get("rust_sha256") == source_digest(root, name, "rust"))


def measured_reference(root: Path, language: str, profile: str, architecture: str,
                       version: str, generated_at: str, rows: list[dict],
                       build_flags: list[str], runtime_flags: list[str],
                       tools: dict[str, str]) -> dict:
    names = load_profile(root, profile)
    expected = {name for name in names if source_digest(root, name, language) is not None}
    measured = [row.get("name") for row in rows]
    if len(measured) != len(set(measured)) or set(measured) != expected:
        raise ValueError(f"{language}: refresh must measure every available implementation exactly once")
    if any(row.get("output_valid") is not True for row in rows):
        raise ValueError(f"{language}: refresh contains output-invalid measurements")
    return {
        "schema_version": 1,
        "language": language,
        "track": {"id": f"{architecture}-{profile}-cachegrind", "architecture": architecture,
                  "profile": profile, "backend": "cachegrind", "measurement_policy": CACHEGRIND_POLICY},
        "version": version, "generated_at": generated_at,
        "build_flags": build_flags, "runtime_flags": runtime_flags, "tools": tools,
        "contract_sha256": contract_digest(root, profile),
        "benchmarks": [{**row, "workload_sha256": workload_digest(root, profile, row["name"]),
                        "source_sha256": source_digest(root, row["name"], language)} for row in rows],
        "missing_implementations": [name for name in names if source_digest(root, name, language) is None],
    }
