#!/usr/bin/env python3
"""Freeze the reference source/fixture inventory and verify migration coverage."""

import argparse
import hashlib
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ORACLE = "df9dae7e1647275f6bc9104618f20ef84a7251be"
REFERENCE = "27edbf054b400623f1856803fa0aef051f443cf8"
MANIFEST = ROOT / "ocaml/inventory.json"


def git(*args):
    return subprocess.check_output(["git", *args], cwd=ROOT)


def kind(path):
    if path.startswith("src/DarkCompiler/stdlib/"):
        return "stdlib"
    if path.startswith("src/DarkCompiler/") and path.endswith(".fs"):
        return "compiler"
    if path.startswith("src/Tests/"):
        return "test-source" if path.endswith(".fs") else "test-input"
    if path in ("dark", "build", "run-tests", "Dockerfile", "global.json"):
        return "entrypoint"
    return None


def fixture(path):
    return kind(path) in ("stdlib", "test-input")


def owner(path):
    if kind(path) == "compiler":
        return "ocaml/lib/" + path[len("src/DarkCompiler/"):-3] + ".ml"
    if kind(path) == "test-source":
        return "ocaml/tests/" + path[len("src/Tests/"):-3] + ".ml"
    return path


def capture():
    entries = []
    for path in git("ls-tree", "-r", "--name-only", ORACLE).decode().splitlines():
        category = kind(path)
        if category is None:
            continue
        content = git("show", f"{ORACLE}:{path}")
        entries.append({"source": path, "kind": category,
                        "sha256": hashlib.sha256(content).hexdigest(),
                        "owner": owner(path)})
    MANIFEST.write_text(json.dumps({"reference": REFERENCE, "oracle": ORACLE,
                                   "entries": entries}, indent=2) + "\n")
    print(f"Frozen {len(entries)} source, fixture, and entrypoint records")


def verify(require_complete):
    manifest = json.loads(MANIFEST.read_text())
    errors = []
    translated = 0
    implementation_count = 0
    for entry in manifest["entries"]:
        source = ROOT / entry["source"]
        if fixture(entry["source"]):
            if not source.is_file() or hashlib.sha256(source.read_bytes()).hexdigest() != entry["sha256"]:
                errors.append(f"Frozen input changed: {entry['source']}")
        if entry["kind"] in ("compiler", "test-source"):
            implementation_count += 1
            implementation = ROOT / entry["owner"]
            interface = implementation.with_suffix(".mli")
            if implementation.is_file() and interface.is_file():
                translated += 1
            elif implementation.is_file() != interface.is_file():
                errors.append(f"Missing interface or implementation: {entry['owner']}")
            elif require_complete:
                errors.append(f"Unported: {entry['source']}")
    print(f"Coverage: {translated}/{implementation_count} implementation/interface pairs")
    for error in errors[:20]:
        print(error)
    if len(errors) > 20:
        print(f"... {len(errors) - 20} more failures")
    return 1 if errors else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("capture", "verify"))
    parser.add_argument("--require-complete", action="store_true")
    args = parser.parse_args()
    if args.command == "capture":
        capture()
        return 0
    return verify(args.require_complete)


if __name__ == "__main__":
    raise SystemExit(main())
