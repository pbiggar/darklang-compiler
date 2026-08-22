#!/usr/bin/env python3
"""Bind a benchmark measurement to its freshly built executable and audited source."""

import argparse
import hashlib
import json
import sys
from pathlib import Path


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def fail(message: str) -> int:
    print(f"Artifact provenance rejected: {message}", file=sys.stderr)
    return 1


def write(args: argparse.Namespace) -> int:
    source = Path(args.source)
    compiler = Path(args.compiler)
    executable = Path(args.executable)
    if not source.is_file() or not compiler.is_file() or not executable.is_file():
        return fail("source, compiler, and executable must all exist")
    manifest = {
        "schema_version": 1,
        "benchmark": args.benchmark,
        "language": args.language,
        "source": {"path": str(source.resolve()), "sha256": digest(source)},
        "compiler": {"path": str(compiler.resolve()), "sha256": digest(compiler)},
        "executable": {"path": str(executable.resolve()), "sha256": digest(executable)},
    }
    Path(args.manifest).write_text(json.dumps(manifest, indent=2) + "\n")
    return 0


def verify(args: argparse.Namespace) -> int:
    try:
        manifest = json.loads(Path(args.manifest).read_text())
    except (OSError, json.JSONDecodeError) as error:
        return fail(f"manifest is unreadable: {error}")
    required = {"schema_version", "benchmark", "language", "source", "compiler", "executable"}
    if not isinstance(manifest, dict) or set(manifest) != required or manifest["schema_version"] != 1:
        return fail("manifest schema is invalid")
    if manifest["benchmark"] != args.benchmark or manifest["language"] != args.language:
        return fail("manifest benchmark or language does not match measurement")
    source = Path(args.source)
    executable = Path(args.executable)
    if not source.is_file() or not executable.is_file():
        return fail("measured source or executable is missing")
    if manifest["source"].get("path") != str(source.resolve()) or manifest["source"].get("sha256") != digest(source):
        return fail("audited source does not match its build manifest")
    if manifest["source"]["sha256"] != args.audited_source_sha256:
        return fail("source does not match the parity audit")
    if manifest["executable"].get("path") != str(executable.resolve()) or manifest["executable"].get("sha256") != digest(executable):
        return fail("executable does not match its build manifest")
    compiler = Path(manifest["compiler"].get("path", ""))
    if not compiler.is_file() or manifest["compiler"].get("sha256") != digest(compiler):
        return fail("compiler identity does not match its build manifest")
    return 0


parser = argparse.ArgumentParser(description=__doc__)
commands = parser.add_subparsers(dest="command", required=True)
for name in ("write", "verify"):
    command = commands.add_parser(name)
    command.add_argument("--benchmark", required=True)
    command.add_argument("--language", required=True)
    command.add_argument("--source", required=True)
    command.add_argument("--executable", required=True)
    command.add_argument("--manifest", required=True)
    if name == "write":
        command.add_argument("--compiler", required=True)
    else:
        command.add_argument("--audited-source-sha256", required=True)
args = parser.parse_args()
sys.exit(write(args) if args.command == "write" else verify(args))
