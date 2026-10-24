#!/usr/bin/env python3
"""Pin source SQL templates for the pure-Dark package-store compatibility suite.

This is a static inventory, not a runtime SQL capture. Interpolated strings and
concatenation fragments remain templates and must acquire concrete workload cases.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess

PIN = "9ca42515810126f9cdf8ab9704ae44cc7e2e7241"
START = re.compile(r"^(?:SELECT|WITH|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|PRAGMA|VACUUM|BEGIN|COMMIT|ROLLBACK)\b", re.I)


def strings(text):
    """Yield decoded source literals while excluding comments in either language."""
    i = 0
    while i < len(text):
        if text.startswith("//", i):
            end = text.find("\n", i)
            i = len(text) if end < 0 else end + 1
        elif text.startswith("(*", i):
            depth, i = 1, i + 2
            while i < len(text) and depth:
                if text.startswith("(*", i):
                    depth, i = depth + 1, i + 2
                elif text.startswith("*)", i):
                    depth, i = depth - 1, i + 2
                else:
                    i += 1
        elif text[i] == '"':
            start = i
            interpolated = i > 0 and text[i - 1] == '$'
            verbatim = i > 0 and text[i - 1] == '@'
            if text.startswith('"""', i):
                end = text.find('"""', i + 3)
                if end < 0:
                    raise ValueError("unterminated triple string")
                value, i = text[i + 3:end], end + 3
            else:
                i += 1
                while i < len(text):
                    if text[i] == '"':
                        if verbatim and text.startswith('""', i):
                            i += 2
                        else:
                            break
                    elif text[i] == '\\' and not verbatim:
                        i += 2
                    else:
                        i += 1
                i += 1
                literal = text[start:i]
                if verbatim:
                    value = literal[1:-1].replace('""', '"')
                else:
                    try:
                        value = json.loads(re.sub(r"\\\r?\n[ \t]*", "", literal))
                    except json.JSONDecodeError:
                        # Non-SQL host strings may contain F#-only escapes.
                        value = literal[1:-1]
            yield start, i, value, interpolated
        else:
            i += 1


def inventory(source):
    files = [path for path in sorted((source / "backend/src/LibDB").glob("*.fs"))
             if path.name not in {"Tracing.fs", "Traces.fs", "TestProcess.fs"}]
    files += sorted((source / "packages/darklang/scm").glob("*.dark"))
    entries, hashes = [], {}
    for path in files:
        relative = path.relative_to(source).as_posix()
        raw = path.read_bytes()
        hashes[relative] = hashlib.sha256(raw).hexdigest()
        text = raw.decode()
        literals = list(strings(text))
        index = 0
        while index < len(literals):
            start, end, sql, dynamic = literals[index]
            index += 1
            # Resolve adjacent literal concatenations; leave variable splices explicit.
            while index < len(literals) and text[end:literals[index][0]].strip() == '+':
                _, end, tail, tail_dynamic = literals[index]
                sql += tail
                dynamic |= tail_dynamic
                index += 1
            sql = sql.strip()
            candidate = re.sub(r"(?m)^\s*--[^\n]*(?:\n|$)", "", sql).lstrip()
            if not START.match(candidate):
                continue
            if re.fullmatch(r"(?:CREATE TABLE|CREATE (?:UNIQUE )?INDEX|INSERT OR IGNORE)", candidate, re.I):
                continue  # Statement-kind filters in Releases, not executable SQL.
            if candidate.startswith("WITH RECURSIVE chain(bid)") and candidate.endswith(")"):
                dynamic = True  # Shared CTE prefix appended at its call sites.
            dynamic |= bool(re.match(r"\s*\+", text[end:]))
            dynamic |= bool(re.search(r"\+\s*\(?\s*\$?$", text[max(0, start - 30):start]))
            entries.append({"source": relative, "line": text.count('\n', 0, start) + 1,
                            "kind": "template" if dynamic else "statement", "sql": sql})
    return {"interpreter_revision": PIN, "scope": "LibDB and Darklang.SCM source literals; dynamic templates require concrete runtime cases",
            "source_sha256": hashes, "entries": entries}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("interpreter", type=Path)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    revision = subprocess.check_output(["git", "-C", str(args.interpreter), "rev-parse", "HEAD"], text=True).strip()
    if revision != PIN:
        parser.error(f"expected interpreter {PIN}, got {revision}; review and update the pin explicitly")
    result = inventory(args.interpreter)
    root = Path(__file__).resolve().parent.parent / "test/fixtures/sqlite-package"
    outputs = {root / "corpus.json": (json.dumps(result, ensure_ascii=False, indent=2) + '\n').encode(),
               root / "schema.sql": (args.interpreter / "backend/migrations/schema/00-initial-schema.sql").read_bytes()}
    for path, content in outputs.items():
        if args.check:
            if path.read_bytes() != content:
                raise SystemExit(f"out of date: {path}")
        else:
            path.write_bytes(content)
    count = len(result["entries"])
    templates = sum(item["kind"] == "template" for item in result["entries"])
    print(f"{count} source SQL entries ({templates} dynamic templates); {len(result['source_sha256'])} pinned source files")


if __name__ == "__main__":
    main()
