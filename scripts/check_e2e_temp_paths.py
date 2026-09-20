#!/usr/bin/env python3
"""Reject E2E tests that write to a fixed shared /tmp path."""

from __future__ import annotations

import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
E2E_ROOT = ROOT / "src" / "Tests" / "e2e"
WRITES_TEMP = re.compile(
    r"(?:let\s+\w+\s*=\s*|overwriteFile\s+|appendToFile\s+|writeFile\s+|deleteFile\s+)"
    r'"/tmp/[^"$]*"'
)


def violations(source: str) -> list[tuple[int, str]]:
    return [
        (number, line.strip())
        for number, line in enumerate(source.splitlines(), start=1)
        if WRITES_TEMP.search(line)
    ]


def main() -> int:
    found: list[str] = []
    for path in sorted(E2E_ROOT.rglob("*.e2e")):
        for number, line in violations(path.read_text(encoding="utf-8")):
            found.append(f"{path.relative_to(ROOT)}:{number}: fixed writable /tmp path: {line}")
    if found:
        print("\n".join(found), file=sys.stderr)
        print(
            "Use a per-process path containing currentPid(), or keep the entire "
            "write/read/delete lifecycle inside one isolated test.",
            file=sys.stderr,
        )
        return 1
    print("E2E temporary-path isolation gate passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
