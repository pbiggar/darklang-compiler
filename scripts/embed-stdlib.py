#!/usr/bin/env python3
"""Generate immutable OCaml strings for the standard library in declaration order."""
from pathlib import Path
import sys


def ocaml_string(value):
    escapes = {34: '\\"', 92: "\\\\", 10: "\\n", 13: "\\r", 9: "\\t"}
    return '"' + "".join(escapes.get(byte, chr(byte) if 32 <= byte < 127
                                   else f"\\{byte:03d}") for byte in value) + '"'


def main():
    root, output = map(Path, sys.argv[1:])
    names = [line.strip() for line in (root / "sources.list").read_text().splitlines()
             if line.strip() and not line.startswith("#")]
    if len(names) != len(set(names)):
        raise ValueError("Duplicate standard-library source")
    for name in names:
        path = Path(name)
        if path.is_absolute() or ".." in path.parts or path.suffix != ".dark":
            raise ValueError(f"Invalid standard-library source: {name}")
    actual = {str(path.relative_to(root)) for path in root.rglob("*.dark")}
    if set(names) != actual:
        raise ValueError(f"Source manifest mismatch: {sorted(set(names) ^ actual)}")
    with output.open("w", encoding="utf-8") as target:
        target.write("(* EmbeddedStdlib.ml - Standard library compiled into the executable. *)\n")
        target.write("let sources = [\n")
        for name in names:
            target.write(f"  ({ocaml_string(name.encode())}, {ocaml_string((root / name).read_bytes())});\n")
        target.write("]\n")


if __name__ == "__main__":
    main()
