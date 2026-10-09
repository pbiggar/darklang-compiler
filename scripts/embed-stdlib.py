#!/usr/bin/env python3
"""Generate immutable OCaml strings for the standard library in declaration order."""
from pathlib import Path
import re
import sys


def ocaml_string(value):
    escapes = {34: '\\"', 92: "\\\\", 10: "\\n", 13: "\\r", 9: "\\t"}
    return '"' + "".join(escapes.get(byte, chr(byte) if 32 <= byte < 127
                                   else f"\\{byte:03d}") for byte in value) + '"'


def main():
    root, output, build_info = map(Path, sys.argv[1:4])
    build_hash = sys.argv[4] or "dev"
    if build_hash != "dev" and re.fullmatch(r"[0-9a-f]{7,40}", build_hash) is None:
        raise ValueError("Invalid compiler build hash")
    names = [line.strip() for line in (root / "library-sources.list").read_text().splitlines()
             if line.strip() and not line.startswith("#")]
    if len(names) != len(set(names)):
        raise ValueError("Duplicate standard-library source")
    for name in names:
        path = Path(name)
        if (path.is_absolute() or ".." in path.parts or path.suffix != ".dark"
                or path.parts[0] not in {"StdLib", "packages"}):
            raise ValueError(f"Invalid standard-library source: {name}")
    actual = {str(path.relative_to(root)) for directory in ("StdLib", "packages")
              for path in (root / directory).rglob("*.dark")}
    if set(names) != actual:
        raise ValueError(f"Source manifest mismatch: {sorted(set(names) ^ actual)}")
    with output.open("w", encoding="utf-8") as target:
        target.write("(* EmbeddedStdlib.ml - Standard library compiled into the executable. *)\n")
        target.write("let sources = [\n")
        for name in names:
            content = (root / name).read_bytes()
            if name == "StdLib/Builtin/__BuildInfo.dark":
                marker = b"__COMPILER_BUILD_HASH__"
                if content.count(marker) != 1:
                    raise ValueError("Expected one compiler build hash marker")
                content = content.replace(marker, build_hash.encode("ascii"))
            target.write(f"  ({ocaml_string(name.encode())}, {ocaml_string(content)});\n")
        target.write("]\n")
    build_info.write_text(
        "(* BuildInfo.ml - Build-time compiler revision, shared with Builtin.getBuildHash. *)\n"
        f"let hash = {ocaml_string(build_hash.encode('ascii'))}\n", encoding="utf-8")


if __name__ == "__main__":
    main()
