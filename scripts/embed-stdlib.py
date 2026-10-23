#!/usr/bin/env python3
"""Generate immutable OCaml strings for the standard library in declaration order."""
from pathlib import Path
import re
import subprocess
import sys


def ocaml_string(value):
    escapes = {34: '\\"', 92: "\\\\", 10: "\\n", 13: "\\r", 9: "\\t"}
    return '"' + "".join(escapes.get(byte, chr(byte) if 32 <= byte < 127
                                   else f"\\{byte:03d}") for byte in value) + '"'


def main():
    root, output, build_info = map(Path, sys.argv[1:4])
    if len(sys.argv) == 5:
        build_hash = sys.argv[4] or "dev"
    else:
        try:
            revision = subprocess.run(["git", "-C", str(root), "rev-parse", "--short", "HEAD"],
                                      capture_output=True, text=True, check=False)
            build_hash = revision.stdout.strip() if revision.returncode == 0 else "dev"
        except FileNotFoundError:
            build_hash = "dev"
    if build_hash != "dev" and re.fullmatch(r"[0-9a-f]{4,64}", build_hash) is None:
        raise ValueError("Invalid compiler build hash")
    names = [line.strip() for line in (root / "library-sources.list").read_text().splitlines()
             if line.strip() and not line.startswith("#")]
    if len(names) != len(set(names)):
        raise ValueError("Duplicate standard-library source")
    for name in names:
        path = Path(name)
        if (path.is_absolute() or ".." in path.parts or path.suffix != ".dark"
                or path.parts[0] != "packages"):
            raise ValueError(f"Invalid standard-library source: {name}")
    actual = {str(path.relative_to(root)) for path in (root / "packages").rglob("*.dark")}
    if set(names) != actual:
        raise ValueError(f"Source manifest mismatch: {sorted(set(names) ^ actual)}")
    with output.open("w", encoding="utf-8") as target:
        target.write("(* EmbeddedStdlib.ml - Standard library compiled into the executable. *)\n")
        target.write("let sources = [\n")
        for name in names:
            content = (root / name).read_bytes()
            if name == "packages/darklang/stdlib/__builtin/__buildInfo.dark":
                marker = b"__COMPILER_BUILD_HASH__"
                if content.count(marker) != 1:
                    raise ValueError("Expected one compiler build hash marker")
                content = content.replace(marker, build_hash.encode("ascii"))
            target.write(f"  ({ocaml_string(name.encode())}, {ocaml_string(content)});\n")
        target.write("]\n")
        imported = root / "upstream/sources.list"
        target.write("let upstreamSources = [\n")
        if imported.exists():
            paths = [line.strip() for line in imported.read_text().splitlines()
                     if line.strip() and not line.startswith("#")]
            for name in paths:
                path = Path(name)
                if path.is_absolute() or ".." in path.parts or path.suffix != ".dark":
                    raise ValueError(f"Invalid imported source: {name}")
                if name.startswith("packages/"):
                    content = (root / "upstream" / path).read_bytes()
                    target.write(f"  ({ocaml_string(('upstream/' + name).encode())}, {ocaml_string(content)});\n")
        target.write("]\n")
        reasons = (root / "porting/compiler-reasons.json").read_bytes()
        target.write(f"let compilerReasons = {ocaml_string(reasons)}\n")
    build_info.write_text(
        "(* BuildInfo.ml - Build-time compiler revision, shared with Builtin.getBuildHash. *)\n"
        f"let hash = {ocaml_string(build_hash.encode('ascii'))}\n", encoding="utf-8")


if __name__ == "__main__":
    main()
