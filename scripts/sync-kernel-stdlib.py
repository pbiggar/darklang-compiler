#!/usr/bin/env python3
"""Copy shared Dark kernel libraries from an interpreter checkout.

--check verifies byte equality (with only the compiler's existing nested-module
file splitting). This script never changes native backend adapters.
"""
import argparse
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parent.parent
FILES = {
    "syscall": "Syscall", "runtime": "Runtime", "kernelABI": "KernelABI",
    "kernelData": "KernelData", "directoryIO": "DirectoryIO", "glob": "Glob",
    "posix/io": "Posix/IO", "posix/errors": "Posix/Errors",
    "posix/accounts": "Posix/Accounts", "posix/temp": "Posix/Temp",
    "fileCopy": "FileCopy", "file": "File",
    "env": "Env",
    "fileSystem": "FileSystem",
}

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("interpreter", type=Path)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    source = args.interpreter / "packages/darklang/stdlib/cli"
    outputs = {f"StdLib/Cli/{dest}.dark": (source / f"{src}.dark").read_bytes()
               for src, dest in FILES.items()}
    filesystem = outputs["StdLib/Cli/FileSystem.dark"].decode()
    nested = re.search(r"^module FileError =\n((?:  .*\n|\n)+)", filesystem, re.MULTILINE)
    if nested is None:
        raise SystemExit("Missing expected FileSystem.FileError module")
    body = "\n".join(line[2:] if line.startswith("  ") else line
                     for line in nested.group(1).splitlines())
    outputs["StdLib/Cli/FileSystem/FileError.dark"] = (
        "module Darklang.Stdlib.Cli.FileSystem.FileError\n\n" + body.rstrip() + "\n").encode()
    outputs["StdLib/Cli/FileSystem.dark"] = (
        filesystem[:nested.start()] + filesystem[nested.end():]).encode()
    posix = (source / "posix.dark").read_text()
    # The compiler embeds nested modules as separately named source units.
    for name in ("Modes", "OpenFlags", "Errno", "StatMode"):
        pattern = rf"^module {name} =\n((?:  .*\n|\n)+)"
        match = re.search(pattern, posix, re.MULTILINE)
        if match is None:
            raise SystemExit(f"Missing expected Posix.{name} module")
        body = "\n".join(line[2:] if line.startswith("  ") else line
                         for line in match.group(1).splitlines())
        outputs[f"StdLib/Cli/Posix/{name}.dark"] = (
            f"module Darklang.Stdlib.Cli.Posix.{name}\n\n{body.rstrip()}\n").encode()
        posix = posix[:match.start()] + posix[match.end():]
    outputs["StdLib/Cli/Posix.dark"] = posix.encode()
    different = []
    for relative, content in outputs.items():
        target = ROOT / relative
        if not target.exists() or target.read_bytes() != content:
            different.append(relative)
            if not args.check:
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(content)
    if args.check and different:
        print("Shared source drift:\n" + "\n".join(different), file=sys.stderr)
        return 1
    print(f"{'Checked' if args.check else 'Synchronized'} {len(outputs)} shared source files")
    return 0

if __name__ == "__main__":
    sys.exit(main())
