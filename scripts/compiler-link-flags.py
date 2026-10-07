#!/usr/bin/env python3
"""Embed the OCaml libraries' non-system dependencies in the compiler executable."""
import json
from pathlib import Path
import subprocess
import sys


def archive(package):
    directory = subprocess.check_output(
        ["pkg-config", "--variable=libdir", package], text=True).strip()
    path = Path(directory) / ("lib" + package + ".a")
    if not path.is_file():
        raise FileNotFoundError(f"Install the {package} development archive: {path}")
    return str(path)


def main():
    system, output = sys.argv[1:]
    archives = [archive("sqlite3"), archive("gmp")]
    if system.startswith("linux"):
        flags = ["-ccopt", "-Wl,--whole-archive," + ",".join(archives) + ",--no-whole-archive"]
    elif system == "macosx":
        flags = [argument for path in archives for argument in ("-ccopt", "-Wl,-force_load," + path)]
        flags += ["-ccopt", "-Wl,-dead_strip_dylibs"]
    else:
        raise ValueError(f"Unsupported standalone compiler host: {system}")
    Path(output).write_text("(" + " ".join(json.dumps(flag) for flag in flags) + ")\n")


if __name__ == "__main__":
    main()
