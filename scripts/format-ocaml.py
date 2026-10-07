#!/usr/bin/env python3
"""Repeat the post-port formatting pass with the standard ocamlformat profile."""
import argparse
from pathlib import Path
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    arguments = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    files = sorted(path for directory in ("src", "bin", "test", "tools")
                   for path in (root / directory).rglob("*")
                   if path.suffix in (".ml", ".mli"))
    subprocess.run(["ocamlformat", "--check" if arguments.check else "--inplace",
                    *map(str, files)], cwd=root, check=True)


if __name__ == "__main__":
    main()
