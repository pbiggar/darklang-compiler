#!/usr/bin/env python3
"""Read native VM pins from the canonical Dockerfile and dependency lock."""

from pathlib import Path
import re


def pins():
    repo = Path(__file__).resolve().parents[2]
    docker = (repo / "Dockerfile").read_text()
    version = re.search(r"^ARG OCAML_VERSION=(\S+)$", docker, re.MULTILINE)
    checksum = re.search(r"echo '([0-9a-f]{64})  ocaml.tar.gz'", docker)
    if not version or not checksum:
        raise ValueError("Cannot read OCaml version/hash from Dockerfile; update VM pin reader")
    packages = [line.strip() for line in (repo / "dependencies.lock").read_text().splitlines()
                if line.strip() and not line.lstrip().startswith("#")]
    dune = [package.removeprefix("dune.") for package in packages if package.startswith("dune.")]
    if len(dune) != 1:
        raise ValueError("dependencies.lock must contain exactly one Dune pin")
    return version[1], checksum[1], dune[0], packages


if __name__ == "__main__":
    version, checksum, _, _ = pins()
    print(version, checksum)
