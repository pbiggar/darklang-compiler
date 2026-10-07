#!/usr/bin/env python3
"""Resolve compiler-host C dependencies, including Homebrew's keg-only ICU."""
import json
import os
from pathlib import Path
import platform
import shlex
import subprocess

if platform.system() == "Darwin":
    environment = dict(os.environ)
    paths = []
    for package in ("icu4c", "sqlite", "curl"):
        prefix = subprocess.check_output(["brew", "--prefix", package], text=True).strip()
        paths.append(str(Path(prefix) / "lib/pkgconfig"))
    environment["PKG_CONFIG_PATH"] = os.pathsep.join(paths + [environment.get("PKG_CONFIG_PATH", "")])
    cflags = shlex.split(subprocess.check_output(
        ["pkg-config", "--cflags", "icu-uc", "sqlite3", "libcurl"], env=environment, text=True))
    libraries = shlex.split(subprocess.check_output(
        ["pkg-config", "--libs", "icu-uc", "sqlite3", "libcurl"], env=environment, text=True))
else:
    # Standard development packages, or the VM's explicit include/library paths.
    cflags = []
    libraries = ["-licuuc", "-lsqlite3", "-lcurl"]

for filename, flags in (("host_cflags.sexp", cflags), ("host_libraries.sexp", libraries)):
    Path(filename).write_text("(" + " ".join(json.dumps(flag) for flag in flags) + ")\n")
