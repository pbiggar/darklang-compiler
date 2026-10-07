#!/usr/bin/env python3
"""Restore snapshot-pinned Ubuntu native prerequisites inside a writable workspace."""

from concurrent.futures import ThreadPoolExecutor
import hashlib
import lzma
from pathlib import Path
import subprocess
import sys
import time
import urllib.error
import urllib.request

BASE = "https://snapshot.ubuntu.com/ubuntu/20260828T000000Z/"
INDEXES = [
    ("ubuntu-base-packages.xz", "noble", "main"),
    ("ubuntu-packages.xz", "noble-updates", "main"),
    ("ubuntu-universe-packages.xz", "noble", "universe"),
]
# Keep runtime libraries alongside headers: fresh VMs need both. QEMU shares
# this sysroot, but is built only when explicitly requested.
NAMES = [
    "libgmp-dev", "libgmp10", "libsqlite3-dev", "libsqlite3-0", "shellcheck",
    "libglib2.0-dev", "libglib2.0-dev-bin", "libglib2.0-0t64", "libffi-dev", "libffi8",
    "libpcre2-dev", "libpcre2-8-0", "libpcre2-16-0", "libpcre2-32-0", "libpcre2-posix3",
    "libmount-dev", "libmount1", "libblkid-dev", "libblkid1", "zlib1g-dev", "zlib1g",
    "libpkgconf3", "pkgconf-bin", "pkgconf", "pkg-config",
]


def download(url, path, validate):
    """Replace a cache entry atomically, accepting it only after validation."""
    if path.is_file():
        try:
            validate(path.read_bytes())
            return
        except (ValueError, lzma.LZMAError, EOFError):
            pass
    temporary = path.with_name(path.name + ".download")
    for attempt in range(3):
        try:
            with urllib.request.urlopen(url, timeout=60) as response:
                data = response.read()
            validate(data)
            temporary.write_bytes(data)
            temporary.replace(path)
            return
        except (urllib.error.URLError, TimeoutError, ValueError, lzma.LZMAError, EOFError):
            if attempt == 2:
                raise
            time.sleep(attempt + 1)


def restore(root):
    downloads = root / "downloads"
    sysroot = root / "qemu-deps/sysroot"
    downloads.mkdir(parents=True, exist_ok=True)
    sysroot.mkdir(parents=True, exist_ok=True)
    packages = {}
    for filename, suite, component in INDEXES:
        path = downloads / filename
        download(BASE + f"dists/{suite}/{component}/binary-amd64/Packages.xz", path,
                 lzma.decompress)
        for record in lzma.decompress(path.read_bytes()).decode().split("\n\n"):
            fields = dict(line.split(": ", 1) for line in record.splitlines()
                          if ": " in line and not line.startswith(" "))
            if "Package" in fields:
                packages[fields["Package"]] = fields

    def fetch(name):
        package = packages[name]
        archive = downloads / Path(package["Filename"]).name

        def validate(data):
            if hashlib.sha256(data).hexdigest() != package["SHA256"]:
                raise ValueError("package checksum mismatch: " + name)

        download(BASE + package["Filename"], archive, validate)
        return name, package["Version"], archive

    with ThreadPoolExecutor(max_workers=4) as executor:
        results = list(executor.map(fetch, NAMES))
    for name, version, archive in results:
        subprocess.run(["dpkg-deb", "-x", str(archive), str(sysroot)], check=True)
        print(name, version, flush=True)
    # Preserve the extracted snapshot libraries; do not overwrite them with
    # whichever library happened to be present in this VM's base image.


if __name__ == "__main__":
    if len(sys.argv) != 2 or not Path(sys.argv[1]).is_absolute():
        sys.exit("Usage: python3 scripts/vm/setup-native-packages.py ABSOLUTE_TOOLCHAIN_DIRECTORY")
    restore(Path(sys.argv[1]))
