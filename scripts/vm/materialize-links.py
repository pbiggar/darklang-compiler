#!/usr/bin/env python3
"""Preserve toolchain archive links as regular files in the VM filesystem."""
import pathlib
import posixpath
import shutil
import sys
import tarfile

archive_path, destination = sys.argv[1:]
root = pathlib.Path(destination).resolve()
with tarfile.open(archive_path) as archive:
    entries = {posixpath.normpath(m.name): m for m in archive.getmembers()}

for name, member in entries.items():
    if not (member.issym() or member.islnk()):
        continue
    target = name
    seen = set()
    while entries[target].issym() or entries[target].islnk():
        if target in seen:
            raise ValueError(f"Archive link cycle: {name}")
        seen.add(target)
        link = entries[target]
        target = posixpath.normpath(
            posixpath.join(posixpath.dirname(target), link.linkname)
            if link.issym() else link.linkname
        )
    source, output = root / target, root / name
    if not source.resolve().is_relative_to(root) or not output.parent.resolve().is_relative_to(root):
        raise ValueError(f"Archive link escapes destination: {name}")
    if output.is_symlink():
        output.unlink()
    shutil.copyfile(source, output)
    output.chmod(entries[target].mode & 0o777)
