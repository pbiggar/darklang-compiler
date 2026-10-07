#!/usr/bin/env python3
"""Check the VM adapter's narrowly scoped Dune test-stamp permissions."""
from pathlib import Path
import os
import subprocess
import sys
import tempfile


CHECK = '''
from pathlib import Path
import os
root = Path("_build/.actions/default/test/regression")
root.mkdir(parents=True)
stamp = root / ("runtest-" + "a" * 32)
stamp.touch()
stamp.chmod(0o444)
expected = 0o644 if os.environ.get("PORT_VM_DUNE_TEST_STAMPS") == "1" else 0o444
assert stamp.stat().st_mode & 0o777 == expected
if expected == 0o644:
    stamp.write_bytes(b"")
    stamp.chmod(0o444)
    stamp.write_bytes(b"")
fd = os.open(stamp, os.O_RDONLY)
try:
    # Simulate a stamp produced before this adapter existed.
    os.fchmod(fd, 0o444)
    if expected == 0o644:
        stamp.resolve().write_bytes(b"")
    stamp.unlink()
    if expected == 0o644:
        assert os.fstat(fd).st_mode & 0o777 == expected
finally:
    os.close(fd)
for name, data in [("ordinary", b""), ("runtest-" + "b" * 32, b"content"),
                   ("runtest-invalid", b"")]:
    path = root / name
    path.write_bytes(data)
    path.chmod(0o444)
    try:
        path.write_bytes(data)
    except PermissionError:
        pass
    assert path.stat().st_mode & 0o777 == 0o444
outside = Path("runtest-" + "c" * 32)
outside.touch()
outside.chmod(0o444)
assert outside.stat().st_mode & 0o777 == 0o444
link = root / ("runtest-" + "d" * 32)
link.symlink_to(outside.resolve())
link.chmod(0o444)
try:
    link.write_bytes(b"")
except PermissionError:
    pass
assert outside.stat().st_mode & 0o777 == 0o444
'''


def main():
    adapter = Path(sys.argv[1]).resolve()
    for enabled in ("0", "1"):
        with tempfile.TemporaryDirectory(prefix="dark-vm-compat-") as temporary:
            environment = dict(os.environ, LD_PRELOAD=str(adapter),
                               PORT_VM_DUNE_TEST_STAMPS=enabled)
            subprocess.run([sys.executable, "-c", CHECK], cwd=temporary,
                           env=environment, check=True, timeout=30)
    print("VM Dune stamp permissions and exclusions passed")


if __name__ == "__main__":
    main()
