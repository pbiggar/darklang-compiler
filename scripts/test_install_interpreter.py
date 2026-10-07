#!/usr/bin/env python3
"""Verify release selection with mixed 32-bit ARM, ARM64 and x64 assets."""
import json
import os
from pathlib import Path
import subprocess
import tempfile

REPO = Path(__file__).resolve().parent.parent
INSTALLER = REPO / "scripts/install-darklang-interpreter.sh"

def verify(architecture, expected):
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        tools = root / "bin"
        tools.mkdir()
        names = [
            "darklang-alpha-test-linux-arm.gz",
            "darklang-alpha-test-linux-arm64.gz",
            "darklang-alpha-test-linux-musl-x64.gz",
            "darklang-alpha-test-linux-x64.gz",
        ]
        release = [{"tag_name": "test", "published_at": "2026-01-01",
                    "assets": [{"name": name, "browser_download_url": "https://fixture/" + name}
                               for name in names]}]
        (tools / "releases.json").write_text(json.dumps(release))
        (tools / "uname").write_text("#!/bin/sh\nprintf '%s\\n' " + architecture + "\n")
        (tools / "curl").write_text("""#!/usr/bin/env python3
import gzip
from pathlib import Path
import sys
url = sys.argv[-1]
if "/releases?" in url:
    sys.stdout.write(Path(__file__).with_name("releases.json").read_text())
elif url.startswith("https://fixture/"):
    name = url.rsplit("/", 1)[1]
    sys.stdout.buffer.write(gzip.compress(("#!/bin/sh\\nprintf '%s\\\\n' " + name + "\\n").encode()))
else:
    raise SystemExit("Unexpected fixture request: " + url)
""")
        for name in ["uname", "curl"]:
            (tools / name).chmod(0o755)
        environment = dict(os.environ, PATH=str(tools) + os.pathsep + os.environ["PATH"])
        output = root / "oracle"
        result = subprocess.run(["bash", str(INSTALLER), "--output", str(output)],
                                env=environment, capture_output=True, text=True, check=True)
        assert ("Asset: " + expected) in result.stdout, result.stdout
        assert os.access(output, os.X_OK)
        actual = subprocess.check_output([str(output)], text=True).strip()
        assert actual == expected, (architecture, actual, expected)

for architecture in ["aarch64", "arm64"]:
    verify(architecture, "darklang-alpha-test-linux-arm64.gz")
for architecture in ["x86_64", "amd64"]:
    verify(architecture, "darklang-alpha-test-linux-x64.gz")
print("Interpreter installer selects native ARM64/x64 assets and excludes 32-bit ARM/musl")
