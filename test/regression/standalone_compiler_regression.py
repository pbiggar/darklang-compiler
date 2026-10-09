#!/usr/bin/env python3
"""Compile with a copied executable and no adjacent repository or data files."""
from pathlib import Path
import json
import os
import platform
import shutil
import subprocess
import sys
import tempfile


def main():
    compiler = Path(sys.argv[1]).resolve()
    version = subprocess.check_output([str(compiler), "--version"], text=True)
    build_hash = next(line.removeprefix("Build: ") for line in version.splitlines()
                      if line.startswith("Build: "))
    if platform.system() == "Linux":
        dependencies = subprocess.check_output(["readelf", "-d", str(compiler)], text=True)
    else:
        dependencies = subprocess.check_output(["otool", "-L", str(compiler)], text=True)
    assert "libsqlite3" not in dependencies and "libgmp" not in dependencies, dependencies
    environment = dict(os.environ)
    environment.pop("LD_LIBRARY_PATH", None)
    with tempfile.TemporaryDirectory(prefix="dark-standalone-") as temporary:
        directory = Path(temporary)
        binary = directory / "dark"
        shutil.copy2(compiler, binary)
        environment["PATH"] = str(directory / "no-tools")
        copied_version = subprocess.check_output([str(binary), "--version"],
                                                cwd=directory, env=environment, text=True)
        assert copied_version == version, copied_version
        source = directory / "input.dark"
        source.write_text('let length = Stdlib.String.length "hé🚀" in\n'
                          'Stdlib.Int.toString length ++ "|" ++ Builtin.getBuildHash ()\n', encoding="utf-8")
        output = directory / "program"
        result = subprocess.run(
            [str(binary), "-q", "--emit-result", str(source), "-o", str(output)],
            cwd=directory, env=environment, capture_output=True, text=True, timeout=120, check=False)
        assert result.returncode == 0, result.stdout + result.stderr
        execution = subprocess.run([str(output)], cwd=directory, env=environment,
                                   capture_output=True, text=True, timeout=10, check=False)
        assert execution.returncode == 0, execution.stderr
        assert json.loads(execution.stdout) == f"3|{build_hash}", execution.stdout
    print("Copied compiler embeds its library, Unicode data and build identity without Git")


if __name__ == "__main__":
    main()
