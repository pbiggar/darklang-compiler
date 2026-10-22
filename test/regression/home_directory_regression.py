#!/usr/bin/env python3
"""Exercise shared HOME precedence and real-UID passwd fallback in native code."""
import os
import pathlib
import subprocess
import sys
import tempfile


def main():
    compiler = str(pathlib.Path(sys.argv[1]).resolve())
    uid = str(os.getuid())
    fallback = "none"
    for line in pathlib.Path("/etc/passwd").read_text().splitlines():
        fields = line.split(":")
        if len(fields) > 5 and fields[2] == uid:
            fallback = "some:" + fields[5]
            break
    with tempfile.TemporaryDirectory(prefix="dark-home-") as directory:
        root = pathlib.Path(directory)
        source = root / "home.dark"
        binary = root / "home"
        source.write_text('''let report =
  match Stdlib.Cli.Env.home () with
  | Some home -> "some:" + home
  | None -> "none"
in let _ = Builtin.print report in 0L
''', encoding="utf-8")
        subprocess.run([compiler, "-q", "--leak-check", str(source), "-o", str(binary)],
                       check=True, timeout=120)
        for home, expected in [("/explicit/雪", "some:/explicit/雪"), ("", "some:"),
                               (None, fallback)]:
            environment = dict(os.environ, USER="not-the-real-user", LOGNAME="root")
            if home is None:
                environment.pop("HOME", None)
            else:
                environment["HOME"] = home
            result = subprocess.run([str(binary)], env=environment, capture_output=True,
                                    timeout=15)
            assert (result.returncode, result.stdout, result.stderr) == (
                0, expected.encode(), b""), result
    print("Shared home uses HOME (including empty), then real-UID /etc/passwd")


if __name__ == "__main__":
    main()
