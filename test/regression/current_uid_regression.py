#!/usr/bin/env python3
"""Compare the compiled interpreter UID builtin with the launching OS process."""
import os
import pathlib
import subprocess
import sys
import tempfile


def main():
    compiler = str(pathlib.Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix="dark-current-uid-") as directory:
        root = pathlib.Path(directory)
        source = root / "uid.dark"
        binary = root / "uid"
        source.write_text('''let isRoot () : Bool = (Builtin.posixGetuid ()) == 0
let report = $"{Stdlib.Int.toString (Builtin.posixGetuid ())}|{Stdlib.Bool.toString (isRoot ())}|{Stdlib.Bool.toString (Stdlib.Cli.Sys.isRoot ())}" in
let _ = Builtin.print report in
0L
''', encoding="utf-8")
        subprocess.run([compiler, "-q", "--leak-check", str(source), "-o", str(binary)],
                       check=True, timeout=120)
        expected = f"{os.getuid()}|{str(os.getuid() == 0).lower()}|{str(os.getuid() == 0).lower()}".encode()
        for user in ["root", "definitely-not-the-current-user"]:
            environment = dict(os.environ, USER=user, LOGNAME=user)
            result = subprocess.run([str(binary)], env=environment, capture_output=True, timeout=15)
            assert (result.returncode, result.stdout, result.stderr) == (0, expected, b""), result
    print("Compiled real UID matches the OS and ignores USER/LOGNAME")


if __name__ == "__main__":
    main()
