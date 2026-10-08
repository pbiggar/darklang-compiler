#!/usr/bin/env python3
"""Check native terminal dimensions, descriptor precedence and fallback behavior."""
import fcntl
import os
import pathlib
import pty
import struct
import subprocess
import sys
import tempfile
import termios


def main():
    compiler = str(pathlib.Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix="dark-terminal-size-") as directory:
        root = pathlib.Path(directory)
        source = root / "size.dark"
        binary = root / "size"
        source.write_text('''let (columns, rows) = Darklang.Cli.Terminal.getSize () in
let report = $"{Stdlib.Int.toString columns}|{Stdlib.Int.toString rows}" in
let _ = Stdlib.Cli.Posix.fdWrite 3 (Stdlib.String.toBlob report) in
0L
''', encoding="utf-8")
        subprocess.run([compiler, "-q", "--leak-check", str(source), "-o", str(binary)],
                       check=True, timeout=120)
        terminals = [pty.openpty() for _ in range(3)]
        try:
            sizes = [(101, 31), (202, 42), (303, 53)]
            for (_, slave), (columns, rows) in zip(terminals, sizes):
                fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))
            attributes = [termios.tcgetattr(slave) for _, slave in terminals]

            def check(streams, expected, dimensions=None):
                environment = os.environ.copy()
                environment.pop("COLUMNS", None)
                environment.pop("LINES", None)
                environment.update(dimensions or {})
                # Keep the report independent of all three descriptors under test.
                with (root / "report").open("w+b") as report:
                    def setup():
                        os.dup2(report.fileno(), 3)
                    result = subprocess.run([str(binary)], stdin=streams[0], stdout=streams[1],
                                            stderr=streams[2], env=environment,
                                            pass_fds=(report.fileno(), 3), preexec_fn=setup,
                                            timeout=15)
                    report.seek(0)
                    actual = report.read()
                assert result.returncode == 0 and actual == expected.encode(), (streams, expected, actual, result)

            slaves = [slave for _, slave in terminals]
            for mask in range(8):
                streams = [slaves[i] if mask & (1 << i) else subprocess.DEVNULL for i in range(3)]
                selected = next((i for i in [1, 0, 2] if mask & (1 << i)), None)
                expected = "80|24" if selected is None else f"{sizes[selected][0]}|{sizes[selected][1]}"
                check(streams, expected)
            check(slaves, "202|42", {"COLUMNS": "999", "LINES": "888"})
            # Zero dimensions on stdout must allow a usable stdin to win.
            fcntl.ioctl(slaves[1], termios.TIOCSWINSZ, struct.pack("HHHH", 0, 202, 0, 0))
            check(slaves, "101|31")
            fcntl.ioctl(slaves[0], termios.TIOCSWINSZ, struct.pack("HHHH", 65535, 40000, 0, 0))
            check([slaves[0], subprocess.DEVNULL, subprocess.DEVNULL], "40000|65535")
            for dimensions, expected in [
                ({"COLUMNS": "132", "LINES": "43"}, "132|43"),
                ({"COLUMNS": " +81 ", "LINES": "25"}, "81|25"),
                ({"COLUMNS": "0", "LINES": "-1"}, "80|24"),
                ({"COLUMNS": "invalid", "LINES": "2147483648"}, "80|24"),
                ({"COLUMNS": "2147483647", "LINES": ""}, "2147483647|24"),
                ({"COLUMNS": "90"}, "90|24"),
            ]:
                check([subprocess.DEVNULL] * 3, expected, dimensions)
            assert [termios.tcgetattr(slave) for slave in slaves] == attributes, "terminal settings changed"
        finally:
            for master, slave in terminals:
                os.close(slave)
                os.close(master)
    print("Terminal size descriptor and fallback regressions passed")


if __name__ == "__main__":
    main()
