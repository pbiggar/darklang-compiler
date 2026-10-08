#!/usr/bin/env python3
"""Check compiled terminal facts against real descriptors and environment changes."""
import os
import pathlib
import pty
import subprocess
import sys
import tempfile
import termios


def main():
    compiler = str(pathlib.Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix="dark-terminal-") as directory:
        root = pathlib.Path(directory)
        source = root / "facts.dark"
        binary = root / "facts"
        source.write_text('''let facts = Stdlib.Cli.Tui.TerminalSupport.currentFacts () in
let report = $"{Stdlib.Bool.toString facts.inputIsTerminal}|{Stdlib.Bool.toString facts.outputIsTerminal}|{facts.terminalName}" in
let _ = Stdlib.Cli.Posix.fdWrite 2 (Stdlib.String.toBlob report) in
0L
''', encoding="utf-8")
        subprocess.run([compiler, "-q", "--leak-check", str(source), "-o", str(binary)], check=True, timeout=120)
        master, slave = pty.openpty()
        try:
            original_attributes = termios.tcgetattr(slave)
            for terminal_in, terminal_out in [(False, False), (True, False), (False, True), (True, True)]:
                for term in [None, "", "xterm-256color", "term-λ"]:
                    environment = os.environ.copy()
                    environment.pop("TERM", None)
                    if term is not None:
                        environment["TERM"] = term
                    result = subprocess.run(
                        [str(binary)], stdin=slave if terminal_in else subprocess.DEVNULL,
                        stdout=slave if terminal_out else subprocess.PIPE, stderr=subprocess.PIPE,
                        env=environment, timeout=15, check=False)
                    expected = f"{str(terminal_in).lower()}|{str(terminal_out).lower()}|{term or ''}".encode()
                    assert result.returncode == 0 and result.stderr == expected, (terminal_in, terminal_out, term, result)
            assert termios.tcgetattr(slave) == original_attributes, "terminal settings changed"
            # A regular file, a pipe and closed descriptors must never count as terminals.
            environment = dict(os.environ, TERM="dumb")
            with (root / "regular").open("w+b") as regular:
                result = subprocess.run([str(binary)], stdin=regular, stdout=regular,
                                        stderr=subprocess.PIPE, env=environment, timeout=15)
                assert result.returncode == 0 and result.stderr == b"false|false|dumb", result
            result = subprocess.run([str(binary)], input=b"", capture_output=True,
                                    env=environment, timeout=15)
            assert result.returncode == 0 and result.stderr == b"false|false|dumb", result
            def close_streams():
                os.close(0)
                os.close(1)
            result = subprocess.run([str(binary)], preexec_fn=close_streams,
                                    stderr=subprocess.PIPE, env=environment, timeout=15)
            assert result.returncode == 0 and result.stderr == b"false|false|dumb", result
        finally:
            os.close(slave)
            os.close(master)
    print("Terminal session descriptor and environment regressions passed")


if __name__ == "__main__":
    main()
