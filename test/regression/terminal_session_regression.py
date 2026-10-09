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
        color_source = root / "color.dark"
        color_binary = root / "color"
        color_source.write_text('''let initial = Darklang.Cli.Terminal.colorEnabled () in
let _ = Stdlib.Cli.Env.set "NO_COLOR" "1" in
let blocked = Darklang.Cli.Terminal.colorEnabled () in
let _ = Stdlib.Cli.Env.set "NO_COLOR" "" in
let empty = Darklang.Cli.Terminal.colorEnabled () in
let _ = Stdlib.Cli.Env.unset "NO_COLOR" in
let unset = Darklang.Cli.Terminal.colorEnabled () in
let report = $"{Stdlib.Bool.toString initial}|{Stdlib.Bool.toString blocked}|{Stdlib.Bool.toString empty}|{Stdlib.Bool.toString unset}" in
let _ = Stdlib.Cli.Posix.fdWrite 2 (Stdlib.String.toBlob report) in
0L
''', encoding="utf-8")
        subprocess.run([compiler, "-q", "--leak-check", str(color_source), "-o", str(color_binary)], check=True, timeout=120)
        interactive_source = root / "interactive.dark"
        interactive_binary = root / "interactive"
        interactive_source.write_text('''let raw = Builtin.stdinIsInteractive () in
let public = Stdlib.Cli.Stdin.isInteractive () in
let report = $"{Stdlib.Bool.toString raw}|{Stdlib.Bool.toString public}" in
let _ = Stdlib.Cli.Posix.fdWrite 2 (Stdlib.String.toBlob report) in
0L
''', encoding="utf-8")
        subprocess.run([compiler, "-q", "--leak-check", str(interactive_source), "-o", str(interactive_binary)], check=True, timeout=120)
        master, slave = pty.openpty()
        try:
            original_attributes = termios.tcgetattr(slave)
            for terminal_in, terminal_out in [(False, False), (True, False), (False, True), (True, True)]:
                result = subprocess.run(
                    [str(interactive_binary)], stdin=slave if terminal_in else subprocess.DEVNULL,
                    stdout=slave if terminal_out else subprocess.PIPE, stderr=subprocess.PIPE,
                    env=dict(os.environ, TERM="", NO_COLOR="1"), timeout=15)
                expected = b"true|true" if terminal_in or terminal_out else b"false|false"
                assert result.returncode == 0 and result.stderr == expected, (terminal_in, terminal_out, result)
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
                for no_color in [None, "", "0", " ", "λ"]:
                    environment = dict(os.environ, TERM="dumb")
                    environment.pop("NO_COLOR", None)
                    if no_color is not None:
                        environment["NO_COLOR"] = no_color
                    result = subprocess.run(
                        [str(color_binary)], stdin=slave if terminal_in else subprocess.DEVNULL,
                        stdout=slave if terminal_out else subprocess.PIPE, stderr=subprocess.PIPE,
                        env=environment, timeout=15)
                    initial = terminal_out and not no_color
                    expected = f"{str(initial).lower()}|false|{str(terminal_out).lower()}|{str(terminal_out).lower()}".encode()
                    assert result.returncode == 0 and result.stderr == expected, (terminal_in, terminal_out, no_color, result)
            assert termios.tcgetattr(slave) == original_attributes, "terminal settings changed"
            # A regular file, a pipe and closed descriptors must never count as terminals.
            environment = dict(os.environ, TERM="dumb")
            with (root / "regular").open("w+b") as regular:
                result = subprocess.run([str(binary)], stdin=regular, stdout=regular,
                                        stderr=subprocess.PIPE, env=environment, timeout=15)
                assert result.returncode == 0 and result.stderr == b"false|false|dumb", result
                result = subprocess.run([str(color_binary)], stdin=regular, stdout=regular,
                                        stderr=subprocess.PIPE, env=environment, timeout=15)
                assert result.returncode == 0 and result.stderr == b"false|false|false|false", result
                result = subprocess.run([str(interactive_binary)], stdin=regular, stdout=regular,
                                        stderr=subprocess.PIPE, timeout=15)
                assert result.returncode == 0 and result.stderr == b"false|false", result
            result = subprocess.run([str(binary)], input=b"", capture_output=True,
                                    env=environment, timeout=15)
            assert result.returncode == 0 and result.stderr == b"false|false|dumb", result
            result = subprocess.run([str(interactive_binary)], input=b"", capture_output=True, timeout=15)
            assert result.returncode == 0 and result.stderr == b"false|false", result
            def close_streams():
                os.close(0)
                os.close(1)
            result = subprocess.run([str(interactive_binary)], preexec_fn=close_streams,
                                    stderr=subprocess.PIPE, timeout=15)
            assert result.returncode == 0 and result.stderr == b"false|false", result
            result = subprocess.run([str(color_binary)], preexec_fn=close_streams,
                                    stderr=subprocess.PIPE, env=environment, timeout=15)
            assert result.returncode == 0 and result.stderr == b"false|false|false|false", result
            result = subprocess.run([str(binary)], preexec_fn=close_streams,
                                    stderr=subprocess.PIPE, env=environment, timeout=15)
            assert result.returncode == 0 and result.stderr == b"false|false|dumb", result
        finally:
            os.close(slave)
            os.close(master)
    print("Terminal session descriptor and environment regressions passed")


if __name__ == "__main__":
    main()
