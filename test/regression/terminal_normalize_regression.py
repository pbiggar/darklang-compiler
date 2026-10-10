#!/usr/bin/env python3
"""Compile and run the original upstream Frame row renderer against embedded Text."""
import pathlib
import subprocess
import sys
import tempfile


def main():
    compiler = str(pathlib.Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix="dark-terminal-normalize-") as directory:
        root = pathlib.Path(directory)
        source = root / "frame.dark"
        binary = root / "frame"
        # RowChange and renderRowChangeAtWidth from interpreter v0.0.35.
        source.write_text('''module Darklang.Stdlib.Cli.Tui.Frame
type RowChange = { row: Int; content: String }
let renderRowChangeAtWidth
  (width: Int)
  (change: RowChange)
  : String =
  let terminalRow = Stdlib.Int.toString (change.row + 1)
  let content = Text.normalizeToWidth change.content width
  $"\\u001b[{terminalRow};1H\\u001b[0m\\u001b[K{content}\\u001b[0m\\r"

let first = renderRowChangeAtWidth 3 (RowChange { row = 0; content = "hello" }) in
let second = renderRowChangeAtWidth 10 (RowChange { row = 1; content = "a\\r\\nb" }) in
let third = renderRowChangeAtWidth 1 (RowChange { row = 2; content = "中" }) in
let _ = Builtin.print (first ++ second ++ third) in
0L
''', encoding="utf-8")
        subprocess.run([compiler, "-q", "--leak-check", str(source), "-o", str(binary)],
                       check=True, timeout=120)
        expected = b"".join(
            f"\x1b[{row};1H\x1b[0m\x1b[K{content}\x1b[0m\r".encode()
            for row, content in [(1, "hel"), (2, "ab"), (3, "")])
        result = subprocess.run([str(binary)], capture_output=True, timeout=15)
        assert (result.returncode, result.stdout, result.stderr) == (0, expected, b""), result
    print("Original Frame row renderer compiles, normalizes and runs without leaks")


if __name__ == "__main__":
    main()
