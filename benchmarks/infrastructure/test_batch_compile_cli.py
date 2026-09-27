#!/usr/bin/env python3
"""End-to-end coverage for compiling independent programs in one CLI process."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path


class BatchCompileCliTests(unittest.TestCase):
    def test_batch_compile_emits_independent_executables(self) -> None:
        project_root = Path(__file__).resolve().parents[2]
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            first_source = root / "first.dark"
            second_source = root / "second.dark"
            first_output = root / "first"
            second_output = root / "second"
            first_source.write_text(
                'let message () : String = "first"\nBuiltin.printLine (message ())\n'
            )
            second_source.write_text(
                'let message () : String = "second"\nBuiltin.printLine (message ())\n'
            )

            compiled = subprocess.run(
                [
                    project_root / "dark",
                    "--batch",
                    "--leak-check",
                    "--quiet",
                    "--",
                    first_source,
                    first_output,
                    second_source,
                    second_output,
                ],
                cwd=project_root,
                capture_output=True,
                text=True,
            )

            self.assertEqual(compiled.returncode, 0, compiled.stdout + compiled.stderr)
            first = subprocess.run(
                [first_output], capture_output=True, text=True, check=False
            )
            second = subprocess.run(
                [second_output], capture_output=True, text=True, check=False
            )
            self.assertEqual(first.returncode, 0, first.stderr)
            self.assertEqual(first.stdout, "first\n")
            self.assertEqual(first.stderr, "")
            self.assertEqual(second.returncode, 0, second.stderr)
            self.assertEqual(second.stdout, "second\n")
            self.assertEqual(second.stderr, "")


if __name__ == "__main__":
    unittest.main()
