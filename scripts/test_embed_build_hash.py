#!/usr/bin/env python3
"""Check compiler build identity stamping, refresh, archive fallback and validation."""
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class BuildHashTests(unittest.TestCase):
    def test_stamps_both_consumers_and_refreshes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "StdLib/Builtin/__BuildInfo.dark"
            source.parent.mkdir(parents=True)
            source.write_text('module Builtin\nlet getBuildHash () : String = "__COMPILER_BUILD_HASH__"\n')
            (root / "packages").mkdir()
            (root / "library-sources.list").write_text("StdLib/Builtin/__BuildInfo.dark\n")
            for argument, expected in [("0123456", "0123456"), ("abcdef0", "abcdef0"), ("", "dev")]:
                result = subprocess.run([sys.executable, str(Path(__file__).with_name("embed-stdlib.py")),
                                         str(root), str(root / "EmbeddedStdlib.ml"),
                                         str(root / "BuildInfo.ml"), argument],
                                        capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn(expected, (root / "EmbeddedStdlib.ml").read_text())
                self.assertIn(f'let hash = "{expected}"', (root / "BuildInfo.ml").read_text())
                self.assertNotIn("__COMPILER_BUILD_HASH__", (root / "EmbeddedStdlib.ml").read_text())
            result = subprocess.run([sys.executable, str(Path(__file__).with_name("embed-stdlib.py")),
                                     str(root), str(root / "EmbeddedStdlib.ml"),
                                     str(root / "BuildInfo.ml"), 'bad"metadata'],
                                    capture_output=True, text=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("Invalid compiler build hash", result.stderr)


if __name__ == "__main__":
    unittest.main()
