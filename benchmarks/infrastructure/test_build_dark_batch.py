#!/usr/bin/env python3
"""Integration coverage for one-process Dark benchmark compilation."""

from __future__ import annotations

import shlex
import subprocess
import tempfile
import unittest
from pathlib import Path


class BuildDarkBatchTests(unittest.TestCase):
    def test_all_benchmarks_use_one_compiler_invocation(self) -> None:
        infrastructure = Path(__file__).resolve().parent
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            invocation_log = root / "invocations"
            quoted_log = shlex.quote(str(invocation_log))
            compiler = root / "compiler"
            compiler.write_text(
                "#!/bin/bash\n"
                f"printf 'called\\n' >> {quoted_log}\n"
                "if [[ \" $* \" != *\" --allow-internal \"* ]]; then exit 42; fi\n"
                "while [[ $# -gt 0 && \"$1\" != -- ]]; do shift; done\n"
                "shift\n"
                "while [[ $# -gt 0 ]]; do\n"
                "  source_path=$1\n"
                "  output_path=$2\n"
                "  printf '%s\\t%s\\n' \"$source_path\" \"$output_path\" >> "
                f"{quoted_log}\n"
                "  printf '#!/bin/sh\\n' > \"$output_path\"\n"
                "  shift 2\n"
                "done\n"
            )
            compiler.chmod(0o755)

            completed = subprocess.run(
                [
                    infrastructure / "build_dark_batch.sh",
                    f"--compiler={compiler}",
                    f"--output-dir={root / 'outputs'}",
                    "ackermann",
                    "fib",
                ],
                capture_output=True,
                text=True,
                check=False,
            )

            self.assertEqual(completed.returncode, 0, completed.stdout + completed.stderr)
            lines = invocation_log.read_text().splitlines()
            self.assertEqual(lines[0], "called")
            self.assertEqual(len(lines), 3)
            self.assertIn("ackermann/dark/main.dark", lines[1])
            self.assertIn("fib/dark/main.dark", lines[2])
            self.assertTrue((root / "outputs" / "ackermann" / "dark" / "main").exists())
            self.assertTrue((root / "outputs" / "fib" / "dark" / "main").exists())


if __name__ == "__main__":
    unittest.main()
