"""test_fuzz_reducer.py - Check structural minimization against a stable compiler failure."""

from pathlib import Path
import os
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
FUZZER = ROOT / "bin/Fuzzer/Debug/net11.0/Fuzzer.dll"


class FuzzReducerTests(unittest.TestCase):
    def test_pops_expressions_and_dependent_declarations(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            bin_directory = directory / "bin"
            bin_directory.mkdir()
            interpreter = bin_directory / "darklang-interpreter"
            interpreter.write_text("#!/bin/sh\nprintf '0\\n'\n", encoding="utf-8")
            interpreter.chmod(0o755)

            source = directory / "finding.dark"
            source.write_text(
                "type FuzzBox = { value: Int64, flag: Bool }\n"
                "let fuzzIdentity (input: Int64) : Bool = input\n"
                "let fuzzEven (count: Int64) : Bool = "
                "if count <= 0L then true else fuzzOdd (count - 1L)\n"
                "let fuzzOdd (count: Int64) : Bool = "
                "if count <= 0L then false else fuzzEven (count - 1L)\n"
                "if true then 4L + 1L else 99L\n",
                encoding="utf-8",
            )

            environment = dict(os.environ)
            environment["PATH"] = f"{bin_directory}:{environment['PATH']}"
            result = subprocess.run(
                [str(ROOT / "scripts/dotnet-host"), str(FUZZER),
                 "--minimize", str(source), "--timeout-ms", "3000",
                 "--artifacts", str(directory / "artifacts")],
                cwd=ROOT, env=environment, text=True, capture_output=True, timeout=90,
            )

            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("Preserved failure: compiler rejected", result.stdout)
            reduced = source.with_suffix(".min.dark").read_text(encoding="utf-8").strip()
            self.assertEqual(
                reduced,
                "let fuzzIdentity (input: Int64) : Bool = input\n4L",
            )


if __name__ == "__main__":
    unittest.main()
