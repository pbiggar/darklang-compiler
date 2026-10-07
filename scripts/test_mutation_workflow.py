#!/usr/bin/env python3
"""Exercise mutation discovery, exact application, restoration and empty selections."""
import importlib.util
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("ocaml_sites", ROOT / "mutation/ocaml_sites.py")
scanner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(scanner)


class MutationTests(unittest.TestCase):
    def test_literals_nested_comments_and_custom_operators(self):
        source = '''(* outer + (* inner - *) "*)" *)
let text = " + " and quoted = {tag| >= |tag}
let character = '+' and escaped = '\\''
let add x y = x + y (* ignored + *)
let float_add x y = x +. y
let choose x y = x <= y && x <> y
'''
        self.assertEqual([(name, line) for name, line, _, _ in scanner.sites(source)],
                         [("ARITH_ADD", 4), ("CMP_LTE", 6), ("LOGIC_AND", 6), ("CMP_NEQ", 6)])

    def test_apply_changes_code_after_literal_and_comment(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "Example.ml"
            path.write_text('let add x y = ignore " + "; (* + *) x + y\n')
            scanner.apply("ARITH_ADD", path, 1)
            self.assertEqual(path.read_text(), 'let add x y = ignore " + "; (* + *) x - y\n')
            with self.assertRaises(ValueError):
                scanner.apply("ARITH_ADD", path, 1)

    def test_shell_workflow_discovers_src_and_restores_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(ROOT / "mutation", root / "mutation",
                            ignore=shutil.ignore_patterns("results", "__pycache__"))
            (root / "src").mkdir()
            source = root / "src/Example.ml"
            original = "let add x y = x + y\n"
            source.write_text(original)
            runner = root / "_build/default/test/tests_main.exe"
            runner.parent.mkdir(parents=True)
            runner.touch()
            runner.chmod(0o755)
            for name, content in {
                "build": '#!/bin/sh\nexit 0\n',
                "run-tests": '#!/bin/sh\ngrep -q "x + y" "$(dirname "$0")/src/Example.ml"\n',
            }.items():
                path = root / name
                path.write_text(content)
                path.chmod(0o755)
            command = ["bash", str(root / "mutation/mutation-test.sh")]
            discovery = subprocess.run(command + ["--dry-run"], capture_output=True, text=True)
            self.assertEqual(discovery.returncode, 0, discovery.stderr)
            self.assertIn("ARITH_ADD", discovery.stdout)
            run = subprocess.run(command + ["--limit=1"], capture_output=True, text=True)
            self.assertEqual(run.returncode, 0, run.stderr)
            self.assertIn("KILLED", (root / "mutation/results/results.csv").read_text())
            self.assertEqual(source.read_text(), original)
            self.assertFalse(source.with_suffix(".ml.mutation_backup").exists())
            empty = subprocess.run(command + ["--dry-run", "--file=missing"], capture_output=True, text=True)
            self.assertNotEqual(empty.returncode, 0)


if __name__ == "__main__":
    unittest.main()
