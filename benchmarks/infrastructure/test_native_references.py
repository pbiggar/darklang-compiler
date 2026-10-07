"""Adapter boundary tests without requiring GHC, Roc, Koka, or the interpreter."""

import tempfile
import contextlib
import io
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

from diagnostic_references import measure_one
from native_references import build_flags, measure_native, roc_mode, version_command
from reference_cli import preflight, refresh
from reference_snapshots import measured_reference, reference_path, save_reference, source_digest
from test_reference_maintenance import fixture


class NativeReferenceTests(unittest.TestCase):
    def test_import_changes_invalidate_measurements_but_build_products_do_not(self):
        for language, extension in (("haskell", "hs"), ("roc", "roc"), ("koka", "kk")):
            with self.subTest(language=language), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                source = root / "problems" / "alpha" / language
                source.mkdir(parents=True)
                (source / f"main.{extension}").write_text("main")
                helper = source / f"Helper.{extension}"
                helper.write_text("v1")
                before = source_digest(root, "alpha", language)
                for artifact in ("benchmark", "Helper.o", "Helper.hi", "build/output.c", ".koka/cache.kki"):
                    path = source / artifact
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_text("generated")
                self.assertEqual(before, source_digest(root, "alpha", language))
                helper.write_text("v2")
                self.assertNotEqual(before, source_digest(root, "alpha", language))

    def test_interpreter_adapter_change_invalidates_its_snapshot_only(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            adapter = root / "infrastructure" / "diagnostic_references.py"
            adapter.parent.mkdir()
            adapter.write_text("adapter v1")
            interpreter_before = source_digest(root, "alpha", "darklang-interpreter")
            dark_before = source_digest(root, "alpha", "dark")
            adapter.write_text("adapter v2")
            self.assertNotEqual(interpreter_before, source_digest(root, "alpha", "darklang-interpreter"))
            self.assertEqual(dark_before, source_digest(root, "alpha", "dark"))

    def test_both_roc_build_interfaces_and_version_commands(self):
        for help_text, expected in (("--opt=speed --jobs=1", "current"), ("--optimize", "legacy")):
            with patch("native_references.subprocess.run", return_value=SimpleNamespace(stdout=help_text)):
                self.assertEqual(roc_mode("roc"), expected)
        self.assertIn("--opt=speed", build_flags("roc", "current"))
        self.assertIn("--optimize", build_flags("roc", "legacy"))
        self.assertEqual(version_command("roc", "roc", "current"), ["roc", "version"])
        self.assertEqual(version_command("roc", "roc", "legacy"), ["roc", "--version"])
        with patch("native_references.subprocess.run", return_value=SimpleNamespace(stdout="unknown")):
            with self.assertRaisesRegex(ValueError, "unsupported build interface"):
                roc_mode("roc")

    def test_native_builds_use_private_sources_and_check_stdout(self):
        for language, extension in (("haskell", "hs"), ("roc", "roc"), ("koka", "kk")):
            with self.subTest(language=language), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary) / "root"
                fixture(root)
                source = root / "problems" / "alpha" / language
                source.mkdir()
                (source / f"main.{extension}").write_text("original")
                (source / f"Helper.{extension}").write_text("helper")
                before = source_digest(root, "alpha", language)
                commands = []
                def execute(command, **kwargs):
                    commands.append(command)
                    if command[0] != "valgrind":
                        copied = Path(command[-1])
                        self.assertNotEqual(copied.parent, source)
                        self.assertTrue((copied.parent / f"Helper.{extension}").is_file())
                        self.assertEqual(kwargs["cwd"], copied.parent)
                        copied.write_text("compiler touched source")
                        binary = command[command.index("-o") + 1] if "-o" in command else next(x.split("=", 1)[1] for x in command if x.startswith("--output="))
                        Path(binary).write_text("binary")
                        return SimpleNamespace(stdout="", stderr="")
                    return SimpleNamespace(stdout="1\n", stderr="==1== I refs: 123\n")
                with patch("native_references.subprocess.run", side_effect=execute):
                    row = measure_native(root, Path(temporary) / "build", "alpha", language, "full", "compiler", "legacy", 60)
                self.assertEqual(row["instructions"], 123)
                self.assertEqual(commands[-1][-1], "1")
                self.assertEqual(before, source_digest(root, "alpha", language))
                with patch("native_references.subprocess.run", side_effect=lambda command, **kwargs: execute(command, **kwargs) if command[0] != "valgrind" else SimpleNamespace(stdout="wrong\n", stderr="==1== I refs: 123\n")):
                    with self.assertRaisesRegex(ValueError, "output mismatch"):
                        measure_native(root, Path(temporary) / "bad-build", "alpha", language, "full", "compiler", "legacy", 60)

    def test_native_output_failure_preserves_snapshot(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            for name in ("alpha", "beta"):
                source = root / "problems" / name / "haskell" / "main.hs"
                source.parent.mkdir()
                source.write_text("main")
            rows = [{"name": name, "instructions": 100, "output_valid": True} for name in ("alpha", "beta")]
            save_reference(root, measured_reference(root, "haskell", "full", "arm64", "v1", "date", rows, [], [], {}))
            path = reference_path(root, "arm64-full-cachegrind", "haskell")
            before = path.read_bytes()
            args = SimpleNamespace(language="haskell", profile="full", timeout=60)
            with patch("reference_cli.preflight", return_value=("v1", {"executable": "ghc"})), patch("reference_cli.measure_native", side_effect=ValueError("output mismatch")):
                with self.assertRaisesRegex(ValueError, "output mismatch"):
                    refresh(root, args)
            self.assertEqual(before, path.read_bytes())

    def test_interpreter_execution_isolates_prepared_state_and_injects_arguments(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "root"
            fixture(root)
            source = root / "problems" / "alpha" / "dark" / "main.dark"
            source.write_text("Stdlib.Cli.__Args.int64 0")
            rundir = Path(temporary) / "runtime"
            rundir.mkdir()
            (rundir / "data.db").write_text("prepared packages")
            build = Path(temporary) / "build"
            build.mkdir()
            def execute(command, **kwargs):
                self.assertEqual(command[0], "valgrind")
                self.assertIn("run", command)
                prepared = Path(command[-1])
                self.assertIn("Ok 1L", prepared.read_text())
                private = Path(kwargs["env"]["DARK_CONFIG_RUNDIR"])
                self.assertNotEqual(private, rundir)
                self.assertEqual((private / "data.db").read_text(), "prepared packages")
                (private / "data.db").write_text("execution trace")
                return SimpleNamespace(returncode=0, stdout="1\n", stderr="==1== I refs: 456\n")
            with patch("diagnostic_references.subprocess.run", side_effect=execute):
                row = measure_one(root, build, "alpha", "darklang-interpreter", "full", Path("/interpreter"), rundir, 60)
            self.assertEqual(row["instructions"], 456)
            self.assertEqual((rundir / "data.db").read_text(), "prepared packages")
            self.assertEqual(source.read_text(), "Stdlib.Cli.__Args.int64 0")

    def test_all_skips_interpreter_without_context(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            args = SimpleNamespace(language="all", profile="full", timeout=60)
            output = io.StringIO()
            # Stop at preflight: no real toolchain runs or snapshots are written.
            with contextlib.redirect_stdout(output), patch("reference_cli.preflight", side_effect=ValueError("stop before measurement")) as check:
                with self.assertRaisesRegex(ValueError, "stop before measurement"):
                    refresh(root, args)
            self.assertIn("interpreter: skipped", output.getvalue())
            self.assertNotEqual(check.call_args.args[1], "darklang-interpreter")

    def test_interpreter_requires_context_and_reuses_dark_sources(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            with self.assertRaisesRegex(ValueError, "prepared"):
                preflight(root, "darklang-interpreter", "full")
            rundir = root / "runtime"
            rundir.mkdir()
            args = SimpleNamespace(darklang_interpreter=root / "dark-cli", darklang_rundir=rundir)
            with patch("reference_cli.shutil.which", side_effect=lambda value: value), patch("reference_cli.implementation_version", return_value="interpreter-v1"), patch("reference_cli.command_version", return_value="valgrind-v1"):
                version, tools = preflight(root, "darklang-interpreter", "full", args)
            self.assertEqual(version, "interpreter-v1")
            self.assertIn("executable", tools)


if __name__ == "__main__":
    unittest.main()
