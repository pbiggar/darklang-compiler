#!/usr/bin/env python3
"""Exercise VM cache recovery and bootstrap failure boundaries without network access."""

import hashlib
import importlib.util
import io
import lzma
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch
import urllib.error
import fcntl

from toolchain_pins import pins

VM = Path(__file__).resolve().parent
TEST_ROOT = VM.parents[1] / "TestResults/vm"
spec = importlib.util.spec_from_file_location("native_packages", VM / "setup-native-packages.py")
packages = importlib.util.module_from_spec(spec)
spec.loader.exec_module(packages)


class DownloadTests(unittest.TestCase):
    def setUp(self):
        TEST_ROOT.mkdir(parents=True, exist_ok=True)
        self.directory = tempfile.TemporaryDirectory(dir=TEST_ROOT)
        self.addCleanup(self.directory.cleanup)
        self.path = Path(self.directory.name) / "archive"

    @staticmethod
    def validate(data):
        if hashlib.sha256(data).digest() != hashlib.sha256(b"complete archive").digest():
            raise ValueError("bad checksum")

    def test_verified_cache_requires_no_network(self):
        self.path.write_bytes(b"complete archive")
        with patch.object(packages.urllib.request, "urlopen") as request:
            packages.download("https://example.invalid/archive", self.path, self.validate)
        request.assert_not_called()

    def test_corrupt_cache_and_partial_download_are_replaced(self):
        self.path.write_bytes(b"truncated")
        partial = self.path.with_name("archive.download")
        partial.write_bytes(b"interrupted")
        with patch.object(packages.urllib.request, "urlopen", return_value=io.BytesIO(b"complete archive")):
            packages.download("https://example.invalid/archive", self.path, self.validate)
        self.assertEqual(self.path.read_bytes(), b"complete archive")
        self.assertFalse(partial.exists())

    def test_checksum_failure_never_promotes_download(self):
        self.path.write_bytes(b"previous invalid cache")
        with patch.object(packages.urllib.request, "urlopen", side_effect=lambda *a, **k: io.BytesIO(b"bad")), \
             patch.object(packages.time, "sleep"), self.assertRaises(ValueError):
            packages.download("https://example.invalid/archive", self.path, self.validate)
        self.assertEqual(self.path.read_bytes(), b"previous invalid cache")

    def test_transient_network_failure_is_retried(self):
        with patch.object(packages.urllib.request, "urlopen", side_effect=[
            urllib.error.URLError("interrupted"), io.BytesIO(b"complete archive")
        ]) as request, patch.object(packages.time, "sleep"):
            packages.download("https://example.invalid/archive", self.path, self.validate)
        self.assertEqual(request.call_count, 2)
        self.assertEqual(self.path.read_bytes(), b"complete archive")

    def test_corrupt_index_is_refetched(self):
        self.path.write_bytes(b"broken xz")
        index = lzma.compress(b"Package: example\n")
        with patch.object(packages.urllib.request, "urlopen", return_value=io.BytesIO(index)):
            packages.download("https://example.invalid/index", self.path, lzma.decompress)
        self.assertEqual(lzma.decompress(self.path.read_bytes()), b"Package: example\n")

    def test_failed_first_download_does_not_create_cache(self):
        with patch.object(packages.urllib.request, "urlopen", side_effect=urllib.error.URLError("denied")), \
             patch.object(packages.time, "sleep"), self.assertRaises(urllib.error.URLError):
            packages.download("https://example.invalid/archive", self.path, self.validate)
        self.assertFalse(self.path.exists())


class BootstrapTests(unittest.TestCase):
    def setUp(self):
        TEST_ROOT.mkdir(parents=True, exist_ok=True)
        self.directory = tempfile.TemporaryDirectory(dir=TEST_ROOT)
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name) / "toolchains with spaces"

    def run_script(self, script, *args, env=None):
        return subprocess.run(["bash", str(VM / script), *map(str, args)],
                              text=True, capture_output=True, env=env, timeout=20)

    def test_relative_directory_rejected_before_install(self):
        result = self.run_script("setup-native-toolchain", "relative")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("absolute", result.stderr)

    def test_usage_and_help(self):
        for script in ["setup-native-toolchain", "check-native-toolchain"]:
            with self.subTest(script=script):
                self.assertNotEqual(self.run_script(script).returncode, 0)
                self.assertEqual(self.run_script(script, "--help").returncode, 0)
                self.assertNotEqual(self.run_script(script, "one", "two").returncode, 0)

    def test_missing_activation_is_actionable(self):
        result = self.run_script("check-native-toolchain", self.root)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("setup-native-toolchain", result.stderr)

    def test_competing_setup_cannot_mutate_toolchain(self):
        self.root.mkdir()
        with (self.root / "setup.lock").open("w") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            result = self.run_script("setup-native-toolchain", self.root)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Another setup", result.stderr)
        self.assertFalse((self.root / "vm-compat.so").exists())

    def test_download_failure_names_stage_log_and_retry(self):
        binary_directory = Path(self.directory.name) / "bin"
        binary_directory.mkdir()
        curl = binary_directory / "curl"
        curl.write_text("#!/bin/sh\necho 'simulated HTTP denial' >&2\nexit 22\n")
        curl.chmod(0o755)
        env = {**os.environ, "PATH": f"{binary_directory}:{os.environ['PATH']}"}
        result = self.run_script("setup-native-toolchain", self.root, env=env)
        self.assertEqual(result.returncode, 22)
        self.assertIn("failed during OCaml", result.stderr)
        self.assertIn("Full log:", result.stderr)
        self.assertIn("simulated HTTP denial", result.stderr)
        self.assertIn("Retry:", result.stderr)
        self.assertFalse((self.root / "activate").exists())
        # The setup process's exit must release the lock for a retry.
        retry = self.run_script("setup-native-toolchain", self.root, env=env)
        self.assertEqual(retry.returncode, 22)
        self.assertNotIn("Another setup", retry.stderr)

    def test_missing_prerequisite_fails_before_install(self):
        binary_directory = Path(self.directory.name) / "bin"
        binary_directory.mkdir()
        for command in ["dirname", "uname"]:
            (binary_directory / command).symlink_to(shutil.which(command))
        env = {**os.environ, "PATH": str(binary_directory)}
        result = subprocess.run([shutil.which("bash"), str(VM / "setup-native-toolchain"), str(self.root)],
                                text=True, capture_output=True, env=env, timeout=20)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Missing prerequisite: gcc", result.stderr)
        self.assertFalse(self.root.exists())

    def test_wrong_active_compiler_is_rejected(self):
        self.root.mkdir()
        binary_directory = self.root / "bin"
        binary_directory.mkdir()
        compiler = binary_directory / "ocamlc"
        compiler.write_text("#!/bin/sh\necho 0.0.0\n")
        compiler.chmod(0o755)
        (self.root / "activate").write_text(f'export PATH="{binary_directory}:$PATH"\n')
        result = self.run_script("check-native-toolchain", self.root)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("expected", result.stderr)
        self.assertIn("found 0.0.0", result.stderr)

    def test_canonical_pins_are_readable(self):
        version, checksum, dune, locked = pins()
        self.assertRegex(version, r"^\d+\.\d+\.\d+$")
        self.assertRegex(checksum, r"^[0-9a-f]{64}$")
        self.assertIn("dune." + dune, locked)


if __name__ == "__main__":
    unittest.main()
