#!/usr/bin/env python3
"""Focused tests for the merge-train benchmark-source integrity gate."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

from benchmarks.check_sources_unchanged import changed_benchmark_sources

GATE = Path(__file__).resolve().parent / "check_sources_unchanged.py"


class BenchmarkSourceGateTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.repo = Path(self.temporary.name)
        subprocess.run(["git", "init", "-q", "-b", "main"], cwd=self.repo, check=True)
        subprocess.run(
            ["git", "config", "user.email", "benchmark-gate@example.invalid"],
            cwd=self.repo,
            check=True,
        )
        subprocess.run(
            ["git", "config", "user.name", "Benchmark Gate Test"],
            cwd=self.repo,
            check=True,
        )
        source = self.repo / "benchmarks/problems/example/dark/main.dark"
        source.parent.mkdir(parents=True)
        source.write_text("1\n", encoding="utf-8")
        infrastructure = self.repo / "benchmarks/infrastructure/runner.py"
        infrastructure.parent.mkdir(parents=True)
        infrastructure.write_text("# runner\n", encoding="utf-8")
        subprocess.run(["git", "add", "."], cwd=self.repo, check=True)
        subprocess.run(
            ["git", "commit", "-q", "-m", "benchmark baseline"],
            cwd=self.repo,
            check=True,
        )
        self.base = subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=self.repo, text=True
        ).strip()

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def commit(self, message: str) -> None:
        subprocess.run(["git", "add", "-A"], cwd=self.repo, check=True)
        subprocess.run(
            ["git", "commit", "-q", "-m", message], cwd=self.repo, check=True
        )

    def test_allows_changes_outside_benchmark_problem_sources(self) -> None:
        path = self.repo / "benchmarks/infrastructure/runner.py"
        path.write_text("# updated runner\n", encoding="utf-8")
        self.commit("change infrastructure")

        self.assertEqual(changed_benchmark_sources(self.repo, self.base), ())

    def test_rejects_modified_benchmark_source(self) -> None:
        path = self.repo / "benchmarks/problems/example/dark/main.dark"
        path.write_text("2\n", encoding="utf-8")
        self.commit("change benchmark")

        self.assertEqual(
            changed_benchmark_sources(self.repo, self.base),
            ("benchmarks/problems/example/dark/main.dark",),
        )

        completed = subprocess.run(
            [
                "python3",
                str(GATE),
                "--repo",
                str(self.repo),
                "--base",
                self.base,
            ],
            text=True,
            capture_output=True,
            check=False,
        )
        self.assertEqual(completed.returncode, 1)
        self.assertIn("Benchmark source integrity gate failed", completed.stdout)
        self.assertIn(
            "benchmarks/problems/example/dark/main.dark", completed.stdout
        )

    def test_rejects_added_deleted_and_renamed_problem_files(self) -> None:
        original = self.repo / "benchmarks/problems/example/dark/main.dark"
        original.unlink()
        added = self.repo / "benchmarks/problems/new/rust/main.rs"
        added.parent.mkdir(parents=True)
        added.write_text("fn main() {}\n", encoding="utf-8")
        manifest = self.repo / "benchmarks/problems/example/Cargo.toml"
        manifest.write_text("[package]\nname = \"example\"\n", encoding="utf-8")
        self.commit("replace benchmark sources")

        self.assertEqual(
            changed_benchmark_sources(self.repo, self.base),
            (
                "benchmarks/problems/example/Cargo.toml",
                "benchmarks/problems/example/dark/main.dark",
                "benchmarks/problems/new/rust/main.rs",
            ),
        )

    def test_merge_train_runs_the_source_gate_before_building(self) -> None:
        config = (GATE.parent.parent / ".mergetrain.yaml").read_text(encoding="utf-8")

        source_gate = (
            "  - name: benchmark-sources\n"
            "    run: python3 benchmarks/check_sources_unchanged.py "
            "--base ${integration_ref}\n"
        )
        self.assertIn(source_gate, config)
        self.assertLess(config.index(source_gate), config.index("  - name: build\n"))


if __name__ == "__main__":
    unittest.main()
