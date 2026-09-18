#!/usr/bin/env python3
"""Fast tests for task-parent benchmark comparison behavior."""

from __future__ import annotations

import json
import tempfile
import unittest
from contextlib import redirect_stdout
from io import StringIO
from pathlib import Path
from unittest.mock import patch

from compare_with_parent import compare_with_parent, resolve_parent
from benchmark_baseline import BaselineError


class ResolveParentTests(unittest.TestCase):
    def test_default_parent_is_the_upstream_merge_base(self) -> None:
        with patch(
            "compare_with_parent.git",
            side_effect=("a" * 40, "a" * 40, ""),
        ) as git:
            parent = resolve_parent(Path("/repo"), None)

        self.assertEqual(parent, "a" * 40)
        self.assertEqual(
            [call.args[1:] for call in git.call_args_list],
            [
                ("merge-base", "HEAD", "@{upstream}"),
                ("rev-parse", "--verify", f"{'a' * 40}^{{commit}}"),
                ("merge-base", "--is-ancestor", "a" * 40, "HEAD"),
            ],
        )

    def test_explicit_parent_must_be_an_ancestor(self) -> None:
        with patch(
            "compare_with_parent.git",
            side_effect=("b" * 40, BaselineError("not an ancestor")),
        ):
            with self.assertRaisesRegex(BaselineError, "not an ancestor"):
                resolve_parent(Path("/repo"), "task-base")


class CompareWithParentTests(unittest.TestCase):
    def setUp(self) -> None:
        self.project_root = Path(__file__).resolve().parent.parent
        self.snapshot_path = (
            self.project_root
            / "benchmarks/baselines/dark-arm64-full-cachegrind.json"
        )
        self.snapshot = json.loads(self.snapshot_path.read_text())
        self.temporary = tempfile.TemporaryDirectory()
        self.results = Path(self.temporary.name)
        for row in self.snapshot["benchmarks"]:
            result = {
                "results": [
                    {
                        "language": "Dark",
                        "instructions": row["instructions"],
                    }
                ]
            }
            (self.results / f"{row['name']}_cachegrind.json").write_text(
                json.dumps(result)
            )

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def compare(self) -> tuple[int, str]:
        output = StringIO()
        with (
            patch(
                "compare_with_parent.git", return_value=self.snapshot_path.read_text()
            ),
            patch("compare_with_parent.machine_architecture", return_value="arm64"),
            redirect_stdout(output),
        ):
            result = compare_with_parent(
                self.project_root, self.results, "a" * 40, quiet=True
            )
        return result, output.getvalue()

    def test_equal_measurements_report_parent_ratio_without_rerunning(self) -> None:
        result, output = self.compare()

        self.assertEqual(result, 0)
        self.assertEqual(
            output,
            "Dark candidate/parent: equal; current/parent geometric ratio "
            "1.000000\n",
        )

    def test_parent_regression_fails(self) -> None:
        first = self.snapshot["benchmarks"][0]
        path = self.results / f"{first['name']}_cachegrind.json"
        document = json.loads(path.read_text())
        document["results"][0]["instructions"] += 1
        path.write_text(json.dumps(document))

        result, output = self.compare()

        self.assertEqual(result, 1)
        self.assertIn("Dark candidate/parent: regressed", output)


if __name__ == "__main__":
    unittest.main()
