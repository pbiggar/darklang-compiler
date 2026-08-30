#!/usr/bin/env python3
"""Fast unit tests for canonical benchmark track identity and comparison."""

from __future__ import annotations

import json
import tempfile
import unittest
from argparse import Namespace
from contextlib import redirect_stdout
from io import StringIO
from pathlib import Path

from benchmark_baseline import (
    BaselineError,
    BenchmarkCount,
    CompilerAttribution,
    TRACKS,
    compare_implementations,
    comparison_dict,
    create_snapshot,
    load_snapshot,
    snapshot_path,
    write_snapshot,
    _quick_command,
)


class BenchmarkTrackTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.benchmarks = Path(self.temporary.name)
        (self.benchmarks / "profiles").mkdir()
        (self.benchmarks / "profiles" / "quick.txt").write_text("alpha\nbeta\n")
        for name in ("alpha", "beta"):
            (self.benchmarks / "problems" / name).mkdir(parents=True)
        (self.benchmarks / "PARITY.json").write_text(
            json.dumps(
                {
                    "schema": 2,
                    "benchmarks": {
                        name: {
                            "quick": {
                                "status": "comparable",
                                "dark_source_sha256": name,
                                "rust_source_sha256": name,
                            }
                        }
                        for name in ("alpha", "beta")
                    },
                }
            )
        )
        self.compiler = CompilerAttribution("a" * 40, "candidate")

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def snapshot(self, language: str, track_id: str, values: tuple[int, int]):
        return create_snapshot(
            self.benchmarks,
            language,
            TRACKS[track_id],
            (BenchmarkCount("alpha", values[0]), BenchmarkCount("beta", values[1])),
            "2026-08-30T00:00:00+00:00",
            self.compiler,
        )

    def test_snapshot_paths_include_complete_track_identity(self) -> None:
        cachegrind = snapshot_path(
            self.benchmarks, "dark", TRACKS["arm64-quick-cachegrind"]
        )
        qemu = snapshot_path(
            self.benchmarks, "dark", TRACKS["x86_64-quick-qemu"]
        )
        self.assertNotEqual(cachegrind, qemu)
        self.assertEqual(qemu.name, "dark-x86_64-quick-qemu.json")

    def test_loading_rejects_a_different_measurement_track(self) -> None:
        source_track = TRACKS["x86_64-quick-qemu"]
        path = snapshot_path(self.benchmarks, "dark", source_track)
        write_snapshot(path, self.snapshot("dark", source_track.id, (100, 200)))
        with self.assertRaisesRegex(BaselineError, "track"):
            load_snapshot(
                path,
                self.benchmarks,
                "dark",
                TRACKS["x86_64-quick-cachegrind"],
            )

    def test_dark_rust_multiplier_requires_matching_track_and_names(self) -> None:
        track = TRACKS["x86_64-quick-qemu"]
        dark = self.snapshot("dark", track.id, (200, 800))
        rust = self.snapshot("rust", track.id, (100, 200))
        comparison = compare_implementations(dark, rust)
        self.assertAlmostEqual(comparison.ratio, 2.8284271247461903)

    def test_fast_projection_does_not_mutate_canonical_track_identity(self) -> None:
        baseline = self.snapshot("dark", "arm64-quick-cachegrind", (100, 200))
        decision = comparison_dict(
            compare_implementations(
                baseline,
                self.snapshot("rust", "arm64-quick-cachegrind", (100, 200)),
            ),
            "quick-fast",
            baseline,
            "projection-only",
        )
        self.assertEqual(decision["track"]["profile"], "quick")
        self.assertEqual(decision["selection_profile"], "quick-fast")

    def test_targeted_quick_comparison_projects_without_advancing_snapshot(self) -> None:
        track = TRACKS["arm64-quick-cachegrind"]
        baseline = self.snapshot("dark", track.id, (100, 200))
        baseline_path = snapshot_path(self.benchmarks, "dark", track)
        write_snapshot(baseline_path, baseline)
        counts_path = self.benchmarks / "counts.tsv"
        counts_path.write_text("alpha\t80\n")
        decision_path = self.benchmarks / "decision.json"

        output = StringIO()
        with redirect_stdout(output):
            result = _quick_command(
                Namespace(
                    benchmarks_dir=str(self.benchmarks),
                    track=track.id,
                    counts=str(counts_path),
                    commit="b" * 40,
                    subject="targeted candidate",
                    timestamp="2026-08-30T01:00:00+00:00",
                    decision_json=str(decision_path),
                    fast=False,
                    reset=False,
                    quiet=True,
                    selection="alpha",
                )
            )

        self.assertEqual(result, 0)
        self.assertIn("Dark targeted selection: improved", output.getvalue())
        self.assertIn(
            "Dark quick snapshot: preserved (targeted-only)", output.getvalue()
        )
        self.assertEqual(
            load_snapshot(baseline_path, self.benchmarks, "dark", track).benchmarks,
            baseline.benchmarks,
        )
        decision = json.loads(decision_path.read_text())
        self.assertEqual(decision["decision"], "improved")
        self.assertEqual(decision["snapshot_action"], "targeted-only")
        self.assertEqual(decision["selection_profile"], "targeted")
        self.assertEqual(decision["selected_benchmarks"], ["alpha"])
        self.assertFalse(decision["promotion_eligible"])
        self.assertEqual(decision["candidate"]["commit"], "b" * 40)
        self.assertAlmostEqual(decision["current_baseline_ratio"], 0.8)
        self.assertEqual(decision["benchmarks"][0]["baseline"], 100)
        self.assertEqual(decision["benchmarks"][0]["current"], 80)

    def test_targeted_quick_comparison_rejects_snapshot_reset(self) -> None:
        track = TRACKS["arm64-quick-cachegrind"]
        write_snapshot(
            snapshot_path(self.benchmarks, "dark", track),
            self.snapshot("dark", track.id, (100, 200)),
        )
        counts_path = self.benchmarks / "counts.tsv"
        counts_path.write_text("alpha\t80\n")

        with self.assertRaisesRegex(BaselineError, "targeted.*cannot reset"):
            _quick_command(
                Namespace(
                    benchmarks_dir=str(self.benchmarks),
                    track=track.id,
                    counts=str(counts_path),
                    commit="b" * 40,
                    subject="targeted candidate",
                    timestamp="2026-08-30T01:00:00+00:00",
                    decision_json=str(self.benchmarks / "decision.json"),
                    fast=False,
                    reset=True,
                    quiet=True,
                    selection="alpha",
                )
            )


if __name__ == "__main__":
    unittest.main()
