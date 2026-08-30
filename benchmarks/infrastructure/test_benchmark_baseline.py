#!/usr/bin/env python3
"""Fast unit tests for canonical benchmark track identity and comparison."""

from __future__ import annotations

import json
import tempfile
import unittest
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


if __name__ == "__main__":
    unittest.main()
