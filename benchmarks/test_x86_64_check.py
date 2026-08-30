#!/usr/bin/env python3
"""Fast tests for canonical x86_64 measurement decisions and reports."""

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent / "infrastructure"))

from benchmark_baseline import (  # noqa: E402
    BenchmarkCount,
    CompilerAttribution,
    Snapshot,
    TRACKS,
    track_dict,
)
from x86_64_check import compare, render_results  # noqa: E402


class X86_64DecisionTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def write_measurement(
        self,
        name: str,
        dark: tuple[int, int],
        *,
        dark_complete: bool,
        rust: tuple[int, int] | None = None,
    ) -> Path:
        path = self.root / f"{name}.json"
        dark_total = 2 if dark_complete else 3
        rust_rows = [] if rust is None else [
            {"name": "alpha", "instructions": rust[0]},
            {"name": "beta", "instructions": rust[1]},
        ]
        path.write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "suite": "dark-compiler",
                    "track": track_dict(TRACKS["x86_64-quick-qemu"]),
                    "requested_languages": ["dark"] if rust is None else ["dark", "rust"],
                    "contract_sha256": "contract",
                    "generated_at": "2026-08-30T00:00:00+00:00",
                    "compiler": {"commit": name * 40, "subject": name},
                    "toolchains": {
                        "dotnet": "10.0.0",
                        "rustc": "rustc 1.89.0 (fixture)",
                        "x86_64_linker": "gcc fixture",
                        "qemu": "qemu-x86_64 version 11.1.1",
                    },
                    "coverage": {
                        "dark": {"measured": 2, "total": dark_total},
                        "rust": {"measured": len(rust_rows), "total": 2},
                    },
                    "measurements": {
                        "dark": [
                            {"name": "alpha", "instructions": dark[0]},
                            {"name": "beta", "instructions": dark[1]},
                        ],
                        "rust": rust_rows,
                    },
                    "unavailable": [],
                }
            )
        )
        return path

    def test_complete_candidate_can_win_against_complete_base_and_rust_reference(self) -> None:
        baseline = self.write_measurement(
            "b", (100, 400), dark_complete=True, rust=(50, 100)
        )
        candidate = self.write_measurement("c", (90, 360), dark_complete=True)
        decision = self.root / "decision.json"

        self.assertEqual(compare(baseline, candidate, decision), 0)
        payload = json.loads(decision.read_text())

        self.assertEqual(payload["decision"], "improved")
        self.assertAlmostEqual(payload["current_baseline_ratio"], 0.9)
        self.assertAlmostEqual(payload["candidate_dark_rust_ratio"], 2.545584412271571)

    def test_incomplete_coverage_cannot_be_reported_as_a_win(self) -> None:
        baseline = self.write_measurement(
            "b", (100, 400), dark_complete=False, rust=(50, 100)
        )
        candidate = self.write_measurement("c", (90, 360), dark_complete=True)
        decision = self.root / "decision.json"

        self.assertEqual(compare(baseline, candidate, decision), 0)
        payload = json.loads(decision.read_text())

        self.assertEqual(payload["decision"], "partial-improved")
        self.assertIsNotNone(payload["candidate_dark_rust_ratio"])

    def test_result_report_contains_overall_multiplier_and_per_workload_rows(self) -> None:
        track = TRACKS["x86_64-quick-qemu"]
        compiler = CompilerAttribution("a" * 40, "candidate")
        dark = Snapshot(
            2,
            "dark-compiler",
            "dark",
            track,
            "contract",
            "2026-08-30T00:00:00+00:00",
            compiler,
            (BenchmarkCount("alpha", 200), BenchmarkCount("beta", 800)),
        )
        rust = Snapshot(
            2,
            "dark-compiler",
            "rust",
            track,
            "contract",
            "2026-08-30T00:00:00+00:00",
            compiler,
            (BenchmarkCount("alpha", 100), BenchmarkCount("beta", 200)),
        )

        payload, markdown = render_results(dark, rust)

        self.assertAlmostEqual(payload["overall_dark_rust_ratio"], 2.8284271247461903)
        self.assertIn("**Overall Dark/Rust:** `2.828427×`", markdown)
        self.assertIn("| beta | 800 | 200 | 4.000× |", markdown)


if __name__ == "__main__":
    unittest.main()
