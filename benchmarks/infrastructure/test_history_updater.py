"""Recorder integration tests using stored fixtures rather than real toolchains."""

import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from history_updater import update_baselines, update_results
from reference_snapshots import read_json, reference_path
from test_reference_maintenance import fixture


class HistoryUpdaterTests(unittest.TestCase):
    def test_rust_refresh_persists_structured_measurements(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            rows = {name: [{"language": "rust", "instructions": 125}] for name in ("alpha", "beta")}
            with patch("history_updater.machine_architecture", return_value="arm64"), \
                 patch("diagnostic_references.command_version", return_value="fixture-version"):
                update_baselines(root, rows, "full", "2026-09-30T00:00:00+00:00")
            stored = read_json(reference_path(root, "arm64-full-cachegrind", "rust"))
            self.assertEqual(stored["version"], "fixture-version")
            self.assertEqual(stored["benchmarks"][0]["instructions"], 125)

    def test_recording_regenerates_reports_from_stored_snapshots(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            update_results(root, None)
            self.assertIn("reports/arm64-full-cachegrind.md", (root / "RESULTS.md").read_text())
            self.assertIn("200", (root / "reports" / "arm64-full-cachegrind.md").read_text())


if __name__ == "__main__":
    unittest.main()
