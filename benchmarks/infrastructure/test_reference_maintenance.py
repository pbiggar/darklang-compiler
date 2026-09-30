"""Reference preservation and compatibility tests; no real toolchains run."""

import json
import tempfile
import threading
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

from benchmark_baseline import BenchmarkCount, CompilerAttribution, TRACKS, create_snapshot, write_snapshot
from benchmark_reports import generate_reports, report_for_track
from reference_cli import refresh
from reference_snapshots import measured_reference, read_json, reference_path, row_status, save_reference, source_digest


def fixture(root: Path, names: tuple[str, ...] = ("alpha", "beta")) -> None:
    """Declare two workloads to exercise independent row compatibility."""
    workloads, parity = {}, {}
    for name in names:
        for language, extension, source in (
            ("dark", "dark", "1L"), ("rust", "rs", "fn main() {}"), ("python", "py", "print(1)"),
        ):
            path = root / "problems" / name / language / f"main.{extension}"
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(source)
        workloads[name] = {mode: {"args": ["1"], "expected_stdout": "1\n"} for mode in ("full", "quick")}
        parity[name] = {"status": "comparable", "quick": {"status": "comparable"},
                        "dark_sha256": source_digest(root, name, "dark"),
                        "rust_sha256": source_digest(root, name, "rust")}
    (root / "profiles.json").write_text(json.dumps({"schema": 1,
        "profiles": {"full": list(names), "quick": list(names)}, "workloads": workloads}))
    (root / "PARITY.json").write_text(json.dumps({"schema": 3, "benchmarks": parity}))
    dark = create_snapshot(root, "dark", TRACKS["arm64-full-cachegrind"],
        [BenchmarkCount(name, count) for name, count in zip(names, (200, 300, 6400))],
        "2026-09-30T00:00:00+00:00", CompilerAttribution("a" * 40, "fixture"))
    write_snapshot(root / "baselines" / "dark-arm64-full-cachegrind.json", dark)


def reference(root: Path, language: str, names: tuple[str, ...] = ("alpha", "beta")) -> dict:
    return measured_reference(root, language, "full", "arm64", "fixture-version", "2026-09-30T00:00:00+00:00",
        [{"name": name, "instructions": 100, "output_valid": True} for name in names], [], [], {})


class ReferenceMaintenanceTests(unittest.TestCase):
    def test_refresh_rejects_omitted_or_output_invalid_implementations(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            rows = reference(root, "python")["benchmarks"]
            with self.assertRaisesRegex(ValueError, "every available implementation"):
                measured_reference(root, "python", "full", "arm64", "v1",
                                   "2026-09-30T00:00:00+00:00", rows[:1], [], [], {})
            rows[0]["output_valid"] = False
            with self.assertRaisesRegex(ValueError, "output-invalid"):
                measured_reference(root, "python", "full", "arm64", "v1",
                                   "2026-09-30T00:00:00+00:00", rows, [], [], {})

    def test_one_changed_workload_preserves_other_rows(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            document = reference(root, "python")
            profiles = read_json(root / "profiles.json")
            profiles["workloads"]["alpha"]["full"]["args"] = ["2"]
            (root / "profiles.json").write_text(json.dumps(profiles))
            self.assertEqual(row_status(root, document, document["benchmarks"][0], "full"), "stale workload")
            self.assertEqual(row_status(root, document, document["benchmarks"][1], "full"), "current")

    def test_source_change_invalidates_measurement(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            document = reference(root, "python")
            (root / "problems" / "alpha" / "python" / "main.py").write_text("print(2)")
            self.assertEqual(row_status(root, document, document["benchmarks"][0], "full"), "stale source")

    def test_failed_refresh_preserves_selected_and_other_languages(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            for language in ("python", "rust"):
                save_reference(root, reference(root, language))
            paths = [reference_path(root, "arm64-full-cachegrind", language) for language in ("python", "rust")]
            before = [path.read_bytes() for path in paths]
            args = SimpleNamespace(language="python", profile="full", timeout=60)
            with patch("reference_cli.preflight", return_value=("fixture-version", {})), \
                 patch("reference_cli.measure_one", side_effect=ValueError("output mismatch")):
                with self.assertRaisesRegex(ValueError, "output mismatch"):
                    refresh(root, args)
            self.assertEqual([path.read_bytes() for path in paths], before)

    def test_successful_refresh_preserves_other_language(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            save_reference(root, reference(root, "rust"))
            path = reference_path(root, "arm64-full-cachegrind", "rust")
            before = path.read_bytes()
            args = SimpleNamespace(language="python", profile="full", timeout=60)
            with patch("reference_cli.preflight", return_value=("fixture-version", {})), \
                 patch("reference_cli.command_version", return_value="fixture-version"), \
                 patch("reference_cli.machine_architecture", return_value="arm64"), \
                 patch("reference_cli.measure_one", side_effect=lambda _root, _temp, name, *_args:
                       {"name": name, "instructions": 150, "output_valid": True}):
                refresh(root, args)
            self.assertEqual(path.read_bytes(), before)
            self.assertIn("unaudited", (root / "reports" / "arm64-full-cachegrind.md").read_text())

    def test_invalid_snapshot_preserves_previous_reference(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            document = reference(root, "python")
            save_reference(root, document)
            path = reference_path(root, "arm64-full-cachegrind", "python")
            before = path.read_bytes()
            document["benchmarks"][0]["instructions"] = -1
            with self.assertRaises(ValueError):
                save_reference(root, document)
            self.assertEqual(path.read_bytes(), before)

    def test_parallel_refresh_orders_rows_and_preserves_snapshot_on_failure(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            save_reference(root, reference(root, "python"))
            path = reference_path(root, "arm64-full-cachegrind", "python")
            args = SimpleNamespace(language="python", profile="full", timeout=60, jobs=2)
            beta_finished = threading.Event()

            def measure(_root, _temporary, name, *_args):
                if name == "alpha":
                    if not beta_finished.wait(5):
                        raise RuntimeError("second worker did not run")
                else:
                    beta_finished.set()
                return {"name": name, "instructions": 150, "output_valid": True}

            with patch("reference_cli.preflight", return_value=("fixture-version", {})), \
                 patch("reference_cli.command_version", return_value="fixture-version"), \
                 patch("reference_cli.machine_architecture", return_value="arm64"), \
                 patch("reference_cli.measure_one", side_effect=measure):
                refresh(root, args)
            self.assertEqual([row["name"] for row in read_json(path)["benchmarks"]], ["alpha", "beta"])
            before = path.read_bytes()
            with patch("reference_cli.preflight", return_value=("fixture-version", {})), \
                 patch("reference_cli.measure_one", side_effect=ValueError("output mismatch")):
                with self.assertRaisesRegex(ValueError, "output mismatch"):
                    refresh(root, args)
            self.assertEqual(path.read_bytes(), before)

    def test_report_check_is_read_only_and_deterministic(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            generate_reports(root)
            path = root / "RESULTS.md"
            path.write_text("outdated\n")
            self.assertIn(path, generate_reports(root, check=True))
            self.assertEqual(path.read_text(), "outdated\n")
            generate_reports(root)
            self.assertEqual(generate_reports(root, check=True), [])

    def test_aggregate_names_fixed_workloads_and_missing_languages(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fixture(root)
            save_reference(root, reference(root, "rust"))
            report = report_for_track(root, "arm64-full-cachegrind")
            self.assertIn("`alpha`, `beta`", report)
            self.assertIn("Darklang interpreter", report)
            self.assertIn("incomplete", report)
            self.assertIn("missing implementation", report)

    def test_partial_language_does_not_shrink_other_aggregates(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            names = ("fib", "tak", "fft")
            fixture(root, names)
            rust = measured_reference(root, "rust", "full", "arm64", "fixture", "2026-09-30T00:00:00+00:00",
                [{"name": name, "instructions": 100, "output_valid": True} for name in names], [], [], {})
            save_reference(root, rust)
            (root / "problems" / "fft" / "python" / "main.py").unlink()
            save_reference(root, reference(root, "python", names[:2]))
            report = report_for_track(root, "arm64-full-cachegrind")
            original = report.split("### Original 2 workloads")[1].split("### All 3 workloads")[0]
            full = report.split("### All 3 workloads")[1]
            self.assertRegex(original, r"Darklang\s*\|\s*2\.449x\s*\|\s*500\s*\|\s*2/2")
            self.assertRegex(full, r"Darklang\s*\|\s*7\.268x\s*\|\s*6,900\s*\|\s*3/3")
            self.assertRegex(full, r"Python\s*\|\s*unavailable\s*\|\s*unavailable\s*\|\s*2/3")


if __name__ == "__main__":
    unittest.main()
