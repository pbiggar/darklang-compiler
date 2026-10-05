"""Check the differential gate rejects missing requests, bytes and invocations."""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location("acceptance", Path(__file__).with_name("acceptance.py"))
acceptance = importlib.util.module_from_spec(spec)
spec.loader.exec_module(acceptance)


class AcceptanceTests(unittest.TestCase):
    def setUp(self):
        scratch = acceptance.ROOT / "TestResults"
        scratch.mkdir(exist_ok=True)
        self.temp = tempfile.TemporaryDirectory(dir=scratch)
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        for side in ("reference", "native"):
            (self.root / (side + "-events")).mkdir()

    def event(self, side, index, data=b"complete executable", request=None, error=None):
        directory = self.root / (side + "-events")
        filename = f"{index}.bin" if data is not None else None
        if data is not None:
            (directory / filename).write_bytes(data)
        (directory / f"{index}.json").write_text(json.dumps({
            "kind": "compile", "request": request or ["source", "options"],
            "binary": filename, "error": error.encode("utf-16-be").hex() if error is not None else None,
        }))

    def compare(self):
        with contextlib.redirect_stdout(io.StringIO()):
            return acceptance.compare(self.root)

    def test_inference_identity_renaming_preserves_diagnostic(self):
        def encoded(value):
            return value.encode("utf-16-be").hex()
        self.assertEqual(
            acceptance.error_identity(encoded('#infer:a$scope$0:' + '1' * 32)),
            acceptance.error_identity(encoded('#infer:a$scope$0:' + '2' * 32)))
        self.assertNotEqual(
            acceptance.error_identity(encoded('#infer:a:' + '1' * 32 + ' #infer:a:' + '1' * 32)),
            acceptance.error_identity(encoded('#infer:a:' + '2' * 32 + ' #infer:a:' + '3' * 32)))

    def test_parallel_order_does_not_change_comparison(self):
        for index, value in enumerate((b"first", b"second", b"first")):
            self.event("reference", index, value)
        for index, value in enumerate((b"first", b"first", b"second")):
            self.event("native", index, value)
        self.assertFalse(self.compare())

    def test_missing_duplicate_invocation_is_rejected(self):
        self.event("reference", 0)
        self.event("reference", 1)
        self.event("native", 0)
        self.assertTrue(self.compare())

    def test_changed_padding_byte_retains_first_difference(self):
        self.event("reference", 0, b"ELF\0padding")
        self.event("native", 0, b"ELF\1padding")
        self.assertTrue(self.compare())
        report = json.loads((self.root / "comparison.json").read_text())
        self.assertEqual(report["failures"][0]["first_difference"], 3)

    def test_same_binary_under_different_request_is_rejected(self):
        self.event("reference", 0, request=["leak-check"])
        self.event("native", 0, request=["default"])
        self.assertTrue(self.compare())

    def test_changed_compile_error_is_rejected(self):
        self.event("reference", 0, None, error="compile-time rejection")
        self.event("native", 0, None, error="different location")
        self.assertTrue(self.compare())

    def test_empty_capture_is_rejected(self):
        with self.assertRaises(ValueError):
            self.compare()


if __name__ == "__main__":
    unittest.main()
