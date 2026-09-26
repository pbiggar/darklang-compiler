#!/usr/bin/env python3
"""Decision tests for the parent-relative full-suite timing gate."""

from __future__ import annotations

import unittest
from unittest import mock

from scripts import test_runtime_gate as gate


class RuntimeGateTests(unittest.TestCase):
    def test_relative_limit_is_strict_and_independent(self) -> None:
        self.assertFalse(gate.exceeds_limit(300.0, 330.0))
        self.assertTrue(gate.exceeds_limit(300.0, 330.01))

    def test_absolute_limit_is_strict_and_independent(self) -> None:
        self.assertFalse(gate.exceeds_limit(900.0, 960.0))
        self.assertTrue(gate.exceeds_limit(900.0, 960.01))

    def test_competing_cpu_time_is_excluded_from_valid_samples(self) -> None:
        ticks = __import__("os").sysconf("SC_CLK_TCK")
        self.assertEqual(gate.competing_cpu_seconds(100 * ticks, 98.0), 2.0)
        self.assertEqual(gate.competing_cpu_seconds(100 * ticks, 101.0), 0.0)

    def test_mismatched_commit_or_host_is_not_accepted(self) -> None:
        sample = {"schema": gate.SCHEMA, "commit": "candidate",
                  "host": "host", "elapsed_seconds": 42.0}
        self.assertEqual(gate.validate_metric(sample, "candidate", "host"), sample)
        with self.assertRaises(gate.GateError):
            gate.validate_metric(sample, "parent", "host")
        with self.assertRaises(gate.GateError):
            gate.validate_metric({**sample, "host": "different"}, "candidate", "host")

    def test_dirty_worktree_cannot_supply_a_cached_commit_measurement(self) -> None:
        with mock.patch.object(gate, "git", side_effect=["candidate", " M source.fs"]):
            with self.assertRaisesRegex(gate.GateError, "clean committed worktree"):
                gate.require_clean_commit(__import__("pathlib").Path("/unused"))


if __name__ == "__main__":
    unittest.main()
