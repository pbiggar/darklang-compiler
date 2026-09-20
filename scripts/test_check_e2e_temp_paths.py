"""Focused tests for the E2E temporary-path policy."""

import unittest

from scripts.check_e2e_temp_paths import violations


class E2ETempPathPolicyTests(unittest.TestCase):
    def test_rejects_fixed_writable_temp_path(self) -> None:
        self.assertEqual(
            violations('let path = "/tmp/shared-test" in writeFile path value'),
            [(1, 'let path = "/tmp/shared-test" in writeFile path value')],
        )

    def test_allows_per_process_interpolated_path(self) -> None:
        source = (
            'let path = $"/tmp/test-{Stdlib.Int.toString '
            '(Stdlib.Cli.Sys.currentPid ())}" in writeFile path value'
        )
        self.assertEqual(violations(source), [])

    def test_allows_read_only_system_path(self) -> None:
        self.assertEqual(violations('isDirectory "/tmp" = true'), [])


if __name__ == "__main__":
    unittest.main()
