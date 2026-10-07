#!/usr/bin/env python3
"""
Validate E2E test expected outputs against the Darklang interpreter.

This script parses canonical .e2e test files, runs their expressions through
the pinned darklang-interpreter, and compares results.

USAGE:
    python3 scripts/validate-darklang.py                    # Validate all e2e tests
    python3 scripts/validate-darklang.py path/to/test.e2e   # Validate specific file
    python3 scripts/validate-darklang.py --verbose          # Show all results
    python3 scripts/validate-darklang.py --show-failures    # Show only failures

MANUAL VALIDATION:
    Run a single expression (limited support):
        darklang-interpreter eval "<expression>"

    Run a Dark script file (full support):
        darklang-interpreter run <file>.dark

HOW IT WORKS:
    The script generates temporary .dark files for each test and executes them
    via `darklang-interpreter run`. This provides broader syntax support than
    the `eval` command.

    Supported constructs:
    - Canonical function definitions
    - Let bindings
    - Lambda expressions
    - Match expressions

    Results are captured using Builtin.debug for comparison.

TEST CONVERSION EXAMPLE:
    For a test like `let x = 5 in x + 1 = 6`, source is embedded unchanged:

        let __result = let x = 5 in x + 1
        Builtin.debug "" __result
        0

SEE ALSO:
    docs/compatibility/overview.md - Routes compatibility ledgers and skip reasons
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Optional


class TestResult(Enum):
    PASS = "PASS"
    FAIL = "FAIL"
    SKIP = "SKIP"
    ERROR = "ERROR"


@dataclass
class TestCase:
    """Represents a single test case from an .e2e file."""
    line_number: int
    expression: str
    expected: str
    preamble: Optional[str] = None


@dataclass
class ValidationResult:
    """Result of validating a single test case."""
    test: TestCase
    result: TestResult
    actual: Optional[str] = None
    skip_reason: Optional[str] = None
    error_message: Optional[str] = None
    converted_expr: Optional[str] = None
    converted_expected: Optional[str] = None


class FileRunner:
    """Runs Dark code via file-based execution using darklang-interpreter run."""

    def __init__(self, temp_dir: Path):
        self.temp_dir = temp_dir
        self.file_counter = 0

    def run_code(self, code: str) -> tuple[str, Optional[str]]:
        """Run Dark code and return (stdout, error_if_any)."""
        # Generate temp file
        filename = self.temp_dir / f"test_{self.file_counter}.dark"
        self.file_counter += 1

        # Write code to file
        with open(filename, 'w') as f:
            f.write(code)

        try:
            # Execute
            result = subprocess.run(
                ['darklang-interpreter', 'run', str(filename)],
                capture_output=True,
                text=True,
                timeout=10
            )
        except subprocess.TimeoutExpired:
            filename.unlink(missing_ok=True)
            return ("", "Interpreter timeout")
        except FileNotFoundError:
            filename.unlink(missing_ok=True)
            return ("", "darklang-interpreter not found")
        finally:
            # Cleanup
            filename.unlink(missing_ok=True)

        # Parse result
        if result.returncode != 0:
            # Extract error message from stderr
            error_output = result.stderr.strip()
            # Look for "Error: " prefix in output
            for line in error_output.splitlines():
                if line.strip().startswith("Error: "):
                    return ("", line.strip()[len("Error: "):])
            return ("", error_output if error_output else result.stdout.strip())
        return (result.stdout.strip(), None)

    def parse_debug_output(self, output: str) -> Optional[str]:
        """Parse Builtin.debug output to extract the value.

        Builtin.debug outputs: DEBUG: <label>: <value>
        """
        for line in output.splitlines():
            line = line.strip()
            if line.startswith("DEBUG: "):
                # Format is "DEBUG: <label>: <value>"
                # Label can be empty, so we look for ": " after "DEBUG: "
                rest = line[len("DEBUG: "):]
                # Find the label separator (first ": ")
                colon_pos = rest.find(": ")
                if colon_pos >= 0:
                    return rest[colon_pos + 2:]
                return rest
        return None


class E2EParser:
    """Parses .e2e test files."""

    def parse_file(self, filepath: Path) -> list[TestCase]:
        """Parse an .e2e file and return list of test cases."""
        tests = []

        with open(filepath, 'r') as f:
            for line_num, line in enumerate(f, 1):
                line = line.strip()

                # Skip empty lines and comments
                if not line or line.startswith('//'):
                    continue

                # Try to parse as a test line
                test = self._parse_test_line(line, line_num)
                if test:
                    tests.append(test)

        return tests

    def _parse_test_line(self, line: str, line_num: int) -> Optional[TestCase]:
        """Parse a single test line."""
        # Legacy `def` preambles are not canonical source and are not rewritten.
        if line.startswith('def '):
            return None

        # Simple case: expr = expected
        # Need to find the last top-level '=' that separates expr from expected
        equals_pos = self._find_test_equals(line)
        if equals_pos is None:
            return None

        expr = line[:equals_pos].strip()
        expected = line[equals_pos + 1:].strip()

        # Strip trailing comments from expected value
        expected = self._strip_comment(expected)

        if not expr or not expected:
            return None

        return TestCase(
            line_number=line_num,
            expression=expr,
            expected=expected
        )

    def _strip_comment(self, value: str) -> str:
        """Strip trailing // comment from a value."""
        # Find // that's not inside a string
        in_string = False
        string_char = None
        i = 0
        while i < len(value):
            char = value[i]

            if char in '"\'`' and (i == 0 or value[i-1] != '\\'):
                if not in_string:
                    in_string = True
                    string_char = char
                elif char == string_char:
                    in_string = False
                    string_char = None
            elif not in_string and char == '/' and i + 1 < len(value) and value[i + 1] == '/':
                return value[:i].strip()

            i += 1

        return value

    def _find_test_equals(self, line: str) -> Optional[int]:
        """Find the position of the '=' that separates expression from expected value."""
        # We need to find the '=' that's at the top level (not inside parens, brackets, etc.)
        # and separates the test expression from the expected value
        # Also, we stop at // comment markers

        depth = 0
        in_string = False
        string_char = None
        last_equals = None

        i = 0
        while i < len(line):
            char = line[i]

            # Handle string literals
            if char in '"\'`' and (i == 0 or line[i-1] != '\\'):
                if not in_string:
                    in_string = True
                    string_char = char
                elif char == string_char:
                    in_string = False
                    string_char = None
                i += 1
                continue

            if in_string:
                i += 1
                continue

            # Stop at // comment (only at top level)
            if char == '/' and i + 1 < len(line) and line[i + 1] == '/' and depth == 0:
                break

            # Track nesting depth
            if char in '([{':
                depth += 1
            elif char in ')]}':
                depth -= 1
            # Handle == and != operators
            elif char == '=' and depth == 0:
                if i + 1 < len(line) and line[i + 1] == '=':
                    i += 2  # Skip ==
                    continue
                if i > 0 and line[i - 1] in '!<>':
                    i += 1  # Skip part of !=, <=, >=
                    continue
                # This could be our test separator or an assignment in def
                last_equals = i

            i += 1

        return last_equals


class CanonicalSource:
    """Build interpreter programs directly from canonical repository source."""

    def prepare(self, source: str) -> str:
        return source

    def generate_file_code(self, expr: str, preamble: Optional[str] = None) -> str:
        lines = []
        if preamble:
            lines.append(preamble)
        lines.append(f"let __result = {expr}")
        lines.append('Builtin.debug "" __result')
        lines.append("0")
        return "\n".join(lines)


class Validator:
    """Validates test cases against the Darklang interpreter."""

    def __init__(self, verbose: bool = False, temp_dir: Optional[Path] = None):
        self.source = CanonicalSource()
        self.verbose = verbose
        # Create temp directory for file-based execution
        if temp_dir is None:
            self._temp_dir_obj = tempfile.TemporaryDirectory()
            self.temp_dir = Path(self._temp_dir_obj.name)
        else:
            self._temp_dir_obj = None
            self.temp_dir = temp_dir
        self.file_runner = FileRunner(self.temp_dir)

    def cleanup(self):
        """Clean up temporary directory."""
        if self._temp_dir_obj:
            self._temp_dir_obj.cleanup()

    def validate_test(self, test: TestCase) -> ValidationResult:
        """Validate a single test case."""
        # Check for skip conditions
        skip_reason = self._should_skip(test)
        if skip_reason:
            return ValidationResult(
                test=test,
                result=TestResult.SKIP,
                skip_reason=skip_reason
            )

        # Embed canonical syntax directly and generate file code.
        try:
            expr_for_run = self._strip_error_expr(test.expression)
            file_code = self.source.generate_file_code(expr_for_run, test.preamble)
            converted_expected = self.source.prepare(test.expected)
            converted_expr = self.source.prepare(expr_for_run)
        except Exception as e:
            return ValidationResult(
                test=test,
                result=TestResult.ERROR,
                error_message=f"Source preparation error: {e}"
            )

        expected_error = self._expected_error_message(test.expression, converted_expected)

        # Run through interpreter via file
        stdout, run_error = self.file_runner.run_code(file_code)

        if expected_error is not None:
            # We expect an error
            if run_error is None:
                # Got success when we expected error
                actual = self.file_runner.parse_debug_output(stdout)
                return ValidationResult(
                    test=test,
                    result=TestResult.FAIL,
                    actual=actual or stdout,
                    converted_expr=converted_expr,
                    converted_expected=converted_expected
                )
            if self._compare_error(run_error, expected_error):
                return ValidationResult(
                    test=test,
                    result=TestResult.PASS,
                    actual=run_error,
                    converted_expr=converted_expr,
                    converted_expected=converted_expected
                )
            else:
                return ValidationResult(
                    test=test,
                    result=TestResult.FAIL,
                    actual=run_error,
                    converted_expr=converted_expr,
                    converted_expected=converted_expected
                )

        # We expect a value, not an error
        if run_error is not None:
            return ValidationResult(
                test=test,
                result=TestResult.ERROR,
                error_message=run_error,
                converted_expr=converted_expr,
                converted_expected=converted_expected
            )

        # Parse the debug output to get the actual value
        actual = self.file_runner.parse_debug_output(stdout)
        if actual is None:
            return ValidationResult(
                test=test,
                result=TestResult.ERROR,
                error_message=f"Could not parse debug output: {stdout}",
                converted_expr=converted_expr,
                converted_expected=converted_expected
            )

        # Compare results
        if self._compare_results(actual, converted_expected):
            return ValidationResult(
                test=test,
                result=TestResult.PASS,
                actual=actual,
                converted_expr=converted_expr,
                converted_expected=converted_expected
            )
        else:
            return ValidationResult(
                test=test,
                result=TestResult.FAIL,
                actual=actual,
                converted_expr=converted_expr,
                converted_expected=converted_expected
            )

    def _expected_error_message(self, expr: str, expected: str) -> Optional[str]:
        """Extract expected error message from an expected value."""
        if re.search(r'\s*=\s*error\s*$', expr):
            return self._strip_error_quotes(expected.strip())

        normalized = expected.strip()
        if normalized == "error":
            return ""
        if normalized.startswith("error="):
            msg = normalized[len("error="):].strip()
            return self._strip_error_quotes(msg)
        return None

    def _strip_error_quotes(self, msg: str) -> str:
        """Strip surrounding quotes from an error message."""
        if (msg.startswith('"') and msg.endswith('"')) or (msg.startswith("'") and msg.endswith("'")):
            return msg[1:-1]
        return msg

    def _is_supported_error(self, msg: str) -> bool:
        """Check if an error message is supported for interpreter validation."""
        return msg in {"&& only supports Booleans", "|| only supports Booleans"}

    def _compare_error(self, actual: str, expected: str) -> bool:
        """Compare actual vs expected error messages."""
        return actual.strip() == expected.strip()

    def _should_skip(self, test: TestCase) -> Optional[str]:
        """Check if test should be skipped.

        See docs/compatibility/overview.md for the compatibility boundary and skip reasons.

        Skip reason categories cover tooling limitations, known semantic bugs,
        missing interpreter stdlib functions, and compiler-internal APIs.
        """
        expr = test.expression
        expected = test.expected

        # === TOOLING DIFFERENCES ===
        # Tests that check error conditions or output that can't be validated
        if 'expect_compile_error' in expected:
            return "eval:compile_error"
        expected_error = self._expected_error_message(expr, expected)
        if expected_error is not None and not self._is_supported_error(expected_error):
            return "eval:error_result"
        if (expected.strip() == 'error' or
            (expected.startswith('"') and 'error' in expected.lower()) or
            (expected_error is None and '= error' in expr)):
            return "eval:error_result"
        if 'stdout=' in expected:
            return "eval:stdout"
        if 'stderr=' in expected:
            return "eval:stderr"
        if 'exit=' in expected:
            return "eval:exit_code"
        if 'Builtin.test' in expr or 'Builtin.test' in expected:
            return "eval:builtin_test"

        # === SEMANTIC BUGS (compiler produces wrong output) ===
        if re.search(r'-?\d+\.\d{3,}', expr):
            return "eval:float_precision"
        # === COMPILER-ONLY INTERNAL FEATURES ===
        if 'Stdlib.__SkewList' in expr or 'Stdlib.__HAMT' in expr:
            return "internal:data_structure"
        if '.__' in expr:
            return "internal:helper_function"

        # === VALIDATION LIMITATIONS ===
        # These are not interpreter differences, but tests we can't validate
        # because they reference types/functions not available to the interpreter

        # Custom types/enums (not defined in the test itself)
        allowed_modules = {'Int64', 'Int32', 'Int16', 'Int8', 'UInt64', 'UInt32', 'UInt16', 'UInt8',
                          'Float', 'String', 'List', 'Dict', 'Option', 'Result', 'Bool', 'Char',
                          'Tuple2', 'Tuple3', 'Math', 'Bytes', 'Base64', 'Uuid', 'Stdlib', 'Some', 'None', 'Ok', 'Error'}
        pascal_matches = re.findall(r'\b([A-Z][a-z]+[A-Za-z]*)\b', expr)
        for pascal_name in pascal_matches:
            if pascal_name not in allowed_modules:
                return f"run:custom_type:{pascal_name}"

        # User-defined functions (not defined in the test preamble)
        func_call_pattern = r'(?<!\.)\b([a-z][a-zA-Z0-9]*)\s*\('
        func_matches = re.findall(func_call_pattern, expr)
        allowed_funcs = {'if', 'match', 'fun', 'let', 'in', 'true', 'false'}
        for func in func_matches:
            if func not in allowed_funcs:
                if test.preamble and f'def {func}' in test.preamble:
                    continue
                return f"run:user_function:{func}"

        return None

    def _strip_error_expr(self, expr: str) -> str:
        """Strip trailing '= error' from test expressions."""
        return re.sub(r'\s*=\s*error\s*$', '', expr).strip()

    def _compare_results(self, actual: str, expected: str) -> bool:
        """Compare actual output with expected value."""
        # Normalize both for comparison
        actual_norm = self._normalize_value(actual)
        expected_norm = self._normalize_value(expected)

        return actual_norm == expected_norm

    def _normalize_value(self, value: str) -> str:
        """Normalize a value for comparison."""
        result = value.strip()

        # Remove L suffix from integers for comparison
        result = re.sub(r'(\d+)L\b', r'\1', result)

        # Strip surrounding double quotes for string comparison
        # Darklang interpreter outputs strings without quotes
        if result.startswith('"') and result.endswith('"'):
            result = result[1:-1]

        # Strip interpreter type prefixes (e.g., "<Int64>.Some(0)" -> "Some(0)")
        result = re.sub(r'^<[^>]+>\.', '', result)

        # Normalize whitespace in lists/tuples
        result = re.sub(r'\s*,\s*', ', ', result)
        result = re.sub(r'\s*;\s*', '; ', result)

        # Normalize list output format (Darklang uses multiline for lists)
        if result.startswith('[') and '\n' in result:
            # Flatten multiline list output
            result = re.sub(r'\[\s*\n\s*', '[', result)
            result = re.sub(r'\s*\n\s*\]', ']', result)
            result = re.sub(r',\s*\n\s*', ', ', result)

        return result


def find_e2e_files(base_path: Path) -> list[Path]:
    """Find all .e2e files in the given path."""
    if base_path.is_file():
        return [base_path] if base_path.suffix == '.e2e' else []

    return sorted(base_path.rglob('*.e2e'))


def print_summary(results: dict[Path, list[ValidationResult]]):
    """Print summary of validation results."""
    total_pass = 0
    total_fail = 0
    total_skip = 0
    total_error = 0

    print("\n" + "=" * 60)
    print("VALIDATION SUMMARY")
    print("=" * 60)

    for filepath, file_results in sorted(results.items()):
        passed = sum(1 for r in file_results if r.result == TestResult.PASS)
        failed = sum(1 for r in file_results if r.result == TestResult.FAIL)
        skipped = sum(1 for r in file_results if r.result == TestResult.SKIP)
        errored = sum(1 for r in file_results if r.result == TestResult.ERROR)

        total_pass += passed
        total_fail += failed
        total_skip += skipped
        total_error += errored

        status = "OK" if failed == 0 and errored == 0 else "FAIL"
        rel_path = filepath.name
        print(f"  {rel_path}: {status} (pass={passed}, fail={failed}, skip={skipped}, error={errored})")

    print("-" * 60)
    print(f"TOTAL: pass={total_pass}, fail={total_fail}, skip={total_skip}, error={total_error}")

    if total_fail > 0 or total_error > 0:
        print("\nValidation FAILED")
        return 1
    else:
        print("\nValidation PASSED")
        return 0


def main():
    parser = argparse.ArgumentParser(
        description="Validate E2E test expected outputs against Darklang interpreter",
        epilog="Run with --help-full for detailed documentation.",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        '--help-full',
        action='store_true',
        help='Show full documentation'
    )
    parser.add_argument(
        'paths',
        nargs='*',
        default=['test/fixtures/e2e'],
        help='Files or directories to validate'
    )
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Show all test results'
    )
    parser.add_argument(
        '--show-failures', '-f',
        action='store_true',
        help='Show only failures'
    )

    args = parser.parse_args()

    if args.help_full:
        print(__doc__)
        return 0

    # Find all e2e files
    all_files = []
    for path_str in args.paths:
        path = Path(path_str)
        if not path.exists():
            print(f"Warning: Path does not exist: {path}")
            continue
        all_files.extend(find_e2e_files(path))

    if not all_files:
        print("No .e2e files found")
        return 1

    # Parse and validate
    e2e_parser = E2EParser()
    validator = Validator(verbose=args.verbose)

    try:
        all_results: dict[Path, list[ValidationResult]] = {}

        for filepath in all_files:
            print(f"\nValidating: {filepath.name}")

            try:
                tests = e2e_parser.parse_file(filepath)
            except Exception as e:
                print(f"  Error parsing file: {e}")
                continue

            file_results = []
            for test in tests:
                result = validator.validate_test(test)
                file_results.append(result)

                # Print result based on verbosity
                if args.verbose or (args.show_failures and result.result in (TestResult.FAIL, TestResult.ERROR)):
                    status_str = result.result.value
                    print(f"  Line {test.line_number}: {status_str}")

                    if result.result == TestResult.FAIL:
                        print(f"    Expected: {test.expected}")
                        print(f"    Actual:   {result.actual}")
                    elif result.result == TestResult.ERROR:
                        print(f"    Error: {result.error_message}")
                    elif result.result == TestResult.SKIP:
                        print(f"    Reason: {result.skip_reason}")

            all_results[filepath] = file_results

            # Quick summary for this file
            if not args.verbose:
                passed = sum(1 for r in file_results if r.result == TestResult.PASS)
                failed = sum(1 for r in file_results if r.result == TestResult.FAIL)
                skipped = sum(1 for r in file_results if r.result == TestResult.SKIP)
                errored = sum(1 for r in file_results if r.result == TestResult.ERROR)
                print(f"  Results: pass={passed}, fail={failed}, skip={skipped}, error={errored}")

        return print_summary(all_results)
    finally:
        validator.cleanup()


if __name__ == '__main__':
    sys.exit(main())
