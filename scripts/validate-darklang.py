#!/usr/bin/env python3
"""
Validate E2E test expected outputs against the Darklang interpreter.

This script parses .e2e test files, converts compiler syntax to Darklang syntax,
runs expressions through the darklang-interpreter, and compares results.

USAGE:
    python3 scripts/validate-darklang.py                    # Validate all e2e tests
    python3 scripts/validate-darklang.py path/to/test.e2e   # Validate specific file
    python3 scripts/validate-darklang.py --verbose          # Show all results
    python3 scripts/validate-darklang.py --show-conversions # Show syntax conversions
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
    - Function definitions (converted to curried lambdas)
    - Let bindings
    - Lambda expressions
    - Match expressions

    Results are captured using Builtin.debug for comparison.

SYNTAX CONVERSIONS:
    The script automatically converts compiler syntax to Darklang syntax:
    - Integer literals: 5 -> 5L
    - List separators: [1, 2] -> [1L; 2L]
    - Function calls: Module.fn(a, b) -> Stdlib.Module.fn a b
    - Lambdas: (x: T) => body -> fun x -> body

TEST CONVERSION EXAMPLE:
    For a test like `let x = 5 in x + 1 = 6`:

        let __result = let x = 5L in x + 1L
        Builtin.debug "" __result
        0L

    For tests with `def` preambles:

        // Original: def add(a: Int64, b: Int64) : Int64 = a + b add(1, 2) = 3
        let add = fun a -> fun b -> a + b
        let __result = add 1L 2L
        Builtin.debug "" __result
        0L

SEE ALSO:
    docs/darklang-differences.md - Documents all known differences and skip reasons
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
    preamble: Optional[str] = None  # e.g., "def foo(...) = ..."


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

    # Pattern for test line: expression = expected
    # Handle inline `def` definitions that precede the test expression
    TEST_PATTERN = re.compile(
        r'^(?P<preamble>(?:def\s+\w+.*?=\s+.*?\s+)+)?'  # Optional def preamble
        r'(?P<expr>.+?)\s*=\s*(?P<expected>.+)$'
    )

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
        # Handle lines with inline def definitions
        # Format: def foo(x: T): T = body expr = expected

        # First, check if line contains a def
        if line.startswith('def '):
            # Find the last '=' that's followed by expected value
            # We need to handle nested def statements
            return self._parse_line_with_def(line, line_num)

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

    def _parse_line_with_def(self, line: str, line_num: int) -> Optional[TestCase]:
        """Parse a line that starts with def."""
        # Pattern: def name(params): RetType = body <actual_expr> = <expected>
        # The challenge is finding where the def body ends and the test expr begins

        # Strategy: Find the pattern where we have the function body followed by
        # a function call or expression, then = expected

        # Look for common patterns like "def foo(...) = ... foo(...) = expected"
        # We need to match the function name and find its call

        # Extract function name
        match = re.match(r'def\s+(\w+)', line)
        if not match:
            return None
        func_name = match.group(1)

        # Find where the function definition ends
        # Look for the function call pattern after the def
        # Pattern: after "def name(...) = body" we should see "name(" or an expression

        # Find all occurrences of the function name
        # The last meaningful '=' should be our test separator

        # Simple heuristic: find the rightmost ' = ' where right side looks like a value
        equals_pos = self._find_test_equals(line)
        if equals_pos is None:
            return None

        preamble_and_expr = line[:equals_pos].strip()
        expected = line[equals_pos + 1:].strip()

        # Now separate preamble (def) from the test expression
        # Look for the function call or expression after the def body

        # Find where the function body ends - usually right before function is called
        # or where a standalone expression begins

        # Use a pattern to find "def...= body func_name(" or similar
        def_pattern = rf'(def\s+{func_name}\s*\([^)]*\)\s*(?::\s*\w+)?\s*=\s*.+?)\s+({func_name}\s*\(.+)$'
        def_match = re.match(def_pattern, preamble_and_expr)

        if def_match:
            preamble = def_match.group(1).strip()
            expr = def_match.group(2).strip()
        else:
            # Try alternate pattern for multiple defs or different structures
            # For now, treat the whole left side as the expression with embedded def
            preamble = None
            expr = preamble_and_expr

        return TestCase(
            line_number=line_num,
            expression=expr,
            expected=expected,
            preamble=preamble
        )

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


class SyntaxConverter:
    """Converts Ralph2 syntax to Darklang syntax."""

    # Mapping of Ralph2 stdlib modules to Darklang
    STDLIB_MODULES = {
        'Int64': 'Stdlib.Int64',
        'Int32': 'Stdlib.Int32',
        'Int16': 'Stdlib.Int16',
        'Int8': 'Stdlib.Int8',
        'UInt64': 'Stdlib.UInt64',
        'UInt32': 'Stdlib.UInt32',
        'UInt16': 'Stdlib.UInt16',
        'UInt8': 'Stdlib.UInt8',
        'Float': 'Stdlib.Float',
        'String': 'Stdlib.String',
        'List': 'Stdlib.List',
        'Dict': 'Stdlib.Dict',
        'Option': 'Stdlib.Option',
        'Result': 'Stdlib.Result',
        'Bool': 'Stdlib.Bool',
        'Char': 'Stdlib.Char',
        'Tuple2': 'Stdlib.Tuple2',
        'Tuple3': 'Stdlib.Tuple3',
        'Math': 'Stdlib.Math',
        'Bytes': 'Stdlib.Bytes',
        'Base64': 'Stdlib.Base64',
        'Uuid': 'Stdlib.Uuid',
    }

    def convert(self, expr: str) -> str:
        """Convert Ralph2 expression to Darklang syntax."""
        result = expr

        # Convert stdlib function calls: Module.func(args) -> Stdlib.Module.func args
        result = self._convert_stdlib_calls(result)

        # Convert lambdas: (x: Int64) => body -> fun x -> body
        result = self._convert_lambdas(result)

        # Convert list syntax: [1, 2, 3] -> [1L; 2L; 3L]
        result = self._convert_lists(result)

        # Convert integer literals: add L suffix
        result = self._convert_int_literals(result)

        # Convert let...in to block syntax: let x = e in body -> (let x = e\nbody)
        result = self._convert_let_in(result)

        return result

    def _convert_let_in(self, expr: str) -> str:
        """Convert let...in expressions to Darklang block syntax.

        Darklang doesn't use 'in' keyword. Instead:
            let x = 5 in x + 1
        becomes:
            (let x = 5L
            x + 1L)
        """
        result = expr

        # Process from right to left to handle nested let...in properly
        # Each iteration handles the rightmost let...in
        max_iterations = 20  # Prevent infinite loops
        iteration = 0

        while ' in ' in result and iteration < max_iterations:
            iteration += 1

            # Find the rightmost ' in ' that's preceded by 'let ... = ...'
            # Work backwards to find the matching let
            in_pos = self._find_rightmost_let_in(result)
            if in_pos is None:
                break

            # Find the matching 'let' for this 'in'
            let_info = self._find_matching_let(result, in_pos)
            if let_info is None:
                break

            let_start, var_name, value_end = let_info
            value = result[value_end:in_pos].strip()

            # Body is everything after ' in '
            body_start = in_pos + 4  # len(' in ')
            body = self._extract_let_body(result, body_start)

            if body is None:
                break

            body_end = body_start + len(body)

            # Build the replacement: (let var = value\nbody)
            replacement = f"(let {var_name} = {value}\n{body})"

            # Replace in result
            result = result[:let_start] + replacement + result[body_end:]

        return result

    def _find_rightmost_let_in(self, expr: str) -> Optional[int]:
        """Find the position of the rightmost ' in ' that's part of a let binding."""
        # Search backwards for ' in '
        pos = len(expr)
        while True:
            pos = expr.rfind(' in ', 0, pos)
            if pos == -1:
                return None
            # Check if there's a 'let' before this 'in'
            prefix = expr[:pos]
            if 'let ' in prefix:
                return pos
            pos -= 1
        return None

    def _find_matching_let(self, expr: str, in_pos: int) -> Optional[tuple[int, str, int]]:
        """Find the let that matches this 'in' position.

        Returns (let_start, var_name, value_start) or None.
        """
        # Look backwards from in_pos for 'let <var> ='
        # The tricky part is handling nested let expressions

        prefix = expr[:in_pos]

        # Find the rightmost 'let' that's not inside a nested block
        depth = 0
        i = len(prefix) - 1
        while i >= 0:
            if prefix[i] == ')':
                depth += 1
            elif prefix[i] == '(':
                depth -= 1
            elif depth == 0:
                # Check for 'let '
                if i >= 3 and prefix[i-3:i+1] == 'let ':
                    # Found potential let, extract var name and value start
                    match = re.match(r'(\w+)\s*=\s*', prefix[i+1:])
                    if match:
                        var_name = match.group(1)
                        value_start = i + 1 + match.end()
                        return (i - 3, var_name, value_start)
            i -= 1

        return None

    def _extract_let_body(self, expr: str, start: int) -> Optional[str]:
        """Extract the body of a let...in expression starting at position start."""
        depth = 0
        in_string = False
        string_char = None
        i = start

        while i < len(expr):
            char = expr[i]

            # Handle string literals
            if char in '"\'`' and (i == 0 or expr[i-1] != '\\'):
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

            # Track nesting
            if char in '([{':
                depth += 1
            elif char in ')]}':
                if depth == 0:
                    # End of body
                    return expr[start:i]
                depth -= 1
            elif char == '=' and depth == 0:
                # Check if this is the test separator (= expected)
                # Look for ' = ' pattern that's not '=='
                if i + 1 < len(expr) and expr[i + 1] != '=':
                    if i > 0 and expr[i - 1] not in '!<>':
                        # This might be the test separator
                        return expr[start:i].rstrip()

            i += 1

        # Rest of expression is the body
        return expr[start:]

    def _convert_stdlib_calls(self, expr: str) -> str:
        """Convert Ralph2 stdlib calls to Darklang syntax."""
        result = expr

        # Pattern: Module.func(arg1, arg2, ...)
        # Convert to: Stdlib.Module.func arg1 arg2 ...

        for ralph_mod, dark_mod in self.STDLIB_MODULES.items():
            # Match Module.function(args) but NOT if already preceded by Stdlib.
            # Negative lookbehind for 'Stdlib.'
            pattern = rf'(?<!Stdlib\.)\b{ralph_mod}\.(\w+)\s*\(([^)]*)\)'

            def replace_call(m):
                func_name = m.group(1)
                args_str = m.group(2).strip()

                if not args_str:
                    return f'{dark_mod}.{func_name}'

                # Parse arguments - need to handle nested parens
                args = self._split_args(args_str)
                args_converted = ' '.join(args)
                return f'{dark_mod}.{func_name} {args_converted}'

            result = re.sub(pattern, replace_call, result)

        # Also convert existing Stdlib.Module.func(args) to Stdlib.Module.func args
        pattern = r'Stdlib\.(\w+)\.(\w+)\s*\(([^)]*)\)'

        def convert_stdlib_parens(m):
            module = m.group(1)
            func_name = m.group(2)
            args_str = m.group(3).strip()

            if not args_str:
                return f'Stdlib.{module}.{func_name}'

            args = self._split_args(args_str)
            args_converted = ' '.join(args)
            return f'Stdlib.{module}.{func_name} {args_converted}'

        result = re.sub(pattern, convert_stdlib_parens, result)

        return result

    def _split_args(self, args_str: str) -> list[str]:
        """Split function arguments, respecting nested parentheses."""
        args = []
        current = []
        depth = 0
        in_string = False
        string_char = None

        for i, char in enumerate(args_str):
            if char in '"\'`' and (i == 0 or args_str[i-1] != '\\'):
                if not in_string:
                    in_string = True
                    string_char = char
                elif char == string_char:
                    in_string = False
                    string_char = None
                current.append(char)
            elif in_string:
                current.append(char)
            elif char in '([{':
                depth += 1
                current.append(char)
            elif char in ')]}':
                depth -= 1
                current.append(char)
            elif char == ',' and depth == 0:
                args.append(''.join(current).strip())
                current = []
            else:
                current.append(char)

        if current:
            args.append(''.join(current).strip())

        return args

    def _convert_lambdas(self, expr: str) -> str:
        """Convert Ralph2 lambda syntax to Darklang."""
        result = expr

        # Pattern: (x: Type) => body
        # Convert to: fun x -> body

        # Single parameter lambda: (x: Type) => body
        single_param = r'\((\w+)\s*:\s*[\w\[\](),<>\s]+\)\s*=>'
        result = re.sub(single_param, r'fun \1 ->', result)

        # Multi-parameter lambda: (x: T1, y: T2) => body
        # Convert to: fun x y -> body
        multi_param = r'\((\w+)\s*:\s*[\w\[\](),<>\s]+(?:\s*,\s*(\w+)\s*:\s*[\w\[\](),<>\s]+)+\)\s*=>'

        def replace_multi_lambda(m):
            full_match = m.group(0)
            # Extract all parameter names
            param_pattern = r'(\w+)\s*:\s*[\w\[\](),<>\s]+'
            params = re.findall(param_pattern, full_match.rstrip('=>').strip('() '))
            return f'fun {" ".join(params)} ->'

        result = re.sub(multi_param, replace_multi_lambda, result)

        return result

    def _convert_lists(self, expr: str) -> str:
        """Convert list syntax: [1, 2, 3] -> [1L; 2L; 3L]."""
        result = expr

        # Find list literals and convert comma to semicolon
        # This is tricky because we need to handle nested structures

        def convert_list_content(match):
            content = match.group(1)
            if not content.strip():
                return '[]'
            # Split by comma (respecting nesting) and join with semicolon
            items = self._split_args(content)
            return '[' + '; '.join(items) + ']'

        # Simple pattern for list literals
        # This won't handle deeply nested cases perfectly
        result = re.sub(r'\[([^\[\]]*)\]', convert_list_content, result)

        return result

    def _convert_int_literals(self, expr: str) -> str:
        """Add L suffix to integer literals."""
        result = expr

        # Match integers that don't already have a suffix
        # Avoid matching:
        # - Floats (have decimal point)
        # - Already suffixed (L, l, s, y)
        # - Part of identifiers

        # Pattern: word boundary, optional minus, digits, word boundary
        # But not followed by L/l/s/y/. and not preceded by letter

        def add_l_suffix(m):
            # Check what comes before and after
            num = m.group(0)
            return num + 'L'

        # Match standalone integers
        # Negative lookahead for decimal point or existing suffix
        # Negative lookbehind for letters, underscore, or decimal point (to avoid matching in identifiers or floats)
        result = re.sub(
            r'(?<![\w\.])(-?\d+)(?![\w\.])',
            add_l_suffix,
            result
        )

        return result

    def convert_expected(self, expected: str) -> str:
        """Convert expected value to Darklang format."""
        result = expected

        # Convert lists
        result = self._convert_lists(result)

        # Convert integers in expected values
        result = self._convert_int_literals(result)

        return result

    def convert_def_to_lambda(self, preamble: str) -> str:
        """Convert def statements to curried lambda form for file mode.

        Example:
            def add(a: Int64, b: Int64) : Int64 = a + b
        Becomes:
            let add = fun a -> fun b -> a + b
        """
        result_lines = []

        # Pattern to match: def name(params) : RetType = body
        # We need to handle multiple defs
        def_pattern = re.compile(
            r'def\s+(\w+)\s*\(([^)]*)\)\s*(?::\s*[\w\[\]<>,\s]+)?\s*=\s*(.+?)(?=\s+def\s|\s*$)',
            re.DOTALL
        )

        for match in def_pattern.finditer(preamble):
            func_name = match.group(1)
            params_str = match.group(2).strip()
            body = match.group(3).strip()

            # Parse parameters (name: Type, ...)
            param_names = []
            if params_str:
                for param in params_str.split(','):
                    param = param.strip()
                    if ':' in param:
                        param_name = param.split(':')[0].strip()
                        param_names.append(param_name)
                    elif param:
                        param_names.append(param)

            # Convert body
            converted_body = self.convert(body)

            # Build curried lambda: fun a -> fun b -> body
            if param_names:
                lambda_expr = converted_body
                for param in reversed(param_names):
                    lambda_expr = f"fun {param} -> {lambda_expr}"
                result_lines.append(f"let {func_name} = {lambda_expr}")
            else:
                result_lines.append(f"let {func_name} = {converted_body}")

        return '\n'.join(result_lines)

    def generate_file_code(self, expr: str, preamble: Optional[str] = None) -> str:
        """Generate complete .dark file code for file-based execution.

        Uses Builtin.debug to output the result, which works for any type.

        Example for expr "let x = 5 in x + 1":
            let __result = let x = 5L in x + 1L
            Builtin.debug "" __result
            0L
        """
        lines = []

        # Add converted preamble (def -> lambda)
        if preamble:
            converted_preamble = self.convert_def_to_lambda(preamble)
            lines.append(converted_preamble)

        # Convert the main expression
        converted_expr = self.convert(expr)

        # Wrap in result capture and debug output
        lines.append(f"let __result = {converted_expr}")
        lines.append('Builtin.debug "" __result')
        lines.append("0L")

        return '\n'.join(lines)


class Validator:
    """Validates test cases against the Darklang interpreter."""

    def __init__(self, verbose: bool = False, show_conversions: bool = False, temp_dir: Optional[Path] = None):
        self.converter = SyntaxConverter()
        self.verbose = verbose
        self.show_conversions = show_conversions
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

        # Convert syntax and generate file code
        try:
            expr_for_run = self._strip_error_expr(test.expression)
            file_code = self.converter.generate_file_code(expr_for_run, test.preamble)
            converted_expected = self.converter.convert_expected(test.expected)
            # For display purposes, show the converted expression
            converted_expr = self.converter.convert(expr_for_run)
        except Exception as e:
            return ValidationResult(
                test=test,
                result=TestResult.ERROR,
                error_message=f"Conversion error: {e}"
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

        See docs/darklang-differences.md for detailed explanations of each skip reason.

        Skip reason categories:
        - Tooling differences: eval:* reasons (error testing, output capture)
        - Syntactic differences: syntax:* reasons (unsupported syntax in interpreter)
        - Semantic bugs: semantic:*, eval:float_precision
        - Missing from interpreter: semantic:bitwise, semantic:boolean_not, stdlib:*
        - Compiler-only features: extension:*, internal:* reasons
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
        # === MISSING FROM INTERPRETER ===
        # Features implemented in compiler that should be added to interpreter
        if ('<<' in expr or '>>' in expr or '^' in expr or '~' in expr or
            re.search(r'(?<!&)&(?!&)', expr) or
            re.search(r'(?<!\|)\|(?!\|)', expr)):
            return "semantic:bitwise"
        if '!' in expr:
            return "semantic:boolean_not"

        # Stdlib functions missing from interpreter
        missing_stdlib_map = {
            'Random.': 'stdlib:random',              # Random.int64
            '.getByteAt': 'stdlib:byte_ops',         # String.getByteAt
            '.take': 'stdlib:missing',               # List.take, String.take
            '.drop': 'stdlib:missing',               # List.drop, String.drop
            '.substring': 'stdlib:missing',          # String.substring
            '.slice': 'stdlib:slice',                # String.slice (different semantics)
            'Int64.sub': 'stdlib:int64_math',
            'Int64.mul': 'stdlib:int64_math',
            'Int64.div': 'stdlib:int64_math',
            'Int64.isEven': 'stdlib:int64_math',
            'Int64.isOdd': 'stdlib:int64_math',
            'Float.toBits': 'stdlib:float_ops',      # Float.toBits - IEEE 754 bit representation
            'Float.toString': 'stdlib:float_ops',    # Float.toString - full precision float to string
            'Float.toInt': 'stdlib:float_ops',       # Float.toInt - conversion
            'Float.abs': 'stdlib:float_ops',         # Float.abs - absolute value
            'Float.negate': 'stdlib:float_ops',      # Float.negate - displays -0.0 as 0.0
            'Float.sqrt': 'stdlib:float_ops',        # Float.sqrt - interpreter uses different function
        }
        for pattern, reason in missing_stdlib_map.items():
            if pattern in expr:
                return reason

        # === COMPILER-ONLY INTERNAL FEATURES ===
        if 'Stdlib.FingerTree' in expr or 'Stdlib.__HAMT' in expr:
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
        default=['src/Tests/e2e'],
        help='Files or directories to validate'
    )
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Show all test results'
    )
    parser.add_argument(
        '--show-conversions', '-c',
        action='store_true',
        help='Show syntax conversions'
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
    validator = Validator(
        verbose=args.verbose,
        show_conversions=args.show_conversions
    )

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

                    if args.show_conversions and result.converted_expr:
                        print(f"    Ralph2:   {test.expression}")
                        print(f"    Darklang: {result.converted_expr}")

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
