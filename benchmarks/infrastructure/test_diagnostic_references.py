#!/usr/bin/env python3
"""Focused tests for diagnostic reference benchmark measurement."""

import re
import unittest
from pathlib import Path

from diagnostic_references import (
    adapt_interpreter_source,
    inject_interpreter_arguments,
    parse_instruction_count,
    source_path,
)
from benchmark_profiles import load_invocation, load_profile


class DiagnosticReferenceTests(unittest.TestCase):
    def test_interpreter_arguments_replace_each_cli_lookup(self) -> None:
        source = (
            "match (Stdlib.Cli.__Args.int64 0, "
            "Stdlib.Cli.__Args.int64 1) with\n"
            "| (Ok first, Ok second) -> first + second\n"
        )

        transformed = inject_interpreter_arguments(source, ("4", "7"))

        self.assertIn("match (Ok 4L, Ok 7L) with", transformed)
        self.assertNotIn("Stdlib.Cli.__Args.int64", transformed)

    def test_interpreter_argument_injection_rejects_an_unknown_index(self) -> None:
        with self.assertRaisesRegex(ValueError, "argument index 2"):
            inject_interpreter_arguments(
                "Stdlib.Cli.__Args.int64 2", ("4", "7")
            )

    def test_interpreter_arguments_support_the_shared_index_helper(self) -> None:
        transformed = inject_interpreter_arguments(
            "match Stdlib.Cli.__Args.int64 index with | Ok value -> value",
            ("4", "7"),
        )

        self.assertIn("match index with | 0 -> Ok 4L | 1 -> Ok 7L", transformed)

    def test_instruction_count_requires_one_positive_cachegrind_summary(self) -> None:
        self.assertEqual(parse_instruction_count("==1== I refs: 12,345\n"), 12345)
        with self.assertRaisesRegex(ValueError, "exactly one"):
            parse_instruction_count("==1== I refs: 12\n==2== I refs: 13\n")

    def test_interpreter_adapter_covers_each_full_workload(self) -> None:
        benchmarks_dir = Path(__file__).resolve().parent.parent
        for name in load_profile(benchmarks_dir, "full"):
            with self.subTest(name=name):
                invocation = load_invocation(benchmarks_dir, "full", name)
                source = source_path(benchmarks_dir, name, "darklang-interpreter").read_text()
                transformed = adapt_interpreter_source(source, tuple(invocation.args))
                self.assertNotIn("Stdlib.Cli.__Args.int64", transformed)
                self.assertNotRegex(source, r"Stdlib\.[A-Za-z0-9_.]*__")

    def test_float_conversion_retains_failure_on_out_of_range_values(self) -> None:
        transformed = adapt_interpreter_source(
            "Stdlib.Int64.fromFloat (1.5)\nStdlib.Cli.Args.int64 0", ("4",))
        self.assertIn("interpreterInt64FromFloat (1.5)", transformed)
        self.assertIn("| Some number -> number", transformed)
        self.assertIn('| None -> Builtin.testRuntimeError "float is outside Int64 range"', transformed)

    def test_interpreter_adapter_translates_compatibility_only_syntax(self) -> None:
        source = (
            "type Tree = Leaf | Node of Int64\n"
            "let f (values: Dict<Int64>) (pair: (Int64 * Int64)) = pair.0\n"
            "Stdlib.Dict.get<Int64> values \"key\"\n"
            "Stdlib.String.equals left right\n"
            "| Ok closed ->\n"
            "            let nextState = moveCompiler (closed.0) next "
            "(closed.0.reversed) (closed.1) state.trimNext in\n"
            "            let withGoto = emit nextState (Goto (closed.2)) in\n"
            "| Ok closed -> use closed.0 closed.1\n"
            "Stdlib.Cli.__Args.int64 0\n"
        )

        transformed = adapt_interpreter_source(source, ("4",))

        self.assertIn("type Tree = Leaf | Node of Int64", transformed)
        self.assertIn("Dict<String, Int64>", transformed)
        self.assertIn("Stdlib.Dict.get<String, Int64>", transformed)
        self.assertIn("Stdlib.Tuple2.first pair", transformed)
        self.assertIn("left == right", transformed)
        self.assertIn("Stdlib.Tuple3.second closedFor", transformed)
        self.assertIn("Stdlib.Tuple2.second closed", transformed)


if __name__ == "__main__":
    unittest.main()
