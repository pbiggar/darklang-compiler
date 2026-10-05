#!/usr/bin/env python3
"""Compare reference/native runner outcomes and report contracts, excluding timings."""

import argparse
from collections import Counter
import json
import math
from pathlib import Path


COUNTS = {
    "passed", "failed", "total", "e2e_batch_size", "e2e_logical_tests",
    "e2e_batch_eligible_tests", "e2e_physical_executions", "e2e_batch_executions",
    "e2e_batched_logical_tests", "e2e_largest_batch",
}
TIMINGS = {
    "total_ms", "unaccounted_ms", "runtime_unaccounted_ms", "overhead_unaccounted_ms",
}
PROFILE_FIELDS = {
    "phases": {"name", "elapsed_ms", "percentage_of_codegen"},
    "categories": {
        "name", "elapsed_ms", "percentage_of_codegen", "functions", "generations",
    },
    "functions": {
        "name", "category", "generations", "elapsed_ms", "lir_instructions",
        "symbolic_instructions",
    },
    "lir_ops": {
        "name", "occurrences", "elapsed_ms", "symbolic_instructions_before_peephole",
        "average_symbolic_instructions_before_peephole",
    },
    "lir_op_functions": {
        "function_name", "category", "opcode", "detail", "occurrences", "elapsed_ms",
        "symbolic_instructions_before_peephole",
    },
}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def finite(value):
    return type(value) in (int, float) and math.isfinite(value)


def load(path):
    with path.open(encoding="utf-8") as stream:
        return json.load(stream)


def check_timings(report):
    require(set(report) == {"summary", "tests", "passes"}, "timing report fields differ")
    summary = report["summary"]
    require(set(summary) == COUNTS | TIMINGS, "timing summary fields differ")
    for key in COUNTS:
        require(type(summary[key]) is int and summary[key] >= 0,
                f"invalid summary counter: {key}")
    for key in TIMINGS:
        require(finite(summary[key]), f"invalid timing: {key}")
    require(summary["passed"] + summary["failed"] == summary["total"],
            "outcome counters are inconsistent")
    require(summary["failed"] == 0, "suite contains failed tests")
    require(summary["total"] > 0, "suite did not select any tests")
    require(isinstance(report["tests"], list), "tests must be an array")
    contracts = Counter()
    for test in report["tests"]:
        keys = set(test)
        require({"name", "total_ms"} <= keys <= {
            "name", "total_ms", "compile_ms", "runtime_ms",
        }, "timed test fields differ")
        require(isinstance(test["name"], str), "test name must be a string")
        for key in keys - {"name"}:
            require(finite(test[key]), f"invalid test timing: {test['name']}: {key}")
        contracts[(test["name"], tuple(sorted(keys)))] += 1
    require(isinstance(report["passes"], list), "passes must be an array")
    for entry in report["passes"]:
        require(set(entry) == {"name", "elapsed_ms", "invocations"},
                "pass timing fields differ")
        require(isinstance(entry["name"], str) and finite(entry["elapsed_ms"]),
                "invalid pass name or timing")
        require(type(entry["invocations"]) is int and entry["invocations"] >= 0,
                f"invalid pass invocation count: {entry['name']}")
    return contracts


def check_profile(report):
    require(set(report) == {"schema_version", "summary"} | set(PROFILE_FIELDS),
            "profile report fields differ")
    require(report["schema_version"] == 10, "profile must use schema version 10")
    require(isinstance(report["summary"], dict), "profile summary must be an object")
    for key, value in report["summary"].items():
        require(finite(value), f"invalid profile summary number: {key}")
    for group, fields in PROFILE_FIELDS.items():
        require(isinstance(report[group], list), f"profile {group} must be an array")
        for entry in report[group]:
            require(set(entry) == fields, f"profile {group} fields differ")
            for key, value in entry.items():
                valid = isinstance(value, str) if key in {
                    "name", "category", "function_name", "opcode", "detail",
                } else finite(value)
                require(valid, f"invalid profile field: {group}: {key}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reference-timings", required=True, type=Path)
    parser.add_argument("--native-timings", required=True, type=Path)
    parser.add_argument("--reference-profile", type=Path)
    parser.add_argument("--native-profile", type=Path)
    args = parser.parse_args()
    if bool(args.reference_profile) != bool(args.native_profile):
        parser.error("provide both profile reports or neither")
    try:
        reference, native = load(args.reference_timings), load(args.native_timings)
        reference_tests, native_tests = check_timings(reference), check_timings(native)
        for key in sorted(COUNTS):
            require(reference["summary"][key] == native["summary"][key],
                    f"{key}: reference={reference['summary'][key]}, "
                    f"native={native['summary'][key]}")
        missing, extra = reference_tests - native_tests, native_tests - reference_tests
        require(not missing and not extra,
                f"timed test contracts differ; missing={list(missing.items())[:5]}, "
                f"extra={list(extra.items())[:5]}")
        if args.reference_profile:
            reference_profile, native_profile = (
                load(args.reference_profile), load(args.native_profile),
            )
            check_profile(reference_profile)
            check_profile(native_profile)
            require(set(reference_profile["summary"]) == set(native_profile["summary"]),
                    "profile summary fields differ")
    except (OSError, ValueError, KeyError, TypeError) as error:
        parser.exit(1, f"Runner report mismatch: {error}\n")
    print(f"Matched {native['summary']['passed']} passing outcomes, all batching counters "
          f"and {sum(native_tests.values())} timed test contracts.")
    if args.native_profile:
        print("Reference and native profile reports conform to schema 10.")


if __name__ == "__main__":
    main()
