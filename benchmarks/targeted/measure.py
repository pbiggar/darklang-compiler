#!/usr/bin/env python3
"""Measure targeted compiler workloads through the native CLI."""
import argparse
from dataclasses import dataclass
import json
from pathlib import Path
import platform
import statistics
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
CASES = Path(__file__).resolve().parent / "cases"


@dataclass(frozen=True)
class Case:
    name: str
    iterations: int
    source: str
    expected: object
    payload: str | None = None
    operations: int = 2


def json_cases():
    def case(name, payload, iterations, declarations, extract, expected):
        source = declarations + f'''let benchmark (remaining: Int64) (checksum: Int64) : Int64 =
    if remaining <= 0L then checksum
    else
        match Stdlib.Json.parse<{extract[0]}> ({json.dumps(payload, ensure_ascii=False)}) with
        | Ok(value) -> {extract[1]}
        | Error(_) -> -1L
benchmark __ITERATIONS__L 0L
'''
        return Case(name, iterations, source, expected, payload)

    flat = {f"field{index:02}": "a" * 240 for index in range(4)}
    leaf = '{"JsonBenchLeaf":["' + "n" * 32 + '"]}'
    while len(leaf.encode()) < 64 * 1024:
        leaf = '{"JsonBenchBranch":[' + leaf + "," + leaf + "]}"
    return [
        case("scalar", "123456789", 10000, "", ("Int64",
             "benchmark (remaining - 1L) (checksum + value)"), lambda n: n * 123456789),
        case("flat_record_1k", json.dumps(flat, separators=(",", ":")), 20,
             "type JsonBenchFlatRecord = { " + ", ".join(f"{key}: String" for key in flat) + " }\n",
             ("JsonBenchFlatRecord", "benchmark (remaining - 1L) (checksum + Stdlib.String.__byteLength value.field00)"),
             lambda n: n * 240),
        case("collection_1k", json.dumps(list(range(256)), separators=(",", ":")), 100, "",
             ("List<Int64>", "\n            match value with\n"
              "            | first :: _ -> benchmark (remaining - 1L) (checksum + first + 1L)\n"
              "            | [] -> -2L"), lambda n: n),
        case("nested_record_sum_64k", '{"root":' + leaf + "}", 2,
             "type JsonBenchNested = JsonBenchLeaf of String | JsonBenchBranch of JsonBenchNested * JsonBenchNested\n"
             "type JsonBenchEnvelope = { root: JsonBenchNested }\n",
             ("JsonBenchEnvelope", "\n            match value.root with\n"
              "            | JsonBenchBranch(_, _) -> benchmark (remaining - 1L) (checksum + 1L)\n"
              "            | JsonBenchLeaf(_) -> benchmark (remaining - 1L) (checksum + 1L)"), lambda n: n),
    ]


def integer_cases():
    def expected(name, n):
        if name == "int128_arithmetic":
            value = (170141183460469231731687303715884100727 + n) % (1 << 128)
            return value - (1 << 128) if value >= (1 << 127) else value
        if name == "uint128_arithmetic":
            return (340282366920938463463374607431768206455 + n) % (1 << 128)
        if name.endswith("comparison_bitwise"):
            return (n // 2) * 3 + n % 2
        if name == "int128_decimal_conversion":
            return n * 40
        if name == "uint128_decimal_conversion":
            return n * 39
        if name == "uuid_parse_format":
            return n * 36
        if name in ("uuid_equality", "uuid_generation"):
            return n
        if name == "uint128_collection_copy":
            return n * 2 - 1 if n else 0
        raise ValueError(f"Unknown benchmark: {name}")

    return [Case(row["name"], row["iterations"],
                 (CASES / (row["name"] + ".dark")).read_text(),
                 lambda n, name=row["name"]: expected(name, n),
                 operations=row["operations_per_iteration"])
            for row in json.loads((CASES / "integer128.json").read_text())]


def capture(command, timeout):
    start = time.perf_counter_ns()
    result = subprocess.run(command, stdin=subprocess.DEVNULL,
                            capture_output=True, text=True, timeout=timeout, check=False)
    elapsed = (time.perf_counter_ns() - start) / 1_000_000
    if result.returncode:
        raise RuntimeError(f"{command[0]} exited {result.returncode}: {result.stdout}{result.stderr}")
    return result, elapsed


def measure(compiler, case, samples, timeout):
    with tempfile.TemporaryDirectory(prefix="dark-targeted-") as temporary:
        directory = Path(temporary)
        source, binary = directory / (case.name + ".dark"), directory / "program"
        source.write_text(case.source.replace("__ITERATIONS__", str(case.iterations)), encoding="utf-8")
        arguments = [str(compiler), "-q", "--emit-result", "--allow-internal", str(source), "-o", str(binary)]
        _, compile_ms = capture(arguments, timeout)
        binary_bytes = binary.stat().st_size

        def execute(expected):
            result, elapsed = capture([str(binary)], timeout)
            if result.stdout.strip() != str(expected):
                raise RuntimeError(f"{case.name}: expected {expected}, got {result.stdout!r}")
            return result, elapsed

        execute(case.expected(case.iterations))
        times = [execute(case.expected(case.iterations))[1] for _ in range(samples)]
        source.write_text(case.source.replace("__ITERATIONS__", "1"), encoding="utf-8")
        capture([arguments[0], "--leak-check", *arguments[1:]], timeout)
        leak, _ = execute(case.expected(1))
        median = statistics.median(times)
        row = {"name": case.name, "iterations": case.iterations,
               "compile_ms": round(compile_ms, 3), "binary_bytes": binary_bytes,
               "runtime_samples_ms": [round(value, 3) for value in times],
               "median_runtime_ms": round(median, 3),
               "leak_check_passed": "leaks:" not in leak.stderr,
               "leak_check_stderr": leak.stderr.strip()}
        if case.payload is not None:
            size = len(case.payload.encode())
            row.update(payload_bytes=size, nanoseconds_per_decode=round(median * 1_000_000 / case.iterations, 3),
                       throughput_mib_per_second=round(size * case.iterations / (median / 1000) / 1048576, 3))
        else:
            row.update(operations_per_iteration=case.operations,
                       nanoseconds_per_iteration=round(median * 1_000_000 / case.iterations, 3),
                       nanoseconds_per_operation=round(median * 1_000_000 / case.iterations / case.operations, 3))
        return row


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("suite", choices=("json", "integer128", "all"))
    parser.add_argument("output", type=Path)
    parser.add_argument("--compiler", type=Path, default=ROOT / "_build/default/bin/dark.exe")
    parser.add_argument("--samples", type=int, default=7)
    parser.add_argument("--timeout", type=float, default=120)
    args = parser.parse_args()
    if args.samples < 1 or args.timeout <= 0:
        parser.error("samples and timeout must be positive")
    targets = {("Linux", "x86_64"): "LinuxX86_64", ("Linux", "aarch64"): "ARM64Backend LinuxARM64",
               ("Darwin", "arm64"): "ARM64Backend MacOSARM64"}
    target = targets.get((platform.system(), platform.machine()))
    if target is None:
        parser.error("Unsupported benchmark host")
    compiler = args.compiler.resolve()
    if not compiler.is_file():
        parser.error(f"Build the compiler first: {compiler}")
    commit = subprocess.run(["git", "-C", str(ROOT), "rev-parse", "HEAD"],
                            capture_output=True, text=True, check=True).stdout.strip()
    suites = ("json", "integer128") if args.suite == "all" else (args.suite,)
    passed = True
    for suite in suites:
        rows = []
        for case in json_cases() if suite == "json" else integer_cases():
            print(f"{suite}: measuring {case.name}", file=sys.stderr, flush=True)
            rows.append(measure(compiler, case, args.samples, args.timeout))
        output = args.output / (suite + ".json") if args.suite == "all" else args.output
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(json.dumps({"schema_version": 2, "compiler_commit": commit,
                                    "target": target, "samples_per_case": args.samples,
                                    "benchmarks": rows}, indent=2) + "\n")
        passed = passed and all(row["leak_check_passed"] for row in rows)
        print(f"{suite}: wrote {output}", file=sys.stderr)
    return 0 if passed else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, subprocess.SubprocessError, RuntimeError, ValueError) as error:
        print(f"Targeted benchmark failed: {error}", file=sys.stderr)
        sys.exit(1)
