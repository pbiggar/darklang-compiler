#!/usr/bin/env python3
"""Measure diagnostic benchmark runtimes without making them performance gates."""

from __future__ import annotations

import argparse
import json
import os
import platform
import re
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from pathlib import Path

from benchmark_baseline import (
    CACHEGRIND_POLICY,
    atomic_write_json,
    contract_digest,
    normalize_architecture,
)
from benchmark_profiles import load_invocation, load_profile
from benchmark_reports import generate_reports
from reference_snapshots import measured_reference, save_reference


LANGUAGES = ("darklang-interpreter", "node", "ocaml", "python")
ARGUMENT_PATTERN = re.compile(
    r"Stdlib\.Cli\.Args\.int64\s+([0-9]+|index)\b"
)
INSTRUCTION_PATTERN = re.compile(r"I refs:\s*([0-9][0-9,]*)")
TUPLE_PROJECTION_PATTERN = re.compile(r"\b([A-Za-z_][A-Za-z0-9_]*)\.([012])\b")
SINGLE_VALUE_DICT_PATTERN = re.compile(r"\bDict<([^,<>]+)>")
SINGLE_VALUE_DICT_FUNCTION_PATTERN = re.compile(
    r"(Stdlib\.Dict\.[A-Za-z0-9_]+)<([^,<>]+)>"
)


def inject_interpreter_arguments(source: str, arguments: tuple[str, ...]) -> str:
    """Replace public CLI argument lookups with equivalent literal Results."""

    def replacement(match: re.Match[str]) -> str:
        if match.group(1) == "index":
            cases = " ".join(
                f"| {index} -> Ok {value}L"
                for index, value in enumerate(arguments)
            )
            return f'(match index with {cases} | _ -> Error "missing benchmark argument")'
        index = int(match.group(1))
        if index >= len(arguments):
            raise ValueError(f"interpreter source requests argument index {index}")
        value = arguments[index]
        if not re.fullmatch(r"-?[0-9]+", value):
            raise ValueError(f"interpreter argument {index} is not an Int64")
        return f"Ok {value}L"

    transformed, replacements = ARGUMENT_PATTERN.subn(replacement, source)
    if replacements == 0:
        raise ValueError("interpreter source has no benchmark argument lookups")
    return transformed


def adapt_interpreter_source(source: str, arguments: tuple[str, ...]) -> str:
    """Translate compiler compatibility syntax to the current interpreter surface."""
    transformed = inject_interpreter_arguments(source, arguments)
    transformed = SINGLE_VALUE_DICT_PATTERN.sub(r"Dict<String, \1>", transformed)
    transformed = SINGLE_VALUE_DICT_FUNCTION_PATTERN.sub(
        r"\1<String, \2>", transformed
    )
    transformed = transformed.replace(
        "| Ok closed ->\n"
        "            let nextState = moveCompiler closed.0 next "
        "closed.0.reversed closed.1 state.trimNext in\n"
        "            let withGoto = emit nextState (Goto closed.2) in",
        "| Ok closedFor ->\n"
        "            let nextState = moveCompiler closedFor.0 next "
        "closedFor.0.reversed closedFor.1 state.trimNext in\n"
        "            let withGoto = emit nextState (Goto closedFor.2) in",
    )
    transformed = transformed.replace(
        "| Ok closed ->\n"
        "            let nextState = moveCompiler (closed.0) next "
        "(closed.0.reversed) (closed.1) state.trimNext in\n"
        "            let withGoto = emit nextState (Goto (closed.2)) in",
        "| Ok closedFor ->\n"
        "            let nextState = moveCompiler (closedFor.0) next "
        "(closedFor.0.reversed) (closedFor.1) state.trimNext in\n"
        "            let withGoto = emit nextState (Goto (closedFor.2)) in",
    )
    triple_names = {
        match.group(1)
        for match in TUPLE_PROJECTION_PATTERN.finditer(transformed)
        if match.group(2) == "2"
    }

    def tuple_projection(match: re.Match[str]) -> str:
        name, index = match.groups()
        module = "Tuple3" if name in triple_names else "Tuple2"
        function = ("first", "second", "third")[int(index)]
        return f"(Stdlib.{module}.{function} {name})"

    transformed = TUPLE_PROJECTION_PATTERN.sub(tuple_projection, transformed)
    transformed = re.sub(r"};(?=\s*\n)", "},", transformed)
    constructors: dict[str, str] = {}
    active_type: str | None = None
    declaration_lines: set[int] = set()
    for line_number, line in enumerate(transformed.splitlines()):
        declaration = re.match(
            r"\s*type\s+([A-Z][A-Za-z0-9_]*)(?:<[^>]+>)?\s*=\s*(.*)", line
        )
        if declaration:
            active_type = declaration.group(1)
            declaration_lines.add(line_number)
            fragments = declaration.group(2).split("|")
        elif active_type is not None and re.match(r"\s*\|", line):
            declaration_lines.add(line_number)
            fragments = [line.split("|", 1)[1]]
        else:
            active_type = None
            fragments = []
        if active_type is not None:
            for fragment in fragments:
                case = re.match(r"\s*([A-Z][A-Za-z0-9_]*)\b", fragment)
                if case and case.group(1) not in {
                    "Int",
                    "Int64",
                    "String",
                    "Bool",
                    "Float",
                    "List",
                    "Dict",
                }:
                    constructors[case.group(1)] = active_type
    adapted_lines = []
    for line_number, line in enumerate(transformed.splitlines()):
        parts = re.split(r"((?<!\|)\|(?![>|])|->)", line)
        in_pattern = False
        adapted = []
        for part in parts:
            if part == "|":
                in_pattern = True
            elif part == "->":
                in_pattern = False
            elif line_number not in declaration_lines and not in_pattern:
                for constructor, type_name in constructors.items():
                    part = re.sub(
                        rf"(?<![.A-Za-z0-9_]){re.escape(constructor)}\b",
                        f"{type_name}.{constructor}",
                        part,
                    )
            adapted.append(part)
        adapted_lines.append("".join(adapted))
    transformed = "\n".join(adapted_lines) + ("\n" if transformed.endswith("\n") else "")
    transformed = transformed.replace(
        "(checksum data 0L 0L).1",
        "(Stdlib.Tuple2.second (checksum data 0L 0L))",
    )
    transformed = transformed.replace("Stdlib.String.equals left right", "left == right")
    if "Stdlib.Int64.fromFloat" in transformed:
        transformed = transformed.replace("Stdlib.Int64.fromFloat", "interpreterInt64FromFloat")
        transformed = (
            "let interpreterInt64FromFloat (value: Float) : Int64 =\n"
            "    match Stdlib.Int64.fromFloat value with\n"
            "    | Some number -> number\n"
            "    | None -> Builtin.testRuntimeError \"float is outside Int64 range\"\n\n"
            + transformed
        )
    needs_grapheme_adapter = "Stdlib.String.toGraphemes" in transformed
    if needs_grapheme_adapter:
        transformed = transformed.replace("Stdlib.String.toGraphemes", "interpreterToGraphemes")
    if needs_grapheme_adapter:
        transformed = (
            "let interpreterToGraphemesLoop (value: String) (index: Int) "
            "(length: Int) (reversed: List<String>) : List<String> =\n"
            "    if index >= length then Stdlib.List.reverse reversed\n"
            "    else interpreterToGraphemesLoop value (index + 1) length "
            "(Stdlib.List.push reversed (Stdlib.String.slice value index (index + 1)))\n\n"
            "let interpreterToGraphemes (value: String) : List<String> =\n"
            "    interpreterToGraphemesLoop value 0 (Stdlib.String.length value) ([])\n\n"
            + transformed
        )
    return transformed


def parse_instruction_count(stderr: str) -> int:
    matches = INSTRUCTION_PATTERN.findall(stderr)
    if len(matches) != 1:
        raise ValueError("Cachegrind did not report exactly one instruction count")
    count = int(matches[0].replace(",", ""))
    if count <= 0:
        raise ValueError("Cachegrind instruction count must be positive")
    return count


def command_version(command: list[str], environment: dict[str, str] | None = None) -> str:
    result = subprocess.run(
        command,
        check=True,
        capture_output=True,
        text=True,
        timeout=60,
        env=environment,
    )
    return (result.stdout or result.stderr).strip().splitlines()[0]


def implementation_version(
    language: str, interpreter: Path | None, interpreter_rundir: Path | None
) -> str:
    if language == "darklang-interpreter":
        if interpreter is None or interpreter_rundir is None:
            raise ValueError("Darklang interpreter binary and rundir are required")
        environment = os.environ.copy()
        environment["DARK_CONFIG_RUNDIR"] = str(interpreter_rundir.resolve()) + "/"
        return command_version([str(interpreter), "version"], environment)
    if language == "node":
        return command_version(["node", "--version"])
    if language == "ocaml":
        return command_version(["ocamlopt", "-version"])
    return command_version(["python3", "--version"])


def source_path(benchmarks_dir: Path, name: str, language: str) -> Path:
    directory = "dark" if language == "darklang-interpreter" else language
    extension = {
        "darklang-interpreter": "dark",
        "node": "js",
        "ocaml": "ml",
        "python": "py",
    }[language]
    return benchmarks_dir / "problems" / name / directory / f"main.{extension}"


def measure_one(
    benchmarks_dir: Path,
    build_dir: Path,
    name: str,
    language: str,
    profile: str,
    interpreter: Path | None,
    interpreter_rundir: Path | None,
    timeout: int,
) -> dict[str, object] | None:
    source = source_path(benchmarks_dir, name, language)
    if not source.is_file():
        return None
    invocation = load_invocation(benchmarks_dir, profile, name)
    arguments = tuple(invocation.args)
    environment = os.environ.copy()
    if language == "darklang-interpreter":
        if interpreter is None or interpreter_rundir is None:
            raise ValueError("Darklang interpreter binary and rundir are required")
        transformed = adapt_interpreter_source(source.read_text(), arguments)
        prepared = build_dir / f"{name}.dark"
        prepared.write_text(transformed)
        command = [str(interpreter), "run", str(prepared)]
        isolated_rundir = build_dir / f"{name}-rundir"
        shutil.copytree(interpreter_rundir, isolated_rundir)
        environment["DARK_CONFIG_RUNDIR"] = str(isolated_rundir.resolve()) + "/"
    elif language == "node":
        command = ["node", "--stack-size=400000", str(source), *arguments]
    elif language == "python":
        command = ["python3", str(source), *arguments]
    else:
        prepared = build_dir / f"{name}.ml"
        shutil.copy2(source, prepared)
        binary = build_dir / f"{name}-ocaml"
        subprocess.run(
            ["ocamlopt", "-O3", str(prepared), "-o", str(binary)],
            check=True,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        command = [str(binary), *arguments]
    measured = subprocess.run(
        [
            "valgrind",
            "--tool=cachegrind",
            "--cache-sim=no",
            "--branch-sim=no",
            "--main-stacksize=536870912",
            "--cachegrind-out-file=/dev/null",
            *command,
        ],
        capture_output=True,
        text=True,
        timeout=timeout,
        env=environment,
    )
    if measured.returncode != 0:
        raise ValueError(
            f"{name} {language} exited {measured.returncode}: "
            f"{measured.stderr.strip()[-500:]}"
        )
    output_valid = measured.stdout == invocation.expected_stdout
    if not output_valid:
        raise ValueError(
            f"{name} {language} output mismatch: expected "
            f"{invocation.expected_stdout!r}, got {measured.stdout!r}"
        )
    return {
        "name": name,
        "instructions": parse_instruction_count(measured.stderr),
        "output_valid": output_valid,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--profile", default="full")
    parser.add_argument("--benchmarks")
    parser.add_argument("--languages", default=",".join(LANGUAGES))
    parser.add_argument("--darklang-interpreter", type=Path)
    parser.add_argument("--darklang-rundir", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--jobs", type=int, default=1)
    parser.add_argument("--timeout", type=int, default=3600)
    parser.add_argument("--allow-partial", action="store_true")
    args = parser.parse_args()
    benchmarks_dir = Path(__file__).resolve().parent.parent
    architecture = normalize_architecture(platform.machine())
    selected = tuple(language.strip() for language in args.languages.split(",") if language.strip())
    unknown = set(selected) - set(LANGUAGES)
    if unknown:
        parser.error(f"unknown languages: {', '.join(sorted(unknown))}")
    if args.jobs < 1 or args.timeout < 1:
        parser.error("--jobs and --timeout must be positive")
    if "darklang-interpreter" in selected and (
        args.darklang_interpreter is None or args.darklang_rundir is None
    ):
        parser.error(
            "interpreter measurements require --darklang-interpreter and "
            "--darklang-rundir"
        )
    required = (
        "valgrind",
        *(language for language in selected if language != "darklang-interpreter"),
    )
    for executable in required:
        executable = {
            "python": "python3",
            "ocaml": "ocamlopt",
        }.get(executable, executable)
        if shutil.which(executable) is None:
            parser.error(f"required executable is unavailable: {executable}")
    names = load_profile(benchmarks_dir, args.profile)
    measured_names = names
    if args.benchmarks:
        measured_names = [name.strip() for name in args.benchmarks.split(",") if name.strip()]
        unknown_benchmarks = set(measured_names) - set(names)
        if unknown_benchmarks:
            parser.error(
                f"benchmarks are not in profile {args.profile}: "
                f"{', '.join(sorted(unknown_benchmarks))}"
            )
    if (args.benchmarks or args.allow_partial) and args.output is None:
        parser.error("partial diagnostic experiments require --output; stored references require a complete refresh")
    implementations: dict[str, object] = {}
    try:
        with tempfile.TemporaryDirectory(prefix="diagnostic-references-") as temporary:
            build_dir = Path(temporary)
            for language in selected:
                previous = implementations.get(language, {})
                previous_rows = previous.get("benchmarks", []) if isinstance(previous, dict) else []
                rows = [
                    row
                    for row in previous_rows
                    if isinstance(row, dict) and row.get("name") not in measured_names
                ]
                previous_errors = previous.get("errors", {}) if isinstance(previous, dict) else {}
                errors = {
                    name: message
                    for name, message in previous_errors.items()
                    if name not in measured_names
                }
                # Every interpreter task receives a private copy of the prepared
                # package/trace store, so parallel lock contention cannot perturb
                # otherwise deterministic Cachegrind counts.
                with ThreadPoolExecutor(max_workers=args.jobs) as executor:
                    futures = {
                        executor.submit(
                            measure_one,
                            benchmarks_dir,
                            build_dir,
                            name,
                            language,
                            args.profile,
                            args.darklang_interpreter,
                            args.darklang_rundir,
                            args.timeout,
                        ): name
                        for name in measured_names
                    }
                    for future in as_completed(futures):
                        name = futures[future]
                        try:
                            row = future.result()
                        except (OSError, ValueError, subprocess.SubprocessError) as error:
                            if not args.allow_partial:
                                raise
                            errors[name] = str(error)
                            print(f"{language} {name}: skipped ({error})", flush=True)
                            continue
                        if row is not None:
                            rows.append(row)
                            print(f"{language} {row['name']}: {row['instructions']:,}", flush=True)
                rows.sort(key=lambda row: names.index(str(row["name"])))
                implementations[language] = {
                    "version": implementation_version(
                        language, args.darklang_interpreter, args.darklang_rundir
                    ),
                    "benchmarks": rows,
                }
                if errors:
                    implementations[language]["errors"] = errors
        timestamp = datetime.now(timezone.utc).isoformat()
        if args.output is not None:
            atomic_write_json(args.output, {
                "schema_version": 1, "architecture": architecture, "profile": args.profile,
                "measurement_policy": CACHEGRIND_POLICY,
                "contract_sha256": contract_digest(benchmarks_dir, args.profile),
                "generated_at": timestamp, "implementations": implementations,
            })
            print(f"Diagnostic experiment written to: {args.output}")
        else:
            for language, implementation in implementations.items():
                document = measured_reference(
                    benchmarks_dir, language, args.profile, architecture,
                    implementation["version"], timestamp, implementation["benchmarks"],
                    ["-O3"] if language == "ocaml" else [],
                    ["--stack-size=400000"] if language == "node" else [],
                    {"valgrind": command_version(["valgrind", "--version"])},
                )
                save_reference(benchmarks_dir, document)
            generate_reports(benchmarks_dir)
            print("Independent diagnostic references and reports updated")
        return 0
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        print(f"Diagnostic reference measurement failed: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
