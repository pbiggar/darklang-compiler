#!/usr/bin/env python3
"""Pull the package catalog from an existing Dark server and compile probes."""

import argparse
import json
import subprocess
import sys
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True, order=True)
class PackageItem:
    kind: str
    name: str
    type_parameters: int = 0


def request_json(server: str, path: str, timeout: float) -> object:
    url = urllib.parse.urljoin(server.rstrip("/") + "/", path.lstrip("/"))
    request = urllib.request.Request(url, headers={"Accept": "application/json"})
    with urllib.request.urlopen(request, timeout=timeout) as response:
        return json.load(response)


def qualified_name(location: object) -> str:
    if not isinstance(location, dict):
        raise ValueError("catalog item has no package location")
    owner = location.get("owner")
    modules = location.get("modules")
    name = location.get("name")
    if not isinstance(owner, str) or not isinstance(name, str) or not isinstance(modules, list):
        raise ValueError("catalog item has an invalid package location")
    if not all(isinstance(module, str) for module in modules):
        raise ValueError("catalog item has invalid package modules")
    return ".".join([owner, *modules, name])


def package_items(catalog: object, kinds: set[str]) -> list[PackageItem]:
    if not isinstance(catalog, dict):
        raise ValueError("package search returned a non-object")
    result: set[PackageItem] = set()
    keys = {"function": "fns", "type": "types", "value": "values"}
    for kind in sorted(kinds):
        entries = catalog.get(keys[kind])
        if not isinstance(entries, list):
            raise ValueError(f"package search omitted the {keys[kind]} list")
        for entry in entries:
            if not isinstance(entry, dict):
                raise ValueError(f"package search returned an invalid {kind}")
            parameter_count = 0
            if kind == "type":
                entity = entry.get("entity")
                declaration = entity.get("declaration") if isinstance(entity, dict) else None
                parameters = declaration.get("typeParams") if isinstance(declaration, dict) else None
                if not isinstance(parameters, list):
                    raise ValueError("package type has no typeParams list")
                parameter_count = len(parameters)
            result.add(PackageItem(kind, qualified_name(entry.get("location")), parameter_count))
    return sorted(result)


def probe_source(item: PackageItem) -> str:
    if item.kind == "type":
        variables = [f"'packageType{index}" for index in range(item.type_parameters)]
        type_name = item.name
        if variables:
            type_name += "<" + ", ".join(variables) + ">"
        type_parameters = "<" + ", ".join(variables) + ">" if variables else ""
        return f"let packageProbe{type_parameters} (value: {type_name}) : {type_name} = value\n\n0L\n"
    return f"let packageProbe = {item.name} in 0L\n"


def write_manifest(items: list[PackageItem], directory: Path) -> Path:
    manifest: list[dict[str, str]] = []
    output = directory / "probe.out"
    for index, item in enumerate(items):
        source = directory / f"probe-{index:05d}.dark"
        source.write_text(probe_source(item), encoding="utf-8")
        manifest.append(
            {
                "kind": item.kind,
                "name": item.name,
                "source": str(source),
                "output": str(output),
            }
        )
    path = directory / "manifest.json"
    path.write_text(json.dumps(manifest), encoding="utf-8")
    return path


def read_report(path: Path) -> list[dict[str, object]]:
    reports: list[dict[str, object]] = []
    with path.open(encoding="utf-8") as report_file:
        for line_number, line in enumerate(report_file, 1):
            value = json.loads(line)
            if not isinstance(value, dict):
                raise ValueError(f"report line {line_number} is not an object")
            reports.append(value)
    return reports


def parse_args() -> argparse.Namespace:
    repository = Path(__file__).resolve().parent.parent
    parser = argparse.ArgumentParser(
        description="Fetch Dark's package catalog and attempt to compile each selected package item."
    )
    parser.add_argument("--server", required=True, help="Package server base URL")
    parser.add_argument("--compiler", type=Path, default=repository / "dark")
    parser.add_argument(
        "--kind",
        action="append",
        choices=("function", "type", "value"),
        help="Package kind to compile; repeatable, defaults to all kinds",
    )
    parser.add_argument("--match", default="", help="Only names containing this text")
    parser.add_argument("--limit", type=int, help="Compile at most this many matching items")
    parser.add_argument("--catalog-timeout", type=float, default=120.0, help="Seconds allowed for catalog download")
    parser.add_argument("--timeout", type=float, help="Optional timeout for the complete compiler batch")
    parser.add_argument(
        "--report",
        type=Path,
        default=repository / "TestResults" / "package-compilation.jsonl",
        help="Complete machine-readable result log",
    )
    parser.add_argument(
        "--failure-log",
        type=Path,
        default=repository / "TestResults" / "package-compilation.log",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.limit is not None and args.limit < 1:
        print("--limit must be positive", file=sys.stderr)
        return 2
    if args.timeout is not None and args.timeout <= 0:
        print("--timeout must be positive", file=sys.stderr)
        return 2
    if args.catalog_timeout <= 0:
        print("--catalog-timeout must be positive", file=sys.stderr)
        return 2
    compiler = args.compiler.resolve()
    if not compiler.is_file():
        print(f"compiler not found: {compiler}; run ./build --ai first", file=sys.stderr)
        return 2

    kinds = set(args.kind or ("function", "type", "value"))
    query = urllib.parse.urlencode(
        {
            "modules": "",
            "searchDepth": "all",
            "entityTypes": ",".join(sorted(kinds)),
        }
    )
    try:
        catalog = request_json(args.server, f"/search?{query}", args.catalog_timeout)
        items = package_items(catalog, kinds)
    except (OSError, ValueError, json.JSONDecodeError, urllib.error.URLError) as error:
        print(f"could not read package catalog from {args.server}: {error}", file=sys.stderr)
        return 2

    match_text = args.match.casefold()
    if match_text:
        items = [item for item in items if match_text in item.name.casefold()]
    if args.limit is not None:
        items = items[: args.limit]
    if not items:
        print("no package items matched", file=sys.stderr)
        return 2

    print(f"Pulled catalog from {args.server}; compiling {len(items)} package item(s)", flush=True)
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.unlink(missing_ok=True)
    with tempfile.TemporaryDirectory(prefix="dark-package-compilation-") as temp:
        directory = Path(temp)
        manifest = write_manifest(items, directory)
        command = [
            str(compiler),
            "--batch",
            "--quiet",
            "--package-server",
            args.server,
            "--manifest",
            str(manifest),
            "--keep-going",
            "--report",
            str(args.report),
        ]
        try:
            result = subprocess.run(
                command,
                capture_output=True,
                text=True,
                timeout=args.timeout,
                check=False,
            )
        except subprocess.TimeoutExpired:
            duration = f" after {args.timeout:g}s" if args.timeout is not None else ""
            print(f"compiler batch timed out{duration}", file=sys.stderr)
            return 2

    try:
        reports = read_report(args.report)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        diagnostic = (result.stderr + result.stdout).strip()
        print(f"could not read compiler report {args.report}: {error}", file=sys.stderr)
        if diagnostic:
            print(diagnostic, file=sys.stderr)
        return 2
    if len(reports) != len(items):
        print(
            f"compiler reported {len(reports)}/{len(items)} package items; see {args.report}",
            file=sys.stderr,
        )
        return 2

    failures: list[tuple[PackageItem, str]] = []
    for item, report in zip(items, reports, strict=True):
        if report.get("kind") != item.kind or report.get("name") != item.name:
            print(f"compiler report does not match manifest at {item.kind} {item.name}", file=sys.stderr)
            return 2
        if report.get("status") == "failed":
            diagnostic = report.get("error")
            failures.append((item, diagnostic if isinstance(diagnostic, str) else "unknown failure"))
        elif report.get("status") != "compiled":
            print(f"compiler report has invalid status for {item.kind} {item.name}", file=sys.stderr)
            return 2

    args.failure_log.parent.mkdir(parents=True, exist_ok=True)
    with args.failure_log.open("w", encoding="utf-8") as log:
        for item, diagnostic in failures:
            log.write(f"=== {item.kind} {item.name} ===\n{diagnostic}\n\n")

    passed = len(items) - len(failures)
    print(f"Compiled: {passed}/{len(items)} succeeded", flush=True)
    print(f"Complete report: {args.report}", flush=True)
    if result.returncode != 0 and not failures:
        diagnostic = (result.stderr + result.stdout).strip()
        print(diagnostic or f"compiler exited {result.returncode}", file=sys.stderr)
        return 2
    if failures:
        for item, diagnostic in failures[:10]:
            first_line = diagnostic.splitlines()[0] if diagnostic else "unknown failure"
            print(f"FAILED {item.kind} {item.name}: {first_line}", file=sys.stderr)
        if len(failures) > 10:
            print(f"... {len(failures) - 10} more failure(s)", file=sys.stderr)
        print(f"Full failure log: {args.failure_log}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
