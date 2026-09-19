"""Compare closed-list workloads using built compilers and pinned QEMU counts."""

import argparse
import hashlib
import json
from pathlib import Path
import platform
import re
import statistics
import subprocess
import tempfile
import time


CASES = {
    "main": "8205000\n", "shared": "360000\n", "captured-list": "9\n",
    "edge-cases": "0\n", "effect-order": "1\n2\n3\n4\n3\n2\n9\n",
    "large-unique": "-32000\n", "large-shared": "31968000\n",
    "runtime-unique": "769500\n", "runtime-shared": "6412500\n",
    "runtime-small": "45000\n", "cross-function": "8205000\n",
    "branch-unique": "960000\n", "branch-shared": "1414500\n",
    "runtime-effects": "count\nvalue\nmap\nmap\nmap\nfold\nfold\nfold\nvalue\nvalue\n39\n",
}


def checked(command, cwd):
    result = subprocess.run(command, cwd=cwd, text=True, capture_output=True, timeout=180)
    if result.returncode:
        raise RuntimeError(f"{command} exited {result.returncode}: {result.stdout}{result.stderr}")
    return result


def measure(repository, source, expected, target, output, native_runs):
    compiler = repository / "dark"
    # The CLI supports an explicit x86_64 target; ARM64 uses the host target.
    if target == "arm64" and (platform.system() != "Linux" or platform.machine() not in ("aarch64", "arm64")):
        raise RuntimeError("ARM64 measurements require a Linux ARM64 host")
    target_args = ["--target=linux-x86_64"] if target == "x86_64" else []
    command = [str(compiler), "--emit-result", *target_args, str(source), "-q", "-o", str(output)]
    start = time.monotonic()
    checked(command, repository)
    compile_ms = (time.monotonic() - start) * 1000
    counter = repository / "benchmarks/infrastructure/qemu_instruction_count.sh"
    execution = checked([str(counter), target, str(output)], repository)
    if execution.stdout != expected:
        raise RuntimeError(f"{source.name}: expected {expected!r}, got {execution.stdout!r}")
    counts = re.findall(r"total insns: (\d+)", execution.stderr)
    if len(counts) != 1 or int(counts[0]) <= 0:
        raise RuntimeError(f"Expected one positive instruction count: {execution.stderr}")
    binary_bytes = output.stat().st_size
    native_ms = []
    host_arch = {"aarch64": "arm64", "arm64": "arm64", "x86_64": "x86_64"}.get(platform.machine())
    if platform.system() == "Linux" and host_arch == target:
        for _ in range(native_runs):
            start = time.monotonic()
            native = checked([str(output)], repository)
            native_ms.append((time.monotonic() - start) * 1000)
            if native.stdout != expected:
                raise RuntimeError(f"{source.name}: native output mismatch: {native.stdout!r}")
    checked(command + ["--leak-check"], repository)
    leak = subprocess.run([str(counter), target, str(output)], cwd=repository, text=True, capture_output=True, timeout=30)
    leak_passed = leak.returncode == 0 and leak.stdout == expected and "leaks:" not in leak.stderr
    return {
        "instructions": int(counts[0]), "compile_ms": compile_ms, "binary_bytes": binary_bytes,
        "native_wall_ms": native_ms,
        "native_median_ms": statistics.median(native_ms) if native_ms else None,
        "leak_check_passed": leak_passed, "leak_check_exit_code": leak.returncode,
        "leak_check_stdout": leak.stdout, "leak_check_stderr": leak.stderr,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", type=Path, required=True)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--target", choices=["arm64", "x86_64"], required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--cases", choices=list(CASES), nargs="+", default=list(CASES))
    parser.add_argument("--native-runs", type=int, default=5)
    args = parser.parse_args()
    if args.native_runs < 1:
        parser.error("--native-runs must be positive")
    roots = {"baseline": args.baseline.resolve(), "candidate": args.candidate.resolve()}
    sources = Path(__file__).resolve().parent
    report = {"target": args.target, "requested_cases": args.cases, "complete": False, "compilers": {}, "workloads": {}}
    for label, repository in roots.items():
        report["compilers"][label] = {
            "commit": checked(["git", "rev-parse", "HEAD"], repository).stdout.strip(),
            "dirty": bool(checked(["git", "status", "--porcelain"], repository).stdout),
            "assembly_sha256": hashlib.sha256((repository / "bin/DarkCompiler/Debug/net10.0/DarkCompiler.dll").read_bytes()).hexdigest(),
        }
    with tempfile.TemporaryDirectory(prefix="list-array-bench-") as temporary:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")
        for name in args.cases:
            expected = CASES[name]
            source = sources / f"{name}.dark"
            measurements = {
                label: measure(repository, source, expected, args.target, Path(temporary) / f"{label}-{name}", args.native_runs)
                for label, repository in roots.items()
            }
            for label, repository in roots.items():
                assembly = repository / "bin/DarkCompiler/Debug/net10.0/DarkCompiler.dll"
                if hashlib.sha256(assembly.read_bytes()).hexdigest() != report["compilers"][label]["assembly_sha256"]:
                    raise RuntimeError(f"{label} compiler changed during measurement; rebuild before comparing")
            measurements["source_sha256"] = hashlib.sha256(source.read_bytes()).hexdigest()
            measurements["instruction_ratio"] = measurements["candidate"]["instructions"] / measurements["baseline"]["instructions"]
            report["workloads"][name] = measurements
            # Preserve completed evidence if a later compile or execution fails.
            args.output.write_text(json.dumps(report, indent=2) + "\n")
            print(f"{name}: ratio={measurements['instruction_ratio']:.6f}", flush=True)
            for label in roots:
                if not measurements[label]["leak_check_passed"]:
                    print(f"{name}: {label} leak check FAILED (exit {measurements[label]['leak_check_exit_code']})", flush=True)
    report["complete"] = True
    args.output.write_text(json.dumps(report, indent=2) + "\n")
    # Baseline instruction measurements must succeed, but an existing baseline
    # instrumentation failure does not excuse a candidate failure or erase the
    # usable performance comparison. Always retain both statuses in the report.
    return 0 if all(case["candidate"]["leak_check_passed"] for case in report["workloads"].values()) else 1


if __name__ == "__main__":
    raise SystemExit(main())
