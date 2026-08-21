#!/usr/bin/env python3
"""Bounded inventory, inspection, and focused verification for agent 001."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import shutil
import statistics
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parents[3]
ARTIFACT_ROOT = ROOT / ".dcb" / "tool-artifacts"
MAX_SUMMARY_BYTES = 4096
MAX_TARGET_BYTES = 2 * 1024 * 1024
MAX_CAPTURE_BYTES = 16 * 1024 * 1024

PASS_FILES = {
    "Parse": "src/DarkCompiler/passes/1_Parser.fs",
    "Type Checking": "src/DarkCompiler/passes/1.5_TypeChecking.fs",
    "AST -> ANF": "src/DarkCompiler/passes/2_AST_to_ANF.fs",
    "ANF Optimizations": "src/DarkCompiler/passes/2.3_ANF_Optimize.fs",
    "ANF Inlining": "src/DarkCompiler/passes/2.4_ANF_Inlining.fs",
    "ANF Direct-Call Specialization": "src/DarkCompiler/passes/2.4.5_ANF_DirectCallSpecialization.fs",
    "Reference Count Insertion": "src/DarkCompiler/passes/2.5_RefCountInsertion.fs",
    "Print Insertion": "src/DarkCompiler/passes/2.6_PrintInsertion.fs",
    "Tail Call Detection": "src/DarkCompiler/passes/2.7_TailCallDetection.fs",
    "ANF -> MIR": "src/DarkCompiler/passes/3_ANF_to_MIR.fs",
    "SSA Construction": "src/DarkCompiler/passes/3.1_SSA_Construction.fs",
    "MIR Optimizations": "src/DarkCompiler/passes/3.5_MIR_Optimize.fs",
    "MIR -> LIR": "src/DarkCompiler/passes/4_MIR_to_LIR.fs",
    "LIR Peephole": "src/DarkCompiler/passes/4.5_LIR_Peephole.fs",
    "Register Allocation": "src/DarkCompiler/passes/5_RegisterAllocation.fs",
    "Function Tree Shaking": "src/DarkCompiler/passes/5.5_FunctionTreeShaking.fs",
    "Code Generation": "src/DarkCompiler/passes/arm64/6_CodeGen.fs",
    "ARM64 Emit": "src/DarkCompiler/passes/arm64/7_Emit.fs",
    "x64 Emit": "src/DarkCompiler/passes/x64/7_Encoding.fs",
}

STAGE_RE = re.compile(r"^  \[(\d+(?:\.\d+)?)/(\d+)\] (.+?)\.\.\.$")
TIMING_RE = re.compile(r"^        (\d+(?:\.\d+)?)ms$")
STRUCTURAL_RE = re.compile(
    r"\b(List|Map|Set|Array|Seq)\.(fold|foldBack|map|collect|filter|contains|tryFind|add|remove)\b|\bwhile\b|\bfor\b"
)


class ToolError(Exception):
    """Expected bounded-tool failure with a stable category and exit code."""

    def __init__(self, category: str, message: str, exit_code: int = 3):
        super().__init__(message)
        self.category = category
        self.exit_code = exit_code
        self.artifact: Path | None = None


def relative(path: Path) -> str:
    return path.resolve().relative_to(ROOT).as_posix()


def bounded_target(raw: str) -> Path:
    candidate = (ROOT / raw).resolve() if not Path(raw).is_absolute() else Path(raw).resolve()
    try:
        candidate.relative_to(ROOT)
    except ValueError as exc:
        raise ToolError("invalid-target", "target must be inside the repository") from exc
    if not candidate.is_file():
        raise ToolError("invalid-target", "target must be an existing file")
    if candidate.stat().st_size > MAX_TARGET_BYTES:
        raise ToolError("target-too-large", f"target exceeds {MAX_TARGET_BYTES} bytes")
    return candidate


def artifact_dir(role: str, identity: list[str]) -> Path:
    digest = hashlib.sha256("\0".join(identity).encode()).hexdigest()[:12]
    path = ARTIFACT_ROOT / f"pass-optimization-{role}-{digest}"
    if path.exists():
        shutil.rmtree(path)
    path.mkdir(parents=True)
    return path


def write_json(path: Path, value: object) -> None:
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def emit(value: dict[str, object], exit_code: int = 0) -> None:
    encoded = (json.dumps(value, separators=(",", ":"), sort_keys=True) + "\n").encode()
    if len(encoded) > MAX_SUMMARY_BYTES:
        encoded = b'{"status":"error","error":"summary-overflow","limitBytes":4096}\n'
        exit_code = 4
    sys.stdout.buffer.write(encoded)
    raise SystemExit(exit_code)


def command_record(argv: list[str], cwd: Path, timeout_seconds: int, output: Path) -> dict[str, object]:
    started = time.monotonic()
    with output.open("wb") as stream:
        child = subprocess.Popen(
            argv,
            cwd=cwd,
            stdout=stream,
            stderr=subprocess.STDOUT,
            start_new_session=True,
        )
        try:
            exit_code: int | str = child.wait(timeout=timeout_seconds)
        except subprocess.TimeoutExpired:
            # Reap the complete command tree before callers may remove an
            # isolated worktree that a compiler descendant still references.
            os.killpg(child.pid, signal.SIGKILL)
            child.wait()
            exit_code = "timeout"
    elapsed_ms = round((time.monotonic() - started) * 1000)
    size = output.stat().st_size
    record = {
        "argv": argv,
        "cwd": str(cwd),
        "timeoutSeconds": timeout_seconds,
        "exit": exit_code,
        "elapsedMs": elapsed_ms,
        "output": str(output),
        "outputBytes": size,
    }
    if size > MAX_CAPTURE_BYTES:
        raise ToolError("capture-too-large", f"child output exceeds {MAX_CAPTURE_BYTES} bytes")
    if exit_code == "timeout":
        raise ToolError("timeout", f"command exceeded {timeout_seconds} seconds")
    if exit_code != 0:
        raise ToolError("command-failed", f"command exited {exit_code}")
    return record


def normalize_label(raw: str) -> str:
    label = raw.replace("→", "->")
    label = re.sub(r" \(user only\)$", "", label)
    label = re.sub(r" \(with stdlib env\)$", "", label)
    label = re.sub(r" \([^()]*(?:const_|copy_|strength_|cfg_|licm|peephole)[^()]*\)$", "", label)
    label = re.sub(r" \((?:ELF|Mach-O)\)$", "", label)
    return label


def parse_timings(path: Path) -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    lines = path.read_text(errors="replace").splitlines()
    timings: list[dict[str, object]] = []
    unmatched: list[dict[str, object]] = []
    for index, line in enumerate(lines):
        stage = STAGE_RE.match(line)
        if stage is None:
            continue
        raw_label = stage.group(3)
        label = normalize_label(raw_label)
        next_line = lines[index + 1] if index + 1 < len(lines) else ""
        timing = TIMING_RE.match(next_line)
        if timing is None:
            unmatched.append({"line": index + 1, "stage": stage.group(1), "label": label})
            continue
        if label not in PASS_FILES:
            raise ToolError("unknown-stage", f"unmapped compiler stage: {label}")
        timings.append(
            {"line": index + 1, "stage": stage.group(1), "label": label, "milliseconds": float(timing.group(1))}
        )
    if not timings:
        raise ToolError("missing-timings", "no adjacent recognized stage/timing pairs")
    return timings, unmatched


def ensure_mapping_is_repo_backed() -> None:
    compiler = (ROOT / "src/DarkCompiler/CompilerLibrary.fs").read_text()
    missing = [label for label, source in PASS_FILES.items() if not (ROOT / source).is_file() or label.split()[0] not in compiler]
    if missing:
        raise ToolError("stale-pass-map", ", ".join(missing))


def inventory(args: argparse.Namespace) -> None:
    benchmark = bounded_target(args.benchmark)
    ensure_mapping_is_repo_backed()
    artifact = artifact_dir("inventory", [relative(benchmark)])
    output = artifact / "compiler-vv.txt"
    binary = artifact / "program.bin"
    command = [str(ROOT / "dark"), "--allow-internal", "-vv", str(benchmark), "-o", str(binary)]
    try:
        record = command_record(command, ROOT, args.timeout_seconds, output)
        timings, unmatched = parse_timings(output)
    except ToolError as exc:
        exc.artifact = artifact
        raise
    ranked = sorted(timings, key=lambda item: (-float(item["milliseconds"]), str(item["label"])))
    evidence = {
        "benchmark": relative(benchmark),
        "passMap": PASS_FILES,
        "command": record,
        "timings": timings,
        "unmatchedStages": unmatched,
        "stopLimits": {"targetBytes": MAX_TARGET_BYTES, "captureBytes": MAX_CAPTURE_BYTES, "timeoutSeconds": args.timeout_seconds},
    }
    write_json(artifact / "evidence.json", evidence)
    emit(
        {
            "status": "ok",
            "role": "inventory",
            "benchmark": relative(benchmark),
            "recognizedPairs": len(timings),
            "unmatchedStages": len(unmatched),
            "topPasses": [{"pass": item["label"], "ms": item["milliseconds"]} for item in ranked[: args.top]],
            "artifact": relative(artifact),
            "commands": 1,
        }
    )


def text_hits(paths: list[Path], patterns: list[str], max_hits: int) -> tuple[list[dict[str, object]], bool]:
    hits: list[dict[str, object]] = []
    for path in sorted(paths):
        if not path.is_file() or path.stat().st_size > MAX_TARGET_BYTES:
            continue
        for number, line in enumerate(path.read_text(errors="replace").splitlines(), 1):
            if any(pattern.casefold() in line.casefold() for pattern in patterns):
                hits.append({"path": relative(path), "line": number, "text": line.strip()})
                if len(hits) == max_hits:
                    return hits, True
    return hits, False


def inspect_role(args: argparse.Namespace) -> None:
    benchmark = bounded_target(args.benchmark)
    normalized_pass = normalize_label(args.pass_name)
    if normalized_pass not in PASS_FILES:
        raise ToolError("invalid-pass", "--pass must be a recognized exact pass label", 2)
    source = ROOT / PASS_FILES[normalized_pass]
    artifact = artifact_dir("inspect", [normalized_pass, relative(benchmark)])
    history_output = artifact / "git-history.txt"
    history_command = ["git", "log", f"-{args.max_history}", "--format=%H%x09%s", "--", relative(source)]
    try:
        command = command_record(history_command, ROOT, 30, history_output)
    except ToolError as exc:
        exc.artifact = artifact
        raise
    source_lines = source.read_text(errors="replace").splitlines()
    structural = [
        {"line": number, "text": line.strip(), "operation": match.group(0)}
        for number, line in enumerate(source_lines, 1)
        for match in [STRUCTURAL_RE.search(line)]
        if match is not None
    ][: args.max_hits]
    tokens = [token for token in re.split(r"[^A-Za-z0-9]+", normalized_pass) if len(token) > 2]
    supporting = list((ROOT / "docs").rglob("*.md")) + list((ROOT / "src/Tests").rglob("*"))
    support_hits, truncated = text_hits(supporting, tokens + [source.stem], args.max_hits)
    hypotheses = []
    categories = [
        ("repeated persistent-collection operation", ["Map.", "Set.", "List."]),
        ("repeated sequence traversal", ["fold", "map", "collect", "filter"]),
        ("repeated membership or lookup", ["contains", "tryFind", "find"]),
    ]
    for name, needles in categories:
        count = sum(1 for line in source_lines if any(needle in line for needle in needles))
        if count:
            hypotheses.append({"hypothesis": name, "matchingLines": count})
    evidence = {
        "pass": normalized_pass,
        "benchmark": relative(benchmark),
        "source": relative(source),
        "timingCaller": "src/DarkCompiler/CompilerLibrary.fs",
        "historyCommand": command,
        "history": history_output.read_text(errors="replace").splitlines(),
        "structuralHotspots": structural,
        "supportingHits": support_hits,
        "supportingHitsTruncated": truncated,
        "hypotheses": hypotheses[:3],
        "stopLimits": {"maxHistory": args.max_history, "maxHits": args.max_hits, "targetBytes": MAX_TARGET_BYTES},
    }
    write_json(artifact / "evidence.json", evidence)
    emit(
        {
            "status": "ok",
            "role": "inspect",
            "pass": normalized_pass,
            "benchmark": relative(benchmark),
            "source": relative(source),
            "historyEntries": len(evidence["history"]),
            "hotspots": len(structural),
            "hypotheses": hypotheses[:3],
            "artifact": relative(artifact),
            "commands": 1,
        }
    )


def resolve_ref(ref: str, artifact: Path, name: str, records: list[dict[str, object]]) -> str:
    if len(ref) > 200 or ref.startswith("-"):
        raise ToolError("invalid-ref", "revision must be a bounded git revision", 2)
    output = artifact / f"resolve-{name}.txt"
    records.append(command_record(["git", "rev-parse", "--verify", f"{ref}^{{commit}}"], ROOT, 30, output))
    commit = output.read_text().strip()
    if re.fullmatch(r"[0-9a-f]{40}", commit) is None:
        raise ToolError("invalid-ref", f"could not resolve {name}")
    return commit


def add_worktree(commit: str, path: Path, artifact: Path, name: str, records: list[dict[str, object]]) -> None:
    output = artifact / f"worktree-add-{name}.txt"
    records.append(command_record(["git", "worktree", "add", "--detach", str(path), commit], ROOT, 120, output))


def remove_worktree(path: Path, artifact: Path, name: str, records: list[dict[str, object]]) -> None:
    if not path.exists():
        return
    output = artifact / f"worktree-remove-{name}.txt"
    records.append(command_record(["git", "worktree", "remove", "--force", str(path)], ROOT, 120, output))


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def normalized_ir_hash(path: Path) -> str:
    kept = []
    for line in path.read_text(errors="replace").splitlines():
        if (
            STAGE_RE.match(line)
            or TIMING_RE.match(line)
            or line.startswith("Compiling:")
            or line.startswith("Output:")
            or line.startswith("Successfully wrote ")
        ):
            continue
        kept.append(line.replace("→", "->").rstrip())
    normalized = "\n".join(kept).encode()
    return hashlib.sha256(normalized).hexdigest()


def median_mad(samples: list[float]) -> tuple[float, float]:
    median = statistics.median(samples)
    mad = statistics.median([abs(value - median) for value in samples])
    return median, mad


def compile_series(
    name: str,
    worktree: Path,
    benchmark_relative: str,
    pass_name: str,
    artifact: Path,
    timeout_seconds: int,
    records: list[dict[str, object]],
) -> tuple[list[float], Path]:
    binary = artifact / f"{name}.bin"
    samples: list[float] = []
    for run in range(4):
        output = artifact / f"{name}-timing-{run}.txt"
        argv = [str(worktree / "dark"), "--allow-internal", "-vv", benchmark_relative, "-o", str(binary)]
        records.append(command_record(argv, worktree, timeout_seconds, output))
        timings, _ = parse_timings(output)
        selected = [float(item["milliseconds"]) for item in timings if item["label"] == pass_name]
        if len(selected) != 1:
            raise ToolError("selected-pass-missing", f"expected one timing for {pass_name}, found {len(selected)}")
        if run > 0:
            samples.append(selected[0])
    return samples, binary


def verify(args: argparse.Namespace) -> None:
    benchmark = bounded_target(args.benchmark)
    normalized_pass = normalize_label(args.pass_name)
    if normalized_pass not in PASS_FILES:
        raise ToolError("invalid-pass", "--pass must be a recognized exact pass label", 2)
    if args.focused_check != "executable":
        raise ToolError("invalid-check", "the supported focused check is exactly 'executable'", 2)
    identity = [normalized_pass, relative(benchmark), args.baseline, args.candidate, args.ir]
    artifact = artifact_dir("verify", identity)
    records: list[dict[str, object]] = []
    evidence: dict[str, object] = {"commands": records}
    baseline_tree = artifact / "work-baseline"
    candidate_tree = artifact / "work-candidate"
    try:
        baseline_commit = resolve_ref(args.baseline, artifact, "baseline", records)
        candidate_commit = resolve_ref(args.candidate, artifact, "candidate", records)
        add_worktree(baseline_commit, baseline_tree, artifact, "baseline", records)
        add_worktree(candidate_commit, candidate_tree, artifact, "candidate", records)
        benchmark_relative = relative(benchmark)
        baseline_samples, baseline_binary = compile_series(
            "baseline", baseline_tree, benchmark_relative, normalized_pass, artifact, args.timeout_seconds, records
        )
        candidate_samples, candidate_binary = compile_series(
            "candidate", candidate_tree, benchmark_relative, normalized_pass, artifact, args.timeout_seconds, records
        )
        ir_flag = {"anf": "--dump-anf", "mir": "--dump-mir", "lir": "--dump-lir"}[args.ir]
        ir_hashes: dict[str, str] = {}
        for name, tree in [("baseline", baseline_tree), ("candidate", candidate_tree)]:
            output = artifact / f"{name}-{args.ir}.txt"
            argv = [str(tree / "dark"), "--allow-internal", ir_flag, benchmark_relative, "-o", str(artifact / f"{name}.bin")]
            records.append(command_record(argv, tree, args.timeout_seconds, output))
            ir_hashes[name] = normalized_ir_hash(output)
        execution: dict[str, dict[str, object]] = {}
        for name, binary in [("baseline", baseline_binary), ("candidate", candidate_binary)]:
            output = artifact / f"{name}-execution.txt"
            records.append(command_record([str(binary)], ROOT, 60, output))
            execution[name] = {"sha256": sha256(output), "bytes": output.stat().st_size}
        baseline_median, baseline_mad = median_mad(baseline_samples)
        candidate_median, candidate_mad = median_mad(candidate_samples)
        delta = baseline_median - candidate_median
        threshold = max(1.0, 3.0 * max(baseline_mad, candidate_mad))
        executable_equal = sha256(baseline_binary) == sha256(candidate_binary)
        ir_equal = ir_hashes["baseline"] == ir_hashes["candidate"]
        execution_equal = execution["baseline"] == execution["candidate"]
        if not executable_equal or not ir_equal or not execution_equal:
            decision, reason = "reject", "correctness-mismatch"
        elif baseline_commit == candidate_commit:
            decision, reason = "reject", "same-revision-control"
        elif delta <= threshold:
            decision, reason = "reject", "neutral-or-noisy"
        else:
            decision, reason = "retain", "clear-focused-win"
        evidence.update(
            {
                "pass": normalized_pass,
                "benchmark": benchmark_relative,
                "baseline": {"commit": baseline_commit, "samplesMs": baseline_samples, "medianMs": baseline_median, "madMs": baseline_mad},
                "candidate": {"commit": candidate_commit, "samplesMs": candidate_samples, "medianMs": candidate_median, "madMs": candidate_mad},
                "deltaMs": delta,
                "thresholdMs": threshold,
                "executableEqual": executable_equal,
                "normalizedIrEqual": ir_equal,
                "executionEqual": execution_equal,
                "irHashes": ir_hashes,
                "execution": execution,
                "decision": decision,
                "reason": reason,
                "stopLimits": {"warmups": 1, "samples": 3, "commandTimeoutSeconds": args.timeout_seconds, "executionTimeoutSeconds": 60},
            }
        )
        remove_worktree(baseline_tree, artifact, "baseline", records)
        remove_worktree(candidate_tree, artifact, "candidate", records)
        evidence["commands"] = records
        write_json(artifact / "evidence.json", evidence)
        emit(
            {
                "status": "ok",
                "role": "verify",
                "decision": decision,
                "reason": reason,
                "pass": normalized_pass,
                "baselineMedianMs": baseline_median,
                "candidateMedianMs": candidate_median,
                "deltaMs": delta,
                "thresholdMs": threshold,
                "executableEqual": executable_equal,
                "normalizedIrEqual": ir_equal,
                "executionEqual": execution_equal,
                "artifact": relative(artifact),
                "commands": len(records),
            }
        )
    except ToolError as exc:
        evidence.update({"status": "invalid", "error": exc.category, "message": str(exc)})
        remove_worktree(baseline_tree, artifact, "baseline", records)
        remove_worktree(candidate_tree, artifact, "candidate", records)
        evidence["commands"] = records
        write_json(artifact / "evidence.json", evidence)
        emit({"status": "error", "role": "verify", "error": exc.category, "message": str(exc), "artifact": relative(artifact)}, exc.exit_code)
    finally:
        if baseline_tree.exists():
            remove_worktree(baseline_tree, artifact, "baseline", records)
        if candidate_tree.exists():
            remove_worktree(candidate_tree, artifact, "candidate", records)


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(
        description="Agent 001 bounded tools. Errors: exit 2 invalid arguments; exit 3 invalid evidence/child failure; exit 4 internal/summary failure."
    )
    roles = result.add_subparsers(dest="role", required=True)
    inventory_parser = roles.add_parser("inventory", help="compile one bounded benchmark and rank adjacent recognized -vv timings")
    inventory_parser.add_argument("--benchmark", required=True)
    inventory_parser.add_argument("--top", type=int, choices=range(1, 11), default=5)
    inventory_parser.add_argument("--timeout-seconds", type=int, choices=range(1, 601), default=600)
    inventory_parser.set_defaults(handler=inventory)
    inspect_parser = roles.add_parser("inspect", help="inspect one exact pass and one bounded benchmark without edits")
    inspect_parser.add_argument("--pass", dest="pass_name", required=True)
    inspect_parser.add_argument("--benchmark", required=True)
    inspect_parser.add_argument("--max-history", type=int, choices=range(1, 21), default=10)
    inspect_parser.add_argument("--max-hits", type=int, choices=range(1, 201), default=80)
    inspect_parser.set_defaults(handler=inspect_role)
    verify_parser = roles.add_parser("verify", help="measure two revisions in isolated worktrees; never runs broad gates")
    verify_parser.add_argument("--pass", dest="pass_name", required=True)
    verify_parser.add_argument("--benchmark", required=True)
    verify_parser.add_argument("--baseline", required=True)
    verify_parser.add_argument("--candidate", required=True)
    verify_parser.add_argument("--ir", choices=["anf", "mir", "lir"], required=True)
    verify_parser.add_argument("--focused-check", required=True)
    verify_parser.add_argument("--timeout-seconds", type=int, choices=range(1, 601), default=600)
    verify_parser.set_defaults(handler=verify)
    return result


def main() -> None:
    try:
        args = parser().parse_args()
        args.handler(args)
    except ToolError as exc:
        artifact = exc.artifact or artifact_dir("error", [getattr(exc, "category", "internal"), str(exc)])
        evidence_path = artifact / "evidence.json"
        prior = json.loads(evidence_path.read_text()) if evidence_path.exists() else {}
        write_json(evidence_path, {**prior, "status": "error", "error": exc.category, "message": str(exc)})
        emit({"status": "error", "error": exc.category, "message": str(exc), "artifact": relative(artifact)}, exc.exit_code)
    except SystemExit:
        raise
    except Exception as exc:
        artifact = artifact_dir("error", [type(exc).__name__, str(exc)])
        write_json(artifact / "evidence.json", {"status": "error", "error": "internal", "type": type(exc).__name__, "message": str(exc)})
        emit({"status": "error", "error": "internal", "message": str(exc), "artifact": relative(artifact)}, 4)


if __name__ == "__main__":
    main()
