#!/usr/bin/env python3
"""
Diff local upstream execution tests against darklang/dark upstream.

This script treats `Builtin.testDerrorMessage` and `error="..."` as equivalent.
Any other content changes are reported as diffs.
It can also store/load an expected diff baseline.
"""

from __future__ import annotations

import argparse
import difflib
import re
import subprocess
import sys
import tempfile
from pathlib import Path


PAT_PAREN_INLINE = re.compile(
    r'\(\s*Builtin\.testDerrorMessage\s+"((?:[^"\\]|\\.)*)"\s*\)'
)
PAT_INLINE = re.compile(r'Builtin\.testDerrorMessage\s+"((?:[^"\\]|\\.)*)"')
PAT_NEXT_LINE_MESSAGE = re.compile(
    r'^\s*"(?P<message>(?:[^"\\]|\\.)*)"\s*(?P<close>\)?)\s*$'
)
PAT_LOCAL_COMPILE_ERROR = re.compile(r'^\s*#compileerror(?:="(?:[^"\\]|\\.)*")?\s*$')

ANSI_RESET = "\033[0m"
ANSI_BOLD = "\033[1m"
ANSI_DIM = "\033[2m"
ANSI_RED = "\033[31m"
ANSI_GREEN = "\033[32m"
ANSI_YELLOW = "\033[33m"
ANSI_CYAN = "\033[36m"


def run(cmd: list[str], cwd: Path | None = None) -> str:
    result = subprocess.run(
        cmd,
        cwd=str(cwd) if cwd else None,
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        command = " ".join(cmd)
        raise RuntimeError(
            f"Command failed ({result.returncode}): {command}\n"
            f"stdout:\n{result.stdout}\n"
            f"stderr:\n{result.stderr}"
        )
    return result.stdout


def normalize_expected_derror_differences(text: str) -> str:
    lines = text.splitlines(keepends=False)
    output: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]

        # Do not rewrite comments.
        if line.lstrip().startswith("//"):
            output.append(line)
            i += 1
            continue

        rewritten = PAT_PAREN_INLINE.sub(lambda m: f'error="{m.group(1)}"', line)
        rewritten = PAT_INLINE.sub(lambda m: f'error="{m.group(1)}"', rewritten)

        # Handle multiline style:
        #   foo = Builtin.testDerrorMessage
        #     "msg"
        if "Builtin.testDerrorMessage" in rewritten and i + 1 < len(lines):
            next_line = lines[i + 1]
            if not next_line.lstrip().startswith("//"):
                match = PAT_NEXT_LINE_MESSAGE.match(next_line)
                if match:
                    rewritten = rewritten.replace(
                        "Builtin.testDerrorMessage",
                        f'error="{match.group("message")}"',
                        1,
                    )
                    if match.group("close"):
                        rewritten += match.group("close")
                    output.append(rewritten)
                    i += 2
                    continue

        output.append(rewritten)
        i += 1

    return "\n".join(output)


def strip_local_expectation_directives(text: str) -> str:
    """Remove compiler-only metadata before comparing with the upstream corpus."""
    return "".join(
        line
        for line in text.splitlines(keepends=True)
        if not PAT_LOCAL_COMPILE_ERROR.match(line.rstrip("\r\n"))
    )


def collect_files(root: Path) -> dict[str, Path]:
    return {
        file.relative_to(root).as_posix(): file
        for file in sorted(root.rglob("*"))
        if file.is_file()
    }


def is_ignored(rel_path: str, ignore_prefixes: list[str]) -> bool:
    return any(rel_path.startswith(prefix) for prefix in ignore_prefixes)


def fetch_upstream_checkout(
    repo_url: str, ref: str, upstream_subdir: str
) -> tuple[tempfile.TemporaryDirectory[str], Path]:
    tempdir = tempfile.TemporaryDirectory()
    repo_dir = Path(tempdir.name) / "dark-upstream"

    run(
        [
            "git",
            "clone",
            "--depth",
            "1",
            "--filter=blob:none",
            "--sparse",
            repo_url,
            str(repo_dir),
        ]
    )

    run(["git", "sparse-checkout", "set", upstream_subdir], cwd=repo_dir)
    run(["git", "fetch", "--depth", "1", "origin", ref], cwd=repo_dir)
    run(["git", "checkout", ref], cwd=repo_dir)

    return tempdir, repo_dir / upstream_subdir


def should_use_color(mode: str, writing_to_file: bool) -> bool:
    if mode == "always":
        return True
    if mode == "never":
        return False
    # auto
    return (not writing_to_file) and sys.stdout.isatty()


def colorize_line(line: str) -> str:
    if line.startswith("upstream:") or line.startswith("local:"):
        return f"{ANSI_DIM}{line}{ANSI_RESET}"
    if line.startswith("expected_diff_file:") or line.startswith("expected_patch_file:"):
        return f"{ANSI_DIM}{line}{ANSI_RESET}"
    if line.startswith("note:"):
        return f"{ANSI_DIM}{line}{ANSI_RESET}"
    if line.startswith("summary"):
        return f"{ANSI_BOLD}{line}{ANSI_RESET}"
    if line.startswith("files only in upstream:") or line.startswith("files only in local:"):
        return f"{ANSI_BOLD}{line}{ANSI_RESET}"
    if line.startswith("unexpected files only in upstream:"):
        return f"{ANSI_BOLD}{line}{ANSI_RESET}"
    if line.startswith("unexpected files only in local:"):
        return f"{ANSI_BOLD}{line}{ANSI_RESET}"
    if line.startswith("expected-only files no longer in upstream-only set:"):
        return f"{ANSI_BOLD}{line}{ANSI_RESET}"
    if line.startswith("expected-only files no longer in local-only set:"):
        return f"{ANSI_BOLD}{line}{ANSI_RESET}"
    if line.startswith("expected meaningful diffs no longer present:"):
        return f"{ANSI_BOLD}{line}{ANSI_RESET}"
    if line.startswith("diff (expected_patch"):
        return f"{ANSI_BOLD}{ANSI_YELLOW}{line}{ANSI_RESET}"
    if line.startswith("diff: "):
        return f"{ANSI_BOLD}{ANSI_YELLOW}{line}{ANSI_RESET}"
    if line.startswith("diff ("):
        return f"{ANSI_BOLD}{ANSI_YELLOW}{line}{ANSI_RESET}"
    if line.startswith("--- "):
        return f"{ANSI_RED}{line}{ANSI_RESET}"
    if line.startswith("+++ "):
        return f"{ANSI_GREEN}{line}{ANSI_RESET}"
    if line.startswith("@@ "):
        return f"{ANSI_CYAN}{line}{ANSI_RESET}"
    if line.startswith("+") and not line.startswith("+++ "):
        return f"{ANSI_GREEN}{line}{ANSI_RESET}"
    if line.startswith("-") and not line.startswith("--- "):
        return f"{ANSI_RED}{line}{ANSI_RESET}"
    return line


def build_current_state(
    upstream_files: dict[str, Path], local_files: dict[str, Path]
) -> tuple[list[str], list[str], list[tuple[str, str, str]]]:
    local_only = sorted(set(local_files) - set(upstream_files))
    upstream_only = sorted(set(upstream_files) - set(local_files))
    shared = sorted(set(local_files) & set(upstream_files))

    meaningful_diffs: list[tuple[str, str, str]] = []
    for rel_path in shared:
        upstream_text = upstream_files[rel_path].read_text(
            encoding="utf-8", errors="replace"
        )
        local_text = local_files[rel_path].read_text(
            encoding="utf-8", errors="replace"
        )

        upstream_normalized = normalize_expected_derror_differences(upstream_text)
        local_normalized = normalize_expected_derror_differences(
            strip_local_expectation_directives(local_text)
        )
        if upstream_normalized != local_normalized:
            meaningful_diffs.append((rel_path, upstream_normalized, local_normalized))

    return upstream_only, local_only, meaningful_diffs


def normalize_patch_text(text: str) -> str:
    normalized = text.replace("\r\n", "\n")
    if normalized == "":
        return ""
    return normalized.rstrip("\n") + "\n"


def build_patch_text(
    upstream_files: dict[str, Path], local_files: dict[str, Path]
) -> str:
    rel_paths = sorted(set(upstream_files) | set(local_files))
    sections: list[str] = []
    with tempfile.TemporaryDirectory() as sanitized_root:
        sanitized_root_path = Path(sanitized_root)

        for rel_path in rel_paths:
            if rel_path in upstream_files and rel_path in local_files:
                left_label = f"a/{rel_path}"
                right_label = f"b/{rel_path}"
                left_path = str(upstream_files[rel_path])
                local_path = local_files[rel_path]
                local_text = local_path.read_text(encoding="utf-8", errors="replace")
                sanitized_text = strip_local_expectation_directives(local_text)
                if sanitized_text == local_text:
                    right_path = str(local_path)
                else:
                    sanitized_path = sanitized_root_path / rel_path
                    sanitized_path.parent.mkdir(parents=True, exist_ok=True)
                    sanitized_path.write_text(sanitized_text, encoding="utf-8")
                    right_path = str(sanitized_path)
            elif rel_path in upstream_files:
                left_label = f"a/{rel_path}"
                right_label = "/dev/null"
                left_path = str(upstream_files[rel_path])
                right_path = "/dev/null"
            else:
                left_label = "/dev/null"
                right_label = f"b/{rel_path}"
                left_path = "/dev/null"
                local_path = local_files[rel_path]
                local_text = local_path.read_text(encoding="utf-8", errors="replace")
                sanitized_path = sanitized_root_path / rel_path
                sanitized_path.parent.mkdir(parents=True, exist_ok=True)
                sanitized_path.write_text(
                    strip_local_expectation_directives(local_text), encoding="utf-8"
                )
                right_path = str(sanitized_path)

            result = subprocess.run(
                [
                    "diff",
                    "-u",
                    "--label",
                    left_label,
                    "--label",
                    right_label,
                    left_path,
                    right_path,
                ],
                text=True,
                capture_output=True,
            )
            if result.returncode not in (0, 1):
                raise RuntimeError(
                    f"Command failed ({result.returncode}): diff -u {left_path} {right_path}\n"
                    f"stdout:\n{result.stdout}\n"
                    f"stderr:\n{result.stderr}"
                )
            if result.returncode == 1:
                sections.append(result.stdout.rstrip("\n"))

    if not sections:
        return ""
    return "\n\n".join(sections).rstrip("\n") + "\n"


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Diff test/fixtures/e2e/upstream against darklang/dark "
            "backend/testfiles/execution while ignoring expected "
            "Builtin.testDerrorMessage vs error= differences."
        )
    )
    parser.add_argument(
        "--local-dir",
        default="test/fixtures/e2e/upstream",
        help="Local directory to compare",
    )
    parser.add_argument(
        "--upstream-dir",
        default=None,
        help=(
            "Existing upstream directory (backend/testfiles/execution). "
            "If omitted, this script fetches from GitHub."
        ),
    )
    parser.add_argument(
        "--repo-url",
        default="https://github.com/darklang/dark.git",
        help="Upstream git repo URL (used when --upstream-dir is omitted)",
    )
    parser.add_argument(
        "--ref",
        default="main",
        help="Upstream git ref to compare against (branch/tag/sha)",
    )
    parser.add_argument(
        "--upstream-subdir",
        default="backend/testfiles/execution",
        help="Subdirectory in upstream repo to compare",
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Optional output file for the diff report",
    )
    parser.add_argument(
        "--expected-diff-file",
        "--expected-patch-file",
        dest="expected_diff_file",
        default="scripts/upstream-execution-expected.patch",
        help=(
            "Path to expected unified diff patch baseline. "
            "If present, current diff is compared against it."
        ),
    )
    parser.add_argument(
        "--ignore-expected-diff",
        action="store_true",
        help="Ignore expected diff baseline even if --expected-diff-file exists",
    )
    parser.add_argument(
        "--regenerate-expected-diff",
        "--regenerate-expected-patch",
        action="store_true",
        help="Write current unified diff as the expected baseline and exit",
    )
    parser.add_argument(
        "--ignore-prefix",
        action="append",
        default=[],
        help=(
            "Path prefix to ignore from comparison (relative to compared roots). "
            "May be provided multiple times."
        ),
    )
    parser.add_argument(
        "--color",
        choices=["auto", "always", "never"],
        default="auto",
        help="Colorize output (default: auto)",
    )
    args = parser.parse_args()

    local_dir = Path(args.local_dir).resolve()
    if not local_dir.exists():
        print(f"Local directory not found: {local_dir}", file=sys.stderr)
        return 2

    tempdir: tempfile.TemporaryDirectory[str] | None = None
    if args.upstream_dir:
        upstream_dir = Path(args.upstream_dir).resolve()
    else:
        tempdir, upstream_dir = fetch_upstream_checkout(
            args.repo_url, args.ref, args.upstream_subdir
        )

    if not upstream_dir.exists():
        print(f"Upstream directory not found: {upstream_dir}", file=sys.stderr)
        if tempdir is not None:
            tempdir.cleanup()
        return 2

    # By default ignore cloud tests and upstream-only metadata/CLI fixtures.
    ignore_prefixes = [
        "cloud/",
        "cli/",
        "README.md",
        "README.me",
    ] + list(args.ignore_prefix)

    upstream_files = {
        k: v
        for k, v in collect_files(upstream_dir).items()
        if not is_ignored(k, ignore_prefixes)
    }
    local_files = {
        k: v
        for k, v in collect_files(local_dir).items()
        if not is_ignored(k, ignore_prefixes)
    }

    upstream_only, local_only, meaningful_diffs = build_current_state(
        upstream_files, local_files
    )
    current_patch_text = normalize_patch_text(build_patch_text(upstream_files, local_files))

    expected_diff_path = Path(args.expected_diff_file).resolve()

    if args.regenerate_expected_diff:
        expected_diff_path.parent.mkdir(parents=True, exist_ok=True)
        expected_diff_path.write_text(current_patch_text, encoding="utf-8")
        print(f"wrote expected patch baseline: {expected_diff_path}")
        print(
            "summary: "
            f"upstream_only={len(upstream_only)}, "
            f"local_only={len(local_only)}, "
            f"meaningful_diffs={len(meaningful_diffs)}"
        )
        if tempdir is not None:
            tempdir.cleanup()
        return 0

    expected_patch_text: str | None = None
    if not args.ignore_expected_diff and expected_diff_path.exists():
        expected_patch_text = normalize_patch_text(
            expected_diff_path.read_text(encoding="utf-8")
        )
    patch_matches_expected = (
        (expected_patch_text == current_patch_text)
        if expected_patch_text is not None
        else None
    )

    report_lines: list[str] = []
    report_lines.append(f"upstream: {upstream_dir}")
    report_lines.append(f"local:    {local_dir}")
    report_lines.append(
        "note: ignoring expected Builtin.testDerrorMessage <-> error=\"...\" differences"
    )
    if expected_patch_text is not None:
        report_lines.append(f"expected_patch_file: {expected_diff_path}")
    elif args.ignore_expected_diff:
        report_lines.append("note: expected diff baseline ignored via --ignore-expected-diff")
    else:
        report_lines.append(
            f"note: expected diff baseline not loaded (file missing): {expected_diff_path}"
        )
    report_lines.append("")
    report_lines.append(
        "summary(raw): "
        f"upstream_only={len(upstream_only)}, "
        f"local_only={len(local_only)}, "
        f"meaningful_diffs={len(meaningful_diffs)}"
    )
    report_lines.append("")

    unexpected_upstream_only = list(upstream_only)
    unexpected_local_only = list(local_only)
    unexpected_meaningful_diffs = list(meaningful_diffs)

    if expected_patch_text is None:
        if unexpected_upstream_only:
            report_lines.append("files only in upstream:")
            report_lines.extend([f"  {f}" for f in unexpected_upstream_only])
            report_lines.append("")

        if unexpected_local_only:
            report_lines.append("files only in local:")
            report_lines.extend([f"  {f}" for f in unexpected_local_only])
            report_lines.append("")
    else:
        report_lines.append(
            "summary(vs_expected_patch): "
            f"matches={'yes' if patch_matches_expected else 'no'}"
        )
        report_lines.append("")

        if not patch_matches_expected:
            report_lines.append("diff (expected_patch vs current_patch):")
            expected_lines = expected_patch_text.splitlines(keepends=False)
            current_lines = current_patch_text.splitlines(keepends=False)
            patch_diff_lines = difflib.unified_diff(
                expected_lines,
                current_lines,
                fromfile=f"expected/{expected_diff_path.name}",
                tofile="current/generated.patch",
                lineterm="",
            )
            report_lines.extend(list(patch_diff_lines))
            report_lines.append("")

    if expected_patch_text is None or not patch_matches_expected:
        for rel_path, upstream_text, local_text in unexpected_meaningful_diffs:
            report_lines.append(f"diff: {rel_path}")
            diff_lines = difflib.unified_diff(
                upstream_text.splitlines(keepends=False),
                local_text.splitlines(keepends=False),
                fromfile=f"upstream/{rel_path}",
                tofile=f"local/{rel_path}",
                lineterm="",
            )
            report_lines.extend(list(diff_lines))
            report_lines.append("")

    # Patch to be applied against upstream root using `patch -p1`.
    if expected_patch_text is None:
        report_lines.append("note: regenerate expected patch with --regenerate-expected-diff")
        report_lines.append("")

    use_color = should_use_color(args.color, writing_to_file=bool(args.output))
    if use_color:
        report_lines = [colorize_line(line) for line in report_lines]

    report = "\n".join(report_lines).rstrip() + "\n"

    if args.output:
        output_path = Path(args.output).resolve()
        output_path.write_text(report, encoding="utf-8")
        print(f"wrote report: {output_path}")
    else:
        print(report, end="")

    if tempdir is not None:
        tempdir.cleanup()

    if expected_patch_text is not None:
        return 0 if patch_matches_expected else 1

    if upstream_only or local_only or meaningful_diffs:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
