#!/usr/bin/env python3
"""Exercise native replay, saved findings, Unicode reduction, and oracle failures."""
from pathlib import Path
import subprocess
import sys
import tempfile


def main():
    tester = Path(sys.argv[1]).resolve()
    with tempfile.TemporaryDirectory(prefix="dark-differential-test-check-") as temporary:
        root = Path(temporary)
        oracle = root / "oracle"
        source = root / "input.dark"
        artifacts = root / "artifacts"

        def run(*arguments):
            return subprocess.run([str(tester), "--interpreter", str(oracle),
                                   "--artifacts", str(artifacts), *arguments],
                                  capture_output=True, text=True, timeout=120, check=False)

        def oracle_output(value):
            oracle.write_text(f"#!/bin/sh\nprintf '%s\\n' '{value}'\n")
            oracle.chmod(0o755)

        oracle_output("42")
        source.write_text("6L * 7L\n")
        replay = run("--replay", str(source))
        assert replay.returncode == 0, replay.stdout + replay.stderr
        oracle_output("true")
        source.write_text('let value = "hé🚀" in value == "hé🚀"\n')
        replay = run("--replay", str(source))
        assert replay.returncode == 0, replay.stdout + replay.stderr

        oracle_output("0")
        source.write_text('let unused = "hé🚀" in if true then 4L + 1L else 99L\n')
        minimized = run("--minimize", str(source))
        assert minimized.returncode == 0, minimized.stdout + minimized.stderr
        reduced = source.with_suffix(".min.dark").read_text()
        assert len(reduced) < len(source.read_text()), reduced
        replay = run("--replay", str(source.with_suffix(".min.dark")))
        assert replay.returncode == 1 and "result mismatch" in replay.stderr, replay

        source.write_text("let bad (input: Int64) : Bool = input\nif true then 4L + 1L else 99L\n")
        minimized = run("--minimize", str(source))
        assert minimized.returncode == 0, minimized.stdout + minimized.stderr
        reduced = source.with_suffix(".min.dark").read_text()
        assert "let bad" in reduced and len(reduced) < len(source.read_text()), reduced
        assert "compiler rejected" in minimized.stdout, minimized

        oracle_output("unsupported")
        source.write_text("42L\n")
        replay = run("--replay", str(source))
        assert replay.returncode != 0 and "interpreter rejected" in replay.stderr, replay
        missing = subprocess.run([str(tester), "--interpreter", str(root / "missing"),
                                  "--replay", str(source), "--artifacts", str(artifacts)],
                                 capture_output=True, text=True, timeout=20, check=False)
        assert missing.returncode == 2 and "not on PATH" in missing.stderr, missing

        oracle_output("0")
        finding = run("--seed", "1234", "--depth", "1", "--limit", "10")
        assert finding.returncode == 1, finding.stdout + finding.stderr
        saved = list(artifacts.glob("seed-*-case-*.dark"))
        assert len(saved) == 1 and saved[0].with_suffix(".txt").is_file(), saved
        assert (artifacts / "current.dark").is_file()
    print("Native tester replay, reduction, oracle boundaries, and artifacts passed")


if __name__ == "__main__":
    main()
