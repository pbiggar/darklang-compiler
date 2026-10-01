"""Compile Haskell, Roc, and Koka in isolated directories and measure native executables."""

from pathlib import Path
import shutil
import subprocess

from benchmark_profiles import load_invocation
from diagnostic_references import parse_instruction_count
from reference_snapshots import source_path


COMPILERS = {"haskell": "ghc", "roc": "roc", "koka": "koka"}


def build_flags(language: str, roc_mode: str = "legacy") -> list[str]:
    if language == "haskell":
        return ["-O2", "-j1", "-fno-full-laziness"]
    if language == "koka":
        return ["-O2", "--compile", "--target=c", "--jobs=1"]
    if roc_mode == "current":
        return ["build", "--opt=speed", "--jobs=1"]
    return ["build", "--optimize", "--max-threads=1"]


def roc_mode(executable: str) -> str:
    help_text = subprocess.run([executable, "build", "--help"], check=True,
                               capture_output=True, text=True, timeout=60).stdout
    if "--opt=" in help_text:
        return "current"
    if "--optimize" in help_text:
        return "legacy"
    raise ValueError("Roc compiler exposes neither --opt=speed nor --optimize; unsupported build interface")


def version_command(language: str, executable: str, mode: str = "legacy") -> list[str]:
    return [executable, "version" if language == "roc" and mode == "current" else "--version"]


def measure_native(root: Path, temporary: Path, name: str, language: str,
                   profile: str, executable: str, mode: str, timeout: int) -> dict:
    original = source_path(root, name, language)
    prepared = temporary / name / language
    shutil.copytree(original.parent, prepared, ignore=shutil.ignore_patterns(
        ".koka", "build", "target", "main", "quick", "benchmark", "*.o", "*.hi", "*.dyn_o", "*.dyn_hi",
    ))
    source = prepared / original.name
    binary = prepared / "benchmark"
    flags = build_flags(language, mode)
    if language == "haskell":
        command = [executable, *flags, "-i" + str(prepared), "-outputdir", str(prepared / "build"),
                   "-o", str(binary), str(source)]
    elif language == "koka":
        command = [executable, *flags, "--builddir=" + str(prepared / "build"),
                   "--output=" + str(binary), str(source)]
    else:
        command = [executable, *flags, "--output=" + str(binary), str(source)]
    subprocess.run(command, cwd=prepared, check=True, capture_output=True, text=True, timeout=timeout)
    if not binary.is_file():
        raise ValueError(f"{name} {language}: compiler did not produce the requested executable")
    invocation = load_invocation(root, profile, name)
    run = subprocess.run([
        "valgrind", "--tool=cachegrind", "--cache-sim=no", "--branch-sim=no",
        "--main-stacksize=536870912", "--cachegrind-out-file=/dev/null", str(binary), *invocation.args,
    ], cwd=prepared, check=True, capture_output=True, text=True, timeout=timeout)
    if run.stdout != invocation.expected_stdout:
        raise ValueError(f"{name} {language} output mismatch: expected {invocation.expected_stdout!r}, got {run.stdout!r}")
    return {"name": name, "instructions": parse_instruction_count(run.stderr), "output_valid": True}
