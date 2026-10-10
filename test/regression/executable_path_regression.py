#!/usr/bin/env python3
"""Check runtime executable identity after relocation and misleading launch arguments."""
import argparse
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("compile_all")
    parser.add_argument("--target", choices=["linux-x86_64", "linux-arm64", "macos-arm64"])
    parser.add_argument("--runner", help="Emulator executable for the selected target")
    arguments = parser.parse_args()
    compile_all = str(Path(arguments.compile_all).resolve())
    with tempfile.TemporaryDirectory(prefix="dark-executable-path-") as directory:
        root = Path(directory)
        source = root / "identity.dark"
        source.write_text('''let ready = Builtin.__launchWord (Builtin.__executableState ()) 0L in
let cached = Builtin.__cachedExecutablePath () in
let _ = Stdlib.Cli.Dir.setCurrent "/" in
let _ = Stdlib.Cli.Env.set "PATH" "/definitely/missing" in
let first = Builtin.getCurrentExecutablePath () in
let second = Stdlib.Cli.Sys.currentExecutablePath () in
let small = if Stdlib.Cli.__Posix.__mac () then first else Builtin.__linuxExecutablePath 1L in
if ready == 1L && cached == Some first && first == second && first == small && first == Builtin.getCurrentExecutablePath () then first
else Builtin.crash "Executable path results differ"
''', encoding="utf-8")
        subprocess.run([compile_all, str(source), str(root)], check=True, timeout=300)
        target = arguments.target or ("macos-arm64" if platform.system() == "Darwin" else (
            "linux-arm64" if platform.machine() in {"aarch64", "arm64"} else "linux-x86_64"))
        binary = root / target
        environment = dict(os.environ, PATH=str(root / "no-tools"))
        unrelated = root / "unrelated"
        unrelated.mkdir()

        def check(path):
            command = ([arguments.runner, "-0", "misleading-argv-zero", str(path)]
                       if arguments.runner else ["misleading-argv-zero"])
            result = subprocess.run(command, executable=arguments.runner or str(path), cwd=unrelated,
                                    env=environment, capture_output=True, text=True, timeout=30)
            assert result.returncode == 0, result.stdout + result.stderr
            assert result.stderr == "", result.stderr
            assert json.loads(result.stdout) == str(path.resolve()), result.stdout

        check(binary)
        # Exceed the Linux initial buffer size and exercise UTF-8 path conversion.
        long_directory = root
        for index in range(4):
            long_directory /= f"directory-{index}-" + "x" * 70
            long_directory.mkdir()
        relocated = long_directory / "copied-é-🚀"
        shutil.copy2(binary, relocated)
        check(relocated)
        renamed = long_directory / "renamed"
        relocated.rename(renamed)
        check(renamed)
        link = root / "launch-link"
        link.symlink_to(renamed)
        check(link)
        relative = subprocess.run([str(link.relative_to(root))], executable="./launch-link",
                                  cwd=root, env=environment, capture_output=True, text=True, timeout=30)
        assert relative.returncode == 0, relative.stdout + relative.stderr
        assert json.loads(relative.stdout) == str(renamed.resolve()), relative.stdout

        # Force absent aux-vector input to exercise argv and PATH fallbacks on
        # hosts where procfs and AT_EXECFN normally hide those branches.
        first_dir, second_dir, third_dir = root / "first", root / "second", root / "third"
        first_dir.mkdir()
        second_dir.mkdir()
        third_dir.mkdir()
        nonexecutable = first_dir / "program"
        nonexecutable.write_text("not executable")
        nonexecutable.chmod(0o600)
        executable = second_dir / "program"
        shutil.copy2(binary, executable)
        relative_link = root / "relative-link"
        relative_link.symlink_to("second/program")
        loop = root / "loop"
        loop.symlink_to("loop")
        quote = lambda value: json.dumps(str(value), ensure_ascii=False)
        expected = f"Some {quote(executable)}"
        checks = [
            f'Builtin.__selectExecutableFallback {quote(executable)} "/missing" "/" None == {expected}',
            f'Builtin.__selectExecutableFallback "/missing" "./relative-link" {quote(root)} None == {expected}',
            f'Builtin.__selectExecutableFallback "" "program" {quote(root)} (Some {quote(str(first_dir) + ":" + str(second_dir))}) == {expected}',
            f'Builtin.__selectExecutableFallback "" "relative-link" {quote(root)} (Some ":/missing") == {expected}',
            f'Builtin.__resolveExecutableCandidate {quote(loop)} "/" == None',
            f'Builtin.__resolveExecutableCandidate {quote(nonexecutable)} "/" == None',
            f'Builtin.__resolveExecutableCandidate {quote(second_dir)} "/" == None',
            'Builtin.__selectExecutableFallback "" "missing" "/" None == None',
        ]
        shutil.copy2(binary, third_dir / "program")
        checks.append(f'Builtin.__selectExecutableFallback "" "program" "/" (Some {quote(str(second_dir) + ":" + str(third_dir))}) == {expected}')
        source.write_text("\n".join(f"let checked{index} = ({expression}) in" for index, expression in enumerate(checks))
                          + "\nif " + " && ".join(f"checked{index}" for index in range(len(checks)))
                          + ' then true else Builtin.crash "Executable fallback selection failed"\n', encoding="utf-8")
        subprocess.run([compile_all, str(source), str(root)], check=True, timeout=300)
        result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=30)
        assert result.returncode == 0, result.stdout + result.stderr
        assert json.loads(result.stdout) is True, result.stdout
        assert result.stderr == "", result.stderr
    print(f"All native targets compile; {target} executable paths survive copy, rename, symlink, Unicode and forged argv[0]")


if __name__ == "__main__":
    main()
