#!/usr/bin/env python3
"""Execute ordinary CLI programs inside automatically supplied native task scopes."""
import pathlib
import platform
import subprocess
import sys
import tempfile


def main():
    if platform.system() != "Linux" or platform.machine() not in ("x86_64", "amd64"):
        print("Automatic native task entry requires Linux x64")
        return
    compiler = str(pathlib.Path(sys.argv[1]).resolve())
    cases = [
        ("demand", '''let pass (value: Int64) : Int64 = value
let delayed = pass (Stdlib.__Task.__delayTransparent 2L 40L) in
let _ = Builtin.print (Stdlib.Int64.toString (delayed + 2L)) in
0L
''', b"42"),
        ("drain", '''let ignored = Stdlib.__Task.__startCurrent (fun _unit ->
  let delayed = Stdlib.__Task.__delayTransparent 2L "finished" in
  Builtin.print delayed) in
let _ = Builtin.print "main" in
0L
''', b"mainfinished"),
        ("nested", '''let before = __rawptr_to_word (__fiber_current ()) in
let nested = Stdlib.__Task.__runMain (fun _unit ->
  __rawptr_to_word (__fiber_current ()) != before) in
let restored = __rawptr_to_word (__fiber_current ()) == before in
let _ = Builtin.print (if before != 0L && nested && restored then "restored" else "lost") in
0L
''', b"restored"),
        ("error", '''let result = Stdlib.__Task.__delayTransparent 2L (Error "failed") in
let text = match result with | Ok value -> Stdlib.Int64.toString value | Error message -> message in
let _ = Builtin.print text in
0L
''', b"failed"),
    ]
    with tempfile.TemporaryDirectory(prefix="dark-automatic-entry-") as directory:
        root = pathlib.Path(directory)
        for name, program, expected in cases:
            source = root / (name + ".dark")
            binary = root / name
            source.write_text(program, encoding="utf-8")
            subprocess.run([compiler, "-q", "--allow-internal", "--leak-check",
                            str(source), "-o", str(binary)], check=True, timeout=120)
            result = subprocess.run([str(binary)], capture_output=True, timeout=15)
            assert (result.returncode, result.stdout, result.stderr) == (0, expected, b""), (name, result)
    print("Automatic CLI task entry forces pending values, drains producers and restores nested scopes")


if __name__ == "__main__":
    main()
