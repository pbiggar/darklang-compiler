#!/usr/bin/env python3
"""Compare Dark's public RSA modular powers with Python's independent pow oracle."""

import argparse
import random
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    generator = random.Random(94607838)
    checks = []
    for width in (1, 2, 3, 17, 31, 32, 33, 256, 257, 384, 512, 1024):
        modulus = generator.getrandbits(width * 8) | (1 << (width * 8 - 1)) | 1
        for base, exponent in ((0, 65537), (modulus - 1, 0xFFFFFFFF),
                               (generator.randrange(modulus), 65537), (generator.randrange(modulus), 0)):
            encoded = [value.to_bytes(size, "big").hex().upper() for value, size in
                ((base, width), (exponent, 4), (modulus, width), (pow(base, exponent, modulus), width))]
            checks.append('check "' + '" "'.join(encoded) + '"')
    source = '''// oracle.dark - Independent public modular-power vectors and scratch cleanup.
let check (base: String) (exponent: String) (modulus: String) (expected: String) : Bool =
  let result = Stdlib.Blob.fromHex base |> Stdlib.Result.andThen (fun base ->
    Stdlib.Blob.fromHex exponent |> Stdlib.Result.andThen (fun exponent ->
      Stdlib.Blob.fromHex modulus |> Stdlib.Result.andThen (fun modulus ->
        Stdlib.__RsaMontgomery.publicPow base exponent modulus |> Stdlib.Result.map Stdlib.Blob.toHex))) in
  match result with | Ok actual -> actual == expected | Error _ -> false
''' + '\nStdlib.printLine (if ' + ' &&\n  '.join(checks) + ' then "DONE" else "FAILED")\n'
    with tempfile.TemporaryDirectory(prefix="dark-montgomery-") as temporary:
        path, binary = Path(temporary) / "oracle.dark", Path(temporary) / "oracle"
        path.write_text(source)
        compiled = subprocess.run([str(args.compiler), str(path), "--leak-check", "-o", str(binary)],
            cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        result = subprocess.run([str(binary)], cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert result.returncode == 0 and not result.stderr and result.stdout.strip() == "DONE", result
    print(f"Public RSA Montgomery powers verified: {len(checks)} Python oracle vectors, 8–8192 bits, cleanup")


if __name__ == "__main__":
    main()
