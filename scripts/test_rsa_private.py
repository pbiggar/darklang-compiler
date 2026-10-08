#!/usr/bin/env python3
"""Check pure Dark RSA private-key import using independent generated PKCS encodings."""

import argparse
import subprocess
import tempfile
from pathlib import Path

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric import rsa

ROOT = Path(__file__).resolve().parents[1]


def tlv(tag, content):
    length = len(content)
    size = (length.bit_length() + 7) // 8
    encoded_length = bytes([length]) if length < 128 else bytes([128 + size]) + length.to_bytes(size, "big")
    return bytes([tag]) + encoded_length + content


def integer(value):
    encoded = value.to_bytes(max(1, (value.bit_length() + 7) // 8), "big")
    return tlv(2, b"\x00" + encoded if encoded[0] & 128 else encoded)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    numbers = key.private_numbers()
    values = [0, numbers.public_numbers.n, numbers.public_numbers.e, numbers.d,
              numbers.p, numbers.q, numbers.dmp1, numbers.dmq1, numbers.iqmp]
    encode = lambda fields: tlv(48, b"".join(integer(value) for value in fields))
    pkcs1 = key.private_bytes(serialization.Encoding.DER, serialization.PrivateFormat.TraditionalOpenSSL,
        serialization.NoEncryption())
    assert pkcs1 == encode(values)
    pkcs8 = key.private_bytes(serialization.Encoding.DER, serialization.PrivateFormat.PKCS8,
        serialization.NoEncryption())
    vectors = [(pkcs1, "pkcs1", True), (pkcs8, "pkcs8", True)]
    for format in (serialization.PrivateFormat.TraditionalOpenSSL, serialization.PrivateFormat.PKCS8):
        vectors.append((key.private_bytes(serialization.Encoding.PEM, format, serialization.NoEncryption()), "pem", True))
    for index in (1, 2, 3, 6, 7, 8):
        changed = list(values)
        changed[index] += 2
        vectors.append((encode(changed), "pkcs1", False))
    for changed in ([1] + values[1:], values[:5] + [values[4]] + values[6:]):
        vectors.append((encode(changed), "pkcs1", False))
    vectors += [(pkcs1[:-1], "pkcs1", False), (pkcs1 + b"\x00", "pkcs1", False),
                (pkcs8[:-1], "pkcs8", False), (pkcs8 + b"\x00", "pkcs8", False)]
    unsupported = rsa.generate_private_key(public_exponent=65537, key_size=3072)
    vectors.append((unsupported.private_bytes(serialization.Encoding.DER, serialization.PrivateFormat.PKCS8,
        serialization.NoEncryption()), "pkcs8", False))
    checks = [f'check "{data.hex()}" "{format}" {str(valid).lower()}' for data, format, valid in vectors]
    source = '''// import.dark - Generated key import and consistency rejection with cleanup accounting.
let check (text: String) (format: String) (valid: Bool) : Bool =
  let parsed = Stdlib.Blob.fromHex text |> Stdlib.Result.andThen (fun data ->
    if format == "pkcs1" then Stdlib.RsaPrivate.parsePkcs1 data
    else if format == "pkcs8" then Stdlib.RsaPrivate.parsePkcs8 data
    else Stdlib.RsaPrivate.parsePem data) in
  match parsed with
  | Error _ -> Stdlib.Bool.not valid
  | Ok key -> valid && Stdlib.Blob.toHex key.modulus == "@N@" && Stdlib.Blob.toHex key.exponent == "010001" && Stdlib.Blob.toHex key.privateExponent == "@D@"
''' + '\nStdlib.printLine (if ' + ' &&\n  '.join(checks) + ' then "DONE" else "FAILED")\n'
    source = source.replace("@N@", numbers.public_numbers.n.to_bytes(256, "big").hex().upper())
    source = source.replace("@D@", numbers.d.to_bytes(256, "big").hex().upper())
    with tempfile.TemporaryDirectory(prefix="dark-private-import-") as temporary:
        path, binary = Path(temporary) / "import.dark", Path(temporary) / "import"
        path.write_text(source)
        compiled = subprocess.run([str(args.compiler), str(path), "--leak-check", "-o", str(binary)],
            cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        result = subprocess.run([str(binary)], cwd=ROOT, text=True, capture_output=True, timeout=30)
        assert result.returncode == 0 and not result.stderr and result.stdout.strip() == "DONE", result
    print(f"RSA private-key import verified: {len(vectors)} generated DER/PEM, consistency and cleanup cases")


if __name__ == "__main__":
    main()
