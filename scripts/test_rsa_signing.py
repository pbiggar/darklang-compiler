#!/usr/bin/env python3
"""Verify pure Dark blinded PSS signatures with an independent cryptography verifier."""

import argparse
import datetime
import subprocess
import tempfile
import time
from pathlib import Path

from cryptography.exceptions import InvalidSignature
from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import padding, rsa
from cryptography.x509.oid import ExtendedKeyUsageOID, NameOID

ROOT = Path(__file__).resolve().parents[1]
MESSAGE = b"TLS server certificate transcript"
DARK = '''// signing.dark - Independent PSS signing, salt diversity and private-operation fault checks.
let run () : Unit =
  match Stdlib.Cli.Args.get 0 with
  | Error _ -> Stdlib.printLine "Invalid mode"
  | Ok mode ->
    let encoded = if mode == "mismatch" then "@WRONG@" else "@KEY@" in
    let parsed = Stdlib.Blob.fromHex "@CERT@" |> Stdlib.Result.andThen (fun certificate ->
      Stdlib.Blob.fromHex encoded |> Stdlib.Result.andThen (fun privateKey -> Stdlib.TlsServerIdentity.create certificate privateKey)) in
    match parsed with
    | Error message -> Stdlib.printLine message
    | Ok identity ->
      let key = identity.key in
      let selected = if mode == "fault" then { key with privateExponent = Stdlib.String.toBlob (Stdlib.String.repeat "\\u0000" 256) } else key in
      match Stdlib.RsaSigning.signSha256 selected (Stdlib.String.toBlob "TLS server certificate transcript") with
      | Error message -> Stdlib.printLine message
      | Ok first ->
        match Stdlib.RsaSigning.signSha256 key (Stdlib.String.toBlob "TLS server certificate transcript") with
        | Error message -> Stdlib.printLine message
        | Ok second ->
          Stdlib.printLine (Stdlib.Blob.toHex first)
          Stdlib.printLine (Stdlib.Blob.toHex second)
run ()
'''


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    parser.add_argument("--artifacts", type=Path, help="Keep generated probe and binary for machine-code inspection")
    args = parser.parse_args()
    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    pem = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
    wrong = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    wrong_pem = wrong.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
    now = datetime.datetime.now(datetime.timezone.utc)
    name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "localhost")])
    certificate = (x509.CertificateBuilder().subject_name(name).issuer_name(name)
        .public_key(key.public_key()).serial_number(1).not_valid_before(now - datetime.timedelta(minutes=1))
        .not_valid_after(now + datetime.timedelta(days=1))
        .add_extension(x509.BasicConstraints(ca=False, path_length=None), critical=True)
        .add_extension(x509.ExtendedKeyUsage([ExtendedKeyUsageOID.SERVER_AUTH]), critical=False)
        .sign(key, hashes.SHA256())).public_bytes(serialization.Encoding.PEM)
    with tempfile.TemporaryDirectory(prefix="dark-rsa-signing-") as temporary:
        directory = args.artifacts or Path(temporary)
        directory.mkdir(parents=True, exist_ok=True)
        path, binary = directory / "signing.dark", directory / "signing"
        path.write_text(DARK.replace("@KEY@", pem.hex()).replace("@CERT@", certificate.hex()).replace("@WRONG@", wrong_pem.hex()))
        compiled = subprocess.run([str(args.compiler), str(path), "--leak-check", "-o", str(binary)],
            cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        started = time.monotonic()
        result = subprocess.run([str(binary), "valid"], cwd=ROOT, text=True, capture_output=True, timeout=60)
        elapsed = time.monotonic() - started
        assert result.returncode == 0 and not result.stderr, result
        lines = result.stdout.splitlines()
        assert len(lines) == 2, result
        signatures = [bytes.fromhex(line) for line in lines]
        assert signatures[0] != signatures[1]
        for signature in signatures:
            assert len(signature) == 256
            key.public_key().verify(signature, MESSAGE, padding.PSS(mgf=padding.MGF1(hashes.SHA256()), salt_length=32), hashes.SHA256())
            try:
                key.public_key().verify(signature, MESSAGE + b"wrong", padding.PSS(mgf=padding.MGF1(hashes.SHA256()), salt_length=32), hashes.SHA256())
                raise AssertionError("Wrong-message signature accepted")
            except InvalidSignature:
                pass
        fault = subprocess.run([str(binary), "fault"], cwd=ROOT, text=True, capture_output=True, timeout=60)
        assert fault.returncode == 0 and not fault.stderr and fault.stdout.strip() == "RSA private operation failed", fault
        mismatch = subprocess.run([str(binary), "mismatch"], cwd=ROOT, text=True, capture_output=True, timeout=30)
        assert mismatch.returncode == 0 and not mismatch.stderr and mismatch.stdout.strip() == "TLS server certificate and private key do not match", mismatch
    print(f"RSA-PSS server signing verified: identity binding, independent signatures, salt diversity, wrong-message/fault rejection, cleanup; two signatures {elapsed:.3f}s")


if __name__ == "__main__":
    main()
