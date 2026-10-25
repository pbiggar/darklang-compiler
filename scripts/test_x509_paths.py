#!/usr/bin/env python3
"""Exercise native X.509 path selection using ephemeral public certificates."""

import datetime
from pathlib import Path
import subprocess
import tempfile

from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.x509.oid import NameOID, ExtendedKeyUsageOID

ROOT = Path(__file__).resolve().parents[1]


def main():
    now = datetime.datetime.now(datetime.timezone.utc)

    def issue(name, issuer=None, ca=True, key=None):
        if key is None:
            key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
        subject = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, name)])
        certificate = (
            x509.CertificateBuilder()
            .subject_name(subject)
            .issuer_name(issuer[1].subject if issuer else subject)
            .public_key(key.public_key())
            .serial_number(x509.random_serial_number())
            .not_valid_before(now - datetime.timedelta(hours=1))
            .not_valid_after(now + datetime.timedelta(days=1))
            .add_extension(x509.BasicConstraints(ca=ca, path_length=None), critical=True)
        )
        if not ca:
            certificate = certificate.add_extension(
                x509.SubjectAlternativeName([x509.DNSName("service.example")]), False
            ).add_extension(x509.ExtendedKeyUsage([ExtendedKeyUsageOID.SERVER_AUTH]), False)
        return key, certificate.sign(issuer[0] if issuer else key, hashes.SHA256())

    root = issue("Root")
    first = issue("First", root)
    second = issue("Second", first)
    leaf = issue("service.example", second, False)
    other_root = issue("Other root")
    decoy = issue("First", other_root)
    cross = issue("First", other_root, key=first[0])
    certificates = dict(root=root, first=first, second=second, leaf=leaf, decoy=decoy, cross=cross)
    declarations = []
    for name, (_, cert) in certificates.items():
        encoded = cert.public_bytes(serialization.Encoding.DER).hex()
        declarations.append(f'let {name} = Builtin.unwrap (Stdlib.Blob.fromHex "{encoded}")')
    cases = [
        ("ordered", "[leaf, second, first]", "[root]", "service.example", True),
        ("reordered", "[leaf, first, second]", "[root]", "service.example", True),
        ("unrelated", "[leaf, decoy, second, first]", "[root]", "service.example", True),
        ("duplicate", "[leaf, second, second, first]", "[root]", "service.example", True),
        ("backtrack", "[leaf, second, cross, first]", "[root]", "service.example", True),
        ("missing issuer", "[leaf, first]", "[root]", "service.example", False),
        ("wrong root", "[leaf, second, first]", "[decoy]", "service.example", False),
        ("wrong host", "[leaf, second, first]", "[root]", "wrong.example", False),
        ("empty chain", "[]", "[root]", "service.example", False),
    ]
    checks = []
    for label, chain, roots, host, expected in cases:
        actual = (f'Stdlib.Result.isOk (Stdlib.__X509Chain.verifyAt {chain} {roots} '
                  f'"{host}" (Stdlib.DateTime.now ()))')
        checks.append(f'Stdlib.printLine (if {actual} == {str(expected).lower()} then "PASS {label}" else "FAIL {label}")')
    with tempfile.TemporaryDirectory(prefix="dark-x509-paths-") as temporary:
        source = Path(temporary) / "paths.dark"
        binary = Path(temporary) / "paths"
        source.write_text("\n".join(declarations + checks) + "\n")
        subprocess.run([str(ROOT / "dark"), "--allow-internal", str(source), "-o", str(binary)], cwd=ROOT, check=True)
        result = subprocess.run([str(binary)], text=True, capture_output=True, timeout=60, check=True)
        expected = "".join(f"PASS {label}\n" for label, *_ in cases)
        if result.stdout != expected or result.stderr:
            raise AssertionError(f"X.509 paths failed: {result.stdout!r} {result.stderr!r}")
        print(f"Native X.509 path checks: {len(cases)}/{len(cases)} passed")


if __name__ == "__main__":
    main()
