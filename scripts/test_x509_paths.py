#!/usr/bin/env python3
"""Exercise native X.509 path selection using ephemeral public certificates."""

import argparse
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
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark", help="Native compiler executable to verify")
    parser.add_argument("--oracle-only", action="store_true", help="Validate generated path fixtures with OpenSSL without compiling Dark")
    parser.add_argument("--paths-only", action="store_true", help="Skip trust-source discovery when comparing verifier versions")
    options = parser.parse_args()
    now = datetime.datetime.now(datetime.timezone.utc)

    def issue(name, issuer=None, ca=True, key=None, expired=False, server_auth=True, path_length=None):
        if key is None:
            key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
        subject = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, name)])
        certificate = (
            x509.CertificateBuilder()
            .subject_name(subject)
            .issuer_name(issuer[1].subject if issuer else subject)
            .public_key(key.public_key())
            .serial_number(x509.random_serial_number())
            .not_valid_before(now - datetime.timedelta(days=2))
            .not_valid_after(now + datetime.timedelta(days=-1 if expired else 1))
            .add_extension(x509.BasicConstraints(ca=ca, path_length=path_length), critical=True)
            .add_extension(x509.SubjectKeyIdentifier.from_public_key(key.public_key()), False)
            .add_extension(x509.AuthorityKeyIdentifier.from_issuer_public_key(
                issuer[0].public_key() if issuer else key.public_key()), False)
        )
        if not ca:
            certificate = certificate.add_extension(
                x509.SubjectAlternativeName([x509.DNSName("service.example")]), False
            ).add_extension(x509.ExtendedKeyUsage([
                ExtendedKeyUsageOID.SERVER_AUTH if server_auth else ExtendedKeyUsageOID.CLIENT_AUTH
            ]), False)
        return key, certificate.sign(issuer[0] if issuer else key, hashes.SHA256())

    root = issue("Root")
    first = issue("First", root)
    second = issue("Second", first)
    leaf = issue("service.example", second, False)
    other_root = issue("Other root")
    decoy = issue("First", other_root)
    expired = issue("service.example", second, False, expired=True)
    client_only = issue("service.example", second, False, server_auth=False)
    not_ca = issue("Not CA", root, False)
    bad_issuer_leaf = issue("service.example", not_ca, False)
    limited_root = issue("Limited root", path_length=0)
    limited_ca = issue("Limited intermediate", limited_root)
    limited_leaf = issue("service.example", limited_ca, False)
    certificates = dict(root=root, first=first, second=second, leaf=leaf, decoy=decoy,
                        expired=expired, clientOnly=client_only, notCA=not_ca,
                        badIssuerLeaf=bad_issuer_leaf, limitedRoot=limited_root,
                        limitedCA=limited_ca, limitedLeaf=limited_leaf)
    cases = [
        ("ordered", "[leaf, second, first]", "[root]", "service.example", True),
        ("reordered", "[leaf, first, second]", "[root]", "service.example", True),
        ("unrelated", "[leaf, decoy, second, first]", "[root]", "service.example", True),
        ("duplicate", "[leaf, second, second, first]", "[root]", "service.example", True),
        ("missing issuer", "[leaf, first]", "[root]", "service.example", False),
        ("wrong root", "[leaf, second, first]", "[decoy]", "service.example", False),
        ("wrong host", "[leaf, second, first]", "[root]", "wrong.example", False),
        ("expired leaf", "[expired, second, first]", "[root]", "service.example", False),
        ("client-only usage", "[clientOnly, second, first]", "[root]", "service.example", False),
        ("issuer is not CA", "[badIssuerLeaf, notCA]", "[root]", "service.example", False),
        ("CA path length", "[limitedLeaf, limitedCA]", "[limitedRoot]", "service.example", False),
        ("empty chain", "[]", "[root]", "service.example", False),
    ]
    with tempfile.TemporaryDirectory(prefix="dark-x509-paths-") as temporary:
        declarations = []
        for name, (_, cert) in certificates.items():
            certificate_path = Path(temporary) / f"{name}.der"
            certificate_path.write_bytes(cert.public_bytes(serialization.Encoding.DER))
            declarations.append(
                f'let {name} = Builtin.unwrap (Stdlib.Cli.FileSystem.readFile "{certificate_path}")')
        def pem(names):
            return b"".join(certificates[name][1].public_bytes(serialization.Encoding.PEM) for name in names)

        def names(expression):
            return [name.strip() for name in expression.strip("[]").split(",") if name.strip()]

        for label, chain, anchors, host, expected in cases:
            members = names(chain)
            if not members:
                continue
            leaf_path = Path(temporary) / "oracle-leaf.pem"
            anchor_path = Path(temporary) / "oracle-anchors.pem"
            intermediate_path = Path(temporary) / "oracle-intermediates.pem"
            leaf_path.write_bytes(pem(members[:1]))
            anchor_path.write_bytes(pem(names(anchors)))
            arguments = ["openssl", "verify", "-trusted", str(anchor_path),
                         "-purpose", "sslserver", "-verify_hostname", host]
            if len(members) > 1:
                intermediate_path.write_bytes(pem(members[1:]))
                arguments += ["-untrusted", str(intermediate_path)]
            oracle = subprocess.run(arguments + [str(leaf_path)], capture_output=True, text=True, timeout=15)
            if (oracle.returncode == 0) != expected:
                raise AssertionError(f"Invalid fixture {label}: {oracle.stdout} {oracle.stderr}")
        print(f"OpenSSL path fixtures: {len(cases) - 1} passed")
        if options.oracle_only:
            return
        authorities = Path(temporary) / "authorities"
        authorities.mkdir()
        root_file = authorities / "root.pem"
        root_file.write_bytes(root[1].public_bytes(serialization.Encoding.PEM))
        (authorities / "not-a-certificate.txt").write_text("not a certificate")
        (authorities / "subdirectory").mkdir()
        sources = [
            ("bundle discovery", f'["{root_file}"]', "[]"),
            ("directory discovery", "[]", f'["{authorities}"]'),
        ]
        if options.paths_only:
            sources = []
        execution_cases = list(cases)
        for index, (label, files, directories) in enumerate(sources):
            loaded = f'Stdlib.__Tls13Client.__readRootsFromSources {files} {directories}'
            declarations.append(f'let discovered{index} = Builtin.unwrap ({loaded})')
            execution_cases.append((label, "[leaf, first, second]", f"discovered{index}", "service.example", True))
        rows = [f'("{label}", {chain}, {anchors}, "{host}", {str(expected).lower()})'
                for label, chain, anchors, host, expected in execution_cases]
        # One verifier call keeps compilation bounded as more cases are added.
        check = ('Stdlib.List.iter cases (fun (label, chain, anchors, host, expected) ->\n'
                 '  let actual = Stdlib.Result.isOk (Stdlib.__X509Chain.verifyAt chain anchors host (Stdlib.DateTime.now ())) in\n'
                 '  Stdlib.printLine ((if actual == expected then "PASS " else "FAIL ") + label))')
        source = Path(temporary) / "paths.dark"
        binary = Path(temporary) / "paths"
        source.write_text("\n".join(declarations) + "\nlet cases = [" + ",\n".join(rows) + "]\n" + check + "\n")
        subprocess.run([str(options.compiler.resolve()), "--allow-internal", str(source), "-o", str(binary)], cwd=ROOT, check=True, timeout=180)
        result = subprocess.run([str(binary)], text=True, capture_output=True, timeout=60, check=True)
        expected = "".join(f"PASS {label}\n" for label, *_ in cases + sources)
        if result.stdout != expected or result.stderr:
            raise AssertionError(f"X.509 paths failed: {result.stdout!r} {result.stderr!r}")
        print(f"Native X.509 path and trust-source checks: {len(cases) + len(sources)} passed")


if __name__ == "__main__":
    main()
