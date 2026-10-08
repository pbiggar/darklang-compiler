#!/usr/bin/env python3
"""Exercise the pure Dark TLS server flight and client authentication using OpenSSL."""

import argparse
import datetime
import ssl
import subprocess
import tempfile
from pathlib import Path

from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.x509.oid import ExtendedKeyUsageOID, NameOID

ROOT = Path(__file__).resolve().parents[1]
DARK = '''// flight.dark - Independent TLS server transcript, Finished and application key probe.
let arg (index: Int64) : String = Stdlib.Cli.Args.get (Stdlib.Int.fromInt64 index) |> Stdlib.Result.withDefault ""
let bytes (index: Int64) : Blob = Stdlib.Blob.fromHex (arg index) |> Stdlib.Result.withDefault Stdlib.Blob.empty
let keys (index: Int64) : Stdlib.Tls13.TrafficKeys = Stdlib.Tls13.TrafficKeys { key = bytes index, iv = bytes (index + 1L), sequence = 0L, cipherSuite = 4865L }
let print (data: Blob) : Unit = Stdlib.printLine (Stdlib.Blob.toHex data)
let start () : Stdlib.Result.Result<Unit, String> =
  Stdlib.Blob.fromHex "@CERT@" |> Stdlib.Result.andThen (fun certificate ->
    Stdlib.Blob.fromHex "@KEY@" |> Stdlib.Result.andThen (fun key ->
      Stdlib.TlsServerIdentity.create certificate key |> Stdlib.Result.andThen (fun identity ->
        Stdlib.Tls13ServerHandshake.start identity (bytes 1L) ["h2", "http/1.1"] None |> Stdlib.Result.andThen (fun flight ->
          Stdlib.Tls13.serializePlaintext 22L flight.hello |> Stdlib.Result.andThen (fun hello ->
            Stdlib.Tls13.sealRecord flight.handshakeSend 22L flight.encrypted |> Stdlib.Result.andThen (fun encrypted ->
              Stdlib.Tls13.sealRecord flight.applicationSend 23L (Stdlib.String.toBlob "server application bytes") |> Stdlib.Result.map (fun application ->
                print hello
                print encrypted.bytes
                print application.bytes
                print flight.handshakeReceive.key
                print flight.handshakeReceive.iv
                print flight.clientFinished
                print flight.applicationReceive.key
                print flight.applicationReceive.iv)))))))
let verify () : Stdlib.Result.Result<Unit, String> =
  let receive = keys 1L in
  let application = keys 4L in
  let flight = Stdlib.Tls13ServerHandshake.Flight { hello = Stdlib.Blob.empty, encrypted = Stdlib.Blob.empty, transcript = Stdlib.Blob.empty, handshakeReceive = receive, handshakeSend = receive, clientFinished = bytes 3L, applicationReceive = application, applicationSend = application, protocol = "h2" } in
  match Stdlib.Tls13.parseRecord (bytes 6L) with
  | Error _ -> Error "Invalid client record"
  | Ok record -> Stdlib.Tls13.openRecord receive record |> Stdlib.Result.andThen (fun opened ->
    if opened.contentType != 22L then Error "Invalid client handshake content type"
    else Stdlib.Tls13ServerHandshake.finish flight opened.content |> Stdlib.Result.andThen (fun ready ->
      let changed = Stdlib.Blob.concat [Stdlib.Blob.slice opened.content 0 35, Stdlib.Blob.__fromInt64List [Stdlib.Int64.bitwiseXor (Stdlib.Blob.__getByte opened.content 35L) 1L]] in
      let rejects = match Stdlib.Tls13ServerHandshake.finish flight changed with | Error _ -> true | Ok _ -> false in
      let truncates = match Stdlib.Tls13ServerHandshake.finish flight (Stdlib.Blob.slice opened.content 0 35) with | Error _ -> true | Ok _ -> false in
      match Stdlib.Tls13.parseRecord record.remaining with
      | Error _ -> Error "Missing client application data"
      | Ok data -> Stdlib.Tls13.openRecord ready.receive data |> Stdlib.Result.map (fun chunk ->
        Stdlib.printLine (if rejects && truncates && chunk.contentType == 23L && Stdlib.Blob.toHex chunk.content == Stdlib.Blob.toHex (Stdlib.String.toBlob "client application bytes") then "DONE" else "FAILED"))))
match (if arg 0L == "start" then start () else verify ()) with
| Error message -> Stdlib.printLine message
| Ok () -> ()
'''


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    now = datetime.datetime.now(datetime.timezone.utc)
    name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "localhost")])
    certificate = (x509.CertificateBuilder().subject_name(name).issuer_name(name)
                   .public_key(key.public_key()).serial_number(1)
                   .not_valid_before(now - datetime.timedelta(minutes=1))
                   .not_valid_after(now + datetime.timedelta(days=1))
                   .add_extension(x509.BasicConstraints(ca=False, path_length=None), critical=True)
                   .add_extension(x509.SubjectAlternativeName([x509.DNSName("localhost")]), critical=False)
                   .add_extension(x509.ExtendedKeyUsage([ExtendedKeyUsageOID.SERVER_AUTH]), critical=False)
                   .sign(key, hashes.SHA256())).public_bytes(serialization.Encoding.PEM)
    pem = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8,
                            serialization.NoEncryption())
    with tempfile.TemporaryDirectory(prefix="dark-server-flight-") as temporary:
        path, binary = Path(temporary) / "flight.dark", Path(temporary) / "flight"
        path.write_text(DARK.replace("@CERT@", certificate.hex()).replace("@KEY@", pem.hex()))
        compiled = subprocess.run([str(args.compiler), str(path), "--allow-internal", "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        for protocols in (["h2", "http/1.1"], ["http/1.1"], []):
            context = ssl.create_default_context(cadata=certificate.decode())
            context.minimum_version = context.maximum_version = ssl.TLSVersion.TLSv1_3
            context.set_ecdh_curve("X25519")
            context.set_alpn_protocols(protocols)
            incoming, outgoing = ssl.MemoryBIO(), ssl.MemoryBIO()
            client = context.wrap_bio(incoming, outgoing, server_side=False, server_hostname="localhost")
            try:
                client.do_handshake()
            except ssl.SSLWantReadError:
                pass
            hello = outgoing.read()
            assert hello[0] == 22 and len(hello) == 5 + int.from_bytes(hello[3:5], "big")
            result = subprocess.run([str(binary), "start", hello[5:].hex()], cwd=ROOT,
                                    text=True, capture_output=True, timeout=30)
            assert result.returncode == 0 and not result.stderr, result
            lines = result.stdout.splitlines()
            assert len(lines) == 8, result
            incoming.write(bytes.fromhex(lines[0] + lines[1]))
            client.do_handshake()
            assert client.selected_alpn_protocol() == (protocols[0] if protocols else None)
            assert client.cipher()[0] == "TLS_AES_128_GCM_SHA256"
            client.write(b"client application bytes")
            response = outgoing.read()
            # TLS 1.3 compatibility CCS is plaintext and not in the transcript.
            if response[0] == 20:
                size = int.from_bytes(response[3:5], "big")
                assert response[5:5 + size] == b"\x01"
                response = response[5 + size:]
            verified = subprocess.run([str(binary), "verify", *lines[3:], response.hex()], cwd=ROOT,
                                      text=True, capture_output=True, timeout=30)
            assert verified.returncode == 0 and not verified.stderr and verified.stdout.strip() == "DONE", verified
            incoming.write(bytes.fromhex(lines[2]))
            assert client.read() == b"server application bytes"
    print("TLS server handshake verified: OpenSSL h2, HTTP/1.1 and no-ALPN; both application directions; invalid Finished rejection; zero leaks")


if __name__ == "__main__":
    main()
