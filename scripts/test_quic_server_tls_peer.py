#!/usr/bin/env python3
"""Verify Dark QUIC server TLS secrets and Finished with an independent aioquic client."""

import argparse
import datetime
import select
import subprocess
import tempfile
from pathlib import Path

from aioquic.buffer import Buffer, encode_uint_var
from aioquic.tls import Context, Direction, Epoch, State
from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives.kdf.hkdf import HKDFExpand
from cryptography.x509.oid import ExtendedKeyUsageOID, NameOID

ROOT = Path(__file__).resolve().parents[1]
DARK = '''// server-tls.dark - Live raw QUIC TLS flight and authenticated application-key release.
let arg (index: Int) : Blob = Stdlib.Cli.Args.get index |> Stdlib.Result.andThen Stdlib.Blob.fromHex |> Stdlib.Result.withDefault Stdlib.Blob.empty
let print (data: Blob) : Unit = Stdlib.printLine (Stdlib.Blob.toHex data)
let finish (handshake: Stdlib.__QuicServerTls.Handshake) : Stdlib.Result.Result<Unit, String> =
  Stdlib.Blob.fromHex (Builtin.stdinReadLine (())) |> Stdlib.Result.andThen (fun message ->
    Stdlib.__QuicServerTls.finish handshake message |> Stdlib.Result.map (fun ready ->
      print ready.send.key
      print ready.send.iv
      print ready.send.hp
      print ready.receive.key
      print ready.receive.iv
      print ready.receive.hp
      print ready.parameters.initialSource
      let changed = Stdlib.Blob.concat [Stdlib.Blob.slice message 0 35, Stdlib.Blob.__fromInt64List [Stdlib.Int64.bitwiseXor (Stdlib.Blob.__getByte message 35L) 1L]] in
      let rejects = match Stdlib.__QuicServerTls.finish handshake changed with | Error _ -> true | Ok _ -> false in
      Stdlib.printLine (if rejects then "DONE" else "FAILED")))
let run () : Stdlib.Result.Result<Unit, String> =
  Stdlib.Blob.fromHex "@CERT@" |> Stdlib.Result.andThen (fun certificate ->
    Stdlib.Blob.fromHex "@KEY@" |> Stdlib.Result.andThen (fun key ->
      Stdlib.TlsServerIdentity.create certificate key |> Stdlib.Result.andThen (fun identity ->
        Stdlib.__QuicServerTls.start identity (arg 0) (arg 1) (arg 2) |> Stdlib.Result.andThen (fun handshake ->
          let flight = handshake.flight in
          print flight.hello
          print flight.encrypted
          print flight.secrets.handshakeServer
          print flight.secrets.handshakeClient
          print flight.secrets.applicationServer
          print flight.secrets.applicationClient
          print flight.clientFinished
          finish handshake))))
match run () with | Ok () -> () | Error message -> Stdlib.printLine ("ERROR " ++ message)
'''


def parameter(kind, data):
    return encode_uint_var(kind) + encode_uint_var(len(data)) + data


def keys(secret):
    def expand(label, length):
        label = b"tls13 " + label
        info = length.to_bytes(2, "big") + bytes([len(label)]) + label + b"\x00"
        return HKDFExpand(algorithm=hashes.SHA256(), length=length, info=info).derive(secret)
    return [expand(b"quic key", 16), expand(b"quic iv", 12), expand(b"quic hp", 16)]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    now = datetime.datetime.now(datetime.timezone.utc)
    name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "localhost")])
    certificate = (x509.CertificateBuilder().subject_name(name).issuer_name(name)
                   .public_key(key.public_key()).serial_number(1)
                   .not_valid_before(now - datetime.timedelta(minutes=1)).not_valid_after(now + datetime.timedelta(days=1))
                   .add_extension(x509.BasicConstraints(ca=False, path_length=None), critical=True)
                   .add_extension(x509.SubjectAlternativeName([x509.DNSName("localhost")]), critical=False)
                   .add_extension(x509.ExtendedKeyUsage([ExtendedKeyUsageOID.SERVER_AUTH]), critical=False)
                   .sign(key, hashes.SHA256())).public_bytes(serialization.Encoding.PEM)
    pem = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
    source_id = b"clientID"
    client_parameters = parameter(15, source_id)
    server_parameters = parameter(0, b"original") + parameter(15, b"serverID")
    with tempfile.TemporaryDirectory(prefix="dark-quic-server-tls-") as temporary:
        source, binary = Path(temporary) / "server-tls.dark", Path(temporary) / "server-tls"
        source.write_text(DARK.replace("@CERT@", certificate.hex()).replace("@KEY@", pem.hex()))
        compiled = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        modes = ("valid", "no-alpn", "wrong-alpn", "no-parameters", "wrong-source", "server-only", "duplicate", "invalid-local")
        for mode in modes:
            protocols = [] if mode == "no-alpn" else ["h2"] if mode == "wrong-alpn" else ["h3"]
            client = Context(is_client=True, alpn_protocols=protocols, cadata=certificate, server_name="localhost")
            parameters = client_parameters
            if mode == "server-only":
                parameters += parameter(2, bytes(16))
            elif mode == "duplicate":
                parameters += client_parameters
            if mode != "no-parameters":
                client.handshake_extensions = [(57, parameters)]
            observed = {}
            client.update_traffic_key_cb = lambda direction, epoch, suite, secret: observed.__setitem__((direction, epoch), secret)
            output = {epoch: Buffer(capacity=65536) for epoch in Epoch}
            client.handle_message(b"", output)
            hello = output[Epoch.INITIAL].data
            arguments = [str(binary), hello.hex(), (b"" if mode == "invalid-local" else server_parameters).hex(),
                         (b"wrong" if mode == "wrong-source" else source_id).hex()]
            if mode != "valid":
                result = subprocess.run(arguments, input="", cwd=ROOT, text=True, capture_output=True, timeout=30)
                assert result.returncode == 0 and not result.stderr and result.stdout.startswith("ERROR "), (mode, result)
                assert len(result.stdout.splitlines()) == 1, (mode, result.stdout)
                continue
            process = subprocess.Popen(arguments, cwd=ROOT, text=True, stdin=subprocess.PIPE,
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            try:
                assert select.select([process.stdout], [], [], 15)[0], "No server flight"
                lines = [process.stdout.readline().strip() for _ in range(7)]
                assert all(lines) and not lines[0].startswith("ERROR"), lines
                hello, flight, hs_server, hs_client, app_server, app_client, expected = map(bytes.fromhex, lines)
                output = {epoch: Buffer(capacity=65536) for epoch in Epoch}
                # Feed fragments through aioquic's independent handshake decoder.
                wire = hello + flight
                for position in range(0, len(wire), 17):
                    client.handle_message(wire[position:position + 17], output)
                assert client.state == State.CLIENT_POST_HANDSHAKE and client.alpn_negotiated == "h3"
                assert client.received_extensions == [(57, server_parameters)]
                for epoch, server, client_secret in ((Epoch.HANDSHAKE, hs_server, hs_client), (Epoch.ONE_RTT, app_server, app_client)):
                    assert observed[(Direction.DECRYPT, epoch)] == server
                    assert observed[(Direction.ENCRYPT, epoch)] == client_secret
                finished = output[Epoch.HANDSHAKE].data
                assert finished == b"\x14\x00\x00\x20" + expected
                stdout, stderr = process.communicate(finished.hex() + "\n", timeout=30)
                assert process.returncode == 0 and not stderr, (stdout, stderr)
                ready = stdout.splitlines()
                assert ready[-1] == "DONE" and bytes.fromhex(ready[6]) == source_id
                assert [bytes.fromhex(line) for line in ready[:6]] == keys(app_server) + keys(app_client)
            finally:
                if process.poll() is None:
                    process.kill()
                    process.communicate()
    print("QUIC server TLS verified: aioquic certificate/Finished, both traffic secrets, packet keys, source binding, malformed parameters, ALPN and zero leaks")


if __name__ == "__main__":
    main()
