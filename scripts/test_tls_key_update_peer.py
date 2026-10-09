#!/usr/bin/env python3
"""Verify directional TLS KeyUpdate against independent AEAD/HKDF and live OpenSSL authentication."""

import argparse
import hashlib
import select
import socket
import ssl
import subprocess
import tempfile
from pathlib import Path

from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.ciphers.aead import AESGCM, ChaCha20Poly1305
from cryptography.hazmat.primitives.kdf.hkdf import HKDFExpand

from test_http2_server_tls_peer import SERVER, start, stop
from test_quic_tls_peer import certificates

ROOT = Path(__file__).resolve().parents[1]
PROBE = '''// probe.dark - Authenticated post-handshake traffic state and response nonce accounting.
type Report = { current: Stdlib.__Tls13KeyUpdate.Processed, replies: List<Blob> }
let arg (index: Int) : Blob = Stdlib.Cli.__Args.get index |> Stdlib.Result.andThen Stdlib.Blob.fromHex |> Stdlib.Result.withDefault Stdlib.Blob.empty
let run () : Stdlib.Result.Result<Unit, String> =
  let suite = Stdlib.Cli.__Args.get 0 |> Stdlib.Result.withDefault "4865" |> Stdlib.Int64.parse |> Stdlib.Result.withDefault 4865L in
  let traffic = if suite == 4866L then Stdlib.__Tls13.trafficKeys384 else if suite == 4867L then Stdlib.__Tls13.trafficKeysChaCha else Stdlib.__Tls13.trafficKeys in
  match traffic (arg 2), traffic (arg 3) with
  | Ok receive, Ok send ->
    let fromServer = (Stdlib.Cli.__Args.get 1 |> Stdlib.Result.withDefault "server") == "client" in
    let state = Stdlib.__Tls13KeyUpdate.create (arg 2) (arg 3) { send with sequence = 3L } fromServer in
    let initial = Report { current = Stdlib.__Tls13KeyUpdate.Processed { state = state, receive = receive, data = None, closed = false, reply = None }, replies = [] } in
    Stdlib.List.fold [arg 4, arg 5, arg 6] (Ok initial) (fun result bytes ->
      if Stdlib.Blob.__byteLength bytes == 0L then result
      else result |> Stdlib.Result.andThen (fun report ->
        Stdlib.__Tls13.parseRecord bytes |> Stdlib.Result.mapError (fun _error -> "record") |> Stdlib.Result.andThen (fun record ->
          Stdlib.__Tls13KeyUpdate.process report.current.state report.current.receive record |> Stdlib.Result.map (fun current ->
            let replies = match current.reply with | None -> report.replies | Some bytes -> Stdlib.List.append report.replies [bytes] in
            Report { current = current, replies = replies })))) |> Stdlib.Result.map (fun report ->
      let state = report.current.state in
      Stdlib.printLine (Stdlib.Blob.toHex state.receiveSecret)
      Stdlib.printLine (Stdlib.Blob.toHex state.sendSecret)
      Stdlib.printLine (Stdlib.Int64.toString report.current.receive.sequence)
      Stdlib.printLine (Stdlib.Int64.toString state.send.sequence)
      Stdlib.printLine (Stdlib.Int64.toString state.updates)
      Stdlib.printLine (Stdlib.Int64.toString state.tickets)
      let _ = Stdlib.List.fold report.replies () (fun _unit bytes -> Stdlib.printLine ("REPLY " ++ Stdlib.Blob.toHex bytes)) in
      Stdlib.printLine (Stdlib.Blob.toHex (Stdlib.Option.withDefault report.current.data Stdlib.Blob.empty)))
  | _, _ -> Error "keys"
match run () with | Ok () -> () | Error _ -> Stdlib.printLine "ERROR"
'''
FAULT = '''// fault.dark - Failed server replies and client EOF cannot reuse an application send nonce.
let arg (index: Int) : Blob = Stdlib.Cli.__Args.get index |> Stdlib.Result.andThen Stdlib.Blob.fromHex |> Stdlib.Result.withDefault Stdlib.Blob.empty
let write (lifecycle: Stream<Unit>) (cell: RawPtr) (fail: Bool) (bytes: Blob) : Stdlib.Result.Result<Unit, Int64> =
  match Stdlib.Stream.next lifecycle with
  | None -> Error 9L
  | Some () ->
    let count = Stdlib.Stream.__cellGet<Int64> cell in
    let _ = Stdlib.Stream.__cellSet cell (count + 1L) in
    Stdlib.printLine ("WRITE " ++ Stdlib.Blob.toHex bytes)
    if fail && count == 0L then Error 5L else Ok ()
let run () : Unit =
  let fail = (Stdlib.Cli.__Args.get 0 |> Stdlib.Result.withDefault "") == "serverfail" in
  match Stdlib.__Tls13.trafficKeys (arg 1), Stdlib.__Tls13.trafficKeys (arg 2), Stdlib.__Network.shutdownSignals () with
  | Ok receive, Ok send, Ok signals ->
    let counter = Stdlib.Stream.__cellNew 0L in
    let lifecycle = Stdlib.Stream.__new (fun _unit -> Some ()) (fun _unit -> Stdlib.Stream.__cellDispose<Int64> counter) in
    let connection = Stdlib.__Network.TcpConnection { lifecycle = lifecycle, watch = None, read = fun _size -> Error 11L, write = fun bytes -> write lifecycle counter fail bytes } in
    let send = { send with sequence = 3L } in
    let transport =
      if fail then Stdlib.__Tls13ServerTcp.transport connection signals None (Stdlib.__Tls13ServerTcp.Accepted { pending = arg 3, ready = Stdlib.__Tls13ServerHandshake.Ready { receive = receive, send = send, protocol = "http/1.1", receiveSecret = arg 1, sendSecret = arg 2 } })
      else Stdlib.__HttpTransport.secure connection (Stdlib.__Tls13Client.SecureKeys { client = send, server = receive, clientSecret = arg 2, serverSecret = arg 1, pending = arg 3 }) in
    let read = transport.read () in
    Stdlib.printLine (match read with | Error _ -> "READ ERROR" | Ok bytes -> if Stdlib.Blob.__byteLength bytes == 0L then "READ EMPTY" else "READ DATA")
    Stdlib.__HttpTransport.close transport
    Stdlib.__HttpTransport.close transport
    Stdlib.printLine (match transport.write (Stdlib.String.toBlob "unused") with | Error _ -> "WRITE CLOSED" | Ok () -> "WRITE OPEN")
    Stdlib.__Network.closeShutdownSignals signals
  | _, _, _ -> Stdlib.printLine "initialization failed"
run ()
'''


def expand(secret, suite, label, length=None):
    length = len(secret) if length is None else length
    label = b"tls13 " + label
    info = length.to_bytes(2, "big") + bytes([len(label)]) + label + b"\x00"
    return HKDFExpand(algorithm=hashes.SHA384() if suite == 4866 else hashes.SHA256(), length=length, info=info).derive(secret)


def material(secret, suite):
    key = expand(secret, suite, b"key", 16 if suite == 4865 else 32)
    iv = expand(secret, suite, b"iv", 12)
    return (ChaCha20Poly1305(key) if suite == 4867 else AESGCM(key)), iv


def nonce(iv, number):
    return bytes(a ^ b for a, b in zip(iv, number.to_bytes(12, "big")))


def seal(secret, suite, number, kind, data):
    cipher, iv = material(secret, suite)
    plain = data + bytes([kind])
    header = b"\x17\x03\x03" + (len(plain) + 16).to_bytes(2, "big")
    return header + cipher.encrypt(nonce(iv, number), plain, header)


def decrypt(secret, suite, number, record):
    cipher, iv = material(secret, suite)
    plain = cipher.decrypt(nonce(iv, number), record[5:], record[:5]).rstrip(b"\x00")
    return plain[-1], plain[:-1]


def read_record(peer):
    def read(size):
        data = b""
        while len(data) < size:
            chunk = peer.recv(size - len(data))
            assert chunk, "unexpected EOF"
            data += chunk
        return data
    header = read(5)
    return header + read(int.from_bytes(header[3:5], "big"))


def authenticated_peer(port, trust, keylog):
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
    context.minimum_version = context.maximum_version = ssl.TLSVersion.TLSv1_3
    context.load_verify_locations(cadata=trust.decode())
    context.set_alpn_protocols(["http/1.1"])
    context.keylog_filename = str(keylog)
    incoming, outgoing = ssl.MemoryBIO(), ssl.MemoryBIO()
    client = context.wrap_bio(incoming, outgoing, server_side=False, server_hostname="localhost")
    peer = socket.create_connection(("127.0.0.1", port), timeout=5)
    try:
        while True:
            try:
                client.do_handshake()
                peer.sendall(outgoing.read())
                break
            except ssl.SSLWantReadError:
                peer.sendall(outgoing.read())
                incoming.write(peer.recv(32768))
        assert client.selected_alpn_protocol() == "http/1.1"
        secrets = {line.split()[0]: bytes.fromhex(line.split()[2]) for line in keylog.read_text().splitlines() if line and not line.startswith("#")}
        return peer, secrets["CLIENT_TRAFFIC_SECRET_0"], secrets["SERVER_TRAFFIC_SECRET_0"]
    except BaseException:
        peer.close()
        raise


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    with tempfile.TemporaryDirectory(prefix="dark-tls-update-") as temporary:
        directory = Path(temporary)
        source, binary = directory / "probe.dark", directory / "probe"
        source.write_text(PROBE)
        result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        count = 0
        for suite in (4865, 4866, 4867):
            size = 48 if suite == 4866 else 32
            receive, send = bytes(range(size)), bytes(range(101, 101 + size))
            updated = expand(receive, suite, b"traffic upd")
            for request in (0, 1):
                for split in (False, True):
                    message = bytes([24, 0, 0, 1, request])
                    first = seal(receive, suite, 0, 22, message[:2] if split else message)
                    second = seal(receive, suite, 1, 22, message[2:]) if split else b""
                    third = seal(updated, suite, 0, 23, b"updated")
                    result = subprocess.run([str(binary), str(suite), "server", receive.hex(), send.hex(), first.hex(), second.hex(), third.hex()], cwd=ROOT, capture_output=True, text=True, timeout=5)
                    assert result.returncode == 0 and not result.stderr, (suite, result.stdout, result.stderr)
                    lines = result.stdout.splitlines()
                    assert bytes.fromhex(lines[0]) == updated
                    assert bytes.fromhex(lines[1]) == (expand(send, suite, b"traffic upd") if request else send)
                    assert lines[2:6] == ["1", "0" if request else "3", "1", "0"], lines
                    if request:
                        assert decrypt(send, suite, 3, bytes.fromhex(lines[6][6:])) == (22, b"\x18\x00\x00\x01\x00")
                    else:
                        assert len(lines) == 7, lines
                    assert bytes.fromhex(lines[-1]) == b"updated"
                    count += 1
            for malformed in (b"\x18\x00\x00\x01\x02", b"\x18\x00\x00\x02\x00\x00", b"\x18\x00\x00\x01\x00\x04\x00\x00\x00", b"\x63\x00\x00\x00", b"\x04\x00\x00\x00"):
                result = subprocess.run([str(binary), str(suite), "server", receive.hex(), send.hex(), seal(receive, suite, 0, 22, malformed).hex(), "", ""], cwd=ROOT, capture_output=True, text=True, timeout=5)
                assert result.returncode == 0 and result.stdout == "ERROR\n" and not result.stderr, (suite, malformed, result.stdout, result.stderr)
                count += 1
        print(f"TLS update AEAD/HKDF oracle: {count} cases across AES128/SHA256, AES256/SHA384 and ChaCha20; zero leaks", flush=True)
        source, fault_binary = directory / "fault.dark", directory / "fault"
        source.write_text(FAULT)
        result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(fault_binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        receive, send = bytes(range(32)), bytes(range(101, 133))
        for mode in ("serverfail", "clienteof"):
            records = seal(receive, 4865, 0, 22, b"\x18\x00\x00\x01\x01")
            if mode == "clienteof":
                records += seal(expand(receive, 4865, b"traffic upd"), 4865, 0, 21, b"\x01\x00")
            result = subprocess.run([str(fault_binary), mode, receive.hex(), send.hex(), records.hex()], cwd=ROOT, capture_output=True, text=True, timeout=5)
            assert result.returncode == 0 and not result.stderr, (mode, result.stdout, result.stderr)
            lines = result.stdout.splitlines()
            assert decrypt(send, 4865, 3, bytes.fromhex(lines[0][6:])) == (22, b"\x18\x00\x00\x01\x00"), lines
            if mode == "serverfail":
                assert lines[1] == "READ ERROR" and lines[3] == "WRITE CLOSED", lines
                assert decrypt(expand(send, 4865, b"traffic upd"), 4865, 0, bytes.fromhex(lines[2][6:])) == (21, b"\x01\x00"), lines
            else:
                assert lines[1:] == ["READ EMPTY", "WRITE CLOSED"], lines
        print("TLS ownership faults: reply-send failure closes with fresh-key nonce; EOF aborts later writes; double close and zero leaks", flush=True)
        source, binary = directory / "server.dark", directory / "server"
        source.write_text(SERVER.replace("@CERT@", leaf.public_bytes(serialization.Encoding.PEM).hex()).replace("@KEY@", key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption()).hex()))
        result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        process, port = start(binary)
        try:
            for mode in ("requested", "fragmented", "unrequested-then-requested"):
                keylog = directory / f"{mode}.keys"
                peer, receive, send = authenticated_peer(port, ca.public_bytes(serialization.Encoding.PEM), keylog)
                with peer:
                    if mode == "unrequested-then-requested":
                        peer.sendall(seal(receive, 4865, 0, 22, b"\x18\x00\x00\x01\x00"))
                        receive = expand(receive, 4865, b"traffic upd")
                    update = b"\x18\x00\x00\x01\x01"
                    if mode == "fragmented":
                        peer.sendall(seal(receive, 4865, 0, 22, update[:2]) + seal(receive, 4865, 1, 22, update[2:]))
                    else:
                        peer.sendall(seal(receive, 4865, 0, 22, update))
                    receive = expand(receive, 4865, b"traffic upd")
                    assert decrypt(send, 4865, 0, read_record(peer)) == (22, b"\x18\x00\x00\x01\x00")
                    send = expand(send, 4865, b"traffic upd")
                    request = f"POST /echo HTTP/1.1\r\nHost: localhost:{port}\r\nContent-Length: 7\r\n\r\nupdated".encode()
                    peer.sendall(seal(receive, 4865, 0, 23, request))
                    response, number = b"", 0
                    while True:
                        kind, data = decrypt(send, 4865, number, read_record(peer))
                        number += 1
                        if kind == 21:
                            assert data == b"\x01\x00"
                            break
                        assert kind == 23
                        response += data
                    assert response.startswith(b"HTTP/1.1 200") and response.endswith(b"updated"), response
                    print(f"Live TLS {mode}: OpenSSL certificate/Finished, directional updates, application response and close_notify", flush=True)
            stop(process)
            print("Key-update listener shutdown: zero leaks", flush=True)
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()


if __name__ == "__main__":
    main()
