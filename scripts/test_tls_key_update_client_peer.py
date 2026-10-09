#!/usr/bin/env python3
"""Verify buffered/lazy Dark HTTP clients handle requested TLS updates after a 70 KiB upload."""

import argparse
import socket
import ssl
import subprocess
import tempfile
import threading
from pathlib import Path

from cryptography.hazmat.primitives import serialization
from h2.config import H2Configuration
from h2.connection import H2Connection
from h2.events import DataReceived, StreamEnded

from test_http2_peer import CLIENT
from test_quic_tls_peer import certificates
from test_tls_key_update_peer import decrypt, expand, read_record, seal

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    with tempfile.TemporaryDirectory(prefix="dark-tls-client-update-") as temporary:
        directory = Path(temporary)
        source, binary = directory / "client.dark", directory / "client"
        source.write_text(CLIENT)
        leaf_file, key_file, ca_file = directory / "leaf.pem", directory / "key.pem", directory / "ca.pem"
        leaf_file.write_bytes(leaf.public_bytes(serialization.Encoding.PEM))
        key_file.write_bytes(key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption()))
        ca_file.write_bytes(ca.public_bytes(serialization.Encoding.PEM))
        result = subprocess.run([str(args.compiler), str(source), "--leak-check", "-o", str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        for protocol in ("http/1.1", "h2"):
            for mode in ("buffered", "streaming"):
                failures, verified = [], []
                with socket.socket() as listener:
                    listener.bind(("127.0.0.1", 0))
                    listener.listen()
                    listener.settimeout(10)
                    port = listener.getsockname()[1]
                    keylog = directory / f"{protocol.replace('/', '-')}-{mode}.keys"

                    def server():
                        try:
                            context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
                            context.minimum_version = context.maximum_version = ssl.TLSVersion.TLSv1_3
                            context.load_cert_chain(leaf_file, key_file)
                            context.set_alpn_protocols([protocol])
                            context.num_tickets = 0
                            context.keylog_filename = str(keylog)
                            incoming, outgoing = ssl.MemoryBIO(), ssl.MemoryBIO()
                            tls = context.wrap_bio(incoming, outgoing, server_side=True)
                            peer, _ = listener.accept()
                            with peer:
                                peer.settimeout(10)
                                while True:
                                    try:
                                        tls.do_handshake()
                                        data = outgoing.read()
                                        if data:
                                            peer.sendall(data)
                                        break
                                    except ssl.SSLWantReadError:
                                        data = outgoing.read()
                                        if data:
                                            peer.sendall(data)
                                        # Feed one record at a time, preserving application
                                        # ciphertext for independent nonce/AEAD accounting.
                                        incoming.write(read_record(peer))
                                assert tls.selected_alpn_protocol() == protocol
                                suite = {"TLS_AES_128_GCM_SHA256": 4865, "TLS_AES_256_GCM_SHA384": 4866, "TLS_CHACHA20_POLY1305_SHA256": 4867}[tls.cipher()[0]]
                                secrets = {line.split()[0]: bytes.fromhex(line.split()[2]) for line in keylog.read_text().splitlines() if line and not line.startswith("#")}
                                receive, send = secrets["CLIENT_TRAFFIC_SECRET_0"], secrets["SERVER_TRAFFIC_SECRET_0"]
                                received_number, sent_number, body, request = 0, 0, b"", b""
                                h2 = H2Connection(config=H2Configuration(client_side=False))
                                if protocol == "h2":
                                    h2.initiate_connection()
                                    peer.sendall(seal(send, suite, sent_number, 23, h2.data_to_send()))
                                    sent_number += 1
                                complete = False
                                while not complete:
                                    kind, data = decrypt(receive, suite, received_number, read_record(peer))
                                    received_number += 1
                                    assert kind == 23
                                    if protocol == "h2":
                                        for event in h2.receive_data(data):
                                            if isinstance(event, DataReceived):
                                                body += event.data
                                                h2.acknowledge_received_data(event.flow_controlled_length, event.stream_id)
                                            if isinstance(event, StreamEnded):
                                                complete = True
                                        reply = h2.data_to_send()
                                        if reply:
                                            peer.sendall(seal(send, suite, sent_number, 23, reply))
                                            sent_number += 1
                                    else:
                                        request += data
                                        if b"\r\n\r\n" in request:
                                            _, body = request.split(b"\r\n\r\n", 1)
                                            complete = len(body) >= 70000
                                assert body == b"x" * 70000 and received_number > 30, (len(body), received_number)
                                peer.sendall(seal(send, suite, sent_number, 22, b"\x18\x00\x00\x01\x01"))
                                send = expand(send, suite, b"traffic upd")
                                if protocol == "h2":
                                    h2.send_headers(1, [(":status", "200"), ("content-length", "7"), ("set-cookie", "a=1"), ("set-cookie", "b=2")])
                                    h2.send_data(1, b"updated", end_stream=True)
                                    response = h2.data_to_send()
                                else:
                                    response = b"HTTP/1.1 200 OK\r\nContent-Length: 7\r\nConnection: close\r\n\r\nupdated"
                                peer.sendall(seal(send, suite, 0, 23, response))
                                # A requested reply uses the next old-client-key nonce,
                                # including every upload and HTTP/2 control record.
                                while True:
                                    kind, data = decrypt(receive, suite, received_number, read_record(peer))
                                    received_number += 1
                                    if kind == 22:
                                        assert data == b"\x18\x00\x00\x01\x00"
                                        verified.append((suite, received_number - 1))
                                        break
                                    assert protocol == "h2" and kind == 23
                                # Keep the authenticated connection open while the
                                # client sends final HTTP/2 receive-credit frames.
                                while peer.recv(4096):
                                    pass
                        except BaseException as error:
                            failures.append(error)

                    worker = threading.Thread(target=server, daemon=True)
                    worker.start()
                    result = subprocess.run([str(binary), str(port), str(ca_file), mode], cwd=ROOT, capture_output=True, text=True, timeout=20)
                    worker.join(timeout=5)
                    assert not worker.is_alive() and not failures and verified, (protocol, mode, failures, verified, result.stdout, result.stderr)
                    assert result.returncode == 0 and result.stdout.startswith("200|7|") and not result.stderr, (protocol, mode, result.returncode, result.stdout, result.stderr)
                    print(f"{protocol} {mode}: 70 KiB upload, requested update at client nonce {verified[0][1]}, authenticated response and zero leaks", flush=True)


if __name__ == "__main__":
    main()
