#!/usr/bin/env python3
"""Verify HTTP/3 headers delayed behind fragmented dynamic QPACK instructions."""

import argparse
import socket
import subprocess
import tempfile
import time
from pathlib import Path

from aioquic.h3.connection import H3Connection
from aioquic.h3.events import DataReceived, HeadersReceived
from aioquic.quic.configuration import QuicConfiguration
from aioquic.quic.connection import QuicConnection
from aioquic.quic.events import HandshakeCompleted
from cryptography.hazmat.primitives import serialization

from test_http3_server_peer import ROOT, SERVER, start, stop
from test_quic_tls_peer import certificates


class ObservedEncoder:
    def __init__(self, encoder):
        self.encoder = encoder
        self.pending = bytearray()
        self.acknowledged = set()

    def __getattr__(self, name):
        return getattr(self.encoder, name)

    def feed_decoder(self, data):
        self.encoder.feed_decoder(data)
        self.pending.extend(data)
        while self.pending:
            first = self.pending[0]
            mask = 127 if first & 128 else 63
            value, offset, shift = first & mask, 1, 0
            if value == mask:
                while True:
                    if offset == len(self.pending):
                        return
                    byte = self.pending[offset]
                    value += (byte & 127) << shift
                    offset += 1
                    if byte < 128:
                        break
                    shift += 7
            if first & 128:
                self.acknowledged.add(value)
            del self.pending[:offset]


class DelayedQpack(H3Connection):
    def __init__(self, connection):
        self.delayed = b""
        self.dynamic = False
        super().__init__(connection)
        self._encoder = ObservedEncoder(self._encoder)

    def _encode_headers(self, stream_id, headers):
        # Prime ls-qpack's reuse policy without putting the first section on
        # the wire, then encode the actual request using newly inserted fields.
        first, _block = self._encoder.encode(stream_id, headers)
        assert not first
        updates, block = self._encoder.encode(stream_id, headers)
        assert updates and block[0], (updates.hex(), block.hex())
        self.delayed = updates
        self.dynamic = True
        return block


def exchange(port, ca, body):
    configuration = QuicConfiguration(is_client=True, alpn_protocols=["h3"], server_name="localhost",
                                      cadata=ca.public_bytes(serialization.Encoding.PEM))
    client = QuicConnection(configuration=configuration)
    http = DelayedQpack(client)
    client.connect(("127.0.0.1", port), time.monotonic())
    sent, authenticated, finished, released = False, False, False, False
    sent_at, release_at, position = 0, 0, 0
    response, headers = bytearray(), []
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
        peer.bind(("127.0.0.1", 0))
        peer.settimeout(0.01)
        deadline = time.monotonic() + 15
        while time.monotonic() < deadline:
            now = time.monotonic()
            timer = client.get_timer()
            if timer is not None and timer <= now:
                client.handle_timer(now)
            if sent and now >= release_at and position < len(http.delayed):
                # Force stream-type and instruction varints / Huffman strings
                # across separate packet/read boundaries, not just one buffer.
                client.send_stream_data(http._local_encoder_stream_id, http.delayed[position:position + 1])
                position += 1
                release_at = now + 0.01
                released = position == len(http.delayed)
            for data, address in client.datagrams_to_send(now):
                peer.sendto(data, address)
            try:
                data, address = peer.recvfrom(8192)
                client.receive_datagram(data, address, now)
            except socket.timeout:
                pass
            while (event := client.next_event()) is not None:
                if isinstance(event, HandshakeCompleted):
                    authenticated = True
                for received in http.handle_event(event):
                    assert sent and released, "response dispatched before QPACK insertions arrived"
                    if isinstance(received, HeadersReceived):
                        headers.extend(received.headers)
                        finished |= received.stream_ended
                    elif isinstance(received, DataReceived):
                        response.extend(received.data)
                        finished |= received.stream_ended
            if authenticated and http.received_settings is not None and not sent:
                http.send_headers(0, [(b":method", b"POST"), (b":scheme", b"https"), (b":authority", b"localhost"),
                                      (b":path", b"/echo"), (b"content-length", str(len(body)).encode()),
                                      (b"x-dynamic", "café ☃".encode())], end_stream=not body)
                if body:
                    http.send_data(0, body, end_stream=True)
                sent, sent_at = True, now
                release_at = sent_at + 0.3
            # Do not accept just a completed response: the peer must also
            # process the server's decoder feedback for the dynamic request.
            if finished and 0 in http._encoder.acknowledged:
                assert http.dynamic and released and now - sent_at >= 0.3
                assert (b":status", b"200") in headers and response == body, (headers, len(response), len(body))
                return
        raise AssertionError(("dynamic exchange timed out", sent, released, position, len(http.delayed), headers, len(response), client._close_event))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    certificate = leaf.public_bytes(serialization.Encoding.PEM)
    private = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
    with tempfile.TemporaryDirectory(prefix="dark-http3-dynamic-") as temporary:
        source, binary = Path(temporary) / "server.dark", Path(temporary) / "server"
        source.write_text(SERVER.replace("@CERT@", certificate.hex()).replace("@KEY@", private.hex()))
        result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                cwd=ROOT, text=True, capture_output=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        process, port = start(binary)
        try:
            exchange(port, ca, b"dynamic")
            exchange(port, ca, bytes(range(251)) * 279)
            stop(process)
        except Exception:
            if process.poll() is None:
                process.terminate()
            stdout, stderr = process.communicate(timeout=3)
            print("Listener diagnostics:", process.returncode, stdout, stderr, flush=True)
            raise
        finally:
            if process.poll() is None:
                process.kill()
            process.communicate()
    print("HTTP/3 dynamic headers delayed behind bytewise encoder instructions, Unicode, 70 KiB bodies, feedback and owned cleanup verified")


if __name__ == "__main__":
    main()
