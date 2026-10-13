#!/usr/bin/env python3
"""Verify public Dark HTTP/3 routing, body limits, Retry replay suppression and shutdown."""

import argparse
import select
import signal
import socket
import subprocess
import tempfile
import time
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.h3.connection import H3Connection, Setting
from aioquic.h3.events import DataReceived, HeadersReceived
from aioquic.quic.configuration import QuicConfiguration
from aioquic.quic.connection import QuicConnection
from aioquic.quic.events import ConnectionTerminated, HandshakeCompleted
from aioquic.quic.packet import QuicPacketType, pull_quic_header
from cryptography.hazmat.primitives import serialization

from test_quic_tls_peer import certificates

ROOT = Path(__file__).resolve().parents[1]
SERVER = '''// server.dark - Public HTTP/3 shared HTTP routing and owned shutdown.
let handler (request: Stdlib.Http.Request) : Stdlib.Http.Response =
  if Stdlib.Bool.not (Stdlib.String.startsWith request.url "https://localhost") then Stdlib.Http.responseWithText "wrong scheme" 500
  else if Stdlib.HttpServer.getMethod request == "HEAD" then Stdlib.Http.responseWithText "representation" 200
  else Stdlib.Http.Response { statusCode = 200, headers = [("set-cookie", "a=1"), ("set-cookie", "b=2")], body = request.body }
let run () : Unit =
  let port = Stdlib.Cli.__Args.get 0 |> Stdlib.Result.withDefault "0" |> Stdlib.Int.parse |> Stdlib.Result.withDefault 0 in
  let identity = Stdlib.Blob.fromHex "@CERT@" |> Stdlib.Result.andThen (fun certificate -> Stdlib.Blob.fromHex "@KEY@" |> Stdlib.Result.andThen (fun key -> Stdlib.TlsServerIdentity.create certificate key)) in
  match identity with
  | Error message -> Stdlib.printLine message
  | Ok identity ->
    let config = Stdlib.HttpServer.Config.Config { port = port, maxBodyBytes = 80000, injectStandardHeaders = false, canonicalizeFromForwardedProto = false, logRequests = false } in
    match Stdlib.HttpServer.Quic.serve config identity handler (fun _unit -> Stdlib.printLine "LISTENING") with
    | Error message -> Stdlib.printLine message
    | Ok () -> Stdlib.printLine "STOPPED"
run ()
'''


def start(binary, port=None, host="127.0.0.1"):
    if port is None:
        with socket.socket(socket.AF_INET6 if ":" in host else socket.AF_INET, socket.SOCK_DGRAM) as reservation:
            reservation.bind((host, 0))
            port = reservation.getsockname()[1]
    process = subprocess.Popen([str(binary), str(port)], cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    assert select.select([process.stdout], [], [], 5)[0], "listener did not start"
    assert process.stdout.readline() == b"LISTENING\n"
    return process, port


def stop(process):
    process.send_signal(signal.SIGTERM)
    stdout, stderr = process.communicate(timeout=3)
    assert process.returncode == 0 and stdout == b"STOPPED\n" and not stderr, (process.returncode, stdout, stderr)


def exchange(peer, port, ca, method=b"POST", body=b"HTTP/3 routing", declared=None, stalled=False, protocols=None, host="127.0.0.1", field_limit=None, idle_timeout=60, handshake_only=False, server_uni_credit=None):
    config = QuicConfiguration(is_client=True, alpn_protocols=["h3"] if protocols is None else protocols,
                              server_name="localhost", cadata=ca.public_bytes(serialization.Encoding.PEM))
    class AdvertisedIdleQuic(QuicConnection):
        def _serialize_transport_parameters(self):
            # Advertise a short peer limit while retaining the harness's
            # longer local timer through the expensive certificate flight.
            # The idle test deliberately sends after this advertised limit
            # to check the server, without its own timer closing first.
            previous = self._configuration.idle_timeout
            self._configuration.idle_timeout = idle_timeout
            try:
                return super()._serialize_transport_parameters()
            finally:
                self._configuration.idle_timeout = previous

    client = AdvertisedIdleQuic(configuration=config)
    if server_uni_credit is not None:
        client._local_max_stream_data_uni = server_uni_credit
    class MethodAwareH3(H3Connection):
        def _get_local_settings(self):
            settings = super()._get_local_settings()
            if field_limit is not None:
                settings[Setting.MAX_FIELD_SECTION_SIZE] = field_limit
            return settings

        def _check_content_length(self, stream):
            # aioquic's H3 layer does not retain the request method. HEAD's
            # Content-Length describes the representation, not DATA bytes.
            # The assertions below separately reject any HEAD response body.
            if method != b"HEAD":
                super()._check_content_length(stream)

    http = MethodAwareH3(client)
    client.connect((host, port), time.monotonic())
    response, headers, finished, sent, replay, retry = bytearray(), [], False, False, None, False
    close = None
    deadline = time.monotonic() + (2 if field_limit == 0 else 1 if protocols is not None else 12)
    while time.monotonic() < deadline:
        now = time.monotonic()
        timer = client.get_timer()
        if timer is not None and timer <= now:
            client.handle_timer(now)
        for data, address in client.datagrams_to_send(now):
            header = pull_quic_header(Buffer(data=data), host_cid_length=8)
            if header.packet_type == QuicPacketType.INITIAL and header.token:
                replay = data
            peer.sendto(data, address)
        try:
            data, address = peer.recvfrom(8192)
        except socket.timeout:
            data = None
        if data is not None:
            header = pull_quic_header(Buffer(data=data), host_cid_length=8)
            retry |= header.packet_type == QuicPacketType.RETRY
            client.receive_datagram(data, address, now)
        while (event := client.next_event()) is not None:
            if isinstance(event, ConnectionTerminated):
                close = event
            if isinstance(event, HandshakeCompleted) and not sent:
                assert event.alpn_protocol == "h3"
                if handshake_only:
                    # Finished is queued locally but has not been emitted.
                    return client, http, replay
                fields = [(b":method", method), (b":scheme", b"https"), (b":authority", b"localhost"), (b":path", b"/echo"),
                          (b"content-length", str(len(body) if declared is None else declared).encode())]
                end_stream = not stalled and (declared is None or declared == len(body))
                http.send_headers(0, fields, end_stream=not body and end_stream)
                if body:
                    http.send_data(0, body, end_stream=end_stream)
                sent = True
                if stalled:
                    for packet, target in client.datagrams_to_send(time.monotonic()):
                        peer.sendto(packet, target)
                    return client, http, replay
            for received in http.handle_event(event):
                if isinstance(received, HeadersReceived):
                    headers.extend(received.headers)
                    finished |= received.stream_ended
                elif isinstance(received, DataReceived):
                    response.extend(received.data)
                    finished |= received.stream_ended
        # The authenticated close is recorded on receipt; aioquic publishes
        # ConnectionTerminated only after its draining timer (three PTOs).
        # Check the received close without making RTT-dependent timer latency
        # part of this interoperability gate.
        if close is None:
            close = client._close_event
        if finished and close is None:
            # Persistent servers retain the connection after a response. The
            # harness explicitly closes its owner before the next connection.
            assert retry and replay is not None
            client.close(error_code=256)
            for packet, target in client.datagrams_to_send(time.monotonic()):
                peer.sendto(packet, target)
            return headers, bytes(response), replay
        if close:
            if field_limit == 0:
                assert sent and not headers and not response, (sent, headers, response)
                return None
            assert finished and close.error_code == 256, (finished, close, headers)
            assert retry and replay is not None
            return headers, bytes(response), replay
    if protocols is not None:
        assert not sent and not headers, "unsupported ALPN was accepted"
        return None
    if field_limit == 0:
        assert sent and not headers and not response, (sent, headers, response)
        return None
    raise AssertionError(("exchange timed out", sent, finished, len(response), headers, close))


def server_owners(peer, port, ca):
    config = QuicConfiguration(is_client=True, alpn_protocols=["h3"], server_name="localhost",
                               cadata=ca.public_bytes(serialization.Encoding.PEM))
    client = QuicConnection(configuration=config)
    # Test-only independent transport control: no response stream has credit
    # until we explicitly grant it; critical unidirectional streams retain credit.
    client._local_max_stream_data_bidi_local = 0
    http = H3Connection(client)
    client.connect(("127.0.0.1", port), time.monotonic())
    headers, bodies, ended = {}, {}, set()
    authenticated = False

    def request(stream, body, declared=None, finished=True):
        bodies[stream] = bytearray()
        http.send_headers(stream, [(b":method", b"POST"), (b":scheme", b"https"),
                                   (b":authority", b"localhost"), (b":path", b"/echo"),
                                   (b"x-shared", b"persistent-request-compression"),
                                   (b"content-length", str(len(body) if declared is None else declared).encode())],
                          end_stream=not body and finished)
        if body:
            http.send_data(stream, body, end_stream=finished)

    def pump():
        nonlocal authenticated
        now = time.monotonic()
        timer = client.get_timer()
        if timer is not None and timer <= now:
            client.handle_timer(now)
        for data, target in client.datagrams_to_send(now):
            peer.sendto(data, target)
        try:
            data, source = peer.recvfrom(8192)
            client.receive_datagram(data, source, time.monotonic())
        except socket.timeout:
            pass
        while (event := client.next_event()) is not None:
            assert not isinstance(event, ConnectionTerminated), event
            if isinstance(event, HandshakeCompleted):
                authenticated = True
            for received in http.handle_event(event):
                if isinstance(received, HeadersReceived):
                    headers[received.stream_id] = received.headers
                elif isinstance(received, DataReceived):
                    bodies[received.stream_id].extend(received.data)
                if received.stream_ended:
                    ended.add(received.stream_id)
        assert client._close_event is None, client._close_event

    def until(predicate):
        deadline = time.monotonic() + 8
        while not predicate():
            assert time.monotonic() < deadline, ("HTTP/3 owners stalled", list(headers), ended)
            pump()

    until(lambda: authenticated)
    request(0, b"a", declared=2, finished=False)
    body = b"b" * 70000
    request(4, body)
    # Only stream 4 receives response credit; stream 0's upload is unfinished.
    client._streams[4].max_stream_data_local = 100000
    until(lambda: 4 in ended)
    assert bodies[4] == body and not bodies[0] and 0 not in ended
    assert [value for name, value in headers[4] if name == b"set-cookie"] == [b"a=1", b"b=2"]
    client.reset_stream(0, error_code=268)
    client.stop_stream(0, error_code=268)
    # Reuse past the initial 32-stream allowance: completed owners must renew
    # MAX_STREAMS while retaining the connection's compression state.
    for index in range(40):
        stream = 8 + index * 4
        value = f"request-{index}".encode()
        request(stream, value)
        client._streams[stream].max_stream_data_local = 1024
        until(lambda: stream in ended)
        assert bodies[stream] == value and (b":status", b"200") in headers[stream]
    client.close(error_code=256)
    for data, target in client.datagrams_to_send(time.monotonic()):
        peer.sendto(data, target)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    with tempfile.TemporaryDirectory(prefix="dark-http3-server-") as temporary:
        source, binary = Path(temporary) / "server.dark", Path(temporary) / "server"
        pem = leaf.public_bytes(serialization.Encoding.PEM)
        private = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
        source.write_text(SERVER.replace("@CERT@", pem.hex()).replace("@KEY@", private.hex()))
        result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        process, port = start(binary)
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
                peer.bind(("127.0.0.1", 0))
                peer.settimeout(0.01)
                server_owners(peer, port, ca)
            print("HTTP/3 stream isolation, selective flow credit, reset and 40 persistent requests verified", flush=True)
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
                peer.bind(("127.0.0.1", 0))
                peer.settimeout(0.01)
                client, http, _replay = exchange(peer, port, ca, body=b"partial", declared=100, stalled=True, idle_timeout=0.5)
                # Let the handshake/control ACKs settle, then deliberately
                # stop driving the peer's own timers. Completing the upload
                # after silence distinguishes the server idle timer from
                # its longer HTTP exchange deadline.
                until = time.monotonic() + 0.3
                while time.monotonic() < until:
                    try:
                        data, address = peer.recvfrom(8192)
                        client.receive_datagram(data, address, time.monotonic())
                    except socket.timeout:
                        pass
                    while (event := client.next_event()) is not None:
                        http.handle_event(event)
                    for data, address in client.datagrams_to_send(time.monotonic()):
                        peer.sendto(data, address)
                # Allow the initial three-PTO floor as well, even if control
                # ACK timing has not supplied an application RTT sample.
                until = time.monotonic() + 4
                while time.monotonic() < until:
                    try:
                        peer.recvfrom(8192)
                    except socket.timeout:
                        pass
                http.send_data(0, b"x" * 93, end_stream=True)
                for data, address in client.datagrams_to_send(time.monotonic()):
                    peer.sendto(data, address)
                until = time.monotonic() + 1
                while time.monotonic() < until:
                    try:
                        data, address = peer.recvfrom(8192)
                    except socket.timeout:
                        continue
                    client.receive_datagram(data, address, time.monotonic())
                    while (event := client.next_event()) is not None:
                        assert not http.handle_event(event), "server accepted upload after negotiated idle expiration"
            print("Negotiated idle timeout discards stalled upload before the HTTP deadline", flush=True)
            # Every nonempty field section exceeds a decoded limit of zero,
            # including indexed :status. The next ordinary exchange also
            # checks that rejecting this response leaves the listener usable.
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
                peer.bind(("127.0.0.1", 0))
                peer.settimeout(0.01)
                exchange(peer, port, ca, body=b"", field_limit=0)
            print("Peer SETTINGS_MAX_FIELD_SECTION_SIZE=0 prevents response HEADERS", flush=True)
            for method, body, declared, status in ((b"POST", bytes(range(251)) * 279, None, b"200"),
                                                   (b"HEAD", b"", None, b"200"),
                                                   (b"POST", b"", 80001, b"413")):
                with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
                    peer.bind(("127.0.0.1", 0))
                    peer.settimeout(0.01)
                    headers, response, replay = exchange(peer, port, ca, method, body, declared)
                    assert (b":status", status) in headers, headers
                    if method == b"HEAD":
                        assert not response and (b"content-length", b"14") in headers, (headers, response)
                    elif status == b"200":
                        assert response == body and (b"set-cookie", b"a=1") in headers and (b"set-cookie", b"b=2") in headers
                    else:
                        assert response == b"Request body too large"
                    # Reusing a consumed address-valid token must not trigger
                    # another certificate flight or Initial-key nonce reuse.
                    peer.sendto(replay, ("127.0.0.1", port))
                    until = time.monotonic() + 0.3
                    while time.monotonic() < until:
                        try:
                            data, _ = peer.recvfrom(8192)
                        except socket.timeout:
                            continue
                        kind = pull_quic_header(Buffer(data=data), host_cid_length=8).packet_type
                        assert kind not in (QuicPacketType.INITIAL, QuicPacketType.HANDSHAKE, QuicPacketType.RETRY), kind
                    print(f"{method.decode()} {status.decode()}: {len(body)} bytes, routing/body limits, explicit encrypted owner close and replay suppression", flush=True)
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
                peer.bind(("127.0.0.1", 0))
                peer.settimeout(0.01)
                exchange(peer, port, ca, protocols=["h2"])
            stop(process)
            print("Unsupported ALPN leaves listener live; idle shutdown and zero leaks", flush=True)
            process, _ = start(binary, port)
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
                peer.bind(("127.0.0.1", 0))
                peer.settimeout(0.01)
                exchange(peer, port, ca, body=b"partial", declared=100, stalled=True)
                stop(process)
            print("Same-port rebind and stalled HTTP/3 body shutdown; zero leaks", flush=True)
        finally:
            if process.poll() is None:
                process.kill()
                stdout, stderr = process.communicate()
                if stdout or stderr:
                    print("Listener diagnostics:", stdout, stderr, flush=True)
            elif process.returncode:
                stdout, stderr = process.communicate()
                print("Listener diagnostics:", process.returncode, stdout, stderr, flush=True)


if __name__ == "__main__":
    main()
