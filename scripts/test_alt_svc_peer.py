#!/usr/bin/env python3
"""Verify owned Alt-Svc sessions against independent TLS/h2 and aioquic peers."""

import argparse
import socket
import ssl
import subprocess
import tempfile
import threading
import time
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.h3.connection import H3Connection
from aioquic.h3.events import HeadersReceived
from aioquic.quic.configuration import QuicConfiguration
from aioquic.quic.connection import QuicConnection
from aioquic.quic.packet import pull_quic_header
from h2.config import H2Configuration
from h2.connection import H2Connection
from h2.events import RequestReceived

from test_http2_peer import certificates, compile_source

ROOT = Path(__file__).resolve().parents[1]
BODY = b"alternative" * 7000
DARK = '''// session.dark - Learn, use, invalidate and relearn an authenticated alternative.
let run () : Unit =
  match Stdlib.Cli.Args.get 0, Stdlib.Cli.Args.get 1 with
  | Ok port, Ok ca ->
    match Stdlib.Cli.FileSystem.readFile ca |> Stdlib.Result.mapError (fun _error -> "CA read failed") |> Stdlib.Result.andThen Stdlib.__Tls13Client.parsePemBundle with
    | Error message -> Stdlib.printLine message
    | Ok roots ->
      let session = Stdlib.HttpClientSession.createTrustedWithRoots roots in
      let url = "https://localhost:" ++ port ++ "/session" in
      let result = session.request "GET" url [] Stdlib.Blob.empty |> Stdlib.Result.andThen (fun first ->
        if first.statusCode != 200 || Stdlib.Blob.length first.body != 3 then Error Stdlib.HttpClient.RequestError.NetworkError
        else let _ = @CLEAR@ in session.request "GET" url [] Stdlib.Blob.empty |> Stdlib.Result.andThen (fun second ->
          if second.statusCode != @SECOND_STATUS@ || Stdlib.Blob.length second.body != @SECOND_SIZE@ then Error Stdlib.HttpClient.RequestError.NetworkError
          else session.request "GET" url [] Stdlib.Blob.empty |> Stdlib.Result.andThen (fun third ->
            if third.statusCode != 200 || Stdlib.Blob.length third.body != @THIRD_SIZE@ then Error Stdlib.HttpClient.RequestError.NetworkError
            else session.stream "GET" url [] Stdlib.Blob.empty |> Stdlib.Result.map (fun fourth ->
              let _ = session.close () in
              let bytes = Stdlib.Stream.toBlob fourth.body in
              let _ = Stdlib.Stream.close fourth.body in
              fourth.statusCode == 200 && Stdlib.Blob.length bytes == @FOURTH_SIZE@)))) in
      let _ = session.close () in
      let _ = session.close () in
      match result with | Ok true -> Stdlib.printLine "DONE" | _ -> Stdlib.printLine "FAILED"
  | _, _ -> Stdlib.printLine "Invalid arguments"
run ()
'''


def check(directory, compiler, mode):
    key, cert, ca = certificates(directory)
    expected = {
        "clear": (200, len(BODY), 3, len(BODY), "()", 2, 2),
        "misdirected": (421, len(BODY), 3, len(BODY), "()", 2, 2),
        "explicit-clear": (200, 3, len(BODY), 3, "session.clear ()", 3, 1),
        "expired": (200, 3, 3, 3, "()", 4, 0),
        "untrusted": (200, 3, 3, 3, "()", 4, 0),
    }[mode]
    program = DARK
    for marker, value in zip(("SECOND_STATUS", "SECOND_SIZE", "THIRD_SIZE", "FOURTH_SIZE", "CLEAR"), expected[:5]):
        program = program.replace(f"@{marker}@", str(value))
    binary = compile_source(directory, "session", program, compiler)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_3
    context.load_cert_chain(cert, key)
    context.set_alpn_protocols(["h2"])
    configuration = QuicConfiguration(is_client=False, alpn_protocols=["h3"])
    if mode == "untrusted":
        alternate = directory / "untrusted"
        alternate.mkdir()
        alternate_key, alternate_cert, _ = certificates(alternate)
        configuration.load_cert_chain(alternate_cert, alternate_key)
    else:
        configuration.load_cert_chain(cert, key)
    failures, tcp_requests, quic_requests, quic_attempts = [], [], [], []
    stopped = threading.Event()
    with socket.socket() as tcp, socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as udp:
        tcp.bind(("127.0.0.1", 0))
        tcp.listen()
        tcp.settimeout(0.1)
        udp.bind(("127.0.0.1", 0))
        udp.settimeout(0.01)
        origin_port, alternative_port = tcp.getsockname()[1], udp.getsockname()[1]

        def tls_peer():
            try:
                while not stopped.is_set():
                    try:
                        raw, _ = tcp.accept()
                    except socket.timeout:
                        continue
                    with context.wrap_socket(raw, server_side=True) as peer:
                        peer.settimeout(10)
                        assert peer.selected_alpn_protocol() == "h2"
                        connection = H2Connection(H2Configuration(client_side=False))
                        connection.initiate_connection()
                        peer.sendall(connection.data_to_send())
                        while not stopped.is_set():
                            data = peer.recv(65536)
                            if not data:
                                break
                            for event in connection.receive_data(data):
                                if isinstance(event, RequestReceived):
                                    assert (b":path", b"/session") in event.headers
                                    tcp_requests.append(event.headers)
                                    connection.send_headers(event.stream_id, [
                                        (b":status", b"200"), (b"content-length", b"3"),
                                        (b"alt-svc", f'h3=":{alternative_port}"; ma={0 if mode == "expired" else 60}'.encode())])
                                    connection.send_data(event.stream_id, b"tcp", end_stream=True)
                            peer.sendall(connection.data_to_send())
            except BaseException as error:
                failures.append(error)

        def quic_peer():
            connections = {}
            try:
                while not stopped.is_set():
                    now = time.monotonic()
                    try:
                        packet, address = udp.recvfrom(8192)
                    except socket.timeout:
                        packet = None
                    if packet is not None:
                        if address not in connections:
                            header = pull_quic_header(Buffer(data=packet), host_cid_length=8)
                            connection = QuicConnection(configuration=configuration,
                                original_destination_connection_id=header.destination_cid)
                            connections[address] = connection, H3Connection(connection)
                            quic_attempts.append(address)
                        connection, http = connections[address]
                        connection.receive_datagram(packet, address, now)
                        while (event := connection.next_event()) is not None:
                            for message in http.handle_event(event):
                                if isinstance(message, HeadersReceived):
                                    assert message.stream_ended and message.stream_id == 0
                                    assert (b":authority", f"localhost:{origin_port}".encode()) in message.headers
                                    assert (b":path", b"/session") in message.headers
                                    quic_requests.append(message.headers)
                                    misdirected = mode == "misdirected" and len(quic_requests) == 1
                                    http.send_headers(0, [(b":status", b"421" if misdirected else b"200"),
                                        (b"content-length", str(len(BODY)).encode()),
                                        (b"alt-svc", f'h3=":{alternative_port}"; ma=60'.encode() if misdirected else b"clear")])
                                    http.send_data(0, BODY, end_stream=True)
                    for connection, _http in connections.values():
                        timer = connection.get_timer()
                        if timer is not None and timer <= now:
                            connection.handle_timer(now)
                        for response, target in connection.datagrams_to_send(now):
                            udp.sendto(response, target)
            except BaseException as error:
                failures.append(error)

        threads = [threading.Thread(target=tls_peer), threading.Thread(target=quic_peer)]
        for thread in threads:
            thread.start()
        try:
            result = subprocess.run([str(binary), str(origin_port), str(ca)], cwd=ROOT,
                text=True, capture_output=True, timeout=30)
        finally:
            stopped.set()
            for thread in threads:
                thread.join(timeout=11)
        assert not any(thread.is_alive() for thread in threads) and not failures, failures
        assert result.returncode == 0 and not result.stderr and result.stdout.strip() == "DONE", result
        assert len(tcp_requests) == expected[5] and len(quic_requests) == expected[6], (mode, tcp_requests, quic_requests)
        if mode == "untrusted":
            assert len(quic_attempts) == 3, quic_attempts
        elif mode == "expired":
            assert not quic_attempts, quic_attempts


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    parser.add_argument("--mode", choices=("clear", "misdirected", "explicit-clear", "expired", "untrusted"))
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="dark-alt-svc-") as temporary:
        for mode in ([args.mode] if args.mode else ("clear", "misdirected", "explicit-clear", "expired", "untrusted")):
            check(Path(temporary), args.compiler, mode)
            print(f"Alt-Svc session verified: {mode}, authenticated negotiation and cleanup", flush=True)


if __name__ == "__main__":
    main()
