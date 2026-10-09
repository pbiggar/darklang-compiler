#!/usr/bin/env python3
"""Check the production Dark QUIC client owner against a lossy test-only aioquic peer."""

import socket
import subprocess
import tempfile
import threading
import time
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.quic.configuration import QuicConfiguration
from aioquic.quic.connection import QuicConnection
from aioquic.quic.events import HandshakeCompleted
from aioquic.quic.packet import encode_quic_retry, pull_quic_header
from cryptography.hazmat.primitives import serialization

from test_quic_tls_peer import certificates

ROOT = Path(__file__).resolve().parents[1]
DARK = """// client.dark - Exercise the production QUIC client handshake and owned socket cleanup.
let run () : Unit =
  match Stdlib.Blob.fromHex "@ROOT@", Stdlib.Cli.Args.get 0,
    Stdlib.Cli.Args.get 1 |> Stdlib.Result.andThen (fun text -> Stdlib.Int64.parse text |> Stdlib.Result.mapError (fun _error -> "port")) with
  | Ok root, Ok mode, Ok port ->
    let peer = Stdlib.__Datagram.Endpoint { address = [127L,0L,0L,1L], port = port } in
    match Stdlib.__QuicClient.connect peer (if mode == "hostname" then "wrong.example.com" else "localhost")
      (if mode == "untrusted" then [] else [root]) 8000L with
    | Error message -> Stdlib.printLine ("ERROR " ++ message)
    | Ok ready -> let _ = Stdlib.__QuicClient.close ready in Stdlib.printLine "AUTHENTICATED"
  | _ -> Stdlib.printLine "Bad arguments"
run ()
"""


def main():
    key, leaf, ca = certificates()
    config = QuicConfiguration(is_client=False, alpn_protocols=["h3"])
    config.certificate, config.private_key = leaf, key
    modes = ("trusted", "drop-initial", "drop-flight", "duplicate", "corrupt", "wrong-address", "retry", "untrusted", "hostname")
    with tempfile.TemporaryDirectory(prefix="dark-quic-client-") as temporary:
        source, binary = Path(temporary) / "client.dark", Path(temporary) / "client"
        source.write_text(DARK.replace("@ROOT@", ca.public_bytes(serialization.Encoding.DER).hex()))
        result = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)], cwd=ROOT,
                                capture_output=True, text=True, timeout=120)
        assert result.returncode == 0, result.stdout + result.stderr
        for mode in modes:
            failures, completed, stats = [], [], {"packets": 0, "numbers": []}
            stopped = threading.Event()
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as listener:
                listener.bind(("127.0.0.1", 0))
                listener.settimeout(0.02)

                def peer():
                    connection, original, retry_cid = None, None, b"retryCID"
                    dropped, retry_sent, altered = False, False, False
                    try:
                        while not stopped.is_set():
                            now = time.monotonic()
                            try:
                                packet, address = listener.recvfrom(8192)
                            except socket.timeout:
                                if connection is not None:
                                    timer = connection.get_timer()
                                    if timer is not None and timer <= now:
                                        connection.handle_timer(now)
                                    for response, target in connection.datagrams_to_send(now):
                                        listener.sendto(response, target)
                                continue
                            stats["packets"] += 1
                            header = pull_quic_header(Buffer(data=packet), host_cid_length=8)
                            if original is None:
                                original = header.destination_cid
                            if mode == "drop-initial" and not dropped:
                                dropped = True
                                continue
                            if mode == "retry" and not retry_sent:
                                retry_sent = True
                                listener.sendto(encode_quic_retry(1, retry_cid, header.source_cid, original, b"validated-address"), address)
                                continue
                            if mode == "retry" and connection is None:
                                assert header.destination_cid == retry_cid and header.token == b"validated-address", header
                            if connection is None:
                                connection = QuicConnection(configuration=config, original_destination_connection_id=original,
                                    retry_source_connection_id=retry_cid if mode == "retry" else None)
                            connection.receive_datagram(packet, address, now)
                            while (event := connection.next_event()) is not None:
                                if isinstance(event, HandshakeCompleted):
                                    completed.append(event)
                            responses = connection.datagrams_to_send(now)
                            if mode == "drop-flight" and not dropped:
                                dropped = True
                                continue
                            if responses and not altered:
                                altered = True
                                response, target = responses[0]
                                if mode == "corrupt":
                                    broken = bytearray(response)
                                    broken[-1] ^= 1
                                    listener.sendto(broken, target)
                                if mode == "wrong-address":
                                    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as stranger:
                                        for candidate, target in responses:
                                            stranger.sendto(candidate, target)
                                    listener.settimeout(0.3)
                                    try:
                                        premature, _ = listener.recvfrom(8192)
                                    except socket.timeout:
                                        pass
                                    else:
                                        raise AssertionError(f"Client responded to a flight from the wrong source port: {premature.hex()}")
                                    finally:
                                        listener.settimeout(0.02)
                                if mode == "duplicate":
                                    responses = [responses[0], responses[0], *responses[1:]]
                            for response, target in responses:
                                listener.sendto(response, target)
                    except BaseException as error:
                        failures.append(error)

                thread = threading.Thread(target=peer)
                thread.start()
                try:
                    result = subprocess.run([str(binary), mode, str(listener.getsockname()[1])], cwd=ROOT,
                        capture_output=True, text=True, timeout=15)
                    if mode not in ("untrusted", "hostname"):
                        deadline = time.monotonic() + 1
                        while not completed and not failures and time.monotonic() < deadline:
                            stopped.wait(0.01)
                finally:
                    stopped.set()
                    thread.join(timeout=2)
                assert not thread.is_alive() and not failures, (mode, failures)
                assert result.returncode == 0 and not result.stderr, (mode, result)
                if mode in ("untrusted", "hostname"):
                    expected = "X.509 certificate chain is not trusted" if mode == "untrusted" else "X.509 certificate does not match hostname"
                    assert result.stdout == "ERROR " + expected + "\n", (mode, result.stdout)
                else:
                    assert result.stdout == "AUTHENTICATED\n" and completed, (mode, result.stdout, stats)
    print("Production QUIC client: live authentication, Initial/flight loss, duplicates, tampering, source binding, Retry and cleanup passed")


if __name__ == "__main__":
    main()
