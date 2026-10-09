#!/usr/bin/env python3
"""Exercise Dark HTTPS discovery against a deterministic UDP DNS peer."""

import socket
import struct
import subprocess
import tempfile
import threading
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DARK = '''// dns.dark - Exercise owned HTTPS DNS discovery and cleanup.
let run () : Unit =
  match Stdlib.Cli.__Args.get 0 |> Stdlib.Result.andThen (fun text -> Stdlib.Int64.parse text |> Stdlib.Result.mapError (fun _error -> "port")) with
  | Error message -> Stdlib.printLine message
  | Ok port ->
    let peer = Stdlib.__Datagram.Endpoint { address = [127L,0L,0L,1L], port = port } in
    match Stdlib.__HttpsDns.lookup peer "example.com" 443L 500L with
    | Error message -> Stdlib.printLine ("ERROR " ++ message)
    | Ok answer -> Stdlib.printLine ("SERVICES " ++ Stdlib.Int.toString (Stdlib.List.length answer.services) ++ " TTL " ++ Stdlib.Int64.toString answer.ttl)
run ()
'''


def name(text):
    return b"".join(bytes([len(part)]) + part.encode() for part in text.split(".")) + b"\0"


def response(query, payload, ttl=60):
    header = query[:2] + struct.pack("!5H", 0x8180, 1, 1, 0, 0)
    return header + query[12:] + b"\xc0\x0c" + struct.pack("!HHIH", 65, 1, ttl, len(payload)) + payload


def main():
    with tempfile.TemporaryDirectory(prefix="dark-https-dns-") as directory:
        source, binary = Path(directory) / "dns.dark", Path(directory) / "dns"
        source.write_text(DARK)
        result = subprocess.run([str(ROOT / "dark"), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                cwd=ROOT, capture_output=True, text=True, timeout=120)
        assert result.returncode == 0, result.stdout + result.stderr
        for mode in ("advertisement", "alias", "cycle", "wrong-source", "wrong-id", "truncated", "timeout"):
            stop, errors, questions = threading.Event(), [], []
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as listener:
                listener.bind(("127.0.0.1", 0))
                listener.settimeout(0.05)

                def peer():
                    try:
                        while not stop.is_set():
                            try:
                                query, target = listener.recvfrom(4096)
                            except socket.timeout:
                                continue
                            assert query[2:12] == bytes.fromhex("01000001000000000000")
                            assert query[-4:] == bytes.fromhex("00410001")
                            questions.append(query)
                            payload = bytes.fromhex("00010000010003026833")
                            if mode == "timeout":
                                continue
                            if mode == "cycle":
                                payload = b"\0\0" + name("example.com")
                            elif mode == "alias" and len(questions) == 1:
                                payload = b"\0\0" + name("svc.example.com")
                            elif mode == "alias":
                                assert query[12:-4] == name("svc.example.com")
                            packet = response(query, payload, 20 if mode == "alias" and len(questions) == 1 else 60)
                            if mode == "wrong-source":
                                with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as forged:
                                    forged.sendto(response(query, b"\0\0\0"), target)
                                time.sleep(0.02)
                            if mode == "wrong-id":
                                wrong = ((int.from_bytes(query[:2], "big") + 1) % 65536).to_bytes(2, "big")
                                listener.sendto(wrong + packet[2:], target)
                            if mode == "truncated":
                                packet = packet[:2] + b"\x83\x80" + packet[4:]
                            listener.sendto(packet, target)
                    except BaseException as error:
                        errors.append(error)

                thread = threading.Thread(target=peer)
                thread.start()
                started = time.monotonic()
                try:
                    result = subprocess.run([str(binary), str(listener.getsockname()[1])], capture_output=True, text=True, timeout=5)
                finally:
                    stop.set()
                    thread.join(timeout=2)
                assert not errors, errors
                assert not thread.is_alive()
                assert result.returncode == 0, result.stdout + result.stderr
                assert "LEAK" not in result.stderr.upper(), result.stderr
                expected = {"cycle": "ERROR HTTPS DNS alias chain too long", "truncated": "ERROR Invalid HTTPS DNS response header",
                            "timeout": "ERROR HTTPS DNS timeout", "alias": "SERVICES 1 TTL 20"}.get(mode, "SERVICES 1 TTL 60")
                assert result.stdout.strip() == expected, (mode, result.stdout, result.stderr)
                assert time.monotonic() - started < 2, mode
                assert len(questions) == (2 if mode == "alias" else 1), (mode, len(questions))
                print(f"PASS {mode}")


if __name__ == "__main__":
    main()
