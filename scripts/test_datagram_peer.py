#!/usr/bin/env python3
"""Verify compiled Dark UDP sockets against independent Python IPv4/IPv6 peers."""

import select
import socket
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def line(process):
    ready, _, _ = select.select([process.stdout], [], [], 10)
    assert ready, "Dark UDP peer stalled"
    value = process.stdout.readline().decode().strip()
    assert value, "Dark UDP peer exited: " + process.stderr.read().decode()
    return value


def check(directory, family):
    ipv6 = family == socket.AF_INET6
    host = "::1" if ipv6 else "127.0.0.1"
    address = "[" + ", ".join(str(x) + "L" for x in socket.inet_pton(family, host)) + "]"
    bind = "bind6" if ipv6 else "bind4"
    with socket.socket(family, socket.SOCK_DGRAM) as reservation:
        reservation.bind((host, 0))
        port = reservation.getsockname()[1]
    source = directory / ("udp6.dark" if ipv6 else "udp4.dark")
    binary = source.with_suffix("")
    source.write_text(f"""// udp.dark - Independent peer, packet boundaries, and owned cleanup.
let loop (connection: Stdlib.__Datagram.Socket) (remaining: Int64) : Unit =
  if remaining == 0L then ()
  else
    let _ =
      match Stdlib.__Datagram.receive connection 1000L with
      | Error code -> Stdlib.printLine ("ERROR " ++ Stdlib.Int64.toString code)
      | Ok message ->
        let _ = Stdlib.printLine ("PEER " ++ Stdlib.Int64.toString message.peer.port ++ " " ++ Stdlib.Blob.toHex message.payload) in
        if message.peer.address != {address} then Stdlib.printLine "BAD ADDRESS"
        else
          match Stdlib.__Datagram.send connection message.peer message.payload 1000L with
          | Error code -> Stdlib.printLine ("SEND ERROR " ++ Stdlib.Int64.toString code)
          | Ok () -> () in
    loop connection (remaining - 1L)
match Stdlib.__Datagram.{bind} {address} {port}L with
| Error code -> Stdlib.printLine ("BIND ERROR " ++ Stdlib.Int64.toString code)
| Ok connection ->
  let _ =
    match Stdlib.__Datagram.{bind} {address} {port}L with
    | Error _ -> Stdlib.printLine "COLLISION"
    | Ok other -> let _ = Stdlib.__Datagram.close other in Stdlib.printLine "BAD COLLISION" in
  let _ = Stdlib.printLine "READY" in
  let _ = loop connection 7L in
  let _ =
    match Stdlib.__Datagram.receive connection 30L with
    | Error code -> if code == 11L || code == 35L then Stdlib.printLine "TIMEOUT" else Stdlib.printLine "BAD TIMEOUT"
    | Ok _ -> Stdlib.printLine "BAD TIMEOUT" in
  let _ = Stdlib.__Datagram.close connection in
  let _ = Stdlib.__Datagram.close connection in
  match Stdlib.__Datagram.receive connection 1000L with
  | Error 9L -> Stdlib.printLine "CLOSED"
  | _ -> Stdlib.printLine "BAD CLOSED"
""")
    compiled = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)],
                              cwd=ROOT, text=True, capture_output=True, timeout=120)
    assert compiled.returncode == 0, compiled.stdout + compiled.stderr
    process = subprocess.Popen([str(binary)], cwd=ROOT,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=0)
    try:
        assert line(process) == "COLLISION"
        assert line(process) == "READY"
        with socket.socket(family, socket.SOCK_DGRAM) as first, socket.socket(family, socket.SOCK_DGRAM) as second:
            for peer in (first, second):
                peer.bind((host, 0))
                peer.settimeout(3)
            packets = [b"", b"first", b"second", bytes(range(256)) * 16, b"x" * 4097, b"x" * 5000, b"after"]
            # Queue different senders before reading: neither datagrams nor source addresses may merge.
            for index, payload in enumerate(packets):
                (first if index % 2 == 0 else second).sendto(payload, (host, port))
            for index, payload in enumerate(packets):
                peer = first if index % 2 == 0 else second
                if len(payload) > 4096:
                    assert line(process) == "ERROR 90"
                else:
                    observed = line(process)
                    expected = f"PEER {peer.getsockname()[1]} {payload.hex().upper()}".strip()
                    assert observed == expected, (index, observed, expected)
                    echoed, sender = peer.recvfrom(8192)
                    assert echoed == payload and sender[:2] == (host, port), (echoed, sender)
        assert line(process) == "TIMEOUT"
        assert line(process) == "CLOSED"
        output, errors = process.communicate(timeout=10)
        assert process.returncode == 0 and not output and not errors, (output, errors, process.returncode)
    finally:
        if process.poll() is None:
            process.kill()
            process.communicate()


def main():
    with tempfile.TemporaryDirectory(prefix="dark-datagram-") as temporary:
        for family in (socket.AF_INET, socket.AF_INET6):
            check(Path(temporary), family)
    print("UDP IPv4/IPv6 independent peers, empty/bounded packets, source addresses, timeout, collision, and cleanup verified")


if __name__ == "__main__":
    main()
