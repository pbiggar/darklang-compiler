#!/usr/bin/env python3
"""Validate native Dark HTTP serving, framing, shutdown, and cleanup with local TCP peers."""

import http.client
from contextlib import closing
import select
import signal
import socket
import struct
import subprocess
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

SOURCE = '''// server.dark - Local native HTTP server contract and lifecycle probe.
let handler (req: Stdlib.Http.Request) : Stdlib.Http.Response =
    let path = Stdlib.HttpServer.getPath req in
    if path == "/echo" then Stdlib.Http.success req.body
    else if path == "/inspect" then
        Stdlib.Http.responseWithText (req.url ++ "|" ++ Stdlib.HttpServer.getMethod req) 200
    else if path == "/headers" then
        Stdlib.Http.responseWithHeaders (Stdlib.String.toBlob "headers")
            [("sErVeR", "custom"), ("strict-transport-security", "custom-hsts"),
             ("Set-Cookie", "a=1"), ("Set-Cookie", "b=2")] 200
    else if path == "/invalid" then
        Stdlib.Http.responseWithHeaders Stdlib.Blob.empty [("X-Bad", "unsafe\\r\\nvalue")] 200
    else
        Stdlib.HttpServer.routeRequest
            [Stdlib.HttpServer.get "/ping" (fun _ -> Stdlib.Http.responseWithText "pong" 200)] req

match Stdlib.Cli.__Args.int64 0, Stdlib.Cli.__Args.get 1 with
| Ok port, Ok mode ->
    let config = Stdlib.HttpServer.Config.Config {
        port = Stdlib.Int.fromInt64 port,
        maxBodyBytes = 16,
        injectStandardHeaders = mode != "plain",
        canonicalizeFromForwardedProto = mode != "plain",
        logRequests = mode == "logging"
    } in
    match Stdlib.HttpServer.serve config handler (fun _ -> Stdlib.printLine "LISTENING") with
    | Ok () -> Stdlib.printLine "STOPPED"
    | Error message -> Stdlib.printLine message
| _, _ -> Stdlib.printLine "Invalid test arguments"
'''


def free_port() -> int:
    with socket.socket() as listener:
        listener.bind(("127.0.0.1", 0))
        return listener.getsockname()[1]


def request(port: int, method: str, path: str, body=None, headers=None):
    with closing(http.client.HTTPConnection("127.0.0.1", port, timeout=5)) as client:
        client.request(method, path, body=body, headers=headers or {})
        response = client.getresponse()
        return response.status, response.getheaders(), response.read()


def wire(port: int, payload: bytes):
    with socket.create_connection(("127.0.0.1", port), timeout=5) as client:
        client.sendall(payload)
        response = http.client.HTTPResponse(client)
        response.begin()
        return response.status, response.getheaders(), response.read()


def start(binary: Path, port: int, mode: str):
    process = subprocess.Popen([str(binary), str(port), mode], cwd=ROOT,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    try:
        assert select.select([process.stdout], [], [], 10)[0], "No listening callback"
        announced = process.stdout.readline()
        assert announced == b"LISTENING\n", ("Bind failed before callback", announced)
        return process
    except BaseException:
        if process.poll() is None:
            process.kill()
        stdout, stderr = process.communicate()
        print("Server startup diagnostics:", process.returncode, stdout, stderr)
        raise


def stop(process, shutdown_signal=signal.SIGTERM):
    process.send_signal(shutdown_signal)
    stdout, stderr = process.communicate(timeout=5)
    assert process.returncode == 0, (process.returncode, stderr)
    assert b"STOPPED\n" in stdout, stdout
    assert stderr == b"", stderr  # Includes compiled leak accounting.
    return stdout


def main() -> None:
    directory = ROOT / "TestResults/ai/http-server-peer"
    directory.mkdir(parents=True, exist_ok=True)
    source, binary = directory / "server.dark", directory / "server"
    source.write_text(SOURCE)
    compiled = subprocess.run([str(ROOT / "dark"), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                              cwd=ROOT, capture_output=True, text=True, timeout=120)
    assert compiled.returncode == 0, compiled.stdout + compiled.stderr
    port = free_port()
    process = start(binary, port, "default")
    try:
        status, headers, body = request(port, "GET", "/ping")
        assert (status, body) == (200, b"pong")
        assert ("Server", "darklang") in headers
        assert ("Strict-Transport-Security", "max-age=31536000; includeSubDomains; preload") in headers
        assert request(port, "GET", "/missing")[0] == 404
        assert request(port, "POST", "/ping")[0] == 404
        assert request(port, "POST", "/echo", b"a\x00b\xff")[2] == b"a\x00b\xff"
        assert request(port, "POST", "/echo", b"x" * 16)[2] == b"x" * 16
        assert request(port, "POST", "/echo", b"x" * 17)[0] == 413
        assert wire(port, b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 17\r\n\r\n")[0] == 413
        assert wire(port, b"POST /echo HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n4\r\necho\r\n0\r\n\r\n")[2] == b"echo"
        assert wire(port, b"POST /echo HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n11\r\n")[0] == 413
        assert wire(port, b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 1\r\nTransfer-Encoding: chunked\r\n\r\n")[0] == 400
        assert wire(port, b"GET / HTTP/1.1\r\nHost: localhost\r\nHost: duplicate\r\n\r\n")[0] == 400
        status, headers, body = request(port, "HEAD", "/headers")
        assert (status, body) == (200, b"")
        assert ("Content-Length", "7") in headers
        with socket.create_connection(("127.0.0.1", port), timeout=5) as oversized_head:
            oversized_head.sendall(b"HEAD /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 17\r\n\r\n")
            chunks = []
            while chunk := oversized_head.recv(4096):
                chunks.append(chunk)
            error_wire = b"".join(chunks)
            assert error_wire.startswith(b"HTTP/1.1 413 ")
            assert error_wire.split(b"\r\n\r\n", 1)[1] == b"", "HEAD error response contained a body"
        with socket.create_connection(("127.0.0.1", port), timeout=5) as client:
            client.sendall(b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 4\r\nExpect: 100-continue\r\n\r\n")
            assert client.recv(4096) == b"HTTP/1.1 100 Continue\r\n\r\n"
            client.sendall(b"echo")
            response = http.client.HTTPResponse(client)
            response.begin()
            assert (response.status, response.read()) == (200, b"echo")
        assert wire(port, b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 4\r\nExpect: unsupported\r\n\r\n")[0] == 417
        status, headers, body = request(port, "GET", "/headers")
        assert (status, body) == (200, b"headers")
        assert [v for k, v in headers if k.lower() == "set-cookie"] == ["a=1", "b=2"]
        assert [v for k, v in headers if k.lower() == "server"] == ["custom"]
        assert [v for k, v in headers if k.lower() == "strict-transport-security"] == ["custom-hsts"]
        assert request(port, "GET", "/invalid")[0] == 500
        assert request(port, "GET", "/inspect?q=%26", headers={"Host": "example.test:8123", "X-Forwarded-Proto": "HTTPS", "X-Http-Method": "DELETE"})[2] == b"https://example.test/inspect?q=%26|GET"
        # Bind failure must never announce a listening server.
        duplicate = subprocess.run([str(binary), str(port), "default"], cwd=ROOT,
                                   capture_output=True, timeout=5)
        assert duplicate.returncode == 0 and duplicate.stderr == b"", duplicate
        assert b"Cannot bind HTTP server:" in duplicate.stdout
        assert b"LISTENING" not in duplicate.stdout
        # An abandoned or reset connection must not kill the listener.
        with socket.create_connection(("127.0.0.1", port), timeout=5) as abandoned:
            abandoned.sendall(b"GET /ping HTTP/1.1\r\n")
        with socket.create_connection(("127.0.0.1", port), timeout=5) as reset:
            reset.setsockopt(socket.SOL_SOCKET, socket.SO_LINGER, struct.pack("ii", 1, 0))
            reset.sendall(b"GET /ping HTTP/1.1\r\nHost: localhost\r\n\r\n")
        assert request(port, "GET", "/ping")[0] == 200
        # Fragmented lines and bodies parse across TCP reads.
        with socket.create_connection(("127.0.0.1", port), timeout=5) as fragmented:
            for part in (b"POST /ec", b"ho HTTP/1.1\r\nHost: localhost\r", b"\nContent-Length: 4\r\n\r\ne", b"cho"):
                fragmented.sendall(part)
                time.sleep(0.01)
            response = http.client.HTTPResponse(fragmented)
            response.begin()
            assert response.read() == b"echo"
        # Socket timeouts are retried, but an absolute monotonic deadline ends
        # a stalled request and releases the sequential listener for the next.
        with socket.create_connection(("127.0.0.1", port), timeout=12) as stalled:
            started = time.monotonic()
            stalled.sendall(b"GET /ping HTTP/1.1\r\n")
            response = http.client.HTTPResponse(stalled)
            response.begin()
            assert response.status == 408
            assert 9 <= time.monotonic() - started <= 12
            response.read()
        assert request(port, "GET", "/ping")[0] == 200
        stop(process)
    finally:
        if process.poll() is None:
            process.kill()
            process.communicate()
    # Rebinding proves the listener was released on graceful shutdown.
    process = start(binary, port, "plain")
    try:
        _, headers, body = request(port, "GET", "/inspect?q=%26", headers={"Host": "example.test:8123", "X-Forwarded-Proto": "https"})
        assert body == b"http://example.test:8123/inspect?q=%26|GET"
        assert not any(k.lower() in ("server", "strict-transport-security") for k, _ in headers)
        # Pending shutdown also interrupts a client that has stalled mid-header.
        with socket.create_connection(("127.0.0.1", port), timeout=5) as stalled:
            stalled.sendall(b"GET /ping HTTP/1.1\r\n")
            stop(process, signal.SIGINT)
    finally:
        if process.poll() is None:
            process.kill()
            process.communicate()
    process = start(binary, port, "logging")
    try:
        assert request(port, "GET", "/ping")[0] == 200
        assert b"[HttpServer] GET /ping 200 " in stop(process)
    finally:
        if process.poll() is None:
            process.kill()
            process.communicate()
    print("HTTP server peer: routing, framing, limits, headers, signals, rebind, and leak checks passed")


if __name__ == "__main__":
    main()
