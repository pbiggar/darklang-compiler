#!/usr/bin/env python3
"""Verify same-port TLS/QUIC serving, Alt-Svc, repeat connections and bind ownership."""

import argparse
import socket
import subprocess
import tempfile
from pathlib import Path

from cryptography.hazmat.primitives import serialization

from test_http3_server_peer import ROOT, SERVER, exchange, start, stop
from test_http2_server_tls_peer import connect, h2_exchange
from test_quic_tls_peer import certificates

CLIENT = '''// client.dark - Learn same-origin HTTP/3 and retain lazy body ownership after session close.
let run () : Unit =
  match Stdlib.Blob.fromHex "@ROOT@" with
  | Error _ -> Stdlib.printLine "ERROR"
  | Ok root ->
    let session = Stdlib.HttpClientSession.createTrustedWithRoots [root] in
    let body = Stdlib.String.join (Stdlib.List.repeatUnsafe 70 (Stdlib.String.repeat "x" 1000)) "" |> Stdlib.String.toBlob in
    let url = "https://localhost:" ++ (Stdlib.Cli.Args.get 0 |> Stdlib.Result.withDefault "0") ++ "/echo" in
    let result = session.request "POST" url [] body |> Stdlib.Result.andThen (fun first ->
      if first.statusCode != 200 || Stdlib.Blob.toList first.body != Stdlib.Blob.toList body then Error Stdlib.HttpClient.RequestError.NetworkError
      else session.request "POST" url [] body |> Stdlib.Result.andThen (fun second ->
        if second.statusCode != 200 || Stdlib.Blob.toList second.body != Stdlib.Blob.toList body then Error Stdlib.HttpClient.RequestError.NetworkError
        else session.stream "POST" url [] body |> Stdlib.Result.map (fun third ->
          session.close ()
          let received = Stdlib.Stream.toList third.body in
          Stdlib.Stream.close third.body
          third.statusCode == 200 && received == Stdlib.Blob.toList body))) in
    session.close ()
    Stdlib.printLine (match result with | Ok true -> "DONE" | _ -> "ERROR")
run ()
'''


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    pem = leaf.public_bytes(serialization.Encoding.PEM)
    trust = ca.public_bytes(serialization.Encoding.PEM)
    private = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
    server = SERVER.replace("Stdlib.HttpServer.Quic.serve", "Stdlib.HttpServer.Secure.serve")
    server = server.replace('  else if Stdlib.HttpServer.getMethod', '''  else if Stdlib.String.endsWith request.url "/clear" then Stdlib.Http.Response { statusCode = 200, headers = [("alt-svc", "clear")], body = Stdlib.Blob.empty }
  else if Stdlib.String.endsWith request.url "/421" then Stdlib.Http.responseWithText "misdirected" 421
  else if Stdlib.HttpServer.getMethod''')
    with tempfile.TemporaryDirectory(prefix="dark-http-secure-", dir=ROOT.parent) as temporary:
        source, binary = Path(temporary) / "server.dark", Path(temporary) / "server"
        source.write_text(server.replace("@CERT@", pem.hex()).replace("@KEY@", private.hex()))
        result = subprocess.run([str(args.compiler), str(source), "--leak-check", "-o", str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        process, port = start(binary)
        try:
            advertisement = f'h3=":{port}"; ma=3600'.encode()
            for protocols in (["http/1.1"], []):
                with connect(port, trust, protocols) as peer:
                    peer.sendall(b"GET /echo HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
                    response = bytearray()
                    while data := peer.recv(65536):
                        response.extend(data)
                    assert response.startswith(b"HTTP/1.1 200") and response.lower().count(b"alt-svc:") == 1, response
                    assert advertisement in response, response
            with connect(port, trust, ["h2"]) as peer:
                h2_exchange(peer, port, "POST", 70000)
            for path, status, advertised in ((b"/clear", b"200", b"clear"), (b"/421", b"421", None)):
                with connect(port, trust, ["http/1.1"]) as peer:
                    peer.sendall(b"GET " + path + b" HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
                    response = bytearray()
                    while data := peer.recv(65536):
                        response.extend(data)
                    assert response.startswith(b"HTTP/1.1 " + status), response
                    assert (b"alt-svc:" in response.lower()) == (advertised is not None), response
                    if advertised:
                        assert b"alt-svc: clear" in response.lower(), response
            for _ in range(3):
                with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
                    peer.bind(("127.0.0.1", 0))
                    peer.settimeout(0.01)
                    body = bytes(range(251)) * 279
                    headers, response, replay = exchange(peer, port, ca, body=body)
                    assert response == body and headers.count((b"alt-svc", advertisement)) == 1, headers
                    peer.sendto(replay, ("127.0.0.1", port))
            client_source, client_binary = Path(temporary) / "client.dark", Path(temporary) / "client"
            client_source.write_text(CLIENT.replace("@ROOT@", ca.public_bytes(serialization.Encoding.DER).hex()))
            result = subprocess.run([str(args.compiler), str(client_source), "--leak-check", "-o", str(client_binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
            assert result.returncode == 0, result.stdout + result.stderr
            result = subprocess.run([str(client_binary), str(port)], cwd=ROOT, capture_output=True, text=True, timeout=90)
            assert result.returncode == 0 and result.stdout == "DONE\n" and not result.stderr, result
            stop(process)
            print("TLS HTTP/1.1/no-ALPN, HTTP/2 upload, Alt-Svc clear/421 and three HTTP/3 exchanges; zero leaks", flush=True)
            process, _ = start(binary, port)
            with socket.create_connection(("127.0.0.1", port), timeout=2) as peer:
                peer.sendall(b"\x16\x03")
                stop(process)
            process, _ = start(binary, port)
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
                peer.bind(("127.0.0.1", 0))
                peer.settimeout(0.01)
                exchange(peer, port, ca, body=b"partial", declared=100, stalled=True)
                stop(process)
            print("Same-port rebind and stalled TCP/QUIC shutdown; zero leaks", flush=True)
        finally:
            if process.poll() is None:
                process.kill()
                stdout, stderr = process.communicate()
                print("Listener diagnostics:", stdout, stderr, flush=True)
            elif process.returncode:
                stdout, stderr = process.communicate()
                print("Listener diagnostics:", process.returncode, stdout, stderr, flush=True)
        for transport, error in ((socket.SOCK_DGRAM, "UDP"), (socket.SOCK_STREAM, "TCP")):
            with socket.socket(socket.AF_INET, transport) as occupied:
                occupied.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                occupied.bind(("127.0.0.1", port))
                if transport == socket.SOCK_STREAM:
                    occupied.listen()
                result = subprocess.run([str(binary), str(port)], cwd=ROOT, capture_output=True, text=True, timeout=5)
                assert result.returncode == 0 and result.stdout == f"Cannot bind secure HTTP {error} server\n" and not result.stderr, result
                with socket.socket(socket.AF_INET, socket.SOCK_STREAM if transport == socket.SOCK_DGRAM else socket.SOCK_DGRAM) as other:
                    other.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                    other.bind(("127.0.0.1", port))
            print(f"Occupied {error} rejects before announcement and releases the other transport", flush=True)


if __name__ == "__main__":
    main()
