#!/usr/bin/env python3
"""Exercise compiled multi-socket readiness against independent TCP/UDP peers."""

import selectors
import socket
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

SOURCE = '''// network_poll.dark - Wait for independent TCP and UDP readiness in one syscall.
let run (tcpPort: Int64) (udpPort: Int64) : Stdlib.Result.Result<Unit, String> =
  Stdlib.__Network.listenTcp4 [127L, 0L, 0L, 1L] tcpPort |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun listener ->
    let result = Stdlib.__Datagram.bind4 [127L, 0L, 0L, 1L] udpPort |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun udp ->
      let result = match udp.watch with
      | None -> Error "Native UDP watch missing"
      | Some watch ->
        Stdlib.printLine "LISTENING"
        let entries = [Stdlib.__Network.Interest { watch = listener.watch, readable = true, writable = false },
          Stdlib.__Network.Interest { watch = watch, readable = true, writable = false }] in
        Stdlib.__Network.waitSockets entries 5000L |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun events ->
          let ready = Stdlib.List.map events (fun event -> Stdlib.Int64.toString event.index) in
          Stdlib.printLine ("READY " ++ Stdlib.String.join ready ",")
          if Stdlib.List.any events (fun event -> event.failed || event.closed || Stdlib.Bool.not event.readable) then Error "Invalid readiness"
          else if Stdlib.List.any events (fun event -> event.index == 1L) then
            Stdlib.__Datagram.receive udp 1000L |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun message ->
              if Stdlib.Blob.toString message.payload == Ok "packet" then Ok () else Error "UDP payload changed")
          else if Stdlib.List.any events (fun event -> event.index == 0L) then
            Stdlib.__Network.acceptTcp listener 1000L |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun connection ->
              let result = match connection.watch with
              | None -> Error "Native TCP watch missing"
              | Some watch ->
                Stdlib.__Network.waitSockets [Stdlib.__Network.Interest { watch = watch, readable = true, writable = false }] 5000L
                  |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun ready ->
                    if Stdlib.List.length ready != 1 then Error "TCP payload not ready"
                    else Stdlib.__Network.readTcp connection 4096L |> Stdlib.Result.mapError Stdlib.Int64.toString
                      |> Stdlib.Result.andThen (fun bytes -> if Stdlib.Blob.toString bytes == Ok "packet" then Ok () else Error "TCP payload changed")) in
              Stdlib.__Network.closeTcp connection
              result)
          else Error "Readiness timed out") in
      Stdlib.__Datagram.close udp
      result) in
    Stdlib.__Network.closeListener listener
    result)
match Stdlib.Cli.__Args.get 0 |> Stdlib.Result.mapError (fun _message -> ()) |> Stdlib.Result.andThen (fun text -> Stdlib.Int64.parse text |> Stdlib.Result.mapError (fun _error -> ())),
  Stdlib.Cli.__Args.get 1 |> Stdlib.Result.mapError (fun _message -> ()) |> Stdlib.Result.andThen (fun text -> Stdlib.Int64.parse text |> Stdlib.Result.mapError (fun _error -> ())) with
| Ok tcpPort, Ok udpPort -> match run tcpPort udpPort with | Ok () -> Stdlib.printLine "DONE" | Error message -> Stdlib.printLine ("ERROR " ++ message)
| _, _ -> Stdlib.printLine "ERROR ports"
'''


def read_line(process):
    with selectors.DefaultSelector() as selector:
        selector.register(process.stdout, selectors.EVENT_READ)
        assert selector.select(10), "Compiled readiness peer stalled"
    return process.stdout.readline().decode().strip()


def reserve_port(kind):
    with socket.socket(socket.AF_INET, kind) as reservation:
        reservation.bind(("127.0.0.1", 0))
        return reservation.getsockname()[1]


def main():
    with tempfile.TemporaryDirectory(prefix="dark-network-poll-") as temporary:
        directory = Path(temporary)
        source, binary = directory / "poll.dark", directory / "poll"
        source.write_text(SOURCE)
        result = subprocess.run(
            [str(ROOT / "dark"), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
            cwd=ROOT, capture_output=True, text=True, timeout=120,
        )
        assert result.returncode == 0, result.stdout + result.stderr
        for kind, expected in [(socket.SOCK_DGRAM, "READY 1"), (socket.SOCK_STREAM, "READY 0")]:
            tcp_port, udp_port = reserve_port(socket.SOCK_STREAM), reserve_port(socket.SOCK_DGRAM)
            process = subprocess.Popen([str(binary), str(tcp_port), str(udp_port)], cwd=ROOT,
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=0)
            try:
                assert read_line(process) == "LISTENING"
                with socket.socket(socket.AF_INET, kind) as peer:
                    if kind == socket.SOCK_DGRAM:
                        peer.sendto(b"packet", ("127.0.0.1", udp_port))
                    else:
                        peer.settimeout(5)
                        peer.connect(("127.0.0.1", tcp_port))
                        peer.sendall(b"packet")
                    assert read_line(process) == expected
                    output, errors = process.communicate(timeout=10)
                    assert process.returncode == 0 and output.strip() == b"DONE" and not errors, (output, errors)
            finally:
                if process.poll() is None:
                    process.kill()
                process.communicate()
    print("TCP/UDP multi-socket readiness, accepted-socket readiness and compiled cleanup verified")


if __name__ == "__main__":
    main()
