#!/usr/bin/env python3
"""Compare authenticated QUIC key generations with test-only aioquic packet protection."""

import subprocess
import tempfile
from pathlib import Path

from aioquic.quic.crypto import CryptoContext, next_key_phase
from aioquic.tls import CipherSuite

ROOT = Path(__file__).resolve().parents[1]
SECRET = b"01234567890123456789012345678901"
DARK = """// keys.dark - Check peer-generated key updates, delayed old packets and authenticated violations.
let opened (state: Stdlib.QuicKeys.State) (bytes: Blob) (now: Int64) (confirmed: Bool)
  : Stdlib.Result.Result<Stdlib.QuicKeys.State, String> =
  Stdlib.QuicKeys.openPacket state bytes 1L now 100L confirmed |> Stdlib.Result.map (fun opened -> opened.state)
let report (result: Stdlib.Result.Result<Stdlib.QuicKeys.State, String>) : Unit =
  match result with
  | Error message -> Stdlib.printLine ("ERROR " ++ message)
  | Ok state ->
    let (boundary, oldLargest) = match state.previous with | None -> (-1L, -1L) | Some previous -> (previous.boundary, previous.largest) in
    Stdlib.printLine ((if state.current.phase then "1" else "0") ++ "," ++ Stdlib.Int64.toString state.count ++ "," ++
      Stdlib.Int64.toString state.first ++ "," ++ Stdlib.Int64.toString state.largest ++ "," ++
      Stdlib.Int64.toString boundary ++ "," ++ Stdlib.Int64.toString oldLargest ++ "," ++
      Stdlib.Blob.toHex state.current.secret ++ "," ++ Stdlib.Blob.toHex state.current.keys.hp ++ "," ++ (if state.acked then "1" else "0"))
let check () : Unit =
@CHECKS@
  ()
check ()
"""


def main():
    first = CryptoContext()
    first.setup(cipher_suite=CipherSuite.AES_128_GCM_SHA256, secret=SECRET, version=1)
    second = next_key_phase(first)
    third = next_key_phase(second)
    # apply_key_phase in aioquic intentionally leaves header protection alone.
    second.hp = third.hp = first.hp
    generations = (first, second, third)

    def packet(generation, number, reserved=False):
        context = generations[generation]
        head = bytes([0x43 | (context.key_phase << 2) | (0x18 if reserved else 0)]) + number.to_bytes(4, "big")
        return context.encrypt_packet(head, b"peer-payload", number)

    initial = f'Stdlib.Blob.fromHex "{SECRET.hex()}" |> Stdlib.Result.andThen Stdlib.QuicKeys.create'

    def receive(expression, generation, number, now=0, confirmed=True, reserved=False):
        data = packet(generation, number, reserved)
        return f'{expression} |> Stdlib.Result.andThen (fun state -> Stdlib.Blob.fromHex "{data.hex()}" |> Stdlib.Result.andThen (fun bytes -> opened state bytes {now}L {str(confirmed).lower()}))'

    updated = receive(initial, 1, 3)
    checks = [
        (receive(initial, 0, 0), 0, (1, 0, 0, -1, -1, True)),
        (updated, 1, (1, 3, 3, 3, -1, False)),
        (receive(updated, 0, 2, now=299), 1, (1, 3, 3, 3, 2, False)),
        (receive(updated, 0, 2, now=301), "QUIC application authentication failed", None),
        (receive(initial, 1, 3, confirmed=False), "Invalid QUIC key update", None),
        (receive(updated, 2, 6), "Invalid QUIC key update", None),
        (receive(updated + ' |> Stdlib.Result.map Stdlib.QuicKeys.acknowledged', 2, 6), 2, (1, 6, 6, 6, 3, False)),
        (receive(receive(initial, 0, 5), 1, 2), "Invalid QUIC key update", None),
        (receive(updated, 0, 4), "Invalid previous QUIC key phase", None),
        (receive(receive(receive(initial, 1, 10), 0, 9), 1, 8), "QUIC key update packet numbers overlap", None),
        (receive(receive(receive(initial, 0, 5), 1, 10), 1, 8), 1, (2, 8, 10, 8, 5, False)),
        (receive(initial, 0, 0, reserved=True), "Invalid QUIC reserved bits", None),
    ]
    expected = []
    for _, generation, values in checks:
        if isinstance(generation, str):
            expected.append("ERROR " + generation)
        else:
            count, begin, largest, boundary, old, acked = values
            from aioquic.quic.crypto import derive_key_iv_hp
            _, _, hp = derive_key_iv_hp(cipher_suite=CipherSuite.AES_128_GCM_SHA256, secret=SECRET, version=1)
            expected.append(f"{generations[generation].key_phase},{count},{begin},{largest},{boundary},{old},{generations[generation].secret.hex().upper()},{hp.hex().upper()},{int(acked)}")
    code = DARK.replace("@CHECKS@", "\n".join(f"  let _ = report ({expression}) in" for expression, _, _ in checks))
    with tempfile.TemporaryDirectory(prefix="dark-quic-keys-") as temporary:
        source, binary = Path(temporary) / "keys.dark", Path(temporary) / "keys"
        source.write_text(code)
        built = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)], cwd=ROOT,
            capture_output=True, text=True, timeout=120)
        assert built.returncode == 0, built.stdout + built.stderr
        result = subprocess.run([str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=30)
        assert result.returncode == 0 and not result.stderr, result
        actual = result.stdout.splitlines()
        assert actual == expected, [(i, want, got) for i, (want, got) in enumerate(zip(expected, actual)) if want != got]
    print("QUIC key updates: 12 independent generation, header protection, reordering, expiry, acknowledgement and authenticated violation checks passed")


if __name__ == "__main__":
    main()
