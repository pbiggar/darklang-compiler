#!/usr/bin/env python3
"""Check compiled QUIC parameter parsing against independent aioquic encodings."""
from pathlib import Path
import random
import subprocess
import tempfile

from aioquic.buffer import Buffer
from aioquic.quic.packet import QuicTransportParameters, push_quic_transport_parameters

ROOT = Path(__file__).resolve().parents[1]
FIELDS = [
    ("max_idle_timeout", "idleTimeout", 0), ("max_udp_payload_size", "maxUdpPayload", 65527),
    ("initial_max_data", "maxData", 0),
    ("initial_max_stream_data_bidi_local", "maxStreamDataBidiLocal", 0),
    ("initial_max_stream_data_bidi_remote", "maxStreamDataBidiRemote", 0),
    ("initial_max_stream_data_uni", "maxStreamDataUni", 0),
    ("initial_max_streams_bidi", "maxStreamsBidi", 0),
    ("initial_max_streams_uni", "maxStreamsUni", 0),
    ("ack_delay_exponent", "ackDelayExponent", 3), ("max_ack_delay", "maxAckDelay", 25),
    ("active_connection_id_limit", "activeConnectionIdLimit", 2),
]


def integer(value):
    buffer = Buffer(capacity=8)
    buffer.push_uint_var(value)
    return buffer.data


def parameter(kind, value):
    return integer(kind) + integer(len(value)) + value


def encode(values):
    buffer = Buffer(capacity=4096)
    push_quic_transport_parameters(buffer, values)
    return buffer.data


def main():
    cases = []
    for seed in range(64):
        rng = random.Random(seed)
        server = seed % 2 == 0
        values = QuicTransportParameters(initial_source_connection_id=rng.randbytes(seed % 21))
        if server:
            values.original_destination_connection_id = rng.randbytes((seed + 1) % 21)
            values.stateless_reset_token = rng.randbytes(16)
        for field, _, _ in FIELDS:
            if rng.choice([False, True]):
                bounds = {"max_udp_payload_size": (1200, 65527), "ack_delay_exponent": (0, 20),
                          "max_ack_delay": (0, 16383), "active_connection_id_limit": (2, 32),
                          "initial_max_streams_bidi": (0, 2**60), "initial_max_streams_uni": (0, 2**60)}
                setattr(values, field, rng.randint(*bounds.get(field, (0, 2**62 - 1))))
        values.disable_active_migration = rng.choice([False, True])
        encoded = encode(values) + parameter(31 * seed + 27, rng.randbytes(seed % 17))
        expected = "OK " + ",".join(str(getattr(values, field) if getattr(values, field) is not None else default)
                                    for field, _, default in FIELDS)
        expected += ":" + values.initial_source_connection_id.hex().upper()
        expected += ":" + ("true" if values.disable_active_migration else "false")
        cases.append((server, encoded, expected))

    client = parameter(15, b"")
    server = parameter(0, b"") + client
    bad = [
        (False, client + parameter(8, integer(2**60 + 1)), "QUIC stream count exceeds 2^60"),
        (False, client + parameter(9, integer(2**60 + 1)), "QUIC stream count exceeds 2^60"),
        (False, client + parameter(15, b""), "Duplicate QUIC transport parameter"),
        (False, client + parameter(99, b"") * 2, "Duplicate QUIC transport parameter"),
        (False, parameter(15, bytes(21)), "QUIC connection ID exceeds 20 bytes"),
        (True, server + parameter(2, bytes(15)), "Invalid QUIC stateless reset token"),
        (True, server + parameter(13, bytes(41)), "Invalid QUIC preferred address"),
        (True, server + parameter(13, bytes(42)), "Invalid QUIC preferred address"),
        (True, server + parameter(13, bytes(24) + b"\x01" + bytes(17)),
         "QUIC preferred address requires a nonempty initial source connection ID"),
        (False, client + parameter(4, b""), "Invalid QUIC integer transport parameter"),
        (False, client + parameter(4, b"\x00\x00"), "Invalid QUIC integer transport parameter"),
        (False, client + parameter(4, b"\xc0"), "Invalid QUIC integer transport parameter"),
        (False, client + parameter(3, integer(1199)), "QUIC maximum UDP payload is below 1200"),
        (False, client + parameter(10, integer(21)), "QUIC ACK delay exponent exceeds 20"),
        (False, client + parameter(11, integer(16384)), "QUIC maximum ACK delay exceeds limit"),
        (False, client + parameter(14, integer(1)), "QUIC active connection ID limit is below 2"),
        (False, client + parameter(12, b"\x00"), "Invalid QUIC disable active migration parameter"),
        (False, client + b"\x40", "Truncated QUIC transport parameter"),
        (False, client + b"\x04\x02\x00", "Truncated QUIC transport parameter"),
        (False, client + b"\x04", "Truncated QUIC transport parameter"),
        (False, client + b"\x04\xc0", "Truncated QUIC transport parameter"),
        (False, client + parameter(99, bytes(4096)), "QUIC transport parameters exceed 4096 bytes"),
        (False, client + b"".join(parameter(1000 + n, b"") for n in range(128)),
         "Too many QUIC transport parameters"),
    ]
    for kind in (0, 2, 13, 16):
        bad.append((False, client + parameter(kind, b""), "Server-only QUIC transport parameter from client"))
    cases += [(role, data, "ERROR " + message) for role, data, message in bad]
    defaults = "OK " + ",".join(str(default) for _, _, default in FIELDS) + "::false"
    cases += [(False, b"\x40\x0f\x40\x00", defaults),
              (False, client + b"".join(parameter(1000 + n, b"") for n in range(127)), defaults),
              (False, client + parameter(99, bytes(4090)), defaults)]
    assert len(cases[-1][1]) == 4096

    fields = ", ".join("Stdlib.Int64.toString p." + dark for _, dark, _ in FIELDS)
    program = '''// parameters.dark - Independent QUIC transport-parameter vectors.
let check (bytes: String) (sender: Stdlib.__QuicParameters.Sender) : Unit =
  match Stdlib.Blob.fromHex bytes |> Stdlib.Result.andThen (Stdlib.__QuicParameters.parse sender) with
  | Error message -> Stdlib.printLine ("ERROR " ++ message)
  | Ok p -> Stdlib.printLine ("OK " ++ Stdlib.String.join [@FIELDS@] "," ++ ":" ++ Stdlib.Blob.toHex p.initialSource ++ ":" ++ (if p.disableActiveMigration then "true" else "false"))
'''.replace("@FIELDS@", fields)
    program += "let runChecks () : Unit =\n"
    program += "\n".join('  let _ = check "' + data.hex() + '" Stdlib.__QuicParameters.Sender.' + ("Server" if role else "Client") + " in"
                         for role, data, _ in cases) + "\n  ()\nrunChecks ()\n"
    with tempfile.TemporaryDirectory(prefix="dark-quic-parameters-") as temporary:
        source, binary = Path(temporary) / "parameters.dark", Path(temporary) / "parameters"
        source.write_text(program)
        compiled = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, capture_output=True, text=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        result = subprocess.run([str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=30)
        assert result.returncode == 0 and not result.stderr, result
        expected = [value for _, _, value in cases]
        actual = result.stdout.splitlines()
        assert actual == expected, [(n, a, b) for n, (a, b) in enumerate(zip(actual, expected)) if a != b]
    print(f"QUIC parameters: {len(cases)} independent encodings, defaults, bounds, malformed values and cleanup passed")


if __name__ == "__main__":
    main()
