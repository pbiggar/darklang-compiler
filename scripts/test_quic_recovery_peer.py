#!/usr/bin/env python3
"""Compare compiled QUIC ACK/loss accounting with independent aioquic NewReno."""
from pathlib import Path
import random
import subprocess
import tempfile

from aioquic.quic.packet import QuicPacketType
from aioquic.quic.packet_builder import QuicSentPacket
from aioquic.quic.rangeset import RangeSet
from aioquic.quic.recovery import QuicPacketRecovery, QuicPacketSpace
from aioquic.tls import Epoch

ROOT = Path(__file__).resolve().parents[1]
DARK = '''// recovery.dark - Compare observable ACK, loss and congestion decisions.
let sendPackets (state: Stdlib.__QuicRecovery.State) (number: Int64) (count: Int64)
  : Stdlib.Result.Result<Stdlib.__QuicRecovery.State, String> =
  if number == count then Ok state
  else Stdlib.__QuicRecovery.sent state (Stdlib.__QuicRecovery.Packet {
    number = number, at = 100L + number, size = 1200L, token = number, payload = Stdlib.String.toBlob "x" }) true false
    |> Stdlib.Result.andThen (fun next -> sendPackets next (number + 1L) count)
let check (count: Int64) (ack: Stdlib.__QuicFrames.Ack) (now: Int64) : Unit =
  let result = Stdlib.__QuicRecovery.create 1200L |> Stdlib.Result.andThen (fun state -> sendPackets state 0L count)
    |> Stdlib.Result.andThen (fun state -> Stdlib.__QuicRecovery.acknowledge state ack now 0L 25L) in
  match result with
  | Error message -> Stdlib.printLine ("ERROR " ++ message)
  | Ok update ->
    let numbers = Stdlib.List.map update.retransmit (fun packet -> Stdlib.Int64.toString packet.number) in
    let rtt = update.state.rtt in
    Stdlib.printLine (Stdlib.Int64.toString update.state.congestion.window ++ " " ++
      Stdlib.Int64.toString update.state.congestion.inFlight ++ " " ++ Stdlib.Int64.toString rtt.latest ++ " " ++
      Stdlib.Int64.toString rtt.smoothed ++ " " ++ Stdlib.Int64.toString rtt.variance ++ " " ++ Stdlib.String.join numbers ",")
'''


def main():
    program, expected = DARK + "let runChecks () : Unit =\n", []
    for seed in range(48):
        rng = random.Random(seed)
        count = rng.randint(1, 10)
        received = sorted(rng.sample(range(count), rng.randint(1, count)))
        now = (200 + received[-1]) / 1000
        losses = []
        space = QuicPacketSpace()
        recovery = QuicPacketRecovery(congestion_control_algorithm="reno", initial_rtt=0.333,
                                      max_datagram_size=1200, peer_completed_address_validation=True,
                                      send_probe=lambda: None)
        recovery.spaces = [space]
        for number in range(count):
            packet = QuicSentPacket(epoch=Epoch.ONE_RTT, in_flight=True, is_ack_eliciting=True,
                                    is_crypto_packet=False, packet_number=number, packet_type=QuicPacketType.ONE_RTT,
                                    sent_time=(100 + number) / 1000, sent_bytes=1200)
            packet.delivery_handlers.append((lambda status, n=number: losses.append(n) if status.name == "LOST" else None, ()))
            recovery.on_packet_sent(packet=packet, space=space)
        ranges = RangeSet()
        for number in received:
            ranges.add(number, number + 1)
        recovery.on_ack_received(ack_rangeset=ranges, ack_delay=0, now=now, space=space)
        expected.append(f"{recovery.congestion_window} {recovery.bytes_in_flight} "
                        f"{round(recovery._rtt_latest * 1000)} {round(recovery._rtt_smoothed * 1000)} "
                        f"{round(recovery._rtt_variance * 1000)} " + ",".join(map(str, losses)))
        dark_ranges = ",".join(f"Stdlib.__QuicFrames.Range {{ smallest = {r.start}L, largest = {r.stop - 1}L }}"
                               for r in reversed(list(ranges)))
        program += f"  let _ = check {count}L (Stdlib.__QuicFrames.Ack {{ delay = 0L, ecn = None, ranges = [{dark_ranges}] }}) {round(now * 1000)}L in\n"
    program += "  ()\nrunChecks ()\n"
    with tempfile.TemporaryDirectory(prefix="dark-quic-recovery-") as temporary:
        source, binary = Path(temporary) / "recovery.dark", Path(temporary) / "recovery"
        source.write_text(program)
        compiled = subprocess.run([str(ROOT / "dark"), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, capture_output=True, text=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        result = subprocess.run([str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=30)
        assert result.returncode == 0 and not result.stderr, result
        actual = result.stdout.splitlines()
        assert actual == expected, [(n, a, b) for n, (a, b) in enumerate(zip(actual, expected)) if a != b]
    print("QUIC recovery: 48 independent ACK/loss/NewReno traces and compiled cleanup passed")


if __name__ == "__main__":
    main()
