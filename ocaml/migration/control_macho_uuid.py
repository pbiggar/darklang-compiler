"""Supply the same UUID entropy input to complete migration-only implementations.

No executable bytes are stripped, masked, or rewritten after generation.
Production F#/OCaml implementations continue to generate fresh UUIDs.
"""
import sys
from pathlib import Path

UUID_HEX = "00112233445546778899aabbccddeeff"
source = Path(sys.argv[1]).read_text()
if sys.argv[1].endswith(".ml"):
    old = "let hex=HostGuid.newGuidN () in"
    new = f'let hex="{UUID_HEX}" in'
    assert source.count(old) == 1
    source = "open Dark_compiler\n" + source.replace(old, new)
else:
    old = "System.Guid.NewGuid().ToByteArray()"
    new = 'System.Guid.Parse("00112233-4455-4677-8899-aabbccddeeff").ToByteArray()'
    assert source.count(old) == 1
    source = source.replace("module Binary_Generation_MachO", "module ControlledMachO", 1).replace(old, new)
print(source, end="")
