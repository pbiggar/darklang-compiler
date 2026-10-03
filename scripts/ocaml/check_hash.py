#!/usr/bin/env python3
"""Compare native clone-name SHA-256 and UTF-16 replacement against hashlib."""
import argparse
import hashlib
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-dir', type=Path, required=True)
    args = parser.parse_args()
    lib = args.build_dir.resolve() / 'default/lib'
    output = ROOT / 'TestResults/ocaml-migration/hash-vectors'
    output.mkdir(parents=True, exist_ok=True)
    raw = [b'', b'abc', b'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', bytes(range(256))]
    raw += [bytes((i * 37 + 11) % 256 for i in range(n)) for n in [1, 55, 56, 63, 64, 65, 111, 112, 127, 128, 129, 4096, 1000000]]
    units = [[], [65], [0x00e9], [101, 0x301], [0xd83d, 0xde00], [0xd800], [0xdc00], [0xd800, 65], [0xd800, 0xd800, 0xdc00], [0xdc00, 0xd800], [0xdfff, 0xdbff], [0, 0xffff, 0xd83d, 0xde00, 0xd800]]
    lines = ['open Dark_compiler', 'let () =']
    for value in raw:
        expression = '"' + ''.join('\\%03d' % byte for byte in value) + '"' if len(value) < 1000 else f'String.init {len(value)} (fun i -> Char.chr ((i * 37 + 11) mod 256))'
        lines.append(f' Printf.printf "%s\\n" (HostHash.sha256 ({expression}));')
    for value in units:
        lines.append(' Printf.printf "%s\\n" (HostHash.sha256Utf8 (HostText.ofUtf16Units [|' + ';'.join(map(str, value)) + '|]));')
    lines.append(' ()')
    source = output / 'hash_probe.ml'
    source.write_text('\n'.join(lines) + '\n')
    native = output / 'hash_probe.exe'
    environment = os.environ.copy()
    environment.pop('LD_PRELOAD', None)
    command = ['ocamlfind', 'ocamlopt', '-linkpkg', '-package', 'uutf,uucp,uunf,uuseg,zarith,yojson']
    for directory in [lib, lib / '.dark_compiler.objs/byte', lib / '.dark_compiler.objs/native']:
        command.extend(['-I', str(directory)])
    subprocess.run(command + [str(lib / 'dark_compiler.cmxa'), str(source), '-o', str(native)], env=environment, check=True)
    native.chmod(0o755)
    actual = subprocess.check_output([str(native)], env=environment, text=True).splitlines()
    expected = [hashlib.sha256(value).hexdigest() for value in raw]
    expected += [hashlib.sha256(b''.join(value.to_bytes(2, 'little') for value in group).decode('utf-16-le', errors='replace').encode('utf-8')).hexdigest() for group in units]
    if actual != expected:
        raise SystemExit('Native SHA-256 or UTF-16 replacement differs from the independent reference')
    print(f'{len(expected)}/{len(expected)} independent SHA-256 vectors passed')

if __name__ == '__main__':
    main()
