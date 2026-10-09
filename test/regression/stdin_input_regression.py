#!/usr/bin/env python3
"""Exercise compiled stdin streams and real terminal key/secret input."""
import os
import pathlib
import pty
import select
import signal
import subprocess
import sys
import tempfile
import termios
import time


def main():
    compiler = str(pathlib.Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix='dark-stdin-') as directory:
        root = pathlib.Path(directory)
        programs = {
            'key': '''let k = Builtin.stdinReadKey () in
let report = $"{Stdlib.Cli.Stdin.KeyRead.toString k}|{k.keyChar}|{Stdlib.Int.toString k.repeat}" in
let _ = Stdlib.Cli.Posix.fdWrite 2 (Stdlib.String.toBlob report) in
0L''',
            'keys': '''let a = Stdlib.Cli.Stdin.readKey () in
let b = Stdlib.Cli.Stdin.readKey () in
let report = $"{Stdlib.Cli.Stdin.KeyRead.toString a}:{Stdlib.Int.toString a.repeat}|{Stdlib.Cli.Stdin.KeyRead.toString b}:{Stdlib.Int.toString b.repeat}" in
let _ = Stdlib.Cli.Posix.fdWrite 2 (Stdlib.String.toBlob report) in
0L''',
            'secret': '''let value = Stdlib.Cli.Stdin.readSecret () in
let _ = Stdlib.Cli.Posix.fdWrite 2 (Stdlib.String.toBlob value) in
0L''',
            'all': '''let value = Stdlib.Cli.Stdin.readAll () in
let _ = Stdlib.Cli.Posix.fdWrite 1 (Stdlib.String.toBlob value) in
0L''',
            'split': '''let a = Builtin.stdinReadExactly 1 in
let b = Builtin.stdinReadExactly 1 in
let c = Builtin.stdinReadAll () in
let _ = Stdlib.Cli.Posix.fdWrite 1 (Stdlib.String.toBlob (a ++ "|" ++ b ++ "|" ++ c)) in
0L''',
            'invalid': 'let _ = Builtin.stdinReadExactly -1 in 0L',
            'overflow': 'let _ = Builtin.stdinReadExactly 2147483648 in 0L',
        }
        binaries = {}
        for name, code in programs.items():
            source = root / (name + '.dark')
            source.write_text(code)
            binary = root / name
            subprocess.run([compiler, '-q', '--leak-check', str(source), '-o', str(binary)], check=True, timeout=120)
            binaries[name] = binary
        for payload in [b'', b'one\r\ntwo\n', '二😀e\u0301\n'.encode(), b'x' * 200000, b'\xffa\xe0\x80\x80']:
            result = subprocess.run([binaries['all']], input=payload, capture_output=True, timeout=30)
            expected = payload.decode('utf-8', 'replace').encode()
            assert (result.returncode, result.stdout, result.stderr) == (0, expected, b''), result
        result = subprocess.run([binaries['split']], input='😀tail'.encode(), capture_output=True, timeout=15)
        assert (result.returncode, result.stdout, result.stderr) == (0, '�|�|tail'.encode(), b''), result
        for name in ['invalid', 'overflow']:
            result = subprocess.run([binaries[name]], input=b'', capture_output=True, timeout=15)
            assert result.returncode != 0 and b'out-of-range' in result.stderr and b'leaks:' not in result.stderr, result
        result = subprocess.run([binaries['key']], input=b'a', capture_output=True, timeout=15)
        assert (result.returncode, result.stderr) == (0, b'Escape||1'), result

        def terminal(name, payload=None, expected=b'', chunks=None, resize=False):
            master, slave = pty.openpty()
            original = termios.tcgetattr(slave)
            process = subprocess.Popen([binaries[name]], stdin=slave, stdout=slave, stderr=subprocess.PIPE)
            try:
                deadline = time.monotonic() + 15
                while termios.tcgetattr(slave)[3] & termios.ICANON:
                    assert process.poll() is None, process.communicate()
                    assert time.monotonic() < deadline, 'terminal reader never entered raw mode'
                    time.sleep(.005)
                if resize:
                    os.kill(process.pid, signal.SIGWINCH)
                elif chunks:
                    for chunk in chunks:
                        os.write(master, chunk)
                        time.sleep(.08)
                else:
                    os.write(master, payload)
                _, report = process.communicate(timeout=15)
                assert (process.returncode, report) == (0, expected), (name, payload, process.returncode, report)
                assert termios.tcgetattr(slave) == original, 'terminal attributes were not restored'
                output = b''
                while select.select([master], [], [], 0)[0]:
                    output += os.read(master, 4096)
                if name == 'secret':
                    assert output == b'\r\n', ('secret input was echoed', output)
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait()
                os.close(master)
                os.close(slave)

        for data, report in [(b'a', b'A|a|1'), (b'A', b'SHIFT+A|A|1'), (b'\x03', b'CTRL+C||1'),
                             (b'\x1bx', b'ALT+X|x|1'), (b'\x1b', b'Escape||1'),
                             (b'\x1b[A', b'UpArrow||1'), (b'\x1b[1;6D', b'CTRL+SHIFT+LeftArrow||1'),
                             (b'\x1b[15~', b'F5||1'), ('二😀\n'.encode(), 'Packet|二😀\n|1'.encode()),
                             (b'\x1b[B\x1b[B', b'DownArrow||2')]:
            terminal('key', data, report)
        terminal('keys', b'\x1b[B\x1b[B\r', b'DownArrow:2|Enter:1')
        terminal('key', expected=b'NoName||1', resize=True)
        terminal('secret', 'sëcret\nignored'.encode(), 'sëcret'.encode())
        terminal('secret', expected=b'ac', chunks=[b'a', b'b', b'\x7f', b'c', b'\r'])
        # Also exercise ordinary binaries without leak instrumentation.
        source = root / 'plain.dark'
        source.write_text(programs['all'])
        subprocess.run([compiler, '-q', str(source), '-o', str(root / 'plain')], check=True, timeout=120)
        result = subprocess.run([root / 'plain'], input=b'plain', capture_output=True, timeout=15)
        assert (result.returncode, result.stdout, result.stderr) == (0, b'plain', b''), result
    print('Stdin stream, UTF-16 boundaries, terminal keys, resize and secret regressions passed')


if __name__ == '__main__':
    main()
