#!/usr/bin/env python3
"""Stress native process captures against concurrent descriptor reuse on Linux."""
import argparse
from pathlib import Path
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-dir', type=Path, required=True)
    parser.add_argument('--ocamlfind', default='ocamlfind')
    args = parser.parse_args()
    graph = args.build_dir.resolve() / 'default'
    output = ROOT / 'TestResults/ocaml-migration/process-capture-regression'
    output.mkdir(parents=True, exist_ok=True)
    for extension in ['mli', 'ml']:
        shutil.copyfile(ROOT / f'scripts/ocaml/process_capture_regression.{extension}',
                        output / f'process_capture_regression.{extension}')
    flags = ['-w', '@a-42', '-warn-error', '@a-42']
    subprocess.run([args.ocamlfind, 'ocamlopt', *flags, '-c',
                    'process_capture_regression.mli'], cwd=output, check=True)
    command = [args.ocamlfind, 'ocamlopt', '-thread', *flags, '-linkpkg',
               '-package', 'unix,threads,uutf,uucp,uunf,uuseg,zarith,yojson']
    for directory in [graph / 'lib', graph / 'tests',
                      graph / 'lib/.dark_compiler.objs/byte',
                      graph / 'tests/.runner_support.objs/byte']:
        command.extend(['-I', str(directory)])
    subprocess.run(command + [str(graph / 'lib/dark_compiler.cmxa'),
                              str(graph / 'tests/runner_support.cmxa'),
                              'process_capture_regression.ml', '-o', 'regression.exe'],
                   cwd=output, check=True)
    subprocess.run([str(output / 'regression.exe')], cwd=ROOT, check=True)


if __name__ == '__main__':
    main()
