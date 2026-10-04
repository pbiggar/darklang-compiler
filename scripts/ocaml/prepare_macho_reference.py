"""Build a migration-only full reference with controlled UUID entropy."""
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]

def prepare(destination, original):
    destination.mkdir(parents=True, exist_ok=True)
    controlled = destination / "ControlledMachO.fsx"
    with controlled.open("w") as output:
        subprocess.run([sys.executable, str(ROOT / "ocaml/migration/control_macho_uuid.py"),
                        str(ROOT / "src/DarkCompiler/backend/arm64/Binary_Generation_MachO.fs")],
                       check=True, stdout=output)
    reference = original.read_text()
    # Process observations also generate complete Mach-O images. Route their
    # entropy input through the full controlled implementation before emission.
    reference = reference.replace("Binary_Generation_MachO.createExecutableWithPools",
                                  "ControlledMachO.createExecutableWithPools")
    for line in reference.splitlines():
        if line.startswith('#r "') or line.startswith('#load "'):
            reference = reference.replace(line, line.split('"')[0] + '"' + str((original.parent / line.split('"')[1]).resolve()) + '"')
    lines = reference.splitlines(keepends=True)
    position = max(n for n, line in enumerate(lines) if line.startswith('#r "')) + 1
    lines.insert(position, '#load "' + str(controlled.resolve()) + '"\n')
    reference = ''.join(lines)
    observation = (ROOT / "scripts/ocaml/macho_observation.fsx.inc").read_text()
    reference = reference.replace("let jsonOutputOptions", observation + "\nlet jsonOutputOptions", 1)
    reference = reference.replace('| "elf-images" -> elfObservation source', '| "macho-images" -> machoObservation source\n        | "elf-images" -> elfObservation source', 1)
    result = destination / "macho_reference.fsx"
    result.write_text(reference)
    return result
