"""Route the full migration emitter through UUID entropy supplied before generation."""
from pathlib import Path
import sys

source = Path(sys.argv[1]).read_text()
if sys.argv[1].endswith(".ml"):
    source = "open Dark_compiler\n" + source.replace("Binary_Generation_MachO.", "ControlledMachO.")
else:
    source = source.replace("module ARM64_Emit", "module ControlledEmit", 1).replace("Binary_Generation_MachO.", "ControlledMachO.")
print(source, end="")
