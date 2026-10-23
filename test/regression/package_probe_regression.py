"""Probe mutually recursive imported types together and retain real missing dependencies."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile


def main():
    probe = Path(sys.argv[1]).resolve()
    with tempfile.TemporaryDirectory(prefix="package-probe-") as temporary:
        root = Path(temporary)
        for directory in ("porting", "upstream/packages"):
            (root / directory).mkdir(parents=True, exist_ok=True)
        (root / "porting/compiler-reasons.json").write_text(
            json.dumps({"missing-implementation": "Unvalidated test declaration"}))
        (root / "upstream/sources.list").write_text("packages/recursive.dark\n")
        (root / "library-sources.list").write_text("")
        (root / "upstream/packages/recursive.dark").write_text("""module Probe
// @compiler blocked: missing-implementation
type Left = | Leaf | ToRight of Right
// @compiler blocked: missing-implementation
type Right = | Tip | ToLeft of Left
// @compiler blocked: missing-implementation
type NeedsAbsent = { missing: Unavailable }
// @compiler blocked: missing-implementation
let isLeaf (value: Left) : Bool =
  match value with
  | Leaf -> true
  | ToRight _ -> false
""")
        manifest, report = root / "manifest.json", root / "report.json"
        manifest.write_text(json.dumps(["upstream/packages/recursive.dark"]))
        subprocess.run([str(probe), "--packages", str(root), str(manifest),
                        str(report)], check=True, timeout=60)
        rows = {row["name"]: row for row in json.loads(report.read_text())}
        assert {name for name, row in rows.items() if row["status"] == "checked"} == {
            "Probe.Left", "Probe.Right", "Probe.isLeaf"}, rows
        blocked = rows["Probe.NeedsAbsent"]
        assert blocked["status"] == "blocked" and "Unavailable" in blocked["diagnostic"], blocked
    print("Package probe checks recursive type groups and records real missing dependencies")


if __name__ == "__main__":
    main()
