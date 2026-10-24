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
            json.dumps({"missing-implementation": "Unvalidated test declaration",
                        "compiler-gap": "Previously inspected limitation"}))
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
        source = root / "upstream/packages/recursive.dark"
        source.write_text(source.read_text() + """
// @compiler blocked: compiler-gap
let supportedDespiteOldReason (value: Bool) : Bool = value
// @compiler blocked: compiler-gap
trait ProbeMarker<'a> =
  // @compiler blocked: compiler-gap
  let mark (value: 'a) : Bool
// @compiler blocked: compiler-gap
impl ProbeMarker for Bool =
  // @compiler blocked: compiler-gap
  let mark (value: Bool) : Bool = value
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
        subprocess.run([str(probe), "--all-blocked", str(root), str(manifest),
                        str(report)], check=True, timeout=60)
        all_rows = {row["name"]: row for row in json.loads(report.read_text())}
        assert "Probe.supportedDespiteOldReason" not in rows, rows
        assert all_rows["Probe.supportedDespiteOldReason"]["status"] == "checked", all_rows
        assert all_rows["Probe.NeedsAbsent"]["status"] == "blocked", all_rows
        for kind in ("trait", "implementation"):
            matching = [row for row in all_rows.values() if row["kind"] == kind]
            assert len(matching) == 1 and matching[0]["status"] == "checked", all_rows
    print("Package probe checks recursive groups, all blocked reasons, and real dependencies")


if __name__ == "__main__":
    main()
