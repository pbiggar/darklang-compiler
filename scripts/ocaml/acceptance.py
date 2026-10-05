#!/usr/bin/env python3
"""Capture every emitted image and compilation result in disposable oracle/native copies.

Instrumentation is inserted before serialization returns, never into generated
bytes. Mach-O copies receive identical UUID entropy before generation. Production
sources and frozen inputs are untouched. Compare requests and duplicate invocation
counts independently of parallel runner ordering, then compare complete files.
"""
import argparse
from collections import Counter, defaultdict
import hashlib
import json
from pathlib import Path
import re
import shutil
import sys
import subprocess

ROOT = Path(__file__).resolve().parents[2]
ORACLE = "df9dae7e1647275f6bc9104618f20ef84a7251be"
UUID = "00112233445546778899aabbccddeeff"


def insert_wrapper(path, name, wrapper):
    text = path.read_text()
    pattern = rf"(?m)^let {name}\b"
    matches = list(re.finditer(pattern, text))
    if len(matches) != 1:
        raise ValueError(f"Expected one {name} definition in {path}")
    start = matches[0].start()
    end_match = re.search(r"(?m)^let\s", text[matches[0].end():])
    end = matches[0].end() + end_match.start() if end_match else len(text)
    block = text[start:end].replace(f"let {name}", f"let parityOriginal{name}", 1)
    path.write_text(text[:start] + block + "\n" + wrapper + "\n" + text[end:])


def native_capture(directory):
    return f'''(* Migration-only raw artifact capture; absent from production graphs. *)
let directory = {json.dumps(str(directory))}
let counter = Atomic.make 0
let record kind request binary error =
 let id = Printf.sprintf "%d-%08d" (Unix.getpid ()) (Atomic.fetch_and_add counter 1) in
 let filename = Option.map (fun bytes ->
  let name = id ^ ".bin" in
  Out_channel.with_open_bin (Filename.concat directory name) (fun output -> Out_channel.output_bytes output bytes); name) binary in
 Yojson.Basic.to_file (Filename.concat directory (id ^ ".json"))
  (`Assoc ["kind",`String kind;"request",`List (List.map (fun value -> `String value) request);
   "binary",(match filename with None -> `Null | Some name -> `String name);
   "error",(match error with None -> `Null | Some message -> `String message)])
'''


def reference_capture(directory):
    return f'''// Migration-only raw artifact capture; absent from production graphs.
module ParityCapture
let directory = {json.dumps(str(directory))}
let mutable private counter = 0
let record (kind: string) (request: string list) (binary: byte array option) (error: string option) =
    let id = sprintf "%d-%08d" System.Environment.ProcessId (System.Threading.Interlocked.Increment(&counter))
    let filename = binary |> Option.map (fun bytes ->
        let name = id + ".bin"
        System.IO.File.WriteAllBytes(System.IO.Path.Combine(directory, name), bytes)
        name)
    let entry = {{| kind = kind; request = List.toArray request;
                   binary = Option.toObj filename; error = Option.toObj error |}}
    System.IO.File.WriteAllText(System.IO.Path.Combine(directory, id + ".json"), System.Text.Json.JsonSerializer.Serialize(entry))
'''


def prepare(destination, native_only=False):
    if destination.exists() and not native_only:
        raise ValueError(f"Destination already exists: {destination}")
    destination.mkdir(parents=True, exist_ok=True)
    for side in (("native",) if native_only else ("reference", "native")):
        copy = destination / side
        if copy.exists():
            raise ValueError(f"Compilation graph already exists: {copy}")
        copy.mkdir()
        if side == "reference":
            archive = subprocess.Popen(["git", "archive", ORACLE], cwd=ROOT, stdout=subprocess.PIPE)
            subprocess.run(["tar", "-x", "-C", str(copy)], stdin=archive.stdout, check=True)
            archive.stdout.close()
            if archive.wait():
                raise ValueError("git archive failed")
        else:
            # Include current source changes while excluding ignored build/results
            # trees; acceptance validates the actual candidate, not stale HEAD.
            files = subprocess.check_output(["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],cwd=ROOT)
            for name in files.decode().split("\0"):
                source = ROOT / name
                if not name or not source.is_file():
                    continue
                output = copy / name
                output.parent.mkdir(parents=True,exist_ok=True)
                shutil.copy2(source,output)
        if side == "native":
            digest = hashlib.sha256()
            for filename in sorted(copy.rglob("*")):
                if filename.is_file():
                    digest.update(str(filename.relative_to(copy)).encode() + b"\0")
                    digest.update(str(filename.stat().st_mode & 0o777).encode() + b"\0")
                    digest.update(hashlib.sha256(filename.read_bytes()).digest())
            native_source_identity = digest.hexdigest()
        events = destination / (side + "-events")
        events.mkdir()
        if side == "reference":
            compiler = copy / "src/DarkCompiler"
            (compiler / "ParityCapture.fs").write_text(reference_capture(events))
            project = compiler / "DarkCompiler.fsproj"
            text = project.read_text().replace('<Compile Include="Crash.fs" />', '<Compile Include="Crash.fs" />\n    <Compile Include="ParityCapture.fs" />')
            project.write_text(text)
            elf = compiler / "backend/arm64/Binary_Generation_ELF.fs"
            macho = compiler / "backend/arm64/Binary_Generation_MachO.fs"
            for path, name, tag in ((elf,"serializeElf","elf"), (macho,"serializeMachO","macho")):
                insert_wrapper(path, name, f'let {name} binary =\n    let bytes = parityOriginal{name} binary\n    ParityCapture.record "{tag}" [] (Some bytes) None\n    bytes')
            text = macho.read_text()
            old = "System.Guid.NewGuid().ToByteArray()"
            if text.count(old) != 1:
                raise ValueError("Mach-O entropy site changed")
            macho.write_text(text.replace(old, 'System.Guid.Parse("00112233-4455-4677-8899-aabbccddeeff").ToByteArray()'))
            instrument_reference_request(compiler / "CompilerLibrary.fs")
            if sys.platform == "darwin":
                # Match native applicability: macOS cannot execute Linux ELFs.
                tests = copy / "src/Tests/compiler-passes/ARM64BinaryTests.fs"
                entry = '    ("execute Linux ARM64 ELF", testExecuteLinuxElf)\n'
                if tests.read_text().count(entry) != 1:
                    raise ValueError("Linux-only test registration changed")
                tests.write_text(tests.read_text().replace(entry, ""))
        else:
            compiler = copy / "ocaml/lib"
            (compiler / "ParityCapture.ml").write_text(native_capture(events))
            (compiler / "ParityCapture.mli").write_text('val record : string -> string list -> bytes option -> string option -> unit\n')
            elf = compiler / "backend/arm64/Backend_Arm64_Binary_Generation_ELF.ml"
            macho = compiler / "backend/arm64/Binary_Generation_MachO.ml"
            for path, name, tag in ((elf,"serializeElf","elf"), (macho,"serializeMachO","macho")):
                insert_wrapper(path, name, f'let {name} binary =\n let bytes = parityOriginal{name} binary in\n ParityCapture.record "{tag}" [] (Some bytes) None; bytes')
            text = macho.read_text()
            old = "let hex=HostGuid.newGuidN () in"
            if text.count(old) != 1:
                raise ValueError("Mach-O entropy site changed")
            macho.write_text(text.replace(old, f'let hex="{UUID}" in'))
            instrument_native_request(compiler / "CompilerLibrary.ml")
            # Migration probes also consume this already-controlled serializer.
            controller = copy / "ocaml/migration/control_macho_uuid.py"
            controller.write_text(controller.read_text().replace('old = "let hex=HostGuid.newGuidN () in"', f'old = \'let hex="{UUID}" in\''))
    (destination / "inputs.json").write_text(json.dumps({
        "oracle": ORACLE,
        "native_parent": subprocess.check_output(["git","rev-parse","HEAD"],cwd=ROOT,text=True).strip(),
        "native_source_sha256": native_source_identity,
        "controlled_macho_uuid": UUID,
        "controlled_source_root": "/port-acceptance/",
        "inventory_sha256": hashlib.sha256((ROOT / "ocaml/inventory.json").read_bytes()).hexdigest(),
    }, indent=2) + "\n")
    print(f"Prepared disposable compilation graphs at {destination}")


OPTION_FIELDS = [
    "DisableFreeList", "DisableANFOpt", "DisableANFConstFolding", "DisableANFConstProp",
    "DisableANFCopyProp", "DisableANFDCE", "DisableANFStrengthReduction", "DisableInlining",
    "DisableTCO", "DisableMIROpt", "DisableMIRSCCP", "DisableMIRCSE", "DisableMIRDCE",
    "DisableMIRLICM", "DisableLIROpt", "DisableLIRPeephole", "DisableFunctionTreeShaking",
    "EnableCoverage", "EnableLeakCheck", "DumpANF", "DumpMIR", "DumpLIR", "DumpIRSummary",
]


def instrument_reference_request(path):
    prefix = json.dumps(str(path.parents[2]) + "/")
    fields = "; ".join("request.Options." + name for name in OPTION_FIELDS)
    text = path.read_text().replace("let compile (request: CompileRequest)", "let parityOriginalCompile (request: CompileRequest)", 1)
    path.write_text(text + f'''
let private parityHex (value: string) = value |> Seq.map (fun c -> sprintf "%04x" (int c)) |> String.concat ""
let compile (request: CompileRequest) =
    let prefix = {prefix}
    let sources = request.Sources |> AST.NonEmptyList.map (fun (source: SourceUnit) ->
        let name = if source.Name.StartsWith(prefix, StringComparison.Ordinal) then "/port-acceptance/" + source.Name.Substring(prefix.Length) else source.Name
        {{ source with Name = name }})
    let request = {{ request with Sources = sources }}
    let report = parityOriginalCompile request
    let target = match report.Target with Platform.LinuxX86_64 -> "linux-x86_64" | Platform.ARM64Backend Platform.LinuxARM64 -> "linux-arm64" | Platform.ARM64Backend Platform.MacOSARM64 -> "macos-arm64"
    let options = [{fields}] |> List.map (fun value -> if value then "1" else "0")
    let mode = match request.Mode with FullProgram -> "program" | TestExpression -> "expression"
    let layout = match request.Options.NativeLayoutProbe with NoNativeLayoutProbe -> "none" | RootWord -> "root" | TupleWords -> "tuple"
    let preamble = match request.Context with StdlibOnly _ -> [] | StdlibWithPreamble (_, p) -> p.SymbolicFunctions |> List.map (fun f -> parityHex f.Name)
    let sources = request.Sources.Head :: request.Sources.Tail |> List.collect (fun source ->
        let purpose = match source.Purpose with NameSyntax.SourceUnitPurpose.Executable -> "executable" | NameSyntax.SourceUnitPurpose.Library -> "library" | NameSyntax.SourceUnitPurpose.Package -> "package"
        [parityHex source.Name; purpose; parityHex source.Source])
    let key = [target; mode; layout; (if request.AllowInternal then "1" else "0"); string request.Verbosity; parityHex (Option.defaultValue "" request.Options.DumpFunction)] @ options @ ["preamble"] @ preamble @ ["sources"] @ sources
    match report.Result with
    | Ok bytes -> ParityCapture.record "compile" key (Some bytes) None
    | Error message -> ParityCapture.record "compile" key None (Some (parityHex message))
    report
''')


def instrument_native_request(path):
    prefix = json.dumps(str(path.parents[2]) + "/")
    fields = "; ".join("request.X.options.O." + name[0].lower() + name[1:] for name in OPTION_FIELDS)
    path.write_text(path.read_text() + f'''
let parityHex value = HostText.utf16Units value |> Array.to_list |> List.map (Printf.sprintf "%04x") |> String.concat ""
let compile (request:X.compileRequest) =
 let prefix = {prefix} in
 let sources = NonEmptyList.map (fun (source:X.sourceUnit) ->
  let name = if String.starts_with ~prefix source.X.name then "/port-acceptance/" ^ String.sub source.X.name (String.length prefix) (String.length source.X.name-String.length prefix) else source.X.name in
  {{source with X.name=name}}) request.X.sources in
 let request = {{request with X.sources=sources}} in
 let report = compile request in
 let target = match report.O.target with Platform.LinuxX86_64 -> "linux-x86_64" | Platform.ARM64Backend Platform.LinuxARM64 -> "linux-arm64" | Platform.ARM64Backend Platform.MacOSARM64 -> "macos-arm64" in
 let options = [{fields}] |> List.map (fun value -> if value then "1" else "0") in
 let mode = match request.X.mode with O.FullProgram -> "program" | O.TestExpression -> "expression" in
 let layout = match request.X.options.O.nativeLayoutProbe with O.NoNativeLayoutProbe -> "none" | O.RootWord -> "root" | O.TupleWords -> "tuple" in
 let preamble = match request.X.context with X.StdlibOnly _ -> [] | X.StdlibWithPreamble (_, p) -> List.map (fun (f:LIR.functionDef) -> parityHex f.LIR.name) p.X.symbolicFunctions in
 let sources = NonEmptyList.toList request.X.sources |> List.concat_map (fun (source:X.sourceUnit) ->
  let purpose = match source.X.purpose with NameSyntax.SourceUnitPurpose.Executable -> "executable" | NameSyntax.SourceUnitPurpose.Library -> "library" | NameSyntax.SourceUnitPurpose.Package -> "package" in
  [parityHex source.X.name; purpose; parityHex source.X.source]) in
 let key = [target;mode;layout;(if request.X.allowInternal then "1" else "0");string_of_int request.X.verbosity;parityHex (Option.value ~default:"" request.X.options.O.dumpFunction)] @ options @ ["preamble"] @ preamble @ ["sources"] @ sources in
 (match report.O.result with Ok bytes -> ParityCapture.record "compile" key (Some bytes) None | Error message -> ParityCapture.record "compile" key None (Some (parityHex message))); report
''')


def load_events(directory):
    groups = defaultdict(list)
    for path in sorted(directory.glob("*.json")):
        event = json.loads(path.read_text())
        if set(event) != {"kind", "request", "binary", "error"}:
            raise ValueError(f"Malformed event: {path}")
        key = json.dumps([event["kind"], event["request"]], ensure_ascii=True)
        binary = directory / event["binary"] if event["binary"] else None
        digest = hashlib.sha256(binary.read_bytes()).hexdigest() if binary else None
        groups[key].append((digest, event["error"], binary, path))
    if not groups:
        raise ValueError(f"No captured invocations in {directory}")
    return groups


def compare(destination):
    left = load_events(destination / "reference-events")
    right = load_events(destination / "native-events")
    failures = []
    totals = Counter()
    for key in sorted(left.keys() | right.keys()):
        a, b = left.get(key, []), right.get(key, [])
        counts_a = Counter((row[0],row[1]) for row in a)
        counts_b = Counter((row[0],row[1]) for row in b)
        kind, request = json.loads(key)
        totals[kind] += len(a)
        if counts_a != counts_b:
            failure = {"kind":kind,"request":request,"reference_count":len(a),"native_count":len(b),
                       "missing":list((counts_a-counts_b).items()),"extra":list((counts_b-counts_a).items())}
            missing = [row for row in a if (row[0],row[1]) in counts_a-counts_b]
            extra = [row for row in b if (row[0],row[1]) in counts_b-counts_a]
            if missing and extra and missing[0][2] and extra[0][2]:
                expected, actual = missing[0][2].read_bytes(), extra[0][2].read_bytes()
                failure.update(reference_file=str(missing[0][2]),native_file=str(extra[0][2]),
                    first_difference=next((n for n,(x,y) in enumerate(zip(expected,actual)) if x != y),min(len(expected),len(actual))))
            failures.append(failure)
        else:
            # Hashes group invocations; file equality independently proves all bytes.
            originals = {row[0]:row[2] for row in a if row[2]}
            for digest, _, binary, _ in b:
                if binary and originals[digest].read_bytes() != binary.read_bytes():
                    raise ValueError("SHA256 collision: complete executable bytes differ")
    report = {"invocations":dict(totals),"request_groups":len(left),"failures":failures}
    (destination / "comparison.json").write_text(json.dumps(report,indent=2)+"\n")
    print(f"Captured invocations: {dict(totals)}; mismatching groups: {len(failures)}")
    print(f"Full comparison: {destination / 'comparison.json'}")
    return bool(failures)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command",choices=("prepare","prepare-native","compare"))
    parser.add_argument("--directory",type=Path,required=True)
    args = parser.parse_args()
    destination = args.directory.resolve()
    try:
        if args.command in ("prepare", "prepare-native"):
            prepare(destination, args.command == "prepare-native")
            return 0
        return int(compare(destination))
    except (OSError,ValueError,subprocess.CalledProcessError) as error:
        parser.exit(1,str(error)+"\n")


if __name__ == "__main__":
    raise SystemExit(main())
