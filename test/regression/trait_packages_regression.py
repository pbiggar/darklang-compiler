#!/usr/bin/env python3
"""Hosted trait closure, bounded package functions and native static dispatch."""
import http.server
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
import urllib.parse


def location(name):
    owner, *parts = name.split(".")
    return dict(owner=owner, modules=parts[:-1], name=parts[-1])


def reference(name, hash_):
    return dict(originalName=name.split("."), resolved={"Ok": [dict(
        name={"Package": [{"Hash": [hash_]}]}, location={"Some": [location(name)]})]})


def main():
    compiler = str(Path(sys.argv[1]).resolve())
    trait = reference("Hosted.ToText", "trait-hash")
    method = reference("Hosted.externalText", "method-hash")
    variable = {"TVariable": ["a"]}
    signature = dict(name="toText", typeParams=[], bounds=[],
                     parameters=[dict(name="value", typ=variable)], returnType={"TString": []})
    entities = {
        ("trait", "trait-hash"): ("Hosted.ToText", dict(typeParams=["a"], bounds=[], methods=[signature])),
        ("impl", "impl-hash"): ("Hosted.Int64.ToText", dict(trait_=trait, traitTypeArgs=[],
            self={"TInt64": []}, typeParams=[], bounds=[], methods=[["toText", method]])),
        ("function", "method-hash"): ("Hosted.externalText", dict(typeParams=[], bounds=[],
            parameters=[dict(name="value", typ={"TInt64": []})], returnType={"TString": []},
            body={"EString": [1, [{"StringText": ["hosted"]}]]})),
        ("function", "describe-hash"): ("Hosted.describe", dict(typeParams=["a"],
            bounds=[dict(param="a", trait_=dict(trait_=trait, typeArgs=[]))],
            parameters=[dict(name="value", typ=variable)], returnType={"TString": []},
            body={"EApply": [1, {"EFnName": [2, dict(originalName=["Hosted", "ToText", "toText"],
                resolved={"Error": ["unresolved"]}), []]}, [], [{"EArg": [3, 0]}]]})),
    }
    entities[("function", "pipe-hash")] = ("Hosted.describePipe", dict(typeParams=["a"],
        bounds=[dict(param="a", trait_=dict(trait_=trait, typeArgs=[]))],
        parameters=[dict(name="value", typ=variable)], returnType={"TString": []},
        body={"EPipe": [1, {"EArg": [2, 0]}, [{"EPipeFnCall": [3,
            dict(originalName=["Hosted", "ToText", "toText"], resolved={"Error": ["unresolved"]}),
            [], [], []]}]]}))
    entities[("function", "stdlib-hash")] = ("Hosted.stdlibDescribe", dict(typeParams=["a"],
        bounds=[dict(param="a", trait_=dict(trait_=reference("Darklang.Stdlib.ToString", "stdlib-trait-hash"), typeArgs=[]))],
        parameters=[dict(name="value", typ=variable)], returnType={"TString": []},
        body={"EApply": [1, {"EFnName": [2, dict(originalName=["Darklang", "Stdlib", "ToString", "toString"],
            resolved={"Error": ["unresolved"]}), []]}, [], [{"EArg": [3, 0]}]]}))
    # Ceilings affect content identity even though trait ceiling checking is deferred.
    ceiling_signature = dict(signature, permissionCeiling={"Some": [[]]})
    entities[("trait", "ceiling-trait-hash")] = ("Hosted.CeilingText", dict(
        typeParams=["a"], bounds=[], methods=[ceiling_signature]))
    requests = []
    for (_, hash_), (_, body) in entities.items():
        body["hash"] = {"Hash": [hash_]}

    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            requests.append(self.path)
            parts = self.path.strip("/").split("/")
            payload = None
            search = urllib.parse.urlparse(self.path)
            query = urllib.parse.parse_qs(search.query)
            if search.path == "/search":
                assert query.get("entityTypes") == ["module"], query
                module = query["modules"][0].split(".")
            else:
                module = parts[0].split(".")
            if self.path == "/Hosted.Int64":
                # A nominal leaf can shadow the implementation's parent module.
                payload = {"hash": {"Hash": ["shadowed-type"]}}
            elif self.path == "/Hosted.Int64.ToText":
                payload = entities[("impl", "impl-hash")][1] if ("impl", "impl-hash") in entities else None
            elif (len(parts) == 1 and parts[0].startswith("Hosted")) or search.path == "/search":
                payload = dict(submodules=[], types=[], values=[], fns=[], traits=[], impls=[], tests=[])
                fields = {"function": "fns", "trait": "traits", "impl": "impls"}
                for (kind, _), (name, body) in entities.items():
                    parent = name.split(".")[:-1]
                    if parent == module:
                        payload[fields[kind]].append(dict(entity=body, location=location(name)))
                    elif parent[:len(module)] == module:
                        child = parent[:len(module) + 1]
                        if child not in payload["submodules"]:
                            payload["submodules"].append(child)
                if search.path == "/search":
                    for field in fields.values():
                        payload[field] = []
            elif len(parts) == 3 and parts[1] == "find":
                for (kind, hash_), (name, _) in entities.items():
                    if kind not in ("trait", "impl") and kind == parts[0] and name == parts[2]:
                        payload = {"Hash": [hash_]}
            elif len(parts) == 4 and parts[1:3] == ["get", "with-location"]:
                entity = entities.get((parts[0], parts[3]))
                if entity and parts[0] not in ("trait", "impl"):
                    name, body = entity
                    payload = dict(entity=body, location=location(name))
            self.send_response(200 if payload else 404)
            self.end_headers()
            self.wfile.write(json.dumps(payload).encode())

        def log_message(self, *_):
            pass

    with http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler) as server:
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        with tempfile.TemporaryDirectory(prefix="trait-packages-") as temporary:
            directory = Path(temporary)
            env = dict(os.environ, XDG_DATA_HOME=temporary)
            source = directory / "input.dark"
            source.write_text('Hosted.describe 42L ++ "|" ++ Hosted.describePipe 42L ++ "|" ++ Hosted.stdlibDescribe 42L\n')
            program = directory / "program"
            result = subprocess.run([compiler, "-q", "--emit-result", "--package-server",
                f"http://127.0.0.1:{server.server_port}", str(source), "-o", str(program)],
                env=env, capture_output=True, text=True, timeout=120)
            assert result.returncode == 0, result.stdout + result.stderr + repr(requests)
            output = subprocess.check_output([str(program)], text=True, timeout=10)
            assert json.loads(output) == "hosted|hosted|42", output
            assert "/Hosted" in requests and "/Hosted.Int64" in requests, requests
            assert not any("stdlib-trait-hash" in request for request in requests), requests
            source.write_text('Hosted.CeilingText.toText 42L\n')
            ceiling = subprocess.run([compiler, "-q", "--emit-result", "--package-server",
                f"http://127.0.0.1:{server.server_port}", str(source), "-o", str(program)],
                env=env, capture_output=True, text=True, timeout=120)
            assert ceiling.returncode != 0, ceiling.stdout + ceiling.stderr
            assert "No implementation of trait" in ceiling.stdout + ceiling.stderr, ceiling.stdout + ceiling.stderr
            source.write_text('Hosted.ToText.toText 42L\n')
            direct = subprocess.run([compiler, "-q", "--emit-result", "--package-server",
                f"http://127.0.0.1:{server.server_port}", str(source), "-o", str(program)],
                env=env, capture_output=True, text=True, timeout=120)
            assert direct.returncode == 0, direct.stdout + direct.stderr
            assert json.loads(subprocess.check_output([str(program)], text=True, timeout=10)) == "hosted"
            # Imported equal candidates must be diagnosed, never newest-wins.
            key = ("impl", "rival-hash")
            rival = dict(entities[("impl", "impl-hash")][1])
            rival["hash"] = {"Hash": ["rival-hash"]}
            entities[key] = ("Hosted.Rival", rival)
            ambiguous = subprocess.run([compiler, "-q", "--emit-result", "--package-server",
                f"http://127.0.0.1:{server.server_port}", str(source), "-o", str(program)],
                env=env, capture_output=True, text=True, timeout=120)
            assert ambiguous.returncode != 0, ambiguous.stdout + ambiguous.stderr
            assert "Ambiguous implementation" in ambiguous.stdout + ambiguous.stderr, ambiguous.stdout + ambiguous.stderr
            del entities[key]
            original_impl = entities.pop(("impl", "impl-hash"))
            missing = subprocess.run([compiler, "-q", "--emit-result", "--package-server",
                f"http://127.0.0.1:{server.server_port}", str(source), "-o", str(program)],
                env=env, capture_output=True, text=True, timeout=120)
            assert missing.returncode != 0, missing.stdout + missing.stderr
            assert "No implementation of trait" in missing.stdout + missing.stderr, missing.stdout + missing.stderr
            entities[("impl", "impl-hash")] = original_impl
            # Refresh the mutable inventory, then prove it is reusable offline.
            refreshed = subprocess.run([compiler, "-q", "--emit-result", "--package-server",
                f"http://127.0.0.1:{server.server_port}", str(source), "-o", str(program)],
                env=env, capture_output=True, text=True, timeout=120)
            assert refreshed.returncode == 0, refreshed.stdout + refreshed.stderr
            server.shutdown()
            server.server_close()
            offline = subprocess.run([compiler, "-q", "--emit-result", "--package-server",
                f"http://127.0.0.1:{server.server_port}", str(source), "-o", str(program)],
                env=env, capture_output=True, text=True, timeout=120)
            assert offline.returncode == 0, offline.stdout + offline.stderr
            assert json.loads(subprocess.check_output([str(program)], text=True, timeout=10)) == "hosted"

        server.shutdown()
    print("Hosted traits, implementations, dependencies and bounded functions execute natively")


if __name__ == "__main__":
    main()
