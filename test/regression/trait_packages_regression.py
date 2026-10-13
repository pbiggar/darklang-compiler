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


def location(name):
    owner, *parts = name.split(".")
    return dict(owner=owner, modules=parts[:-1], name=parts[-1])


def reference(name, hash_):
    return dict(originalName=name.split("."), resolved={"Ok": [dict(
        name={"Package": [{"Hash": [hash_]}]}, location={"Some": [location(name)]})]})


def main():
    compiler = str(Path(sys.argv[1]).resolve())
    trait = reference("Hosted.ToText", "trait-hash")
    method = reference("Hosted.Int64.ToText.toText", "method-hash")
    variable = {"TVariable": ["a"]}
    signature = dict(name="toText", typeParams=[], bounds=[],
                     parameters=[dict(name="value", typ=variable)], returnType={"TString": []})
    entities = {
        ("trait", "trait-hash"): ("Hosted.ToText", dict(typeParams=["a"], bounds=[], methods=[signature])),
        ("impl", "impl-hash"): ("Hosted.Int64.ToText", dict(trait_=trait, traitTypeArgs=[],
            self={"TInt64": []}, typeParams=[], bounds=[], methods=[["toText", method]])),
        ("function", "method-hash"): ("Hosted.Int64.ToText.toText", dict(typeParams=[], bounds=[],
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
    requests = []

    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            requests.append(self.path)
            parts = self.path.strip("/").split("/")
            payload = None
            if len(parts) == 3 and parts[1] == "find":
                for (kind, hash_), (name, _) in entities.items():
                    if kind == parts[0] and name == parts[2]:
                        payload = {"Hash": [hash_]}
            elif len(parts) == 4 and parts[1:3] == ["get", "with-location"]:
                entity = entities.get((parts[0], parts[3]))
                if entity:
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
            source.write_text('Hosted.Int64.ToText.toText 42L ++ "|" ++ Hosted.describe 42L ++ "|" ++ Hosted.describePipe 42L\n')
            program = directory / "program"
            result = subprocess.run([compiler, "-q", "--emit-result", "--package-server",
                f"http://127.0.0.1:{server.server_port}", str(source), "-o", str(program)],
                env=env, capture_output=True, text=True, timeout=120)
            assert result.returncode == 0, result.stdout + result.stderr
            output = subprocess.check_output([str(program)], text=True, timeout=10)
            assert json.loads(output) == "hosted|hosted|hosted", output
            assert "/trait/get/with-location/trait-hash" in requests, requests
            assert "/impl/get/with-location/impl-hash" in requests, requests
        server.shutdown()
    print("Hosted traits, implementations, dependencies and bounded functions execute natively")


if __name__ == "__main__":
    main()
