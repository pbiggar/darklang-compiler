#!/usr/bin/env python3
"""Exercise manifest schema and package URL policies through the CLI."""
import json
import pathlib
import subprocess
import sys
import tempfile


def main():
    compiler = str(pathlib.Path(sys.argv[1]).resolve())
    cases = [
        (None, "must contain a JSON array"),
        ([], "must contain at least one item"),
        ([None], "item 1 must be an object"),
        ([{"kind": "source", "name": "example", "source": 42, "output": "out"}],
         "item 1: source must be a non-empty string"),
        ([{"kind": "source", "name": "example", "source": "input.dark"}],
         "item 1: output must be a non-empty string"),
    ]
    with tempfile.TemporaryDirectory(prefix="dark-manifest-") as directory:
        manifest = pathlib.Path(directory) / "manifest.json"
        for value, expected in cases:
            manifest.write_text(json.dumps(value), encoding="utf-8")
            result = subprocess.run(
                [compiler, "--batch", "--quiet", "--manifest", str(manifest)],
                capture_output=True, text=True, timeout=30, check=False)
            assert result.returncode != 0, (value, result.stdout)
            assert expected in result.stderr, (value, expected, result.stderr)
    for url in ["http://", "https:example.com", "ftp://example.com",
                "http://example.com:bad", "http://example.com:80x",
                "http://example.com:65536"]:
        result = subprocess.run(
            [compiler, "--package-server=" + url, "--help"],
            capture_output=True, text=True, timeout=30, check=False)
        assert result.returncode != 0, (url, "accepted invalid HTTP URL")
        assert "absolute HTTP(S) URL" in result.stdout + result.stderr, (url, result)
    for url in ["https://EXAMPLE.com:443", "http://[::1]:8080/packages"]:
        result = subprocess.run(
            [compiler, "--package-server=" + url, "--help"],
            capture_output=True, text=True, timeout=30, check=False)
        assert result.returncode == 0, (url, result.stdout, result.stderr)
    print("CLI manifest schema and HTTP URL regression checks passed")


if __name__ == "__main__":
    main()
