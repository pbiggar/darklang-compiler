#!/usr/bin/env python3
"""Discover and replace OCaml operators outside comments and string/char literals."""
import argparse
from bisect import bisect_right
from pathlib import Path
import re

MUTATIONS = {
    "ARITH_ADD": ("+", "-"), "ARITH_SUB": ("-", "+"),
    "ARITH_MUL": ("*", "/"), "ARITH_DIV": ("/", "*"),
    "CMP_LT": ("<", ">"), "CMP_GT": (">", "<"),
    "CMP_LTE": ("<=", ">="), "CMP_GTE": (">=", "<="),
    "CMP_NEQ": ("<>", "="), "LOGIC_AND": ("&&", "||"),
    "LOGIC_OR": ("||", "&&"),
}
OPERATORS = re.compile(r"[!$%&*+\-./:<=>?@^|~]+")
CHARACTER = re.compile(r"'(?:\\(?:[0-9]{3}|x[0-9a-fA-F]{2}|o[0-7]{3}|u\{[0-9a-fA-F]+\}|[^\n])|[^'\\\n])'")
QUOTED_STRING = re.compile(r"\{([a-z_]*)\|")
EXCLUDED = {"IRPrinting.ml", "Binary.ml", "ELF.ml", "Binary_Generation_MachO.ml",
            "Backend_Arm64_Binary_Generation_ELF.ml", "Binary_Generation_ELF_X86_64.ml",
            "Output.ml", "Platform.ml"}


def string_end(source, index):
    index += 1
    while index < len(source):
        if source[index] == "\\":
            index = min(index + 2, len(source))
        elif source[index] == '"':
            return index + 1
        else:
            index += 1
    return len(source)


def quoted_end(source, match):
    end = source.find("|" + match[1] + "}", match.end())
    return len(source) if end < 0 else end + len(match[1]) + 2


def code_only(source):
    """Keep offsets and newlines, including across nested OCaml comments."""
    output = list(source)
    index = 0
    while index < len(source):
        start = index
        if source.startswith("(*", index):
            depth = 1
            index += 2
            while index < len(source) and depth:
                if source.startswith("(*", index):
                    depth += 1
                    index += 2
                elif source.startswith("*)", index):
                    depth -= 1
                    index += 2
                elif source[index] == '"':
                    index = string_end(source, index)
                elif quoted := QUOTED_STRING.match(source, index):
                    index = quoted_end(source, quoted)
                else:
                    index += 1
        elif source[index] == '"':
            index = string_end(source, index)
        elif character := CHARACTER.match(source, index):
            index = character.end()
        elif quoted := QUOTED_STRING.match(source, index):
            index = quoted_end(source, quoted)
        else:
            index += 1
            continue
        for offset in range(start, index):
            if output[offset] != "\n":
                output[offset] = " "
    return "".join(output)


def sites(source):
    newlines = [index for index, character in enumerate(source) if character == "\n"]
    names = {operator: name for name, (operator, _) in MUTATIONS.items()}
    seen = set()
    for match in OPERATORS.finditer(code_only(source)):
        name = names.get(match[0])
        line = bisect_right(newlines, match.start()) + 1
        if name is not None and (name, line) not in seen:
            seen.add((name, line))
            yield name, line, match.start(), match.end()


def discover(root, pattern, category):
    directory = root / "src"
    if not directory.is_dir():
        raise ValueError(f"Compiler source directory is missing: {directory}")
    count = 0
    for path in sorted(directory.rglob("*.ml")):
        if path.name in EXCLUDED or "Test" in path.name or pattern not in str(path):
            continue
        for name, line, _, _ in sites(path.read_text()):
            if category == "all" or name.startswith({"arith": "ARITH_", "cmp": "CMP_", "logic": "LOGIC_"}[category]):
                print(f"{name}:{path.relative_to(root)}:{line}")
                count += 1
    if not count:
        raise ValueError("No OCaml mutation sites matched the requested selection")


def apply(name, path, line):
    source = path.read_text()
    for candidate, candidate_line, start, end in sites(source):
        if candidate == name and candidate_line == line:
            path.write_text(source[:start] + MUTATIONS[name][1] + source[end:])
            return
    raise ValueError(f"Mutation site no longer exists: {name}:{path}:{line}; rediscover sites")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    discovery = commands.add_parser("discover")
    discovery.add_argument("root", type=Path)
    discovery.add_argument("--file", default="")
    discovery.add_argument("--type", choices=("all", "arith", "cmp", "logic"), default="all")
    mutation = commands.add_parser("apply")
    mutation.add_argument("name", choices=MUTATIONS)
    mutation.add_argument("path", type=Path)
    mutation.add_argument("line", type=int)
    arguments = parser.parse_args()
    try:
        if arguments.command == "discover":
            discover(arguments.root.resolve(), arguments.file, arguments.type)
        else:
            apply(arguments.name, arguments.path, arguments.line)
    except (OSError, ValueError) as error:
        parser.exit(1, f"{error}\n")


if __name__ == "__main__":
    main()
