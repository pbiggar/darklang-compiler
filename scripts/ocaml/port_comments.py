#!/usr/bin/env python3
"""Preserve reference explanations next to the translated declarations."""
import argparse
import json
import re
import subprocess
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DECL = re.compile(r"^(?:let(?: rec)?(?: internal| private| inline| mutable)*|and|type(?: internal| private)?)\s+([A-Za-z][A-Za-z0-9_']*)")
NATIVE = re.compile(r"^[ \t]*(?:let(?: rec)?|and|type)\s+(?:(?:\([^\n)]+\)|'[A-Za-z0-9_]+)\s+)?([A-Za-z][A-Za-z0-9_']*)", re.M)


def line_comments(text):
    """Scan comments without mistaking quoted // or nested block text for code."""
    i, line, quoted, block = 0, 0, None, 0
    while i < len(text):
        if text[i] == '\n':
            line += 1
        if block:
            if text.startswith('(*', i):
                block += 1
                i += 2
            elif text.startswith('*)', i):
                block -= 1
                i += 2
            else:
                i += 1
        elif quoted:
            if quoted == '"""':
                if text.startswith(quoted, i):
                    quoted = None
                    i += 3
                else:
                    i += 1
            elif quoted == '@"':
                if text.startswith('""', i):
                    i += 2
                elif text[i] == '"':
                    quoted = None
                    i += 1
                else:
                    i += 1
            elif text[i] == '\\':
                i += 2
            elif text[i] == quoted:
                quoted = None
                i += 1
            else:
                i += 1
        elif text.startswith('//', i):
            end = text.find('\n', i)
            if end == -1:
                end = len(text)
            start = i + (3 if text.startswith('///', i) else 2)
            yield line, text[start:end].lstrip()
            i = end
        elif text.startswith('(*', i):
            block, i = 1, i + 2
        elif text.startswith('"""', i):
            quoted, i = '"""', i + 3
        elif text.startswith('@"', i):
            quoted, i = '@"', i + 2
        elif text[i] == '"':
            quoted, i = '"', i + 1
        elif text[i] == "'" and re.match(r"'(?:\\(?:u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8}|.|[0-9]{3})|[^'\n])'", text[i:]):
            quoted, i = "'", i + 1
        else:
            i += 1


def ocaml_comments(text):
    """Read actual comment bodies; code strings are not comment coverage."""
    token = re.compile(r'"(?:\\.|[^"\\])*"|\{(?P<raw>[a-z_]*)\|.*?\|(?P=raw)\}|\(\*|\*\)', re.S)
    depth, start = 0, 0
    for match in token.finditer(text):
        if match[0] == '(*':
            if depth == 0:
                start = match.end()
            depth += 1
        elif match[0] == '*)' and depth:
            depth -= 1
            if depth == 0:
                yield text[start:match.start()]


def safe(text):
    # OCaml parses string literals inside comments. Balance unmatched quotes;
    # separate nested delimiters used as examples rather than comment syntax.
    text = text.replace('(*', '( *').replace('*)', '* )')
    return text.replace('"', "'") if text.count('"') % 2 else text


def compact(text):
    return re.sub(r'\s+', '', text)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    manifest = json.loads((ROOT / 'ocaml/inventory.json').read_text())
    contents = {p: p.read_text() for folder in ['ocaml/lib', 'ocaml/tests'] for p in (ROOT / folder).rglob('*.ml')}
    comment_text = {path: '\n'.join(ocaml_comments(text)) for path, text in contents.items()}
    definitions = defaultdict(list)
    for path, text in contents.items():
        for match in NATIVE.finditer(text):
            definitions[match[1]].append((path, match.start()))
    insertions = defaultdict(lambda: defaultdict(list))
    missing = []
    for entry in manifest['entries']:
        if entry['kind'] not in ['compiler', 'test-source']:
            continue
        owner = ROOT / entry['owner']
        if owner not in contents:
            continue
        source_path = ROOT / entry['source']
        # Retired implementation sources remain the unchanged historical oracle.
        source = source_path.read_text() if source_path.is_file() else subprocess.check_output(
            ['git', 'show', manifest['oracle'] + ':' + entry['source']], cwd=ROOT, text=True)
        lines = source.splitlines()
        declarations = [(i, match[1]) for i, line in enumerate(lines) if (match := DECL.match(line))]
        for line, comment in line_comments(source):
            if not comment.strip() or re.fullmatch(r'[-=]+', comment):
                continue
            previous = [name for at, name in declarations if at <= line]
            following = [(at, name) for at, name in declarations if at > line]
            anchor = previous[-1] if previous else None
            # Documentation preceding a declaration belongs to that declaration.
            if following and all(not text.strip() or text.lstrip().startswith('//') for text in lines[line:following[0][0]]):
                anchor = following[0][1]
            native_name = anchor[0].lower() + anchor[1:] if anchor else None
            targets = definitions.get(anchor, []) or definitions.get(native_name, [])
            local = [target for target in targets if target[0] == owner]
            if local:
                target = local[0]
            else:
                # A common name in another component is not this declaration.
                target = owner, 0
            path, position = target
            if compact(safe(comment)) in compact(comment_text[path]):
                continue
            if comment not in insertions[path][position]:
                insertions[path][position].append(comment)
                missing.append((entry['source'], line + 1, str(path.relative_to(ROOT)), anchor))
    if args.check:
        print(f'Reference comment coverage: {len(missing)} missing comment lines')
        for item in missing[:10]:
            print(item)
        return bool(missing)
    for path, positions in insertions.items():
        text = contents[path]
        for position in sorted(positions, reverse=True):
            comments = '\n'.join('   ' + safe(comment) for comment in positions[position])
            text = text[:position] + '(*\n' + comments + '\n*)\n' + text[position:]
        path.write_text(text)
    output = ROOT / 'TestResults/ocaml-migration/comment-placement.json'
    output.write_text(json.dumps(missing, indent=2))
    print(f'Preserved {len(missing)} reference comment lines across {len(insertions)} native components')
    return False

if __name__ == '__main__':
    raise SystemExit(main())
