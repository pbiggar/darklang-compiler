#!/usr/bin/env python3
"""Verify the pinned static SQL corpus against its actual SQLite schema."""
import importlib.util
import json
from pathlib import Path
import re
import sqlite3
import sys


def statements(sql):
    start = 0
    for i, char in enumerate(sql):
        if char == ';' and sqlite3.complete_statement(sql[start:i + 1]):
            yield sql[start:i + 1]
            start = i + 1
    if sql[start:].strip():
        yield sql[start:]


def main():
    fixtures, extractor = map(Path, sys.argv[1:3])
    spec = importlib.util.spec_from_file_location('sqlite_corpus_extractor', extractor)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    source = '// "SELECT not_sql"\nlet sql = "SELECT 1 \\\n    AS n"\n(* "SELECT nope" (* nested *) *)\nlet x = $"SELECT {field} FROM t"'
    literals = list(module.strings(source))
    assert [s[2] for s in literals] == ['SELECT 1 AS n','SELECT {field} FROM t'], literals
    assert literals[1][3] and not literals[0][3]
    corpus = json.loads((fixtures / 'corpus.json').read_text())
    assert corpus['interpreter_revision'] == module.PIN
    connection = sqlite3.connect(':memory:')
    connection.executescript((fixtures / 'schema.sql').read_text())
    count, templates = 0, 0
    for entry in corpus['entries']:
        if entry['kind'] == 'template':
            templates += 1
            continue
        assert entry['source'] in corpus['source_sha256']
        for sql in statements(entry['sql']):
            params = {name:None for name in re.findall(r'[@:$]([A-Za-z_][A-Za-z_0-9]*)',sql)}
            try:
                connection.execute('EXPLAIN ' + sql, params)
            except sqlite3.Error as error:
                raise AssertionError((entry['source'],entry['line'],sql,str(error))) from error
            count += 1
    assert count > 200 and templates > 0
    print(f'Package SQLite corpus: {count} concrete statements prepare against the pinned schema; {templates} source templates need concrete workload cases')


if __name__ == '__main__':
    main()
