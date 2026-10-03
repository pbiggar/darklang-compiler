"""Generate exhaustive, typed migration encoders from the complete IR interfaces.

This handles the closed record/union grammar used by the memory and ANF data
models. Unknown field types fail generation, so new IR cases cannot silently
disappear from observations. Production IR definitions are never modified.
"""
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCHEMAS = [("MemoryModel", "MemoryModel", "memory/MemoryModel.mli"), ("ANF", "InstrumentedANF", "ir/anf/ANF.mli")]


def product_parts(text):
    parts, depth, start = [], 0, 0
    for index, char in enumerate(text):
        if char == '(':
            depth += 1
        elif char == ')':
            depth -= 1
        elif char == '*' and depth == 0:
            parts.append(text[start:index].strip())
            start = index + 1
    parts.append(text[start:].strip())
    return parts


def definitions(path):
    text = path.read_text()
    text = re.sub(r'\(\*.*?\*\)', '', text, flags=re.S)
    text = re.sub(r'\[@@@.*?\]', '', text)
    matches = list(re.finditer(r'^(?:type|and) (\w+)(?:\s*=\s*(.*))?$', text, re.M))
    result = []
    for index, match in enumerate(matches):
        end = matches[index + 1].start() if index + 1 < len(matches) else len(text)
        body = (match[2] or '') + text[match.end():end]
        body = re.split(r'\n(?:val |module )', body)[0].strip()
        result.append((match[1], body))
    return result


def emit():
    entries = [(source, module, name, body) for source, module, path in SCHEMAS for name, body in definitions(ROOT / 'lib' / path)]
    known = {(source, name): source[0].lower() + source[1:] + '_' + name for source, _, name, _ in entries}
    aliases = {"int": "int32", "int32": "int32Native", "int64": "int64", "string": "SemanticJson.string", "bool": "boolean", "float": "float64", "AST.semanticType": "SemanticAST.semanticType", "AST.functionId": "functionId", "MemoryModel.IntSet.t": "integerSet", "IntSet.t": "integerSet", "ExprIdMap.t": "coverageMap"}

    def encoder(source, typ, value):
        typ = typ.strip()
        if typ.startswith('(') and typ.endswith(')'):
            typ = typ[1:-1]
        if len(product_parts(typ)) > 1:
            parts = product_parts(typ)
            values = [f'part{i}' for i in range(len(parts))]
            return '(let ' + ', '.join(values) + ' = ' + value + ' in tuple [' + '; '.join(encoder(source, p, v) for p, v in zip(parts, values)) + '])'
        if typ.endswith(' StringOrder.Map.t'):
            inner = typ[:-len(' StringOrder.Map.t')]
            return 'stringMap (fun item -> ' + encoder(source, inner, 'item') + ') ' + value
        for suffix, constructor in [(' list', 'list'), (' option', 'option'), (' array', 'array')]:
            if typ.endswith(suffix):
                inner = typ[:-len(suffix)]
                return constructor + ' (fun item -> ' + encoder(source, inner, 'item') + ') ' + value
        if typ.startswith('string ') and typ.endswith('ExprIdMap.t'):
            return 'coverageMap ' + value
        fn = aliases.get(typ)
        if fn:
            return fn + ' ' + value
        if '.' in typ:
            namespace, typ = typ.rsplit('.', 1)
        else:
            namespace = source
        fn = known.get((namespace, typ))
        if fn is None:
            raise ValueError(f'Unknown {source} field type: {typ}')
        return fn + ' ' + value

    interface = '(* Exhaustive typed memory/ANF migration encoders. *)\nopen Dark_compiler\n'
    prelude = '''(* Exhaustive typed memory/ANF migration encoders. *)
open Dark_compiler
let scalar kind value = `Assoc ["kind", `String kind; "value", `String value]
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let array encode values = `List (Array.to_list (Array.map encode values))
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let int32 value = scalar "int32" (string_of_int value)
let int32Native value = scalar "int32" (Int32.to_string value)
let int64 value = scalar "int64" (Int64.to_string value)
let boolean value = `Bool value
let float64 value = scalar "float64" (Printf.sprintf "%016Lx" (Int64.bits_of_float value))
let unsigned value = Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value)
let functionId value = SemanticJson.union "FunctionId" "FunctionId" [scalar "uint64" (unsigned (AST.functionIdValue value))]
let integerSet values = `Assoc ["set", list int32 (MemoryModel.IntSet.elements values)]
let coverageMap values = `Assoc ["map", list (fun (key, value) -> tuple [int32 key; SemanticJson.string value]) (InstrumentedANF.ExprIdMap.bindings values)]
let stringMap encode values = `Assoc ["map", list (fun (key, value) -> tuple [SemanticJson.string key; encode value]) (StringOrder.Map.bindings values)]
'''
    bodies = []
    for source, module, name, body in entries:
        fn = known[source, name]
        interface += f'val {fn} : {module}.{name} -> Yojson.Basic.t\n'
        fsharp_name = {'functionDef': 'Function', 'typedParam': 'TypedParam'}.get(name, name[0].upper() + name[1:])
        if name == 'typeMap':
            expr = '(let first, types = InstrumentedANF.observationParts value in SemanticJson.record "TypeMap" ["FirstId", int32 first; "Types", array (option SemanticAST.semanticType) types])'
        elif body.startswith('{'):
            fields = [field.strip().split(':', 1) for field in body.strip('{} \n').split(';') if field.strip()]
            output = []
            for field, typ in fields:
                field = field.strip()
                fsharp_field = 'Type' if name == 'typedParam' and field == 'typ' else field[0].upper() + field[1:]
                output.append('"' + fsharp_field + '", ' + encoder(source, typ, f'value.{module}.{field}'))
            expr = 'SemanticJson.record "' + fsharp_name + '" [' + '; '.join(output) + ']'
        elif '|' in body or re.match(r'^[A-Z]', body):
            branches = []
            for case in body.split('|'):
                case = case.strip()
                if not case:
                    continue
                constructor, _, payload = case.partition(' of ')
                parts = product_parts(payload) if payload else []
                values = [f'field{i}' for i in range(len(parts))]
                pattern = module + '.' + constructor + (' (' + ', '.join(values) + ')' if values else '')
                encodings = [encoder(source, typ, value) for typ, value in zip(parts, values)]
                if name == 'sizedInt':
                    kind = constructor.lower()
                    formatter = 'Int32.to_string' if constructor == 'Int32' else 'unsigned' if constructor == 'UInt64' else 'Int64.to_string' if constructor in ['Int64', 'UInt32'] else 'string_of_int'
                    encodings = [f'scalar "{kind}" ({formatter} field0)']
                branches.append(' | ' + pattern + ' -> SemanticJson.union "' + fsharp_name + '" "' + constructor + '" [' + '; '.join(encodings) + ']')
            expr = 'match value with\n' + '\n'.join(branches)
        else:
            expr = encoder(source, body, 'value')
        bodies.append(f'{"let rec" if not bodies else "and"} {fn} (value : {module}.{name}) = {expr}')
    # Write the complete interface before its implementation.
    body_text = '\n'.join(bodies)
    for helper in ['int32Native', 'int64']:
        if not re.search(r'\b' + helper + r' [a-z(]', body_text):
            prelude = re.sub(r'^let ' + helper + r' .*\n', '', prelude, flags=re.M)
    (ROOT / 'migration/SemanticANF.mli').write_text(interface)
    (ROOT / 'migration/SemanticANF.ml').write_text(prelude + body_text + '\n')


if __name__ == '__main__':
    emit()
