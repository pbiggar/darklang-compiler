"""Observe every memory/ANF constructor and verify the frozen schema's case set."""
from pathlib import Path
from generate_ir_observation import SCHEMAS, definitions, product_parts

ROOT = Path(__file__).resolve().parents[1]
ENTRIES = [(source, module, name, body) for source, module, path in SCHEMAS for name, body in definitions(ROOT / 'lib' / path)]
DEFS = {(source, name): (module, body) for source, module, name, body in ENTRIES}


def field_name(name, field):
    return 'Type' if name == 'typedParam' and field == 'typ' else field[0].upper() + field[1:]


def sample(source, typ, fs):
    typ = typ.strip()
    if typ.startswith('(') and typ.endswith(')'):
        typ = typ[1:-1]
    parts = product_parts(typ)
    if len(parts) > 1:
        return '(' + ', '.join(sample(source, part, fs) for part in parts) + ')'
    if typ.endswith(' list'):
        value = sample(source, typ[:-5], fs)
        return '[' + value + '; ' + value + ']'
    if typ.endswith(' option'):
        return '(Some (' + sample(source, typ[:-7], fs) + '))'
    if typ.endswith(' array'):
        return '[||]'
    if typ.endswith(' StringOrder.Map.t'):
        inner = sample(source, typ[:-len(' StringOrder.Map.t')], fs)
        return '(' + ('Map.ofList' if fs else 'Dark_compiler.StringOrder.Map.of_list') + ' [(source, ' + inner + ')])'
    if typ == 'string ExprIdMap.t':
        return '(' + ('Map.ofList' if fs else 'InstrumentedANF.ExprIdMap.of_list') + ' [(3, source)])'
    base = {'int': '3', 'int32': '3' if fs else '3l', 'int64': '3L', 'string': 'source', 'bool': 'true', 'float': '-0.0',
            'AST.semanticType': 'AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])',
            'AST.functionId': 'AST.functionId System.UInt64.MaxValue' if fs else 'Dark_compiler.AST.functionId (-1L)',
            'IntSet.t': 'Set.ofList [3; 1]' if fs else 'Dark_compiler.MemoryModel.IntSet.of_list [3; 1]'}
    if typ in base:
        return '(' + base[typ] + ')'
    if '.' in typ:
        source, typ = typ.rsplit('.', 1)
    module, body = DEFS[source, typ]
    prefix = source if fs else ('Dark_compiler.' + module if source == 'MemoryModel' else module)
    defaults = {('MemoryModel', 'rcShape'): 'Immediate', ('MemoryModel', 'rcReleasePlan'): 'NoReleasePlan', ('MemoryModel', 'rcPayloadReleasePlan'): 'NoPayloadRelease',
                ('ANF', 'atom'): 'StringLiteral source', ('ANF', 'cExpr'): 'Atom (ANF.StringLiteral source)' if fs else 'Atom (InstrumentedANF.StringLiteral source)',
                ('ANF', 'aExpr'): 'Return ANF.UnitLiteral' if fs else 'Return InstrumentedANF.UnitLiteral', ('ANF', 'typeMap'): 'TypeMap.empty'}
    if (source, typ) in defaults:
        return '(' + prefix + '.' + defaults[source, typ] + ')'
    if body.startswith('{'):
        fields = [field.strip().split(':', 1) for field in body.strip('{} \n').split(';') if field.strip()]
        type_name = {'functionDef': 'Function'}.get(typ, typ[0].upper() + typ[1:]) if fs else typ
        record = '{' + '; '.join(prefix + ('.' + type_name if fs else '') + '.' + (field_name(typ, field.strip()) if fs else field.strip()) + ' = ' + sample(source, field_type, fs) for field, field_type in fields) + '}'
        return '(' + record + ' : ' + prefix + '.' + type_name + ')'
    if '|' in body or body[0].isupper():
        case = next(case.strip() for case in body.split('|') if case.strip())
        return case_value(source, module, typ, case, fs)
    return sample(source, body, fs)


def case_value(source, module, name, case, fs):
    constructor, _, payload = case.partition(' of ')
    prefix = source if fs else ('Dark_compiler.' + module if source == 'MemoryModel' else module)
    values = [sample(source, part, fs) for part in product_parts(payload)] if payload else []
    if name == 'sizedInt':
        values = [({'Int8': '3y', 'Int16': '3s', 'Int32': '3', 'Int64': '3L', 'UInt8': '3uy', 'UInt16': '3us', 'UInt32': '3ul', 'UInt64': '3UL'} if fs else {'Int8': '3', 'Int16': '3', 'Int32': '3l', 'Int64': '3L', 'UInt8': '3', 'UInt16': '3', 'UInt32': '3L', 'UInt64': '3L'})[constructor]]
    return '(' + prefix + '.' + constructor + (' (' + ', '.join(values) + ')' if values else '') + ')'


def emit():
    native, reference = [], []
    native_shapes, reference_shapes = [], []
    for source, module, name, body in ENTRIES:
        if name in ['typeMap', 'exprId', 'coverageMapping']:
            continue
        fsharp = {'functionDef': 'Function'}.get(name, name[0].upper() + name[1:])
        encoder = 'SemanticANF.' + source[0].lower() + source[1:] + '_' + name
        if '|' in body or (body and body[0].isupper() and not body.startswith('IntSet')):
            cases = [case.strip() for case in body.split('|') if case.strip()]
            natives = [case_value(source, module, name, case, False) for case in cases]
            references = [case_value(source, module, name, case, True) for case in cases]
            shapes = [(case.partition(' of ')[0], len(product_parts(case.partition(' of ')[2])) if ' of ' in case else 0) for case in cases]
            native_shapes.append('list (fun (name, fields) -> tuple [SemanticJson.string name; int fields]) [' + '; '.join('("' + name + '", ' + str(count) + ')' for name, count in shapes) + ']')
            reference_shapes.append('encode typeof<(string * int) list> (box (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<' + source + '.' + fsharp + '>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList))')
        else:
            natives, references = [sample(source, name, False)], [sample(source, name, True)]
        native.append('list ' + encoder + ' [' + '; '.join(natives) + ']')
        reference.append('encode typeof<' + source + '.' + fsharp + ' list> (box [' + '; '.join(references) + '])')
    interface = '(* Complete memory/ANF constructor fixtures for the immutable oracle. *)\nval observe : string -> Yojson.Basic.t\n'
    implementation = '''(* Complete memory/ANF constructor fixtures for the immutable oracle. *)
open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let int value = `Assoc ["kind", `String "int32"; "value", `String (string_of_int value)]
let observe source = tuple [
'''+ 'tuple [' + ';\n'.join(native) + '];\ntuple [' + ';\n'.join(native_shapes) + ']]\n'
    (ROOT / 'migration/IRFixtures.mli').write_text(interface)
    (ROOT / 'migration/IRFixtures.ml').write_text(implementation)
    code = '// BEGIN GENERATED IR FIXTURES\nlet irFixtures source =\n    let tuple values = namedArray "tuple" (Array.ofList values)\n    tuple [tuple [\n' + ';\n'.join('        ' + value for value in reference) + '];\n      tuple [\n' + ';\n'.join('        ' + value for value in reference_shapes) + ']]\n// END GENERATED IR FIXTURES\n\n'
    script = ROOT.parent / 'scripts/ocaml/semantic_reference.fsx'
    text = script.read_text()
    if '// BEGIN GENERATED IR FIXTURES' in text:
        start = text.index('// BEGIN GENERATED IR FIXTURES')
        end = text.index('// END GENERATED IR FIXTURES', start) + len('// END GENERATED IR FIXTURES\n\n')
        text = text[:start] + code + text[end:]
    else:
        text = text.replace('let anfObservation source =', code + 'let anfObservation source =')
    script.write_text(text)


if __name__ == '__main__':
    emit()
