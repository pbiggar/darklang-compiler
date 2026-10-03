"""Generate complete typed LIR encoders; reject every unknown field type."""
from pathlib import Path
import re
from generate_ir_observation import definitions, product_parts

ROOT = Path(__file__).resolve().parents[1]
ENTRIES = definitions(ROOT / 'lib/ir/lir/LIR.mli')

def source_name(name):
    return {'cfg': 'CFG', 'functionDef': 'Function'}.get(name, name[0].upper()+name[1:])

def encoder(typ, value):
    typ = typ.strip()
    if typ.startswith('(') and typ.endswith(')'):
        typ = typ[1:-1]
    if len(product_parts(typ)) > 1:
        parts = product_parts(typ)
        values = [f'part{i}' for i in range(len(parts))]
        return '(let '+', '.join(values)+' = '+value+' in tuple ['+'; '.join(encoder(t,v) for t,v in zip(parts,values))+'])'
    for suffix, constructor in [(' list','list'), (' option','option')]:
        if typ.endswith(suffix):
            return constructor+' (fun item -> '+encoder(typ[:-len(suffix)],'item')+') '+value
    map_types = {
        'StringOrder.Map.t': 'SemanticJson.string',
        'LabelMap.t': 'label',
        'ReleasePlanSummaryMap.t': '(fun (flag, key) -> tuple [boolean flag; rcReleasePlanMemoKey key])',
        'RefCountDecRequirementMap.t': '(fun (kind, key) -> tuple [rcKind kind; rcReleasePlanMemoKey key])',
        'SemanticTypeMap.t': 'SemanticAST.semanticType',
    }
    for module, key in map_types.items():
        if typ.endswith(' '+module):
            namespace = module if module.startswith('StringOrder') else 'LIR.'+module
            return '`Assoc ["map", list (fun (key, item) -> tuple [('+key+') key; '+encoder(typ[:-len(module)],'item')+']) ('+namespace.replace('.t','.bindings')+' '+value+')]'
    sets = {'StringOrder.Set.t': ('StringOrder.Set', 'SemanticJson.string'), 'RcReleasePlanMemoKeySet.t': ('LIR.RcReleasePlanMemoKeySet','rcReleasePlanMemoKey'), 'RcKindSet.t': ('LIR.RcKindSet','rcKind'), 'MemoryPlanning.SemanticTypeSet.t': ('MemoryPlanning.SemanticTypeSet','SemanticAST.semanticType')}
    if typ in sets:
        module, enc = sets[typ]
        return '`Assoc ["set", list '+enc+' ('+module+'.elements '+value+')]'
    aliases = {'int':'SemanticJson.int32', 'int64':'int64', 'float':'float64', 'string':'SemanticJson.string', 'bool':'boolean', 'AST.functionId':'functionId', 'AST.semanticType':'SemanticAST.semanticType', 'MemoryModel.rcReleasePlan':'ProductionANF.memoryModel_rcReleasePlan', 'MemoryModel.rcMetadata':'ProductionANF.memoryModel_rcMetadata', 'MemoryModel.canonicalBufferKind':'ProductionANF.memoryModel_canonicalBufferKind'}
    if typ in aliases:
        return aliases[typ]+' '+value
    if typ in dict(ENTRIES):
        return typ+' '+value
    raise ValueError('Unknown LIR encoder type: '+typ)

def emit():
    interface = '(* Complete typed symbolic LIR encoders. *)\nopen Dark_compiler\n'
    prelude = '''(* Complete typed symbolic LIR encoders. *)
[@@@warning "-4"]
open Dark_compiler
let scalar kind value = `Assoc ["kind", `String kind; "value", `String value]
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let boolean value = `Bool value
let int64 value = scalar "int64" (Int64.to_string value)
let float64 value = scalar "float64" (Printf.sprintf "%016Lx" (Int64.bits_of_float value))
let functionId value = SemanticJson.union "FunctionId" "FunctionId" [scalar "uint64" (Printf.sprintf "%Lu" (AST.functionIdValue value))]
'''
    bodies = []
    for name, body in ENTRIES:
        interface += f'val {name} : LIR.{name} -> Yojson.Basic.t\n'
        if body.startswith('{'):
            fields = [field.strip().split(':',1) for field in body.strip('{} \n').split(';') if field.strip()]
            encoded = []
            for field, typ in fields:
                field = field.strip()
                original = 'Type' if field == 'typ' else 'CFG' if field == 'cfg' else field[0].upper()+field[1:]
                encoded.append('"'+original+'", '+encoder(typ,'value.LIR.'+field))
            expr = 'SemanticJson.record "'+source_name(name)+'" ['+'; '.join(encoded)+']'
        elif '|' in body or re.match('^[A-Z]',body):
            clauses = []
            for case in body.split('|'):
                if not case.strip(): continue
                constructor, _, payload = case.strip().partition(' of ')
                parts = product_parts(payload) if payload else []
                values = [f'field{i}' for i in range(len(parts))]
                pattern = 'LIR.'+constructor+(' ('+', '.join(values)+')' if values else '')
                output = [encoder(t,v) for t,v in zip(parts,values)]
                if name == 'instr' and constructor == 'PrintChars':
                    output = ['list (fun value -> scalar "uint8" (string_of_int value)) field0']
                clauses.append(' | '+pattern+' -> SemanticJson.union "'+source_name(name)+'" "'+constructor+'" ['+'; '.join(output)+']')
            expr = 'match value with\n'+'\n'.join(clauses)
        else: expr = encoder(body,'value')
        bodies.append(('let rec' if not bodies else 'and')+' '+name+' (value : LIR.'+name+') = '+expr)
    (ROOT/'migration/ProductionLIR.mli').write_text(interface)
    (ROOT/'migration/ProductionLIR.ml').write_text(prelude+'\n'.join(bodies)+'\n')

if __name__ == '__main__': emit()
