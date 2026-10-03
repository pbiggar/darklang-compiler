"""Generate all symbolic LIR constructors against the reflected source schema."""
from pathlib import Path
from generate_lir_observation import ENTRIES, source_name
from generate_ir_observation import product_parts

ROOT = Path(__file__).resolve().parents[1]
DEFS = dict(ENTRIES)
UNIONS = ['physReg','physFPReg','reg','fReg','operand','condition','rcKind','cliOperation','label','instr','terminator','rcReleasePlanMemoKey','arm64SlotInitRootRetainTarget']

def sample(typ, fs):
    typ = typ.strip()
    if typ.startswith('(') and typ.endswith(')'): typ = typ[1:-1]
    parts = product_parts(typ)
    if len(parts) > 1: return '('+', '.join(sample(t,fs) for t in parts)+')'
    if typ.endswith(' list'):
        item = sample(typ[:-5],fs)
        return '['+item+'; '+item+']'
    if typ.endswith(' option'): return '(Some ('+sample(typ[:-7],fs)+'))'
    aliases = {'int':'3','int64':'-3L','float':'-0.0','bool':'true','string':'source',
               'reg':'LIR.Virtual 3','fReg':'LIR.FVirtual (-1)','label':'LIR.Label source',
               'operand':'operand','AST.semanticType':'AST.TRecord (source,[AST.TInt64;AST.TList AST.TString])',
               'AST.functionId':'AST.functionId System.UInt64.MaxValue' if fs else 'AST.functionId (-1L)',
               'MemoryModel.rcReleasePlan':'MemoryModel.RootRelease (16,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (16,[MemoryModel.FieldRelease (8,MemoryModel.RecursiveRelease (AST.TRecord (source,[])))]))',
               'MemoryModel.rcMetadata':'{MemoryModel.RcMetadata.ReleasePlanCacheKey=Some source;ReleasePlan=Some (MemoryModel.RecursiveRelease (AST.TList AST.TString));SourceType=Some AST.TString}' if fs else '{MemoryModel.releasePlanCacheKey=Some source;releasePlan=Some (MemoryModel.RecursiveRelease (AST.TList AST.TString));sourceType=Some AST.TString}',
               'MemoryModel.canonicalBufferKind':'MemoryModel.NullableGraphemeCluster'}
    if typ in aliases: return '('+aliases[typ]+')'
    if typ not in DEFS: raise ValueError('Unknown fixture '+typ)
    return case_value(typ,next(c.strip() for c in DEFS[typ].split('|') if c.strip()),fs)

def case_value(name, case, fs):
    constructor, _, payload = case.partition(' of ')
    values = [sample(t,fs) for t in product_parts(payload)] if payload else []
    if name=='instr' and constructor=='PrintChars': values=['[0uy;127uy;255uy]' if fs else '[0;127;255]']
    return 'LIR.'+constructor+(' ('+', '.join(values)+')' if values else '')

def emit():
    for fs, destination in [(False,ROOT/'migration/LIRFixtures.ml'),(True,ROOT.parent/'TestResults/ocaml-migration/lir-fixtures-reference.fsx')]:
        values, shapes = [], []
        instructions = ''
        for name in UNIONS:
            cases = [c.strip() for c in DEFS[name].split('|') if c.strip()]
            samples = '['+';\n'.join(case_value(name,c,fs) for c in cases)+']'
            if name=='instr': instructions = samples
            if fs:
                values.append('enc ('+samples+' : LIR.'+source_name(name)+' list)')
                shapes.append('enc (FSharpType.GetUnionCases(typeof<LIR.'+source_name(name)+'>) |> Array.map (fun case -> case.Name,case.GetFields().Length) |> Array.toList)')
            else:
                values.append('list ProductionLIR.'+name+' '+samples)
                shapes.append('list (fun (name,fields) -> tuple [SemanticJson.string name; SemanticJson.int32 fields]) ['+'; '.join('("'+c.partition(' of ')[0]+'",'+str(len(product_parts(c.partition(' of ')[2])) if ' of ' in c else 0)+')' for c in cases)+']')
        if fs:
            code='let lirConstructorFixtures (source:string) =\n    let enc (value:\'a) = encode typeof<\'a> (box value)\n    let tuple values = namedArray "tuple" (Array.ofList values)\n    let operand = LIR.StringSymbol source\n    tuple [tuple ['+';\n'.join(values)+'];tuple ['+';\n'.join(shapes)+']]\nlet lirInstructionFixturesWithOperand (source:string) operand : LIR.Instr list = '+instructions+'\nlet lirInstructionFixtures source = lirInstructionFixturesWithOperand source (LIR.StringSymbol source)\n'
        else:
            code='''(* Complete symbolic LIR constructors and reflected schema parity. *)
open Dark_compiler
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let observe source = let operand = LIR.StringSymbol source in tuple [tuple [
'''+ ';\n'.join(values)+'];tuple ['+';\n'.join(shapes)+']]\nlet instructionsWithOperand source operand = '+instructions+'\nlet instructions source = instructionsWithOperand source (LIR.StringSymbol source)\n'
        destination.write_text(code)
    (ROOT/'migration/LIRFixtures.mli').write_text('val observe : string -> Yojson.Basic.t\nval instructions : string -> Dark_compiler.LIR.instr list\nval instructionsWithOperand : string -> Dark_compiler.LIR.operand -> Dark_compiler.LIR.instr list\n')

if __name__=='__main__': emit()
