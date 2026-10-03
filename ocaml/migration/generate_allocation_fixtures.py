"""Generate every LIR constructor with independent direct register operands."""
from pathlib import Path
from generate_lir_fixtures import DEFS, case_value, sample
from generate_ir_observation import product_parts

ROOT=Path(__file__).resolve().parents[1]

def emit(fs):
    instructions=[]
    for case in (c.strip() for c in DEFS['instr'].split('|') if c.strip()):
        name,_,payload=case.partition(' of ')
        indices={'reg':0,'fReg':0}
        values=[]
        for typ in product_parts(payload) if payload else []:
            if typ in indices:
                array='regs' if typ=='reg' else 'fregs'
                index=indices[typ]
                values.append(f'({array}[{index}])' if fs else f'({array}.({index}))')
                indices[typ]+=1
            else: values.append(sample(typ,fs))
        value='LIR.'+name+(' ('+', '.join(values)+')' if values else '')
        if name=='PrintChars': value=case_value('instr',case,fs)
        instructions.append(value)
    body='['+';\n'.join(instructions)+']\n'
    if fs:
        header='let lirAllocationFixtures (source:string) (regs:LIR.Reg array) (fregs:LIR.FReg array) operand typ : LIR.Instr list =\n'
        if '(reg)' in body: header+='    let reg=regs[0]\n'
        if '(freg)' in body: header+='    let freg=fregs[0]\n'
        header+='    '
        path=ROOT.parent/'TestResults/ocaml-migration/allocation-fixtures-reference.fsx'
    else:
        header='(* All LIR instructions with independently varied direct register roles. *)\nopen Dark_compiler\nlet instructions source regs fregs operand typ =\n'
        if '(reg)' in body: header+=' let reg=regs.(0) in\n'
        if '(freg)' in body: header+=' let freg=fregs.(0) in\n'
        header+=' '
        path=ROOT/'migration/AllocationFixtures.ml'
    path.write_text(header+body)

if __name__=='__main__':
    (ROOT/'migration/AllocationFixtures.mli').write_text('val instructions : string -> Dark_compiler.LIR.reg array -> Dark_compiler.LIR.fReg array -> Dark_compiler.LIR.operand -> Dark_compiler.AST.semanticType -> Dark_compiler.LIR.instr list\n')
    emit(False)
    emit(True)
