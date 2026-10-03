"""Generate a typed structural visitor from the checked schema, without reflection."""
import re
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
source = (ROOT / "migration/checked_ast_observation.inc").read_text()
matches = list(re.finditer(r"^(?:let(?: rec)?|and) (observation\w+)\b", source, re.M))
nodes = {m[1]: source[m.start():matches[i+1].start() if i+1<len(matches) else len(source)]
         for i,m in enumerate(matches)}
predefined = set("""observationScalar observationUnion observationTuple observationRecord observationString observationInt observationOption observationBinding observationFunction observationTypeId observationConstructorId observationFieldId observationScopeId observationGroupId observationMemberId""".split())
needed = set()
def visit(name):
    if name in predefined or name in needed:
        return
    needed.add(name)
    for dependency in re.findall(r"\bobservation\w+\b", nodes[name]):
        if dependency != name:
            visit(dependency)
visit("observationExpr")
prelude = """(* Typed structural formatting of the complete checked expression schema.
   Opaque identity descriptions come from AST's diagnostic boundary. *)
[@@@warning "-4"]
open! CheckedAST
open! StructuralValue
let unsigned value = Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value)
let observationScalar kind text =
 let suffix = match kind with "int8" -> "y" | "uint8" -> "uy" | "int16" -> "s" | "uint16" -> "us" | "int64" -> "L" | "uint64" -> "UL" | "uint32" -> "u" | _ -> "" in
 if kind = "float64" then Scalar (HostFloat.structural (Int64.float_of_bits (Int64.of_string ("0x" ^ text))))
 else Scalar (text ^ suffix)
let observationUnion _ case fields = Union (case, fields)
let observationTuple fields = Tuple fields
let observationRecord _ fields = Record fields
let observationString value = Text value
let observationInt value = Scalar (string_of_int value)
let observationOption encode = function None -> Union ("None", []) | Some value -> Union ("Some", [encode value])
let opaque encode value = Scalar (HostStructuralFormat.format (encode value))
let observationBinding = opaque AST.DiagnosticFormatting.binding
let observationFunction = opaque AST.DiagnosticFormatting.func
let observationTypeId = opaque AST.DiagnosticFormatting.typ
let observationConstructorId = opaque AST.DiagnosticFormatting.constructor
let observationFieldId = opaque AST.DiagnosticFormatting.field
let observationScopeId = opaque AST.DiagnosticFormatting.scope
let observationGroupId = opaque AST.DiagnosticFormatting.group
let observationMemberId = opaque AST.DiagnosticFormatting.memberId
"""

def group(private):
    parts=[]
    for match in matches:
        name=match[1]
        if name not in needed or not private and name == "observationSemanticType":
            continue
        body=nodes[name].strip()
        if name=="observationNonEmpty":
            body="and observationNonEmpty : 'a. ('a -> StructuralValue.value) -> 'a NonEmptyList.t -> StructuralValue.value = fun encode value -> observationRecord \"NonEmptyList\" [\"Head\", encode value.NonEmptyList.head; \"Tail\", Sequence (List.map encode value.NonEmptyList.tail)]"
        elif name=="observationCheckedType":
            body='and observationCheckedType value = observationUnion "CheckedType" "CheckedType" [observationSemanticType (CheckedAST.semanticType value)]' if private else 'and observationCheckedType value = Scalar (HostStructuralFormat.format (privateCheckedType value))'
        elif name=="observationRecordFields":
            if private:
                body="and observationRecordFields : 'a. ('a -> StructuralValue.value) -> 'a recordFields -> StructuralValue.value = fun encodeA value -> observationUnion \"RecordFields\" \"RecordFields\" [Sequence (List.map (fun (field, value) -> observationTuple [observationFieldId field; encodeA value]) (CheckedAST.recordFieldsInSourceOrder value))]"
            else:
                body='and observationRecordFields _encode value = Scalar (HostStructuralFormat.format (privateRecordFields privateExpr value))'
        body=re.sub(r'^(?:let(?: rec)?|and) ', 'let rec ' if not parts else 'and ',body,count=1)
        body=body.replace('Yojson.Basic.t','StructuralValue.value').replace('`List','Sequence').replace('`Bool x','Scalar (if x then "true" else "false")')
        if private:
            body=re.sub(r'\bobservation(\w+)\b',lambda match: "private"+match[1] if "observation"+match[1] in needed or match[1] in {"Binding","Function","TypeId","ConstructorId","FieldId","ScopeId","GroupId","MemberId"} else match[0],body)
        parts.append(body)
    return "\n".join(parts)
privateHelpers="\n".join("let private"+name+" = AST.DiagnosticFormatting."+method for name,method in [("Binding","binding"),("Function","func"),("TypeId","typ"),("ConstructorId","constructor"),("FieldId","field"),("ScopeId","scope"),("GroupId","group"),("MemberId","memberId")])
output=prelude+"\n"+privateHelpers+"\n"+group(True)+"\n"+group(False)+"\nlet value = observationExpr\nlet expr expression = HostStructuralFormat.format (value expression)\nlet toString expression = HostStructuralFormat.format (privateExpr expression)\n"
(ROOT / "lib/CheckedStructuralFormat.ml").write_text(output)
print(f"Generated {len(needed)} typed visitors for both public and private diagnostic layouts")
