(* Complete ANF/MIR parser boundary observations over frozen and new fixture inputs. *)
module J=Semantic_observation.SemanticJson
module A=Semantic_observation.ProductionANF
module M=Semantic_observation.ProductionMIR
let result encode=function Ok value->J.union "FSharpResult" "Ok" [encode value]|Error error->J.union "FSharpResult" "Error" [J.string error]
let row source=J.tuple [J.string source;result A.aNF_tempId (ANFParser.parseTempId source);result A.aNF_atom (ANFParser.parseAtom source);result A.aNF_binOp (ANFParser.parseOp source);result A.aNF_cExpr (ANFParser.parseCExpr source);result A.aNF_program (ANFParser.parseANF source);result M.vReg (MIRParser.parseVReg source);result M.operand (MIRParser.parseOperand source);result M.binOp (MIRParser.parseOp source);result M.program (MIRParser.parseMIR source);result M.program (MIRParser.parseMIRWithEntryLabel "custom" source)]
let observe source=
 let open Yojson.Basic.Util in
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/ir_parser_fixtures.json" in
 let rows key=`List (List.map (fun value->row (to_string value)) (fixtures |> member key |> to_list)) in
 J.tuple [row source;rows "anf";rows "mir"]
