(* Complete ANF/MIR parser boundary observations over frozen and new fixture inputs. *)
module J=Semantic_observation.SemanticJson
module A=Semantic_observation.ProductionANF
module ISA=Semantic_observation.MachineISAObservation
module L=Semantic_observation.ProductionLIR
module M=Semantic_observation.ProductionMIR
let result encode=function Ok value->J.union "FSharpResult" "Ok" [encode value]|Error error->J.union "FSharpResult" "Error" [J.string error]
let row source=J.tuple [J.string source;result A.aNF_tempId (ANFParser.parseTempId source);result A.aNF_atom (ANFParser.parseAtom source);result A.aNF_binOp (ANFParser.parseOp source);result A.aNF_cExpr (ANFParser.parseCExpr source);result A.aNF_program (ANFParser.parseANF source);result M.vReg (MIRParser.parseVReg source);result M.operand (MIRParser.parseOperand source);result M.binOp (MIRParser.parseOp source);result M.program (MIRParser.parseMIR source);result M.program (MIRParser.parseMIRWithEntryLabel "custom" source);result L.physReg (LIRParser.parsePhysReg source);result L.reg (LIRParser.parseRegister source);result L.operand (LIRParser.parseOperand source);result L.program (LIRParser.parseLIR source);result ISA.armReg (ARM64SymbolicParser.parseReg source);result ISA.armCondition (ARM64SymbolicParser.parseCond source);result ISA.symLabelRef (ARM64SymbolicParser.parseLabelRef source);result ISA.symInstr (ARM64SymbolicParser.parseInstruction 42 source);result (fun values->`List (List.map ISA.symInstr values)) (ARM64SymbolicParser.parseARM64Symbolic source)]
let observe source=
 let open Yojson.Basic.Util in
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/ir_parser_fixtures.json" in
 let rows key=`List (List.map (fun value->row (to_string value)) (fixtures |> member key |> to_list)) in
 J.tuple [row source;rows "anf";rows "mir";rows "lir";rows "symbolic"]
