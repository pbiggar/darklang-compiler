(*
   Resolve.fs - ARM64 symbolic literal-pool collection before offset assignment.
*)
[@@@warning "-4"]
let collectPoolsFromLabelRefs labelRefs =
 let strings,floats=Seq.fold_left (fun (strings,floats) -> function
 | Symbolic.DataLabel (Symbolic.StringLiteral value) -> value::strings,floats
 | Symbolic.DataLabel (Symbolic.FloatLiteral value) -> strings,value::floats
 | _ -> strings,floats) ([],[]) labelRefs in
 LiteralPool.createStringPool (List.to_seq (List.rev strings)),LiteralPool.createFloatPool (List.to_seq (List.rev floats))
let collectPools instructions=collectPoolsFromLabelRefs (Seq.filter_map (function
 | Symbolic.ADRP (_,labelRef) | Symbolic.ADD_label (_,_,labelRef) | Symbolic.ADR (_,labelRef) -> Some labelRef
 | _ -> None) (List.to_seq instructions))
