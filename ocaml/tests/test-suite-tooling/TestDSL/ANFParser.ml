(*
   ANFParser.fs - Parser for ANF (A-Normal Form) DSL
   Parses human-readable ANF text into ANF.Program data structures.
   Example ANF:
   let t0 = 3 * 4
   let t1 = 2 + t0
   return t1
*)
(* Parse the original ANF text grammar without changing fixture acceptance or errors. *)
open Dark_compiler
open ANF
open DSLPattern
let (let*)=Result.bind
(*
   Parse temp ID from text like "t0", "t1", etc.
*)
let parseTempId text=
 let trimmed=HostText.trim text in
 let invalid ()=Error ("Invalid temp id '"^trimmed^"' (expected 't0', 't1', etc.)") in
 match matched [literal "t";Capture [digits]] trimmed with
 |Some groups->(match HostText.tryParseInt32 groups.(1) with Some id->Ok (TempId (Int32.to_int id))|None->invalid ())
 |None->invalid ()
(*
   Parse atom (literal or temp variable)
*)
let parseAtom text=
 let text=HostText.trim text in
 match matched [literal "u64[";Capture [digits];literal "]"] text with
 |Some groups->(match integer 64 false groups.(1) with Some value->Ok (IntLiteral (UInt64 (Z.to_int64 (if Z.testbit value 63 then Z.sub value (Z.shift_left Z.one 64) else value))))|None->Error ("Invalid UInt64 literal '"^text^"'"))
 |None->match matched [literal "str[";Capture [Repeat (Dot,0,true)];literal "]"] text with
 |Some groups->Result.map (fun text->StringLiteral text) (Common.parseEscapedText groups.(1))
 |None->match parseTempId text with Ok tid->Ok (Var tid)|Error _->
 match int64 text with Some n->Ok (IntLiteral (Int64 n))|None->Error ("Invalid atom '"^text^"' (expected a literal or temp variable)")
(*
   Parse binary operator
*)
let parseOp text=match HostText.trim text with
 |"+"->Ok Add|"-"->Ok Sub|"*"->Ok Mul|"/"->Ok Div|op->Error ("Invalid operator '"^op^"' (expected +, -, *, or /)")
(*
   Parse complex expression (right side of let binding)
   Try binary operation: "2 + 3" or "t0 * t1"
   Just an atom
*)
let parseCExpr text=
 let text=HostText.trim text in
 match matched [Capture [Repeat (Dot,1,false)];space;Capture [Alternatives (List.map (fun op->[literal op]) ["+";"-";"*";"/"])];space;Capture [any]] text with
 |Some groups->let* left=parseAtom groups.(1) in let* op=parseOp groups.(2) in let* right=parseAtom groups.(3) in Ok (Prim (op,left,right))
 |None->Result.map (fun atom->Atom atom) (parseAtom text)
(*
   Parse ANF expression recursively
   Try return pattern: "return t0"
   Try let pattern: "let t0 = 3 + 4"
*)
let rec parseAExpr lineNum=function
 |[]->Error "Unexpected end of input (expected 'return')"
 |line::rest->let line=HostText.trim line in
 match matched [literal "return";spaces;Capture [any]] line with
 |Some groups->(match rest with _::_->Error (Printf.sprintf "Line %d: Unexpected line after return" (lineNum+1))|[]->Result.map_error (fun e->Printf.sprintf "Line %d: %s" lineNum e) (Result.map (fun atom->Return atom) (parseAtom groups.(1))))
 |None->match matched [literal "let";spaces;Capture [literal "t";digits];space;literal "=";space;Capture [any]] line with
 |Some groups->let prefix e=Printf.sprintf "Line %d: %s" lineNum e in
 let* tid=Result.map_error prefix (parseTempId groups.(1)) in
 let* cexpr=Result.map_error prefix (parseCExpr groups.(2)) in
 let* body=parseAExpr (lineNum+1) rest in Ok (Let (tid,cexpr,body))
 |None->Error (Printf.sprintf "Line %d: Expected 'let' or 'return', got: %s" lineNum line)
(*
   Parse ANF program from text
   No functions, just main expression
*)
let parseANF text=Result.map (fun expr->Program ([],expr)) (parseAExpr 1 (Common.stripCommentsAndEmpty text))
