(* IRPrinting.ml - Render compiler dumps and Unicode-insensitive function filters. *)
let containsIgnoreCase text pattern =
  Text.contains (Text.caseFold text) (Text.caseFold pattern)

let escapeStringContent input =
  let buffer = Buffer.create (String.length input) in
  String.iter
    (function
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '"' -> Buffer.add_string buffer "\\\""
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | '\000' -> Buffer.add_string buffer "\\0"
      | character -> Buffer.add_char buffer character)
    input;
  Buffer.contents buffer

(*
   Append a type suffix when available
*)
let appendTypeSuffix typ value =
  match typ with
  | None -> value
  | Some typ -> value ^ " : " ^ StructuralFormat.semanticType typ

let commaSeparated printer values = String.concat ", " (List.map printer values)

let functionNameMatches filter name =
  match filter with
  | None -> true
  | Some pattern -> containsIgnoreCase name pattern

let noFunctionMatchText = function
  | Some pattern -> "No functions matched '" ^ pattern ^ "'."
  | None -> "Functions: 0"
