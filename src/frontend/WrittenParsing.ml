(*
   WrittenParsing.ml - Enter the copied interpreter parser for executable source units.
*)
(* WrittenParsing.ml - Enter the native parser's validated source boundary. *)
let parse mode source =
  Result.map_error (fun diagnostics -> String.concat "\n" (List.map (Parser.renderDiagnostic source) diagnostics))
    (Parser.parseFor mode source)
