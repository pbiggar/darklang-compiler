(* semantic_probe.ml - Observe native semantic results for migration requests. *)
let rec requests () =
  match input_line stdin with
  | line ->
      let open Yojson.Basic.Util in
      let request = Yojson.Basic.from_string line in
      let stage = request |> member "stage" |> to_string in
      let source = request |> member "source" |> to_string in
      let value = match stage with
        | "tokens" -> Semantic_observation.SemanticJson.tokens (Dark_compiler.Lexer.tokenize source)
        | stage -> failwith ("Unsupported native observation stage: " ^ stage)
      in
      print_endline (Yojson.Basic.to_string (`Assoc ["schema", `Int 1; "stage", `String stage; "value", value]));
      requests ()
  | exception End_of_file -> ()
let () = requests ()
