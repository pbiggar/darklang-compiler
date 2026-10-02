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
        | "parser-support" -> Semantic_observation.SemanticJson.parserSupport source
        | "parameters" | "effects" -> Semantic_observation.SemanticJson.declarationSupport stage source
        | "validated" -> Semantic_observation.SemanticJson.validated source
        | "rendered" -> Semantic_observation.SemanticJson.rendered source
        | "ast" -> Semantic_observation.SemanticJson.ast source
        | "bindings" -> Semantic_observation.SemanticJson.bindings source
        | "types" -> Semantic_observation.SemanticJson.types source
        | "patterns" -> Semantic_observation.SemanticJson.patterns source
        | stage -> failwith ("Unsupported native observation stage: " ^ stage)
      in
      print_endline (Yojson.Basic.to_string (`Assoc ["schema", `Int 1; "stage", `String stage; "value", value]));
      requests ()
  | exception End_of_file -> ()
let () = requests ()
