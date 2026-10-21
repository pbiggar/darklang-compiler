(* parser_port_regression.ml - Preserve pinned interpreter syntax and structural validation. *)
[@@@warning "-4-42"]
open Dark_compiler
module WT = WrittenTypes

let failures = ref 0
let run name check =
  try check (); Printf.printf "PASS %s\n%!" name
  with Failure message -> incr failures; Printf.printf "FAIL %s: %s\n%!" name message
let require condition message = if not condition then failwith message
let parsed source =
  let result = Parser.parse source in
  require (result.ParserSupport.diagnostics = [])
    (String.concat "; " (List.map (fun (d : ParserSupport.diagnostic) -> d.message) result.diagnostics));
  match result.parsed with Some (WT.SourceFile file) -> file | None -> failwith "No source tree"
let rejects source text =
  let result = Parser.parse source in
  require (List.exists (fun (d : ParserSupport.diagnostic) -> Text.contains d.message text) result.diagnostics)
    ("Expected diagnostic: " ^ text)
let accepts source () = ignore (parsed source)

let () =
  run "nested generic function arrow" (fun () ->
    match (parsed "type F = List<List<Int>> -> Int").WT.declarations with
    | [WT.DType { definition = WT.TDAlias (WT.TFn (_, [(WT.TList (_, _, _, WT.TList _, _), _)], WT.TInt _)); _ }] -> ()
    | _ -> failwith "Arrow was absorbed by a List argument");
  run "unwrap node" (fun () ->
    let file = parsed "let f (x: Int) : Int = x?" in
    require (Text.contains (WrittenFormatter.syntaxKey file) "EUnwrap") "Missing unwrap node");
  run "postfix chains" (accepts "let f (x: Int) : Int = x??.field.1");
  run "tuple selectors" (fun () ->
    match (parsed "value.1.2").WT.exprsToEval with
    | [WT.ERecordFieldAccess (_, WT.ERecordFieldAccess (_, _, (_, "1"), _), (_, "2"), _)] -> ()
    | _ -> failwith "Numeric selectors lost their identities");
  List.iter (fun source -> run ("reject " ^ source) (fun () -> rejects source "++"))
    ["\"a\" ++ \"b\""; "(++)"; "\"a\" |> (++) \"b\""];
  run "unwrap requires function" (fun () -> rejects "value?" "enclosing function");
  run "unwrap adjacency" (fun () -> rejects "let f (x: Int) : Int = x ?" "directly after");
  run "unwrap pipe boundary" (fun () -> rejects "let f (x: Int) : Int = x |> transform?" "pipe segment");
  run "blank trait parameter" (fun () -> rejects "trait Show<'a> =\n  let show (___: 'a) : String" "Blank parameter");
  run "duplicate trait method" (fun () -> rejects "trait Show<'a> =\n  let show (x: 'a) : String\n  let show (x: 'a) : String" "Duplicate binding");
  run "duplicate trait parameters remain syntax" (accepts "trait Show<'a> =\n  let show (x: 'a) (x: 'a) : String");
  run "duplicate impl alias" (fun () -> rejects "impl Show for Int =\n  let show = Helpers.show\n  let show = Helpers.show" "Duplicate binding");
  run "current effect names" (accepts "let f () :{Concurrency, Runtime} Unit = ()");
  run "enum capitalization" (fun () -> rejects "type T = | __Case" "at least one case");
  run "sqlerror is an ordinary expression" (fun () ->
    match (parsed "1 = sqlerror \"message\"").WT.declarations with
    | [WT.DAssertion { expected = WT.TEExpr _; _ }] -> ()
    | _ -> failwith "Retired SQL marker remains special syntax");
  run "malformed selector avoids duplicate error" (fun () ->
    let result = Parser.parse "t.300y" in
    require (List.length result.ParserSupport.diagnostics = 1) "Expected only the lexer error");
  run "interpreter depth cap" (fun () -> rejects (String.make 210 '(' ^ "1" ^ String.make 210 ')') "nesting too deep");
  if !failures <> 0 then failwith (Printf.sprintf "%d parser port regressions failed" !failures)
