(* parser_port_regression.ml - Preserve pinned interpreter syntax and structural validation. *)
[@@@warning "-4-42"]

open Dark_compiler
module WT = WrittenTypes

let failures = ref 0

let run name check =
  try
    check ();
    Printf.printf "PASS %s\n%!" name
  with Failure message ->
    incr failures;
    Printf.printf "FAIL %s: %s\n%!" name message

let require condition message = if not condition then failwith message

let parsed source =
  let result = Parser.parse source in
  require
    (result.ParserSupport.diagnostics = [])
    (String.concat "; "
       (List.map
          (fun (d : ParserSupport.diagnostic) -> d.ParserSupport.message)
          result.ParserSupport.diagnostics));
  match result.ParserSupport.parsed with
  | Some (WT.SourceFile file) -> file
  | None -> failwith "No source tree"

let rejects source text =
  let result = Parser.parse source in
  require
    (List.exists
       (fun (d : ParserSupport.diagnostic) ->
         Text.contains d.ParserSupport.message text)
       result.ParserSupport.diagnostics)
    ("Expected diagnostic: " ^ text)

let accepts source () = ignore (parsed source)

let () =
  run "aligned multiline package test outcome" (fun () ->
      let source =
        "test failing =\n\
        \  Builtin.crash \"boom\"\n\
        \  => raises \"boom\"\n\
         val following = 1"
      in
      (match (parsed source).WT.declarations with
      | [ WT.DTest test; WT.DValue following ] ->
          require
            (match test.WT.expectedError with
            | Some (_, WT.RuntimeError "boom") -> true
            | _ -> false)
            "Multiline expected outcome lost";
          require
            (match test.WT.body with WT.EApply _ -> true | _ -> false)
            "Outcome consumed as a body statement";
          require
            (following.WT.name.WT.name = "following")
            "Following declaration lost"
      | _ -> failwith "Package test or following declaration changed");
      match Parser.parseFor Validation.Package source with
      | Ok _ -> ()
      | Error _ -> failwith "Valid multiline package test rejected");
  run "multiline outcome after a statement block" (fun () ->
      ignore
        (parsed
           "module Example =\n\
           \  test failing =\n\
           \    let message = \"boom\"\n\
           \    Builtin.crash message\n\
           \    => raises \"boom\""));
  run "spaced multiline outcome remains invalid" (fun () ->
      require
        ((Parser.parse "test failing =\n  1\n  = > raises \"boom\"")
           .ParserSupport.diagnostics <> [])
        "Non-adjacent outcome delimiter accepted");
  run "malformed character stays one token in type contexts" (fun () ->
      List.iter
        (fun prefix ->
          match Lexer.tokenize (prefix ^ " 'ab'") with
          | Ok ([ _; { Lexer.token = Tokenizer.TCharLit "ab"; _ }; _ ], []) ->
              ()
          | _ -> failwith ("Character split after " ^ prefix))
        [ "="; "("; "*"; "<"; ","; ":"; "->"; "of" ]);
  run "malformed character recovery preserves following declaration" (fun () ->
      let result = Parser.parse "val x = 'ab'\nval following = 1" in
      require
        (List.length result.ParserSupport.diagnostics = 1)
        "Malformed character produced cascaded diagnostics";
      match result.ParserSupport.parsed with
      | Some
          (WT.SourceFile
             { WT.declarations = [ WT.DValue first; WT.DValue following ]; _ })
        ->
          require
            (match first.WT.body with
            | WT.EChar (_, Some (_, "ab"), _, _) -> true
            | _ -> false)
            "Malformed character became an application";
          require
            (following.WT.name.WT.name = "following")
            "Following declaration lost"
      | _ -> failwith "Character recovery changed declaration boundaries");
  run "malformed trait uses expression recovery" (fun () ->
      let result = Parser.parse "trait =\nval following = 1" in
      require
        (result.ParserSupport.diagnostics <> [])
        "Malformed trait accepted";
      match result.ParserSupport.parsed with
      | Some
          (WT.SourceFile
             {
               WT.declarations = [ WT.DValue following ];
               exprsToEval = [ WT.EError _; WT.EError _ ];
               _;
             }) ->
          require
            (following.WT.name.WT.name = "following")
            "Following declaration lost"
      | _ -> failwith "Malformed trait fabricated a trait declaration");
  run "nested generic function arrow" (fun () ->
      match (parsed "type F = List<List<Int>> -> Int").WT.declarations with
      | [
       WT.DType
         {
           WT.definition =
             WT.TDAlias
               (WT.TFn (_, [ (WT.TList (_, _, _, WT.TList _, _), _) ], WT.TInt _));
           _;
         };
      ] ->
          ()
      | _ -> failwith "Arrow was absorbed by a List argument");
  run "unwrap node" (fun () ->
      let file = parsed "let f (x: Int) : Int = x?" in
      require
        (Text.contains (WrittenFormatter.syntaxKey file) "EUnwrap")
        "Missing unwrap node");
  run "postfix chains" (accepts "let f (x: Int) : Int = x??.field.1");
  run "unwrap stays inside lambda" (fun () ->
      match (parsed "fun x -> x?").WT.exprsToEval with
      | [ WT.ELambda (_, _, WT.EUnwrap _, _, _) ] -> ()
      | _ -> failwith "Postfix unwrap escaped its lambda body");
  run "unwrap outside lambda still needs function" (fun () ->
      rejects "(fun x -> x)?" "enclosing function");
  run "unwrap application argument" (fun () ->
      match (parsed "fun x -> f x?").WT.exprsToEval with
      | [ WT.ELambda (_, _, WT.EApply (_, _, _, [ WT.EUnwrap _ ]), _, _) ] -> ()
      | _ ->
          failwith "Unwrap attached to the whole call instead of its argument");
  run "unwrap parenthesized application" (fun () ->
      match (parsed "fun x -> (f x)?").WT.exprsToEval with
      | [ WT.ELambda (_, _, WT.EUnwrap (_, WT.EApply _, _), _, _) ] -> ()
      | _ -> failwith "Unwrap did not attach to the parenthesized call");
  run "unwrap inside pipe lambda" (accepts "value |> fun x -> x?");
  run "tuple selectors" (fun () ->
      match (parsed "value.1.2").WT.exprsToEval with
      | [
       WT.ERecordFieldAccess
         (_, WT.ERecordFieldAccess (_, _, (_, "1"), _), (_, "2"), _);
      ] ->
          ()
      | _ -> failwith "Numeric selectors lost their identities");
  List.iter
    (fun source -> run ("reject " ^ source) (fun () -> rejects source "++"))
    [ "\"a\" ++ \"b\""; "(++)"; "\"a\" |> (++) \"b\"" ];
  run "unwrap requires function" (fun () ->
      rejects "value?" "enclosing function");
  run "unwrap adjacency" (fun () ->
      rejects "let f (x: Int) : Int = x ?" "directly after");
  run "unwrap pipe boundary" (fun () ->
      rejects "let f (x: Int) : Int = x |> transform?" "pipe segment");
  run "blank trait parameter" (fun () ->
      rejects "trait Show<'a> =\n  let show (___: 'a) : String"
        "Blank parameter");
  run "duplicate trait method" (fun () ->
      rejects
        "trait Show<'a> =\n\
        \  let show (x: 'a) : String\n\
        \  let show (x: 'a) : String"
        "Duplicate binding");
  run "duplicate trait parameters remain syntax"
    (accepts "trait Show<'a> =\n  let show (x: 'a) (x: 'a) : String");
  run "duplicate impl alias" (fun () ->
      rejects
        "impl Show for Int =\n\
        \  let show = Helpers.show\n\
        \  let show = Helpers.show"
        "Duplicate binding");
  run "duplicate impl definitions" (fun () ->
      rejects
        "impl Show for Int =\n\
        \  let show (x: Int) : String = \"first\"\n\
        \  let show (x: Int) : String = \"second\""
        "Duplicate binding");
  run "current effect names"
    (accepts "let f () :{Concurrency, Runtime} Unit = ()");
  run "enum capitalization" (fun () ->
      rejects "type T = | __Case" "at least one case");
  run "sqlerror is an ordinary expression" (fun () ->
      match (parsed "1 = sqlerror \"message\"").WT.declarations with
      | [ WT.DAssertion { WT.expected = WT.TEExpr _; _ } ] -> ()
      | _ -> failwith "Retired SQL marker remains special syntax");
  run "malformed selector avoids duplicate error" (fun () ->
      let result = Parser.parse "t.300y" in
      require
        (List.length result.ParserSupport.diagnostics = 1)
        "Expected only the lexer error");
  run "interpreter depth cap" (fun () ->
      rejects
        (String.make 210 '(' ^ "1" ^ String.make 210 ')')
        "nesting too deep");
  run "last permitted expression depth"
    (accepts (String.make 199 '(' ^ "1" ^ String.make 199 ')'));
  run "first excessive expression depth" (fun () ->
      rejects
        (String.make 200 '(' ^ "1" ^ String.make 200 ')')
        "nesting too deep");
  if !failures <> 0 then
    failwith (Printf.sprintf "%d parser port regressions failed" !failures)
