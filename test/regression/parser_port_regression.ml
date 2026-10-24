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
  run "generated binary64 integer boundary literals" (fun () ->
      for offset = 0 to 255 do
        let integer = Int64.add 9007199254740992L (Int64.of_int offset) in
        List.iter
          (fun sign ->
            let source = sign ^ Int64.to_string integer ^ ".0" in
            if offset mod 2 = 0 then ignore (parsed source)
            else
              rejects source
                "Float literal loses precision specified in the source")
          [ ""; "-" ]
      done);
  run "float precision diagnostics cover expressions and patterns" (fun () ->
      List.iter
        (fun source ->
          List.iter
            (fun program ->
              let result = Parser.parse program in
              require
                (List.exists
                   (fun (d : ParserSupport.diagnostic) ->
                     d.ParserSupport.code = "PARSE-FLOAT-PRECISION")
                   result.ParserSupport.diagnostics)
                ("Missing precision diagnostic: " ^ program))
            [ source; "match x with | " ^ source ^ " -> 1 | _ -> 0" ])
        [
          "10000000000000001.0";
          "10000000000000000.1";
          "0.10000000000000000";
          "9007199254740993.0";
          "9007199254740995.0";
          "1.7976931348623158e308";
          "2.4703282292062328e-324";
          "1.00000000000000011102230246251565404236316680908203125";
          "1.00000000000000011102230246251565404236316680908203126";
        ]);
  run "float normalization preserves exact runtime bits" (fun () ->
      List.iter
        (fun (source, expected) ->
          match (parsed source).WT.exprsToEval with
          | [ WT.EFloat (_, negative, whole, fraction) ] ->
              let value =
                float_of_string
                  ((if negative then "-" else "") ^ whole ^ "." ^ fraction)
              in
              require
                (Int64.bits_of_float value = expected)
                ("Incorrect float bits: " ^ source);
              require
                (whole = "0" || whole.[0] <> '0')
                "Whole digits must be canonical"
          | _ -> failwith ("Not a float: " ^ source))
        [
          ("0.1", 0x3fb999999999999aL);
          ("0.10000000000000001", 0x3fb999999999999aL);
          ("0.3", 0x3fd3333333333333L);
          ("10000000000000002.0", 0x4341c37937e08001L);
          ("1.7976931348623157e308", 0x7fefffffffffffffL);
          ("2.2250738585072014e-308", 0x0010000000000000L);
          ("2.225073858507201e-308", 0x000fffffffffffffL);
          ("0.000001e1", 0x3ee4f8b588e368f1L);
          ("0.00000001e1", 0x3e7ad7f29abcaf48L);
          ("00.0000001", 0x3e7ad7f29abcaf48L);
          ("5e-324", 1L);
          ("-0.0", Int64.min_int);
          ("0e999999999999999999999", 0L);
          ("1" ^ String.make 417 '0' ^ "e-401", 0x4341c37937e08000L);
          ("0." ^ String.make 416 '0' ^ "1e401", 0x3c9cd2b297d889bcL);
        ]);
  run "float range diagnostics are independent of exponent spelling" (fun () ->
      List.iter
        (fun source -> rejects source "Float literal overflows binary64")
        [ "1e309"; "1e401"; "1e999999999999999999999" ];
      List.iter
        (fun source ->
          rejects source "Nonzero float literal underflows to zero")
        [ "2e-324"; "1e-401"; "-1e-401"; "1e-999999999999999999999" ]);
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
