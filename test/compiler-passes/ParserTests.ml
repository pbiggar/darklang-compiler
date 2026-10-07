(*
   ParserTests.ml - Focused syntax and lexer checks for the copied interpreter parser.
*)
(* ParserTests.ml - Translate every original focused parser and lexer test. *)
[@@@warning "-4-42"]

open Dark_compiler
module WT = WrittenTypes

let parseSource source =
  Result.map Validation.ValidatedSourceFile.toWrittenTypes
    (WrittenParsing.parse Validation.Script source)

let longNumbers () =
  let count = 2000 in
  match Lexer.tokenize (String.concat " " (List.init count (fun _ -> "0"))) with
  | Ok (tokens, []) when List.length tokens >= count -> Ok ()
  | Ok (tokens, diagnostics) ->
      Error
        (Printf.sprintf
           "Expected %d numeric tokens without diagnostics; got %d tokens and \
            %d diagnostics"
           count (List.length tokens) (List.length diagnostics))
  | Error error -> Error error

let tupleLet () =
  Result.bind
    (parseSource
       "let pairsToStrings (pairs: List<(String * String)>) : List<String> =\n\
       \    Stdlib.List.map<(String * String), String> pairs (fun pair -> let \
        (key, value) = pair in key ++ value)\n\
        let identity (value: String) : String = value") (fun parsed ->
      match parsed.WT.declarations with
      | [ WT.DFunction _; WT.DFunction _ ] -> Ok ()
      | declarations ->
          Error
            (Printf.sprintf "Expected two function declarations, got %d"
               (List.length declarations)))

let multipleArgs () =
  Result.bind
    (parseSource "let recurse (a: Int8) (b: Int8) : Int8 = recurse a b")
    (fun parsed ->
      match parsed.WT.declarations with
      | [
       WT.DFunction
         {
           WT.body =
             WT.EApply
               (_, _, _, [ WT.EVariable (_, "a"); WT.EVariable (_, "b") ]);
           _;
         };
      ] ->
          Ok ()
      | _ -> Error "Expected a two-argument call")

let subtraction () =
  Result.bind
    (parseSource
       "let dropLast (value: String) : Int64 = (Stdlib.String.__byteLength \
        value) - 1L") (fun parsed ->
      match parsed.WT.declarations with
      | [
       WT.DFunction
         {
           WT.body =
             WT.EInfix
               ( _,
                 (_, WT.InfixFnCall WT.ArithmeticMinus),
                 WT.EApply _,
                 WT.EInt64 _ );
           _;
         };
      ] ->
          Ok ()
      | _ -> Error "Expected subtraction after a call")

let topExpression () =
  Result.bind
    (parseSource "let identity (value: Int64) : Int64 = value\n\nidentity 1L")
    (fun parsed ->
      match (parsed.WT.declarations, parsed.WT.exprsToEval) with
      | [ WT.DFunction _ ], [ WT.EApply _ ] -> Ok ()
      | declarations, expressions ->
          Error
            (Printf.sprintf
               "Expected a function then an entry expression, got %d \
                declarations and %d expressions"
               (List.length declarations) (List.length expressions)))

let moduleScope () =
  Result.bind
    (WrittenParsing.parse Validation.Script
       "module Darklang.Example.Nested\n\n1L") (fun source ->
      Result.bind (WrittenSource.items source) (function
        | [
            WrittenSource.Expression
              ([ "Darklang"; "Example"; "Nested" ], WT.EInt64 _);
          ] ->
            Ok ()
        | _ -> Error "Expected a module-scoped expression"))

let powerAndXor () =
  let check source expected =
    Result.bind (parseSource source) (fun parsed ->
        match parsed.WT.exprsToEval with
        | [ WT.EInfix (_, (_, WT.InfixFnCall actual), _, _) ]
          when actual = expected ->
            Ok ()
        | _ -> Error ("Unexpected interpreter parse for " ^ source))
  in
  Result.bind (check "2 ** 3" WT.ArithmeticPower) (fun () ->
      check "2 ^ 3" WT.BitwiseXor)

let tests =
  [
    ("Long numeric token streams are stack safe", longNumbers);
    ("Tuple lets do not open nested function layout", tupleLet);
    ("Space application keeps multiple arguments", multipleArgs);
    ("Subtraction follows a parenthesized call", subtraction);
    ("Top-level expressions follow function declarations", topExpression);
    ("Module expressions retain their resolution scope", moduleScope);
    ("Copied interpreter distinguishes power and xor", powerAndXor);
  ]
