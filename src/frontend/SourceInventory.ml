(* SourceInventory.ml - Index parsed declarations and tests before checking their bodies. *)
module W = WrittenTypes
module T = Tokenizer
module A = CompilerAnnotations

type kind =
  | Function
  | Value
  | Type
  | Trait
  | Implementation
  | TraitMethod
  | NamedTest
  | Assertion
  | Expression

type entry = {
  name : string;
  kind : kind;
  position : T.pos;
  disposition : A.disposition;
}

type source = { tree : W.sourceFile; entries : entry list }

let ( let* ) = Result.bind

let kindName = function
  | Function -> "function"
  | Value -> "val"
  | Type -> "type"
  | Trait -> "trait"
  | Implementation -> "implementation"
  | TraitMethod -> "trait-method"
  | NamedTest -> "named-test"
  | Assertion -> "assertion"
  | Expression -> "expression"

let isTest = function
  | NamedTest | Assertion -> true
  | Function | Value | Type | Trait | Implementation | TraitMethod | Expression
    ->
      false

let entries tokens (tree : W.sourceFile) =
  let typeText range =
    List.filter
      (fun (token : Lexer.spannedToken) ->
        token.Lexer.range.T.start >= range.T.start
        && token.range.T.end_ <= range.T.end_)
      tokens
    |> List.map (fun (token : Lexer.spannedToken) -> token.Lexer.text)
    |> String.concat ""
  in
  let entry path name kind range =
    {
      name = String.concat "." (path @ [ name ]);
      kind;
      position = range.T.start;
      disposition = A.Run;
    }
  in
  let rec declarations path rows =
    List.concat_map
      (function
        | W.DModule modul ->
            declarations (path @ W.moduleNameParts modul) modul.W.declarations
        | W.DFunction fn ->
            [ entry path fn.W.name.W.name Function fn.W.keywordLet ]
        | W.DValue value ->
            [ entry path value.W.name.W.name Value value.W.keywordVal ]
        | W.DType typ -> [ entry path typ.W.name.W.name Type typ.W.keywordType ]
        | W.DTest test ->
            [ entry path test.W.name.W.name NamedTest test.W.keywordTest ]
        | W.DAssertion test ->
            [
              entry path
                ("assertion@" ^ string_of_int (test.W.range.T.start.T.row + 1))
                Assertion test.W.range;
            ]
        | W.DExpr expr -> [ expression path expr ]
        | W.DTrait trait_ ->
            entry path trait_.W.name.W.name Trait trait_.W.keywordTrait
            :: List.map
                 (fun (method_ : W.traitMethodDecl) ->
                   entry
                     (path @ [ trait_.W.name.W.name ])
                     method_.W.name.W.name TraitMethod method_.W.keywordLet)
                 trait_.W.methods
        | W.DImpl impl ->
            let name =
              "impl "
              ^ typeText impl.W.trait_.W.range
              ^ " for "
              ^ typeText (W.typeReferenceRange impl.W.forType)
            in
            entry path name Implementation impl.W.keywordImpl
            :: List.map
                 (function
                   | W.IMethod fn ->
                       entry (path @ [ name ]) fn.W.name.W.name Function
                         fn.W.keywordLet
                   | W.IAlias value ->
                       entry (path @ [ name ]) value.W.name.W.name Value
                         value.W.keywordVal)
                 impl.W.members)
      rows
  and expression path expr =
    let range = W.exprRange expr in
    entry path
      ("expression@" ^ string_of_int (range.T.start.T.row + 1))
      Expression range
  in
  declarations [] tree.W.declarations
  @ List.map (expression []) tree.W.exprsToEval

let parse ~reasons source =
  let parsed = Parser.parse source in
  match (parsed.ParserSupport.diagnostics, parsed.ParserSupport.parsed) with
  | [], Some (W.SourceFile tree) ->
      let* tokens, _ = Lexer.tokenize source in
      let entries = entries tokens tree in
      let items =
        List.map
          (fun entry ->
            { A.position = entry.position; isTest = isTest entry.kind })
          entries
      in
      let* annotations = A.read ~reasons ~items source in
      Ok
        {
          tree;
          entries =
            List.map
              (fun entry ->
                { entry with disposition = A.at annotations entry.position })
              entries;
        }
  | diagnostics, _ ->
      Error
        (String.concat "\n"
           (List.map (Parser.renderDiagnostic source) diagnostics))
