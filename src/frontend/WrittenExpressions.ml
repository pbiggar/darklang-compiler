(* Complete direct source-expression checking with contextual inference. *)
[@@@warning "-4"]

module WT = WrittenTypes
module C = CheckedAST
open! WT
module M = StringOrder.Map
module S = StringOrder.Set
open! WrittenTypeSupport

let bind = Result.bind
let map = Result.map

(*
   The success payload does not exist for these constructors.
   TNever keeps the type precise while the runtime reports the failed unwrap.
*)
let rec checkExpression (globals : globals) locals symbols expected expression =
  let checkedLiteral expected symbols typ expr =
    match (expected, typ) with
    | Some (AST.TVar wanted), AST.TVar actual
      when wanted <> actual
           && S.mem wanted globals.typeParams
           && S.mem actual globals.typeParams ->
        Error ("Expected type parameter '" ^ wanted ^ "', got '" ^ actual ^ "'")
    | _ -> WrittenTypeSupport.checkedLiteral expected symbols typ expr
  in
  let literal = checkedLiteral expected symbols in
  let check = checkExpression globals locals in
  match expression with
  | WT.EVariable (_, name)
    when restrictedIdentifier globals.allowInternal [ name ] ->
      Error ("Internal identifier not allowed in user code: " ^ name)
  | (WT.EFnName (_, name) | WT.EApply (_, WT.EFnName (_, name), _, _))
    when restrictedIdentifier globals.allowInternal (qualifiedFnName name) ->
      Error
        ("Internal identifier not allowed in user code: "
        ^ String.concat "." (qualifiedFnName name))
  | WT.EUnit _ -> literal AST.TUnit C.UnitLiteral
  | WT.EBool (_, value) -> literal AST.TBool (C.BoolLiteral value)
  | WT.EInt (_, (_, value)) -> literal AST.TInt (C.BigIntLiteral value)
  | WT.EInt64 (_, (_, value), _) -> literal AST.TInt64 (C.Int64Literal value)
  | WT.EInt8 (_, (_, value), _) -> literal AST.TInt8 (C.Int8Literal value)
  | WT.EUInt8 (_, (_, value), _) -> literal AST.TUInt8 (C.UInt8Literal value)
  | WT.EInt16 (_, (_, value), _) -> literal AST.TInt16 (C.Int16Literal value)
  | WT.EUInt16 (_, (_, value), _) -> literal AST.TUInt16 (C.UInt16Literal value)
  | WT.EInt32 (_, (_, value), _) -> literal AST.TInt32 (C.Int32Literal value)
  | WT.EUInt32 (_, (_, value), _) -> literal AST.TUInt32 (C.UInt32Literal value)
  | WT.EUInt64 (_, (_, value), _) -> literal AST.TUInt64 (C.UInt64Literal value)
  | WT.EInt128 (_, (_, value), _) -> literal AST.TInt128 (C.Int128Literal value)
  | WT.EUInt128 (_, (_, value), _) ->
      literal AST.TUInt128 (C.UInt128Literal value)
  | WT.EFloat (_, negative, whole, fraction) -> (
      match WrittenPatternSupport.floatLiteral negative whole fraction with
      | Some value -> literal AST.TFloat64 (C.FloatLiteral value)
      | None ->
          Error
            ("Invalid Float literal '"
            ^ (if negative then "-" else "")
            ^ whole ^ "." ^ fraction ^ "'"))
  | WT.EChar (_, Some (_, value), _, _) ->
      literal AST.TChar (C.CharLiteral value)
  | WT.EChar _ -> Error "Empty Char literal"
  | WT.EString (_, _, segments, _, _) ->
      let result =
        List.fold_left
          (fun result segment ->
            bind result (fun (reversed, symbols) ->
                match segment with
                | WT.StringText (_, value) ->
                    Ok (C.StringText value :: reversed, symbols)
                | WT.StringInterpolation (_, expr, _, _) ->
                    bind (check symbols None expr) (fun (typ, expr, symbols) ->
                        if
                          Option.is_some
                            (Unification.reconcileTypes None AST.TString typ)
                        then Ok (C.StringExpr expr :: reversed, symbols)
                        else
                          Error
                            ("Expected String in string interpolation, got "
                            ^ StructuralFormat.semanticType typ))))
          (Ok ([], symbols))
          segments
      in
      bind result (fun (reversed, symbols) ->
          let expr =
            match List.rev reversed with
            | [] -> C.StringLiteral ""
            | [ C.StringText text ] -> C.StringLiteral text
            | parts -> C.InterpolatedString parts
          in
          checkedLiteral expected symbols AST.TString expr)
  | WT.EVariable (_, name) -> (
      let value =
        match M.find_opt name locals with
        | Some _ as found -> found
        | None -> resolveValue globals [ name ]
      in
      match value with
      | Some (typ, id) -> checkedLiteral expected symbols typ (C.Local id)
      | None -> (
          match resolveFunction globals [ name ] with
          | Some signature ->
              checkedLiteral expected symbols
                (AST.TFunction (signature.parameters, signature.return))
                (C.FuncRef signature.id)
          | None -> Error ("Unbound local variable '" ^ name ^ "'")))
  | WT.ELambda (range, patterns, body, keyword, arrow) ->
      WrittenLambdaSupport.check checkExpression globals locals symbols expected
        range patterns body keyword arrow
  | WT.EFnName (_, name) -> (
      let spelling = String.concat "." (qualifiedFnName name) in
      match M.find_opt spelling locals with
      | Some (typ, id) -> checkedLiteral expected symbols typ (C.Local id)
      | None -> (
          match spelling with
          | "Builtin.testNan" | "Builtin.testNan_v0" ->
              literal AST.TFloat64
                (C.FloatLiteral (Int64.float_of_bits 0xfff8000000000000L))
          | "Builtin.testInfinity" | "Builtin.testInfinity_v0" ->
              literal AST.TFloat64 (C.FloatLiteral infinity)
          | "Builtin.blobEmpty" -> literal AST.TBlob (C.BlobLiteral "")
          | _ -> (
              match resolveFunction globals (qualifiedFnName name) with
              | Some signature ->
                  checkedLiteral expected symbols
                    (AST.TFunction (signature.parameters, signature.return))
                    (C.FuncRef signature.id)
              | None -> (
                  match resolveValue globals (qualifiedFnName name) with
                  | Some (typ, id) ->
                      checkedLiteral expected symbols typ (C.Local id)
                  | None ->
                      Error ("Unknown function or value '" ^ spelling ^ "'")))))
  | WT.EApply (range, WT.EFnName (nameRange, name), typeArgs, args)
    when name.modules = [] && M.mem name.fn.name locals ->
      check symbols expected
        (WT.EApply
           (range, WT.EVariable (nameRange, name.fn.name), typeArgs, args))
  | WT.EApply (_, WT.EFnName (_, name), typeArgs, args)
    when List.mem (qualifiedFnName name)
           [
             [ "Builtin"; "negate" ];
             [ "Builtin"; "boolNot" ];
             [ "Builtin"; "bitwiseNot" ];
             [ "Builtin"; "unwrap" ];
           ] ->
      WrittenApplicationSupport.builtin checkExpression checkedLiteral globals
        locals symbols expected name.fn.name typeArgs args
  | WT.EApply (range, WT.EFnName (_, name), typeArgs, args)
    when Option.is_some (resolveFunction globals (qualifiedFnName name)) ->
      WrittenCallSupport.checkNamed checkExpression checkedLiteral globals
        locals symbols expected range name typeArgs args
  | WT.EApply (range, target, typeArgs, args) ->
      WrittenApplicationSupport.indirect checkExpression checkedLiteral globals
        locals symbols expected range target typeArgs args
  | WT.EEnum (_, name, (_, caseName), fields, _) ->
      WrittenConstructorSupport.check checkExpression checkedLiteral globals
        locals symbols expected name caseName fields
  | WT.ERecord (_, name, fields, _, _) ->
      WrittenRecordSupport.record checkExpression checkedLiteral globals locals
        symbols expected name fields
  | WT.ERecordFieldAccess (_, record, (_, name), _) ->
      WrittenRecordSupport.access checkExpression checkedLiteral globals locals
        symbols expected record name
  | WT.ERecordUpdate (_, record, updates, _, _, _) ->
      WrittenRecordSupport.update checkExpression checkedLiteral globals locals
        symbols expected record updates
  | WT.EMatch (_, scrutinee, cases, _, _) ->
      WrittenCollectionSupport.matchExpression checkExpression checkedLiteral
        globals locals symbols expected scrutinee cases
  | WT.ETuple (_, first, _, second, rest, _, _) ->
      WrittenCollectionSupport.tuple checkExpression checkedLiteral globals
        locals symbols expected
        (first :: second :: List.map snd rest)
  | WT.EList (_, contents, _, _) ->
      WrittenCollectionSupport.list checkExpression checkedLiteral globals
        locals symbols expected contents
  | WT.EDict (_, entries, _, _, _) ->
      WrittenCollectionSupport.dict checkExpression checkedLiteral globals
        locals symbols expected entries
  | WT.EPipe (range, first, segments) ->
      let lowered =
        List.fold_left
          (fun result (_, segment) ->
            map
              (fun input ->
                match segment with
                | WT.EPipeInfix (_, op, right) ->
                    WT.EInfix (range, op, input, right)
                | WT.EPipeFnCall (_, name, typeArgs, args) ->
                    WT.EApply
                      ( range,
                        WT.EFnName (name.range, name),
                        typeArgs,
                        input :: args )
                | WT.EPipeEnum (_, name, caseName, fields, dot) ->
                    WT.EEnum (range, name, caseName, input :: fields, dot)
                | WT.EPipeLambda (_, [ pattern ], body, keyword, arrow) ->
                    WT.ELet (range, pattern, input, body, keyword, arrow)
                | WT.EPipeLambda (_, patterns, body, keyword, arrow) ->
                    WT.EApply
                      ( range,
                        WT.ELambda (range, patterns, body, keyword, arrow),
                        [],
                        [ input ] )
                | WT.EPipeVariableOrFnCall (nameRange, name) ->
                    let target =
                      if M.mem name locals then WT.EVariable (nameRange, name)
                      else
                        let identifier : WT.identifier =
                          { range = nameRange; name }
                        in
                        let qualified : WT.qualifiedFnIdentifier =
                          { range = nameRange; modules = []; fn = identifier }
                        in
                        WT.EFnName (nameRange, qualified)
                    in
                    WT.EApply (range, target, [], [ input ]))
              result)
          (Ok first) segments
      in
      bind lowered (check symbols expected)
  | WT.EIf (_, condition, yes, no, _, _, _) ->
      bind (check symbols (Some AST.TBool) condition)
        (fun (_, condition, symbols) ->
          let thenResult =
            match (check symbols expected yes, expected, no) with
            | Error _, None, Some fallback ->
                bind (check symbols None fallback) (fun (typ, _, _) ->
                    check symbols (Some typ) yes)
            | Ok (typ, _, _), None, Some fallback
              when Unification.containsTVar typ ->
                bind (check symbols None fallback) (fun (typ, _, _) ->
                    check symbols (Some typ) yes)
            | result, _, _ -> result
          in
          bind thenResult (fun (thenType, yes, symbols) ->
              let elseResult =
                match no with
                | Some no ->
                    check symbols
                      (if thenType = AST.TNever then expected else Some thenType)
                      no
                | None ->
                    checkedLiteral (Some thenType) symbols AST.TUnit
                      C.UnitLiteral
              in
              bind elseResult (fun (elseType, no, symbols) ->
                  match Unification.reconcileTypes None thenType elseType with
                  | Some typ ->
                      checkedLiteral expected symbols typ
                        (C.If (condition, yes, no))
                  | None ->
                      Error
                        ("Conditional branches have incompatible types: "
                        ^ StructuralFormat.semanticType thenType
                        ^ " and "
                        ^ StructuralFormat.semanticType elseType))))
  | WT.ELet (range, pattern, value, body, _, _) ->
      WrittenLetSupport.check checkExpression globals locals symbols expected
        range pattern value body
  | WT.EStatement (_, first, next) ->
      bind (check symbols (Some AST.TUnit) first) (fun (_, first, symbols) ->
          map
            (fun (typ, next, symbols) ->
              (typ, C.Sequence (first, next), symbols))
            (check symbols expected next))
  | WT.EInfix (_, (_, infix), left, right) ->
      WrittenOperatorSupport.check checkExpression checkedLiteral globals locals
        symbols expected infix left right
  | WT.EError _ ->
      Error "Expression requires source name resolution and type checking"
