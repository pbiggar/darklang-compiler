(*
   WrittenSource.ml - Preserve interpreter declarations and module scopes for direct source checking.
*)
(* WrittenSource.ml - Flatten module-scoped declarations and retain source name references. *)
[@@@warning "-4"]

module WT = WrittenTypes

(*
   The checker receives source syntax with its scope, without constructing a
   separate untyped semantic program.
*)
type item =
  | Function of string list * WT.fnDecl
  | Value of string list * WT.valueDecl
  | Type of string list * WT.typeDecl
  | Expression of string list * WT.expr

let moduleSegments name =
  match NameSyntax.tryParseLegacySpelling name with
  | Some qualified ->
      Ok (List.map NameSyntax.identifierText (NameSyntax.segments qualified))
  | None -> Error ("Invalid parsed module name '" ^ name ^ "'")

let rec flattenDeclaration path = function
  | WT.DFunction fn -> Ok [ Function (path, fn) ]
  | WT.DValue value -> Ok [ Value (path, value) ]
  | WT.DType typ -> Ok [ Type (path, typ) ]
  | WT.DExpr expression -> Ok [ Expression (path, expression) ]
  | WT.DModule modul ->
      Result.bind
        (moduleSegments (snd modul.WT.name))
        (fun segments ->
          flattenDeclarations (path @ segments) modul.WT.declarations)
  | WT.DTypeDB _ -> Error "Test-only database declaration in executable source"
  | WT.DTest _ -> Error "Test assertion in executable source"

and flattenDeclarations path declarations =
  Result.map List.rev
    (List.fold_left
       (fun state declaration ->
         Result.bind state (fun reversed ->
             Result.map
               (fun current -> List.rev current @ reversed)
               (flattenDeclaration path declaration)))
       (Ok []) declarations)

let items validated =
  let source = Validation.ValidatedSourceFile.toWrittenTypes validated in
  Result.map
    (fun declarations ->
      declarations
      @ List.map
          (fun expression -> Expression ([], expression))
          source.WT.exprsToEval)
    (flattenDeclarations [] source.WT.declarations)

(*
   Enforce source-unit entry ownership before composing declarations for checking.
*)
let validateSourceUnits requireEntry units =
  Result.bind
    (ResultList.traverse
       (fun (name, purpose, source) ->
         Result.bind (items source) (fun declarations ->
             let count =
               List.fold_left
                 (fun count -> function
                   | Expression _ -> count + 1 | _ -> count)
                 0 declarations
             in
             if count > 0 && purpose <> NameSyntax.SourceUnitPurpose.Executable
             then
               let purpose =
                 match purpose with
                 | NameSyntax.SourceUnitPurpose.Executable -> "Executable"
                 | NameSyntax.SourceUnitPurpose.Library -> "Library"
                 | NameSyntax.SourceUnitPurpose.Package -> "Package"
               in
               Error
                 (Printf.sprintf
                    "Source unit '%s' has %d executable entry expression(s), \
                     but %s units must contain declarations only"
                    name count purpose)
             else Ok (source, count)))
       units)
    (fun validated ->
      let count =
        List.fold_left (fun count (_, value) -> count + value) 0 validated
      in
      if requireEntry && count <> 1 then
        Error
          (Printf.sprintf
             "Executable program must contain exactly one entry expression; \
              found %d"
             count)
      else if (not requireEntry) && count <> 0 then
        Error
          (Printf.sprintf
             "Declaration-only program must not contain entry expressions; \
              found %d"
             count)
      else Ok (List.map fst validated))

let qualifiedFn (name : WT.qualifiedFnIdentifier) =
  String.concat "."
    (List.map
       (fun ((identifier : WT.identifier), _) -> identifier.WT.name)
       name.WT.modules
    @ [ name.WT.fn.WT.name ])

let qualifiedType (name : WT.qualifiedTypeIdentifier) =
  String.concat "."
    (List.map
       (fun ((identifier : WT.identifier), _) -> identifier.WT.name)
       name.WT.modules
    @ [ name.WT.typ.WT.name ])

let rec typeNames = function
  | WT.TCustom name ->
      qualifiedType name :: List.concat_map typeNames name.WT.typeArgs
  | WT.TList (_, _, _, inner, _) -> typeNames inner
  | WT.TDict (_, _, _, key, _, value, _) -> typeNames key @ typeNames value
  | WT.TTuple (_, first, _, second, rest, _, _) ->
      List.concat_map typeNames (first :: second :: List.map snd rest)
  | WT.TFn (_, args, result) ->
      List.concat_map (fun (arg, _) -> typeNames arg) args @ typeNames result
  | _ -> []

let rec expressionNames expression =
  let many expressions = List.concat_map expressionNames expressions in
  match expression with
  | WT.EFnName (_, name) -> [ qualifiedFn name ]
  | WT.EVariable (_, name) -> [ name ]
  | WT.EApply (_, target, types, args) ->
      expressionNames target @ List.concat_map typeNames types @ many args
  | WT.EInfix (_, _, left, right) | WT.EStatement (_, left, right) ->
      many [ left; right ]
  | WT.ELet (_, _, value, body, _, _) -> many [ value; body ]
  | WT.EIf (_, condition, yes, no, _, _, _) ->
      many ([ condition; yes ] @ Option.to_list no)
  | WT.EList (_, elements, _, _) -> many (List.map fst elements)
  | WT.ETuple (_, first, _, second, rest, _, _) ->
      many (first :: second :: List.map snd rest)
  | WT.ERecordFieldAccess (_, record, _, _) -> expressionNames record
  | WT.ELambda (_, _, body, _, _) -> expressionNames body
  | WT.ERecord (_, name, fields, _, _) ->
      qualifiedType name
      :: (List.concat_map typeNames name.WT.typeArgs
         @ many (List.map (fun (_, _, value) -> value) fields))
  | WT.EDict (_, entries, _, _, _) ->
      List.concat_map (fun (_, key, _, value) -> many [ key; value ]) entries
  | WT.ERecordUpdate (_, record, updates, _, _, _) ->
      expressionNames record
      @ many (List.map (fun (_, _, value) -> value) updates)
  | WT.EEnum (_, name, _, fields, _) ->
      qualifiedType name
      :: (List.concat_map typeNames name.WT.typeArgs @ many fields)
  | WT.EMatch (_, scrutinee, cases, _, _) ->
      expressionNames scrutinee
      @ List.concat_map
          (fun arm ->
            (match arm.WT.whenCondition with
              | None -> []
              | Some (_, condition) -> expressionNames condition)
            @ expressionNames arm.WT.rhs)
          cases
  | WT.EPipe (_, initial, segments) ->
      expressionNames initial
      @ List.concat_map
          (fun (_, segment) ->
            match segment with
            | WT.EPipeInfix (_, _, value) -> expressionNames value
            | WT.EPipeLambda (_, _, body, _, _) -> expressionNames body
            | WT.EPipeEnum (_, name, _, fields, _) ->
                qualifiedType name
                :: (List.concat_map typeNames name.WT.typeArgs @ many fields)
            | WT.EPipeFnCall (_, name, typeArgs, args) ->
                qualifiedFn name
                :: (List.concat_map typeNames typeArgs @ many args)
            | WT.EPipeVariableOrFnCall (_, name) -> [ name ])
          segments
  | WT.EString (_, _, segments, _, _) ->
      List.concat_map
        (function
          | WT.StringText _ -> []
          | WT.StringInterpolation (_, value, _, _) -> expressionNames value)
        segments
  | _ -> []

(*
   Package lookup needs qualified references from source, not a lowered AST.
*)
let qualifiedNames units =
  Result.map
    (fun groups ->
      let names =
        List.concat_map
          (function
            | Function (_, definition) ->
                List.concat_map
                  (function
                    | WT.FPUnit _ -> []
                    | WT.FPNormal (_, _, typ, _, _, _, _) -> typeNames typ)
                  definition.WT.parameters
                @ typeNames definition.WT.returnType
                @ expressionNames definition.WT.body
            | Value (_, definition) -> expressionNames definition.WT.body
            | Type (_, definition) -> (
                match definition.WT.definition with
                | WT.TDAlias target -> typeNames target
                | WT.TDRecord fields ->
                    List.concat_map
                      (fun ((field : WT.recordFieldSyntax), _) ->
                        typeNames field.WT.typ)
                      fields
                | WT.TDEnum cases ->
                    List.concat_map
                      (fun (_, case) ->
                        List.concat_map
                          (fun (field : WT.enumFieldSyntax) ->
                            typeNames field.WT.typ)
                          case.WT.fields)
                      cases)
            | Expression (_, expression) -> expressionNames expression)
          (List.concat groups)
      in
      let seen = Hashtbl.create 32 in
      List.filter
        (fun name ->
          if (not (String.contains name '.')) || Hashtbl.mem seen name then
            false
          else begin
            Hashtbl.add seen name ();
            true
          end)
        names)
    (ResultList.traverse items units)
