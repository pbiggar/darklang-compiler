(* Named records, field access, and source-ordered record updates. *)
[@@@warning "-4"]
module WT = WrittenTypes
module C = CheckedAST
open! WT
open! C
module M = StringOrder.Map
module S = StringOrder.Set
open! WrittenTypeSupport
let bind = Result.bind
let map = Result.map
let rec declarationFieldsForInference globals (entry : typeEntry) = match entry.definition with
 | WT.TDRecord fields -> ResultList.traverse (fun ((field : WT.recordFieldSyntax), _) -> map (fun typ -> snd field.name, typ) (resolveWrittenType globals.allowInternal globals.types entry.path (S.of_list entry.params) field.typ)) fields
 | WT.TDAlias target -> bind (resolveWrittenType globals.allowInternal globals.types entry.path (S.of_list entry.params) target) (function
  | AST.TRecord (name, args) -> (match M.find_opt name globals.types with Some entry when List.length entry.params = List.length args -> let subst = M.of_list (List.combine entry.params args) in map (List.map (fun (name, typ) -> name, Types.applyTypeArguments subst typ)) (declarationFieldsForInference globals entry) | _ -> Error ("Unknown record type '" ^ name ^ "'"))
  | _ -> Error "Expected a record type")
 | _ -> Error "Expected a record type"
let checkFields check globals locals symbols canonical declared fields =
 List.fold_left (fun result (fieldName, value) -> bind result (fun (reversed, seen, symbols) ->
  if S.mem fieldName seen then Error ("Duplicate record field '" ^ fieldName ^ "'") else
  match M.find_opt fieldName declared with None -> Error ("Unknown field '" ^ fieldName ^ "' on " ^ canonical) | Some (index, typ) ->
   map (fun (_, value, symbols) -> let id, symbols = C.internField canonical fieldName index symbols in (id, value) :: reversed, S.add fieldName seen, symbols) (check globals locals symbols (Some typ) value))) (Ok ([], S.empty, symbols)) fields
let record check literal globals locals symbols expected (name : WT.qualifiedTypeIdentifier) fields =
 let infer () = bind (findNamedType globals name) (fun (canonical, entry) ->
  let concrete = match expected with Some ((AST.TRecord (wanted, args) | AST.TSum (wanted, args)) as typ) -> wanted = canonical && List.length args = List.length entry.params && not (Unification.containsTVar typ) | _ -> false in
  if concrete || entry.params = [] then Ok None else
  bind (declarationFieldsForInference globals entry) (fun declared ->
   let byName = M.of_list declared in
   let inference = List.fold_left (fun result (_, (_, fieldName), value) -> bind result (fun (patterns, actuals, symbols) -> match M.find_opt fieldName byName with
    | None -> Error ("Unknown field '" ^ fieldName ^ "' on " ^ canonical)
    | Some pattern -> map (fun (actual, _, symbols) -> pattern :: patterns, actual :: actuals, symbols) (check globals locals symbols None value))) (Ok ([], [], symbols)) fields in
   bind inference (fun (patterns, actuals, _) -> map Option.some (Unification.inferTypeArgs entry.params (List.rev patterns) (List.rev actuals) None None)))) in
 bind (if name.typeArgs = [] then infer () else Ok None) (fun inferred ->
  bind (resolveNamedType globals expected name inferred) (fun (canonical, entry, args) ->
   bind (recordFields globals.allowInternal globals.types entry args) (fun declarations ->
    let declared = M.of_list (List.mapi (fun index (name, typ) -> name, (index, typ)) declarations) in
    let typeId, symbols = C.internType canonical symbols in
    bind (checkFields check globals locals symbols canonical declared (List.map (fun (_, (_, name), value) -> name, value) fields)) (fun (reversed, _, symbols) ->
     bind (C.completeRecordFields typeId (List.length declarations) (List.rev reversed)) (fun complete ->
      let reference : C.recordReference = {typeId; typeArgs = List.map C.checkedType args} in
      literal expected symbols (AST.TRecord (canonical, args)) (C.RecordLiteral (reference, complete)))))))
let access check literal globals locals symbols expected record fieldName =
 bind (check globals locals symbols None record) (fun (typ, record, symbols) -> match typ with
 | AST.TRecord (canonical, args) -> (match M.find_opt canonical globals.types with
  | None -> Error ("Unknown record type '" ^ canonical ^ "'")
  | Some entry -> bind (recordFields globals.allowInternal globals.types entry args) (fun fields ->
   match List.find_opt (fun (_, (name, _)) -> name = fieldName) (List.mapi (fun index value -> index, value) fields) with
   | None -> Error ("Unknown field '" ^ fieldName ^ "' on " ^ canonical)
   | Some (index, (_, typ)) -> let id, symbols = C.internField canonical fieldName index symbols in literal expected symbols typ (C.RecordAccess (record, id))))
 | _ -> Error "Field access requires a record value")
let update check literal globals locals symbols expected record updates =
 bind (check globals locals symbols None record) (fun (typ, record, symbols) -> match typ with
 | AST.TRecord (canonical, args) -> (match M.find_opt canonical globals.types with
  | None -> Error ("Unknown record type '" ^ canonical ^ "'")
  | Some entry -> bind (recordFields globals.allowInternal globals.types entry args) (fun fields ->
   let declared = M.of_list (List.mapi (fun index (name, typ) -> name, (index, typ)) fields) in
   bind (checkFields check globals locals symbols canonical declared (List.map (fun ((_, name), _, value) -> name, value) updates)) (fun (reversed, _, symbols) -> literal expected symbols typ (C.RecordUpdate (record, List.rev reversed)))))
 | _ -> Error "Record update requires a record value")
