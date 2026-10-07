(*
   CheckRecordLiterals.ml - Check RecordLiteral expressions while preserving source diagnostics and order.
*)
(* CheckRecordLiterals.ml - Check RecordLiteral expressions while preserving source diagnostics and order. *)
open! AST
open CheckingDiagnostics
module M = StringOrder.Map
module S = StringOrder.Set
(*
   Type name is required (parser enforces this, but check for safety)
   Check that all fields are present and have correct types
   Check for missing fields
   Check for extra fields
   Type check each field in source order. Record layout order is
   applied during lowering, after every initializer has run.
*)
let check checkExpr env registry lookup generic warnings modules aliases expected (reference : AST.recordReference) fields =
 let ( let* ) = Result.bind in
 let name = reference.sourceTypeName in
 if name = "" then Error (GenericError "Record literal requires type name: use 'TypeName { field = value, ... }'") else
 let normalized = List.map (fun ((field : AST.recordFieldReference), value) -> (if field.sourceFieldName = "___" then "" else field.sourceFieldName), value) fields in
 let counts = List.fold_left (fun acc (name, _) -> M.add name (1 + Option.value (M.find_opt name acc) ~default:0) acc) M.empty normalized in
 match List.find_opt (fun (name, _) -> name = "") normalized, List.find_opt (fun (name, _) -> M.find name counts > 1) normalized with
 | Some _, _ -> Error (GenericError "Empty key in record creation")
 | _, Some (name, _) -> Error (GenericError ("Duplicate field `" ^ name ^ "`"))
 | None, None -> match Types.tryResolveRecordLiteralInfo aliases registry reference with
  | None -> Error (GenericError ("Unknown record type: " ^ name))
  | Some (resolvedName, aliasArgs, info) ->
    let explicitError = if reference.typeArgs = [] then None else
     let arity = match M.find_opt reference.sourceTypeName aliases with Some (params, _) -> List.length params | None -> List.length info.Types.typeParams in
     if arity = List.length reference.typeArgs then None else Some ("Record type argument arity mismatch: expected " ^ string_of_int arity ^ ", got " ^ string_of_int (List.length reference.typeArgs)) in
    match explicitError with Some message -> Error (GenericError message) | None ->
    let initialArgs = if List.length aliasArgs = List.length info.Types.typeParams then aliasArgs else [] in
    let initialSubst = match Types.buildRecordFieldSubstitutionFromParams info.Types.typeParams initialArgs with Ok subst -> subst | Error _ -> M.empty in
    let expectedFields = List.map (fun (name, typ) -> name, Types.applyTypeArguments initialSubst typ) info.Types.fields in
    let fieldMap = M.of_list normalized in
    let missing = List.filter_map (fun (name, _) -> if M.mem name fieldMap then None else Some name) expectedFields in
    if missing <> [] then Error (GenericError ("Missing fields in record literal: " ^ String.concat ", " missing)) else
    let expectedNames = S.of_list (List.map fst expectedFields) in
    let extra = List.filter_map (fun (name, _) -> if S.mem name expectedNames then None else Some name) normalized in
    if extra <> [] then Error (GenericError ("Unknown fields in record literal: " ^ String.concat ", " extra)) else
    let expectedTypes = List.fold_left (fun acc (name, typ) -> if M.mem name acc then acc else M.add name typ acc) M.empty expectedFields in
    let rec checkFields remaining acc bindings = match remaining with
     | [] -> Ok (List.rev acc, bindings)
     | (field, value) :: rest -> match M.find_opt field expectedTypes with
       | None -> Crash.crash ("Record field '" ^ field ^ "' disappeared after validation")
       | Some expectedField ->
         let legacy actual = Error (GenericError (Types.formatLegacyRecordFieldTypeError aliases field expectedField actual value)) in
         match checkExpr value env registry lookup generic warnings modules aliases (Some expectedField) with
         | Error (TypeMismatch (_, actual, _)) -> legacy actual
         | Error error -> Error error
         | Ok (actual, value) ->
           match Unification.matchTypes (Types.resolveType aliases expectedField) (Types.resolveType aliases actual) with
           | Error _ -> legacy actual
           | Ok newBindings ->
             let rec find index = function [] -> Crash.crash ("Validated record field '" ^ field ^ "' has no declaration slot") | (name, _) :: rest -> if name = field then index else find (index + 1) rest in
             let index = find 0 info.Types.fields in
             checkFields rest ((AST.resolvedRecordFieldReference resolvedName field index, value) :: acc) (bindings @ newBindings) in
    let* fields, rawBindings = checkFields normalized [] [] in
    match Unification.consolidateBindings rawBindings with
    | Error message -> Error (GenericError ("Incompatible generic record field types: " ^ message))
    | Ok subst ->
      let args = if List.length initialArgs = List.length info.Types.typeParams then List.map (Types.applySubst subst) initialArgs
       else List.map (fun name -> Option.value (M.find_opt name subst) ~default:(TVar name)) info.Types.typeParams in
      let args = match expected with Some expected -> (match Types.resolveType aliases expected with
        | TRecord (name, args) when Types.resolveTypeName aliases name = resolvedName && List.length args = List.length info.Types.typeParams -> args
        | _ -> args) | None -> args in
      let inferred = TRecord (resolvedName, args) in
      let reference = {AST.sourceTypeName = reference.sourceTypeName; resolvedTypeName = resolvedName; typeArgs = args} in
      match expected with
      | Some expected when not (Unification.typesCompatibleWithAliases aliases expected inferred) -> Error (TypeMismatch (expected, inferred, "record literal"))
      | Some _ | None -> Ok (inferred, RecordLiteral (reference, fields))
[@@warning "-4"]
