(*
   TypeRegistries.ml - Describe source record, alias, and variable registries used during preparation.
*)
(* Source record, alias, variable, and semantic-identity preparation registries. *)
[@@@warning "-4"]

module M = StringOrder.Map
module S = StringOrder.Set

type recordTypeInfo = {
  typeParams : string list;
  fields : (string * AST.semanticType) list;
}

(*
   Type registry - maps nominal record identities to declared metadata.
*)
type typeRegistry = recordTypeInfo M.t

let recordFieldsRegistry (registry : typeRegistry) =
  M.map (fun info -> info.fields) registry

let recordTypeParamsRegistry (registry : typeRegistry) =
  M.map (fun info -> info.typeParams) registry

(*
   Qualified constructor identities are collision-free. Bare names can
   be overwritten when several sum types expose cases such as
   ParseError.BadFormat, which previously dropped whole enum shapes.
*)
let rcSumShapeRegistryFromVariantLookup lookup =
  let sumNames =
    M.bindings lookup
    |> List.map (fun (_, (name, _, _, _)) -> name)
    |> S.of_list
  in
  let rec canonicalize = function
    | AST.TRecord (name, []) when S.mem name sumNames -> AST.TSum (name, [])
    | AST.TRecord (name, args) -> AST.TRecord (name, List.map canonicalize args)
    | AST.TSum (name, args) -> AST.TSum (name, List.map canonicalize args)
    | AST.TFunction (args, ret) ->
        AST.TFunction (List.map canonicalize args, canonicalize ret)
    | AST.TTuple items -> AST.TTuple (List.map canonicalize items)
    | AST.TList item -> AST.TList (canonicalize item)
    | AST.TDict (key, value) -> AST.TDict (canonicalize key, canonicalize value)
    | typ -> typ
  in
  let variants =
    M.bindings lookup
    |> List.filter (fun (variant, (name, _, _, _)) ->
        String.starts_with ~prefix:(name ^ ".") variant)
  in
  let grouped =
    List.fold_left
      (fun registry (_, (name, params, tag, fields)) ->
        let payload =
          match fields with
          | [] -> None
          | [ field ] -> Some field
          | fields -> Some (AST.TTuple fields)
        in
        let previousParams, previous =
          Option.value (M.find_opt name registry) ~default:(params, [])
        in
        M.add name
          (previousParams, (tag, payload, List.length fields) :: previous)
          registry)
      M.empty variants
  in
  M.map
    (fun (params, variants) ->
      ({
         MemoryModel.typeParams = params;
         payloads =
           List.stable_sort
             (fun (left, _, _) (right, _, _) -> Int.compare left right)
             variants
           |> List.map (fun (tag, typ, _) -> (tag, Option.map canonicalize typ));
         unaryPayloadTags =
           List.filter_map
             (fun (tag, _, count) -> if count = 1 then Some tag else None)
             variants
           |> MemoryModel.IntSet.of_list;
       }
        : MemoryModel.rcSumShapeInfo))
    grouped

(*
   Function registry keyed by semantic identity. Names are retained as
   definition metadata for diagnostics and backend symbol emission.
*)
type functionRegistry = (string * AST.semanticType) FunctionIdMap.t

(*
   Display metadata for every resolved function identity, including compiler
   intrinsics that do not have ordinary checked definitions.
*)
type functionNameRegistry = string FunctionIdMap.t

(*
   Resolve the small set of explicitly named lowering conventions to their
   canonical semantic identities without rebuilding the inverse name table at
   every recursive expression-lowering step.
*)
type functionIdRegistry = AST.functionId M.t

let functionIdsFromNames names =
  FunctionIdMap.toSeq names
  |> Seq.map (fun (id, name) -> (name, id))
  |> M.of_seq

type typeNameRegistry = CheckedAST.semanticMetadata

let emptyTypeNames = { CheckedAST.typeNames = CheckedAST.TypeIdMap.empty }
let typeNamesFromSymbols = CheckedAST.semanticMetadata

let tryFindConstructorTag id (_ : typeNameRegistry) =
  Some (AST.constructorRuntimeTag id)

let tryFindFieldIndex id (_ : typeNameRegistry) =
  Some (AST.fieldRuntimeIndex id)

let listHeadUnsafeFunction ids typ =
  let resolve name =
    match M.find_opt name ids with
    | Some id -> id
    | None ->
        Crash.crash
          ("List pattern helper '" ^ name
         ^ "' is absent from the function registry")
  in
  (* Int64 payloads, including JSON views, need no ownership wrapper. JSON
    field tuples still need the typed accessor to retain their string. *)
  let accessor =
    match typ with
    | AST.TTuple [ AST.TString; AST.TInt64 ] ->
        Some "Darklang.Stdlib.Json.__viewFieldListHead"
    | _ -> None
  in
  match accessor with
  | Some name when M.mem name ids -> (resolve name, false)
  | _ when typ = AST.TFloat64 ->
      (resolve "Darklang.Stdlib.List.__headUnsafeFloat", false)
  | _ -> (resolve "Darklang.Stdlib.List.__headUnsafe_i64", true)

(*
   Pattern matching reads list payloads without taking an ownership edge.
   Typed accessors materialize owned return values in their callee; the erased
   i64 accessor cannot, because its compiled return type carries no managed
   payload shape for reference-count insertion.
*)
let listHeadUnsafeExpr ids typ atom =
  let id, borrowed = listHeadUnsafeFunction ids typ in
  if borrowed then ANF.BorrowedCall (id, [ atom ]) else ANF.Call (id, [ atom ])

(*
   Alias registry - maps type alias names to their type params and target types
   For simple record aliases: "Vec" -> ([], TRecord "Point")
*)
type aliasRegistry = (string list * AST.semanticType) M.t

let canonicalizeBareSumTypeRefsWithPredicate isSum typ =
  let rec visit = function
    | AST.TRecord (name, []) when isSum name -> AST.TSum (name, [])
    | AST.TRecord (name, args) -> AST.TRecord (name, List.map visit args)
    | AST.TSum (name, args) -> AST.TSum (name, List.map visit args)
    | AST.TFunction (args, ret) -> AST.TFunction (List.map visit args, visit ret)
    | AST.TTuple items -> AST.TTuple (List.map visit items)
    | AST.TList item -> AST.TList (visit item)
    | AST.TStream item -> AST.TStream (visit item)
    | AST.TDict (key, value) -> AST.TDict (visit key, visit value)
    | typ -> typ
  in
  visit typ

let canonicalizeBareSumTypeRefsWithNames names =
  canonicalizeBareSumTypeRefsWithPredicate (fun name -> S.mem name names)

let canonicalizeBareSumTypeRefs lookup =
  canonicalizeBareSumTypeRefsWithPredicate (fun name ->
      M.exists (fun _ (owner, _, _, _) -> name = owner) lookup)

let canonicalizeNamedTypeRefs records sums typ =
  let rec visit = function
    | AST.TSum (name, args) when S.mem name records && not (S.mem name sums) ->
        AST.TRecord (name, List.map visit args)
    | AST.TRecord (name, args) when S.mem name sums ->
        AST.TSum (name, List.map visit args)
    | AST.TRecord (name, args) -> AST.TRecord (name, List.map visit args)
    | AST.TSum (name, args) -> AST.TSum (name, List.map visit args)
    | AST.TFunction (args, ret) -> AST.TFunction (List.map visit args, visit ret)
    | AST.TTuple items -> AST.TTuple (List.map visit items)
    | AST.TList item -> AST.TList (visit item)
    | AST.TDict (key, value) -> AST.TDict (visit key, visit value)
    | typ -> typ
  in
  visit typ

(*
   Resolve a type name through the alias registry
   If the name is an alias for a record type, returns the resolved record name
   Otherwise returns the original name
*)
let rec resolveRecordTypeName aliases name =
  match M.find_opt name aliases with
  | Some ([], AST.TRecord (target, _) | [], AST.TSum (target, _)) ->
      resolveRecordTypeName aliases target
  | _ -> name

let rec resolveAliasTypeForRegistry aliases typ =
  let visit = resolveAliasTypeForRegistry aliases in
  match typ with
  | AST.TRecord (name, []) | AST.TSum (name, []) -> (
      match M.find_opt name aliases with
      | Some ([], target) -> visit target
      | _ -> typ)
  | AST.TRecord (name, args) -> AST.TRecord (name, List.map visit args)
  | AST.TSum (name, args) -> AST.TSum (name, List.map visit args)
  | AST.TTuple items -> AST.TTuple (List.map visit items)
  | AST.TList item -> AST.TList (visit item)
  | AST.TStream item -> AST.TStream (visit item)
  | AST.TDict (key, value) -> AST.TDict (visit key, visit value)
  | AST.TFunction (args, ret) -> AST.TFunction (List.map visit args, visit ret)
  | _ -> typ

let resolveRegistryFields aliases =
  List.map (fun (name, typ) -> (name, resolveAliasTypeForRegistry aliases typ))

(*
   Expand a type registry to include alias entries
   If "Vec" aliases to "Point" and "Point" has fields [x, y], then "Vec" also gets [x, y]
   Target not found, skip
   Not a non-generic record alias, skip
*)
let expandTypeRegWithAliases registry aliases =
  let resolved =
    M.map
      (fun info ->
        { info with fields = resolveRegistryFields aliases info.fields })
      registry
  in
  M.fold
    (fun alias (params, target) acc ->
      match (params, target) with
      | [], AST.TRecord (target, _) -> (
          match M.find_opt (resolveRecordTypeName aliases target) resolved with
          | Some info -> M.add alias { info with typeParams = params } acc
          | None -> acc)
      | _ -> acc)
    aliases resolved

module BindingMap = CheckedAST.BindingIdMap

(*
   Variable environment - maps variable names to their TempIds and types
   The type information is used for type-directed field lookup in record access
*)
type varEnv = (ANF.tempId * AST.semanticType) BindingMap.t

(*
   Extract just the type environment from VarEnv for use with inferType
   Monomorphization Support for Generic Functions
   The Dark compiler uses monomorphization to handle generics - each generic
   function instantiation becomes a separate specialized function with a
   mangled name (e.g., identity<Int64> → identity_i64).
   Algorithm:
   1. Collect all generic function definitions (functions with TypeParams)
   2. Scan for TypeApp expressions (calls to generic functions with type args)
   3. For each unique (funcName, [typeArgs]) pair:
   - Substitute type parameters with concrete types in the function body
   - Generate a specialized function with mangled name
   4. Replace all TypeApp calls with regular Calls to mangled names
   5. Iterate until fixed-point (new specializations may contain more TypeApps)
   Key design decisions:
   - No runtime type info: all types resolved at compile time
   - Name mangling encodes types: identity_i64, swap_str_bool
   - Iterative: handles nested generics like List<Option<T>>
   See docs/compiler/frontend/generics.md for detailed documentation.
*)
let typeEnvFromVarEnv env = BindingMap.map snd env
