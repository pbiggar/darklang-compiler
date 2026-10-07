(*
   Types.ml - Define checking environments and resolve declared source types.
*)
(* Types.mli - Checking environments, substitutions, and nominal type resolution. *)
(*
   Type environment - maps variable names to their types
*)
type typeEnv = AST.semanticType StringOrder.Map.t

(*
   Function parameter-name registry - maps function names to ordered parameter names
*)
type funcParamNameRegistry = string list StringOrder.Map.t

(*
   Type registry - maps record type names to their ordered field definitions.
*)
type typeRegistry = (string * AST.semanticType) list StringOrder.Map.t

(*
   Precomputed record metadata reused across separate type-checking invocations.
*)
type recordTypeInfo = {
  fields : (string * AST.semanticType) list;
  fieldTypes : AST.semanticType StringOrder.Map.t;
  typeParams : string list;
}

(*
   Indexed view retained in TypeCheckEnv for reuse by separate compilations.
*)
type indexedTypeRegistry = recordTypeInfo StringOrder.Map.t

(*
   Sum type registry - maps sum type names to their variant lists (name, tag, fields)
*)
type sumTypeRegistry =
  (string * int * AST.semanticType list) list StringOrder.Map.t

type sumVariantInfo = {
  name : string;
  tag : int;
  fields : AST.semanticType list;
}

type sumTypeInfo = { typeParams : string list; variants : sumVariantInfo list }

(*
   Indexed sum metadata retained in TypeCheckEnv so separate compilations do
   not rebuild it from the complete constructor lookup.
*)
type indexedSumTypeRegistry = sumTypeInfo StringOrder.Map.t

(*
   Variant lookup - maps variant names to (type name, type params, tag index, field types)
   Type params are the generic type parameters of the containing sum type
*)
type variantLookup =
  (string * string list * int * AST.semanticType list) StringOrder.Map.t

(*
   Generic function registry and call-site policy controls.
   `Functions` contains entries only for functions that have type parameters.
*)
type genericFuncRegistry = {
  functions : string list StringOrder.Map.t;
  requireExplicitTypeArgsForBareCalls : bool;
}

(*
   Alias registry - maps type alias names to (type params, target type)
   Example: type Id = String -> ("Id", ([], TString))
   Example: type Outer<a> = Inner<a, Int64> -> ("Outer", (["a"], TSum("Inner", [TVar "a"; TInt64])))
*)
type aliasRegistry = (string list * AST.semanticType) StringOrder.Map.t

(*
   Type substitution - maps type variable names to concrete types
*)
type substitution = AST.semanticType StringOrder.Map.t

(*
   Collected type checking environment - can be passed to compile user code with stdlib
*)
type typeCheckEnv = {
  typeCatalog : CheckedAST.typeCatalog;
  functionCatalog : CheckedAST.functionCatalog;
  typeReg : typeRegistry;
  indexedTypeReg : indexedTypeRegistry;
  recordTypeNames : StringOrder.Set.t;
  variantLookup : variantLookup;
  indexedSumTypeReg : indexedSumTypeRegistry;
  sumTypeNames : StringOrder.Set.t;
  funcEnv : typeEnv;
  values : AST.semanticType StringOrder.Map.t;
  funcParamNames : funcParamNameRegistry;
  genericFuncReg : genericFuncRegistry;
  genericFuncDefs : AST.functionDef StringOrder.Map.t;
  moduleRegistry : AST.moduleRegistry;
  aliasReg : aliasRegistry;
  resolutionEnv : NameResolution.resolutionEnvironment;
}

open! AST
module M = StringOrder.Map
module S = StringOrder.Set

let tryFindVariant reference name lookup =
  M.find_opt
    (match constructorReferenceTypeName reference with
    | None -> name
    | Some owner -> owner ^ "." ^ name)
    lookup

let unqualifiedVariantOwnerCount name lookup =
  M.fold
    (fun key (owner, _, _, _) owners ->
      if key = owner ^ "." ^ name then S.add owner owners else owners)
    lookup S.empty
  |> S.cardinal

(*
   Merge two TypeCheckEnv, with overlay taking precedence on conflicts
   Used for separate compilation: merge stdlib env with user env
   Module registry is constant, use base
*)
let mergeTypeCheckEnv base overlay =
  let merge left right = M.fold M.add right left in
  {
    typeCatalog = overlay.typeCatalog;
    functionCatalog = overlay.functionCatalog;
    typeReg = merge base.typeReg overlay.typeReg;
    indexedTypeReg = merge base.indexedTypeReg overlay.indexedTypeReg;
    recordTypeNames = S.union base.recordTypeNames overlay.recordTypeNames;
    variantLookup = merge base.variantLookup overlay.variantLookup;
    indexedSumTypeReg = merge base.indexedSumTypeReg overlay.indexedSumTypeReg;
    sumTypeNames = S.union base.sumTypeNames overlay.sumTypeNames;
    funcEnv = merge base.funcEnv overlay.funcEnv;
    values = merge base.values overlay.values;
    funcParamNames = merge base.funcParamNames overlay.funcParamNames;
    genericFuncReg =
      {
        functions =
          merge base.genericFuncReg.functions overlay.genericFuncReg.functions;
        requireExplicitTypeArgsForBareCalls =
          base.genericFuncReg.requireExplicitTypeArgsForBareCalls
          || overlay.genericFuncReg.requireExplicitTypeArgsForBareCalls;
      };
    genericFuncDefs = merge base.genericFuncDefs overlay.genericFuncDefs;
    moduleRegistry = base.moduleRegistry;
    aliasReg = merge base.aliasReg overlay.aliasReg;
    resolutionEnv =
      NameResolution.merge base.resolutionEnv overlay.resolutionEnv;
  }

(*
   Resolve a type name through the alias registry
   If the name is an alias, recursively resolve to the underlying type name
   A name-only projection cannot preserve instantiated target arguments.
   Leave those aliases to resolveAliasTargetType, which carries substitution.
*)
let rec resolveTypeName aliases name =
  (match M.find_opt name aliases with
  | Some ([], TRecord (target, [])) -> resolveTypeName aliases target
  | Some _ | None -> name)
  [@warning "-4"]

let mapTypeChildren transform = function
  | TFunction (args, result) ->
      TFunction (List.map transform args, transform result)
  | TTuple args -> TTuple (List.map transform args)
  | TRecord (name, args) -> TRecord (name, List.map transform args)
  | TSum (name, args) -> TSum (name, List.map transform args)
  | TList inner -> TList (transform inner)
  | TStream inner -> TStream (transform inner)
  | TDict (key, value) -> TDict (transform key, transform value)
  | ( TVar _ | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128
    | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64
    | TString | TBlob | TChar | TDateTime | TUnit | TNever | TInternalRawPtr )
    as typ ->
      typ

(*
   Apply a substitution to a type, replacing type variables with concrete types
   Unbound type variable remains as-is
   Concrete types are unchanged
*)
let rec applySubstWithSeen seen subst typ =
  match typ with
  | TVar name | TInferenceVar (_, name) -> (
      if S.mem name seen then typ
      else
        match M.find_opt name subst with
        | None -> typ
        | Some value -> applySubstWithSeen (S.add name seen) subst value)
  | TFunction _ | TTuple _ | TRecord _ | TSum _ | TList _ | TStream _ | TDict _
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
  | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
  | TDateTime | TUnit | TNever | TInternalRawPtr ->
      mapTypeChildren (applySubstWithSeen seen subst) typ

(*
   Apply a substitution to a type, replacing type variables with concrete types
*)
let applySubst subst typ = applySubstWithSeen S.empty subst typ

(*
   Instantiate declared type parameters simultaneously. A replacement may use
   the same name as a parameter in a nested declaration and must not itself be
   substituted (for example Outer<'a> = Inner<String, 'a>).
*)
let rec applyTypeArguments subst typ =
  match typ with
  | TVar name | TInferenceVar (_, name) -> (
      match M.find_opt name subst with Some value -> value | None -> typ)
  | TFunction _ | TTuple _ | TRecord _ | TSum _ | TList _ | TStream _ | TDict _
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
  | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
  | TDateTime | TUnit | TNever | TInternalRawPtr ->
      mapTypeChildren (applyTypeArguments subst) typ

(*
   Collect type variable names in first-seen order.
*)
let rec collectTypeVarsInType typ acc =
  match typ with
  | TVar name | TInferenceVar (_, name) ->
      if List.mem name acc then acc else acc @ [ name ]
  | TFunction (args, result) ->
      collectTypeVarsInType result
        (List.fold_left (fun acc typ -> collectTypeVarsInType typ acc) acc args)
  | TTuple args | TRecord (_, args) | TSum (_, args) ->
      List.fold_left (fun acc typ -> collectTypeVarsInType typ acc) acc args
  | TList inner | TStream inner -> collectTypeVarsInType inner acc
  | TDict (key, value) ->
      collectTypeVarsInType value (collectTypeVarsInType key acc)
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
  | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
  | TDateTime | TUnit | TNever | TInternalRawPtr ->
      acc

let recordTypeInfo typeParams fields : recordTypeInfo =
  let reversed, fieldTypes =
    List.fold_left
      (fun (ordered, types) ((name, typ) as field) ->
        if M.mem name types then (ordered, types)
        else (field :: ordered, M.add name typ types))
      ([], M.empty) fields
  in
  { fields = List.rev reversed; fieldTypes; typeParams }

let buildRecordFieldSubstitutionFromParams params args =
  if List.length params <> List.length args then
    Error
      (Printf.sprintf "Record type argument arity mismatch: expected %d, got %d"
         (List.length params) (List.length args))
  else Ok (M.of_list (List.combine params args))

(*
   Build a substitution for generic record fields from concrete type arguments.
*)
let rec resolveAliasTargetType aliases typ =
  match typ with
  | TRecord (name, args) | TSum (name, args) -> (
      match M.find_opt name aliases with
      | Some (params, target) when List.length args <= List.length params ->
          let subst =
            M.of_list
              (List.combine
                 (List.filteri (fun index _ -> index < List.length args) params)
                 args)
          in
          resolveAliasTargetType aliases (applyTypeArguments subst target)
      | Some _ | None -> mapTypeChildren (resolveAliasTargetType aliases) typ)
  | TFunction _ | TTuple _ | TList _ | TStream _ | TDict _ | TVar _
  | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8
  | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob
  | TChar | TDateTime | TUnit | TNever | TInternalRawPtr ->
      mapTypeChildren (resolveAliasTargetType aliases) typ

let tryResolveRecordLiteralInfo aliases registry
    (reference : AST.recordReference) =
  match
    resolveAliasTargetType aliases
      (TRecord (reference.sourceTypeName, reference.typeArgs))
  with
  | TRecord (name, args) | TSum (name, args) ->
      Option.map (fun info -> (name, args, info)) (M.find_opt name registry)
  | TFunction _ | TTuple _ | TList _ | TStream _ | TDict _ | TVar _
  | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8
  | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob
  | TChar | TDateTime | TUnit | TNever | TInternalRawPtr ->
      None

(*
   Build a substitution from type parameters and type arguments
*)
let buildSubstitution params args =
  if List.length params <> List.length args then
    Error
      (Printf.sprintf "Expected %d type arguments, got %d" (List.length params)
         (List.length args))
  else Ok (M.of_list (List.combine params args))

let typeArgumentLabel count =
  if count = 1 then "type argument" else "type arguments"

let argumentLabel count = if count = 1 then "argument" else "arguments"

let formatTypeArgumentArityError name expected actual =
  Printf.sprintf "%s expects %d %s, but got %d %s" name expected
    (typeArgumentLabel expected)
    actual (typeArgumentLabel actual)

let formatValueArgumentArityError name expected actual =
  Printf.sprintf "%s expects %d %s, but got %d %s" name expected
    (argumentLabel expected) actual (argumentLabel actual)

(*
   Apply a type substitution to an expression
   This is used to propagate concrete types through nested TypeApp nodes
   Apply substitution to both type arguments and value arguments
*)
let rec applySubstToExpr subst expr =
  let visit = applySubstToExpr subst in
  match expr with
  | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
  | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
  | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
  | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _
  | RuntimeError _ ->
      expr
  | BoundaryRender (renderer, value) -> BoundaryRender (renderer, visit value)
  | BinOp (op, left, right) -> BinOp (op, visit left, visit right)
  | UnaryOp (op, value) -> UnaryOp (op, visit value)
  | Let (pattern, value, body) -> Let (pattern, visit value, visit body)
  | RecursiveLet (recursion, value, body) ->
      RecursiveLet (recursion, visit value, visit body)
  | If (condition, yes, no) -> If (visit condition, visit yes, visit no)
  | Sequence (first, next) -> Sequence (visit first, visit next)
  | Apply (func, args, values) ->
      Apply
        ( visit func,
          List.map (applySubst subst) args,
          NonEmptyList.map visit values )
  | TupleLiteral elements -> TupleLiteral (List.map visit elements)
  | TupleAccess (tuple, index) -> TupleAccess (visit tuple, index)
  | DictLiteral (keyType, valueType, entries) ->
      DictLiteral
        ( applySubst subst keyType,
          applySubst subst valueType,
          List.map (fun (key, value) -> (visit key, visit value)) entries )
  | RecordLiteral (reference, fields) ->
      RecordLiteral
        ( {
            reference with
            typeArgs = List.map (applySubst subst) reference.typeArgs;
          },
          List.map (fun (name, value) -> (name, visit value)) fields )
  | RecordUpdate (record, fields) ->
      RecordUpdate
        ( visit record,
          List.map (fun (name, value) -> (name, visit value)) fields )
  | RecordAccess (record, field) -> RecordAccess (visit record, field)
  | Constructor (reference, name, fields) ->
      Constructor (reference, name, List.map visit fields)
  | Match (scrutinee, cases) ->
      Match
        ( visit scrutinee,
          List.map
            (fun (case : AST.matchCase) ->
              {
                case with
                guard = Option.map visit case.guard;
                body = visit case.body;
              })
            cases )
  | ListLiteral elements -> ListLiteral (List.map visit elements)
  | Lambda (params, annotation, body) ->
      Lambda
        ( NonEmptyList.map
            (fun (parameter : AST.lambdaParameter) ->
              {
                parameter with
                sourceAnnotation =
                  Option.map (applySubst subst) parameter.sourceAnnotation;
                inferredType =
                  Option.map (applySubst subst) parameter.inferredType;
              })
            params,
          Option.map (applySubst subst) annotation,
          visit body )
  | IndirectApply (func, args) ->
      IndirectApply (visit func, NonEmptyList.map visit args)
  | Closure (name, captures) -> Closure (name, List.map visit captures)
  | InterpolatedString parts ->
      InterpolatedString
        (List.map
           (function
             | StringText text -> StringText text
             | StringExpr expr -> StringExpr (visit expr))
           parts)

(*
   Resolve a type by expanding any type aliases (recursively)
   Returns the fully resolved type with all aliases replaced by their targets
   Resolve type arguments first.
   Check if this record name is actually a type alias.
   Mismatched type args, return as-is (error caught elsewhere)
   Build substitution and apply to target type
   Recursively resolve in case target is also an alias
   Not an alias, it's a real record type
   Check if this sum type name is actually a type alias
   Type alias with (possibly) type arguments
   Not an alias, resolve type arguments recursively
   Primitive types and type variables are unchanged
*)
let rec resolveType aliases typ =
  match typ with
  | TRecord (name, args) -> (
      let args = List.map (resolveType aliases) args in
      match M.find_opt name aliases with
      | Some (params, target) when List.length params = List.length args ->
          resolveType aliases
            (applyTypeArguments (M.of_list (List.combine params args)) target)
      | Some _ | None -> TRecord (name, args))
  | TSum (name, args) -> (
      match M.find_opt name aliases with
      | Some (params, target) when List.length params = List.length args ->
          resolveType aliases
            (applyTypeArguments (M.of_list (List.combine params args)) target)
      | Some _ -> typ
      | None -> TSum (name, List.map (resolveType aliases) args))
  | TFunction _ | TTuple _ | TList _ | TStream _ | TDict _ | TVar _
  | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8
  | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob
  | TChar | TDateTime | TUnit | TNever | TInternalRawPtr ->
      mapTypeChildren (resolveType aliases) typ

let resolveAliasesInTypeRegistry aliases registry =
  M.map (List.map (fun (name, typ) -> (name, resolveType aliases typ))) registry

let sumTypeNamesFromVariantLookup lookup =
  M.fold (fun _ (name, _, _, _) names -> S.add name names) lookup S.empty

let indexSumTypeRegistry lookup =
  M.fold
    (fun key (name, params, tag, fields) indexed ->
      let prefix = name ^ "." in
      if not (String.starts_with ~prefix key) then indexed
      else
        let variant : sumVariantInfo =
          {
            name =
              String.sub key (String.length prefix)
                (String.length key - String.length prefix);
            tag;
            fields;
          }
        in
        match M.find_opt name indexed with
        | None ->
            M.add name { typeParams = params; variants = [ variant ] } indexed
        | Some info
          when List.exists
                 (fun (existing : sumVariantInfo) -> existing.tag = tag)
                 info.variants ->
            indexed
        | Some info ->
            M.add name
              { info with variants = info.variants @ [ variant ] }
              indexed)
    lookup M.empty
  |> M.map (fun info ->
      {
        info with
        variants =
          List.stable_sort
            (fun (left : sumVariantInfo) right ->
              Int.compare left.tag right.tag)
            info.variants;
      })

let rec canonicalizeBareSumTypeRefsWithNames names typ =
  match typ with
  | TRecord (name, []) when S.mem name names -> TSum (name, [])
  | TRecord _ | TSum _ | TFunction _ | TTuple _ | TList _ | TStream _ | TDict _
  | TVar _ | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
  | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString
  | TBlob | TChar | TDateTime | TUnit | TNever | TInternalRawPtr ->
      mapTypeChildren (canonicalizeBareSumTypeRefsWithNames names) typ

(*
   Resolve the parser's provisional generic named-type shape against nominal
   declarations. Generic spellings initially use TSum because parsing happens
   before the record and sum registries are available.
*)
let rec canonicalizeDeclaredTypeRefsWithSumTypeNames registry names typ =
  let visit = canonicalizeDeclaredTypeRefsWithSumTypeNames registry names in
  match typ with
  | TSum (name, args) when M.mem name registry && not (S.mem name names) ->
      TRecord (name, List.map visit args)
  | TRecord (name, args) when S.mem name names ->
      TSum (name, List.map visit args)
  | TRecord _ | TSum _ | TFunction _ | TTuple _ | TList _ | TStream _ | TDict _
  | TVar _ | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
  | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString
  | TBlob | TChar | TDateTime | TUnit | TNever | TInternalRawPtr ->
      mapTypeChildren visit typ

(*
   Build the indexed record metadata used by checking and generated helpers.
   Separate-compilation callers use this for user records referenced by a
   concrete stdlib specialization.
*)
let indexTypeRegistry lookup params registry =
  let names = sumTypeNamesFromVariantLookup lookup in
  M.mapi
    (fun name fields ->
      let fields =
        List.map
          (fun (field, typ) ->
            ( field,
              canonicalizeDeclaredTypeRefsWithSumTypeNames registry names typ ))
          fields
      in
      let params =
        match M.find_opt name params with
        | Some params -> params
        | None ->
            Crash.crash ("Missing declared record parameters for '" ^ name ^ "'")
      in
      recordTypeInfo params fields)
    registry

(*
   Compare two types for equality, resolving type aliases first
   This allows "Vec" and "Point" to be considered equal when Vec aliases Point
*)
let typesEqual aliases first second =
  AST.compareSemanticType
    (resolveType aliases first)
    (resolveType aliases second)
  = 0

let truncateLegacyRecordValueText text =
  let units = Text.scalars text in
  if Array.length units > 10 then Text.ofScalars (Array.sub units 0 10) ^ "..."
  else text

let formatLegacyRecordFieldTypeError aliases name expected actual expr =
  let expectedText =
    CheckingDiagnostics.typeToString (resolveType aliases expected)
  in
  let actualText =
    CheckingDiagnostics.typeToString (resolveType aliases actual)
  in
  let valueText =
    match CheckingDiagnostics.tryFormatLiteralValue expr with
    | Some value -> truncateLegacyRecordValueText value
    | None -> actualText
  in
  Printf.sprintf
    "Failed to create record. Expected %s for field `%s`, but got %s (%s)"
    expectedText name valueText
    (CheckingDiagnostics.withIndefiniteArticle actualText)
