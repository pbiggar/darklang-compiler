(* SpecializationIdentity.ml - Name concrete generic instances and normalize typed parameters. *)
module C = CheckedAST
module M = StringOrder.Map

module FunctionSet = Set.Make (struct
  type t = AST.functionId

  let compare left right =
    Int64.unsigned_compare (AST.functionIdValue left)
      (AST.functionIdValue right)
end)

type genericFunctionArtifact = {
  symbols : C.symbols;
  func : C.functionDef;
  directDependencies : FunctionSet.t;
}

(*
   Generic function registry - maps names to definitions together with the
   symbol namespace in which their local identities were allocated.
*)
type genericFuncDefs = genericFunctionArtifact M.t

(*
   Specialization key - a generic function instantiated with specific types
   (funcName, typeArgs)
*)
type specKey = string * AST.semanticType list

module SpecOrder = struct
  type t = specKey

  let rec compareTypes left right =
    match (left, right) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | a :: aa, b :: bb ->
        let order = AST.compareSemanticType a b in
        if order = 0 then compareTypes aa bb else order

  let compare (a, aa) (b, bb) =
    let order = StringOrder.compare a b in
    if order = 0 then compareTypes aa bb else order
end

module SpecMap = Map.Make (SpecOrder)
module SpecSet = Set.Make (SpecOrder)

(*
   Specialization registry - tracks which specializations are needed
   Maps (funcName, typeArgs) -> specialized name
*)
type specRegistry = string SpecMap.t

(*
   Result of specializing generic functions from a spec set
*)
type specializationResult = {
  specializedFuncs : genericFunctionArtifact list;
  specRegistry : specRegistry;
  externalSpecs : SpecSet.t;
  symbols : C.symbols;
}

(* Summarize direct semantic dependencies once at the checked-unit boundary.
   Specialized bodies receive a fresh summary after type substitution. *)
let rec directDependencies expr =
  let combine expressions =
    List.fold_left
      (fun calls item -> FunctionSet.union calls (directDependencies item))
      FunctionSet.empty expressions
  in
  let args values = combine (NonEmptyList.toList values) in
  match expr with
  | C.FuncRef id -> FunctionSet.singleton id
  | C.BoundaryRender (id, value) ->
      FunctionSet.add id (directDependencies value)
  | C.Call (id, values) | C.TypeApp (id, _, values) ->
      FunctionSet.add id (args values)
  | C.Closure (id, captures) -> FunctionSet.add id (combine captures)
  | C.UnaryOp (_, value) | C.TupleAccess (value, _) | C.RecordAccess (value, _)
    ->
      directDependencies value
  | C.BinOp (_, left, right)
  | C.Sequence (left, right)
  | C.Let (_, left, right)
  | C.RecursiveLet (_, left, right) ->
      combine [ left; right ]
  | C.If (condition, thenBranch, elseBranch) ->
      combine [ condition; thenBranch; elseBranch ]
  | C.TupleLiteral values -> combine (C.tupleElementsToList values)
  | C.ListLiteral values | C.Constructor (_, values) -> combine values
  | C.DictLiteral (_, _, entries) ->
      combine (List.concat_map (fun (key, value) -> [ key; value ]) entries)
  | C.RecordLiteral (_, fields) ->
      combine (List.map snd (C.recordFieldsInSourceOrder fields))
  | C.RecordUpdate (record, fields) -> combine (record :: List.map snd fields)
  | C.Match (scrutinee, cases) ->
      combine
        (scrutinee
        :: List.concat_map
             (fun (case : C.matchCase) ->
               case.C.body :: Option.to_list case.C.guard)
             (NonEmptyList.toList cases))
  | C.Lambda (_, _, body) -> directDependencies body
  | C.Apply (func, values) | C.IndirectApply (func, values) ->
      combine (func :: NonEmptyList.toList values)
  | C.InterpolatedString parts ->
      combine
        (List.filter_map
           (function
             | C.StringExpr value -> Some value | C.StringText _ -> None)
           parts)
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.Int8Literal _
  | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _ | C.UInt16Literal _
  | C.UInt32Literal _ | C.UInt64Literal _ | C.UInt128Literal _
  | C.BigIntLiteral _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.Local _ | C.RuntimeError _ ->
      FunctionSet.empty

(* Extract generic function definitions (functions with type parameters)
   from a program. Used for on-demand monomorphization of stdlib generics. *)
let extractGenericFuncDefs program =
  let symbols = C.programSymbols program in
  C.programTopLevels program
  |> List.filter_map (function
    | C.FunctionDef f when f.C.typeParams <> [] ->
        Some
          ( f.C.name,
            {
              symbols = C.catalogForCheckedUnit symbols;
              func = f;
              directDependencies = directDependencies f.C.body;
            } )
    | _ -> None)
  |> M.of_list
[@@warning "-4"]

let importSpecializedFunctions targetSymbols artifacts =
  let symbols, functions =
    List.fold_left
      (fun (symbols, functions) artifact ->
        (* Specializations are produced from immutable forks of a source symbol
     table. Equal namespace tokens therefore do not imply that ordinals
     allocated on separate forks identify the same generated function.
     Import each artifact through its names so the destination owns one
     collision-free identity namespace. *)
        let symbols, imported =
          if
            C.tryFindFunctionId artifact.func.C.name symbols
            = Some artifact.func.C.id
            && C.functionName artifact.func.C.id symbols
               = Some artifact.func.C.name
          then (symbols, [ C.FunctionDef artifact.func ])
          else
            C.composeTopLevels artifact.symbols symbols
              [ C.FunctionDef artifact.func ]
        in
        let imported =
          match imported with
          | [ C.FunctionDef func ] -> func
          | _ ->
              Crash.crash "Generic function import changed its top-level shape"
        in
        (symbols, imported :: functions))
      (targetSymbols, []) artifacts
  in
  (symbols, List.rev functions)
[@@warning "-4"]

let mangleTypeVarName name = String.concat "$u" (String.split_on_char '_' name)

(* Convert a type to a string for name mangling *)
let rec typeToMangledName = function
  | AST.TInt8 -> "i8"
  | AST.TInt16 -> "i16"
  | AST.TInt32 -> "i32"
  | AST.TInt64 -> "i64"
  | AST.TInt128 -> "i128"
  | AST.TInt -> "int"
  | AST.TUInt8 -> "u8"
  | AST.TUInt16 -> "u16"
  | AST.TUInt32 -> "u32"
  | AST.TUInt64 -> "u64"
  | AST.TUInt128 -> "u128"
  | AST.TBool -> "bool"
  | AST.TFloat64 -> "f64"
  | AST.TString -> "str"
  | AST.TBlob -> "blob"
  | AST.TChar -> "char"
  | AST.TDateTime -> "datetime"
  | AST.TUnit -> "unit"
  | AST.TNever -> "runtime_error"
  | AST.TFunction (params, ret) ->
      "fn_"
      ^ String.concat "_" (List.map typeToMangledName params)
      ^ "_to_" ^ typeToMangledName ret
  | AST.TTuple types ->
      "tup"
      ^ string_of_int (List.length types)
      ^ "_"
      ^ String.concat "_" (List.map typeToMangledName types)
  | AST.TRecord (name, []) | AST.TSum (name, []) -> name
  | AST.TRecord (name, args) | AST.TSum (name, args) ->
      name ^ "_" ^ String.concat "_" (List.map typeToMangledName args)
  | AST.TList typ -> "list_" ^ typeToMangledName typ
  | AST.TStream typ -> "stream_" ^ typeToMangledName typ
  | AST.TDict (key, value) ->
      "dict_" ^ typeToMangledName key ^ "_" ^ typeToMangledName value
  | AST.TVar name ->
      mangleTypeVarName name (* Should not appear after monomorphization *)
  | AST.TInferenceVar (displayName, _) -> mangleTypeVarName displayName
  | AST.TInternalRawPtr -> "rawptr" (* Internal raw pointer type *)

(* Check if a type contains any type variables *)
let[@warning "-4"] rec containsTypeVar = function
  | AST.TVar _ | AST.TInferenceVar _ -> true
  | AST.TFunction (params, ret) ->
      List.exists containsTypeVar params || containsTypeVar ret
  | AST.TTuple types | AST.TRecord (_, types) | AST.TSum (_, types) ->
      List.exists containsTypeVar types
  | AST.TList typ -> containsTypeVar typ
  | AST.TDict (key, value) -> containsTypeVar key || containsTypeVar value
  | _ -> false

(* Generate a specialized function name *)
let specName name types =
  if types = [] then name
  else name ^ "_" ^ String.concat "_" (List.map typeToMangledName types)

let isGenericKeyIntrinsicName name = name = "__hash" || name = "__key_eq"
let exprArgsToList = NonEmptyList.toList

let exprArgsFromList args =
  match NonEmptyList.tryFromList args with
  | Some args -> args
  | None -> NonEmptyList.singleton C.UnitLiteral

let paramsToList = NonEmptyList.toList
let lambdaParameterType (param : C.lambdaParameter) = C.semanticType param.C.typ

let rec letPatternBindingTypes pattern typ =
  match (pattern, typ) with
  | C.LPVariable name, typ -> [ (name, typ) ]
  | C.LPWildcard, _ | C.LPUnit, _ -> []
  | C.LPTuple (first, second, rest), AST.TTuple types ->
      let patterns = first :: second :: rest in
      if List.length patterns <> List.length types then
        Crash.crash
          "Typed lambda tuple pattern changed arity before ANF lowering"
      else
        List.concat_map
          (fun (pattern, typ) -> letPatternBindingTypes pattern typ)
          (List.combine patterns types)
  | C.LPTuple _, _ ->
      Crash.crash
        "Typed lambda tuple pattern lost its tuple type before ANF lowering"
[@@warning "-4"]

let lambdaParameterBindings (param : C.lambdaParameter) =
  letPatternBindingTypes param.C.pattern (lambdaParameterType param)

let lowerLambdaParameters symbols parameters body =
  let reversed, symbols =
    List.fold_left
      (fun (reversed, symbols) (index, (param : C.lambdaParameter)) ->
        let typ = lambdaParameterType param in
        match param.C.pattern with
        | C.LPVariable id -> (((id, typ), None) :: reversed, symbols)
        | pattern ->
            let id, symbols =
              C.allocateBinding
                ("__lambda_pattern_arg_" ^ string_of_int index)
                symbols
            in
            (((id, typ), Some (pattern, id)) :: reversed, symbols))
      ([], symbols)
      (List.mapi
         (fun index value -> (index, value))
         (NonEmptyList.toList parameters))
  in
  let lowered = List.rev reversed in
  let body =
    List.fold_right
      (fun (pattern, id) continuation ->
        C.Let (pattern, C.Local id, continuation))
      (List.filter_map snd lowered)
      body
  in
  (List.map fst lowered, body, symbols)
[@@warning "-4"]

let paramsFromList context params =
  match NonEmptyList.tryFromList params with
  | Some params -> params
  | None ->
      Crash.crash ("Internal error: " ^ context ^ " produced zero parameters")

let syntheticUnitParamPrefix = "$unit"

let isSyntheticUnitParam symbols (id, typ) =
  typ = AST.TUnit
  && Option.fold ~none:false
       ~some:(String.starts_with ~prefix:syntheticUnitParamPrefix)
       (C.bindingName id symbols)

let normalizeSyntheticNullaryParams symbols params =
  match params with
  | [ single ] when isSyntheticUnitParam symbols single -> []
  | _ -> params

let[@warning "-4"] normalizeSyntheticNullaryArgAtoms types expressions atoms =
  match (types, expressions, atoms) with
  | [], [ C.UnitLiteral ], [ _ ] -> []
  | _ -> atoms

let unresolvedKeyIntrinsicTypeArgErrorExpr runtimeErrorId name =
  C.Call
    ( runtimeErrorId,
      NonEmptyList.singleton
        (C.StringLiteral
           ("Internal error: unresolved type arguments for " ^ name)) )

(* Preserve left-to-right argument evaluation before forcing a runtime error. *)
(*
   Type substitution - maps type variable names to concrete types
*)
let wrapWithIgnoredArgEvaluations args body =
  List.fold_left
    (fun acc arg -> C.Let (C.LPWildcard, arg, acc))
    body (List.rev args)
