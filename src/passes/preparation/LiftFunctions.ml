(* LiftFunctions.ml - Resolve lifted function references and program-level closure wrappers. *)
[@@@warning "-4"]

module C = CheckedAST
module A = ClosureAnalysis
module P = ClosureComparisons
module L = LiftExpressions
module S = SpecializationIdentity
module T = TypeSubstitution
module R = TypeRegistries
module B = C.BindingIdMap
module BS = A.BindingSet
module TS = A.TypeListSet
module M = StringOrder.Map
module Names = StringOrder.Set

module TypeSet = Set.Make (struct
  type t = AST.semanticType

  let compare = AST.compareSemanticType
end)

let ( let* ) = Result.bind
let merge base overlay = M.fold M.add overlay base
let mergeBindings base overlay = B.fold B.add overlay base

(*
   State extended with semantic function catalogs and emitted wrappers.
*)
type liftStateWithFuncs = {
  state : A.liftState;
  funcParams : AST.semanticType list FunctionIdMap.t;
  generatedWrappers : (AST.functionId * AST.functionId) FunctionIdMap.t;
}

type functionCatalog = {
  params : AST.semanticType list FunctionIdMap.t;
  returnTypes : AST.semanticType FunctionIdMap.t;
  genericDefs : (string list * AST.semanticType) FunctionIdMap.t;
}

(*
   Add function parameters to the type environment
   Restore original TypeEnv (remove parameters) after processing the function
*)
let liftLambdasInFunc (func : C.functionDef) state =
  let bindings =
    C.functionParameterTypes func
    |> NonEmptyList.toList |> List.to_seq |> B.of_seq
  in
  let child =
    { state with A.typeEnv = mergeBindings state.A.typeEnv bindings }
  in
  let* body, next = L.liftLambdasInExpr func.C.body child in
  Ok ({ func with C.body }, { next with A.typeEnv = state.A.typeEnv })

let ordinal id =
  let value = AST.functionIdValue id in
  Z.to_string
    (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)
     else Z.of_int64 value)

(*
   Generate a wrapper for a named function used as a value.
   Create wrapper: __funcref_wrapper_N(__closure, ...params) = origFunc(...params)
*)
let generateFuncWrapper original params returns withFuncs =
  match
    ( FunctionIdMap.tryFind original params,
      FunctionIdMap.tryFind original returns )
  with
  | Some parameters, Some returnType ->
      let name, named =
        A.freshLiftedName withFuncs.state "__funcref_wrapper_"
      in
      let storage = AST.TInternalRawPtr in
      let closure, symbols = C.allocateBinding "__closure" named.A.symbols in
      let parameters, symbols =
        List.fold_left
          (fun (parameters, symbols) (index, typ) ->
            let id, symbols =
              C.allocateBinding ("__arg" ^ string_of_int index) symbols
            in
            ((id, typ) :: parameters, symbols))
          ([], symbols)
          (List.mapi (fun index typ -> (index, typ)) parameters)
      in
      let parameters = List.rev parameters in
      let closureParam = (closure, AST.TTuple [ AST.TInt64; storage ]) in
      let id, symbols = C.internFunction name symbols in
      let body =
        C.Call
          ( original,
            S.exprArgsFromList (List.map (fun (id, _) -> C.Local id) parameters)
          )
      in
      let func =
        {
          C.id;
          name;
          typeParams = [];
          params =
            C.checkedParams
              (S.paramsFromList "generateFuncWrapper"
                 (closureParam :: parameters));
          returnType = C.checkedType returnType;
          body;
          recursion = None;
        }
      in
      let comparison, symbols =
        P.makeClosureComparator (name ^ "__comparison") [] false
          withFuncs.state.A.variantLookup symbols
      in
      let next =
        {
          withFuncs with
          state =
            {
              named with
              A.symbols;
              liftedFunctions = comparison :: named.A.liftedFunctions;
            };
          generatedWrappers =
            FunctionIdMap.add original (id, comparison.C.id)
              withFuncs.generatedWrappers;
        }
      in
      Ok (func, next)
  | None, _ ->
      Error ("Cannot find parameters for function '" ^ ordinal original ^ "'")
  | _, None ->
      Error ("Cannot find return type for function '" ^ ordinal original ^ "'")

let rec containsIndirectApply expr =
  let occurs = containsIndirectApply in
  let arguments args = List.exists occurs (NonEmptyList.toList args) in
  match expr with
  | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _
  | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _ | C.UInt8Literal _
  | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _
  | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _ | C.BlobLiteral _
  | C.CharLiteral _ | C.FloatLiteral _ | C.RuntimeError _ | C.FuncRef _
  | C.GenericFuncRef _ ->
      false
  | C.Local _ -> false
  | C.BoundaryRender (_, value)
  | C.UnaryOp (_, value)
  | C.TupleAccess (value, _)
  | C.RecordAccess (value, _) ->
      occurs value
  | C.BinOp (_, left, right) | C.Sequence (left, right) ->
      occurs left || occurs right
  | C.Let (_, value, body) | C.RecursiveLet (_, value, body) ->
      occurs value || occurs body
  | C.If (condition, yes, no) -> occurs condition || occurs yes || occurs no
  | C.Call (_, args) | C.TypeApp (_, _, args) -> arguments args
  | C.TupleLiteral elements ->
      List.exists occurs (C.tupleElementsToList elements)
  | C.DictLiteral (_, _, entries) ->
      List.exists (fun (key, value) -> occurs key || occurs value) entries
  | C.RecordLiteral (_, fields) ->
      List.exists
        (fun (_, value) -> occurs value)
        (C.recordFieldsInSourceOrder fields)
  | C.RecordUpdate (record, fields) ->
      occurs record || List.exists (fun (_, value) -> occurs value) fields
  | C.Constructor (_, fields) | C.ListLiteral fields | C.Closure (_, fields) ->
      List.exists occurs fields
  | C.Match (value, cases) ->
      occurs value
      || List.exists
           (fun (case : C.matchCase) ->
             Option.fold ~none:false ~some:occurs case.C.guard
             || occurs case.C.body)
           (NonEmptyList.toList cases)
  | C.Lambda (_, _, body) -> occurs body
  | C.Apply (target, args) -> occurs target || arguments args
  | C.IndirectApply _ -> true
  | C.InterpolatedString parts ->
      List.exists
        (function
          | C.StringText _ -> false | C.StringExpr value -> occurs value)
        parts

(*
   A separately compiled caller may compare any closure returned by this
   compilation unit. Retain comparator metadata for function values nested in
   exported result types without requiring advance knowledge of their callers.
*)
let collectEscapingFunctionParams registry variants typ =
  let rec collect visited current =
    if TypeSet.mem current visited then TS.empty
    else
      let visited = TypeSet.add current visited in
      let many types =
        List.map (collect visited) types |> List.fold_left TS.union TS.empty
      in
      match current with
      | AST.TFunction (parameters, result) ->
          TS.add parameters (collect visited result)
      | AST.TList element | AST.TStream element -> collect visited element
      | AST.TDict (key, value) ->
          TS.union (collect visited key) (collect visited value)
      | AST.TTuple elements -> many elements
      | AST.TRecord (name, args) -> (
          match M.find_opt name registry with
          | None -> TS.empty
          | Some (info : R.recordTypeInfo) ->
              let subst =
                Option.value
                  (T.buildDeclaredRecordFieldSubst info args)
                  ~default:M.empty
              in
              List.map
                (fun (_, typ) -> T.applySubstToType subst typ)
                info.R.fields
              |> many)
      | AST.TSum (name, args) ->
          M.bindings variants
          |> List.concat_map (fun (_, (owner, parameters, _, fields)) ->
              if owner <> name then []
              else
                let subst =
                  if List.length parameters = List.length args then
                    M.of_list (List.combine parameters args)
                  else M.empty
                in
                List.map (T.applySubstToType subst) fields)
          |> List.sort_uniq AST.compareSemanticType
          |> many
      | AST.TInt64 | AST.TInt128 | AST.TInt | AST.TInt32 | AST.TInt16
      | AST.TInt8 | AST.TUInt64 | AST.TUInt128 | AST.TUInt32 | AST.TUInt16
      | AST.TUInt8 | AST.TBool | AST.TString | AST.TBlob | AST.TChar
      | AST.TDateTime | AST.TFloat64 | AST.TUnit | AST.TNever
      | AST.TInternalRawPtr | AST.TVar _ | AST.TInferenceVar _ ->
          TS.empty
  in
  collect TypeSet.empty typ

let sumNames variants =
  M.fold
    (fun _ (owner, _, _, _) names -> Names.add owner names)
    variants Names.empty

let canonicalVariants names variants =
  M.map
    (fun (owner, parameters, tag, fields) ->
      let boundary =
        String.starts_with ~prefix:"Darklang.LanguageTools.ProgramTypes." owner
        || String.starts_with ~prefix:"Darklang.LanguageTools.RuntimeTypes."
             owner
      in
      ( owner,
        parameters,
        tag,
        if boundary then
          List.map (R.canonicalizeBareSumTypeRefsWithNames names) fields
        else fields ))
    variants

let canonicalRecords names registry =
  let recordNames = M.bindings registry |> List.map fst |> Names.of_list in
  M.map
    (fun (info : R.recordTypeInfo) ->
      {
        info with
        R.fields =
          List.map
            (fun (name, typ) ->
              (name, R.canonicalizeNamedTypeRefs recordNames names typ))
            info.R.fields;
      })
    registry

(*
   Canonical type view reused by lambda lifting when a compilation unit adds
   no local type declarations. Context builders compute it once; units with
   local declarations still rebuild the merged view below.
*)
let prepareLambdaLiftBaseTypes registry variants =
  let names = sumNames variants in
  (canonicalRecords names registry, canonicalVariants names variants)

(*
   Collect function identities that are used as values (not in call position).
*)
let collectFuncRefsInExpr expr known =
  let rec collect bound expr =
    let child = collect bound in
    let many values = List.concat_map child values in
    let args values = many (NonEmptyList.toList values) in
    match expr with
    | C.BoundaryRender (_, value) -> child value
    | C.GenericFuncRef _ ->
        Crash.crash "Unspecialized function value reached lambda lifting"
    | C.FuncRef id -> if FunctionIdMap.containsKey id known then [ id ] else []
    | C.Call (_, values) | C.TypeApp (_, _, values) -> args values
    | C.Let (pattern, value, body) ->
        child value
        @ collect
            (BS.union bound (BS.of_list (C.letPatternBindings pattern)))
            body
    | C.RecursiveLet (recursion, value, body) ->
        let bound = BS.add (C.recursiveBindingId recursion) bound in
        collect bound value @ collect bound body
    | C.If (condition, yes, no) -> many [ condition; yes; no ]
    | C.Sequence (first, next) | C.BinOp (_, first, next) ->
        many [ first; next ]
    | C.UnaryOp (_, value) | C.TupleAccess (value, _) | C.RecordAccess (value, _)
      ->
        child value
    | C.TupleLiteral values -> many (C.tupleElementsToList values)
    | C.ListLiteral values | C.Constructor (_, values) | C.Closure (_, values)
      ->
        many values
    | C.DictLiteral (_, _, entries) ->
        List.concat_map (fun (key, value) -> [ key; value ]) entries |> many
    | C.RecordLiteral (_, fields) ->
        C.recordFieldsInSourceOrder fields |> List.map snd |> many
    | C.RecordUpdate (record, fields) -> many (record :: List.map snd fields)
    | C.Match (scrutinee, cases) ->
        child scrutinee
        @ (NonEmptyList.toList cases
          |> List.concat_map (fun (case : C.matchCase) ->
              let names =
                NonEmptyList.toList case.C.patterns
                |> List.concat_map C.patternBindings
                |> BS.of_list
              in
              let bound = BS.union bound names in
              Option.fold ~none:[] ~some:(collect bound) case.C.guard
              @ collect bound case.C.body))
    | C.Lambda (parameters, _, body) ->
        let names =
          NonEmptyList.toList parameters
          |> List.concat_map (fun (parameter : C.lambdaParameter) ->
              C.letPatternBindings parameter.C.pattern)
          |> BS.of_list
        in
        collect (BS.union bound names) body
    | C.Apply (target, args) | C.IndirectApply (target, args) ->
        many (target :: NonEmptyList.toList args)
    | _ -> []
  in
  collect BS.empty expr

(*
   Replace function references with wrapper references in an expression
   Monomorphize a program: collect all specializations, generate specialized functions, replace TypeApps
   Uses iterative approach: keep specializing until no new concrete TypeApps are found
*)
let replaceInExpr wrappers expr =
  let rec replace bound expr =
    let recurse = replace bound in
    match expr with
    | C.GenericFuncRef _ ->
        Crash.crash "Unspecialized function value reached wrapper lowering"
    | C.FuncRef id -> (
        match FunctionIdMap.tryFind id wrappers with
        | Some (wrapper, comparison) ->
            C.Closure (wrapper, [ C.FuncRef comparison ])
        | None -> expr)
    | C.Closure (id, captures) -> (
        match FunctionIdMap.tryFind id wrappers with
        | Some (wrapper, comparison) ->
            C.Closure
              (wrapper, C.FuncRef comparison :: List.map recurse captures)
        | None -> C.Closure (id, List.map recurse captures))
    | C.Let (pattern, value, body) ->
        let child =
          BS.union bound (BS.of_list (C.letPatternBindings pattern))
        in
        C.Let (pattern, recurse value, replace child body)
    | C.RecursiveLet (recursion, value, body) ->
        let child = BS.add (C.recursiveBindingId recursion) bound in
        C.RecursiveLet (recursion, replace child value, replace child body)
    | C.Match (value, cases) ->
        C.Match
          ( recurse value,
            NonEmptyList.map
              (fun (case : C.matchCase) ->
                let names =
                  NonEmptyList.toList case.C.patterns
                  |> List.concat_map C.patternBindings
                  |> BS.of_list
                in
                let child = BS.union bound names in
                {
                  case with
                  C.guard = Option.map (replace child) case.C.guard;
                  body = replace child case.C.body;
                })
              cases )
    | C.Lambda (parameters, annotation, body) ->
        let names =
          NonEmptyList.toList parameters
          |> List.concat_map (fun (parameter : C.lambdaParameter) ->
              C.letPatternBindings parameter.C.pattern)
          |> BS.of_list
        in
        C.Lambda (parameters, annotation, replace (BS.union bound names) body)
    | C.BoundaryRender (renderer, value) ->
        C.BoundaryRender (renderer, recurse value)
    | C.BinOp (operation, left, right) ->
        C.BinOp (operation, recurse left, recurse right)
    | C.UnaryOp (operation, value) -> C.UnaryOp (operation, recurse value)
    | C.If (condition, yes, no) ->
        C.If (recurse condition, recurse yes, recurse no)
    | C.Sequence (first, next) -> C.Sequence (recurse first, recurse next)
    | C.Call (target, args) -> C.Call (target, NonEmptyList.map recurse args)
    | C.TypeApp (target, parameters, args) ->
        C.TypeApp (target, parameters, NonEmptyList.map recurse args)
    | C.TupleLiteral elements ->
        C.TupleLiteral (C.mapTupleElements recurse elements)
    | C.TupleAccess (value, index) -> C.TupleAccess (recurse value, index)
    | C.DictLiteral (key, value, entries) ->
        C.DictLiteral
          ( key,
            value,
            List.map (fun (key, value) -> (recurse key, recurse value)) entries
          )
    | C.RecordLiteral (reference, fields) ->
        C.RecordLiteral (reference, C.mapRecordFields recurse fields)
    | C.RecordUpdate (record, fields) ->
        C.RecordUpdate
          ( recurse record,
            List.map (fun (name, value) -> (name, recurse value)) fields )
    | C.RecordAccess (record, field) -> C.RecordAccess (recurse record, field)
    | C.Constructor (reference, fields) ->
        C.Constructor (reference, List.map recurse fields)
    | C.ListLiteral values -> C.ListLiteral (List.map recurse values)
    | C.Apply (target, args) ->
        C.Apply (recurse target, NonEmptyList.map recurse args)
    | C.IndirectApply (target, args) ->
        C.IndirectApply (recurse target, NonEmptyList.map recurse args)
    | _ -> expr
  in
  replace BS.empty expr

(*
   Replace function references with wrapper references in a TopLevel
*)
let replaceFuncRefsWithWrappers wrappers = function
  | C.FunctionDef func ->
      C.FunctionDef { func with C.body = replaceInExpr wrappers func.C.body }
  | C.Expression expr -> C.Expression (replaceInExpr wrappers expr)
  | C.ValueDef value ->
      C.ValueDef { value with C.body = replaceInExpr wrappers value.C.body }
  | C.TypeDef _ as value -> value

(*
   Lift lambdas in a program, generating new top-level functions
   First pass: collect all function definitions and their parameters
   Collect user function return types
   Collect user generic function definitions (for TypeApp substitution)
   Second pass: find all functions used as values and generate wrappers
   Look for references to known functions in call arguments.
   Generate wrappers for functions used as values
   Replace function references with wrapper references in the program
   Add wrappers and lifted functions to the program
*)
let liftLambdasInProgram baseRegistry baseVariants baseFunctions program =
  let symbols = C.programSymbols program in
  let tops = C.programTopLevels program in
  let definitions =
    List.filter_map
      (function
        | C.TypeDef (_, value) -> Some (C.semanticTypeDef value) | _ -> None)
      tops
  in
  let registryBase =
    definitions
    |> List.filter_map (function
      | AST.RecordDef (name, parameters, fields) ->
          Some
            ( name,
              {
                R.typeParams = parameters;
                fields = T.firstDeclaredRecordFields fields;
              } )
      | _ -> None)
    |> M.of_list
  in
  let aliases =
    definitions
    |> List.filter_map (function
      | AST.TypeAlias (name, parameters, target) ->
          Some (name, (parameters, target))
      | _ -> None)
    |> M.of_list
  in
  let registry =
    R.expandTypeRegWithAliases
      (T.resolveAliasesInTypeRegistry aliases registryBase)
      aliases
  in
  let variants =
    let collisions = AST.collidingConstructorCaseNames definitions in
    definitions
    |> List.filter_map (function
      | AST.SumTypeDef (name, parameters, variants) ->
          Some (name, parameters, variants)
      | _ -> None)
    |> List.fold_left
         (fun lookup (owner, parameters, variants) ->
           List.fold_left
             (fun lookup (index, (variant : AST.variant)) ->
               let tag =
                 if Names.mem variant.AST.name collisions then
                   AST.constructorRuntimeIdentity owner variant.AST.name
                 else index
               in
               let info = (owner, parameters, tag, variant.AST.fields) in
               let lookup =
                 if M.mem variant.AST.name lookup then lookup
                 else M.add variant.AST.name info lookup
               in
               M.add (owner ^ "." ^ variant.AST.name) info lookup)
             lookup
             (List.mapi (fun index value -> (index, value)) variants))
         M.empty
  in
  let mergedRegistry = merge baseRegistry registry in
  let rawVariants = merge baseVariants variants in
  let names = sumNames rawVariants in
  let mergedVariants =
    if M.is_empty variants then baseVariants
    else canonicalVariants names rawVariants
  in
  let canonicalRegistry =
    if M.is_empty registry && M.is_empty variants then baseRegistry
    else canonicalRecords names mergedRegistry
  in
  let functions =
    List.filter_map
      (function C.FunctionDef func -> Some func | _ -> None)
      tops
  in
  let userParams =
    List.map
      (fun (func : C.functionDef) ->
        ( func.C.id,
          C.functionParameterTypes func |> NonEmptyList.toList |> List.map snd
        ))
      functions
    |> FunctionIdMap.ofList
  in
  let userReturns =
    List.map
      (fun (func : C.functionDef) -> (func.C.id, C.functionReturnType func))
      functions
    |> FunctionIdMap.ofList
  in
  let userGeneric =
    List.filter_map
      (fun (func : C.functionDef) ->
        if func.C.typeParams = [] then None
        else Some (func.C.id, (func.C.typeParams, C.functionReturnType func)))
      functions
    |> FunctionIdMap.ofList
  in
  let params = FunctionIdMap.merge baseFunctions.params userParams in
  let returns = FunctionIdMap.merge baseFunctions.returnTypes userReturns in
  let generic = FunctionIdMap.merge baseFunctions.genericDefs userGeneric in
  let locallyCompared =
    List.filter_map
      (fun (func : C.functionDef) ->
        if containsIndirectApply func.C.body then
          List.find_map
            (fun (_, typ) ->
              match typ with
              | AST.TFunction (parameters, _) -> Some parameters
              | _ -> None)
            (NonEmptyList.toList (C.functionParameterTypes func))
        else None)
      functions
    |> TS.of_list
  in
  let escaping =
    List.map
      (fun func ->
        collectEscapingFunctionParams canonicalRegistry mergedVariants
          (C.functionReturnType func))
      functions
    |> List.fold_left TS.union TS.empty
  in
  let comparable = TS.union locallyCompared escaping in
  let runtimeReturns =
    List.fold_left
      (fun returns name ->
        match C.tryFindFunctionId name symbols with
        | Some id -> FunctionIdMap.add id AST.TNever returns
        | None ->
            Crash.crash
              ("Builtin function '" ^ name ^ "' has no allocated identity"))
      returns
      [ "Builtin.crash" ]
  in
  let initial =
    {
      A.symbols;
      counter = 0;
      liftedFunctions = [];
      comparisonFuncs = A.ComparisonMap.empty;
      comparableFunctionParams = comparable;
      typeEnv = B.empty;
      funcParams = params;
      funcReturnTypes = runtimeReturns;
      genericFuncDefs = generic;
      typeReg = canonicalRegistry;
      variantLookup = mergedVariants;
      recursiveSelf = None;
    }
  in
  let rec process remaining state acc =
    match remaining with
    | [] -> Ok (List.rev acc, state)
    | C.FunctionDef func :: rest ->
        let* func, state = liftLambdasInFunc func state in
        process rest state (C.FunctionDef func :: acc)
    | C.Expression expr :: rest ->
        let* expr, state = L.liftLambdasInExpr expr state in
        process rest state (C.Expression expr :: acc)
    | C.ValueDef value :: rest ->
        let* body, state = L.liftLambdasInExpr value.C.body state in
        process rest state (C.ValueDef { value with C.body } :: acc)
    | (C.TypeDef _ as value) :: rest -> process rest state (value :: acc)
  in
  let* tops, next = process tops initial [] in
  let used =
    List.concat_map
      (function
        | C.FunctionDef func -> collectFuncRefsInExpr func.C.body params
        | C.Expression expr -> collectFuncRefsInExpr expr params
        | C.ValueDef value -> collectFuncRefsInExpr value.C.body params
        | _ -> [])
      (* Named references also occur in bodies moved into lifted functions.
         Those bodies need the same hidden-environment adapter as the original
         declarations; collecting only tops leaves direct-call code addresses
         in closure slots. *)
      (tops @ List.map (fun func -> C.FunctionDef func) next.A.liftedFunctions)
  in
  let _, used =
    List.fold_left
      (fun (seen, acc) id ->
        if S.FunctionSet.mem id seen then (seen, acc)
        else (S.FunctionSet.add id seen, id :: acc))
      (S.FunctionSet.empty, []) used
  in
  let used = List.rev used in
  let withFuncs =
    {
      state = next;
      funcParams = params;
      generatedWrappers = FunctionIdMap.empty;
    }
  in
  let rec generate remaining state acc =
    match remaining with
    | [] -> Ok (acc, state)
    | id :: rest ->
        let* func, state = generateFuncWrapper id params returns state in
        generate rest state (func :: acc)
  in
  let* wrappers, final = generate used withFuncs [] in
  let tops =
    List.map (replaceFuncRefsWithWrappers final.generatedWrappers) tops
  in
  let lifted =
    List.rev (wrappers @ final.state.A.liftedFunctions)
    |> List.map (fun func ->
        replaceFuncRefsWithWrappers final.generatedWrappers (C.FunctionDef func))
  in
  Ok (C.programFromCheckedParts (final.state.A.symbols, lifted @ tops))
