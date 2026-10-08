(*
   ClosureAnalysis.ml - Track closure environments, free variables, and inferred capture types.
*)
[@@@warning "-4"]
module C = CheckedAST
module M = StringOrder.Map
module B = C.BindingIdMap
module R = TypeRegistries
module P = LoweringPrimitives
module S = SpecializationIdentity
module T = TypeSubstitution
module BindingSet = Set.Make (struct type t = AST.bindingId let compare = AST.compareBindingId end)
module TypeListOrder = struct
 type t = AST.semanticType list
 let rec compare left right = match left, right with
 | [], [] -> 0 | [], _ -> -1 | _, [] -> 1 | a :: aa, b :: bb -> let order = AST.compareSemanticType a b in if order = 0 then compare aa bb else order
end
module TypeListSet = Set.Make (TypeListOrder)
module ComparisonMap = Map.Make (struct
 type t = AST.functionId * AST.semanticType list
 let compare (left, leftTypes) (right, rightTypes) = let order = Int64.unsigned_compare (AST.functionIdValue left) (AST.functionIdValue right) in if order = 0 then TypeListOrder.compare leftTypes rightTypes else order
end)
module IntMap = Map.Make (Int)
type liftState = {
 symbols : C.symbols; counter : int; liftedFunctions : C.functionDef list;
 comparisonFuncs : string ComparisonMap.t; comparableFunctionParams : TypeListSet.t;
 typeEnv : AST.semanticType B.t; funcParams : AST.semanticType list FunctionIdMap.t;
 funcReturnTypes : AST.semanticType FunctionIdMap.t;
 genericFuncDefs : (string list * AST.semanticType) FunctionIdMap.t;
 typeReg : R.typeRegistry; variantLookup : P.variantLookup;
 recursiveSelf : (AST.bindingId * AST.bindingId * AST.semanticType * C.recursiveMember) option
}
let int32Next value = Int32.to_int (Int32.add (Int32.of_int value) 1l)
let liftedNameExists state name =
 Option.fold ~none:false ~some:(fun id -> FunctionIdMap.containsKey id state.funcParams) (C.tryFindFunctionId name state.symbols)
 || List.exists (fun (func : C.functionDef) -> func.C.name = name) state.liftedFunctions
let rec findNextLiftedNameCounter state prefix counter =
 if liftedNameExists state (prefix ^ string_of_int counter) then findNextLiftedNameCounter state prefix (int32Next counter) else counter
let freshLiftedName state prefix =
 let counter = findNextLiftedNameCounter state prefix state.counter in prefix ^ string_of_int counter, {state with counter = int32Next counter}
let mergeBindings left right = B.fold B.add right left
let rec matchPatternBindingTypes registry variants names pattern scrutinee =
 let recurse = matchPatternBindingTypes registry variants names in
 let accumulate patterns typ = List.fold_left (fun current pattern -> mergeBindings current (recurse pattern typ)) B.empty patterns in
 match pattern with
 | C.POr alternatives -> recurse (NonEmptyList.head alternatives) scrutinee
 | C.PVariable id -> B.singleton id scrutinee
 | C.PWildcard | C.PUnit | C.PInt64 _ | C.PBigInt _ | C.PInt128Literal _ | C.PInt8Literal _ | C.PInt16Literal _ | C.PInt32Literal _
 | C.PUInt8Literal _ | C.PUInt16Literal _ | C.PUInt32Literal _ | C.PUInt64Literal _ | C.PUInt128Literal _ | C.PBool _ | C.PString _ | C.PChar _ | C.PFloat _ -> B.empty
 | C.PTuple patterns -> (match scrutinee with AST.TTuple types when List.length patterns = List.length types -> List.fold_left (fun current (pattern, typ) -> mergeBindings current (recurse pattern typ)) B.empty (List.combine patterns types) | _ -> B.empty)
 | C.PConstructor (constructor, patterns) -> (match P.tryFindVariantForTypeById constructor scrutinee names variants with
   | Some (owner, parameters, _, fields) when List.length patterns = List.length fields ->
     let subst = match scrutinee with AST.TSum (name, args) when name = owner && List.length parameters = List.length args -> M.of_list (List.combine parameters args) | _ -> M.empty in
     List.fold_left (fun current (pattern, typ) -> mergeBindings current (recurse pattern (T.applySubstToType subst typ))) B.empty (List.combine patterns fields)
   | _ -> B.empty)
 | C.PList patterns -> (match scrutinee with AST.TList typ -> accumulate patterns typ | _ -> B.empty)
 | C.PListCons (head, tail) -> (match scrutinee with AST.TList typ -> let head = accumulate head typ in mergeBindings head (recurse tail scrutinee) | _ -> B.empty)
let lambdaNeedsComparison parameters state =
 NonEmptyList.toList parameters |> List.map S.lambdaParameterType |> fun types -> TypeListSet.mem types state.comparableFunctionParams
(*
   Collect free variables in an expression (variables not bound by let or lambda parameters)
   Closure captures may contain free variables
*)
let rec freeVars expr bound =
 let recurse expr = freeVars expr bound in
 let many values = List.map recurse values |> List.fold_left BindingSet.union BindingSet.empty in
 let args values = many (NonEmptyList.toList values) in
 let withBindings ids = List.fold_left (fun bound id -> BindingSet.add id bound) bound ids in
 match expr with
 | C.UnitLiteral | C.Int64Literal _ | C.Int128Literal _ | C.BigIntLiteral _ | C.Int8Literal _ | C.Int16Literal _ | C.Int32Literal _
 | C.UInt8Literal _ | C.UInt16Literal _ | C.UInt32Literal _ | C.UInt64Literal _ | C.UInt128Literal _ | C.BoolLiteral _ | C.StringLiteral _
 | C.BlobLiteral _ | C.CharLiteral _ | C.FloatLiteral _ | C.RuntimeError _ | C.FuncRef _ -> BindingSet.empty
 | C.Local id -> if BindingSet.mem id bound then BindingSet.empty else BindingSet.singleton id
 | C.BoundaryRender (_, value) | C.UnaryOp (_, value) | C.TupleAccess (value, _) | C.RecordAccess (value, _) -> recurse value
 | C.BinOp (_, left, right) | C.Sequence (left, right) -> BindingSet.union (recurse left) (recurse right)
 | C.Let (pattern, value, body) -> let values = recurse value in let body = freeVars body (withBindings (C.letPatternBindings pattern)) in BindingSet.union values body
 | C.RecursiveLet (recursion, value, body) -> let bound = BindingSet.add (C.recursiveBindingId recursion) bound in BindingSet.union (freeVars value bound) (freeVars body bound)
 | C.If (condition, yes, no) -> BindingSet.union (recurse condition) (BindingSet.union (recurse yes) (recurse no))
 | C.Call (_, values) | C.TypeApp (_, _, values) -> args values
 | C.TupleLiteral values -> many (C.tupleElementsToList values)
 | C.ListLiteral values | C.Constructor (_, values) | C.Closure (_, values) -> many values
 | C.DictLiteral (_, _, entries) -> List.concat_map (fun (key, value) -> [recurse key; recurse value]) entries |> List.fold_left BindingSet.union BindingSet.empty
 | C.RecordLiteral (_, fields) -> C.recordFieldsInSourceOrder fields |> List.map (fun (_, value) -> recurse value) |> List.fold_left BindingSet.union BindingSet.empty
 | C.RecordUpdate (record, fields) -> let record = recurse record in let fields = List.map (fun (_, value) -> recurse value) fields |> List.fold_left BindingSet.union BindingSet.empty in BindingSet.union record fields
 | C.Match (value, cases) ->
   let value = recurse value in
   let cases = NonEmptyList.toList cases |> List.map (fun (case : C.matchCase) ->
    let names = NonEmptyList.toList case.C.patterns |> List.concat_map C.patternBindings in
    let bound = withBindings names in
    let guard = Option.fold ~none:BindingSet.empty ~some:(fun value -> freeVars value bound) case.C.guard in BindingSet.union guard (freeVars case.C.body bound)) |> List.fold_left BindingSet.union BindingSet.empty in BindingSet.union value cases
 | C.Lambda (parameters, _, body) -> let names = NonEmptyList.toList parameters |> List.concat_map (fun (parameter : C.lambdaParameter) -> C.letPatternBindings parameter.C.pattern) in freeVars body (withBindings names)
 | C.Apply (target, values) | C.IndirectApply (target, values) -> let target = recurse target in let values = args values in BindingSet.union target values
 | C.InterpolatedString parts -> List.filter_map (function C.StringText _ -> None | C.StringExpr value -> Some (recurse value)) parts |> List.fold_left BindingSet.union BindingSet.empty
(*
   Simple type inference for lambda lifting - infers types of simple expressions
   This allows let-bound variables to be captured in nested lambdas
   Two types the checker already proved compatible, where either may still
   carry what a literal leaves open (`[]` is a List<t>, `None` an Option<t>):
   the concrete side wins at every position; two variables keep the first; a
   real shape mismatch is None.
*)
let rec reconcileBranchTypes left right =
 let reconcileAll lefts rights = if List.length lefts <> List.length rights then None else
 List.fold_left (fun accumulated (left, right) -> Option.bind accumulated (fun accumulated -> Option.map (fun value -> value :: accumulated) (reconcileBranchTypes left right))) (Some []) (List.combine lefts rights) |> Option.map List.rev in
 if AST.compareSemanticType left right = 0 then Some left else match left, right with
 | AST.TVar _, concrete | concrete, AST.TVar _ -> Some concrete
 | AST.TNever, concrete | concrete, AST.TNever -> Some concrete
 | AST.TSum (left, leftArgs), AST.TSum (right, rightArgs) when left = right -> Option.map (fun args -> AST.TSum (left, args)) (reconcileAll leftArgs rightArgs)
 | AST.TRecord (left, leftArgs), AST.TRecord (right, rightArgs) when left = right -> Option.map (fun args -> AST.TRecord (left, args)) (reconcileAll leftArgs rightArgs)
 | AST.TList left, AST.TList right -> Option.map (fun value -> AST.TList value) (reconcileBranchTypes left right)
 | AST.TTuple left, AST.TTuple right -> Option.map (fun values -> AST.TTuple values) (reconcileAll left right)
 | AST.TDict (leftKey, leftValue), AST.TDict (rightKey, rightValue) -> Option.bind (reconcileBranchTypes leftKey rightKey) (fun key -> Option.map (fun value -> AST.TDict (key, value)) (reconcileBranchTypes leftValue rightValue))
 | AST.TFunction (leftParameters, leftResult), AST.TFunction (rightParameters, rightResult) -> Option.bind (reconcileAll leftParameters rightParameters) (fun parameters -> Option.map (fun result -> AST.TFunction (parameters, result)) (reconcileBranchTypes leftResult rightResult))
 | _ -> None
(*
   Recursively infer types of tuple elements
   Open at the element: reconciled against the other arm or branch.
   Sum type constructor has the sum type; infer generic args from fields when possible.
   `a != b` parses as Not (a == b), so a lambda ending in it is common.
   Look up the generic function's definition and apply type substitution
   Build substitution from type params to type args
   Fall back to funcReturnTypes for non-generic or arity mismatch
   Arms agree up to what a literal leaves open: `[]` is a List<t> next
   to a List<Int64> arm, `None` an Option<t> next to a Some.
   Complex expressions require full type inference
*)
let rec simpleInferType expr typeEnv funcParams funcReturns genericDefs typeReg variants names =
 let infer expr = simpleInferType expr typeEnv funcParams funcReturns genericDefs typeReg variants names in
 let inferWith environment expr = simpleInferType expr environment funcParams funcReturns genericDefs typeReg variants names in
 let fieldIndex id = match R.tryFindFieldIndex id names with Some value -> value | None -> Crash.crash "Checked field identity is absent from layout metadata" in
 let integer = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TInt | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128 -> true | _ -> false in
 let numeric typ = integer typ || typ = AST.TFloat64 in
 let collect values = List.fold_right (fun value result -> Option.bind value (fun value -> Option.map (fun values -> value :: values) result)) values (Some []) in
 match expr with
 | C.Int64Literal _ -> Some AST.TInt64 | C.Int128Literal _ -> Some AST.TInt128 | C.BigIntLiteral _ -> Some AST.TInt
 | C.Int8Literal _ -> Some AST.TInt8 | C.Int16Literal _ -> Some AST.TInt16 | C.Int32Literal _ -> Some AST.TInt32
 | C.UInt8Literal _ -> Some AST.TUInt8 | C.UInt16Literal _ -> Some AST.TUInt16 | C.UInt32Literal _ -> Some AST.TUInt32
 | C.UInt64Literal _ -> Some AST.TUInt64 | C.UInt128Literal _ -> Some AST.TUInt128
 | C.BoolLiteral _ -> Some AST.TBool | C.StringLiteral _ | C.InterpolatedString _ -> Some AST.TString | C.BlobLiteral _ -> Some AST.TBlob
 | C.CharLiteral _ -> Some AST.TChar | C.FloatLiteral _ -> Some AST.TFloat64 | C.UnitLiteral -> Some AST.TUnit
 | C.Local id -> B.find_opt id typeEnv
 | C.RecordUpdate (record, _) -> infer record
 | C.FuncRef id -> (match FunctionIdMap.tryFind id funcParams, FunctionIdMap.tryFind id funcReturns with Some parameters, Some result -> Some (AST.TFunction (parameters, result)) | _ -> None)
 | C.Let (pattern, value, body) ->
   let environment = match infer value with Some typ -> List.fold_left (fun current (name, typ) -> B.add name typ current) typeEnv (S.letPatternBindingTypes pattern typ) | None -> typeEnv in inferWith environment body
 | C.RecursiveLet (recursion, _, body) -> inferWith (B.add (C.recursiveBindingId recursion) (C.recursiveMemberType recursion) typeEnv) body
 | C.TupleLiteral values -> Option.map (fun values -> AST.TTuple values) (collect (List.map infer (C.tupleElementsToList values)))
 | C.TupleAccess (value, index) -> (match infer value with Some (AST.TTuple types) when index >= 0 && index < List.length types -> Some (List.nth types index) | _ -> None)
 | C.ListLiteral [] -> Some (AST.TList (AST.TVar "__empty_list_elem"))
 | C.ListLiteral (first :: _) -> Option.map (fun value -> AST.TList value) (infer first)
 | C.DictLiteral (key, value, _) -> Some (AST.TDict (C.semanticType key, C.semanticType value))
 | C.RecordLiteral (reference, fields) -> (match P.tryFindRecordTypeNameById reference.C.typeId names with
   | None -> None
   | Some name -> (match M.find_opt name typeReg with
    | None -> Some (AST.TRecord (name, []))
    | Some (info : R.recordTypeInfo) ->
      let expected = List.map (fun (_, typ) -> R.canonicalizeBareSumTypeRefs variants typ) info.R.fields in
      let actual = C.recordFieldsInSourceOrder fields |> List.map (fun (field, value) -> fieldIndex field, value) |> List.to_seq |> IntMap.of_seq in
      let bindings = List.fold_left (fun accumulated (index, expected) -> match IntMap.find_opt index actual with
       | None -> accumulated
       | Some value -> (match infer value with None -> accumulated | Some typ ->
        (match T.matchTypePattern expected (R.canonicalizeBareSumTypeRefs variants typ) with Ok values -> accumulated @ values | Error _ -> accumulated))) [] (List.mapi (fun index typ -> index, typ) expected) in
      (match T.consolidateTypeBindings bindings with
       | Error _ -> Some (AST.TRecord (name, []))
       | Ok subst -> let args = if reference.C.typeArgs = [] then List.map (fun name -> Option.value (M.find_opt name subst) ~default:(AST.TVar name)) info.R.typeParams else C.semanticTypeArgs reference.C.typeArgs in Some (AST.TRecord (name, args)))))
 | C.RecordAccess (record, field) -> (match infer record with
   | Some (AST.TRecord (name, args)) -> (match M.find_opt name typeReg with
    | Some (info : R.recordTypeInfo) -> Option.map (fun (_, typ) -> match T.buildDeclaredRecordFieldSubst info args with Some subst -> T.applySubstToType subst typ | None -> typ) (let index = fieldIndex field in if index < 0 then None else List.nth_opt info.R.fields index)
    | None -> None)
   | _ -> None)
 | C.Constructor (reference, fields) ->
   let found = Option.bind (P.tryFindSumTypeNameById reference.C.typeId names) (fun owner -> P.tryFindVariantByConstructorId reference.C.typeId owner reference.C.constructorId variants) in
   (match found with
    | Some (owner, parameters, _, patterns) ->
      let defaults = List.map (fun name -> AST.TVar name) parameters in
      let fallback () = Some (AST.TSum (owner, defaults)) in
      if List.length patterns <> List.length fields then fallback () else
      let inferred = List.map (fun (pattern, value) -> Option.bind (infer value) (fun actual -> Result.to_option (T.matchTypePattern pattern actual))) (List.combine patterns fields) in
      (match collect inferred with None -> fallback () | Some bindings ->
       (match T.consolidateTypeBindings (List.concat bindings) with Error _ -> fallback () | Ok subst -> Some (AST.TSum (owner, List.map (fun name -> Option.value (M.find_opt name subst) ~default:(AST.TVar name)) parameters))))
    | None -> Option.map (fun name -> AST.TSum (name, [])) (P.tryFindSumTypeNameById reference.C.typeId names))
 | C.BinOp (operation, left, right) ->
   let left = infer left in let right = infer right in (match operation with
    | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod | AST.Pow -> (match left, right with
      | Some left, Some right when AST.compareSemanticType left right = 0 && numeric left -> Some left
      | Some (AST.TVar _), Some right when numeric right -> Some right
      | Some left, Some (AST.TVar _) when numeric left -> Some left | _ -> None)
    | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor -> (match left, right with Some left, Some right when AST.compareSemanticType left right = 0 && integer left -> Some left | _ -> None)
    | AST.Eq | AST.Neq | AST.Lt | AST.Gt | AST.Lte | AST.Gte | AST.And | AST.Or -> Some AST.TBool | AST.StringConcat -> Some AST.TString)
 | C.UnaryOp (AST.Not, _) -> Some AST.TBool | C.UnaryOp ((AST.Neg | AST.BitNot), value) -> infer value
 | C.Call (id, _) -> FunctionIdMap.tryFind id funcReturns
 | C.TypeApp (id, args, _) -> (match FunctionIdMap.tryFind id genericDefs with
   | Some (parameters, result) when List.length parameters = List.length args -> Some (T.applySubstToType (M.of_list (List.combine parameters (C.semanticTypeArgs args))) result)
   | _ -> FunctionIdMap.tryFind id funcReturns)
 | C.If (_, yes, no) -> (match infer yes, infer no with
   | Some yes, Some no when AST.compareSemanticType yes no = 0 -> Some yes
   | Some (AST.TSum (yes, args)), Some (AST.TSum (no, [])) when yes = no -> Some (AST.TSum (yes, args))
   | Some (AST.TSum (yes, [])), Some (AST.TSum (no, args)) when yes = no -> Some (AST.TSum (no, args))
   | Some AST.TNever, Some no -> Some no | Some yes, Some AST.TNever -> Some yes
   | Some yes, Some no -> reconcileBranchTypes yes no | _ -> None)
 | C.Sequence (_, next) -> infer next
 | C.Match (value, cases) ->
   let scrutinee = infer value in
   let types = List.map (fun (case : C.matchCase) ->
    let environment = match scrutinee with
     | None -> typeEnv | Some typ -> List.map (fun pattern -> matchPatternBindingTypes typeReg variants names pattern typ) (NonEmptyList.toList case.C.patterns) |> List.fold_left mergeBindings typeEnv in
    inferWith environment case.C.body) (NonEmptyList.toList cases) in
   (match collect types with None | Some [] -> None | Some (first :: rest) -> List.fold_left (fun merged typ -> Option.bind merged (fun merged -> reconcileBranchTypes merged typ)) (Some first) rest)
 | C.Lambda (parameters, _, body) ->
   let types = NonEmptyList.toList parameters |> List.map S.lambdaParameterType in
   let bindings = NonEmptyList.toList parameters |> List.concat_map S.lambdaParameterBindings |> List.to_seq |> B.of_seq in
   Option.map (fun result -> AST.TFunction (types, result)) (inferWith (mergeBindings typeEnv bindings) body)
 | C.Apply (target, args) -> (match infer target with
   | Some (AST.TFunction (parameters, result)) ->
     let count = List.length (S.exprArgsToList args) in
     if count = List.length parameters then Some result else if count < List.length parameters then
     let rec skip count values = if count = 0 then values else match values with [] -> [] | _ :: rest -> skip (count - 1) rest in Some (AST.TFunction (skip count parameters, result)) else None
   | _ -> None)
 | C.IndirectApply _ -> Some AST.TBool
 | C.Closure _ | C.RuntimeError _ | C.BoundaryRender _ -> None
let inferLambdaReturnType body state =
 match simpleInferType body state.typeEnv state.funcParams state.funcReturnTypes state.genericFuncDefs state.typeReg state.variantLookup (R.typeNamesFromSymbols state.symbols) with
 | Some AST.TNever -> Ok AST.TUnit | Some result -> Ok result
 | None -> let target = match body with C.Call (id, _) -> Option.fold ~none:"" ~some:(fun name -> " (call target: " ^ name ^ ")") (C.functionName id state.symbols) | _ -> "" in
 Error ("Lambda lifting could not infer return type for lambda body" ^ target ^ ": " ^ CheckedStructuralFormat.toString body)
