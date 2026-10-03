(* Operators.fs - Lower numeric operators and structural equality into ANF. *)
[@@@warning "-4"]
module A = ANF
module P = LoweringPrimitives
module R = TypeRegistries
module T = TypeSubstitution
module M = StringOrder.Map
(*
   Never reached - StringConcat handled as CExpr
*)
let convertBinOp = function
 | AST.Add -> A.Add | AST.Sub -> A.Sub | AST.Mul -> A.Mul | AST.Div -> A.Div | AST.Mod -> A.Mod
 | AST.Pow -> Crash.crash "Exponentiation must lower through the canonical numeric power function"
 | AST.Shl -> A.Shl | AST.Shr -> A.Shr | AST.BitAnd -> A.BitAnd | AST.BitOr -> A.BitOr | AST.BitXor -> A.BitXor
 | AST.Eq -> A.Eq | AST.Neq -> A.Neq | AST.Lt -> A.Lt | AST.Gt -> A.Gt | AST.Lte -> A.Lte | AST.Gte -> A.Gte | AST.And -> A.And | AST.Or -> A.Or | AST.StringConcat -> A.Add
(*
   Arbitrary-precision Int values use tagged words or limb buffers. Route
   operations through the pure Int stdlib implementation instead of fixed-width
   machine primitives.
*)
let integerFunctionForBinOp resolve typ op =
 let owner = match op, typ with
 | AST.Pow, AST.TInt -> Some "Darklang.Stdlib.Int" | AST.Pow, AST.TInt8 -> Some "Darklang.Stdlib.Int8" | AST.Pow, AST.TInt16 -> Some "Darklang.Stdlib.Int16" | AST.Pow, AST.TInt32 -> Some "Darklang.Stdlib.Int32" | AST.Pow, AST.TInt64 -> Some "Darklang.Stdlib.Int64"
 | AST.Pow, AST.TUInt8 -> Some "Darklang.Stdlib.UInt8" | AST.Pow, AST.TUInt16 -> Some "Darklang.Stdlib.UInt16" | AST.Pow, AST.TUInt32 -> Some "Darklang.Stdlib.UInt32" | AST.Pow, AST.TUInt64 -> Some "Darklang.Stdlib.UInt64"
 | AST.Pow, AST.TFloat64 | AST.Mod, AST.TFloat64 -> Some "Darklang.Stdlib.Float"
 | _, AST.TInt -> Some "Darklang.Stdlib.Int" | _, AST.TInt128 -> Some "Darklang.Stdlib.Int128" | _, AST.TUInt128 -> Some "Darklang.Stdlib.UInt128" | _ -> None in
 let name = match op with
 | AST.Add -> Some "add" | AST.Sub -> Some "subtract" | AST.Mul -> Some "multiply" | AST.Div -> Some "divide" | AST.Mod -> Some "mod" | AST.Pow -> Some "power"
 | AST.Shl -> Some "shiftLeft" | AST.Shr -> Some "shiftRight" | AST.BitAnd -> Some "bitwiseAnd" | AST.BitOr -> Some "bitwiseOr" | AST.BitXor -> Some "bitwiseXor"
 | AST.Lt -> Some "lessThan" | AST.Gt -> Some "greaterThan" | AST.Lte -> Some "lessThanOrEqualTo" | AST.Gte -> Some "greaterThanOrEqualTo"
 | AST.Eq | AST.Neq | AST.And | AST.Or | AST.StringConcat -> None in
 match owner, name with Some owner, Some name -> Some (resolve (owner ^ "." ^ name)) | _ -> None
(*
   Convert AST.UnaryOp to ANF.UnaryOp
*)
let convertUnaryOp = function AST.Neg -> A.Neg | AST.Not -> A.Not | AST.BitNot -> A.BitNot
(*
   Check if a type requires structural equality (compound types)
*)
let isCompoundType = function AST.TTuple _ | AST.TRecord _ | AST.TSum _ -> true | _ -> false
(*
   Generate structural equality comparison for compound types.
   Returns a list of bindings and the final result atom that holds the comparison result.
   Keep bindings in reverse order during construction to avoid quadratic
   list appends when comparing deeply nested structures.
   UUID is a nominal single-case sum over an immutable UInt128
   block, so its payload needs value equality rather than pointer
   equality.
   Other sums retain the established primitive payload comparison;
   multi-variant, heterogeneous payload dispatch is a separate
   structural-equality design boundary.
   Infer the type of an expression using type environment and registries
   Used for type-directed field lookup in record access
*)
let rec generateStructuralEquality resolve left right typ gen registry variants cases =
 let addForwardBindingsToRev reversed bindings = List.fold_left (fun reversed binding -> binding :: reversed) reversed bindings in
 let combineComparisonResult previous next reversed gen = match previous with None -> Some next, reversed, gen | Some previous -> let id, gen = A.freshVar gen in Some (A.Var id), (id, A.Prim (A.And, previous, next)) :: reversed, gen in
 let finalizeBindings result reversed gen = match result with Some result -> List.rev reversed, result, gen | None -> let id, gen = A.freshVar gen in List.rev ((id, A.Atom (A.BoolLiteral true)) :: reversed), A.Var id, gen in
 let primitiveEquality typ left right = match typ with
 | AST.TInt128 -> A.Call (resolve "Darklang.Stdlib.Int128.__equals", [left; right])
 | AST.TUInt128 -> A.Call (resolve "Darklang.Stdlib.UInt128.__equals", [left; right])
 | AST.TInt -> A.Call (resolve "Darklang.Stdlib.Int.__equals", [left; right])
 | AST.TString -> A.CanonicalBufferEq (MemoryModel.Utf8String, left, right)
 | AST.TChar -> A.CanonicalBufferEq (MemoryModel.GraphemeCluster, left, right)
 | _ -> A.Prim (A.Eq, left, right) in
 let single expression = let id, gen = A.freshVar gen in [id, expression], A.Var id, gen in
 let compareElements types get =
  let rec loop index types result reversed gen = match types with
  | [] -> finalizeBindings result reversed gen
  | typ :: rest ->
    let leftId, gen1 = A.freshVar gen in let leftGet = get left index in
    let rightId, gen2 = A.freshVar gen1 in let rightGet = get right index in
    let reversed = addForwardBindingsToRev reversed [leftId, leftGet; rightId, rightGet] in
    let next, reversed, gen3 = if isCompoundType typ then
     let bindings, result, gen = generateStructuralEquality resolve (A.Var leftId) (A.Var rightId) typ gen2 registry variants cases in result, addForwardBindingsToRev reversed bindings, gen
     else let id, gen = A.freshVar gen2 in let expression = primitiveEquality typ (A.Var leftId) (A.Var rightId) in A.Var id, (id, expression) :: reversed, gen in
    let result, reversed, gen4 = combineComparisonResult result next reversed gen3 in loop (index + 1) rest result reversed gen4 in
  loop 0 types None [] gen in
 match typ with
 | AST.TTuple types -> compareElements types (fun atom index -> A.TupleGet (atom, index))
 | AST.TRecord (name, args) -> (match M.find_opt name registry with None -> single (A.Prim (A.Eq, left, right)) | Some (info : R.recordTypeInfo) ->
   let descriptor = T.recordDescriptor name args info in
   let fields = match T.buildDeclaredRecordFieldSubst info args with Some subst -> List.map (fun (name, typ) -> name, T.applySubstToType subst typ) info.R.fields | None -> info.R.fields in
   compareElements (List.map snd fields) (fun atom index -> A.RecordGet (descriptor, atom, index)))
 | AST.TSum (name, args) ->
   let payload = Option.value (M.find_opt name cases) ~default:M.empty |> M.exists (fun _ (case : P.sumCase) -> case.P.fields <> []) in
   (match P.nullablePointerSumPayloadType name args cases with
   | Some AST.TString -> single (A.CanonicalBufferEq (MemoryModel.NullableUtf8String, left, right))
   | Some AST.TChar -> single (A.CanonicalBufferEq (MemoryModel.NullableGraphemeCluster, left, right))
   | Some _ -> single (A.Prim (A.Eq, left, right))
   | None when Option.is_some (P.spareImmediateSumSentinel name args cases) -> single (A.Prim (A.Eq, left, right))
   | None when P.transparentSumPayloadType name args cases = Some AST.TString || P.transparentSumPayloadType name args cases = Some AST.TChar -> single (primitiveEquality AST.TString left right)
   | None when Option.is_some (P.transparentSumPayloadType name args cases) || not payload -> single (A.Prim (A.Eq, left, right))
   | None ->
     let leftTag, gen1 = A.freshVar gen in let rightTag, gen2 = A.freshVar gen1 in let tagEq, gen3 = A.freshVar gen2 in
     let leftPayload, gen4 = A.freshVar gen3 in let rightPayload, gen5 = A.freshVar gen4 in let payloadEq, gen6 = A.freshVar gen5 in let result, gen7 = A.freshVar gen6 in
     let comparison = if name = "Uuid" then A.Call (resolve "Darklang.Stdlib.UInt128.__equals", [A.Var leftPayload; A.Var rightPayload]) else A.Prim (A.Eq, A.Var leftPayload, A.Var rightPayload) in
     [leftTag, A.TupleGet (left, 0); rightTag, A.TupleGet (right, 0); tagEq, A.Prim (A.Eq, A.Var leftTag, A.Var rightTag); leftPayload, A.TupleGet (left, 1); rightPayload, A.TupleGet (right, 1); payloadEq, comparison; result, A.Prim (A.And, A.Var tagEq, A.Var payloadEq)], A.Var result, gen7)
 | _ -> single (A.Prim (A.Eq, left, right))
