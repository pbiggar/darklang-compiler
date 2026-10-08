(* LoweringTypeInference.ml - Recover checked expression types for representation-directed ANF lowering. *)
[@@@warning "-4"]

module C = CheckedAST
module P = LoweringPrimitives
module R = TypeRegistries
module S = SpecializationIdentity
module T = TypeSubstitution
module A = ClosureAnalysis
module M = StringOrder.Map
module B = C.BindingIdMap
module I = Map.Make (Int)

let ( let* ) = Result.bind
let merge base overlay = M.fold M.add overlay base
let mergeBindings base overlay = B.fold B.add overlay base
let display = StructuralFormat.semanticType

let rec substituteType subst typ =
  match typ with
  | AST.TVar name -> Option.value (M.find_opt name subst) ~default:typ
  | AST.TTuple elements -> AST.TTuple (List.map (substituteType subst) elements)
  | AST.TRecord (name, args) ->
      AST.TRecord (name, List.map (substituteType subst) args)
  | AST.TList element -> AST.TList (substituteType subst element)
  | AST.TDict (key, value) ->
      AST.TDict (substituteType subst key, substituteType subst value)
  | AST.TSum (name, args) ->
      AST.TSum (name, List.map (substituteType subst) args)
  | AST.TFunction (args, result) ->
      AST.TFunction
        (List.map (substituteType subst) args, substituteType subst result)
  | _ -> typ

let rec _extractPatternBindings sums variants pattern scrutinee =
  let recurse = _extractPatternBindings sums variants in
  let accumulate patterns typ =
    List.fold_left
      (fun bindings pattern -> merge bindings (recurse pattern typ))
      M.empty patterns
  in
  let fields parameters fields patterns =
    if List.length patterns <> List.length fields then M.empty
    else
      let subst =
        match scrutinee with
        | AST.TSum (_, args) when List.length parameters = List.length args ->
            M.of_list (List.combine parameters args)
        | _ -> M.empty
      in
      List.fold_left
        (fun bindings (pattern, typ) ->
          merge bindings (recurse pattern (substituteType subst typ)))
        M.empty
        (List.combine patterns fields)
  in
  match pattern with
  | AST.POr alternatives -> recurse (NonEmptyList.head alternatives) scrutinee
  | AST.PVar name -> M.singleton name scrutinee
  | AST.PWildcard | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
  | AST.PInt8Literal _ | AST.PInt16Literal _ | AST.PInt32Literal _
  | AST.PUInt8Literal _ | AST.PUInt16Literal _ | AST.PUInt32Literal _
  | AST.PUInt64Literal _ | AST.PUInt128Literal _ | AST.PUnit | AST.PBool _
  | AST.PString _ | AST.PChar _ | AST.PFloat _ ->
      M.empty
  | AST.PTuple patterns -> (
      let types =
        match scrutinee with
        | AST.TTuple types when List.length types = List.length patterns ->
            Some types
        | AST.TVar variable ->
            Some
              (List.mapi
                 (fun index _ ->
                   AST.TVar
                     ("__tuple_elem_" ^ variable ^ "_" ^ string_of_int index))
                 patterns)
        | AST.TNever ->
            Some
              (List.mapi
                 (fun index _ ->
                   AST.TVar ("__tuple_elem_runtime_error_" ^ string_of_int index))
                 patterns)
        | _ -> None
      in
      match types with
      | Some types when List.length patterns = List.length types ->
          List.fold_left
            (fun bindings (pattern, typ) ->
              merge bindings (recurse pattern typ))
            M.empty
            (List.combine patterns types)
      | _ -> M.empty)
  | AST.PConstructor (name, patterns) -> (
      match M.find_opt name variants with
      | Some (_, parameters, _, types) -> fields parameters types patterns
      | None -> Crash.crash ("Unknown constructor '" ^ name ^ "' in pattern"))
  | AST.PResolvedConstructor (owner, _, tag, patterns) -> (
      match P.tryFindVariantByTag owner tag sums.P.cases with
      | Some (_, parameters, _, types) -> fields parameters types patterns
      | None ->
          Crash.crash
            ("Unknown resolved constructor tag '" ^ string_of_int tag
           ^ "' for '" ^ owner ^ "'"))
  | AST.PList patterns -> (
      match scrutinee with
      | AST.TList element -> accumulate patterns element
      | AST.TVar _ | AST.TNever ->
          accumulate patterns (AST.TVar "__list_elem_unknown")
      | _ -> M.empty)
  | AST.PListCons (patterns, tail) -> (
      match scrutinee with
      | AST.TList element ->
          merge (accumulate patterns element) (recurse tail scrutinee)
      | AST.TVar _ | AST.TNever ->
          merge
            (accumulate patterns (AST.TVar "__list_elem_unknown"))
            (recurse tail scrutinee)
      | _ -> M.empty)

let joinTypes category mismatch preferred other =
  if preferred = other then Ok preferred
  else if preferred = AST.TNever then Ok other
  else if other = AST.TNever then Ok preferred
  else
    let resolve preferred other =
      match T.matchTypePattern preferred other with
      | Error _ -> Error category
      | Ok bindings ->
          let* subst = T.consolidateTypeBindings bindings in
          Ok (T.applySubstToType subst preferred)
    in
    let left = resolve preferred other in
    let right = resolve other preferred in
    match (left, right) with
    | Ok left, Ok right ->
        if S.containsTypeVar left && not (S.containsTypeVar right) then Ok right
        else if S.containsTypeVar right && not (S.containsTypeVar left) then
          Ok left
        else Ok left
    | Ok left, Error _ -> Ok left
    | Error _, Ok right -> Ok right
    | Error _, Error _ -> Error (mismatch preferred other)

let ordinal id =
  let bits = AST.functionIdValue id in
  Z.to_string
    (if bits < 0L then Z.add (Z.of_int64 bits) (Z.shift_left Z.one 64)
     else Z.of_int64 bits)

(*
   Type checker should have enforced completeness already.
   Record update returns the same type as the record being updated
   Preserve unknown element type for empty lists
   Infer from first case body, but first extend environment with pattern variables
   Infer scrutinee type to help with pattern variable typing
   Helper to extract pattern variable names and infer their types
   Preserve unresolved tuple element types rather than dropping bindings
   or defaulting to a concrete numeric type.
   Non-matching tuple patterns must not introduce bindings with fabricated types.
   Type checking treats these as non-matching alternatives.
   Grouped alternatives may include impossible list branches (for example `0 | [_]`).
   Treat those as contributing no bindings rather than crashing.
   Impossible list-cons alternatives must not fabricate bindings.
   Type args may be unavailable in ANF inferType.
   Use Unit to avoid leaking unresolved type variables into later passes.
   Runtime errors are bottom-like: branch and match inference select
   the type of the reachable value-producing alternatives.
   Look up function return type from the function registry
   Check if it's a module function (e.g., Stdlib.File.exists)
   Check if it's a monomorphized intrinsic (e.g., __raw_get_i64)
   These are raw memory operations that work with 8-byte values
   Preserve the monomorphized return type; defaulting to Int64 can
   incorrectly mark pattern-match branches as impossible.
   __raw_slot_init<T> returns Unit
   Key intrinsics for Dict - monomorphized versions
   __hash<k> returns Int64 (hash value)
   __key_eq<k> returns Bool (equality check)
   Dict intrinsics - monomorphized versions
   __empty_dict<k, v> returns Dict<k, v> - but at ANF level it's Int64 (null ptr)
   __dict_is_null<k, v> returns Bool
   __dict_get_tag<k, v> returns Int64 (tag bits)
   __dict_to_rawptr<k, v> returns RawPtr
   __rawptr_to_dict<k, v> returns Dict<k, v>
   List intrinsics - monomorphized versions for the skew list.
   __list_is_null<a> returns Bool
   __list_get_tag<a> returns Int64 (tag bits)
   __list_to_rawptr<a> returns RawPtr
   __rawptr_to_list<a> returns List<a> - parse element type from mangled name
   Preserve the semantic list type for match/type inference.
   Generic function call - not yet implemented
   Lambda has function type (paramTypes) -> returnType
   Apply result is the return type of the function
   Function reference has the function's type
   Closure has function type (without the closure param)
   Interpolated strings are always String type
   Build the final direct-payload skew-list forest for a list literal.
   Element expressions have already been evaluated into atoms in source order.
*)
let rec inferTypeCore sums names expr environment registry variants functions
    functionNames modules =
  let infer = inferTypeCore sums names in
  let recurse expr =
    infer expr environment registry variants functions functionNames modules
  in
  let withEnv expr environment =
    infer expr environment registry variants functions functionNames modules
  in
  let fieldIndex field =
    match R.tryFindFieldIndex field names with
    | Some index -> index
    | None ->
        Crash.crash "Checked field identity is absent from layout metadata"
  in
  let constructorTag constructor =
    match R.tryFindConstructorTag constructor names with
    | Some tag -> tag
    | None ->
        Crash.crash
          "Checked constructor identity is absent from layout metadata"
  in
  match expr with
  | C.BoundaryRender _ | C.StringLiteral _ | C.InterpolatedString _ ->
      Ok AST.TString
  | C.RuntimeError _ -> Ok AST.TNever
  | C.UnitLiteral -> Ok AST.TUnit
  | C.Int64Literal _ -> Ok AST.TInt64
  | C.Int128Literal _ -> Ok AST.TInt128
  | C.BigIntLiteral _ -> Ok AST.TInt
  | C.Int8Literal _ -> Ok AST.TInt8
  | C.Int16Literal _ -> Ok AST.TInt16
  | C.Int32Literal _ -> Ok AST.TInt32
  | C.UInt8Literal _ -> Ok AST.TUInt8
  | C.UInt16Literal _ -> Ok AST.TUInt16
  | C.UInt32Literal _ -> Ok AST.TUInt32
  | C.UInt64Literal _ -> Ok AST.TUInt64
  | C.UInt128Literal _ -> Ok AST.TUInt128
  | C.BoolLiteral _ -> Ok AST.TBool
  | C.BlobLiteral _ -> Ok AST.TBlob
  | C.CharLiteral _ -> Ok AST.TChar
  | C.FloatLiteral _ -> Ok AST.TFloat64
  | C.Local id -> (
      match B.find_opt id environment with
      | Some typ -> Ok typ
      | None -> Error "Cannot infer type: undefined local binding identity")
  | C.DictLiteral (key, value, _) ->
      Ok (AST.TDict (C.semanticType key, C.semanticType value))
  | C.RecordLiteral (reference, fields) -> (
      match P.tryFindRecordTypeNameById reference.C.typeId names with
      | None -> Error "Unknown semantic record type"
      | Some owner -> (
          match M.find_opt owner registry with
          | None -> Error ("Unknown record type: " ^ owner)
          | Some (info : R.recordTypeInfo) ->
              let expected =
                List.map
                  (fun (name, typ) ->
                    (name, R.canonicalizeBareSumTypeRefs variants typ))
                  info.R.fields
              in
              let values =
                C.recordFieldsInSourceOrder fields
                |> List.map (fun (field, value) -> (fieldIndex field, value))
                |> List.to_seq |> I.of_seq
              in
              let rec bindings remaining acc =
                match remaining with
                | [] -> Ok acc
                | (index, typ) :: rest -> (
                    match I.find_opt index values with
                    | None -> bindings rest acc
                    | Some expr ->
                        let* actual = recurse expr in
                        let actual =
                          R.canonicalizeBareSumTypeRefs variants actual
                        in
                        let* more = T.matchTypePattern typ actual in
                        bindings rest (acc @ more))
              in
              let* bindings =
                bindings
                  (List.mapi (fun index (_, typ) -> (index, typ)) expected)
                  []
              in
              let* subst = T.consolidateTypeBindings bindings in
              let args =
                if reference.C.typeArgs = [] then
                  List.map
                    (fun parameter ->
                      Option.value
                        (M.find_opt parameter subst)
                        ~default:(AST.TVar parameter))
                    info.R.typeParams
                else C.semanticTypeArgs reference.C.typeArgs
              in
              Ok (AST.TRecord (owner, args))))
  | C.RecordUpdate (record, _) -> recurse record
  | C.RecordAccess (record, field) -> (
      let* typ = recurse record in
      match typ with
      | AST.TRecord (owner, args) -> (
          match M.find_opt owner registry with
          | None -> Error ("Unknown record type: " ^ owner)
          | Some (info : R.recordTypeInfo) -> (
              let index = fieldIndex field in
              let field =
                if index < 0 then None else List.nth_opt info.R.fields index
              in
              match field with
              | None ->
                  Error
                    ("Record type " ^ owner
                   ^ " has no field at the resolved slot")
              | Some (_, typ) ->
                  let typ =
                    match T.buildDeclaredRecordFieldSubst info args with
                    | Some subst -> T.applySubstToType subst typ
                    | None -> typ
                  in
                  Ok typ))
      | _ -> Error "Cannot access field on non-record type")
  | C.TupleLiteral elements ->
      let results = List.map recurse (C.tupleElementsToList elements) in
      let* types = ResultList.sequenceResults results in
      Ok (AST.TTuple types)
  | C.TupleAccess (tuple, index) -> (
      let* typ = recurse tuple in
      match typ with
      | AST.TTuple elements when index >= 0 && index < List.length elements ->
          Ok (List.nth elements index)
      | AST.TTuple _ ->
          Error ("Tuple index " ^ string_of_int index ^ " out of bounds")
      | _ -> Error "Cannot access index on non-tuple type")
  | C.Constructor (reference, values) -> (
      match P.tryFindSumTypeNameById reference.C.typeId names with
      | None -> Error "Unknown semantic constructor type"
      | Some owner -> (
          let tag = constructorTag reference.C.constructorId in
          match
            P.tryFindVariantByConstructorId reference.C.typeId owner
              reference.C.constructorId variants
          with
          | None -> Error ("Unknown constructor tag: " ^ string_of_int tag)
          | Some (owner, parameters, _, patterns) ->
              let defaults = List.map (fun name -> AST.TVar name) parameters in
              let args = C.semanticTypeArgs reference.C.typeArgs in
              if List.length args = List.length parameters then
                Ok (AST.TSum (owner, args))
              else if List.length patterns <> List.length values then
                Ok (AST.TSum (owner, defaults))
              else
                let* bindings =
                  List.fold_left
                    (fun result (pattern, value) ->
                      let* bindings = result in
                      let* actual = recurse value in
                      Ok
                        (match T.matchTypePattern pattern actual with
                        | Ok more -> bindings @ more
                        | Error _ -> bindings))
                    (Ok [])
                    (List.combine patterns values)
                in
                let args =
                  match T.consolidateTypeBindings bindings with
                  | Error _ -> defaults
                  | Ok subst ->
                      List.map
                        (fun parameter ->
                          Option.value
                            (M.find_opt parameter subst)
                            ~default:(AST.TVar parameter))
                        parameters
                in
                Ok (AST.TSum (owner, args))))
  | C.ListLiteral [] -> Ok (AST.TList (AST.TVar "t"))
  | C.ListLiteral (first :: _) ->
      Result.map (fun typ -> AST.TList typ) (recurse first)
  | C.Let (pattern, value, body) ->
      let* typ = recurse value in
      let child =
        List.fold_left
          (fun env (name, typ) -> B.add name typ env)
          environment
          (S.letPatternBindingTypes pattern typ)
      in
      withEnv body child
  | C.RecursiveLet (recursion, _, body) ->
      withEnv body
        (B.add
           (C.recursiveBindingId recursion)
           (C.recursiveMemberType recursion)
           environment)
  | C.If (_, yes, no) ->
      let* yes = recurse yes in
      let* no = recurse no in
      joinTypes "Branch type mismatch"
        (fun left right ->
          "If branches have incompatible types: then=" ^ P.typeToString left
          ^ ", else=" ^ P.typeToString right)
        yes no
  | C.Sequence (_, next) -> recurse next
  | C.BinOp (op, left, right) -> (
      let same () =
        let* left = recurse left in
        let* right = recurse right in
        if left = AST.TNever then Ok right
        else if right = AST.TNever then Ok left
        else if left = right then Ok left
        else
          let combined =
            let* bindings = T.matchTypePattern left right in
            let* subst = T.consolidateTypeBindings bindings in
            Ok (T.applySubstToType subst left)
          in
          Result.map_error
            (fun _ ->
              "Binary operator operands must match: left=" ^ display left
              ^ ", right=" ^ display right)
            combined
      in
      match op with
      | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod | AST.Pow -> (
          let* operand = same () in
          match operand with
          | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128
          | AST.TInt | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
          | AST.TUInt128 | AST.TFloat64 ->
              Ok operand
          | _ ->
              Error
                ("Arithmetic operator requires numeric operands, got "
               ^ display operand))
      | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor -> (
          let* operand = same () in
          match operand with
          | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128
          | AST.TInt | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
          | AST.TUInt128 ->
              Ok operand
          | _ ->
              Error
                ("Bitwise operator requires integer operands, got "
               ^ display operand))
      | AST.Eq | AST.Neq | AST.And | AST.Or -> Ok AST.TBool
      | AST.Lt | AST.Gt | AST.Lte | AST.Gte -> (
          let* operand = same () in
          match operand with
          | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt
          | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TFloat64
            ->
              Ok AST.TBool
          | _ ->
              Error
                ("Comparison operator requires numeric operands, got "
               ^ display operand))
      | AST.StringConcat -> Ok AST.TString)
  | C.UnaryOp (op, value) -> (
      let* operand = recurse value in
      match op with
      | AST.Neg -> (
          match operand with
          | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt
          | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TFloat64
            ->
              Ok operand
          | _ ->
              Error ("Negation requires numeric operand, got " ^ display operand)
          )
      | AST.Not ->
          if operand = AST.TBool then Ok AST.TBool
          else
            Error ("Logical not requires Bool operand, got " ^ display operand)
      | AST.BitNot -> (
          match operand with
          | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt ->
              Ok operand
          | _ ->
              Error
                ("Bitwise not requires integer operand, got " ^ display operand)
          ))
  | C.Match (scrutinee, cases) ->
      let patternType =
        match recurse scrutinee with
        | Ok typ -> typ
        | Error message ->
            Crash.crash
              ("Pattern match: Could not determine scrutinee type: " ^ message)
      in
      let inferCase (case : C.matchCase) =
        let bindings =
          NonEmptyList.toList case.C.patterns
          |> List.fold_left
               (fun bindings pattern ->
                 mergeBindings bindings
                   (A.matchPatternBindingTypes registry variants names pattern
                      patternType))
               B.empty
        in
        withEnv case.C.body (mergeBindings environment bindings)
      in
      let* first = inferCase (NonEmptyList.head cases) in
      List.fold_left
        (fun result case ->
          let* previous = result in
          let* next = inferCase case in
          joinTypes "Match case type mismatch"
            (fun left right ->
              "Match cases have incompatible types: " ^ P.typeToString left
              ^ " vs " ^ P.typeToString right)
            previous next)
        (Ok first) cases.NonEmptyList.tail
  | C.Call (id, args) -> (
      let args = NonEmptyList.toList args in
      let name =
        match FunctionIdMap.tryFind id functions with
        | Some (name, _) -> Some name
        | None -> FunctionIdMap.tryFind id functionNames
      in
      if name = Some "Builtin.unwrap" then
        match args with
        | [ arg ] -> (
            let* typ = recurse arg in
            match typ with
            | AST.TSum ("Darklang.Stdlib.Option.Option", [ value ]) -> Ok value
            | AST.TSum ("Darklang.Stdlib.Result.Result", [ value; _ ]) ->
                Ok value
            | AST.TSum ("Darklang.Stdlib.Option.Option", []) -> (
                match arg with
                | C.Constructor (reference, [ payload ])
                  when P.constructorReferenceMatches
                         "Darklang.Stdlib.Option.Option" "Some" reference names
                         variants ->
                    recurse payload
                | _ -> Ok AST.TUnit)
            | AST.TSum ("Darklang.Stdlib.Result.Result", []) -> (
                match arg with
                | C.Constructor (reference, [ payload ])
                  when P.constructorReferenceMatches
                         "Darklang.Stdlib.Result.Result" "Ok" reference names
                         variants ->
                    recurse payload
                | _ -> Ok AST.TUnit)
            | _ ->
                Error
                  ("Internal error: Builtin.unwrap expects Option/Result \
                    argument, got " ^ P.typeToString typ))
        | _ ->
            Error
              ("Internal error: Builtin.unwrap expects 1 argument, got "
              ^ string_of_int (List.length args))
      else if
        name = Some "Builtin.testRuntimeError" || name = Some "Builtin.crash"
      then
        match args with
        | [ _ ] -> Ok AST.TNever
        | _ ->
            Error
              ("Internal error: runtime failure function expects 1 argument, \
                got "
              ^ string_of_int (List.length args))
      else
        match FunctionIdMap.tryFind id functions with
        | Some (_, AST.TFunction (_, result)) -> Ok result
        | Some (name, _) ->
            Error ("Expected function type for " ^ name ^ " in funcReg")
        | None -> (
            let moduleFunction =
              Option.bind name (DarkStdlib.tryGetFunction modules)
            in
            match moduleFunction with
            | Some (func, _) -> Ok func.AST.returnType
            | None ->
                let name =
                  match name with
                  | Some name -> name
                  | None ->
                      Crash.crash
                        ("Type inference lost function name metadata for \
                          FunctionId " ^ ordinal id)
                in
                let prefix value = String.starts_with ~prefix:value name in
                let suffix value =
                  String.sub name (String.length value)
                    (String.length name - String.length value)
                in
                let parsed prefix =
                  P.tryParseMangledTypeWithSumTypeNames sums.P.names
                    (suffix prefix)
                in
                if prefix "Builtin.pmEvaluateValue_" then
                  Result.map
                    (fun typ ->
                      AST.TSum ("Darklang.Stdlib.Option.Option", [ typ ]))
                    (parsed "Builtin.pmEvaluateValue_")
                else if prefix "__raw_get_" then parsed "__raw_get_"
                else if prefix "__raw_take_" then parsed "__raw_take_"
                else if prefix "__stream_to_rawptr_" then Ok AST.TInternalRawPtr
                else if prefix "__rawptr_to_stream_" then
                  Result.map
                    (fun typ -> AST.TStream typ)
                    (parsed "__rawptr_to_stream_")
                else if prefix "__raw_slot_init_" then Ok AST.TUnit
                else if prefix "__hash_" then Ok AST.TInt64
                else if prefix "__key_eq_" then Ok AST.TBool
                else if prefix "__empty_dict_" then Ok AST.TInt64
                else if prefix "__dict_is_null_" then Ok AST.TBool
                else if prefix "__dict_get_tag_" then Ok AST.TInt64
                else if prefix "__dict_to_rawptr_" then Ok AST.TInternalRawPtr
                else if prefix "__rawptr_to_dict_" then
                  P.tryParseMangledTypeWithSumTypeNames sums.P.names
                    ("dict_" ^ suffix "__rawptr_to_dict_")
                else if prefix "__list_is_null_" then Ok AST.TBool
                else if prefix "__list_get_tag_" then Ok AST.TInt64
                else if prefix "__list_to_rawptr_" then Ok AST.TInternalRawPtr
                else if prefix "__rawptr_to_list_" then
                  Result.map
                    (fun typ -> AST.TList typ)
                    (parsed "__rawptr_to_list_")
                else if prefix "__list_empty_" then
                  Result.map (fun typ -> AST.TList typ) (parsed "__list_empty_")
                else Error ("Unknown function: '" ^ name ^ "'")))
  | C.TypeApp _ -> Error "Generic function calls not yet implemented"
  | C.Lambda (parameters, _, body) ->
      let params = NonEmptyList.toList parameters in
      let types = List.map S.lambdaParameterType params in
      let child =
        List.concat_map S.lambdaParameterBindings params
        |> List.fold_left
             (fun env (name, typ) -> B.add name typ env)
             environment
      in
      Result.map
        (fun result -> AST.TFunction (types, result))
        (withEnv body child)
  | C.Apply (target, _) -> (
      let* typ = recurse target in
      match typ with
      | AST.TFunction (_, result) -> Ok result
      | _ -> Error "Apply requires a function type")
  | C.IndirectApply (target, _) -> (
      let* typ = recurse target in
      match typ with
      | AST.TFunction (_, result) -> Ok result
      | _ -> Error "Indirect apply requires a function type")
  | C.GenericFuncRef (_, _, typ) -> Ok (C.semanticType typ)
  | C.FuncRef id -> (
      match FunctionIdMap.tryFind id functions with
      | Some (_, typ) -> Ok typ
      | None -> Error "Cannot infer type: undefined function identity")
  | C.Closure (id, _) -> (
      match FunctionIdMap.tryFind id functions with
      | Some (_, AST.TFunction (_ :: rest, result)) ->
          Ok (AST.TFunction (rest, result))
      | Some (_, typ) -> Ok typ
      | None -> Error "Cannot infer type: undefined closure function identity")
