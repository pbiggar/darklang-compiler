(*
   CheckBinaryOperations.ml - Check BinOp expressions while preserving source diagnostics and order.
*)
(* CheckBinaryOperations.ml - Check BinOp expressions while preserving source diagnostics and order. *)
open! AST
open CheckingDiagnostics
module M = StringOrder.Map
module C = ComparisonPlanning
(*
   Arithmetic operators: T -> T -> T (where T is int or float)
   Check left operand to determine numeric type
   Runtime Dark operators inspect both operand values before
   deciding whether their numeric representations agree.
   Comparison operators: T -> T -> bool
   Eq and Neq: work on any type (structural equality for complex types)
   Lt, Gt, Lte, Gte: only work on numeric types
   Check without context first so rejected comparisons keep
   the right operand's actual type in their diagnostic.
   Check left operand to determine type
   Equality works on any type - both operands must be same type
   A distinct nominal enum is a valid equality
   operand. Retry without forcing the left type;
   same-type generic calls keep the contextual
   inference from the successful first check.
   In generic contexts, one side can still contain type variables
   while the other side has become concrete.
   Ordering only works on numeric types.
   Allow unresolved type variables here so guards like
   `match Error 5 with | Ok x when x > 2 -> ...` can infer x as Int64.
   Boolean operators: bool -> bool -> bool
   Exponentiation is defined by the canonical numeric modules. The
   128-bit modules intentionally have no power operation.
   Internal bitwise operators: Int -> Int -> Int (same integer type).
   String concatenation: string -> string -> string
*)
let check checkExpr sums env registry lookup generic warnings modules aliases expected op left right =
 let ( let* ) = Result.bind in
 let recurse value expected = checkExpr value env registry lookup generic warnings modules aliases expected in
 let result typ expr name = match expected with Some other when other <> typ -> Error (TypeMismatch (other, typ, "result of " ^ name)) | Some _ | None -> Ok (typ, expr) in
 let runtime value = tryExtractKnownTestRuntimeErrorMessage M.empty value in
 let numeric typ = match Types.resolveType aliases typ with TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TFloat64 as numeric -> Some numeric | _ -> None in
 let integer = function TInt8 | TInt16 | TInt32 | TInt64 | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 -> true | _ -> false in
 let invariant kind = Crash.crash ("Non-" ^ kind ^ " operator reached " ^ kind ^ " type-checking path: " ^ StructuralFormat.binOp op) in
 match op with
 | Add | Sub | Mul | Div | Mod ->
   let name = match op with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" | _ -> invariant "arithmetic" in
   (match runtime left with Some message -> Error (GenericError ("Uncaught exception: " ^ message)) | None ->
    let* leftType, left = recurse left None in
    match numeric leftType with
    | Some leftNumeric ->
      let* rightType, right = recurse right None in
      if rightType <> leftNumeric then Ok (leftNumeric, Let (LPVariable "__dark_numeric_operands", TupleLiteral [left; right], RuntimeError ("Cannot perform numeric operation on " ^ typeToString leftType ^ " and " ^ typeToString rightType)))
      else result leftNumeric (BinOp (op, left, right)) name
    | None -> match leftType with
      | TVar _ | TInferenceVar _ ->
        let rightExpected = Option.bind expected numeric in
        let* rightType, right = recurse right rightExpected in
        let inferred = match rightExpected with Some typ -> Some typ | None -> numeric rightType in
        (match inferred with None -> Error (InvalidOperation (name, [leftType])) | Some typ -> match expected with
         | Some other when not (Unification.typesCompatible other typ) -> Error (TypeMismatch (other, typ, "result of " ^ name))
         | Some _ | None -> Ok (typ, BinOp (op, left, right)))
      | other -> Error (InvalidOperation (name, [other])))
 | Eq | Neq | Lt | Gt | Lte | Gte ->
   let name = match op with Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">" | Lte -> "<=" | Gte -> ">=" | _ -> invariant "comparison" in
   let comparisonResult =
    let* leftType, leftChecked = recurse left None in
    let rightWithout = recurse right None in
    let rightResult = match rightWithout with Ok (typ, _) when Unification.containsTVar typ -> recurse right (Some leftType) | Ok _ -> rightWithout | Error _ -> recurse right (Some leftType) in
    let* rightType, rightChecked = rightResult in
    let* leftType, leftChecked = if Unification.containsTVar leftType && not (Unification.containsTVar rightType) then recurse left (Some rightType) else Ok (leftType, leftChecked) in
    let* plan = C.classifyComparison aliases registry lookup sums op leftType rightType in
    let expression = match plan with
     | C.EqualityComparison typ -> let equality = C.buildEqExprForType aliases lookup typ leftChecked rightChecked in if op = Neq then UnaryOp (Not, equality) else equality
     | C.OrderingComparison typ -> C.buildOrderingExprForType op typ leftChecked rightChecked in
    result TBool expression name in
   let lambdaLiteralFastPath = Some comparisonResult in
   (match lambdaLiteralFastPath with
    | Some value -> value
    | None ->
      let* leftType, leftChecked = recurse left None in
      match op with
      | Eq | Neq ->
        let rightWithExpected = recurse right (Some leftType) in
        let rightResult = match Types.resolveType aliases leftType, rightWithExpected with TSum _, Error (TypeMismatch _) -> recurse right None | _ -> rightWithExpected in
        let* rightType, rightChecked = rightResult in
        let distinct = match Types.resolveType aliases leftType, Types.resolveType aliases rightType with
         | TSum (leftName, _), TSum (rightName, _) when leftName <> rightName -> Some (Ok (TBool, Let (LPVariable "__dark_nominal_equality_operands", TupleLiteral [leftChecked; rightChecked], BoolLiteral (op = Neq)))) | _ -> None in
        (match distinct with Some value -> value | None ->
         match Unification.reconcileTypes (Some aliases) leftType rightType with
         | None -> Error (TypeMismatch (leftType, rightType, "right operand of " ^ name))
         | Some comparable ->
           let namedPartial candidate = match candidate with
            | Lambda (params, _annotation, body) ->
              let params = NonEmptyList.toList params in
              let generated = List.for_all (fun (param : AST.lambdaParameter) -> match param.pattern with LPVariable name -> String.starts_with ~prefix:"__partial_" name | _ -> false) params in
              let details = match body with Apply (Var name, args, values) -> Some (name, args, NonEmptyList.toList values) | _ -> None in
              (match generated, details with
               | true, Some (name, args, values) ->
                 let remaining = List.length params in let applied = List.length values - remaining in
                 let trailing = if applied >= 0 then List.drop applied values else [] in
                 let trailingParameters = applied > 0 && List.length trailing = remaining && List.for_all2 (fun arg (param : AST.lambdaParameter) -> match param.pattern with LPVariable name -> arg = Var name | _ -> false) trailing params in
                 let concrete = match M.find_opt name env with
                  | Some typ when args = [] -> Some typ
                  | Some typ -> (match M.find_opt name generic.Types.functions with Some params when List.length params = List.length args -> Some (Types.applySubst (M.of_list (List.combine params args)) typ) | _ -> None)
                  | None -> None in
                 (match trailingParameters, concrete with true, Some (TFunction (types, _)) when applied <= List.length types ->
                   let identity = if args = [] then name else name ^ "<" ^ String.concat ", " (List.map typeToString args) ^ ">" in
                   Some (identity, List.combine (List.take applied types) (List.take applied values)) | _ -> None)
               | _ -> None)
            | _ -> None in
           let partialEquality (leftIdentity, leftState) (rightIdentity, rightState) =
            let bindings prefix = List.mapi (fun index (typ, value) -> prefix ^ string_of_int index, typ, value) in
            let left = bindings "__dark_partial_left_" leftState and right = bindings "__dark_partial_right_" rightState in
            let comparisons = if leftIdentity = rightIdentity && List.length left = List.length right then
             List.map2 (fun (left, typ, _) (right, _, _) -> C.buildEqExprForType aliases lookup typ (Var left) (Var right)) left right else [BoolLiteral false] in
            List.fold_right (fun (name, _, value) body -> Let (LPVariable name, value, body)) (left @ right) (C.chainAndExpr comparisons) in
           let equality = match Types.resolveType aliases comparable, namedPartial leftChecked, namedPartial rightChecked with
            | TFunction _, Some left, Some right -> partialEquality left right
            | _ -> C.buildEqExprForType aliases lookup comparable leftChecked rightChecked in
           result TBool (if op = Neq then UnaryOp (Not, equality) else equality) name)
      | Lt | Gt | Lte | Gte ->
        let* rightType, rightChecked = recurse right (Some leftType) in
        (match Unification.reconcileTypes (Some aliases) leftType rightType with
         | None -> Error (TypeMismatch (leftType, rightType, "right operand of " ^ name))
         | Some (TInt8 | TInt16 | TInt32 | TInt64 | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TFloat64) -> result TBool (BinOp (op, leftChecked, rightChecked)) name
         | Some other -> Error (InvalidOperation (name, [other])))
      | _ -> Error (GenericError ("Unexpected comparison operator: " ^ name)))
 | And | Or ->
   let name = if op = And then "&&" else "||" in
   let boolean operand =
    let checked = match op with And -> recurse operand (Some TBool) | Or -> recurse operand None | _ -> recurse operand None in
    match checked with
    | Ok (TBool, value) -> Ok value | Ok _ -> Error (GenericError (name ^ " only supports Booleans"))
    | Error (TypeMismatch (TBool, _, _)) when op = And -> Error (GenericError (name ^ " only supports Booleans"))
    | Error error -> Error error in
   (match op, runtime left with And, Some message -> Error (GenericError message) | _ ->
    let* left = boolean left in
    let known = isKnownTestRuntimeErrorExpr M.empty right in
    let shortCircuit = match op, left, known with And, BoolLiteral false, true -> Some false | Or, BoolLiteral true, true -> Some true | _ -> None in
    match shortCircuit with Some value -> result TBool (BoolLiteral value) name | None ->
     match op, runtime right with And, Some message -> Error (GenericError message) | _ -> let* right = boolean right in result TBool (BinOp (op, left, right)) name)
 | Pow ->
   let supports = function TInt | TInt8 | TInt16 | TInt32 | TInt64 | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TFloat64 -> true | _ -> false in
   let* leftType, left = recurse left None in
   if not (supports leftType) then Error (GenericError ("Cannot perform numeric operation on " ^ typeToString leftType ^ " and " ^ typeToString leftType)) else
   let* rightType, right = recurse right (Some leftType) in
   if rightType <> leftType then Error (TypeMismatch (leftType, rightType, "right operand of ^")) else result leftType (BinOp (Pow, left, right)) "^"
 | Shl | Shr | BitAnd | BitOr | BitXor ->
   let name = match op with Shl -> "<<" | Shr -> ">>" | BitAnd -> "&" | BitOr -> "|" | BitXor -> "^" | _ -> invariant "bitwise" in
   let* leftType, left = recurse left None in
   if not (integer leftType) then Error (InvalidOperation (name, [leftType])) else
   let* rightType, right = recurse right (Some leftType) in
   if rightType <> leftType then Error (TypeMismatch (leftType, rightType, "right operand of " ^ name)) else result leftType (BinOp (op, left, right)) name
 | StringConcat ->
   (match runtime left with Some message -> Error (GenericError ("Uncaught exception: " ^ message)) | None ->
    let* leftType, left = recurse left (Some TString) in
    let stringLike typ = typ = TString || typ = TChar in
    if not (stringLike leftType) then Error (InvalidOperation ("++", [leftType])) else
    match runtime right with Some message -> Error (GenericError ("Uncaught exception: " ^ message)) | None ->
     let* rightType, right = recurse right (Some TString) in
     if not (stringLike rightType) then Error (TypeMismatch (TString, rightType, "right operand of ++")) else result TString (BinOp (op, left, right)) "++")
[@@warning "-4"]
