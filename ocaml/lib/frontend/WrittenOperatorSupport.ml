(* Source operator checking from WrittenChecking.fs, including structural equality. *)
module WT = WrittenTypes
module C = CheckedAST
open WrittenTypeSupport
let bind = Result.bind
let[@warning "-4"] check checkExpression checkedLiteral globals locals symbols expected infix left right =
 let check = checkExpression globals locals in
 bind (check symbols None left) (fun (leftType, checkedLeft, afterLeft) ->
  let rec needsEnumContext = function
   | WT.EEnum _ -> true
   | WT.ETuple (_, first, _, second, rest, _, _) -> List.exists needsEnumContext (first :: second :: List.map snd rest)
   | WT.EList (_, contents, _, _) -> List.exists (fun (item, _) -> needsEnumContext item) contents
   | _ -> false in
  let rightExpected = match infix with
   | WT.InfixFnCall WT.StringConcat -> None
   | WT.InfixFnCall (WT.ComparisonEquals | WT.ComparisonNotEquals) -> if needsEnumContext right then Some leftType else None
   | _ -> Some leftType in
  bind (check afterLeft rightExpected right) (fun (rightType, checkedRight, finalSymbols) ->
   let integer = List.mem leftType [AST.TInt; AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128] in
   let numeric = integer || leftType = AST.TFloat64 in
   let mixed = (leftType = AST.TChar && rightType = AST.TString) || (leftType = AST.TString && rightType = AST.TChar) in
   let selection = match infix with
    | WT.BinOp WT.BinOpAnd -> AST.And, leftType = AST.TBool, AST.TBool
    | WT.BinOp WT.BinOpOr -> AST.Or, leftType = AST.TBool, AST.TBool
    | WT.InfixFnCall op -> match op with
     | WT.ArithmeticPlus -> AST.Add, numeric, leftType
     | WT.ArithmeticMinus -> AST.Sub, numeric, leftType
     | WT.ArithmeticMultiply -> AST.Mul, numeric, leftType
     | WT.ArithmeticDivide -> AST.Div, numeric, leftType
     | WT.ArithmeticModulo -> AST.Mod, numeric, leftType
     | WT.ArithmeticPower -> AST.Pow, numeric, leftType
     | WT.BitwiseAnd -> AST.BitAnd, integer, leftType
     | WT.BitwiseOr -> AST.BitOr, integer, leftType
     | WT.BitwiseXor -> AST.BitXor, integer, leftType
     | WT.ShiftLeft -> AST.Shl, integer, leftType
     | WT.ShiftRight -> AST.Shr, integer, leftType
     | WT.ComparisonEquals -> AST.Eq, structuralEqualityCompatible globals leftType rightType && not mixed, AST.TBool
     | WT.ComparisonNotEquals -> AST.Neq, structuralEqualityCompatible globals leftType rightType && not mixed, AST.TBool
     | WT.ComparisonLessThan -> AST.Lt, numeric, AST.TBool
     | WT.ComparisonLessThanOrEqual -> AST.Lte, numeric, AST.TBool
     | WT.ComparisonGreaterThan -> AST.Gt, numeric, AST.TBool
     | WT.ComparisonGreaterThanOrEqual -> AST.Gte, numeric, AST.TBool
     | WT.StringConcat -> let stringLike typ = List.mem typ [AST.TString; AST.TChar; AST.TNever] in AST.StringConcat, stringLike leftType && stringLike rightType, (if leftType = AST.TNever || rightType = AST.TNever then AST.TNever else AST.TString) in
   match selection with
   | _ when leftType = AST.TNever -> checkedLiteral expected finalSymbols AST.TNever checkedLeft
   | _ when rightType = AST.TNever -> checkedLiteral expected finalSymbols AST.TNever (C.Sequence (checkedLeft, checkedRight))
   | AST.Pow, _, _ when leftType = AST.TInt128 -> Error "Cannot perform numeric operation on Int128 and Int128"
   | AST.Pow, _, _ when leftType = AST.TUInt128 -> Error "Cannot perform numeric operation on UInt128 and UInt128"
   | ((AST.Eq | AST.Neq) as op), true, resultType when (match leftType with AST.TList _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TDict _ | AST.TVar _ | AST.TInferenceVar _ | AST.TFunction _ -> true | _ -> false) ->
    let normalized = if Option.is_none (Unification.reconcileTypes None leftType rightType) && structuralEqualityCompatible globals leftType rightType then convertStructuralRecord globals leftType rightType checkedRight finalSymbols else Ok (checkedRight, finalSymbols) in
    bind normalized (fun (rightValue, symbols) ->
     let marker = ComparisonPlanning.internalTypeAppMarkerName ComparisonPlanning.EqHelperDispatch in
     let id, symbols = C.internFunction marker symbols in
     let equality = C.TypeApp (id, [C.checkedType leftType], NonEmptyList.fromList [checkedLeft; rightValue]) in
     checkedLiteral expected symbols resultType (if op = AST.Neq then C.UnaryOp (AST.Not, equality) else equality))
   | op, true, resultType -> checkedLiteral expected finalSymbols resultType (C.BinOp (op, checkedLeft, checkedRight))
   | (AST.Eq | AST.Neq), false, _ when mixed -> Error "Cannot compare Char and String"
   | _ -> Error "Operator is unavailable for this type"))
