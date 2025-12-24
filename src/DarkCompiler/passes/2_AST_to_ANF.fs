// 2_AST_to_ANF.fs - ANF Transformation (Pass 2)
//
// Transforms AST into A-Normal Form (ANF).
//
// Algorithm:
// - Recursively processes nested expressions
// - Converts complex operands to atoms (literals or variables)
// - Introduces let-bindings for intermediate computations
// - Uses VarGen for generating fresh temporary variable names
//
// Example:
//   BinOp(Add, IntLiteral(2), BinOp(Mul, IntLiteral(3), IntLiteral(4)))
//   →
//   let tmp0 = 3; let tmp1 = 4; let tmp2 = tmp0 * tmp1;
//   let tmp3 = 2; let tmp4 = tmp3 + tmp2; return tmp4

module AST_to_ANF

open ANF

/// Convert AST.BinOp to ANF.BinOp
let convertBinOp (op: AST.BinOp) : ANF.BinOp =
    match op with
    | AST.Add -> ANF.Add
    | AST.Sub -> ANF.Sub
    | AST.Mul -> ANF.Mul
    | AST.Div -> ANF.Div
    | AST.Eq -> ANF.Eq
    | AST.Neq -> ANF.Neq
    | AST.Lt -> ANF.Lt
    | AST.Gt -> ANF.Gt
    | AST.Lte -> ANF.Lte
    | AST.Gte -> ANF.Gte
    | AST.And -> ANF.And
    | AST.Or -> ANF.Or

/// Convert AST.UnaryOp to ANF.UnaryOp
let convertUnaryOp (op: AST.UnaryOp) : ANF.UnaryOp =
    match op with
    | AST.Neg -> ANF.Neg
    | AST.Not -> ANF.Not

/// Convert AST expression to ANF
/// env maps user variable names to ANF TempIds
let rec toANF (expr: AST.Expr) (varGen: ANF.VarGen) (env: Map<string, ANF.TempId>) : ANF.AExpr * ANF.VarGen =
    match expr with
    | AST.IntLiteral n ->
        // Integer literal becomes return
        (ANF.Return (ANF.IntLiteral n), varGen)

    | AST.BoolLiteral b ->
        // Boolean literal becomes return
        (ANF.Return (ANF.BoolLiteral b), varGen)

    | AST.Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some tempId -> (ANF.Return (ANF.Var tempId), varGen)
        | None -> failwith $"Undefined variable: {name}"  // TODO: use Result

    | AST.Let (name, value, body) ->
        // Let binding: convert value to atom, allocate fresh temp, convert body with extended env
        let (valueAtom, valueBindings, varGen1) = toAtom value varGen env
        let (tempId, varGen2) = ANF.freshVar varGen1
        let env' = Map.add name tempId env
        let (bodyExpr, varGen3) = toANF body varGen2 env'

        // Build: valueBindings + let tempId = valueAtom + body
        let finalExpr = ANF.Let (tempId, ANF.Atom valueAtom, bodyExpr)
        let exprWithBindings = wrapBindings valueBindings finalExpr
        (exprWithBindings, varGen3)

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: convert to 0 - expr
        // Special case: -INT64_MIN should stay as INT64_MIN (from lexer sentinel)
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            (ANF.Return (ANF.IntLiteral System.Int64.MinValue), varGen)
        | _ ->
            toANF (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom
        let (innerAtom, innerBindings, varGen1) = toAtom innerExpr varGen env

        // Create unary op and bind to fresh variable
        let (tempVar, varGen2) = ANF.freshVar varGen1
        let anfOp = convertUnaryOp op
        let cexpr = ANF.UnaryPrim (anfOp, innerAtom)

        // Build the expression: innerBindings + let tempVar = op
        let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
        let exprWithBindings = wrapBindings innerBindings finalExpr

        (exprWithBindings, varGen2)

    | AST.BinOp (op, left, right) ->
        // Convert operands to atoms
        let (leftAtom, leftBindings, varGen1) = toAtom left varGen env
        let (rightAtom, rightBindings, varGen2) = toAtom right varGen1 env

        // Create binop and bind to fresh variable
        let (tempVar, varGen3) = ANF.freshVar varGen2
        let anfOp = convertBinOp op
        let cexpr = ANF.Prim (anfOp, leftAtom, rightAtom)

        // Build the expression: leftBindings + rightBindings + let tempVar = op
        let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
        let exprWithRight = wrapBindings rightBindings finalExpr
        let exprWithLeft = wrapBindings leftBindings exprWithRight

        (exprWithLeft, varGen3)

    | AST.If (cond, thenBranch, elseBranch) ->
        // If expression: convert condition to atom, both branches to ANF
        let (condAtom, condBindings, varGen1) = toAtom cond varGen env
        let (thenExpr, varGen2) = toANF thenBranch varGen1 env
        let (elseExpr, varGen3) = toANF elseBranch varGen2 env

        // Build the expression: condBindings + if condAtom then thenExpr else elseExpr
        let finalExpr = ANF.If (condAtom, thenExpr, elseExpr)
        let exprWithBindings = wrapBindings condBindings finalExpr

        (exprWithBindings, varGen3)

/// Convert an AST expression to an atom, introducing let bindings as needed
and toAtom (expr: AST.Expr) (varGen: ANF.VarGen) (env: Map<string, ANF.TempId>) : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
    match expr with
    | AST.IntLiteral n ->
        (ANF.IntLiteral n, [], varGen)

    | AST.BoolLiteral b ->
        (ANF.BoolLiteral b, [], varGen)

    | AST.Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some tempId -> (ANF.Var tempId, [], varGen)
        | None -> failwith $"Undefined variable: {name}"  // TODO: use Result

    | AST.Let (name, value, body) ->
        // Let binding in atom position: need to evaluate and return the body as an atom
        let (valueAtom, valueBindings, varGen1) = toAtom value varGen env
        let (tempId, varGen2) = ANF.freshVar varGen1
        let env' = Map.add name tempId env
        let (bodyAtom, bodyBindings, varGen3) = toAtom body varGen2 env'

        // All bindings: valueBindings + binding tempId to value + bodyBindings
        let allBindings = valueBindings @ [(tempId, ANF.Atom valueAtom)] @ bodyBindings
        (bodyAtom, allBindings, varGen3)

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: convert to 0 - expr
        // Special case: -INT64_MIN should stay as INT64_MIN (from lexer sentinel)
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            (ANF.IntLiteral System.Int64.MinValue, [], varGen)
        | _ ->
            toAtom (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom, create binding
        let (innerAtom, innerBindings, varGen1) = toAtom innerExpr varGen env

        // Create the operation
        let (tempVar, varGen2) = ANF.freshVar varGen1
        let anfOp = convertUnaryOp op
        let cexpr = ANF.UnaryPrim (anfOp, innerAtom)

        // Return the temp variable as atom, plus all bindings
        let allBindings = innerBindings @ [(tempVar, cexpr)]
        (ANF.Var tempVar, allBindings, varGen2)

    | AST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        let (leftAtom, leftBindings, varGen1) = toAtom left varGen env
        let (rightAtom, rightBindings, varGen2) = toAtom right varGen1 env

        // Create the operation
        let (tempVar, varGen3) = ANF.freshVar varGen2
        let anfOp = convertBinOp op
        let cexpr = ANF.Prim (anfOp, leftAtom, rightAtom)

        // Return the temp variable as atom, plus all bindings
        let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
        (ANF.Var tempVar, allBindings, varGen3)

/// Wrap let bindings around an expression
and wrapBindings (bindings: (ANF.TempId * ANF.CExpr) list) (expr: ANF.AExpr) : ANF.AExpr =
    List.foldBack (fun (var, cexpr) acc -> ANF.Let (var, cexpr, acc)) bindings expr
