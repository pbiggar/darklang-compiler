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
let convertOp (op: AST.BinOp) : ANF.BinOp =
    match op with
    | AST.Add -> ANF.Add
    | AST.Sub -> ANF.Sub
    | AST.Mul -> ANF.Mul
    | AST.Div -> ANF.Div

/// Convert AST expression to ANF
let rec toANF (expr: AST.Expr) (varGen: ANF.VarGen) : ANF.AExpr * ANF.VarGen =
    match expr with
    | AST.IntLiteral n ->
        // Base case: literal becomes return
        (ANF.Return (ANF.IntLiteral n), varGen)

    | AST.Neg innerExpr ->
        // Unary negation: convert to 0 - expr
        // Special case: -INT64_MIN should stay as INT64_MIN (from lexer sentinel)
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            (ANF.Return (ANF.IntLiteral System.Int64.MinValue), varGen)
        | _ ->
            toANF (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen

    | AST.BinOp (op, left, right) ->
        // Convert operands to atoms
        let (leftAtom, leftBindings, varGen1) = toAtom left varGen
        let (rightAtom, rightBindings, varGen2) = toAtom right varGen1

        // Create binop and bind to fresh variable
        let (tempVar, varGen3) = ANF.freshVar varGen2
        let anfOp = convertOp op
        let cexpr = ANF.Prim (anfOp, leftAtom, rightAtom)

        // Build the expression: leftBindings + rightBindings + let tempVar = op
        let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
        let exprWithRight = wrapBindings rightBindings finalExpr
        let exprWithLeft = wrapBindings leftBindings exprWithRight

        (exprWithLeft, varGen3)

/// Convert an AST expression to an atom, introducing let bindings as needed
and toAtom (expr: AST.Expr) (varGen: ANF.VarGen) : ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen =
    match expr with
    | AST.IntLiteral n ->
        (ANF.IntLiteral n, [], varGen)

    | AST.Neg innerExpr ->
        // Unary negation: convert to 0 - expr
        // Special case: -INT64_MIN should stay as INT64_MIN (from lexer sentinel)
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            (ANF.IntLiteral System.Int64.MinValue, [], varGen)
        | _ ->
            toAtom (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen

    | AST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        let (leftAtom, leftBindings, varGen1) = toAtom left varGen
        let (rightAtom, rightBindings, varGen2) = toAtom right varGen1

        // Create the operation
        let (tempVar, varGen3) = ANF.freshVar varGen2
        let anfOp = convertOp op
        let cexpr = ANF.Prim (anfOp, leftAtom, rightAtom)

        // Return the temp variable as atom, plus all bindings
        let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
        (ANF.Var tempVar, allBindings, varGen3)

/// Wrap let bindings around an expression
and wrapBindings (bindings: (ANF.TempId * ANF.CExpr) list) (expr: ANF.AExpr) : ANF.AExpr =
    List.foldBack (fun (var, cexpr) acc -> ANF.Let (var, cexpr, acc)) bindings expr
