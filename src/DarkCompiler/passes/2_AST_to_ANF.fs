// 2_AST_to_ANF.fs - ANF Transformation (Pass 2)
//
// Transforms AST into A-Normal Form (ANF).
//
// This pass converts nested expressions into a flat sequence of let-bindings
// where all operands are simple (atoms). This makes evaluation order explicit
// and simplifies subsequent compiler passes.
//
// Input: AST
// Output: ANF
//
// Example transformation of "2 + 3 * 4":
//   Input AST:  BinOp(Add, IntLiteral(2), BinOp(Mul, IntLiteral(3), IntLiteral(4)))
//   Output ANF: let tmp0 = 3
//               let tmp1 = 4
//               let tmp2 = tmp0 * tmp1
//               let tmp3 = 2
//               let tmp4 = tmp3 + tmp2
//               return tmp4

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
