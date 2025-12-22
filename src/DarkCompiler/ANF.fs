module ANF

/// Unique identifier for temporary variables
type TempId = TempId of int

/// Atomic expressions (cannot be decomposed further)
type Atom =
    | IntLiteral of int64
    | Var of TempId

/// Binary operations on atoms
type BinOp =
    | Add
    | Sub
    | Mul
    | Div

/// Complex expressions (produce values)
type CExpr =
    | Atom of Atom
    | Prim of BinOp * Atom * Atom

/// ANF expressions with explicit sequencing
type AExpr =
    | Let of TempId * CExpr * AExpr
    | Return of Atom

/// ANF program
type Program = Program of AExpr

/// Fresh variable generator (functional style)
type VarGen = VarGen of int

/// Generate a fresh temporary variable
let freshVar (VarGen n) : TempId * VarGen =
    (TempId n, VarGen (n + 1))

/// Initial variable generator
let initialVarGen = VarGen 0

/// Convert AST.BinOp to ANF.BinOp
let convertOp (op: AST.BinOp) : BinOp =
    match op with
    | AST.Add -> Add
    | AST.Sub -> Sub
    | AST.Mul -> Mul
    | AST.Div -> Div

/// Convert AST expression to ANF
let rec toANF (expr: AST.Expr) (varGen: VarGen) : AExpr * VarGen =
    match expr with
    | AST.IntLiteral n ->
        // Base case: literal becomes return
        (Return (IntLiteral n), varGen)

    | AST.BinOp (op, left, right) ->
        // Convert operands to atoms
        let (leftAtom, leftBindings, varGen1) = toAtom left varGen
        let (rightAtom, rightBindings, varGen2) = toAtom right varGen1

        // Create binop and bind to fresh variable
        let (tempVar, varGen3) = freshVar varGen2
        let anfOp = convertOp op
        let cexpr = Prim (anfOp, leftAtom, rightAtom)

        // Build the expression: leftBindings + rightBindings + let tempVar = op
        let finalExpr = Let (tempVar, cexpr, Return (Var tempVar))
        let exprWithRight = wrapBindings rightBindings finalExpr
        let exprWithLeft = wrapBindings leftBindings exprWithRight

        (exprWithLeft, varGen3)

/// Convert an AST expression to an atom, introducing let bindings as needed
and toAtom (expr: AST.Expr) (varGen: VarGen) : Atom * (TempId * CExpr) list * VarGen =
    match expr with
    | AST.IntLiteral n ->
        (IntLiteral n, [], varGen)

    | AST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        let (leftAtom, leftBindings, varGen1) = toAtom left varGen
        let (rightAtom, rightBindings, varGen2) = toAtom right varGen1

        // Create the operation
        let (tempVar, varGen3) = freshVar varGen2
        let anfOp = convertOp op
        let cexpr = Prim (anfOp, leftAtom, rightAtom)

        // Return the temp variable as atom, plus all bindings
        let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
        (Var tempVar, allBindings, varGen3)

/// Wrap let bindings around an expression
and wrapBindings (bindings: (TempId * CExpr) list) (expr: AExpr) : AExpr =
    List.foldBack (fun (var, cexpr) acc -> Let (var, cexpr, acc)) bindings expr
