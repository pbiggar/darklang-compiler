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

/// Type registry - maps record type names to their field definitions
type TypeRegistry = Map<string, (string * AST.Type) list>

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
/// typeReg maps record type names to field definitions
let rec toANF (expr: AST.Expr) (varGen: ANF.VarGen) (env: Map<string, ANF.TempId>) (typeReg: TypeRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
    match expr with
    | AST.IntLiteral n ->
        // Integer literal becomes return
        Ok (ANF.Return (ANF.IntLiteral n), varGen)

    | AST.BoolLiteral b ->
        // Boolean literal becomes return
        Ok (ANF.Return (ANF.BoolLiteral b), varGen)

    | AST.StringLiteral s ->
        // String literal becomes return
        Ok (ANF.Return (ANF.StringLiteral s), varGen)

    | AST.FloatLiteral f ->
        // Float literal becomes return
        Ok (ANF.Return (ANF.FloatLiteral f), varGen)

    | AST.Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some tempId -> Ok (ANF.Return (ANF.Var tempId), varGen)
        | None -> Error $"Undefined variable: {name}"

    | AST.Let (name, value, body) ->
        // Let binding: convert value to atom, allocate fresh temp, convert body with extended env
        toAtom value varGen env typeReg |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
            let (tempId, varGen2) = ANF.freshVar varGen1
            let env' = Map.add name tempId env
            toANF body varGen2 env' typeReg |> Result.map (fun (bodyExpr, varGen3) ->
                // Build: valueBindings + let tempId = valueAtom + body
                let finalExpr = ANF.Let (tempId, ANF.Atom valueAtom, bodyExpr)
                let exprWithBindings = wrapBindings valueBindings finalExpr
                (exprWithBindings, varGen3)))

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: handle differently based on operand type
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            Ok (ANF.Return (ANF.IntLiteral System.Int64.MinValue), varGen)
        | AST.FloatLiteral f ->
            // Constant-fold negative float literals at compile time
            Ok (ANF.Return (ANF.FloatLiteral (-f)), varGen)
        | _ ->
            // Integer negation: convert to 0 - expr
            toANF (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom
        toAtom innerExpr varGen env typeReg |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create unary op and bind to fresh variable
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let anfOp = convertUnaryOp op
            let cexpr = ANF.UnaryPrim (anfOp, innerAtom)

            // Build the expression: innerBindings + let tempVar = op
            let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
            let exprWithBindings = wrapBindings innerBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        // Convert operands to atoms
        toAtom left varGen env typeReg |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg |> Result.map (fun (rightAtom, rightBindings, varGen2) ->
                // Create binop and bind to fresh variable
                let (tempVar, varGen3) = ANF.freshVar varGen2
                let anfOp = convertBinOp op
                let cexpr = ANF.Prim (anfOp, leftAtom, rightAtom)

                // Build the expression: leftBindings + rightBindings + let tempVar = op
                let finalExpr = ANF.Let (tempVar, cexpr, ANF.Return (ANF.Var tempVar))
                let exprWithRight = wrapBindings rightBindings finalExpr
                let exprWithLeft = wrapBindings leftBindings exprWithRight

                (exprWithLeft, varGen3)))

    | AST.If (cond, thenBranch, elseBranch) ->
        // If expression: convert condition to atom, both branches to ANF
        toAtom cond varGen env typeReg |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toANF thenBranch varGen1 env typeReg |> Result.bind (fun (thenExpr, varGen2) ->
                toANF elseBranch varGen2 env typeReg |> Result.map (fun (elseExpr, varGen3) ->
                    // Build the expression: condBindings + if condAtom then thenExpr else elseExpr
                    let finalExpr = ANF.If (condAtom, thenExpr, elseExpr)
                    let exprWithBindings = wrapBindings condBindings finalExpr

                    (exprWithBindings, varGen3))))

    | AST.Call (funcName, args) ->
        // Function call: convert all arguments to atoms
        let rec convertArgs (argExprs: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match argExprs with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | arg :: rest ->
                toAtom arg vg env typeReg
                |> Result.bind (fun (argAtom, argBindings, vg') ->
                    convertArgs rest vg' (argAtom :: accAtoms) (accBindings @ argBindings))

        convertArgs args varGen [] []
        |> Result.map (fun (argAtoms, argBindings, varGen1) ->
            // Bind call result to fresh variable
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let callExpr = ANF.Call (funcName, argAtoms)
            let finalExpr = ANF.Let (resultVar, callExpr, ANF.Return (ANF.Var resultVar))
            let exprWithBindings = wrapBindings argBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.TupleLiteral elements ->
        // Convert all elements to atoms
        let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | elem :: rest ->
                toAtom elem vg env typeReg
                |> Result.bind (fun (elemAtom, elemBindings, vg') ->
                    convertElements rest vg' (elemAtom :: accAtoms) (accBindings @ elemBindings))

        convertElements elements varGen [] []
        |> Result.map (fun (elemAtoms, elemBindings, varGen1) ->
            // Create TupleAlloc and bind to fresh variable
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let tupleExpr = ANF.TupleAlloc elemAtoms
            let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
            let exprWithBindings = wrapBindings elemBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.TupleAccess (tupleExpr, index) ->
        // Convert tuple to atom and create TupleGet
        toAtom tupleExpr varGen env typeReg
        |> Result.map (fun (tupleAtom, tupleBindings, varGen1) ->
            let (resultVar, varGen2) = ANF.freshVar varGen1
            let getExpr = ANF.TupleGet (tupleAtom, index)
            let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
            let exprWithBindings = wrapBindings tupleBindings finalExpr

            (exprWithBindings, varGen2))

    | AST.RecordLiteral (typeName, fields) ->
        // Records are compiled like tuples - allocate heap space and store fields
        // Get field order from type registry (or use order from literal if anonymous)
        let fieldOrder =
            if typeName = "" then
                fields |> List.map fst  // Use literal order for anonymous records
            else
                match Map.tryFind typeName typeReg with
                | Some typeFields -> typeFields |> List.map fst
                | None -> fields |> List.map fst  // Fallback to literal order

        // Reorder field values according to type definition order
        let fieldMap = Map.ofList fields
        let orderedValues =
            fieldOrder
            |> List.choose (fun fname -> Map.tryFind fname fieldMap)

        // Convert to TupleLiteral and reuse tuple handling
        toANF (AST.TupleLiteral orderedValues) varGen env typeReg

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        // Need to determine field index from type
        // First, we need to get the record type to find field index
        // For now, we use a simple approach: get the record type from typeReg

        // Convert record expression to get its type
        // This is tricky since we don't have type info here
        // As a workaround, we'll need the record expression's type
        // For now, assume we can infer it from the first RecordLiteral in scope

        // Get field index by looking up in all record types
        let findFieldIndex () =
            typeReg
            |> Map.toList
            |> List.tryPick (fun (_, fields) ->
                fields
                |> List.tryFindIndex (fun (name, _) -> name = fieldName)
                |> Option.map (fun idx -> idx))

        match findFieldIndex () with
        | Some index ->
            toAtom recordExpr varGen env typeReg
            |> Result.map (fun (recordAtom, recordBindings, varGen1) ->
                let (resultVar, varGen2) = ANF.freshVar varGen1
                let getExpr = ANF.TupleGet (recordAtom, index)
                let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
                let exprWithBindings = wrapBindings recordBindings finalExpr
                (exprWithBindings, varGen2))
        | None ->
            Error $"Unknown field: {fieldName}"

/// Convert an AST expression to an atom, introducing let bindings as needed
and toAtom (expr: AST.Expr) (varGen: ANF.VarGen) (env: Map<string, ANF.TempId>) (typeReg: TypeRegistry) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
    match expr with
    | AST.IntLiteral n ->
        Ok (ANF.IntLiteral n, [], varGen)

    | AST.BoolLiteral b ->
        Ok (ANF.BoolLiteral b, [], varGen)

    | AST.StringLiteral s ->
        Ok (ANF.StringLiteral s, [], varGen)

    | AST.FloatLiteral f ->
        Ok (ANF.FloatLiteral f, [], varGen)

    | AST.Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some tempId -> Ok (ANF.Var tempId, [], varGen)
        | None -> Error $"Undefined variable: {name}"

    | AST.Let (name, value, body) ->
        // Let binding in atom position: need to evaluate and return the body as an atom
        toAtom value varGen env typeReg |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
            let (tempId, varGen2) = ANF.freshVar varGen1
            let env' = Map.add name tempId env
            toAtom body varGen2 env' typeReg |> Result.map (fun (bodyAtom, bodyBindings, varGen3) ->
                // All bindings: valueBindings + binding tempId to value + bodyBindings
                let allBindings = valueBindings @ [(tempId, ANF.Atom valueAtom)] @ bodyBindings
                (bodyAtom, allBindings, varGen3)))

    | AST.UnaryOp (AST.Neg, innerExpr) ->
        // Unary negation: handle differently based on operand type
        match innerExpr with
        | AST.IntLiteral n when n = System.Int64.MinValue ->
            // The lexer stores INT64_MIN as a sentinel for "9223372036854775808"
            // When negated, it should remain INT64_MIN (mathematically correct)
            Ok (ANF.IntLiteral System.Int64.MinValue, [], varGen)
        | AST.FloatLiteral f ->
            // Constant-fold negative float literals at compile time
            Ok (ANF.FloatLiteral (-f), [], varGen)
        | _ ->
            // Integer negation: convert to 0 - expr
            toAtom (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom, create binding
        toAtom innerExpr varGen env typeReg |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create the operation
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let anfOp = convertUnaryOp op
            let cexpr = ANF.UnaryPrim (anfOp, innerAtom)

            // Return the temp variable as atom, plus all bindings
            let allBindings = innerBindings @ [(tempVar, cexpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        toAtom left varGen env typeReg |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg |> Result.map (fun (rightAtom, rightBindings, varGen2) ->
                // Create the operation
                let (tempVar, varGen3) = ANF.freshVar varGen2
                let anfOp = convertBinOp op
                let cexpr = ANF.Prim (anfOp, leftAtom, rightAtom)

                // Return the temp variable as atom, plus all bindings
                let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                (ANF.Var tempVar, allBindings, varGen3)))

    | AST.If (condExpr, thenExpr, elseExpr) ->
        // If expression in atom position: convert all parts to atoms, create IfValue
        toAtom condExpr varGen env typeReg |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toAtom thenExpr varGen1 env typeReg |> Result.bind (fun (thenAtom, thenBindings, varGen2) ->
                toAtom elseExpr varGen2 env typeReg |> Result.bind (fun (elseAtom, elseBindings, varGen3) ->
                    // Create a temporary for the result
                    let (tempVar, varGen4) = ANF.freshVar varGen3
                    // Create an IfValue CExpr
                    let ifCExpr = ANF.IfValue (condAtom, thenAtom, elseAtom)
                    // Return temp as atom with all bindings
                    let allBindings = condBindings @ thenBindings @ elseBindings @ [(tempVar, ifCExpr)]
                    Ok (ANF.Var tempVar, allBindings, varGen4))))

    | AST.Call (funcName, args) ->
        // Function call in atom position: convert all arguments to atoms
        let rec convertArgs (argExprs: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match argExprs with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | arg :: rest ->
                toAtom arg vg env typeReg
                |> Result.bind (fun (argAtom, argBindings, vg') ->
                    convertArgs rest vg' (argAtom :: accAtoms) (accBindings @ argBindings))

        convertArgs args varGen [] []
        |> Result.map (fun (argAtoms, argBindings, varGen1) ->
            // Create a temporary for the call result
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let callCExpr = ANF.Call (funcName, argAtoms)
            // Return temp as atom with all bindings
            let allBindings = argBindings @ [(tempVar, callCExpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.TupleLiteral elements ->
        // Convert all elements to atoms
        let rec convertElements (elems: AST.Expr list) (vg: ANF.VarGen) (accAtoms: ANF.Atom list) (accBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom list * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (List.rev accAtoms, accBindings, vg)
            | elem :: rest ->
                toAtom elem vg env typeReg
                |> Result.bind (fun (elemAtom, elemBindings, vg') ->
                    convertElements rest vg' (elemAtom :: accAtoms) (accBindings @ elemBindings))

        convertElements elements varGen [] []
        |> Result.map (fun (elemAtoms, elemBindings, varGen1) ->
            // Create a temporary for the tuple
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let tupleCExpr = ANF.TupleAlloc elemAtoms
            // Return temp as atom with all bindings
            let allBindings = elemBindings @ [(tempVar, tupleCExpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.TupleAccess (tupleExpr, index) ->
        // Convert tuple to atom and create TupleGet
        toAtom tupleExpr varGen env typeReg
        |> Result.map (fun (tupleAtom, tupleBindings, varGen1) ->
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let getCExpr = ANF.TupleGet (tupleAtom, index)
            // Return temp as atom with all bindings
            let allBindings = tupleBindings @ [(tempVar, getCExpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.RecordLiteral (typeName, fields) ->
        // Records are compiled like tuples
        let fieldOrder =
            if typeName = "" then
                fields |> List.map fst
            else
                match Map.tryFind typeName typeReg with
                | Some typeFields -> typeFields |> List.map fst
                | None -> fields |> List.map fst

        let fieldMap = Map.ofList fields
        let orderedValues =
            fieldOrder
            |> List.choose (fun fname -> Map.tryFind fname fieldMap)

        // Reuse tuple handling
        toAtom (AST.TupleLiteral orderedValues) varGen env typeReg

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        let findFieldIndex () =
            typeReg
            |> Map.toList
            |> List.tryPick (fun (_, fields) ->
                fields
                |> List.tryFindIndex (fun (name, _) -> name = fieldName)
                |> Option.map (fun idx -> idx))

        match findFieldIndex () with
        | Some index ->
            toAtom recordExpr varGen env typeReg
            |> Result.bind (fun (recordAtom, recordBindings, varGen1) ->
                let (tempVar, varGen2) = ANF.freshVar varGen1
                let getCExpr = ANF.TupleGet (recordAtom, index)
                let allBindings = recordBindings @ [(tempVar, getCExpr)]
                Ok (ANF.Var tempVar, allBindings, varGen2))
        | None ->
            Error $"Unknown field: {fieldName}"

/// Wrap let bindings around an expression
and wrapBindings (bindings: (ANF.TempId * ANF.CExpr) list) (expr: ANF.AExpr) : ANF.AExpr =
    List.foldBack (fun (var, cexpr) acc -> ANF.Let (var, cexpr, acc)) bindings expr

/// Convert a function definition to ANF
let convertFunction (funcDef: AST.FunctionDef) (varGen: ANF.VarGen) (typeReg: TypeRegistry) : Result<ANF.Function * ANF.VarGen, string> =
    // Allocate TempIds for parameters
    let (paramIds, varGen1) =
        funcDef.Params
        |> List.fold (fun (ids, vg) (_, _) ->
            let (tempId, vg') = ANF.freshVar vg
            (ids @ [tempId], vg')) ([], varGen)

    // Build environment mapping param names to TempIds
    let paramEnv =
        List.zip (List.map fst funcDef.Params) paramIds
        |> Map.ofList

    // Convert body
    toANF funcDef.Body varGen1 paramEnv typeReg
    |> Result.map (fun (body, varGen2) ->
        ({ Name = funcDef.Name; Params = paramIds; Body = body }, varGen2))

/// Convert a program to ANF
let convertProgram (program: AST.Program) : Result<ANF.Program, string> =
    let (AST.Program topLevels) = program
    let varGen = ANF.VarGen 0

    // Build type registry from type definitions
    let typeReg : TypeRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.RecordDef (name, fields)) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    // Separate functions and expressions
    let functions = topLevels |> List.choose (function AST.FunctionDef f -> Some f | _ -> None)
    let expressions = topLevels |> List.choose (function AST.Expression e -> Some e | _ -> None)

    // Convert all functions
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg typeReg
            |> Result.bind (fun (anfFunc, vg') ->
                convertFunctions rest vg' (anfFunc :: acc))

    convertFunctions functions varGen []
    |> Result.bind (fun (anfFuncs, varGen1) ->
        // Convert main expression (if any)
        match expressions with
        | [expr] ->
            toANF expr varGen1 Map.empty typeReg
            |> Result.map (fun (anfExpr, _) ->
                ANF.Program (anfFuncs, Some anfExpr))
        | [] ->
            // No main expression - just functions
            Ok (ANF.Program (anfFuncs, None))
        | _ ->
            Error "Multiple top-level expressions not allowed")
