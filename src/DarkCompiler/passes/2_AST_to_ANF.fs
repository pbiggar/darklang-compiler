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

/// Variant lookup - maps variant names to (type name, tag index, payload type)
type VariantLookup = Map<string, (string * int * AST.Type option)>

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
/// variantLookup maps variant names to (type name, tag index)
let rec toANF (expr: AST.Expr) (varGen: ANF.VarGen) (env: Map<string, ANF.TempId>) (typeReg: TypeRegistry) (variantLookup: VariantLookup) : Result<ANF.AExpr * ANF.VarGen, string> =
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
        toAtom value varGen env typeReg variantLookup |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
            let (tempId, varGen2) = ANF.freshVar varGen1
            let env' = Map.add name tempId env
            toANF body varGen2 env' typeReg variantLookup |> Result.map (fun (bodyExpr, varGen3) ->
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
            toANF (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg variantLookup

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom
        toAtom innerExpr varGen env typeReg variantLookup |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
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
        toAtom left varGen env typeReg variantLookup |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg variantLookup |> Result.map (fun (rightAtom, rightBindings, varGen2) ->
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
        toAtom cond varGen env typeReg variantLookup |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toANF thenBranch varGen1 env typeReg variantLookup |> Result.bind (fun (thenExpr, varGen2) ->
                toANF elseBranch varGen2 env typeReg variantLookup |> Result.map (fun (elseExpr, varGen3) ->
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
                toAtom arg vg env typeReg variantLookup
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
                toAtom elem vg env typeReg variantLookup
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
        toAtom tupleExpr varGen env typeReg variantLookup
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
        toANF (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup

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
            toAtom recordExpr varGen env typeReg variantLookup
            |> Result.map (fun (recordAtom, recordBindings, varGen1) ->
                let (resultVar, varGen2) = ANF.freshVar varGen1
                let getExpr = ANF.TupleGet (recordAtom, index)
                let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
                let exprWithBindings = wrapBindings recordBindings finalExpr
                (exprWithBindings, varGen2))
        | None ->
            Error $"Unknown field: {fieldName}"

    | AST.Constructor (_, variantName, payload) ->
        match Map.tryFind variantName variantLookup with
        | None ->
            Error $"Unknown constructor: {variantName}"
        | Some (typeName, tag, _) ->
            // Check if ANY variant in this type has a payload
            // If so, all variants must be heap-allocated for consistency
            // Note: We get typeName from variantLookup, not from AST (which may be empty)
            let typeHasPayloadVariants =
                variantLookup
                |> Map.exists (fun _ (tName, _, pType) -> tName = typeName && pType.IsSome)

            match payload with
            | None when not typeHasPayloadVariants ->
                // Pure enum type (no payloads anywhere): return tag as an integer
                Ok (ANF.Return (ANF.IntLiteral (int64 tag)), varGen)
            | None ->
                // No payload but type has other variants with payloads
                // Heap-allocate as [tag] for consistent representation
                let tagAtom = ANF.IntLiteral (int64 tag)
                let (resultVar, varGen1) = ANF.freshVar varGen
                let tupleExpr = ANF.TupleAlloc [tagAtom]
                let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
                Ok (finalExpr, varGen1)
            | Some payloadExpr ->
                // Variant with payload: allocate [tag, payload] on heap
                toAtom payloadExpr varGen env typeReg variantLookup
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (int64 tag)
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (resultVar, varGen2) = ANF.freshVar varGen1
                    let tupleExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
                    let exprWithBindings = wrapBindings payloadBindings finalExpr
                    (exprWithBindings, varGen2))

    | AST.Match (scrutinee, cases) ->
        // Compile match to if-else chain
        // First convert scrutinee to atom
        toAtom scrutinee varGen env typeReg variantLookup
        |> Result.bind (fun (scrutineeAtom, scrutineeBindings, varGen1) ->
            // Convert pattern to a tag value for comparison
            let patternToTag (pattern: AST.Pattern) : Result<int64 option, string> =
                match pattern with
                | AST.PWildcard -> Ok None  // Wildcard matches everything
                | AST.PVar _ -> Ok None     // Variable matches everything
                | AST.PLiteral n -> Ok (Some n)  // Literal is the value itself
                | AST.PConstructor (variantName, _) ->
                    match Map.tryFind variantName variantLookup with
                    | Some (_, tag, _) -> Ok (Some (int64 tag))
                    | None -> Error $"Unknown constructor in pattern: {variantName}"

            // Check if the TYPE that a variant belongs to has any variant with a payload
            // This determines if values are heap-allocated or simple integers
            let typeHasAnyPayload (variantName: string) : bool =
                match Map.tryFind variantName variantLookup with
                | Some (typeName, _, _) ->
                    variantLookup
                    |> Map.exists (fun _ (tName, _, pType) -> tName = typeName && pType.IsSome)
                | None -> false

            // Extract pattern bindings and compile body with extended environment
            let rec extractAndCompileBody (pattern: AST.Pattern) (body: AST.Expr) (scrutAtom: ANF.Atom) (currentEnv: Map<string, ANF.TempId>) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                match pattern with
                | AST.PWildcard -> toANF body vg currentEnv typeReg variantLookup
                | AST.PLiteral _ -> toANF body vg currentEnv typeReg variantLookup
                | AST.PVar name ->
                    // Bind scrutinee to variable name
                    let (tempId, vg1) = ANF.freshVar vg
                    let env' = Map.add name tempId currentEnv
                    toANF body vg1 env' typeReg variantLookup
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let expr = ANF.Let (tempId, ANF.Atom scrutAtom, bodyExpr)
                        (expr, vg2))
                | AST.PConstructor (variantName, payloadPattern) ->
                    match payloadPattern with
                    | None -> toANF body vg currentEnv typeReg variantLookup
                    | Some innerPattern ->
                        // Extract payload from heap-allocated variant
                        // Variant layout: [tag:8][payload:8], so payload is at index 1
                        let (payloadVar, vg1) = ANF.freshVar vg
                        let payloadExpr = ANF.TupleGet (scrutAtom, 1)
                        // Now compile the inner pattern with the payload
                        extractAndCompileBody innerPattern body (ANF.Var payloadVar) currentEnv vg1
                        |> Result.map (fun (innerExpr, vg2) ->
                            let expr = ANF.Let (payloadVar, payloadExpr, innerExpr)
                            (expr, vg2))

            // Build the if-else chain from cases
            let rec buildChain (remaining: (AST.Pattern * AST.Expr) list) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                match remaining with
                | [] ->
                    // No cases left - shouldn't happen if we have wildcard/var
                    Error "Non-exhaustive pattern match"
                | [(pattern, body)] ->
                    // Last case - just return the body (assumes exhaustive or wildcard)
                    extractAndCompileBody pattern body scrutineeAtom env vg
                | (pattern, body) :: rest ->
                    patternToTag pattern
                    |> Result.bind (fun tagOpt ->
                        match tagOpt with
                        | None ->
                            // Wildcard or var - matches everything, no more cases
                            extractAndCompileBody pattern body scrutineeAtom env vg
                        | Some tag ->
                            // For types with any payload variants, we need to compare the tag at offset 0
                            let needsTagLoad =
                                match pattern with
                                | AST.PConstructor (variantName, _) -> typeHasAnyPayload variantName
                                | _ -> false

                            if needsTagLoad then
                                // Load tag from heap (index 0), then compare
                                let (tagVar, vg0) = ANF.freshVar vg
                                let tagLoadExpr = ANF.TupleGet (scrutineeAtom, 0)
                                let tagAtom = ANF.IntLiteral tag
                                extractAndCompileBody pattern body scrutineeAtom env vg0
                                |> Result.bind (fun (thenExpr, vg1) ->
                                    buildChain rest vg1
                                    |> Result.map (fun (elseExpr, vg2) ->
                                        let (cmpVar, vg3) = ANF.freshVar vg2
                                        let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, tagAtom)
                                        let ifExpr = ANF.If (ANF.Var cmpVar, thenExpr, elseExpr)
                                        let letCmp = ANF.Let (cmpVar, cmpExpr, ifExpr)
                                        let letTag = ANF.Let (tagVar, tagLoadExpr, letCmp)
                                        (letTag, vg3)))
                            else
                                // Simple enum - scrutinee IS the tag
                                let tagAtom = ANF.IntLiteral tag
                                extractAndCompileBody pattern body scrutineeAtom env vg
                                |> Result.bind (fun (thenExpr, vg1) ->
                                    buildChain rest vg1
                                    |> Result.map (fun (elseExpr, vg2) ->
                                        let (cmpVar, vg3) = ANF.freshVar vg2
                                        let cmpExpr = ANF.Prim (ANF.Eq, scrutineeAtom, tagAtom)
                                        let ifExpr = ANF.If (ANF.Var cmpVar, thenExpr, elseExpr)
                                        let finalExpr = ANF.Let (cmpVar, cmpExpr, ifExpr)
                                        (finalExpr, vg3))))

            buildChain cases varGen1
            |> Result.map (fun (chainExpr, varGen2) ->
                let exprWithBindings = wrapBindings scrutineeBindings chainExpr
                (exprWithBindings, varGen2)))

/// Convert an AST expression to an atom, introducing let bindings as needed
and toAtom (expr: AST.Expr) (varGen: ANF.VarGen) (env: Map<string, ANF.TempId>) (typeReg: TypeRegistry) (variantLookup: VariantLookup) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
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
        toAtom value varGen env typeReg variantLookup |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
            let (tempId, varGen2) = ANF.freshVar varGen1
            let env' = Map.add name tempId env
            toAtom body varGen2 env' typeReg variantLookup |> Result.map (fun (bodyAtom, bodyBindings, varGen3) ->
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
            toAtom (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg variantLookup

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom, create binding
        toAtom innerExpr varGen env typeReg variantLookup |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create the operation
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let anfOp = convertUnaryOp op
            let cexpr = ANF.UnaryPrim (anfOp, innerAtom)

            // Return the temp variable as atom, plus all bindings
            let allBindings = innerBindings @ [(tempVar, cexpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        toAtom left varGen env typeReg variantLookup |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg variantLookup |> Result.map (fun (rightAtom, rightBindings, varGen2) ->
                // Create the operation
                let (tempVar, varGen3) = ANF.freshVar varGen2
                let anfOp = convertBinOp op
                let cexpr = ANF.Prim (anfOp, leftAtom, rightAtom)

                // Return the temp variable as atom, plus all bindings
                let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                (ANF.Var tempVar, allBindings, varGen3)))

    | AST.If (condExpr, thenExpr, elseExpr) ->
        // If expression in atom position: convert all parts to atoms, create IfValue
        toAtom condExpr varGen env typeReg variantLookup |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toAtom thenExpr varGen1 env typeReg variantLookup |> Result.bind (fun (thenAtom, thenBindings, varGen2) ->
                toAtom elseExpr varGen2 env typeReg variantLookup |> Result.bind (fun (elseAtom, elseBindings, varGen3) ->
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
                toAtom arg vg env typeReg variantLookup
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
                toAtom elem vg env typeReg variantLookup
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
        toAtom tupleExpr varGen env typeReg variantLookup
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
        toAtom (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup

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
            toAtom recordExpr varGen env typeReg variantLookup
            |> Result.bind (fun (recordAtom, recordBindings, varGen1) ->
                let (tempVar, varGen2) = ANF.freshVar varGen1
                let getCExpr = ANF.TupleGet (recordAtom, index)
                let allBindings = recordBindings @ [(tempVar, getCExpr)]
                Ok (ANF.Var tempVar, allBindings, varGen2))
        | None ->
            Error $"Unknown field: {fieldName}"

    | AST.Constructor (_, variantName, payload) ->
        match Map.tryFind variantName variantLookup with
        | None ->
            Error $"Unknown constructor: {variantName}"
        | Some (typeName, tag, _) ->
            // Check if ANY variant in this type has a payload
            // Note: We get typeName from variantLookup, not from AST (which may be empty)
            let typeHasPayloadVariants =
                variantLookup
                |> Map.exists (fun _ (tName, _, pType) -> tName = typeName && pType.IsSome)

            match payload with
            | None when not typeHasPayloadVariants ->
                // Pure enum type: return tag as an integer (no bindings needed)
                Ok (ANF.IntLiteral (int64 tag), [], varGen)
            | None ->
                // No payload but type has other variants with payloads
                // Heap-allocate as [tag] for consistent representation
                let tagAtom = ANF.IntLiteral (int64 tag)
                let (tempVar, varGen1) = ANF.freshVar varGen
                let tupleCExpr = ANF.TupleAlloc [tagAtom]
                Ok (ANF.Var tempVar, [(tempVar, tupleCExpr)], varGen1)
            | Some payloadExpr ->
                // Variant with payload: allocate [tag, payload] on heap
                toAtom payloadExpr varGen env typeReg variantLookup
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (int64 tag)
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (tempVar, varGen2) = ANF.freshVar varGen1
                    let tupleCExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let allBindings = payloadBindings @ [(tempVar, tupleCExpr)]
                    (ANF.Var tempVar, allBindings, varGen2))

    | AST.Match (scrutinee, cases) ->
        // Match in atom position - compile and extract result
        toANF (AST.Match (scrutinee, cases)) varGen env typeReg variantLookup
        |> Result.bind (fun (matchExpr, varGen1) ->
            // The match compiles to an if-else chain that returns a value
            // We need to extract that value into a temp variable
            // For now, just return an error - complex match in atom position needs more work
            Error "Match expressions in atom position not yet supported (use let binding)")

/// Wrap let bindings around an expression
and wrapBindings (bindings: (ANF.TempId * ANF.CExpr) list) (expr: ANF.AExpr) : ANF.AExpr =
    List.foldBack (fun (var, cexpr) acc -> ANF.Let (var, cexpr, acc)) bindings expr

/// Convert a function definition to ANF
let convertFunction (funcDef: AST.FunctionDef) (varGen: ANF.VarGen) (typeReg: TypeRegistry) (variantLookup: VariantLookup) : Result<ANF.Function * ANF.VarGen, string> =
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
    toANF funcDef.Body varGen1 paramEnv typeReg variantLookup
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

    // Build variant lookup from sum type definitions
    // Maps variant name -> (type name, tag index, payload type)
    let variantLookup : VariantLookup =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.SumTypeDef (typeName, variants)) ->
                Some (typeName, variants)
            | _ -> None)
        |> List.collect (fun (typeName, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, idx, variant.Payload))))
        |> Map.ofList

    // Separate functions and expressions
    let functions = topLevels |> List.choose (function AST.FunctionDef f -> Some f | _ -> None)
    let expressions = topLevels |> List.choose (function AST.Expression e -> Some e | _ -> None)

    // Convert all functions
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg typeReg variantLookup
            |> Result.bind (fun (anfFunc, vg') ->
                convertFunctions rest vg' (anfFunc :: acc))

    convertFunctions functions varGen []
    |> Result.bind (fun (anfFuncs, varGen1) ->
        // Convert main expression (if any)
        match expressions with
        | [expr] ->
            toANF expr varGen1 Map.empty typeReg variantLookup
            |> Result.map (fun (anfExpr, _) ->
                ANF.Program (anfFuncs, Some anfExpr))
        | [] ->
            // No main expression - just functions
            Ok (ANF.Program (anfFuncs, None))
        | _ ->
            Error "Multiple top-level expressions not allowed")
