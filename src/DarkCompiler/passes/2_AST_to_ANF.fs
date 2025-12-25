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

/// Function registry - maps function names to their return types
type FunctionRegistry = Map<string, AST.Type>

/// Variable environment - maps variable names to their TempIds and types
/// The type information is used for type-directed field lookup in record access
type VarEnv = Map<string, ANF.TempId * AST.Type>

/// Extract just the type environment from VarEnv for use with inferType
let typeEnvFromVarEnv (varEnv: VarEnv) : Map<string, AST.Type> =
    varEnv |> Map.map (fun _ (_, t) -> t)

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

/// Infer the type of an expression using type environment and registries
/// Used for type-directed field lookup in record access
let rec inferType (expr: AST.Expr) (typeEnv: Map<string, AST.Type>) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) : Result<AST.Type, string> =
    match expr with
    | AST.IntLiteral _ -> Ok AST.TInt64
    | AST.BoolLiteral _ -> Ok AST.TBool
    | AST.StringLiteral _ -> Ok AST.TString
    | AST.FloatLiteral _ -> Ok AST.TFloat64
    | AST.Var name ->
        match Map.tryFind name typeEnv with
        | Some t -> Ok t
        | None -> Error $"Cannot infer type: undefined variable '{name}'"
    | AST.RecordLiteral (typeName, fields) ->
        if typeName = "" then
            // Anonymous record literal - try to find matching type by field names
            let literalFieldNames = fields |> List.map fst |> Set.ofList
            let matchingTypes =
                typeReg
                |> Map.toList
                |> List.filter (fun (_, typeFields) ->
                    let typeFieldNames = typeFields |> List.map fst |> Set.ofList
                    typeFieldNames = literalFieldNames)
                |> List.map fst
            match matchingTypes with
            | [singleMatch] -> Ok (AST.TRecord singleMatch)
            | [] -> Error "Cannot infer type: no record type matches the field names"
            | matches ->
                let names = String.concat ", " matches
                Error $"Ambiguous record literal: matches multiple types: {names}"
        else
            Ok (AST.TRecord typeName)
    | AST.RecordAccess (recordExpr, fieldName) ->
        inferType recordExpr typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFind (fun (name, _) -> name = fieldName) fields with
                    | Some (_, fieldType) -> Ok fieldType
                    | None -> Error $"Record type {typeName} has no field '{fieldName}'"
                | None -> Error $"Unknown record type: {typeName}"
            | _ -> Error $"Cannot access field on non-record type")
    | AST.TupleLiteral elems ->
        elems
        |> List.map (fun e -> inferType e typeEnv typeReg variantLookup funcReg)
        |> List.fold (fun acc r ->
            match acc, r with
            | Ok types, Ok t -> Ok (types @ [t])
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.map AST.TTuple
    | AST.TupleAccess (tupleExpr, index) ->
        inferType tupleExpr typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun tupleType ->
            match tupleType with
            | AST.TTuple elemTypes when index >= 0 && index < List.length elemTypes ->
                Ok (List.item index elemTypes)
            | AST.TTuple _ -> Error $"Tuple index {index} out of bounds"
            | _ -> Error "Cannot access index on non-tuple type")
    | AST.Constructor (_, variantName, _) ->
        match Map.tryFind variantName variantLookup with
        | Some (typeName, _, _) -> Ok (AST.TSum typeName)
        | None -> Error $"Unknown constructor: {variantName}"
    | AST.ListLiteral _ -> Ok AST.TList
    | AST.Let (name, value, body) ->
        inferType value typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun valueType ->
            let typeEnv' = Map.add name valueType typeEnv
            inferType body typeEnv' typeReg variantLookup funcReg)
    | AST.If (_, thenExpr, _) ->
        // Both branches should have same type, just infer from then branch
        inferType thenExpr typeEnv typeReg variantLookup funcReg
    | AST.BinOp (op, _, _) ->
        match op with
        | AST.Add | AST.Sub | AST.Mul | AST.Div -> Ok AST.TInt64
        | AST.Eq | AST.Neq | AST.Lt | AST.Gt | AST.Lte | AST.Gte | AST.And | AST.Or -> Ok AST.TBool
    | AST.UnaryOp (op, _) ->
        match op with
        | AST.Neg -> Ok AST.TInt64
        | AST.Not -> Ok AST.TBool
    | AST.Match (_, cases) ->
        // Infer from first case body
        match cases with
        | (_, body) :: _ -> inferType body typeEnv typeReg variantLookup funcReg
        | [] -> Error "Empty match expression"
    | AST.Call (funcName, _) ->
        // Look up function return type from the function registry
        match Map.tryFind funcName funcReg with
        | Some returnType -> Ok returnType
        | None -> Error $"Unknown function: '{funcName}'"

/// Convert AST expression to ANF
/// env maps user variable names to ANF TempIds and their types
/// typeReg maps record type names to field definitions
/// variantLookup maps variant names to (type name, tag index)
/// funcReg maps function names to their return types
let rec toANF (expr: AST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) : Result<ANF.AExpr * ANF.VarGen, string> =
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
        | Some (tempId, _) -> Ok (ANF.Return (ANF.Var tempId), varGen)
        | None -> Error $"Undefined variable: {name}"

    | AST.Let (name, value, body) ->
        // Let binding: convert value to atom, allocate fresh temp, convert body with extended env
        // Infer the type of the value for type-directed field lookup
        let typeEnv = typeEnvFromVarEnv env
        inferType value typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun valueType ->
            toAtom value varGen env typeReg variantLookup funcReg |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
                let (tempId, varGen2) = ANF.freshVar varGen1
                let env' = Map.add name (tempId, valueType) env
                toANF body varGen2 env' typeReg variantLookup funcReg |> Result.map (fun (bodyExpr, varGen3) ->
                    // Build: valueBindings + let tempId = valueAtom + body
                    let finalExpr = ANF.Let (tempId, ANF.Atom valueAtom, bodyExpr)
                    let exprWithBindings = wrapBindings valueBindings finalExpr
                    (exprWithBindings, varGen3))))

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
            toANF (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg variantLookup funcReg

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom
        toAtom innerExpr varGen env typeReg variantLookup funcReg |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
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
        toAtom left varGen env typeReg variantLookup funcReg |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg variantLookup funcReg |> Result.map (fun (rightAtom, rightBindings, varGen2) ->
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
        toAtom cond varGen env typeReg variantLookup funcReg |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toANF thenBranch varGen1 env typeReg variantLookup funcReg |> Result.bind (fun (thenExpr, varGen2) ->
                toANF elseBranch varGen2 env typeReg variantLookup funcReg |> Result.map (fun (elseExpr, varGen3) ->
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
                toAtom arg vg env typeReg variantLookup funcReg
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
                toAtom elem vg env typeReg variantLookup funcReg
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
        toAtom tupleExpr varGen env typeReg variantLookup funcReg
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
        toANF (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup funcReg

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        // Use type-directed lookup: infer the record type, then find field index
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFindIndex (fun (name, _) -> name = fieldName) fields with
                    | Some index ->
                        toAtom recordExpr varGen env typeReg variantLookup funcReg
                        |> Result.map (fun (recordAtom, recordBindings, varGen1) ->
                            let (resultVar, varGen2) = ANF.freshVar varGen1
                            let getExpr = ANF.TupleGet (recordAtom, index)
                            let finalExpr = ANF.Let (resultVar, getExpr, ANF.Return (ANF.Var resultVar))
                            let exprWithBindings = wrapBindings recordBindings finalExpr
                            (exprWithBindings, varGen2))
                    | None ->
                        Error $"Record type '{typeName}' has no field '{fieldName}'"
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error $"Cannot access field '{fieldName}' on non-record type")

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
                toAtom payloadExpr varGen env typeReg variantLookup funcReg
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (int64 tag)
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (resultVar, varGen2) = ANF.freshVar varGen1
                    let tupleExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let finalExpr = ANF.Let (resultVar, tupleExpr, ANF.Return (ANF.Var resultVar))
                    let exprWithBindings = wrapBindings payloadBindings finalExpr
                    (exprWithBindings, varGen2))

    | AST.ListLiteral elements ->
        // Compile list literal as linked list: Nil = 0, Cons = [tag=1, head, tail]
        // Build right-to-left: start with Nil, then Cons(last, Nil), etc.
        let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (tailAtom: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (tailAtom, allBindings, vg)
            | elem :: rest ->
                // First build the rest of the list
                buildList rest vg tailAtom allBindings
                |> Result.bind (fun (restAtom, restBindings, vg1) ->
                    // Now add this element: Cons(elem, rest)
                    toAtom elem vg1 env typeReg variantLookup funcReg
                    |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                        let (consVar, vg3) = ANF.freshVar vg2
                        // Cons = [tag=1, head, tail]
                        let consExpr = ANF.TupleAlloc [ANF.IntLiteral 1L; elemAtom; restAtom]
                        let newBindings = restBindings @ elemBindings @ [(consVar, consExpr)]
                        (ANF.Var consVar, newBindings, vg3)))

        if List.isEmpty elements then
            // Empty list is Nil (represented as 0)
            Ok (ANF.Return (ANF.IntLiteral 0L), varGen)
        else
            // Build the list starting with Nil
            buildList elements varGen (ANF.IntLiteral 0L) []
            |> Result.map (fun (listAtom, listBindings, varGen1) ->
                let finalExpr = ANF.Return listAtom
                let exprWithBindings = wrapBindings listBindings finalExpr
                (exprWithBindings, varGen1))

    | AST.Match (scrutinee, cases) ->
        // Compile match to if-else chain
        // First convert scrutinee to atom
        toAtom scrutinee varGen env typeReg variantLookup funcReg
        |> Result.bind (fun (scrutineeAtom, scrutineeBindings, varGen1) ->
            // Check if any pattern needs to access list structure
            // If so, we must ensure scrutinee is a variable (can't TupleGet on literal)
            let hasNonEmptyListPattern =
                cases |> List.exists (fun (pat, _) ->
                    match pat with
                    | AST.PList (_ :: _) -> true
                    | _ -> false)

            // If there are non-empty list patterns, bind the scrutinee to a variable
            let (scrutineeAtom', scrutineeBindings', varGen1') =
                match scrutineeAtom with
                | ANF.Var _ -> (scrutineeAtom, scrutineeBindings, varGen1)
                | _ when hasNonEmptyListPattern ->
                    let (tempVar, vg) = ANF.freshVar varGen1
                    (ANF.Var tempVar, scrutineeBindings @ [(tempVar, ANF.Atom scrutineeAtom)], vg)
                | _ -> (scrutineeAtom, scrutineeBindings, varGen1)

            // Check if the TYPE that a variant belongs to has any variant with a payload
            // This determines if values are heap-allocated or simple integers
            let typeHasAnyPayload (variantName: string) : bool =
                match Map.tryFind variantName variantLookup with
                | Some (typeName, _, _) ->
                    variantLookup
                    |> Map.exists (fun _ (tName, _, pType) -> tName = typeName && pType.IsSome)
                | None -> false

            // Check if pattern always matches (wildcard or variable)
            let rec patternAlwaysMatches (pattern: AST.Pattern) : bool =
                match pattern with
                | AST.PWildcard -> true
                | AST.PVar _ -> true
                | _ -> false

            // Extract pattern bindings and compile body with extended environment
            // Note: Pattern-bound variables use TInt64 as placeholder type since full type inference
            // for pattern variables requires scrutinee type analysis (future improvement)
            let rec extractAndCompileBody (pattern: AST.Pattern) (body: AST.Expr) (scrutAtom: ANF.Atom) (currentEnv: VarEnv) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                match pattern with
                | AST.PWildcard -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PLiteral _ -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PBool _ -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PString _ -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PFloat _ -> toANF body vg currentEnv typeReg variantLookup funcReg
                | AST.PVar name ->
                    // Bind scrutinee to variable name
                    let (tempId, vg1) = ANF.freshVar vg
                    // Use TInt64 as placeholder - pattern variable type tracking is future work
                    let env' = Map.add name (tempId, AST.TInt64) currentEnv
                    toANF body vg1 env' typeReg variantLookup funcReg
                    |> Result.map (fun (bodyExpr, vg2) ->
                        let expr = ANF.Let (tempId, ANF.Atom scrutAtom, bodyExpr)
                        (expr, vg2))
                | AST.PConstructor (_, payloadPattern) ->
                    match payloadPattern with
                    | None -> toANF body vg currentEnv typeReg variantLookup funcReg
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
                | AST.PTuple patterns ->
                    // Extract each element and bind pattern variables
                    let rec extractTupleElements (pats: AST.Pattern list) (idx: int) (env: VarEnv) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                        match pats with
                        | [] -> toANF body vg env typeReg variantLookup funcReg
                        | pat :: rest ->
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemExpr = ANF.TupleGet (scrutAtom, idx)
                            extractAndCompileBody pat body (ANF.Var elemVar) env vg1
                            |> Result.bind (fun _ ->
                                // For non-binding patterns, just continue
                                // For binding patterns, add to env with placeholder type
                                let newEnv =
                                    match pat with
                                    | AST.PVar name -> Map.add name (elemVar, AST.TInt64) env
                                    | _ -> env
                                extractTupleElements rest (idx + 1) newEnv vg1)
                    // Simplified: extract all bindings first, then compile body
                    let rec collectTupleBindings (pats: AST.Pattern list) (idx: int) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, List.rev bindings, vg)
                        | pat :: rest ->
                            let (elemVar, vg1) = ANF.freshVar vg
                            let elemExpr = ANF.TupleGet (scrutAtom, idx)
                            let binding = (elemVar, elemExpr)
                            match pat with
                            | AST.PVar name ->
                                // Use TInt64 as placeholder type
                                let newEnv = Map.add name (elemVar, AST.TInt64) env
                                collectTupleBindings rest (idx + 1) newEnv (binding :: bindings) vg1
                            | AST.PWildcard ->
                                collectTupleBindings rest (idx + 1) env bindings vg1
                            | _ ->
                                // For nested patterns, we need more complex handling
                                // For now, just continue with the element bound
                                collectTupleBindings rest (idx + 1) env (binding :: bindings) vg1
                    collectTupleBindings patterns 0 currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings bindings bodyExpr
                            (finalExpr, vg2)))
                | AST.PRecord (_, fieldPatterns) ->
                    // Extract each field and bind pattern variables
                    let rec collectRecordBindings (fields: (string * AST.Pattern) list) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) (fieldIdx: int) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match fields with
                        | [] -> Ok (env, List.rev bindings, vg)
                        | (_, pat) :: rest ->
                            let (fieldVar, vg1) = ANF.freshVar vg
                            let fieldExpr = ANF.TupleGet (scrutAtom, fieldIdx)
                            let binding = (fieldVar, fieldExpr)
                            match pat with
                            | AST.PVar name ->
                                // Use TInt64 as placeholder type
                                let newEnv = Map.add name (fieldVar, AST.TInt64) env
                                collectRecordBindings rest newEnv (binding :: bindings) vg1 (fieldIdx + 1)
                            | AST.PWildcard ->
                                collectRecordBindings rest env bindings vg1 (fieldIdx + 1)
                            | _ ->
                                collectRecordBindings rest env (binding :: bindings) vg1 (fieldIdx + 1)
                    collectRecordBindings fieldPatterns currentEnv [] vg 0
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings bindings bodyExpr
                            (finalExpr, vg2)))
                | AST.PList patterns ->
                    // Extract list elements: walk through Cons cells
                    // List layout: Nil = 0, Cons = [tag=1, head, tail]
                    let rec collectListBindings (pats: AST.Pattern list) (listAtom: ANF.Atom) (env: VarEnv) (bindings: (ANF.TempId * ANF.CExpr) list) (vg: ANF.VarGen) : Result<VarEnv * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
                        match pats with
                        | [] -> Ok (env, List.rev bindings, vg)
                        | pat :: rest ->
                            // Extract head from current Cons cell (index 1)
                            let (headVar, vg1) = ANF.freshVar vg
                            let headExpr = ANF.TupleGet (listAtom, 1)
                            let headBinding = (headVar, headExpr)
                            // Extract tail from current Cons cell (index 2)
                            let (tailVar, vg2) = ANF.freshVar vg1
                            let tailExpr = ANF.TupleGet (listAtom, 2)
                            let tailBinding = (tailVar, tailExpr)
                            match pat with
                            | AST.PVar name ->
                                // Use TInt64 as placeholder type
                                let newEnv = Map.add name (headVar, AST.TInt64) env
                                collectListBindings rest (ANF.Var tailVar) newEnv (tailBinding :: headBinding :: bindings) vg2
                            | AST.PWildcard ->
                                collectListBindings rest (ANF.Var tailVar) env (tailBinding :: bindings) vg2
                            | _ ->
                                collectListBindings rest (ANF.Var tailVar) env (tailBinding :: headBinding :: bindings) vg2
                    collectListBindings patterns scrutAtom currentEnv [] vg
                    |> Result.bind (fun (newEnv, bindings, vg1) ->
                        toANF body vg1 newEnv typeReg variantLookup funcReg
                        |> Result.map (fun (bodyExpr, vg2) ->
                            let finalExpr = wrapBindings bindings bodyExpr
                            (finalExpr, vg2)))

            // Build comparison expression for a pattern
            let rec buildPatternComparison (pattern: AST.Pattern) (scrutAtom: ANF.Atom) (vg: ANF.VarGen) : Result<(ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen) option, string> =
                match pattern with
                | AST.PWildcard -> Ok None
                | AST.PVar _ -> Ok None
                | AST.PLiteral n ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral n)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PBool b ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.BoolLiteral b)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PString s ->
                    // String comparison - for now just compare as atoms
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.StringLiteral s)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PFloat f ->
                    let (cmpVar, vg1) = ANF.freshVar vg
                    let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.FloatLiteral f)
                    Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                | AST.PConstructor (variantName, _) ->
                    match Map.tryFind variantName variantLookup with
                    | Some (_, tag, _) ->
                        if typeHasAnyPayload variantName then
                            // Load tag from heap (index 0), then compare
                            let (tagVar, vg1) = ANF.freshVar vg
                            let tagLoadExpr = ANF.TupleGet (scrutAtom, 0)
                            let (cmpVar, vg2) = ANF.freshVar vg1
                            let cmpExpr = ANF.Prim (ANF.Eq, ANF.Var tagVar, ANF.IntLiteral (int64 tag))
                            Ok (Some (ANF.Var cmpVar, [(tagVar, tagLoadExpr); (cmpVar, cmpExpr)], vg2))
                        else
                            // Simple enum - scrutinee IS the tag
                            let (cmpVar, vg1) = ANF.freshVar vg
                            let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral (int64 tag))
                            Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                    | None -> Error $"Unknown constructor in pattern: {variantName}"
                | AST.PTuple _ -> Ok None  // Tuple patterns just bind, don't compare
                | AST.PRecord _ -> Ok None  // Record patterns just bind, don't compare
                | AST.PList patterns ->
                    // List pattern comparison: check length matches
                    // [] matches Nil (0), [a, b, ...] needs scrutinee to be non-nil first
                    // IMPORTANT: Must check non-nil BEFORE loading tag to avoid null deref
                    if List.isEmpty patterns then
                        // Empty list pattern: check scrutinee == 0
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Prim (ANF.Eq, scrutAtom, ANF.IntLiteral 0L)
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))
                    else
                        // Non-empty list pattern: check scrutinee != 0 (not Nil)
                        // We only check that it's a Cons (non-nil). Length matching
                        // is not fully validated here - mismatched lengths may crash
                        // when extracting elements. Full validation would need nested ifs.
                        let (cmpVar, vg1) = ANF.freshVar vg
                        let cmpExpr = ANF.Prim (ANF.Neq, scrutAtom, ANF.IntLiteral 0L)
                        Ok (Some (ANF.Var cmpVar, [(cmpVar, cmpExpr)], vg1))

            // Compile a list pattern with proper length validation.
            // Generates nested if-expressions that check each element exists before extracting.
            // This fixes the bug where [a, b] pattern would crash when matching against [1].
            let rec compileListPatternWithChecks
                (patterns: AST.Pattern list)
                (listAtom: ANF.Atom)
                (currentEnv: VarEnv)
                (body: AST.Expr)
                (elseExpr: ANF.AExpr)
                (vg: ANF.VarGen)
                : Result<ANF.AExpr * ANF.VarGen, string> =

                match patterns with
                | [] ->
                    // All elements extracted - compile the body
                    // Note: We don't check tail == 0 here, so lists with more elements
                    // than the pattern will still match. This is a simplification.
                    // TODO: Add exact length matching if needed
                    toANF body vg currentEnv typeReg variantLookup funcReg

                | pat :: restPatterns ->
                    // Check current position is non-nil (list has at least one more element)
                    let (checkVar, vg1) = ANF.freshVar vg
                    let checkExpr = ANF.Prim (ANF.Neq, listAtom, ANF.IntLiteral 0L)

                    // Extract head (index 1) and tail (index 2) from Cons cell
                    let (headVar, vg2) = ANF.freshVar vg1
                    let headExpr = ANF.TupleGet (listAtom, 1)
                    let (tailVar, vg3) = ANF.freshVar vg2
                    let tailExpr = ANF.TupleGet (listAtom, 2)

                    // Update environment based on pattern type
                    let newEnv =
                        match pat with
                        | AST.PVar name -> Map.add name (headVar, AST.TInt64) currentEnv
                        | _ -> currentEnv

                    // Recursively compile rest of list pattern
                    compileListPatternWithChecks restPatterns (ANF.Var tailVar) newEnv body elseExpr vg3
                    |> Result.map (fun (innerExpr, vg4) ->
                        // Build nested structure:
                        // let checkVar = (listAtom != 0) in
                        //   if checkVar then
                        //     let headVar = TupleGet(listAtom, 1) in
                        //     let tailVar = TupleGet(listAtom, 2) in
                        //     <innerExpr>
                        //   else
                        //     <elseExpr>
                        let withTail = ANF.Let (tailVar, tailExpr, innerExpr)
                        let withHead = ANF.Let (headVar, headExpr, withTail)
                        let ifExpr = ANF.If (ANF.Var checkVar, withHead, elseExpr)
                        (ANF.Let (checkVar, checkExpr, ifExpr), vg4))

            // Build the if-else chain from cases
            let rec buildChain (remaining: (AST.Pattern * AST.Expr) list) (vg: ANF.VarGen) : Result<ANF.AExpr * ANF.VarGen, string> =
                match remaining with
                | [] ->
                    // No cases left - shouldn't happen if we have wildcard/var
                    Error "Non-exhaustive pattern match"
                | [(pattern, body)] ->
                    // Last case - for most patterns just compile the body
                    // For non-empty list patterns, we still need proper length checking
                    match pattern with
                    | AST.PList (_ :: _ as listPatterns) ->
                        // For list patterns as last case, generate proper checks
                        // If the pattern doesn't match, this is a non-exhaustive match
                        // We generate an expression that returns 0 as a fallback (TODO: proper error)
                        let fallbackExpr = ANF.Return (ANF.IntLiteral 0L)
                        compileListPatternWithChecks listPatterns scrutineeAtom' env body fallbackExpr vg
                    | _ ->
                        // Other patterns - original behavior
                        extractAndCompileBody pattern body scrutineeAtom' env vg
                | (pattern, body) :: rest ->
                    if patternAlwaysMatches pattern then
                        // Wildcard or var - matches everything, no more cases
                        extractAndCompileBody pattern body scrutineeAtom' env vg
                    else
                        // Non-empty list patterns need special handling with interleaved checks
                        match pattern with
                        | AST.PList (_ :: _ as listPatterns) ->
                            // Build the else branch first (rest of cases)
                            buildChain rest vg
                            |> Result.bind (fun (elseExpr, vg1) ->
                                // Use the new interleaved check-and-extract function
                                compileListPatternWithChecks listPatterns scrutineeAtom' env body elseExpr vg1)
                        | _ ->
                            // Original logic for other patterns
                            buildPatternComparison pattern scrutineeAtom' vg
                            |> Result.bind (fun cmpOpt ->
                                match cmpOpt with
                                | None ->
                                    // Pattern always matches
                                    extractAndCompileBody pattern body scrutineeAtom' env vg
                                | Some (condAtom, bindings, vg1) ->
                                    extractAndCompileBody pattern body scrutineeAtom' env vg1
                                    |> Result.bind (fun (thenExpr, vg2) ->
                                        buildChain rest vg2
                                        |> Result.map (fun (elseExpr, vg3) ->
                                            let ifExpr = ANF.If (condAtom, thenExpr, elseExpr)
                                            let finalExpr = wrapBindings bindings ifExpr
                                            (finalExpr, vg3))))

            buildChain cases varGen1'
            |> Result.map (fun (chainExpr, varGen2) ->
                let exprWithBindings = wrapBindings scrutineeBindings' chainExpr
                (exprWithBindings, varGen2)))

/// Convert an AST expression to an atom, introducing let bindings as needed
and toAtom (expr: AST.Expr) (varGen: ANF.VarGen) (env: VarEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
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
        | Some (tempId, _) -> Ok (ANF.Var tempId, [], varGen)
        | None -> Error $"Undefined variable: {name}"

    | AST.Let (name, value, body) ->
        // Let binding in atom position: need to evaluate and return the body as an atom
        // Infer the type of the value for type-directed field lookup
        let typeEnv = typeEnvFromVarEnv env
        inferType value typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun valueType ->
            toAtom value varGen env typeReg variantLookup funcReg |> Result.bind (fun (valueAtom, valueBindings, varGen1) ->
                let (tempId, varGen2) = ANF.freshVar varGen1
                let env' = Map.add name (tempId, valueType) env
                toAtom body varGen2 env' typeReg variantLookup funcReg |> Result.map (fun (bodyAtom, bodyBindings, varGen3) ->
                    // All bindings: valueBindings + binding tempId to value + bodyBindings
                    let allBindings = valueBindings @ [(tempId, ANF.Atom valueAtom)] @ bodyBindings
                    (bodyAtom, allBindings, varGen3))))

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
            toAtom (AST.BinOp (AST.Sub, AST.IntLiteral 0L, innerExpr)) varGen env typeReg variantLookup funcReg

    | AST.UnaryOp (op, innerExpr) ->
        // Unary operation: convert operand to atom, create binding
        toAtom innerExpr varGen env typeReg variantLookup funcReg |> Result.map (fun (innerAtom, innerBindings, varGen1) ->
            // Create the operation
            let (tempVar, varGen2) = ANF.freshVar varGen1
            let anfOp = convertUnaryOp op
            let cexpr = ANF.UnaryPrim (anfOp, innerAtom)

            // Return the temp variable as atom, plus all bindings
            let allBindings = innerBindings @ [(tempVar, cexpr)]
            (ANF.Var tempVar, allBindings, varGen2))

    | AST.BinOp (op, left, right) ->
        // Complex expression: convert operands to atoms, create binding
        toAtom left varGen env typeReg variantLookup funcReg |> Result.bind (fun (leftAtom, leftBindings, varGen1) ->
            toAtom right varGen1 env typeReg variantLookup funcReg |> Result.map (fun (rightAtom, rightBindings, varGen2) ->
                // Create the operation
                let (tempVar, varGen3) = ANF.freshVar varGen2
                let anfOp = convertBinOp op
                let cexpr = ANF.Prim (anfOp, leftAtom, rightAtom)

                // Return the temp variable as atom, plus all bindings
                let allBindings = leftBindings @ rightBindings @ [(tempVar, cexpr)]
                (ANF.Var tempVar, allBindings, varGen3)))

    | AST.If (condExpr, thenExpr, elseExpr) ->
        // If expression in atom position: convert all parts to atoms, create IfValue
        toAtom condExpr varGen env typeReg variantLookup funcReg |> Result.bind (fun (condAtom, condBindings, varGen1) ->
            toAtom thenExpr varGen1 env typeReg variantLookup funcReg |> Result.bind (fun (thenAtom, thenBindings, varGen2) ->
                toAtom elseExpr varGen2 env typeReg variantLookup funcReg |> Result.bind (fun (elseAtom, elseBindings, varGen3) ->
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
                toAtom arg vg env typeReg variantLookup funcReg
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
                toAtom elem vg env typeReg variantLookup funcReg
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
        toAtom tupleExpr varGen env typeReg variantLookup funcReg
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
        toAtom (AST.TupleLiteral orderedValues) varGen env typeReg variantLookup funcReg

    | AST.RecordAccess (recordExpr, fieldName) ->
        // Records are compiled like tuples - field access becomes TupleGet
        // Use type-directed lookup: infer the record type, then find field index
        let typeEnv = typeEnvFromVarEnv env
        inferType recordExpr typeEnv typeReg variantLookup funcReg
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord typeName ->
                // Look up field index in the specific record type
                match Map.tryFind typeName typeReg with
                | Some fields ->
                    match List.tryFindIndex (fun (name, _) -> name = fieldName) fields with
                    | Some index ->
                        toAtom recordExpr varGen env typeReg variantLookup funcReg
                        |> Result.bind (fun (recordAtom, recordBindings, varGen1) ->
                            let (tempVar, varGen2) = ANF.freshVar varGen1
                            let getCExpr = ANF.TupleGet (recordAtom, index)
                            let allBindings = recordBindings @ [(tempVar, getCExpr)]
                            Ok (ANF.Var tempVar, allBindings, varGen2))
                    | None ->
                        Error $"Record type '{typeName}' has no field '{fieldName}'"
                | None ->
                    Error $"Unknown record type: {typeName}"
            | _ ->
                Error $"Cannot access field '{fieldName}' on non-record type")

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
                toAtom payloadExpr varGen env typeReg variantLookup funcReg
                |> Result.map (fun (payloadAtom, payloadBindings, varGen1) ->
                    let tagAtom = ANF.IntLiteral (int64 tag)
                    // Create TupleAlloc [tag, payload] and bind to fresh variable
                    let (tempVar, varGen2) = ANF.freshVar varGen1
                    let tupleCExpr = ANF.TupleAlloc [tagAtom; payloadAtom]
                    let allBindings = payloadBindings @ [(tempVar, tupleCExpr)]
                    (ANF.Var tempVar, allBindings, varGen2))

    | AST.ListLiteral elements ->
        // Compile list literal as linked list in atom position
        let rec buildList (elems: AST.Expr list) (vg: ANF.VarGen) (tailAtom: ANF.Atom) (allBindings: (ANF.TempId * ANF.CExpr) list) : Result<ANF.Atom * (ANF.TempId * ANF.CExpr) list * ANF.VarGen, string> =
            match elems with
            | [] -> Ok (tailAtom, allBindings, vg)
            | elem :: rest ->
                // First build the rest of the list
                buildList rest vg tailAtom allBindings
                |> Result.bind (fun (restAtom, restBindings, vg1) ->
                    // Now add this element: Cons(elem, rest)
                    toAtom elem vg1 env typeReg variantLookup funcReg
                    |> Result.map (fun (elemAtom, elemBindings, vg2) ->
                        let (consVar, vg3) = ANF.freshVar vg2
                        // Cons = [tag=1, head, tail]
                        let consExpr = ANF.TupleAlloc [ANF.IntLiteral 1L; elemAtom; restAtom]
                        let newBindings = restBindings @ elemBindings @ [(consVar, consExpr)]
                        (ANF.Var consVar, newBindings, vg3)))

        if List.isEmpty elements then
            // Empty list is Nil (represented as 0)
            Ok (ANF.IntLiteral 0L, [], varGen)
        else
            // Build the list starting with Nil
            buildList elements varGen (ANF.IntLiteral 0L) []

    | AST.Match (scrutinee, cases) ->
        // Match in atom position - compile and extract result
        toANF (AST.Match (scrutinee, cases)) varGen env typeReg variantLookup funcReg
        |> Result.bind (fun (matchExpr, varGen1) ->
            // The match compiles to an if-else chain that returns a value
            // We need to extract that value into a temp variable
            // For now, just return an error - complex match in atom position needs more work
            Error "Match expressions in atom position not yet supported (use let binding)")

/// Wrap let bindings around an expression
and wrapBindings (bindings: (ANF.TempId * ANF.CExpr) list) (expr: ANF.AExpr) : ANF.AExpr =
    List.foldBack (fun (var, cexpr) acc -> ANF.Let (var, cexpr, acc)) bindings expr

/// Convert a function definition to ANF
let convertFunction (funcDef: AST.FunctionDef) (varGen: ANF.VarGen) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) : Result<ANF.Function * ANF.VarGen, string> =
    // Allocate TempIds for parameters
    let (paramIds, varGen1) =
        funcDef.Params
        |> List.fold (fun (ids, vg) (_, _) ->
            let (tempId, vg') = ANF.freshVar vg
            (ids @ [tempId], vg')) ([], varGen)

    // Build environment mapping param names to (TempId, Type)
    let paramEnv : VarEnv =
        List.zip funcDef.Params paramIds
        |> List.map (fun ((name, typ), tempId) -> (name, (tempId, typ)))
        |> Map.ofList

    // Convert body
    toANF funcDef.Body varGen1 paramEnv typeReg variantLookup funcReg
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

    // Build function registry - maps function names to their return types
    let funcReg : FunctionRegistry =
        functions
        |> List.map (fun f -> (f.Name, f.ReturnType))
        |> Map.ofList

    // Convert all functions
    let rec convertFunctions (funcs: AST.FunctionDef list) (vg: ANF.VarGen) (acc: ANF.Function list) : Result<ANF.Function list * ANF.VarGen, string> =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunction func vg typeReg variantLookup funcReg
            |> Result.bind (fun (anfFunc, vg') ->
                convertFunctions rest vg' (anfFunc :: acc))

    convertFunctions functions varGen []
    |> Result.bind (fun (anfFuncs, varGen1) ->
        // Convert main expression (if any)
        match expressions with
        | [expr] ->
            let emptyEnv : VarEnv = Map.empty
            toANF expr varGen1 emptyEnv typeReg variantLookup funcReg
            |> Result.map (fun (anfExpr, _) ->
                ANF.Program (anfFuncs, Some anfExpr))
        | [] ->
            // No main expression - just functions
            Ok (ANF.Program (anfFuncs, None))
        | _ ->
            Error "Multiple top-level expressions not allowed")
