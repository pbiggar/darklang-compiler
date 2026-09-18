// TypeSubstitution.fs - Substitute concrete source types and instantiate generic function bodies.

module TypeSubstitution

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity

type Substitution = Map<string, AST.Type>

/// Apply a substitution to a type, replacing type variables with concrete types
let rec applySubstToType (subst: Substitution) (typ: AST.Type) : AST.Type =
    match typ with
    | AST.TVar name ->
        match Map.tryFind name subst with
        | Some concreteType -> concreteType
        | None -> typ  // Unbound type variable remains as-is
    | AST.TFunction (paramTypes, returnType) ->
        AST.TFunction (List.map (applySubstToType subst) paramTypes, applySubstToType subst returnType)
    | AST.TTuple elemTypes ->
        AST.TTuple (List.map (applySubstToType subst) elemTypes)
    | AST.TList elemType ->
        AST.TList (applySubstToType subst elemType)
    | AST.TStream elemType ->
        AST.TStream (applySubstToType subst elemType)
    | AST.TDict (keyType, valueType) ->
        AST.TDict (applySubstToType subst keyType, applySubstToType subst valueType)
    | AST.TSum (name, typeArgs) ->
        AST.TSum (name, List.map (applySubstToType subst) typeArgs)
    | AST.TRecord (name, typeArgs) ->
        AST.TRecord (name, List.map (applySubstToType subst) typeArgs)
    | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
    | AST.TInt128
    | AST.TInt
    | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
    | AST.TUInt128
    | AST.TBool | AST.TFloat64 | AST.TString | AST.TBlob | AST.TChar | AST.TDateTime | AST.TUnit | AST.TRuntimeError | AST.TRawPtr ->
        typ  // Concrete types are unchanged

/// Native record layouts have one keyed slot per field. When a declaration
/// repeats a name, the interpreter's head-first lookup makes the first
/// declaration authoritative and the later declarations do not add slots.
let internal firstDeclaredRecordFields
    (fields: (string * AST.Type) list)
    : (string * AST.Type) list =
    fields
    |> List.fold (fun (seen, retainedRev) ((name, _) as field) ->
        if Set.contains name seen then (seen, retainedRev)
        else (Set.add name seen, field :: retainedRev)) (Set.empty, [])
    |> snd
    |> List.rev

let internal buildDeclaredRecordFieldSubst
    (recordInfo: RecordTypeInfo)
    (typeArgs: AST.Type list)
    : Substitution option =
    if List.length recordInfo.TypeParams = List.length typeArgs then
        Some (List.zip recordInfo.TypeParams typeArgs |> Map.ofList)
    else
        None

let internal recordDescriptor
    (reference: CheckedAST.RecordReference)
    (recordInfo: RecordTypeInfo)
    : ANF.RecordDescriptor =
    let fields = recordInfo.Fields
    let concreteFields =
        match buildDeclaredRecordFieldSubst recordInfo reference.TypeArgs with
        | Some subst ->
            fields
            |> List.map (fun (name, fieldType) ->
                (name, applySubstToType subst fieldType))
        | None -> fields
    {
        SourceTypeName = reference.TypeName
        RuntimeTypeName = reference.TypeName
        TypeArgs = reference.TypeArgs
        Fields = concreteFields
    }

/// Match a type pattern (may contain type variables) against a concrete type.
let rec matchTypePattern (pattern: AST.Type) (actual: AST.Type) : Result<(string * AST.Type) list, string> =
    match pattern with
    | AST.TVar name ->
        match actual with
        | AST.TVar actualName when actualName = name -> Ok []
        | _ -> Ok [(name, actual)]
    | _ ->
        match actual with
        | AST.TVar _ ->
            // ANF-side inference may observe unresolved constructor type args.
            // Treat unconstrained actual type variables as compatible placeholders.
            Ok []
        | _ ->
            match pattern with
            | AST.TFunction (patternParams, patternRet) ->
                match actual with
                | AST.TFunction (actualParams, actualRet) when List.length patternParams = List.length actualParams ->
                    let paramResults =
                        List.zip patternParams actualParams
                        |> List.map (fun (p, a) -> matchTypePattern p a)
                    let retResult = matchTypePattern patternRet actualRet
                    (paramResults @ [retResult])
                    |> List.fold (fun acc res ->
                        match acc, res with
                        | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e) (Ok [])
                | _ -> Error "Function type mismatch"
            | AST.TTuple patternElems ->
                match actual with
                | AST.TTuple actualElems when List.length patternElems = List.length actualElems ->
                    List.zip patternElems actualElems
                    |> List.map (fun (p, a) -> matchTypePattern p a)
                    |> List.fold (fun acc res ->
                        match acc, res with
                        | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e) (Ok [])
                | _ -> Error "Tuple type mismatch"
            | AST.TRecord (patternName, patternArgs) ->
                match actual with
                | AST.TRecord (actualName, actualArgs)
                    when patternName = actualName && List.length patternArgs = List.length actualArgs ->
                    List.zip patternArgs actualArgs
                    |> List.map (fun (p, a) -> matchTypePattern p a)
                    |> List.fold (fun acc res ->
                        match acc, res with
                        | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e) (Ok [])
                | _ -> Error "Record type mismatch"
            | AST.TSum (patternName, patternArgs) ->
                match actual with
                | AST.TSum (actualName, actualArgs)
                    when patternName = actualName && List.length patternArgs = List.length actualArgs ->
                    List.zip patternArgs actualArgs
                    |> List.map (fun (p, a) -> matchTypePattern p a)
                    |> List.fold (fun acc res ->
                        match acc, res with
                        | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e) (Ok [])
                | _ ->
                    Error $"Sum type mismatch: expected {typeToString pattern}, got {typeToString actual}"
            | AST.TList patternElem ->
                match actual with
                | AST.TList actualElem -> matchTypePattern patternElem actualElem
                | _ -> Error "List type mismatch"
            | AST.TDict (patternKey, patternValue) ->
                match actual with
                | AST.TDict (actualKey, actualValue) ->
                    match matchTypePattern patternKey actualKey, matchTypePattern patternValue actualValue with
                    | Ok keyBindings, Ok valueBindings -> Ok (keyBindings @ valueBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e
                | _ -> Error "Dict type mismatch"
            | _ ->
                if pattern = actual then Ok [] else Error "Type mismatch"

/// Consolidate type variable bindings, preferring concrete types when both appear.
let consolidateTypeBindings (bindings: (string * AST.Type) list) : Result<Map<string, AST.Type>, string> =
    bindings
    |> List.fold (fun acc (name, typ) ->
        acc |> Result.bind (fun m ->
            match Map.tryFind name m with
            | None -> Ok (Map.add name typ m)
            | Some existingType ->
                if existingType = typ then
                    Ok m
                elif containsTypeVar existingType && not (containsTypeVar typ) then
                    Ok (Map.add name typ m)
                elif containsTypeVar typ && not (containsTypeVar existingType) then
                    Ok m
                elif containsTypeVar existingType && containsTypeVar typ then
                    Ok m
                else
                    Error $"Type variable {name} has conflicting inferences: {typeToString existingType} vs {typeToString typ}"))
        (Ok Map.empty)

/// Apply a substitution to an expression, replacing type variables in type annotations
let rec applySubstToExpr (subst: Substitution) (expr: CheckedAST.Expr) : CheckedAST.Expr =
    match expr with
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _
    | CheckedAST.Local _ | CheckedAST.NamedValue _ | CheckedAST.FuncRef _ | CheckedAST.Closure _ | CheckedAST.RuntimeError _ ->
        expr  // No types to substitute in literals, variables, function references, and closures
    | CheckedAST.BoundaryRender (renderer, value) ->
        CheckedAST.BoundaryRender (renderer, applySubstToExpr subst value)
    | CheckedAST.BinOp (op, left, right) ->
        CheckedAST.BinOp (op, applySubstToExpr subst left, applySubstToExpr subst right)
    | CheckedAST.UnaryOp (op, inner) ->
        CheckedAST.UnaryOp (op, applySubstToExpr subst inner)
    | CheckedAST.Let (pattern, value, body) ->
        CheckedAST.Let (pattern, applySubstToExpr subst value, applySubstToExpr subst body)
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        CheckedAST.RecursiveLet (recursion, applySubstToExpr subst value, applySubstToExpr subst body)
    | CheckedAST.If (cond, thenBranch, elseBranch) ->
        CheckedAST.If (applySubstToExpr subst cond, applySubstToExpr subst thenBranch, applySubstToExpr subst elseBranch)
    | CheckedAST.Sequence (first, next) ->
        CheckedAST.Sequence (applySubstToExpr subst first, applySubstToExpr subst next)
    | CheckedAST.Call (funcName, args) ->
        CheckedAST.Call (funcName, AST.NonEmptyList.map (applySubstToExpr subst) args)
    | CheckedAST.TypeApp (funcName, typeArgs, args) ->
        // Substitute in type arguments and value arguments
        CheckedAST.TypeApp (
            funcName,
            List.map (applySubstToType subst) typeArgs,
            AST.NonEmptyList.map (applySubstToExpr subst) args
        )
    | CheckedAST.TupleLiteral elements ->
        CheckedAST.TupleLiteral (List.map (applySubstToExpr subst) elements)
    | CheckedAST.TupleAccess (tuple, index) ->
        CheckedAST.TupleAccess (applySubstToExpr subst tuple, index)
    | CheckedAST.DictLiteral (keyType, valueType, entries) ->
        CheckedAST.DictLiteral (
            applySubstToType subst keyType,
            applySubstToType subst valueType,
            entries
            |> List.map (fun (key, value) ->
                (applySubstToExpr subst key, applySubstToExpr subst value))
        )
    | CheckedAST.RecordLiteral (reference, fields) ->
        CheckedAST.RecordLiteral (
            { reference with TypeArgs = List.map (applySubstToType subst) reference.TypeArgs },
            List.map (fun (n, e) -> (n, applySubstToExpr subst e)) fields
        )
    | CheckedAST.RecordUpdate (record, updates) ->
        CheckedAST.RecordUpdate (applySubstToExpr subst record, List.map (fun (n, e) -> (n, applySubstToExpr subst e)) updates)
    | CheckedAST.RecordAccess (record, fieldName) ->
        CheckedAST.RecordAccess (applySubstToExpr subst record, fieldName)
    | CheckedAST.Constructor (reference, fields) ->
        CheckedAST.Constructor (reference, List.map (applySubstToExpr subst) fields)
    | CheckedAST.Match (scrutinee, cases) ->
        CheckedAST.Match (applySubstToExpr subst scrutinee,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map (applySubstToExpr subst); Body = applySubstToExpr subst mc.Body }))
    | CheckedAST.ListLiteral elements ->
        CheckedAST.ListLiteral (List.map (applySubstToExpr subst) elements)
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        // Substitute types in parameter annotations and body
        let substParams =
            parameters
            |> AST.NonEmptyList.map (fun parameter ->
                { parameter with Type = applySubstToType subst parameter.Type })
        CheckedAST.Lambda (substParams, returnAnnotation |> Option.map (applySubstToType subst), applySubstToExpr subst body)
    | CheckedAST.Apply (func, args) ->
        CheckedAST.Apply (applySubstToExpr subst func, AST.NonEmptyList.map (applySubstToExpr subst) args)
    | CheckedAST.IndirectApply (func, args) ->
        CheckedAST.IndirectApply (applySubstToExpr subst func, AST.NonEmptyList.map (applySubstToExpr subst) args)
    | CheckedAST.InterpolatedString parts ->
        let substPart part =
            match part with
            | CheckedAST.StringText s -> CheckedAST.StringText s
            | CheckedAST.StringExpr e -> CheckedAST.StringExpr (applySubstToExpr subst e)
        CheckedAST.InterpolatedString (List.map substPart parts)

/// Resolve type aliases to their target types
let rec resolveAliasType (aliasReg: AliasRegistry) (typ: AST.Type) : AST.Type =
    match typ with
    | AST.TRecord (name, []) ->
        match Map.tryFind name aliasReg with
        | Some ([], targetType) -> resolveAliasType aliasReg targetType
        | Some (_, _) -> typ
        | None -> typ
    | AST.TSum (name, []) ->
        match Map.tryFind name aliasReg with
        | Some ([], targetType) -> resolveAliasType aliasReg targetType
        | Some (_, _) -> typ
        | None -> typ
    | AST.TSum (name, args) ->
        match Map.tryFind name aliasReg with
        | Some (typeParams, targetType) ->
            if List.length typeParams <> List.length args then
                typ
            else
                let subst = List.zip typeParams args |> Map.ofList
                let substituted = applySubstToType subst targetType
                resolveAliasType aliasReg substituted
        | None ->
            AST.TSum (name, List.map (resolveAliasType aliasReg) args)
    | AST.TRecord (name, args) ->
        AST.TRecord (name, List.map (resolveAliasType aliasReg) args)
    | AST.TTuple elems ->
        AST.TTuple (List.map (resolveAliasType aliasReg) elems)
    | AST.TList elem ->
        AST.TList (resolveAliasType aliasReg elem)
    | AST.TDict (k, v) ->
        AST.TDict (resolveAliasType aliasReg k, resolveAliasType aliasReg v)
    | AST.TFunction (args, ret) ->
        AST.TFunction (List.map (resolveAliasType aliasReg) args, resolveAliasType aliasReg ret)
    | _ -> typ

let resolveAliasesInTypeRegistry (aliasReg: AliasRegistry) (typeReg: TypeRegistry) : TypeRegistry =
    typeReg
    |> Map.map (fun _ info ->
        { info with
            Fields =
                info.Fields
                |> List.map (fun (fieldName, fieldType) -> (fieldName, resolveAliasType aliasReg fieldType)) })

/// Resolve type aliases within function signatures
let resolveAliasesInFunction (aliasReg: AliasRegistry) (funcDef: CheckedAST.FunctionDef) : CheckedAST.FunctionDef =
    let resolvedParams =
        funcDef.Params
        |> AST.NonEmptyList.map (fun (name, typ) -> (name, resolveAliasType aliasReg typ))
    let resolvedReturnType = resolveAliasType aliasReg funcDef.ReturnType
    { funcDef with Params = resolvedParams; ReturnType = resolvedReturnType }

/// Specialize a generic function definition with specific type arguments
let specializeFunction (funcDef: CheckedAST.FunctionDef) (typeArgs: AST.Type list) : CheckedAST.FunctionDef =
    // Build substitution from type parameters to type args
    let subst =
        if List.length funcDef.TypeParams <> List.length typeArgs then
            Crash.crash
                $"Specialization arity mismatch for {funcDef.Name}: expected {List.length funcDef.TypeParams}, got {List.length typeArgs}"
        else
            List.zip funcDef.TypeParams typeArgs |> Map.ofList
    // Generate specialized name
    let specializedName = specName funcDef.Name typeArgs
    // Apply substitution to parameters, return type, and body
    let specializedParams =
        funcDef.Params
        |> AST.NonEmptyList.map (fun (name, ty) -> (name, applySubstToType subst ty))
    let specializedReturnType = applySubstToType subst funcDef.ReturnType
    let specializedBody = applySubstToExpr subst funcDef.Body
    { Name = specializedName
      TypeParams = []  // Specialized function has no type parameters
      Params = specializedParams
      ReturnType = specializedReturnType
      Body = specializedBody
      Recursion = funcDef.Recursion }

/// Collect all TypeApp call sites from an expression
