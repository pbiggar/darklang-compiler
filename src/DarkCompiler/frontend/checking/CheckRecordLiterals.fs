// CheckRecordLiterals.fs - Check RecordLiteral expressions while preserving source diagnostics and order.

module CheckRecordLiterals

open AST
open CheckingDiagnostics
open CheckingTypes
open TypeUnification
open CheckExpressionSupport

let internal check (checkExpr: ExpressionChecker) (env: TypeEnv) (typeReg: IndexedTypeRegistry) (variantLookup: VariantLookup) (genericFuncReg: GenericFuncRegistry) (warningSettings: WarningSettings) (moduleRegistry: ModuleRegistry) (aliasReg: AliasRegistry) (expectedType: SemanticType option) (reference: RecordReference) (fields: (RecordFieldReference * Expr) list) : Result<SemanticType * Expr, TypeError> =
    let typeName = reference.SourceTypeName
    // Type name is required (parser enforces this, but check for safety)
    if typeName = "" then
        Error (GenericError "Record literal requires type name: use 'TypeName { field = value, ... }'")
    else
        let normalizedFields =
            fields
            |> List.map (fun (reference, value) ->
                (if reference.SourceFieldName = "___" then "" else reference.SourceFieldName), value)
        let invalidEmptyField = normalizedFields |> List.tryFind (fst >> (=) "")
        let duplicateField =
            normalizedFields
            |> List.map fst
            |> List.countBy id
            |> List.tryPick (fun (name, count) -> if count > 1 then Some name else None)
        match invalidEmptyField, duplicateField with
        | Some _, _ -> Error (GenericError "Empty key in record creation")
        | _, Some duplicate -> Error (GenericError $"Duplicate field `{duplicate}`")
        | None, None ->
        match tryResolveRecordLiteralInfo aliasReg typeReg reference with
        | None ->
            Error (GenericError $"Unknown record type: {typeName}")
        | Some (resolvedTypeName, aliasTypeArgs, recordInfo) ->
            let explicitArityError =
                if List.isEmpty reference.TypeArgs then None
                else
                    let sourceArity =
                        match Map.tryFind reference.SourceTypeName aliasReg with
                        | Some (parameters, _) -> List.length parameters
                        | None -> List.length recordInfo.TypeParams
                    if sourceArity = List.length reference.TypeArgs then None
                    else Some $"Record type argument arity mismatch: expected {sourceArity}, got {List.length reference.TypeArgs}"
            match explicitArityError with
            | Some message -> Error (GenericError message)
            | None ->
            let initialTypeArgs =
                if List.length aliasTypeArgs = List.length recordInfo.TypeParams then aliasTypeArgs
                else []
            let initialSubstitution =
                match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams initialTypeArgs with
                | Ok substitution -> substitution
                | Error _ -> Map.empty
            let expectedFields =
                recordInfo.Fields
                |> List.map (fun (fieldName, fieldType) ->
                    (fieldName,
                     fieldType
                     |> applyTypeArguments initialSubstitution))

            // Check that all fields are present and have correct types
            let fieldMap = Map.ofList normalizedFields

            // Check for missing fields
            let missingFields =
                expectedFields
                |> List.filter (fun (fname, _) -> not (Map.containsKey fname fieldMap))
                |> List.map fst

            if not (List.isEmpty missingFields) then
                let missingStr = String.concat ", " missingFields
                Error (GenericError $"Missing fields in record literal: {missingStr}")
            else
                // Check for extra fields
                let expectedFieldNames = expectedFields |> List.map fst |> Set.ofList
                let extraFields =
                    normalizedFields
                    |> List.filter (fun (fname, _) -> not (Set.contains fname expectedFieldNames))
                    |> List.map fst

                if not (List.isEmpty extraFields) then
                    let extraStr = String.concat ", " extraFields
                    Error (GenericError $"Unknown fields in record literal: {extraStr}")
                else
                    // Type check each field in source order. Record layout order is
                    // applied during lowering, after every initializer has run.
                    let expectedFieldTypes =
                        expectedFields
                        |> List.fold (fun lookup (fieldName, fieldType) ->
                            if Map.containsKey fieldName lookup then lookup
                            else Map.add fieldName fieldType lookup) Map.empty

                    let rec checkFieldsInOrder
                        (remaining: (string * Expr) list)
                        (accFields: (RecordFieldReference * Expr) list)
                        (accBindings: (string * SemanticType) list)
                        : Result<(RecordFieldReference * Expr) list * (string * SemanticType) list, TypeError> =
                        match remaining with
                        | [] -> Ok (List.rev accFields, accBindings)
                        | (fname, fieldExpr) :: rest ->
                            match Map.tryFind fname expectedFieldTypes with
                            | Some expectedFieldType ->
                                match
                                    checkExpr
                                        fieldExpr
                                        env
                                        typeReg
                                        variantLookup
                                        genericFuncReg
                                        warningSettings
                                        moduleRegistry
                                        aliasReg
                                        (Some expectedFieldType)
                                with
                                | Error (TypeMismatch (_, actualType, _)) ->
                                    Error
                                        (GenericError
                                            (formatLegacyRecordFieldTypeError
                                                aliasReg
                                                fname
                                                expectedFieldType
                                                actualType
                                                fieldExpr))
                                | Error err ->
                                    Error err
                                | Ok (actualType, fieldExpr') ->
                                    let resolvedExpectedFieldType = resolveType aliasReg expectedFieldType
                                    let resolvedActualType = resolveType aliasReg actualType
                                    match matchTypes resolvedExpectedFieldType resolvedActualType with
                                    | Ok newBindings ->
                                        let fieldIndex =
                                            recordInfo.Fields
                                            |> List.tryFindIndex (fst >> (=) fname)
                                            |> Option.defaultWith (fun () ->
                                                Crash.crash $"Validated record field '{fname}' has no declaration slot")
                                        let fieldReference =
                                            resolvedRecordFieldReference resolvedTypeName fname fieldIndex
                                        checkFieldsInOrder
                                            rest
                                            ((fieldReference, fieldExpr') :: accFields)
                                            (accBindings @ newBindings)
                                    | Error _ ->
                                        Error
                                            (GenericError
                                                (formatLegacyRecordFieldTypeError
                                                    aliasReg
                                                    fname
                                                    expectedFieldType
                                                    actualType
                                                    fieldExpr))
                            | None ->
                                Crash.crash $"Record field '{fname}' disappeared after validation"

                    checkFieldsInOrder normalizedFields [] []
                    |> Result.bind (fun (fields', rawBindings) ->
                        match consolidateBindings rawBindings with
                        | Error msg ->
                            Error (GenericError $"Incompatible generic record field types: {msg}")
                        | Ok subst ->
                            let inferredTypeArgs =
                                match initialTypeArgs with
                                | args when List.length args = List.length recordInfo.TypeParams ->
                                    args |> List.map (applySubst subst)
                                | _ ->
                                    recordInfo.TypeParams
                                    |> List.map (fun name -> Map.tryFind name subst |> Option.defaultValue (TVar name))
                            let inferredTypeArgs =
                                match expectedType with
                                | Some expected ->
                                    match resolveType aliasReg expected with
                                    | TRecord (expectedName, expectedArgs)
                                        when resolveTypeName aliasReg expectedName = resolvedTypeName
                                             && List.length expectedArgs = List.length recordInfo.TypeParams ->
                                        expectedArgs
                                    | _ -> inferredTypeArgs
                                | None -> inferredTypeArgs
                            let inferredRecordType = TRecord (resolvedTypeName, inferredTypeArgs)
                            let resolvedReference = {
                                SourceTypeName = reference.SourceTypeName
                                ResolvedTypeName = resolvedTypeName
                                TypeArgs = inferredTypeArgs
                            }

                            match expectedType with
                            | Some expected ->
                                if typesCompatibleWithAliases aliasReg expected inferredRecordType then
                                    Ok (inferredRecordType, RecordLiteral (resolvedReference, fields'))
                                else
                                    Error (TypeMismatch (expected, inferredRecordType, "record literal"))
                            | None ->
                                Ok (inferredRecordType, RecordLiteral (resolvedReference, fields')))
