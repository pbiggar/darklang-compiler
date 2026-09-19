// AST_to_ANF.fs - Assemble typed function conversion and declaration registries.

module AST_to_ANF

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open TypeSubstitution
open ClosureAnalysis
open ClosureComparisons
open LiftExpressions
open LiftFunctions
open LoweringTypeInference
open LoweringExpressions

let toANF
    (expr: CheckedAST.Expr)
    (varGen: ANF.VarGen)
    (env: VarEnv)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (funcReg: FunctionRegistry)
    (moduleRegistry: AST.ModuleRegistry)
    : Result<ANF.AExpr * ANF.VarGen, string> =
    toANFCore
        (sumTypeNamesFromVariantLookup variantLookup)
        (DestructionAnalysis.inertFunctionScopes Map.empty)
        expr
        varGen
        env
        typeReg
        variantLookup
        funcReg
        (funcReg |> Map.map (fun _ (name, _) -> name))
        moduleRegistry

/// Convert a function definition to ANF
/// VarGen is passed in and out to maintain globally unique TempIds across functions
/// (needed for TypeMap which maps TempId -> Type across the whole program)
let allocateTypedParams
    (loweredParams: (AST.BindingId * AST.Type) list)
    (varGen: ANF.VarGen)
    : ANF.TypedParam list * ANF.VarGen =
    loweredParams
    |> List.mapFold (fun vg (_, typ) ->
        let (tempId, vg') = ANF.freshVar vg
        ({ ANF.TypedParam.Id = tempId; Type = typ }, vg')) varGen

let private convertFunctionWithSumTypeNames
    (symbols: CheckedAST.Symbols)
    (sumTypeNames: Set<string>)
    (inertScopes: Set<AST.FunctionId>)
    (funcDef: CheckedAST.FunctionDef)
    (varGen: ANF.VarGen)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (funcReg: FunctionRegistry)
    (functionNames: FunctionNameRegistry)
    (moduleRegistry: AST.ModuleRegistry)
    : Result<ANF.Function * ANF.VarGen, string> =
    let loweredParams = paramsToList funcDef.Params |> normalizeSyntheticNullaryParams symbols

    // Allocate TempIds for parameters, bundled with their types
    let (typedParams, varGen1) =
        allocateTypedParams loweredParams varGen

    // Build environment mapping param names to (TempId, Type)
    let paramEnv : VarEnv =
        List.zip loweredParams typedParams
        |> List.map (fun ((name, _), typedParam) -> (name, (typedParam.Id, typedParam.Type)))
        |> Map.ofList

    let unboundLocals =
        ClosureAnalysis.freeVars funcDef.Body (loweredParams |> List.map fst |> Set.ofList)
    let bodyResult =
        if Set.isEmpty unboundLocals then
            toANFCore sumTypeNames inertScopes funcDef.Body varGen1 paramEnv typeReg variantLookup funcReg functionNames moduleRegistry
        else
            let names =
                unboundLocals
                |> Set.toList
                |> List.map (fun id ->
                    CheckedAST.bindingName id symbols |> Option.defaultValue "<unknown-binding>")
                |> String.concat ", "
            Error $"Function '{funcDef.Name}' has unbound checked locals: {names}"
    // Convert body
    bodyResult
    |> Result.map (fun (body, varGen2) ->
        ({ Id = funcDef.Id
           Name = funcDef.Name
           TypedParams = typedParams
           ReturnType = funcDef.ReturnType
           ReturnOwnership = ANF.OwnedReturn
           Body = body }, varGen2))

let convertFunction
    (symbols: CheckedAST.Symbols)
    (funcDef: CheckedAST.FunctionDef)
    (varGen: ANF.VarGen)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (funcReg: FunctionRegistry)
    (moduleRegistry: AST.ModuleRegistry)
    : Result<ANF.Function * ANF.VarGen, string> =
    convertFunctionWithSumTypeNames
        symbols
        (sumTypeNamesFromVariantLookup variantLookup)
        (DestructionAnalysis.inertFunctionScopes Map.empty)
        funcDef
        varGen
        typeReg
        variantLookup
        funcReg
        (funcReg |> Map.map (fun _ (name, _) -> name))
        moduleRegistry

/// Result type that includes registries needed for later passes
type ConversionResult = {
    Program: ANF.Program
    RecursiveMembers: Map<string, AST.LoweredRecursiveMember>
    TypeReg: TypeRegistry
    RecordFieldsReg: Map<string, (string * AST.Type) list>
    RecordTypeParamsReg: Map<string, string list>
    VariantLookup: VariantLookup
    RcSumShapeReg: MemoryModel.RcSumShapeRegistry
    FuncReg: FunctionRegistry
    FuncParams: Map<string, (string * AST.Type) list>  // Function name -> param list with types
    ModuleRegistry: AST.ModuleRegistry
}

/// Result type for user-only ANF conversion (functions not merged with stdlib)
/// Used for compiling user code separately from the prebuilt stdlib
type UserOnlyResult = {
    ScopeContracts: Map<AST.FunctionId, DestructionAnalysis.FunctionScopeContract>
    UserFunctions: ANF.Function list   // Only user functions, not merged with stdlib
    NonInlineableFunctionNames: Set<AST.FunctionId> // Late external specializations compiled in this unit
    MainExpr: ANF.AExpr                // User's main expression
    TypeReg: TypeRegistry              // Merged registries (for lookups)
    RecordFieldsReg: Map<string, (string * AST.Type) list>
    RecordTypeParamsReg: Map<string, string list>
    VariantLookup: VariantLookup
    SumTypeNames: Set<string>
    LocalRecordFieldsReg: Map<string, (string * AST.Type) list>
    LocalVariantLookup: VariantLookup
    RcSumShapeReg: MemoryModel.RcSumShapeRegistry
    FuncReg: FunctionRegistry
    FunctionNames: FunctionNameRegistry
    LocalReturnTypes: Map<string, AST.Type>
    FuncParams: Map<string, (string * AST.Type) list>
    ModuleRegistry: AST.ModuleRegistry
    RecursiveMembers: Map<string, AST.LoweredRecursiveMember>
}

/// Registry bundle used during ANF conversion
type Registries = {
    ScopeContracts: Map<AST.FunctionId, DestructionAnalysis.FunctionScopeContract>
    TypeReg: TypeRegistry
    RecordFieldsReg: Map<string, (string * AST.Type) list>
    RecordTypeParamsReg: Map<string, string list>
    VariantLookup: VariantLookup
    SumTypeNames: Set<string>
    RcSumShapeReg: MemoryModel.RcSumShapeRegistry
    FuncReg: FunctionRegistry
    FunctionNames: FunctionNameRegistry
    FuncParams: Map<string, (string * AST.Type) list>
    ModuleRegistry: AST.ModuleRegistry
    RecursiveMembers: Map<string, AST.LoweredRecursiveMember>
}

/// Retain semantic recursive identities alongside lowered ANF. Native symbol
/// strings remain presentation keys; recursive ownership and group layout are
/// recovered exclusively from this registry.
let loweredRecursiveMemberRegistry
    (functions: CheckedAST.FunctionDef list)
    : Map<string, AST.LoweredRecursiveMember> =
    functions
    |> List.choose (fun func ->
        match func.Recursion with
        | Some typed ->
            Some (
                func.Name,
                ({ Typed = typed; EnvironmentIndex = typed.Resolved.GroupIndex }
                    : AST.LoweredRecursiveMember)
            )
        | _ -> None)
    |> Map.ofList

/// Split program into type defs, function defs, and a single expression
let splitDeclarations (program: CheckedAST.Program) : Result<AST.TypeDef list * CheckedAST.FunctionDef list, string> =
    let (CheckedAST.Program (_, topLevels)) = program
    let expressions = topLevels |> List.filter (function CheckedAST.Expression _ -> true | _ -> false)
    if List.isEmpty expressions then
        Ok (
            topLevels |> List.choose (function CheckedAST.TypeDef (_, definition) -> Some definition | _ -> None),
            topLevels |> List.choose (function CheckedAST.FunctionDef definition -> Some definition | _ -> None)
        )
    else
        Error $"Declaration-only program must not contain entry expressions; found {expressions.Length}"

let splitTopLevels (program: CheckedAST.Program) : Result<AST.TypeDef list * CheckedAST.FunctionDef list * CheckedAST.Expr, string> =
    let (CheckedAST.Program (_, topLevels)) = program
    let typeDefs =
        topLevels
        |> List.choose (function CheckedAST.TypeDef (_, t) -> Some t | _ -> None)
    let functions =
        topLevels
        |> List.choose (function CheckedAST.FunctionDef f -> Some f | _ -> None)
    let expressions =
        topLevels
        |> List.choose (function CheckedAST.Expression e -> Some e | _ -> None)

    let hasMainFunc = functions |> List.exists (fun f -> f.Name = "main")
    let hasStartFunc = functions |> List.exists (fun f -> f.Name = "_start")
    if hasMainFunc then
        Error "Function name 'main' is reserved"
    elif hasStartFunc then
        Error "Function name '_start' is reserved"
    else
        match expressions with
        | [expr] -> Ok (typeDefs, functions, expr)
        | [] -> Error "Program must have a main expression"
        | _ -> Error "Multiple top-level expressions not allowed"

/// Build alias registry from type definitions
let buildAliasRegistry (typeDefs: AST.TypeDef list) : AliasRegistry =
    typeDefs
    |> List.choose (function
        | AST.TypeAlias (name, typeParams, targetType) -> Some (name, (typeParams, targetType))
        | _ -> None)
    |> Map.ofList

/// Resolve type aliases inside function definitions
let resolveAliasesInFunctions (aliasReg: AliasRegistry) (functions: CheckedAST.FunctionDef list) : CheckedAST.FunctionDef list =
    functions |> List.map (resolveAliasesInFunction aliasReg)

let private buildRegistriesInternal
    (symbols: CheckedAST.Symbols)
    (includeModuleFunctionParams: bool)
    (moduleRegistry: AST.ModuleRegistry)
    (typeDefs: AST.TypeDef list)
    (aliasReg: AliasRegistry)
    (functions: CheckedAST.FunctionDef list)
    : Registries =
    let typeRegBase : TypeRegistry =
        typeDefs
        |> List.choose (function
            | AST.RecordDef (name, typeParams, fields) ->
                Some (name, { TypeParams = typeParams; Fields = firstDeclaredRecordFields fields })
            | _ -> None)
        |> Map.ofList

    let rawVariantLookup : VariantLookup =
        let collidingCaseNames = AST.collidingConstructorCaseNames typeDefs
        typeDefs
        |> List.choose (function
            | AST.SumTypeDef (typeName, typeParams, variants) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.fold (fun lookup (typeName, typeParams, variants) ->
            variants
            |> List.indexed
            |> List.fold (fun typeLookup (ordinal, variant) ->
                let tag =
                    if Set.contains variant.Name collidingCaseNames then
                        AST.constructorRuntimeIdentity typeName variant.Name
                    else
                        ordinal
                let info = (typeName, typeParams, tag, variant.Fields)
                let withBare =
                    if Map.containsKey variant.Name typeLookup then typeLookup
                    else Map.add variant.Name info typeLookup
                Map.add $"{typeName}.{variant.Name}" info withBare) lookup) Map.empty

    let sumTypeNames = sumTypeNamesFromVariantLookup rawVariantLookup

    let variantLookup : VariantLookup =
        rawVariantLookup
        |> Map.map (fun _ (typeName, typeParams, tag, fieldTypes) ->
            (typeName,
             typeParams,
             tag,
             fieldTypes
             |> List.map (canonicalizeBareSumTypeRefsWithNames sumTypeNames)))

    let recordNames = typeRegBase |> Map.keys |> Set.ofSeq
    let typeReg =
        typeRegBase
        |> resolveAliasesInTypeRegistry aliasReg
        |> fun reg -> expandTypeRegWithAliases reg aliasReg
        |> Map.map (fun _ info ->
            { info with
                Fields =
                    info.Fields
                    |> List.map (fun (fieldName, fieldType) ->
                        (fieldName,
                         canonicalizeNamedTypeRefs
                            recordNames
                            sumTypeNames
                            fieldType)) })

    let funcReg : FunctionRegistry =
        functions
        |> List.map (fun f ->
            let paramTypes = f.Params |> paramsToList |> normalizeSyntheticNullaryParams symbols |> List.map snd
            let funcType = AST.TFunction (paramTypes, f.ReturnType)
            (f.Id, (f.Name, funcType)))
        |> Map.ofList

    let functionNames : FunctionNameRegistry =
        functions
        |> List.fold (fun names func -> Map.add func.Id func.Name names) (CheckedAST.functionNames symbols)

    let userFuncParams : Map<string, (string * AST.Type) list> =
        functions
        |> List.map (fun f ->
            let parameters =
                paramsToList f.Params
                |> List.mapi (fun index (id, typ) ->
                    match CheckedAST.bindingName id symbols with
                    | Some name -> (name, typ)
                    | None -> ($"arg{index}", typ))
            (f.Name, parameters))
        |> Map.ofList

    let moduleFuncParams : Map<string, (string * AST.Type) list> =
        if includeModuleFunctionParams then
            moduleRegistry
            |> Map.map (fun _ moduleFunc ->
                moduleFunc.ParamTypes |> List.mapi (fun i typ -> ($"arg{i}", typ)))
        else
            Map.empty

    let funcParams =
        Map.fold (fun acc k v -> Map.add k v acc) userFuncParams moduleFuncParams

    {
        TypeReg = typeReg
        ScopeContracts =
            ExtractListRegions.scopeContracts
                (fun types expr -> inferTypeCore sumTypeNames expr types typeReg variantLookup funcReg functionNames moduleRegistry)
                functions
        RecordFieldsReg = recordFieldsRegistry typeReg
        RecordTypeParamsReg = recordTypeParamsRegistry typeReg
        VariantLookup = variantLookup
        SumTypeNames = sumTypeNames
        RcSumShapeReg = rcSumShapeRegistryFromVariantLookup variantLookup
        FuncReg = funcReg
        FunctionNames = functionNames
        FuncParams = funcParams
        ModuleRegistry = moduleRegistry
        RecursiveMembers = loweredRecursiveMemberRegistry functions
    }

/// Build standalone registries from type and function definitions.
let buildRegistries
    (symbols: CheckedAST.Symbols)
    (moduleRegistry: AST.ModuleRegistry)
    (typeDefs: AST.TypeDef list)
    (aliasReg: AliasRegistry)
    (functions: CheckedAST.FunctionDef list)
    : Registries =
    buildRegistriesInternal symbols true moduleRegistry typeDefs aliasReg functions

/// Build only the declaration overlay for a context that already contains the
/// module function parameters. Reconstructing that constant projection for
/// every separately compiled user unit is both redundant and expensive.
let buildOverlayRegistries
    (symbols: CheckedAST.Symbols)
    (moduleRegistry: AST.ModuleRegistry)
    (typeDefs: AST.TypeDef list)
    (aliasReg: AliasRegistry)
    (functions: CheckedAST.FunctionDef list)
    : Registries =
    buildRegistriesInternal symbols false moduleRegistry typeDefs aliasReg functions

/// Merge registries with overlay taking precedence (module registry stays from base)
let mergeRegistries (baseRegs: Registries) (overlay: Registries) : Registries =
    let mergeMaps m1 m2 = Map.fold (fun acc k v -> Map.add k v acc) m1 m2
    {
        TypeReg = mergeMaps baseRegs.TypeReg overlay.TypeReg
        ScopeContracts = mergeMaps baseRegs.ScopeContracts overlay.ScopeContracts
        RecordFieldsReg = mergeMaps baseRegs.RecordFieldsReg overlay.RecordFieldsReg
        RecordTypeParamsReg = mergeMaps baseRegs.RecordTypeParamsReg overlay.RecordTypeParamsReg
        VariantLookup = mergeMaps baseRegs.VariantLookup overlay.VariantLookup
        SumTypeNames = Set.union baseRegs.SumTypeNames overlay.SumTypeNames
        RcSumShapeReg = mergeMaps baseRegs.RcSumShapeReg overlay.RcSumShapeReg
        FuncReg = mergeMaps baseRegs.FuncReg overlay.FuncReg
        FunctionNames = mergeMaps baseRegs.FunctionNames overlay.FunctionNames
        FuncParams = mergeMaps baseRegs.FuncParams overlay.FuncParams
        ModuleRegistry = baseRegs.ModuleRegistry
        RecursiveMembers = mergeMaps baseRegs.RecursiveMembers overlay.RecursiveMembers
    }

/// Convert functions to ANF, returning updated VarGen
let convertFunctions
    (symbols: CheckedAST.Symbols)
    (registries: Registries)
    (varGen: ANF.VarGen)
    (functions: CheckedAST.FunctionDef list)
    : Result<ANF.Function list * ANF.VarGen, string> =
    let sumTypeNames = registries.SumTypeNames
    let inertScopes = DestructionAnalysis.inertFunctionScopes registries.ScopeContracts
    let rec loop funcs vg acc =
        match funcs with
        | [] -> Ok (List.rev acc, vg)
        | func :: rest ->
            convertFunctionWithSumTypeNames
                symbols
                sumTypeNames
                inertScopes
                func
                vg
                registries.TypeReg
                registries.VariantLookup
                registries.FuncReg
                registries.FunctionNames
                registries.ModuleRegistry
            |> Result.bind (fun (anfFunc, vg') ->
                loop rest vg' (anfFunc :: acc))
    let ownershipContext : AnalyzeFunctionOwnership.Context = {
        TypeReg = registries.TypeReg
        RecordFieldsReg = registries.RecordFieldsReg
        RecordTypeParamsReg = registries.RecordTypeParamsReg
        VariantLookup = registries.VariantLookup
        SumTypeNames = registries.SumTypeNames
        RcSumShapeReg = registries.RcSumShapeReg
        FuncReg = registries.FuncReg
        FunctionNames = registries.FunctionNames
        ModuleRegistry = registries.ModuleRegistry
    }
    AnalyzeFunctionOwnership.analyze ownershipContext functions
    |> Result.mapError (fun error -> $"Whole-function ownership analysis failed: {error}")
    |> Result.bind (fun _ -> loop functions varGen [])

/// Convert an expression to ANF with the given VarGen
let convertExprToAnf
    (registries: Registries)
    (varGen: ANF.VarGen)
    (expr: CheckedAST.Expr)
    : Result<ANF.AExpr * ANF.VarGen, string> =
    let emptyEnv : VarEnv = Map.empty
    let sumTypeNames = registries.SumTypeNames
    toANFCore sumTypeNames (DestructionAnalysis.inertFunctionScopes registries.ScopeContracts) expr varGen emptyEnv registries.TypeReg registries.VariantLookup registries.FuncReg registries.FunctionNames registries.ModuleRegistry

/// Synthesize an entrypoint function from a main expression
let synthesizeEntryFunction (name: string) (returnType: AST.Type) (body: ANF.AExpr) : ANF.Function =
    { Id = AST.functionIdForName name
      Name = name
      TypedParams = []
      ReturnType = returnType
      ReturnOwnership = ANF.OwnedReturn
      Body = body }
