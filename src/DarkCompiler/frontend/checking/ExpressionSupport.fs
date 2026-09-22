// ExpressionSupport.fs - Typed recursive checking interface and call-argument diagnostics.

module CheckExpressionSupport

open AST
open CheckingDiagnostics
open CheckingTypes
open TypeUnification

type internal ExpressionChecker = Expr -> TypeEnv -> IndexedTypeRegistry -> VariantLookup -> GenericFuncRegistry -> WarningSettings -> ModuleRegistry -> AliasRegistry -> SemanticType option -> Result<SemanticType * Expr, TypeError>

let internal paramNameForLegacyError
    (funcParamNameReg: Map<string, string list>)
    (funcName: string)
    (paramIndex: int)
    : string =
    let zeroBasedParamIndex = paramIndex - 1

    let rec tryGetAtIndex (index: int) (remaining: string list) : string option =
        match remaining with
        | [] -> None
        | item :: rest ->
            if index = 0 then Some item else tryGetAtIndex (index - 1) rest

    let resolvedParamName =
        if zeroBasedParamIndex < 0 then
            None
        else
            tryLookupResolved funcName funcParamNameReg
            |> Option.bind (fun (paramNames, _resolvedName) -> tryGetAtIndex zeroBasedParamIndex paramNames)

    match resolvedParamName with
    | Some paramName -> paramName
    | None -> $"arg{paramIndex}"
