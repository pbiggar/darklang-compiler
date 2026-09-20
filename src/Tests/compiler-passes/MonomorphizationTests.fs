// MonomorphizationTests.fs - Unit tests for AST monomorphization
//
// Ensures monomorphization preserves unresolved type variables in specializations.

module MonomorphizationTests

open AST
open CheckedAST
open TypeRegistries
open SpecializationIdentity
open Monomorphization
open ClosureAnalysis
open ClosureComparisons
open LiftExpressions
open LiftFunctions
open PrepareFunctions
open LoweringExpressions
open AST_to_ANF
type TestResult = Result<unit, string>

let testPreservesTypeVarsInSpecialization () : TestResult =
    let xId, symbols = CheckedAST.allocateBinding "x" (CheckedAST.emptySymbols ())
    let id, symbols = CheckedAST.internFunction "id" symbols
    let funcDef : CheckedAST.FunctionDef =
        { Id = id
          Name = "id"
          TypeParams = ["t"]
          Params = NonEmptyList.singleton (xId, TVar "t")
          ReturnType = TVar "t"
          Body = CheckedAST.Local xId
          Recursion = None }

    let program =
        CheckedAST.Program (
            symbols,
            [ CheckedAST.FunctionDef funcDef
              CheckedAST.Expression (
                  CheckedAST.TypeApp (id, [TVar "t"], NonEmptyList.singleton (CheckedAST.Int64Literal 1L))
              ) ]
        )

    let (CheckedAST.Program (_, topLevels)) = monomorphize program
    let funcNames =
        topLevels
        |> List.choose (function
            | CheckedAST.FunctionDef f -> Some f.Name
            | _ -> None)

    if List.contains "id_t" funcNames && not (List.contains "id_i64" funcNames) then
        Ok ()
    else
        Error "Expected monomorphized function id_t without defaulting to id_i64"

let testReplaceTypeAppsWithRegistry () : TestResult =
    let id, symbols = CheckedAST.internFunction "id" (CheckedAST.emptySymbols ())
    let specializedId, symbols = CheckedAST.internFunction "id_i64" symbols
    let expr = TypeApp (id, [TInt64], NonEmptyList.singleton (Int64Literal 1L))
    let registry : SpecRegistry = Map.ofList [ (("id", [TInt64]), "id_i64") ]
    match replaceTypeAppsWithRegistry symbols registry expr with
    | Ok (Call (name, args))
        when name = specializedId
             && NonEmptyList.toList args = [Int64Literal 1L] -> Ok ()
    | Ok result -> Error $"Unexpected replacement result: {result}"
    | Error msg -> Error $"Unexpected error: {msg}"

let testReplaceTypeAppsWithRegistryMissingSpec () : TestResult =
    let id, symbols = CheckedAST.internFunction "id" (CheckedAST.emptySymbols ())
    let expr = TypeApp (id, [TInt64], NonEmptyList.singleton (Int64Literal 1L))
    let registry : SpecRegistry = Map.empty
    match replaceTypeAppsWithRegistry symbols registry expr with
    | Ok _ -> Error "Expected missing specialization error"
    | Error _ -> Ok ()

let testSpecializeFromSpecs () : TestResult =
    let xId, symbols = CheckedAST.allocateBinding "x" (CheckedAST.emptySymbols ())
    let id, symbols = CheckedAST.internFunction "id" symbols
    let funcDef : CheckedAST.FunctionDef =
        { Id = id
          Name = "id"
          TypeParams = ["t"]
          Params = NonEmptyList.singleton (xId, TVar "t")
          ReturnType = TVar "t"
          Body = CheckedAST.Local xId
          Recursion = None }

    let genericDefs : GenericFuncDefs =
        Map.ofList [ ("id", { Symbols = symbols; Function = funcDef }) ]
    let initialSpecs : Set<SpecKey> = Set.ofList [ ("id", [TInt64]) ]
    let result = specializeFromSpecs genericDefs initialSpecs
    let hasFunction =
        result.SpecializedFuncs
        |> List.exists (fun artifact -> artifact.Function.Name = "id_i64")
    let hasRegistry = Map.containsKey ("id", [TInt64]) result.SpecRegistry
    if hasFunction && hasRegistry then
        Ok ()
    else
        Error "Expected specializeFromSpecs to produce id_i64 and registry entry"

let tests = [
    ("Preserve TVar in monomorphization", testPreservesTypeVarsInSpecialization)
    ("Replace TypeApps with registry", testReplaceTypeAppsWithRegistry)
    ("Replace TypeApps with registry missing spec", testReplaceTypeAppsWithRegistryMissingSpec)
    ("Specialize from specs", testSpecializeFromSpecs)
]
