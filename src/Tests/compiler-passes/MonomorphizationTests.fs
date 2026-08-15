// MonomorphizationTests.fs - Unit tests for AST monomorphization
//
// Ensures monomorphization preserves unresolved type variables in specializations.

module MonomorphizationTests

open AST
open AST_to_ANF

type TestResult = Result<unit, string>

let testPreservesTypeVarsInSpecialization () : TestResult =
    let funcDef : FunctionDef =
        { Name = "id"
          TypeParams = ["t"]
          Params = NonEmptyList.singleton ("x", TVar "t")
          ReturnType = TVar "t"
          Body = Var "x"
          Recursion = None }

    let program =
        Program [
            FunctionDef funcDef
            Expression (TypeApp ("id", [TVar "t"], NonEmptyList.singleton (Int64Literal 1L)))
        ]

    let (Program topLevels) = monomorphize program
    let funcNames =
        topLevels
        |> List.choose (function
            | FunctionDef f -> Some f.Name
            | _ -> None)

    if List.contains "id_t" funcNames && not (List.contains "id_i64" funcNames) then
        Ok ()
    else
        Error "Expected monomorphized function id_t without defaulting to id_i64"

let testReplaceTypeAppsWithRegistry () : TestResult =
    let expr = TypeApp ("id", [TInt64], NonEmptyList.singleton (Int64Literal 1L))
    let registry : SpecRegistry = Map.ofList [ (("id", [TInt64]), "id_i64") ]
    match replaceTypeAppsWithRegistry registry expr with
    | Ok (Call (name, args))
        when name = "id_i64"
             && NonEmptyList.toList args = [Int64Literal 1L] -> Ok ()
    | Ok result -> Error $"Unexpected replacement result: {result}"
    | Error msg -> Error $"Unexpected error: {msg}"

let testReplaceTypeAppsWithRegistryMissingSpec () : TestResult =
    let expr = TypeApp ("id", [TInt64], NonEmptyList.singleton (Int64Literal 1L))
    let registry : SpecRegistry = Map.empty
    match replaceTypeAppsWithRegistry registry expr with
    | Ok _ -> Error "Expected missing specialization error"
    | Error _ -> Ok ()

let testSpecializeFromSpecs () : TestResult =
    let funcDef : FunctionDef =
        { Name = "id"
          TypeParams = ["t"]
          Params = NonEmptyList.singleton ("x", TVar "t")
          ReturnType = TVar "t"
          Body = Var "x"
          Recursion = None }

    let genericDefs : GenericFuncDefs = Map.ofList [ ("id", funcDef) ]
    let initialSpecs : Set<SpecKey> = Set.ofList [ ("id", [TInt64]) ]
    let result = specializeFromSpecs genericDefs initialSpecs
    let hasFunction =
        result.SpecializedFuncs
        |> List.exists (fun f -> f.Name = "id_i64")
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
