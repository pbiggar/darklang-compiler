// OwnedFunctionGroupTests.fs - Deterministic owned-HIR call-group laws.

module OwnedFunctionGroupTests

open OwnedIR

type private TestLeaf = TestLeaf

let private unitValue : HIR.Value = { Id = HIR.ValueId 0; Type = AST.TUnit }

let private condition : HIR.Operand = {
    Expression = CheckedAST.BoolLiteral true
    Type = AST.TBool
    Inputs = Map.empty
}

let private block operations : Block<TestLeaf, string> = {
    Body = {
        Parameters = []
        Operations = operations
        Result = unitValue
    }
}

let private call target =
    Evaluate (HIR.Call {
        Target = TestIds.functionIdForName target
        Arguments = [unitValue]
        Result = unitValue
    })

let private branch ifTrue ifFalse =
    Evaluate (HIR.Branch (unitValue, condition, block ifTrue, block ifFalse))

let private definition name operations : Function<TestLeaf, string> = {
    Definition = {
        Id = TestIds.functionIdForName name
        Name = name
        Body = {
            Body = {
                Parameters = [{ Name = "unit"; Binding = AST.bindingId 0; Value = unitValue }]
                Operations = operations
                Result = unitValue
            }
        }
    }
    Ownership = {
        Parameters = [UnmanagedParameter]
        Result = UnmanagedResult
    }
}

let private summary group =
    let names =
        group
        |> OwnedFunctionGroups.functions
        |> List.map (fun definition -> definition.Definition.Name)
    names,
    OwnedFunctionGroups.isRecursive group,
    OwnedFunctionGroups.internalDependencies group,
    OwnedFunctionGroups.externalTargets group

let private testDiscoversCalleeFirstRecursiveGroups () =
    let entry = definition "entry" [branch [call "mutualA"] [call "external"]]
    let mutualA = definition "mutualA" [call "mutualB"]
    let leaf =
        let opaqueCall : HIR.Operand = {
            Expression = CheckedAST.Call (
                TestIds.functionIdForName "opaque",
                AST.NonEmptyList.singleton CheckedAST.UnitLiteral)
            Type = AST.TUnit
            Inputs = Map.empty
        }
        definition "leaf" [Evaluate (HIR.ScalarBinding (unitValue, opaqueCall))]
    let self = definition "self" [call "self"]
    let mutualB = definition "mutualB" [call "mutualA"; call "leaf"]
    let actual =
        OwnedFunctionGroups.discover [entry; mutualA; leaf; self; mutualB]
        |> Result.map (List.map summary)
    let expected = Ok [
        (["leaf"], false, Set.empty, Set.empty)
        (["mutualA"; "mutualB"], true, Set.singleton (TestIds.functionIdForName "leaf"), Set.empty)
        (["entry"], false, Set.singleton (TestIds.functionIdForName "mutualA"), Set.singleton (TestIds.functionIdForName "external"))
        (["self"], true, Set.empty, Set.empty)
    ]
    if actual = expected then Ok ()
    else Error $"Expected stable callee-first owned function groups {expected}, got {actual}"

let private testRejectsDuplicateFunctionNames () =
    let duplicate = definition "duplicate" []
    let actual = OwnedFunctionGroups.discover [duplicate; duplicate]
    let expected =
        Error (OwnedFunctionGroups.DuplicateFunctionName (TestIds.functionIdForName "duplicate"))
    if actual = expected then Ok ()
    else Error $"Expected duplicate owned functions to fail grouping, got {actual}"

let private testAcceptsEmptyProgram () =
    match OwnedFunctionGroups.discover [] with
    | Ok [] -> Ok ()
    | actual -> Error $"Expected an empty program to contain no call groups, got {actual}"

let private testDiscoversDeepAcyclicGraphCalleeFirst () =
    let functionCount = 256
    let name index = sprintf "chain%04i" index
    let definitions =
        [0 .. functionCount - 1]
        |> List.map (fun index ->
            let operations =
                if index + 1 < functionCount then [call (name (index + 1))]
                else []
            definition (name index) operations)
    let actual =
        OwnedFunctionGroups.discover definitions
        |> Result.map (fun groups ->
            groups
            |> List.map (fun group ->
                group
                |> OwnedFunctionGroups.functions
                |> List.map (fun memberDefinition -> memberDefinition.Definition.Name)))
    let expected =
        [functionCount - 1 .. -1 .. 0]
        |> List.map (fun index -> [name index])
        |> Ok
    if actual = expected then Ok ()
    else Error "Expected every deep acyclic owned-HIR group in callee-first order"

let tests = [
    "Owned HIR discovers stable callee-first recursive groups", testDiscoversCalleeFirstRecursiveGroups
    "Owned HIR call grouping rejects duplicate function names", testRejectsDuplicateFunctionNames
    "Owned HIR call grouping accepts an empty program", testAcceptsEmptyProgram
    "Owned HIR discovers a deep acyclic graph in callee-first order", testDiscoversDeepAcyclicGraphCalleeFirst
]
