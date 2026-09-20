// ListHIRTests.fs - Region eligibility, ownership accounting, and storage budgets.

module ListHIRTests

let private call name args =
    CheckedAST.Call (TestIds.functionIdForName name, AST.NonEmptyList.fromList args)
let private binding name =
    name |> Seq.fold (fun hash ch -> (hash * 31) + int ch) 17 |> AST.bindingId
let private local name = CheckedAST.Local (binding name)
let private bind name value body = CheckedAST.Let (CheckedAST.LPVariable (binding name), value, body)
let private values count = CheckedAST.ListLiteral ([1 .. count] |> List.map (int64 >> CheckedAST.Int64Literal))
let private map input =
    call
        "Darklang.Stdlib.List.map_i64_i64"
        [input; CheckedAST.Closure (TestIds.functionIdForName "mapCallback", [])]
let private reverse input = call "Darklang.Stdlib.List.reverse_i64" [input]
let private ownershipBoundary input =
    call "Darklang.Stdlib.List.__arrayOwnershipBoundary_i64" [input]
let private fold input =
    call
        "Darklang.Stdlib.List.fold_i64_i64"
        [ input
          CheckedAST.Int64Literal 0L
          CheckedAST.Closure (TestIds.functionIdForName "foldCallback", []) ]
let private repeat = call "Darklang.Stdlib.List.repeatUnsafe_i64" [CheckedAST.BigIntLiteral 3I; CheckedAST.Int64Literal 7L]
let private bytes constant : ListRegion.AllocationBytes = { ConstantBytes = constant; RuntimeBuffers = Map.empty }
let private runtimeBytes constant terms : ListRegion.AllocationBytes = { ConstantBytes = constant; RuntimeBuffers = Map.ofList terms }

let private functions : TypeRegistries.FunctionRegistry =
    [
        "mapCallback", AST.TFunction ([AST.TRawPtr; AST.TInt64], AST.TInt64)
        "foldCallback", AST.TFunction ([AST.TRawPtr; AST.TInt64; AST.TInt64], AST.TInt64)
        "Darklang.Stdlib.List.map_i64_i64", AST.TFunction ([AST.TList AST.TInt64; AST.TFunction ([AST.TInt64], AST.TInt64)], AST.TList AST.TInt64)
        "Darklang.Stdlib.List.reverse_i64", AST.TFunction ([AST.TList AST.TInt64], AST.TList AST.TInt64)
        "Darklang.Stdlib.List.fold_i64_i64", AST.TFunction ([AST.TList AST.TInt64; AST.TInt64; AST.TFunction ([AST.TInt64; AST.TInt64], AST.TInt64)], AST.TInt64)
        "Darklang.Stdlib.List.__arrayOwnershipBoundary_i64", AST.TFunction ([AST.TList AST.TInt64], AST.TList AST.TInt64)
        "Darklang.Stdlib.List.repeatUnsafe_i64", AST.TFunction ([AST.TInt; AST.TInt64], AST.TList AST.TInt64)
    ]
    |> List.map (fun (name, typ) -> TestIds.functionIdForName name, (name, typ))
    |> Map.ofList

let private extractWithParameters parameterTypes expression =
    let functionNames = functions |> Map.map (fun _ (name, _) -> name)
    let infer types expr =
        LoweringTypeInference.inferTypeCore
            Set.empty TypeRegistries.emptyTypeNames expr types Map.empty Map.empty functions functionNames Map.empty
    ExtractListRegions.tryExtract
        (Set.ofList [
            TestIds.functionIdForName "mapCallback"
            TestIds.functionIdForName "foldCallback"
        ])
        functionNames
        parameterTypes
        infer
        (fun expr -> ClosureAnalysis.freeVars expr Set.empty)
        expression

let private extract expression = extractWithParameters Map.empty expression

let private checkBudget expression expected () =
    match extract expression with
    | None -> Error "Expected a closed List<Int64> region"
    | Some region ->
        ListLiveness.verifyFunctional region |> Result.bind (fun () ->
            let owned = region |> SelectListStorage.selectStorage |> ElaborateListOwnership.elaborateOwnership
            VerifyListOwnership.verify owned |> Result.bind (fun () ->
                let actual = ListAllocationBudget.allocationBudget owned
                if actual = expected then Ok () else Error $"Storage budget: expected {expected}, got {actual}"))

let private checkSummary expression expected = checkBudget expression (ListRegion.Complete expected)

let private unique = bind "xs" (values 3) (bind "ys" (map (local "xs")) (fold (reverse (local "ys"))))
let private shared =
    bind "xs" (values 3)
        (bind "ys" (map (local "xs"))
            (bind "old" (fold (local "xs")) (fold (local "ys"))))

let private runtimeUnique = fold (map (ownershipBoundary (values 3)))
let private runtimeShared =
    bind "xs" (values 3)
        (bind "alias" (local "xs")
            (bind "ys" (map (ownershipBoundary (local "xs")))
                (bind "old" (fold (local "alias")) (fold (local "ys")))))

let private rejects expression () =
    match extract expression with
    | None -> Ok ()
    | Some _ -> Error "An unsupported or escaping list entered array storage"

let private testLoweredBudget () =
    AST_to_ANF.toANF unique ANF.initialVarGen Map.empty Map.empty Map.empty functions Map.empty
    |> Result.bind (fun (body, _) ->
        let rec counts expression =
            match expression with
            | ANF.Jump _ | ANF.Return _ -> 0, 0
            | ANF.Join (_, yes, no)
            | ANF.If (_, yes, no) -> let a, b = counts yes in let c, d = counts no in a + c, b + d
            | ANF.Let (_, operation, tail) ->
                let allocations, releases = counts tail
                match operation with
                | ANF.RawAlloc _ -> allocations + 1, releases
                | ANF.RefCountDec _ -> allocations, releases + 1
                | _ -> allocations, releases
        if counts body = (1, 1) then Ok ()
        else Error $"Expected one native array allocation and release, got {counts body}")

let private rejectsOwnership operations () =
    let result: HIR.Value = { Id = HIR.ValueId 1000; Type = AST.TInt64 }
    let block : ListRegion.OwnedBlock =
        { Body = { Parameters = []; Operations = List.concat operations; Result = result } }
    match VerifyListOwnership.verifyBlockOwnership block with
    | Error _ -> Ok ()
    | Ok () -> Error "Ownership verifier accepted an invalid lifetime"

let private root = HIR.ValueId 0
let private rootValue: HIR.Value = { Id = root; Type = AST.TList AST.TInt64 }
let private drops values = values |> List.map OwnedIR.Drop
let private construct releases : ListRegion.OwnedOperation list =
    OwnedIR.Evaluate (HIR.Leaf (ListRegion.Construct (rootValue, ListRegion.Literal []))) :: drops releases

let private zero : ListRegion.AllocationSummary = { Allocations = 0; AllocatedBytes = bytes 0L; Copies = 0; ReusedTransforms = 0; Releases = 0 }
let private allocated = { zero with Allocations = 1; AllocatedBytes = bytes 56L }
let private choice yes no = CheckedAST.If (CheckedAST.BoolLiteral true, yes, no)
let private branchUses = bind "xs" (values 3) (choice (fold (map (local "xs"))) (fold (reverse (local "xs"))))
let private branchJoin = bind "xs" (values 3) (bind "selected" (choice (fold (reverse (local "xs"))) (CheckedAST.Int64Literal 7L)) (fold (local "xs")))
let private manyBranches count =
    bind "xs" (values 3)
        (List.foldBack (fun index body -> bind $"branch{index}" (choice (fold (local "xs")) (CheckedAST.Int64Literal 7L)) body) [1 .. count] (fold (local "xs")))
let private ownedBlock releases operations : ListRegion.OwnedBlock =
    let result: HIR.Value = { Id = HIR.ValueId 1001; Type = AST.TInt64 }
    { Body = { Parameters = []; Operations = drops releases @ List.concat operations; Result = result } }
let private ownedBranch yes no : ListRegion.OwnedOperation list =
    let result: HIR.Value = { Id = HIR.ValueId 1002; Type = AST.TInt64 }
    let condition: HIR.Operand = { Expression = CheckedAST.BoolLiteral true; Type = AST.TBool; Inputs = Map.empty }
    [OwnedIR.Evaluate (HIR.Branch (result, condition, yes, no))]
let private transform releases : ListRegion.OwnedOperation list =
    OwnedIR.Evaluate (HIR.Leaf (ListRegion.Transform ({ Id = HIR.ValueId 1; Type = AST.TList AST.TInt64 }, rootValue, (ListRegion.Reverse, ListRegion.Consume))))
    :: drops releases

let private runtimeSharedBlock () : ListRegion.OwnedBlock =
    let output: HIR.Value = { Id = HIR.ValueId 1; Type = AST.TList AST.TInt64 }
    let result: HIR.Value = { Id = HIR.ValueId 2; Type = AST.TUnit }
    let element value: HIR.Operand = {
        Expression = CheckedAST.Int64Literal value
        Type = AST.TInt64
        Inputs = Map.empty
    }
    let operations: ListRegion.OwnedOperation list = [
        OwnedIR.Evaluate (HIR.Leaf (ListRegion.Construct (rootValue, ListRegion.Literal [element 1L; element 2L; element 3L])))
        OwnedIR.Dup root
        OwnedIR.Evaluate (HIR.Leaf (ListRegion.Transform (output, rootValue, (ListRegion.Reverse, ListRegion.ConsumeOrCopy))))
        OwnedIR.Drop root
        OwnedIR.Drop output.Id
    ]
    { Body = { Parameters = []; Operations = operations; Result = result } }

let private testRuntimeSharedOwnership () =
    runtimeSharedBlock () |> VerifyListOwnership.verifyBlockOwnership

let private testPrimitiveContracts () =
    let listValue id : HIR.Value = { Id = HIR.ValueId id; Type = AST.TList AST.TInt64 }
    let scalarValue: HIR.Value = { Id = HIR.ValueId 2; Type = AST.TInt64 }
    let input, output = listValue 0, listValue 1
    let scalar: HIR.Operand = { Expression = CheckedAST.Int64Literal 0L; Type = AST.TInt64; Inputs = Map.empty }
    let callback: HIR.Operand =
        { Expression = CheckedAST.Closure (TestIds.functionIdForName "mapCallback", [])
          Type = AST.TFunction ([AST.TInt64], AST.TInt64)
          Inputs = Map.empty }
    let construct = ListRegion.primitiveContract (ListRegion.Construct (output, ListRegion.Literal [scalar]))
    let transform =
        ListRegion.primitiveContract (
            ListRegion.Transform (output, input, (ListRegion.Map callback, ListRegion.StaticReuse)))
    let fold = ListRegion.primitiveContract (ListRegion.Fold (scalarValue, input, scalar, callback))
    let alias (contract: HIR.PrimitiveContract) = contract.Outputs |> List.map (fun result -> result.Alias)
    if alias construct <> [HIR.FreshManaged]
       || construct.Effects <> Set.ofList [HIR.MayEvaluateOpaqueSource; HIR.MayAllocate] then
        Error $"Unexpected construction contract: {construct}"
    elif alias transform <> [HIR.MayReuseInput input]
         || transform.Effects <>
            Set.ofList [HIR.MayEvaluateOpaqueSource; HIR.MayAllocate; HIR.MayInvokeUserCode
                        HIR.ReadsOwnedStorage; HIR.WritesOwnedStorage] then
        Error $"Unexpected transformation contract: {transform}"
    elif alias fold <> [HIR.NoManagedAlias]
         || fold.Effects <> Set.ofList [HIR.MayEvaluateOpaqueSource; HIR.MayInvokeUserCode; HIR.ReadsOwnedStorage] then
        Error $"Unexpected fold contract: {fold}"
    else Ok ()

let tests = [
    "Scope destruction rejects transitive callers and accepts safe recursive components", (fun () ->
        let fid = TestIds.functionIdForName
        let contract local calls : DestructionAnalysis.FunctionScopeContract =
            { LocalDestruction = local; Calls = calls |> List.map fid |> Set.ofList }
        let contracts =
            [ "resource", contract DestructionAnalysis.UnprovenScope []
              "indirect", contract DestructionAnalysis.InertScope ["resource"]
              "caller", contract DestructionAnalysis.InertScope ["indirect"]
              "unknown", contract DestructionAnalysis.InertScope ["external"]
              "left", contract DestructionAnalysis.InertScope ["right"]
              "right", contract DestructionAnalysis.InertScope ["left"; "Builtin.printLine"] ]
            |> List.map (fun (name, value) -> fid name, value)
            |> Map.ofList
        let functionNames =
            ["resource"; "indirect"; "caller"; "unknown"; "external"; "left"; "right"; "Builtin.print"; "Builtin.printLine"]
            |> List.map (fun name -> fid name, name)
            |> Map.ofList
        let actual = DestructionAnalysis.inertFunctionScopes functionNames contracts
        let expected =
            ["left"; "right"; "Builtin.print"; "Builtin.printLine"]
            |> List.map fid
            |> Set.ofList
        if actual = expected then Ok () else Error $"Unexpected inert scopes: {actual}")
    "Scope destruction replacement revokes caller proofs", (fun () ->
        let fid = TestIds.functionIdForName
        let safe : DestructionAnalysis.FunctionScopeContract = { LocalDestruction = DestructionAnalysis.InertScope; Calls = Set.empty }
        let contracts =
            Map.ofList [
                fid "callee", safe
                fid "caller", { safe with Calls = Set.singleton (fid "callee") }
            ]
        let replaced =
            Map.add
                (fid "callee")
                { safe with LocalDestruction = DestructionAnalysis.UnprovenScope }
                contracts
        let functionNames = Map.ofList [fid "callee", "callee"; fid "caller", "caller"]
        if Set.contains (fid "caller") (DestructionAnalysis.inertFunctionScopes functionNames contracts)
           && not (Set.contains (fid "caller") (DestructionAnalysis.inertFunctionScopes functionNames replaced)) then Ok ()
        else Error "Replacing a definition did not revoke its transitive scope proof")
    "Scope destruction does not trust a shadowed primitive", (fun () ->
        let functionId = TestIds.functionIdForName "Builtin.printLine"
        let contracts = Map.ofList [functionId, { DestructionAnalysis.LocalDestruction = DestructionAnalysis.UnprovenScope; DestructionAnalysis.Calls = Set.empty }]
        let functionNames = Map.ofList [functionId, "Builtin.printLine"]
        if Set.contains functionId (DestructionAnalysis.inertFunctionScopes functionNames contracts) then Error "Shadowed primitive retained its built-in contract"
        else Ok ())
    "List HIR accepts deep shared continuations", (fun () ->
        match extract (manyBranches 64) with
        | None -> Error "Expected a region with sixty-four scalar joins"
        | Some region -> region |> SelectListStorage.selectStorage |> ElaborateListOwnership.elaborateOwnership |> VerifyListOwnership.verify)
    "List HIR rejects list-valued branch joins", rejects (bind "xs" (values 3) (bind "selected" (choice (reverse (local "xs")) (local "xs")) (fold (local "selected"))))
    "List HIR rejects branch callbacks hiding aliases", rejects (bind "xs" (values 3) (choice (fold (call "Darklang.Stdlib.List.map_i64_i64" [local "xs"; CheckedAST.Closure (TestIds.functionIdForName "mapCallback", [local "xs"])])) (CheckedAST.Int64Literal 7L)))
    "List HIR consumes independently on mutually exclusive paths", checkBudget branchUses (ListRegion.Conditional (allocated, ListRegion.Complete { zero with ReusedTransforms = 1; Releases = 1 }, ListRegion.Complete { zero with ReusedTransforms = 1; Releases = 1 }, ListRegion.Complete zero))
    "List HIR preserves a source needed after the join", checkBudget branchJoin (ListRegion.Conditional (allocated, ListRegion.Complete { allocated with Copies = 1; Releases = 1 }, ListRegion.Complete zero, ListRegion.Complete { zero with Releases = 1 }))
    "List HIR releases unused inputs on the other edge", checkBudget (bind "xs" (values 3) (choice (fold (local "xs")) (CheckedAST.Int64Literal 7L))) (ListRegion.Conditional (allocated, ListRegion.Complete { zero with Releases = 1 }, ListRegion.Complete { zero with Releases = 1 }, ListRegion.Complete zero))
    "List HIR budgets branch-local constructors separately", checkBudget (bind "xs" (values 3) (choice (fold repeat) (fold (values 3)))) (ListRegion.Conditional ({ allocated with Releases = 1 }, ListRegion.Complete { zero with Allocations = 1; AllocatedBytes = runtimeBytes 0L [HIR.ValueId 1, 1L]; Releases = 1 }, ListRegion.Complete { allocated with Releases = 1 }, ListRegion.Complete zero))
    "List HIR verifier rejects mismatched branch ownership", rejectsOwnership [construct []; ownedBranch (ownedBlock [root] []) (ownedBlock [] [])]
    "List HIR verifier rejects branch-local leaked values", rejectsOwnership [ownedBranch (ownedBlock [] [construct []]) (ownedBlock [] [])]
    "List HIR verifier rejects duplicate identities across branches", rejectsOwnership [ownedBranch (ownedBlock [] [construct [root]]) (ownedBlock [] [construct [root]])]
    "List HIR verifier rejects double edge cleanup", rejectsOwnership [construct []; ownedBranch (ownedBlock [root; root] []) (ownedBlock [root] [])]
    "List HIR verifies runtime copy-on-write after ownership duplication", testRuntimeSharedOwnership
    "List HIR rejects static consumption after ownership duplication", rejectsOwnership [construct []; [OwnedIR.Dup root]; transform [root; HIR.ValueId 1]]
    "List HIR selects runtime reuse for an ownership boundary", checkBudget runtimeUnique (ListRegion.RuntimeConditional (allocated, ListRegion.Complete { zero with ReusedTransforms = 1 }, ListRegion.Complete { allocated with Copies = 1 }, ListRegion.Complete { zero with Releases = 1 }))
    "List HIR protects aliases across a runtime ownership boundary", checkBudget runtimeShared (ListRegion.RuntimeConditional (allocated, ListRegion.Complete { zero with ReusedTransforms = 1 }, ListRegion.Complete { allocated with Copies = 1 }, ListRegion.Complete { zero with Releases = 2 }))
    "List HIR consumes unique map/reverse storage", checkSummary unique { Allocations = 1; AllocatedBytes = bytes 56L; Copies = 0; ReusedTransforms = 2; Releases = 1 }
    "List HIR copies a surviving source version", checkSummary shared { Allocations = 2; AllocatedBytes = bytes 112L; Copies = 1; ReusedTransforms = 0; Releases = 2 }
    "List HIR normalizes aliases before last-use solving", checkSummary (bind "xs" (values 3) (bind "alias" (local "xs") (fold (reverse (local "alias"))))) { Allocations = 1; AllocatedBytes = bytes 56L; Copies = 0; ReusedTransforms = 1; Releases = 1 }
    "List HIR releases unused construction", checkSummary (bind "xs" (values 3) (CheckedAST.Int64Literal 1L)) { Allocations = 1; AllocatedBytes = bytes 56L; Copies = 0; ReusedTransforms = 0; Releases = 1 }
    "List HIR supports the largest recyclable array", checkSummary (fold (reverse (values 28))) { Allocations = 1; AllocatedBytes = bytes 256L; Copies = 0; ReusedTransforms = 1; Releases = 1 }
    "List HIR budgets runtime construction and consuming transforms", checkSummary (fold (reverse (map repeat))) { Allocations = 1; AllocatedBytes = runtimeBytes 0L [root, 1L]; Copies = 0; ReusedTransforms = 2; Releases = 1 }
    "List HIR budgets runtime copies through aliases", checkSummary (bind "xs" repeat (bind "alias" (local "xs") (bind "ys" (map (local "xs")) (bind "old" (fold (local "alias")) (fold (local "ys")))))) { Allocations = 2; AllocatedBytes = runtimeBytes 0L [root, 2L]; Copies = 1; ReusedTransforms = 0; Releases = 2 }
    "List HIR keeps independent runtime extents distinct", checkSummary (
        let outerId = AST.bindingId 1001
        let innerId = AST.bindingId 1002
        CheckedAST.Let (
            CheckedAST.LPVariable outerId,
            repeat,
            CheckedAST.Let (
                CheckedAST.LPVariable innerId,
                repeat,
                fold (reverse (CheckedAST.Local innerId))
            )
        )) { Allocations = 2; AllocatedBytes = runtimeBytes 0L [root, 1L; HIR.ValueId 1, 1L]; Copies = 0; ReusedTransforms = 1; Releases = 2 }
    "List HIR retains runtime origin when copying a consumed transform", checkSummary (bind "ys" (map repeat) (bind "zs" (map (local "ys")) (bind "old" (fold (local "ys")) (fold (local "zs"))))) { Allocations = 2; AllocatedBytes = runtimeBytes 0L [root, 2L]; Copies = 1; ReusedTransforms = 1; Releases = 2 }
    "List HIR preserves native allocation budget", testLoweredBudget
    "List HIR rejects escaping lists", rejects (bind "xs" (values 3) (local "xs"))
    "List HIR reclaims arrays beyond the fixed heap classes", checkSummary (fold (reverse (values 29))) { Allocations = 1; AllocatedBytes = bytes 272L; Copies = 0; ReusedTransforms = 1; Releases = 1 }
    "List HIR rejects borrowed input lists", rejects (fold (reverse (local "external")))
    "List HIR rejects scalar wrappers around borrowed managed inputs", (fun () ->
        let expression = CheckedAST.Let (CheckedAST.LPWildcard, reverse (local "external"), CheckedAST.Int64Literal 1L)
        match extractWithParameters (Map.ofList [binding "external", AST.TList AST.TInt64]) expression with
        | None -> Ok ()
        | Some _ -> Error "A scalar wrapper admitted a borrowed list parameter")
    "List HIR declares primitive effects and alias provenance", testPrimitiveContracts
    "List HIR rejects managed elements", rejects (bind "xs" (CheckedAST.ListLiteral [CheckedAST.StringLiteral "a"]) (CheckedAST.Int64Literal 0L))
    "List HIR rejects callbacks capturing region lists", rejects (bind "xs" (values 3) (fold (call "Darklang.Stdlib.List.map_i64_i64" [local "xs"; CheckedAST.Closure (TestIds.functionIdForName "mapCallback", [local "xs"])])))
    "List HIR verifier rejects duplicate drop", rejectsOwnership [construct [root; root]]
    "List HIR verifier rejects leaked roots", rejectsOwnership [construct []]
    "List HIR verifier rejects reused identities", rejectsOwnership [construct [root]; construct [root]]
    "List HIR verifier rejects mutation after drop", rejectsOwnership [construct [root]; transform [HIR.ValueId 1]]
]
