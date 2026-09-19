// RegionContractTests.fs - Unit ownership laws independent of collection layout and codegen.

module RegionContractTests

open OwnedIR

let private fid = AST.functionIdForName

let private identity = function
    | "a" -> HIR.ValueId 0
    | "b" -> HIR.ValueId 1
    | "c" -> HIR.ValueId 2
    | "aAlias" -> HIR.ValueId 3
    | "scalar" -> HIR.ValueId 4
    | name -> Crash.crash $"Unsupported ownership fixture identity {name}"
let private binding = function
    | "a" -> AST.bindingId 0
    | "b" -> AST.bindingId 1
    | "c" -> AST.bindingId 2
    | "aAlias" -> AST.bindingId 3
    | name -> Crash.crash $"Unsupported ownership fixture binding {name}"
let private bindingName id =
    ["a"; "b"; "c"; "aAlias"]
    |> List.tryFind (fun name -> binding name = id)
    |> Option.defaultWith (fun () -> Crash.crash "Unsupported ownership fixture binding identity")
let private value name : HIR.Value = { Id = identity name; Type = AST.TInt64 }
let private unitValue : HIR.Value = { Id = HIR.ValueId 100; Type = AST.TUnit }
let private reference name : HIR.Operand =
    let id = binding name
    { Expression = CheckedAST.Local id; Type = AST.TInt64; Inputs = Map.ofList [id, value name] }
let private condition : HIR.Operand = { Expression = CheckedAST.BoolLiteral true; Type = AST.TBool; Inputs = Map.empty }

type private TestLeaf = {
    Ownership: Contract<string>
    Uniqueness: UniquenessContract<string>
}

let private semantics : Semantics<TestLeaf, string> = {
    Leaf = fun leaf -> leaf.Ownership
    LeafUniqueness = fun leaf -> leaf.Uniqueness
    CallOwnership = fun call ->
        match call.Target with
        | id when id = fid "borrow" -> Some { Parameters = [BorrowedCallParameter]; Result = BorrowedCallResult 0 }
        | id when id = fid "consume" -> Some { Parameters = [ConsumedCallParameter]; Result = ProducedCallResult }
        | id when id = fid "produce" -> Some { Parameters = []; Result = ProducedCallResult }
        | id when id = fid "consumeUnique" -> Some { Parameters = [UniqueCallParameter]; Result = UnmanagedCallResult }
        | id when id = fid "produceUnique" -> Some { Parameters = []; Result = UniqueProducedCallResult }
        | id when id = fid "consumeTwice" -> Some { Parameters = [ConsumedCallParameter; ConsumedCallParameter]; Result = UnmanagedCallResult }
        | id when id = fid "discard" -> Some { Parameters = [ConsumedCallParameter]; Result = UnmanagedCallResult }
        | id when id = fid "invalidBorrow" -> Some { Parameters = [ConsumedCallParameter]; Result = BorrowedCallResult 0 }
        | id when id = fid "negativeBorrow" -> Some { Parameters = [BorrowedCallParameter]; Result = BorrowedCallResult -1 }
        | _ -> None
    ScalarUses = fun value ->
        value.Inputs |> Map.keys |> Seq.map bindingName |> Set.ofSeq
    ScalarEscapes = fun value ->
        match value.Expression with
        | CheckedAST.Local id when id = binding "escape" ->
            value.Inputs |> Map.keys |> Seq.map bindingName |> Set.ofSeq
        | _ -> Set.empty
    BlockArgument = fun value ->
        match value.Id with
        | HIR.ValueId 0 -> Managed "a"
        | HIR.ValueId 1 -> Managed "b"
        | HIR.ValueId 2 -> Managed "c"
        | HIR.ValueId 3 -> Managed "a"
        | _ -> Unmanaged
}

let private drops values = values |> List.map Drop
let private leaf inputs outputs required uniqueOutputs : TestLeaf =
    { Ownership = { Inputs = inputs; Outputs = outputs }
      Uniqueness = { RequiredInputs = Set.ofList required; UniqueOutputs = Set.ofList uniqueOutputs } }
let private stepWithUniqueness inputs outputs required uniqueOutputs releases : Step<TestLeaf, string> list =
    Evaluate (HIR.Leaf (leaf inputs outputs required uniqueOutputs)) :: drops releases
let private step inputs outputs releases : Step<TestLeaf, string> list =
    stepWithUniqueness inputs outputs [] outputs releases
let private block entry operations : Block<TestLeaf, string> =
    { Body = { Parameters = []; Operations = drops entry @ List.concat operations; Result = unitValue } }
let private blockResult entry operations result : Block<TestLeaf, string> =
    { Body = { Parameters = []; Operations = drops entry @ List.concat operations; Result = result } }
let private functionBlock parameters operations result : Block<TestLeaf, string> =
    { Body =
        { Parameters =
              parameters
              |> List.map (fun name -> ({ Name = name; Binding = binding name; Value = value name }: HIR.Parameter))
          Operations = List.concat operations
          Result = result } }
let private branch predicate yes no : Step<TestLeaf, string> list =
    [Evaluate (HIR.Branch ({ Id = HIR.ValueId 101; Type = AST.TUnit }, predicate, yes, no))]
let private managedBranch result predicate yes no : Step<TestLeaf, string> list =
    [Evaluate (HIR.Branch (result, predicate, yes, no))]
let private read name releases : Step<TestLeaf, string> list =
    Evaluate (HIR.ScalarBinding ({ Id = HIR.ValueId 102; Type = AST.TInt64 }, reference name)) :: drops releases
let private escape name releases : Step<TestLeaf, string> list =
    let operand = { reference name with Expression = CheckedAST.Local (binding "escape") }
    Evaluate (HIR.ScalarBinding ({ Id = HIR.ValueId 103; Type = AST.TInt64 }, operand)) :: drops releases
let private call target arguments result : Step<TestLeaf, string> list =
    [Evaluate (HIR.Call { Target = fid target; Arguments = arguments; Result = result })]
let private duplicate value : Step<TestLeaf, string> list = [Dup value]
let private dropOne value : Step<TestLeaf, string> list = [Drop value]
let private check expected region () =
    let actual = VerifyOwnership.verifyClosed semantics region
    if actual = expected then Ok () else Error $"Expected {expected}, got {actual}"
let private checkFunction expected signature body () =
    let actual = VerifyOwnership.verifyFunction semantics signature body
    if actual = expected then Ok () else Error $"Expected {expected}, got {actual}"
let private checkCallSignature expected signature () =
    let actual = VerifyOwnership.callSignatureOfFunction signature
    if actual = expected then Ok () else Error $"Expected {expected}, got {actual}"

let tests = [
    "Function ownership signatures permit borrowed results from borrowed parameters", checkFunction (Ok ())
        { Parameters = [BorrowedParameter "a"]; Result = BorrowedResult "a" }
        (functionBlock ["a"] [] (value "a"))
    "Function ownership signatures cover unmanaged parameters explicitly", checkFunction (Ok ())
        { Parameters = [UnmanagedParameter]; Result = UnmanagedResult }
        (functionBlock ["scalar"] [] unitValue)
    "Function ownership signatures reject omitted unmanaged parameters", checkFunction
        (Error InconsistentFunctionParameters)
        { Parameters = []; Result = UnmanagedResult }
        (functionBlock ["scalar"] [] unitValue)
    "Function ownership signatures reject ownership in the wrong parameter position", checkFunction
        (Error InconsistentFunctionParameters)
        { Parameters = [BorrowedParameter "a"; UnmanagedParameter]; Result = UnmanagedResult }
        (functionBlock ["scalar"; "a"] [dropOne "a"] unitValue)
    "Function ownership signatures derive positional call contracts", checkCallSignature
        (Ok {
            Parameters = [UnmanagedCallParameter; BorrowedCallParameter; UniqueCallParameter]
            Result = BorrowedCallResult 1
        })
        {
            Parameters = [UnmanagedParameter; BorrowedParameter "a"; UniqueParameter "b"]
            Result = BorrowedResult "a"
        }
    "Function ownership signatures reject borrowed calls from transferred parameters", checkCallSignature
        (Error (InvalidBorrowedResult "a"))
        { Parameters = [ConsumedParameter "a"]; Result = BorrowedResult "a" }
    "Function ownership signatures reject consuming borrowed parameters", checkFunction (Error (InvalidDrop "a"))
        { Parameters = [BorrowedParameter "a"]; Result = UnmanagedResult }
        (functionBlock ["a"] [step [Consumed "a"] [] []] unitValue)
    "Function ownership signatures transfer consumed parameters into produced results", checkFunction (Ok ())
        { Parameters = [ConsumedParameter "a"]; Result = ProducedResult "a" }
        (functionBlock ["a"] [] (value "a"))
    "Function ownership signatures transfer locally produced results", checkFunction (Ok ())
        { Parameters = []; Result = ProducedResult "a" }
        (functionBlock [] [step [] ["a"] []] (value "a"))
    "Unique function results accept locally exclusive values", checkFunction (Ok ())
        { Parameters = []; Result = UniqueProducedResult "a" }
        (functionBlock [] [step [] ["a"] []] (value "a"))
    "Unique function results reject ordinary consumed parameters", checkFunction (Error (NonUniqueUse "a"))
        { Parameters = [ConsumedParameter "a"]; Result = UniqueProducedResult "a" }
        (functionBlock ["a"] [] (value "a"))
    "Unique function parameters satisfy unique-consuming operations", checkFunction (Ok ())
        { Parameters = [UniqueParameter "a"]; Result = UnmanagedResult }
        (functionBlock ["a"] [stepWithUniqueness [Consumed "a"] [] ["a"] [] []] unitValue)
    "Ordinary consumed parameters do not satisfy unique-consuming operations", checkFunction (Error (NonUniqueUse "a"))
        { Parameters = [ConsumedParameter "a"]; Result = UnmanagedResult }
        (functionBlock ["a"] [stepWithUniqueness [Consumed "a"] [] ["a"] [] []] unitValue)
    "Function ownership signatures require consumed parameters to leave the function", checkFunction (Error (UndroppedValues (Set.singleton "a")))
        { Parameters = [ConsumedParameter "a"]; Result = UnmanagedResult }
        (functionBlock ["a"] [] unitValue)
    "Function ownership signatures reject producing borrowed parameters", checkFunction (Error (InvalidProducedResult "a"))
        { Parameters = [BorrowedParameter "a"]; Result = ProducedResult "a" }
        (functionBlock ["a"] [] (value "a"))
    "Explicit dup permits producing ownership from borrowed parameters", checkFunction (Ok ())
        { Parameters = [BorrowedParameter "a"]; Result = ProducedResult "a" }
        (functionBlock ["a"] [duplicate "a"] (value "a"))
    "Function ownership signatures reject borrowing consumed parameters", checkFunction (Error (InvalidBorrowedResult "a"))
        { Parameters = [ConsumedParameter "a"]; Result = BorrowedResult "a" }
        (functionBlock ["a"] [] (value "a"))
    "Function ownership signatures cover every managed parameter", checkFunction (Error InconsistentFunctionParameters)
        { Parameters = []; Result = UnmanagedResult }
        (functionBlock ["a"] [] unitValue)
    "Function ownership signatures reject duplicate parameters", checkFunction (Error (DuplicateParameter "a"))
        { Parameters = [BorrowedParameter "a"; BorrowedParameter "a"]; Result = UnmanagedResult }
        (functionBlock ["a"] [] unitValue)
    "Function ownership signatures agree with the managed result identity", checkFunction (Error InconsistentFunctionResult)
        { Parameters = [ConsumedParameter "a"]; Result = ProducedResult "b" }
        (functionBlock ["a"] [] (value "a"))
    "Call ownership signatures preserve borrowed arguments and results", check (Ok ())
        (block [] [step [] ["a"] []; call "borrow" [value "a"] (value "aAlias"); step [Consumed "a"] [] []])
    "Call ownership signatures transfer consumed arguments into produced results", check (Ok ())
        (block [] [step [] ["a"] []; call "consume" [value "a"] (value "b"); step [Consumed "b"] [] []])
    "Unique call parameters reject results without exclusivity provenance", check (Error (NonUniqueUse "a"))
        (block [] [call "produce" [] (value "a"); call "consumeUnique" [value "a"] unitValue])
    "Unique call results satisfy unique call parameters", check (Ok ())
        (block [] [call "produceUnique" [] (value "a"); call "consumeUnique" [value "a"] unitValue])
    "Explicit dup satisfies repeated consumed call arguments", check (Ok ())
        (block [] [step [] ["a"] []; duplicate "a"; call "consumeTwice" [value "a"; value "aAlias"] unitValue])
    "Repeated consumed call arguments reject missing dup", check (Error (InvalidDrop "a"))
        (block [] [step [] ["a"] []; call "consumeTwice" [value "a"; value "aAlias"] unitValue])
    "Call ownership signatures reject unavailable consumed arguments", check (Error (InvalidUse "a"))
        (block [] [call "discard" [value "a"] unitValue])
    "Call ownership signatures reject unknown targets", check (Error (UnknownCallOwnership (fid "opaque")))
        (block [] [call "opaque" [value "a"] unitValue])
    "Call ownership signatures reject borrowed results from consumed parameters", check (Error (InvalidBorrowedCallResult (fid "invalidBorrow", 0)))
        (block [] [step [] ["a"] []; call "invalidBorrow" [value "a"] (value "a")])
    "Call ownership signatures reject negative borrowed-result parameters", check (Error (InvalidBorrowedCallResult (fid "negativeBorrow", -1)))
        (block [] [step [] ["a"] []; call "negativeBorrow" [value "a"] (value "aAlias")])
    "Ownership contracts borrow before consuming multiple inputs", check (Ok ())
        (block [] [step [] ["a"; "b"] []; step [Borrowed "a"; Consumed "a"; Consumed "b"] ["c"] ["c"]])
    "Ownership contracts reject duplicate consumed units", check (Error (InvalidDrop "a"))
        (block [] [step [] ["a"] []; step [Consumed "a"; Consumed "a"] [] []])
    "Explicit dup permits multiple consuming uses", check (Ok ())
        (block [] [step [] ["a"] []; duplicate "a"; step [Consumed "a"; Consumed "a"] [] []])
    "Fresh outputs satisfy unique-consuming operations", check (Ok ())
        (block [] [step [] ["a"] []; stepWithUniqueness [Consumed "a"] [] ["a"] [] []])
    "Explicit dup temporarily prevents unique use", check (Error (NonUniqueUse "a"))
        (block [] [step [] ["a"] []; duplicate "a"; stepWithUniqueness [Consumed "a"] [] ["a"] [] []])
    "Explicit drop restores unique use after dup", check (Ok ())
        (block [] [step [] ["a"] []; duplicate "a"; dropOne "a"; stepWithUniqueness [Consumed "a"] [] ["a"] [] []])
    "Explicit dup of a borrowed parameter creates a droppable unit", checkFunction (Ok ())
        { Parameters = [BorrowedParameter "a"]; Result = UnmanagedResult }
        (functionBlock ["a"] [duplicate "a"; step [Consumed "a"] [] []] unitValue)
    "Ownership contracts reject duplicate result identities", check (Error (DuplicateDefinition "a"))
        (block [] [step [] ["a"; "a"] []])
    "Ownership contracts reject unknown borrows", check (Error (InvalidUse "a"))
        (block [] [step [Borrowed "a"] [] []])
    "Ownership contracts reject drop after consume", check (Error (InvalidDrop "a"))
        (block [] [step [] ["a"] []; step [Consumed "a"] [] ["a"]])
    "Ownership contracts account for scalar operand reads", check (Ok ())
        (block [] [step [] ["a"] []; read "a" ["a"]])
    "Scalar escape revokes exclusivity provenance", check (Error (NonUniqueUse "a"))
        (block [] [step [] ["a"] []; escape "a" []; stepWithUniqueness [Consumed "a"] [] ["a"] [] []])
    "Ownership contracts reject scalar use after release", check (Error (InvalidUse "a"))
        (block [] [step [] ["a"] ["a"]; read "a" []])
    "Ownership contracts check branch conditions", check (Error (InvalidUse "a"))
        (block [] [branch (reference "a") (block [] []) (block [] [])])
    "Ownership contracts check block results", check (Error (InvalidUse "a"))
        (blockResult [] [step [] ["a"] ["a"]] (value "a"))
    "Ownership contracts allow consumption on exclusive paths", check (Ok ())
        (block [] [step [] ["a"] []; branch condition
            (block [] [step [Consumed "a"] [] []])
            (block [] [step [Consumed "a"] [] []])])
    "Ownership contracts preserve values through nested joins", check (Ok ())
        (block [] [step [] ["a"] []; branch condition
            (block [] [branch condition (block [] [read "a" []]) (block [] [])])
            (block [] [read "a" []]); step [Consumed "a"] [] []])
    "Explicit dup and drop balance across joins", check (Ok ())
        (block [] [step [] ["a"] []; branch condition
            (block [] [duplicate "a"; dropOne "a"])
            (block [] []); step [Consumed "a"] [] []])
    "Explicit dup counts must agree across joins", check (Error InconsistentJoin)
        (block [] [step [] ["a"] []; branch condition
            (block [] [duplicate "a"])
            (block [] [])])
    "Ownership contracts reject inconsistent joins", check (Error InconsistentJoin)
        (block [] [step [] ["a"] []; branch condition (block ["a"] []) (block [] [])])
    "Ownership contracts enforce global branch identity freshness", check (Error (DuplicateDefinition "a"))
        (block [] [branch condition (block [] [step [] ["a"] ["a"]]) (block [] [step [] ["a"] ["a"]])])
    "Ownership contracts reject double edge drop", check (Error (InvalidDrop "a"))
        (block [] [step [] ["a"] []; branch condition (block ["a"; "a"] []) (block ["a"] [])])
    "Ownership contracts transfer distinct managed block arguments", check (Ok ())
        (block [] [
            managedBranch (value "c") condition
                (blockResult [] [step [] ["a"] []] (value "a"))
                (blockResult [] [step [] ["b"] []] (value "b"))
            step [Consumed "c"] [] []
        ])
    "Managed joins preserve exclusivity from both incoming values", check (Ok ())
        (block [] [
            managedBranch (value "c") condition
                (blockResult [] [step [] ["a"] []] (value "a"))
                (blockResult [] [step [] ["b"] []] (value "b"))
            stepWithUniqueness [Consumed "c"] [] ["c"] [] []
        ])
    "Managed joins reject uniqueness from only one incoming value", check (Error (NonUniqueUse "c"))
        (block [] [
            managedBranch (value "c") condition
                (blockResult [] [step [] ["a"] []] (value "a"))
                (blockResult [] [stepWithUniqueness [] ["b"] [] [] []] (value "b"))
            stepWithUniqueness [Consumed "c"] [] ["c"] [] []
        ])
    "Ownership contracts rename one incoming value on exclusive edges", check (Ok ())
        (block [] [
            step [] ["a"] []
            managedBranch (value "c") condition
                (blockResult [] [] (value "a"))
                (blockResult [] [] (value "a"))
            step [Consumed "c"] [] []
        ])
    "Ownership contracts reject mixed managed block arguments", check (Error InconsistentBlockArgument)
        (block [] [
            managedBranch (value "c") condition
                (blockResult [] [step [] ["a"] []] (value "a"))
                (block [] [])
        ])
    "Ownership contracts reject a released block argument", check (Error (InvalidUse "a"))
        (block [] [
            managedBranch (value "c") condition
                (blockResult [] [step [] ["a"] ["a"]] (value "a"))
                (blockResult [] [step [] ["b"] []] (value "b"))
        ])
    "Ownership contracts reject incoming roots in closed regions", check (Error (InvalidDrop "a"))
        (block ["a"] [])
    "Ownership contracts reject leaked units", check (Error (UndroppedValues (Set.singleton "a")))
        (block [] [step [] ["a"] []])
    "Ownership contracts reject undropped duplicate units", check (Error (UndroppedValues (Set.singleton "a")))
        (block [] [step [] ["a"] []; duplicate "a"; step [Consumed "a"] [] []])
]
