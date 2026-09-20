// ANFDirectCallSpecializationTests.fs - Tests for internal direct-call signature specialization.
//
// Covers uniform-literal parameters across recursive call graphs, plus functions
// whose indirect uses make their complete calling convention unavailable.

module ANFDirectCallSpecializationTests

open ANF

type TestResult = Result<unit, string>

let private intAtom (value: int64) : Atom =
    IntLiteral (Int64 value)

let private param id =
    { Id = TempId id; Type = AST.TInt64 }

let private typedParam id typ =
    { Id = TempId id; Type = typ }

let private functionByName (name: string) (functions: Function list) : Function option =
    functions |> List.tryFind (fun func -> func.Name = name)

let rec private directCallArgs (target: string) (expr: AExpr) : Atom list option =
    match expr with
    | Jump _ | Return _ -> None
    | Let (_, Call (name, args), _) when name = AST.functionIdForName target -> Some args
    | Let (_, BorrowedCall (name, args), _) when name = AST.functionIdForName target -> Some args
    | Let (_, TailCall (name, args), _) when name = AST.functionIdForName target -> Some args
    | Let (_, _, body) -> directCallArgs target body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        match directCallArgs target thenBranch with
        | Some args -> Some args
        | None -> directCallArgs target elseBranch

let rec private containsAtom (expected: Atom) (expr: AExpr) : bool =
    let cexprContains cexpr =
        match cexpr with
        | Prim (_, left, right) -> left = expected || right = expected
        | _ -> false
    match expr with
    | Return atom -> atom = expected
    | Jump (_, atom) -> atom = expected
    | Join (_, continuation, entry) -> containsAtom expected continuation || containsAtom expected entry
    | Let (_, cexpr, body) -> cexprContains cexpr || containsAtom expected body
    | If (condition, thenBranch, elseBranch) ->
        condition = expected
        || containsAtom expected thenBranch
        || containsAtom expected elseBranch

let rec private containsAggregateAllocation (expr: AExpr) : bool =
    let isAllocation = function
        | TupleAlloc _ | RecordAlloc _ -> true
        | _ -> false
    match expr with
    | Jump _ | Return _ -> false
    | Let (_, cexpr, body) -> isAllocation cexpr || containsAggregateAllocation body
    | Join (_, continuation, entry)
    | If (_, continuation, entry) ->
        containsAggregateAllocation continuation || containsAggregateAllocation entry

let private expectArity (name: string) (expected: int) (functions: Function list) : TestResult =
    match functionByName name functions with
    | None -> Error $"Expected function '{name}'"
    | Some func when List.length func.TypedParams = expected -> Ok ()
    | Some func ->
        Error $"Expected '{name}' to have {expected} parameters, found {List.length func.TypedParams}"

let private expectDirectCallArity
    (callerName: string)
    (calleeName: string)
    (expected: int)
    (functions: Function list)
    : TestResult =
    match functionByName callerName functions with
    | None -> Error $"Expected caller '{callerName}'"
    | Some caller ->
        match directCallArgs calleeName caller.Body with
        | Some args when List.length args = expected -> Ok ()
        | Some args ->
            Error $"Expected '{callerName}' call to '{calleeName}' to have {expected} arguments, found {List.length args}"
        | None -> Error $"Expected '{callerName}' to call '{calleeName}'"

let testUniformLiteralParametersRewriteRecursiveGroup () : TestResult =
    let fValue = param 0
    let fConstant = param 1
    let gValue = param 2
    let gConstant = param 3
    let f =
        { Id = AST.functionIdForName "f"
          Name = "f"
          TypedParams = [fValue; fConstant]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 4,
                Prim (Add, Var fValue.Id, Var fConstant.Id),
                Let (
                    TempId 5,
                    Call (AST.functionIdForName "g", [Var (TempId 4); intAtom 7L]),
                    Return (Var (TempId 5))
                )
            ) }
    let g =
        { Id = AST.functionIdForName "g"
          Name = "g"
          TypedParams = [gValue; gConstant]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 6,
                Prim (Sub, Var gValue.Id, Var gConstant.Id),
                Let (
                    TempId 7,
                    TailCall (AST.functionIdForName "f", [Var (TempId 6); intAtom 7L]),
                    Return (Var (TempId 7))
                )
            ) }
    let main =
        Let (TempId 8, Call (AST.functionIdForName "f", [intAtom 1L; intAtom 7L]), Return (Var (TempId 8)))
    let (Program (functions, main')) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([f; g], main))
    match expectArity "f" 1 functions with
    | Error err -> Error err
    | Ok () ->
        match expectArity "g" 1 functions with
        | Error err -> Error err
        | Ok () ->
            match expectDirectCallArity "f" "g" 1 functions with
            | Error err -> Error err
            | Ok () ->
                match expectDirectCallArity "g" "f" 1 functions with
                | Error err -> Error err
                | Ok () ->
                    match directCallArgs "f" main', functionByName "f" functions, functionByName "g" functions with
                    | Some [_], Some f', Some g'
                        when containsAtom (intAtom 7L) f'.Body
                             && containsAtom (intAtom 7L) g'.Body -> Ok ()
                    | Some [_], _, _ -> Error "Expected the removed parameters to be replaced by their literal"
                    | _ -> Error "Expected main call to recursive group to lose the uniform literal argument"

let testDifferingLiteralsRetainUnspecializedFallback () : TestResult =
    let constant = param 0
    let dynamic = param 3
    let target =
        { Id = AST.functionIdForName "target"
          Name = "target"
          TypedParams = [constant]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (Var constant.Id) }
    let caller =
        { Id = AST.functionIdForName "caller"
          Name = "caller"
          TypedParams = [dynamic]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Call (AST.functionIdForName "target", [intAtom 7L]),
                Let (
                    TempId 2,
                    Call (AST.functionIdForName "target", [intAtom 8L]),
                    Let (
                        TempId 4,
                        TailCall (AST.functionIdForName "target", [Var dynamic.Id]),
                        Return (Var (TempId 4))
                    )
                )
            ) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([target; caller], Return UnitLiteral))
    match expectArity "target" 1 functions with
    | Error err -> Error err
    | Ok () -> expectDirectCallArity "caller" "target" 1 functions

let testFiniteScalarLiteralsCreateBoundedClones () : TestResult =
    let constant = param 0
    let target =
        { Id = AST.functionIdForName "target"
          Name = "target"
          TypedParams = [constant]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Prim (Add, Var constant.Id, intAtom 1L),
                Return (Var (TempId 1))
            ) }
    let caller =
        { Id = AST.functionIdForName "caller"
          Name = "caller"
          TypedParams = []
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 2,
                Call (AST.functionIdForName "target", [intAtom 7L]),
                Let (
                    TempId 3,
                    TailCall (AST.functionIdForName "target", [intAtom 8L]),
                    Return (Var (TempId 3))
                )
            ) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([target; caller], Return UnitLiteral))
    let clones =
        functions
        |> List.filter (fun func -> func.Name.StartsWith("target__literal_"))
    match clones with
    | [first; second]
        when List.isEmpty first.TypedParams
             && List.isEmpty second.TypedParams
             && containsAtom (intAtom 7L) first.Body
             && containsAtom (intAtom 8L) second.Body ->
        match functionByName "caller" functions with
        | Some caller'
            when directCallArgs first.Name caller'.Body = Some []
                 && directCallArgs second.Name caller'.Body = Some [] -> Ok ()
        | _ -> Error "Expected literal call sites to target their zero-argument clones"
    | _ -> Error $"Expected two literal-specialized target clones, found {List.length clones}"

let testRecursiveCloneKeepsTailCallAndReducedSignature () : TestResult =
    let value = param 0
    let mode = param 1
    let loop =
        { Id = AST.functionIdForName "loop"
          Name = "loop"
          TypedParams = [value; mode]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 2,
                Prim (Sub, Var value.Id, intAtom 1L),
                Let (
                    TempId 3,
                    TailCall (AST.functionIdForName "loop", [Var (TempId 2); Var mode.Id]),
                    Return (Var (TempId 3))
                )
            ) }
    let main =
        Let (
            TempId 4,
            Call (AST.functionIdForName "loop", [intAtom 10L; intAtom 1L]),
            Let (
                TempId 5,
                Call (AST.functionIdForName "loop", [intAtom 10L; intAtom 2L]),
                Return (Var (TempId 5))
            )
        )
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram (Program ([loop], main))
    let clones =
        functions
        |> List.filter (fun func -> func.Name.StartsWith("loop__literal_"))
    match clones with
    | [first; second]
        when List.length first.TypedParams = 1
             && List.length second.TypedParams = 1 ->
        match directCallArgs first.Name first.Body, directCallArgs second.Name second.Body with
        | Some [_], Some [_] -> Ok ()
        | _ -> Error "Expected each recursive tail call to target its reduced-signature clone"
    | _ -> Error $"Expected two recursive literal clones, found {List.length clones}"

let testUniformStringLiteralIsSpecialized () : TestResult =
    let text = typedParam 0 AST.TString
    let target =
        { Id = AST.functionIdForName "managed"
          Name = "managed"
          TypedParams = [text]
          ReturnType = AST.TString
          ReturnOwnership = OwnedReturn
          Body = Return (Var text.Id) }
    let caller =
        { Id = AST.functionIdForName "caller"
          Name = "caller"
          TypedParams = []
          ReturnType = AST.TString
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Call (AST.functionIdForName "managed", [StringLiteral "second"]),
                Let (
                    TempId 2,
                    Call (AST.functionIdForName "managed", [StringLiteral "second"]),
                    Return (Var (TempId 2))
                )
            ) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([target; caller], Return UnitLiteral))
    match expectArity "managed" 0 functions with
    | Error err -> Error err
    | Ok () -> expectDirectCallArity "caller" "managed" 0 functions

let testImmediateSemanticValuesCreateClones () : TestResult =
    let cases =
        [ ("charValue", AST.TChar, StringLiteral "a", StringLiteral "b")
          ("dateValue", AST.TDateTime, intAtom 10L, intAtom 20L)
          ("enumValue", AST.TSum ("Mode", []), intAtom 0L, intAtom 1L) ]
    let checkCase (name, typ, firstValue, secondValue) =
        let value = typedParam 0 typ
        let target =
            { Id = AST.functionIdForName name
              Name = name
              TypedParams = [value]
              ReturnType = typ
              ReturnOwnership = OwnedReturn
              Body = Return (Var value.Id) }
        let caller =
            { Id = AST.functionIdForName $"{name}Caller"
              Name = $"{name}Caller"
              TypedParams = []
              ReturnType = typ
              ReturnOwnership = OwnedReturn
              Body =
                Let (
                    TempId 1,
                    Call (AST.functionIdForName name, [firstValue]),
                    Let (
                        TempId 2,
                        Call (AST.functionIdForName name, [secondValue]),
                        Return (Var (TempId 2))
                    )
                ) }
        let (Program (functions, _)) =
            ANF_DirectCallSpecialization.specializeProgram
                (Program ([target; caller], Return UnitLiteral))
        let clones =
            functions
            |> List.filter (fun func -> func.Name.StartsWith($"{name}__literal_"))
        if List.length clones = 2
           && (clones |> List.forall (fun func -> List.isEmpty func.TypedParams)) then Ok ()
        else Error $"Expected two literal-specialized clones for {name}, found {List.length clones}"
    cases
    |> List.fold (fun result case -> Result.bind (fun () -> checkCase case) result) (Ok ())

let testKnownIndirectTargetBecomesSpecializable () : TestResult =
    let value = param 0
    let target =
        { Id = AST.functionIdForName "knownTarget"
          Name = "knownTarget"
          TypedParams = [value]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (Var value.Id) }
    let caller =
        { Id = AST.functionIdForName "knownCaller"
          Name = "knownCaller"
          TypedParams = []
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                IndirectCall (FuncRef (AST.functionIdForName "knownTarget"), [intAtom 7L]),
                Return (Var (TempId 1))
            ) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([target; caller], Return UnitLiteral))
    match expectArity "knownTarget" 0 functions with
    | Error err -> Error err
    | Ok () -> expectDirectCallArity "knownCaller" "knownTarget" 0 functions

let testMismatchedBottomPlaceholderDoesNotSeedClone () : TestResult =
    let value = typedParam 0 AST.TString
    let target =
        { Id = AST.functionIdForName "bottomTarget"
          Name = "bottomTarget"
          TypedParams = [value]
          ReturnType = AST.TString
          ReturnOwnership = OwnedReturn
          Body = Return (Var value.Id) }
    let caller =
        { Id = AST.functionIdForName "bottomCaller"
          Name = "bottomCaller"
          TypedParams = []
          ReturnType = AST.TString
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Call (AST.functionIdForName "bottomTarget", [StringLiteral "first"]),
                Let (
                    TempId 2,
                    Call (AST.functionIdForName "bottomTarget", [StringLiteral "second"]),
                    Let (
                        TempId 3,
                        Call (AST.functionIdForName "bottomTarget", [UnitLiteral]),
                        Return (Var (TempId 2))
                    )
                )
            ) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([target; caller], Return UnitLiteral))
    let clones =
        functions
        |> List.filter (fun func -> func.Name.StartsWith("bottomTarget__literal_"))
    if List.length clones = 2
       && (clones |> List.forall (fun func -> not (containsAtom UnitLiteral func.Body))) then Ok ()
    else Error "Expected only representation-compatible String clones"

let testConstructionValuesCreateClones () : TestResult =
    let checkCase name typ firstConstruction secondConstruction =
        let value = typedParam 0 typ
        let target =
            { Id = AST.functionIdForName name
              Name = name
              TypedParams = [value]
              ReturnType = typ
              ReturnOwnership = OwnedReturn
              Body = Return (Var value.Id) }
        let caller =
            { Id = AST.functionIdForName $"{name}Caller"
              Name = $"{name}Caller"
              TypedParams = []
              ReturnType = typ
              ReturnOwnership = OwnedReturn
              Body =
                Let (
                    TempId 1,
                    firstConstruction,
                    Let (
                        TempId 2,
                        Call (AST.functionIdForName name, [Var (TempId 1)]),
                        Let (
                            TempId 3,
                            secondConstruction,
                            Let (
                                TempId 4,
                                Call (AST.functionIdForName name, [Var (TempId 3)]),
                                Return (Var (TempId 4))
                            )
                        )
                    )
                ) }
        let (Program (functions, _)) =
            ANF_DirectCallSpecialization.specializeProgram
                (Program ([target; caller], Return UnitLiteral))
        let clones =
            functions
            |> List.filter (fun func -> func.Name.StartsWith($"{name}__literal_"))
        if List.length clones = 2
           && (clones |> List.forall (fun func -> List.isEmpty func.TypedParams)) then Ok ()
        else Error $"Expected two construction-specialized clones for {name}, found {List.length clones}"
    let int128Result =
        checkCase
            "wideValue"
            AST.TInt128
            (Call (AST.functionIdForName "Darklang.Stdlib.Int128.__fromWords", [IntLiteral (UInt64 1UL); IntLiteral (UInt64 0UL)]))
            (Call (AST.functionIdForName "Darklang.Stdlib.Int128.__fromWords", [IntLiteral (UInt64 2UL); IntLiteral (UInt64 0UL)]))
    let uint128Result =
        Result.bind
            (fun () ->
                checkCase
                    "unsignedWideValue"
                    AST.TUInt128
                    (Call (AST.functionIdForName "Darklang.Stdlib.UInt128.__fromWords", [IntLiteral (UInt64 3UL); IntLiteral (UInt64 0UL)]))
                    (Call (AST.functionIdForName "Darklang.Stdlib.UInt128.__fromWords", [IntLiteral (UInt64 4UL); IntLiteral (UInt64 0UL)])))
            int128Result
    let tupleResult =
        Result.bind
            (fun () ->
                checkCase
                    "tupleValue"
                    (AST.TTuple [AST.TInt64; AST.TBool])
                    (TupleAlloc [intAtom 1L; BoolLiteral true])
                    (TupleAlloc [intAtom 2L; BoolLiteral false]))
            uint128Result
    let descriptor =
        { SourceTypeName = "SmallRecord"
          RuntimeTypeName = "SmallRecord"
          TypeArgs = []
          Fields = [("count", AST.TInt64); ("enabled", AST.TBool)]
          ValueType = AST.TRecord ("SmallRecord", []) }
    Result.bind
        (fun () ->
            checkCase
                "recordValue"
                (AST.TRecord ("SmallRecord", []))
                (RecordAlloc (descriptor, [intAtom 1L; BoolLiteral true]))
                (RecordAlloc (descriptor, [intAtom 2L; BoolLiteral false])))
        tupleResult

let testThreeFieldAggregatesSpecializeAndPruneCallerConstruction () : TestResult =
    let checkCase name typ firstConstruction secondConstruction =
        let value = typedParam 0 typ
        let target =
            { Id = AST.functionIdForName name
              Name = name
              TypedParams = [value]
              ReturnType = typ
              ReturnOwnership = OwnedReturn
              Body = Return (Var value.Id) }
        let callerName = $"{name}Caller"
        let caller =
            { Id = AST.functionIdForName callerName
              Name = callerName
              TypedParams = []
              ReturnType = typ
              ReturnOwnership = OwnedReturn
              Body =
                Let (
                    TempId 1,
                    firstConstruction,
                    Let (
                        TempId 2,
                        Call (AST.functionIdForName name, [Var (TempId 1)]),
                        Let (
                            TempId 3,
                            secondConstruction,
                            Let (
                                TempId 4,
                                Call (AST.functionIdForName name, [Var (TempId 3)]),
                                Return (Var (TempId 4))
                            )
                        )
                    )
                ) }
        let (Program (functions, _)) =
            ANF_DirectCallSpecialization.specializeProgram
                (Program ([target; caller], Return UnitLiteral))
        let clones =
            functions
            |> List.filter (fun func -> func.Name.StartsWith($"{name}__literal_"))
        match functionByName callerName functions with
        | Some rewrittenCaller
            when List.length clones = 2
                 && (clones
                     |> List.forall (fun specialized ->
                         List.isEmpty specialized.TypedParams
                         && containsAggregateAllocation specialized.Body
                         && Option.isSome (directCallArgs specialized.Name rewrittenCaller.Body)))
                 && not (containsAggregateAllocation rewrittenCaller.Body) -> Ok ()
        | _ -> Error $"Expected three-field {name} specialization to rematerialize only inside the clone"
    let tupleResult =
        checkCase
            "tuple3Value"
            (AST.TTuple [AST.TInt64; AST.TBool; AST.TUInt64])
            (TupleAlloc [intAtom 1L; BoolLiteral true; IntLiteral (UInt64 2UL)])
            (TupleAlloc [intAtom 3L; BoolLiteral false; IntLiteral (UInt64 4UL)])
    let descriptor =
        { SourceTypeName = "ThreeFieldRecord"
          RuntimeTypeName = "ThreeFieldRecord"
          TypeArgs = []
          Fields = [("count", AST.TInt64); ("enabled", AST.TBool); ("tag", AST.TUInt64)]
          ValueType = AST.TRecord ("ThreeFieldRecord", []) }
    Result.bind
        (fun () ->
            checkCase
                "record3Value"
                (AST.TRecord ("ThreeFieldRecord", []))
                (RecordAlloc (descriptor, [intAtom 1L; BoolLiteral true; IntLiteral (UInt64 2UL)]))
                (RecordAlloc (descriptor, [intAtom 3L; BoolLiteral false; IntLiteral (UInt64 4UL)])))
        tupleResult

let testFloatLiteralKeysPreserveDistinctBitPatterns () : TestResult =
    let value = typedParam 0 AST.TFloat64
    let target =
        { Id = AST.functionIdForName "floatBits"
          Name = "floatBits"
          TypedParams = [value]
          ReturnType = AST.TFloat64
          ReturnOwnership = OwnedReturn
          Body = Return (Var value.Id) }
    let caller =
        { Id = AST.functionIdForName "caller"
          Name = "caller"
          TypedParams = []
          ReturnType = AST.TFloat64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Call (AST.functionIdForName "floatBits", [FloatLiteral 0.0]),
                Let (
                    TempId 2,
                    Call (AST.functionIdForName "floatBits", [FloatLiteral -0.0]),
                    Return (Var (TempId 2))
                )
            ) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([target; caller], Return UnitLiteral))
    let clones =
        functions
        |> List.filter (fun func -> func.Name.StartsWith("floatBits__literal_"))
    if List.length clones = 2
       && (clones |> List.forall (fun func -> List.isEmpty func.TypedParams)) then Ok ()
    else Error "Expected +0.0 and -0.0 to retain distinct literal clone keys"

let testLiteralCloneCountIsCapped () : TestResult =
    let constant = param 0
    let target =
        { Id = AST.functionIdForName "capped"
          Name = "capped"
          TypedParams = [constant]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (Var constant.Id) }
    let rec callEach values nextId body =
        match values with
        | [] -> body
        | value :: rest ->
            Let (
                TempId nextId,
                Call (AST.functionIdForName "capped", [intAtom value]),
                callEach rest (nextId + 1) body
            )
    let caller =
        { Id = AST.functionIdForName "caller"
          Name = "caller"
          TypedParams = []
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = callEach [1L; 2L; 3L; 4L; 5L] 1 (Return (intAtom 0L)) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([target; caller], Return UnitLiteral))
    let clones =
        functions
        |> List.filter (fun func -> func.Name.StartsWith("capped__literal_"))
    match functionByName "caller" functions with
    | Some caller'
        when List.length clones = 4
             && directCallArgs "capped" caller'.Body = Some [intAtom 5L] -> Ok ()
    | _ -> Error $"Expected four clones and a fifth-call fallback, found {List.length clones} clones"

let testSixteenCloneProgramBoundarySpecializes () : TestResult =
    let targets =
        [0 .. 3]
        |> List.map (fun index ->
            let name = $"programBoundary{index}"
            let value = param index
            { Id = AST.functionIdForName name
              Name = name
              TypedParams = [value]
              ReturnType = AST.TInt64
              ReturnOwnership = OwnedReturn
              Body = Return (Var value.Id) })
    let calls =
        targets
        |> List.collect (fun target ->
            [1L .. 4L]
            |> List.map (fun value -> (target.Id, intAtom value)))
    let main =
        List.foldBack
            (fun (index, (target, argument)) body ->
                Let (TempId (100 + index), Call (target, [argument]), body))
            (calls |> List.indexed)
            (Return (intAtom 0L))
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram (Program (targets, main))
    let clones =
        functions
        |> List.filter (fun func -> func.Name.Contains("__literal_"))
    if List.length clones = 16 && (clones |> List.forall (fun func -> List.isEmpty func.TypedParams)) then Ok ()
    else Error $"Expected all sixteen allowed program-level clones, found {List.length clones}"

let testSpecializedRecursiveSignaturesReachMirAndLir () : TestResult =
    let counter = param 0
    let mode = param 1
    let accumulator = param 2
    let loop =
        { Id = AST.functionIdForName "pipelineLoop"
          Name = "pipelineLoop"
          TypedParams = [counter; mode; accumulator]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 3,
                Prim (Sub, Var counter.Id, intAtom 1L),
                Let (
                    TempId 4,
                    Prim (Add, Var accumulator.Id, Var mode.Id),
                    Let (
                        TempId 5,
                        TailCall (AST.functionIdForName "pipelineLoop", [Var (TempId 3); Var mode.Id; Var (TempId 4)]),
                        Return (Var (TempId 5))
                    )
                )
            ) }
    let main =
        Let (
            TempId 6,
            Call (AST.functionIdForName "pipelineLoop", [intAtom 10L; intAtom 2L; intAtom 0L]),
            Let (
                TempId 7,
                Call (AST.functionIdForName "pipelineLoop", [intAtom 10L; intAtom 3L; intAtom 0L]),
                Return (Var (TempId 7))
            )
        )
    let specialized =
        ANF_DirectCallSpecialization.specializeProgram (Program ([loop], main))
    let typeMap =
        [0..7]
        |> List.map (fun id -> (TempId id, AST.TInt64))
        |> Map.ofList
    match
        ANF_to_MIR.toMIR
            specialized
            typeMap
            Map.empty
            AST.TInt64
            Map.empty
            Map.empty
            false
            Map.empty
    with
    | Error err -> Error $"Expected specialized ANF to lower to MIR: {err}"
    | Ok (MIR.Program (mirFunctions, _, _) as mirProgram) ->
        let mirClones =
            mirFunctions
            |> List.filter (fun func -> func.Name.StartsWith("pipelineLoop__literal_"))
        let validMir =
            mirClones
            |> List.forall (fun func -> List.length func.TypedParams = 2)
        if List.length mirClones <> 2 || not validMir then
            Error $"Expected two reduced-signature recursive clones in MIR:\n{MIRPrinter.formatMIR mirProgram}"
        else
            match MIR_to_LIR.toLIR mirProgram with
            | Error err -> Error $"Expected specialized MIR to lower to LIR: {err}"
            | Ok (LIR.Program (lirFunctions, _, _)) ->
                let lirClones =
                    lirFunctions
                    |> List.filter (fun func -> func.Name.StartsWith("pipelineLoop__literal_"))
                let validLir =
                    lirClones
                    |> List.forall (fun func -> List.length func.TypedParams = 2)
                if List.length lirClones = 2 && validLir then Ok ()
                else Error "Expected two reduced-signature recursive clones in LIR"

let testAddressTakenAndClosureTargetsAreExcluded () : TestResult =
    let addressParam = param 0
    let closureParam = param 1
    let addressTaken =
        { Id = AST.functionIdForName "addressTaken"
          Name = "addressTaken"
          TypedParams = [addressParam]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (intAtom 1L) }
    let closureTarget =
        { Id = AST.functionIdForName "closureTarget"
          Name = "closureTarget"
          TypedParams = [closureParam]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (intAtom 2L) }
    let observer =
        { Id = AST.functionIdForName "observer"
          Name = "observer"
          TypedParams = []
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 2,
                Atom (FuncRef (AST.functionIdForName "addressTaken")),
                Let (
                    TempId 3,
                    ClosureAlloc (AST.functionIdForName "closureTarget", []),
                    Let (
                        TempId 4,
                        Call (AST.functionIdForName "addressTaken", [intAtom 7L]),
                        Let (
                            TempId 5,
                            Call (AST.functionIdForName "closureTarget", [intAtom 7L]),
                            Return (Var (TempId 5))
                        )
                    )
                )
            ) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([addressTaken; closureTarget; observer], Return UnitLiteral))
    match expectArity "addressTaken" 1 functions with
    | Error err -> Error err
    | Ok () ->
        match expectArity "closureTarget" 1 functions with
        | Error err -> Error err
        | Ok () ->
            match expectDirectCallArity "observer" "addressTaken" 1 functions with
            | Error err -> Error err
            | Ok () -> expectDirectCallArity "observer" "closureTarget" 1 functions

let tests = [
    ("Uniform literals rewrite recursive direct-call groups", testUniformLiteralParametersRewriteRecursiveGroup)
    ("Differing literals retain unspecialized fallback", testDifferingLiteralsRetainUnspecializedFallback)
    ("Finite scalar literals create bounded clones", testFiniteScalarLiteralsCreateBoundedClones)
    ("Recursive clones keep tail calls and reduced signatures", testRecursiveCloneKeepsTailCallAndReducedSignature)
    ("Uniform string literals are specialized", testUniformStringLiteralIsSpecialized)
    ("Immediate semantic values create clones", testImmediateSemanticValuesCreateClones)
    ("Known indirect targets become specializable", testKnownIndirectTargetBecomesSpecializable)
    ("Mismatched bottom placeholders do not seed clones", testMismatchedBottomPlaceholderDoesNotSeedClone)
    ("Construction values create clones", testConstructionValuesCreateClones)
    ("Three-field aggregates specialize and prune caller construction", testThreeFieldAggregatesSpecializeAndPruneCallerConstruction)
    ("Float literal keys preserve distinct bit patterns", testFloatLiteralKeysPreserveDistinctBitPatterns)
    ("Literal clone count is capped", testLiteralCloneCountIsCapped)
    ("Sixteen-clone program boundary specializes", testSixteenCloneProgramBoundarySpecializes)
    ("Specialized recursive signatures reach MIR and LIR", testSpecializedRecursiveSignaturesReachMirAndLir)
    ("Address-taken and closure targets are excluded", testAddressTakenAndClosureTargetsAreExcluded)
]
