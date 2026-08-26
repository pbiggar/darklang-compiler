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
    | Return _ -> None
    | Let (_, Call (name, args), _) when name = target -> Some args
    | Let (_, BorrowedCall (name, args), _) when name = target -> Some args
    | Let (_, TailCall (name, args), _) when name = target -> Some args
    | Let (_, _, body) -> directCallArgs target body
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
    | Let (_, cexpr, body) -> cexprContains cexpr || containsAtom expected body
    | If (condition, thenBranch, elseBranch) ->
        condition = expected
        || containsAtom expected thenBranch
        || containsAtom expected elseBranch

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
        { Name = "f"
          TypedParams = [fValue; fConstant]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 4,
                Prim (Add, Var fValue.Id, Var fConstant.Id),
                Let (
                    TempId 5,
                    Call ("g", [Var (TempId 4); intAtom 7L]),
                    Return (Var (TempId 5))
                )
            ) }
    let g =
        { Name = "g"
          TypedParams = [gValue; gConstant]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 6,
                Prim (Sub, Var gValue.Id, Var gConstant.Id),
                Let (
                    TempId 7,
                    TailCall ("f", [Var (TempId 6); intAtom 7L]),
                    Return (Var (TempId 7))
                )
            ) }
    let main =
        Let (TempId 8, Call ("f", [intAtom 1L; intAtom 7L]), Return (Var (TempId 8)))
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
        { Name = "target"
          TypedParams = [constant]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (Var constant.Id) }
    let caller =
        { Name = "caller"
          TypedParams = [dynamic]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Call ("target", [intAtom 7L]),
                Let (
                    TempId 2,
                    Call ("target", [intAtom 8L]),
                    Let (
                        TempId 4,
                        TailCall ("target", [Var dynamic.Id]),
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
        { Name = "target"
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
        { Name = "caller"
          TypedParams = []
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 2,
                Call ("target", [intAtom 7L]),
                Let (
                    TempId 3,
                    TailCall ("target", [intAtom 8L]),
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
        { Name = "loop"
          TypedParams = [value; mode]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 2,
                Prim (Sub, Var value.Id, intAtom 1L),
                Let (
                    TempId 3,
                    TailCall ("loop", [Var (TempId 2); Var mode.Id]),
                    Return (Var (TempId 3))
                )
            ) }
    let main =
        Let (
            TempId 4,
            Call ("loop", [intAtom 10L; intAtom 1L]),
            Let (
                TempId 5,
                Call ("loop", [intAtom 10L; intAtom 2L]),
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

let testManagedLiteralsAreNotSpecialized () : TestResult =
    let text = typedParam 0 AST.TString
    let target =
        { Name = "managed"
          TypedParams = [text]
          ReturnType = AST.TString
          ReturnOwnership = OwnedReturn
          Body = Return (Var text.Id) }
    let caller =
        { Name = "caller"
          TypedParams = []
          ReturnType = AST.TString
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Call ("managed", [StringLiteral "first"]),
                Let (
                    TempId 2,
                    Call ("managed", [StringLiteral "second"]),
                    Return (Var (TempId 2))
                )
            ) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([target; caller], Return UnitLiteral))
    let clones =
        functions
        |> List.filter (fun func -> func.Name.StartsWith("managed__literal_"))
    match expectArity "managed" 1 functions with
    | Error err -> Error err
    | Ok () when List.isEmpty clones -> expectDirectCallArity "caller" "managed" 1 functions
    | Ok () -> Error "Expected managed string literals to retain the original calling convention"

let testFloatLiteralKeysPreserveDistinctBitPatterns () : TestResult =
    let value = typedParam 0 AST.TFloat64
    let target =
        { Name = "floatBits"
          TypedParams = [value]
          ReturnType = AST.TFloat64
          ReturnOwnership = OwnedReturn
          Body = Return (Var value.Id) }
    let caller =
        { Name = "caller"
          TypedParams = []
          ReturnType = AST.TFloat64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Call ("floatBits", [FloatLiteral 0.0]),
                Let (
                    TempId 2,
                    Call ("floatBits", [FloatLiteral -0.0]),
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
        { Name = "capped"
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
                Call ("capped", [intAtom value]),
                callEach rest (nextId + 1) body
            )
    let caller =
        { Name = "caller"
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

let testSpecializedRecursiveSignaturesReachMirAndLir () : TestResult =
    let counter = param 0
    let mode = param 1
    let accumulator = param 2
    let loop =
        { Name = "pipelineLoop"
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
                        TailCall ("pipelineLoop", [Var (TempId 3); Var mode.Id; Var (TempId 4)]),
                        Return (Var (TempId 5))
                    )
                )
            ) }
    let main =
        Let (
            TempId 6,
            Call ("pipelineLoop", [intAtom 10L; intAtom 2L; intAtom 0L]),
            Let (
                TempId 7,
                Call ("pipelineLoop", [intAtom 10L; intAtom 3L; intAtom 0L]),
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
            Error $"Expected two reduced-signature recursive clones in MIR:\n{IRPrinter.formatMIR mirProgram}"
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
        { Name = "addressTaken"
          TypedParams = [addressParam]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (intAtom 1L) }
    let closureTarget =
        { Name = "closureTarget"
          TypedParams = [closureParam]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (intAtom 2L) }
    let observer =
        { Name = "observer"
          TypedParams = []
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 2,
                Atom (FuncRef "addressTaken"),
                Let (
                    TempId 3,
                    ClosureAlloc ("closureTarget", []),
                    Let (
                        TempId 4,
                        Call ("addressTaken", [intAtom 7L]),
                        Let (
                            TempId 5,
                            Call ("closureTarget", [intAtom 7L]),
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
    ("Managed literals are not specialized", testManagedLiteralsAreNotSpecialized)
    ("Float literal keys preserve distinct bit patterns", testFloatLiteralKeysPreserveDistinctBitPatterns)
    ("Literal clone count is capped", testLiteralCloneCountIsCapped)
    ("Specialized recursive signatures reach MIR and LIR", testSpecializedRecursiveSignaturesReachMirAndLir)
    ("Address-taken and closure targets are excluded", testAddressTakenAndClosureTargetsAreExcluded)
]
