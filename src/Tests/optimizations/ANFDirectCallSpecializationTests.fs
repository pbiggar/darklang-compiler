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

let testDifferingLiteralsRetainParameter () : TestResult =
    let constant = param 0
    let target =
        { Name = "target"
          TypedParams = [constant]
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = Return (Var constant.Id) }
    let caller =
        { Name = "caller"
          TypedParams = []
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body =
            Let (
                TempId 1,
                Call ("target", [intAtom 7L]),
                Let (
                    TempId 2,
                    Call ("target", [intAtom 8L]),
                    Return (Var (TempId 2))
                )
            ) }
    let (Program (functions, _)) =
        ANF_DirectCallSpecialization.specializeProgram
            (Program ([target; caller], Return UnitLiteral))
    match expectArity "target" 1 functions with
    | Error err -> Error err
    | Ok () -> expectDirectCallArity "caller" "target" 1 functions

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
    ("Differing literals retain direct-call parameters", testDifferingLiteralsRetainParameter)
    ("Address-taken and closure targets are excluded", testAddressTakenAndClosureTargetsAreExcluded)
]
