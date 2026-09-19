// CallTests.fs - Verify call-result ownership and canonical source metadata.

module RcCallTests

open MemoryModel
open ANF
open RcTypeFacts
open RcCleanup
open RefCountInsertion
open MemoryShapeTests
open RcCleanupTests

let private functionRegistry entries : TypeRegistries.FunctionRegistry =
    entries
    |> List.map (fun (name, typ) -> AST.functionIdForName name, (name, typ))
    |> Map.ofList

let testBorrowedCallMaterializesOwnedLocal () : TestResult =
    let nodeType = AST.TList AST.TInt64
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            ("consumer", AST.TFunction ([nodeType; AST.TInt64], AST.TInt64))
            ("Darklang.Stdlib.List.__node2GetChild_i64", AST.TFunction ([nodeType; AST.TInt64], nodeType))
            ("Darklang.Stdlib.List.__nodeMeasure_i64", AST.TFunction ([nodeType], AST.TInt64))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let nodeParam = TempId 0
    let indexParam = TempId 1
    let childTemp = TempId 2
    let measureTemp = TempId 3

    let func : Function = {
        Id = AST.functionIdForName "consumer"
        Name = "consumer"
        TypedParams = [
            { Id = nodeParam; Type = nodeType }
            { Id = indexParam; Type = AST.TInt64 }
        ]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                BorrowedCall (AST.functionIdForName "Darklang.Stdlib.List.__node2GetChild_i64", [Var nodeParam; Var indexParam]),
                Let (
                    measureTemp,
                    Call (AST.functionIdForName "Darklang.Stdlib.List.__nodeMeasure_i64", [Var childTemp]),
                    Return (Var measureTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if not (hasRefCountIncForTemp childTemp transformed.Body) then
        Error "BorrowedCall must retain its borrowed result when materializing a local value"
    elif not (hasRefCountDecForTemp childTemp transformed.Body) then
        Error "Materialized BorrowedCall local must release its retained ownership edge"
    else
        Ok ()

let testReturnedBorrowedCallMaterializesOwnership () : TestResult =
    let nodeType = AST.TList AST.TInt64
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            ("project", AST.TFunction ([nodeType], nodeType))
            ("borrowChild", AST.TFunction ([nodeType], nodeType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let nodeParam = TempId 0
    let childTemp = TempId 1
    let func : Function = {
        Id = AST.functionIdForName "project"
        Name = "project"
        TypedParams = [
            { Id = nodeParam; Type = nodeType }
        ]
        ReturnType = nodeType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                BorrowedCall (AST.functionIdForName "borrowChild", [Var nodeParam]),
                Return (Var childTemp)
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp childTemp transformed.Body then
        Ok ()
    else
        Error "Returning a BorrowedCall result must retain it to materialize owned return storage"

let testCallReturningClosureGetsAutoDecAfterUse () : TestResult =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            ("makeClosure", AST.TFunction ([], closureType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let closureTemp = TempId 0
    let resultTemp = TempId 1
    let func : Function = {
        Id = AST.functionIdForName "caller"
        Name = "caller"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                closureTemp,
                Call (AST.functionIdForName "makeClosure", []),
                Let (
                    resultTemp,
                    ClosureCall (Var closureTemp, [IntLiteral (Int64 5L)]),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp closureTemp transformed.Body then
        Ok ()
    else
        Error "Call result with function type should receive automatic closure RefCountDec after use"

let testClosureCallReturningClosureGetsAutoDecAfterUse () : TestResult =
    let returnedClosureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let makerClosureType = AST.TFunction ([AST.TInt64], returnedClosureType)
    let funcReg : TypeRegistries.FunctionRegistry =
        functionRegistry [
            ("makeClosure", makerClosureType)
            ("returnedClosure", returnedClosureType)
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let makerTemp = TempId 0
    let returnedTemp = TempId 1
    let resultTemp = TempId 2
    let func : Function = {
        Id = AST.functionIdForName "caller"
        Name = "caller"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                makerTemp,
                ClosureAlloc (AST.functionIdForName "makeClosure", []),
                Let (
                    returnedTemp,
                    ClosureCall (Var makerTemp, [IntLiteral (Int64 5L)]),
                    Let (
                        resultTemp,
                        Atom (IntLiteral (Int64 0L)),
                        Return (Var resultTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp returnedTemp transformed.Body then
        Ok ()
    else
        Error "ClosureCall result with function return type should receive automatic closure RefCountDec after use"

let testPureEnumBindingDoesNotGetAutomaticDec () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg =
            Map.ofList [
                ("Color", { TypeParams = []; Payloads = [0, None; 1, None] })
            ]
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let enumTemp = TempId 0
    let resultTemp = TempId 1
    let enumType = AST.TSum ("Color", [])
    let func : Function = {
        Id = AST.functionIdForName "pureEnumBinding"
        Name = "pureEnumBinding"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                enumTemp,
                TypedAtom (IntLiteral (Int64 0L), enumType),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp enumTemp transformed.Body then
        Error "Pure enum binding should classify as immediate and must not get automatic RefCountDec"
    else
        Ok ()

let testGenericPureEnumBindingDoesNotGetAutomaticDec () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup =
            Map.ofList [
                ("Left", ("Phantom", ["a"], 0, []))
                ("Right", ("Phantom", ["a"], 1, []))
            ]
        SumShapeReg =
            Map.ofList [
                ("Phantom", { TypeParams = ["a"]; Payloads = [0, None; 1, None] })
            ]
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let enumTemp = TempId 0
    let resultTemp = TempId 1
    let enumType = AST.TSum ("Phantom", [AST.TString])
    let func : Function = {
        Id = AST.functionIdForName "genericPureEnumBinding"
        Name = "genericPureEnumBinding"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                enumTemp,
                TypedAtom (IntLiteral (Int64 0L), enumType),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp enumTemp transformed.Body then
        Error "Generic pure enum binding should classify from variant metadata and must not get automatic RefCountDec"
    else
        Ok ()

let testProgramRcFreshTempsFollowExistingProgramTemps () : TestResult =
    let lowTemp = TempId 1000
    let highTemp = TempId 6000
    let func : Function = {
        Id = AST.functionIdForName "freshTempBoundary"
        Name = "freshTempBoundary"
        TypedParams = []
        ReturnType = AST.TUnit
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                lowTemp,
                Atom (IntLiteral (Int64 0L)),
                Let (
                    highTemp,
                    Call (AST.functionIdForName "makeString", []),
                    Return UnitLiteral
                )
            )
    }
    let conversion : AST_to_ANF.ConversionResult = {
        Program = Program ([func], Return UnitLiteral)
        OwnershipContracts = Map.empty
        RecursiveMembers = Map.empty
        TypeReg = Map.empty
        RecordFieldsReg = Map.empty
        RecordTypeParamsReg = Map.empty
        VariantLookup = Map.empty
        RcSumShapeReg = Map.empty
        FuncReg = functionRegistry [("makeString", AST.TFunction ([], AST.TString))]
        FuncParams = Map.empty
        ModuleRegistry = Map.empty
    }

    let rec definedTemps (expr: AExpr) : TempId list =
        match expr with
        | Jump _ | Return _ -> []
        | Let (tempId, _, body) -> tempId :: definedTemps body
        | Join (_, thenBranch, elseBranch)
        | If (_, thenBranch, elseBranch) ->
            definedTemps thenBranch @ definedTemps elseBranch

    match insertRCInProgram conversion with
    | Error err -> Error $"Expected RC insertion to succeed, got {err}"
    | Ok (Program ([transformed], _), _) ->
        let definitions = definedTemps transformed.Body
        let distinctDefinitions = Set.ofList definitions
        let greatestDefinition =
            definitions
            |> List.map (fun (TempId tempId) -> tempId)
            |> List.max
        if Set.count distinctDefinitions <> List.length definitions then
            Error $"RC insertion reused an existing TempId: {definitions}"
        elif greatestDefinition <= 6000 then
            Error $"Expected an RC temporary after t6000, greatest was t{greatestDefinition}"
        else
            Ok ()
    | Ok _ -> Error "Expected the transformed program to contain one function"

let testProgramRcRejectsDriftedOwnershipContract () : TestResult =
    let functionId = AST.functionIdForName "driftedOwnership"
    let func : Function = {
        Id = functionId
        Name = "driftedOwnership"
        TypedParams = [{ Id = TempId 0; Type = AST.TInt64 }]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body = Return (Var (TempId 0))
    }
    let conversion : AST_to_ANF.ConversionResult = {
        Program = Program ([func], Return UnitLiteral)
        OwnershipContracts =
            Map.ofList [
                functionId,
                ({
                    Parameters = [OwnedIR.UniqueCallParameter]
                    Result = OwnedIR.UnmanagedCallResult
                 }: OwnedIR.CallSignature)
            ]
        RecursiveMembers = Map.empty
        TypeReg = Map.empty
        RecordFieldsReg = Map.empty
        RecordTypeParamsReg = Map.empty
        VariantLookup = Map.empty
        RcSumShapeReg = Map.empty
        FuncReg = functionRegistry [("driftedOwnership", AST.TFunction ([AST.TInt64], AST.TInt64))]
        FuncParams = Map.empty
        ModuleRegistry = Map.empty
    }
    match insertRCInProgram conversion with
    | Error message when message.Contains("Ownership contract parameter representation changed") -> Ok ()
    | actual -> Error $"Expected RC insertion to reject ownership-contract drift, got {actual}"

let testBareSumTypeRefsAreCanonicalizedForRcSourceTypes () : TestResult =
    let payloadType = AST.TRecord ("Payload", [])
    let dictType = AST.TDict (AST.TInt64, payloadType)
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup =
            Map.ofList [
                ("Empty", ("Payload", [], 0, []))
                ("SomePayload", ("Payload", [], 1, [AST.TString]))
            ]
        SumShapeReg =
            Map.ofList [
                ("Payload", { TypeParams = []; Payloads = [0, None; 1, Some AST.TString] })
            ]
        FuncReg =
            functionRegistry [
                ("mkDict", AST.TFunction ([], dictType))
            ]
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let dictTemp = TempId 0
    let resultTemp = TempId 1
    let func : Function = {
        Id = AST.functionIdForName "canonicalBareSum"
        Name = "canonicalBareSum"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                dictTemp,
                Call (AST.functionIdForName "mkDict", []),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen
    match tryRefCountDecSourceTypeForTemp dictTemp transformed.Body with
    | Some (AST.TDict (AST.TInt64, AST.TSum ("Payload", []))) ->
        Ok ()
    | Some other ->
        Error $"Expected dict dec source type to canonicalize Payload as a sum, got {other}"
    | None ->
        Error "Expected dict binding to receive automatic RefCountDec"
