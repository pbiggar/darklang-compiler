// ARM64CodeGenTests.fs - Unit tests for ARM64 code generation from LIR.
//
// These tests inspect symbolic ARM64 instructions for ownership-sensitive
// lowering decisions that do not need a full executable harness.

module ARM64CodeGenTests

type TestResult = Result<unit, string>

let private rcMetadata (typ: AST.Type) : ANF.RcMetadata =
    {
        ANF.ReleasePlan = Some (ANF.rcReleasePlanOfTypeWithSums Map.empty Map.empty typ)
        ANF.SourceType = Some typ
    }

let private makeSimpleProgramWithVariants
    (instrs: LIR.Instr list)
    (variants: LIR.VariantRegistry)
    : LIR.Program =
    let label = LIR.Label "_start_entry"
    let block : LIR.BasicBlock = {
        Label = label
        Instrs = instrs
        Terminator = LIR.Ret
    }
    let func : LIR.Function = {
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, block)]
        }
        StackSize = 0
        UsedCalleeSaved = []
    }
    LIR.Program ([func], variants, Map.empty)

let testRawSetPureEnumDoesNotEmitGenericRetain () : TestResult =
    let enumType = AST.TSum ("RawSetPureEnum", [AST.TString])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            ("RawSetPureEnum",
                { TypeParams = ["a"]
                  Variants =
                    [
                        { Name = "RawSetPureA"; Tag = 0; Payload = None }
                        { Name = "RawSetPureB"; Tag = 1; Payload = None }
                    ] })
        ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RawSet (
                    LIR.Physical LIR.X0,
                    LIR.Physical LIR.X1,
                    LIR.Physical LIR.X3,
                    Some enumType)
            ]
            variants

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let emittedGenericRetain =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X15, ARM64.X3, 16s)
                | ARM64Symbolic.LDR (ARM64.X14, ARM64.X3, 16s) ->
                    true
                | _ ->
                    false)
        if emittedGenericRetain then
            Error "RawSet of a generic pure enum emitted a generic heap retain"
        else
            Ok ()

let testListTuple3BytesListDictListValueUsesTypedDictHelper () : TestResult =
    let tupleType = AST.TTuple [ AST.TBytes; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TList AST.TInt64) ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata (AST.TList tupleType)))
            ]
            Map.empty

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsTypedDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_dict_refcount_dec_list_value_helper" ->
                    true
                | _ ->
                    false)
        if callsTypedDictListHelper then
            Ok ()
        else
            Error "List of tuple(bytes, list, dict<int, list<int>>) did not emit typed dict-list value release helper"

let private assertListElementUsesTypedDictListHelper (elementType: AST.Type) (caseName: string) : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata (AST.TList elementType)))
            ]
            Map.empty

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsTypedDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_dict_refcount_dec_list_value_helper" ->
                    true
                | _ ->
                    false)
        if callsTypedDictListHelper then
            Ok ()
        else
            Error $"{caseName} did not emit typed dict-list value release helper"

let testListTuple3StringListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [ AST.TString; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TList AST.TInt64) ])
        "List of tuple(string, list, dict<int, list<int>>)"

let testListTuple3ClosureListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "List of tuple(closure, list, dict<int, list<int>>)"

let testListTuple4StringBytesListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TString
            AST.TBytes
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "List of tuple(string, bytes, list, dict<int, list<int>>)"

let testListTuple4ClosureStringListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TString
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "List of tuple(closure, string, list, dict<int, list<int>>)"

let testListTuple4ClosureBytesListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TBytes
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "List of tuple(closure, bytes, list, dict<int, list<int>>)"

let testListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TDict (AST.TInt64, AST.TList AST.TInt64))
        "List of dict<int, list<int>>"

let testListNestedTupleDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TString
            AST.TBytes
            AST.TTuple [ AST.TDict (AST.TInt64, AST.TList AST.TInt64); AST.TString ]
            AST.TList AST.TInt64
        ])
        "List of tuple(string, bytes, tuple(dict<int, list<int>>, string), list<int>)"

let tests : (string * (unit -> TestResult)) list = [
    ("RawSet pure enum skips generic retain", testRawSetPureEnumDoesNotEmitGenericRetain)
    ("List tuple3 bytes/list/dict-list uses typed dict helper", testListTuple3BytesListDictListValueUsesTypedDictHelper)
    ("List tuple3 string/list/dict-list uses typed dict helper", testListTuple3StringListDictListValueUsesTypedDictHelper)
    ("List tuple3 closure/list/dict-list uses typed dict helper", testListTuple3ClosureListDictListValueUsesTypedDictHelper)
    ("List tuple4 string/bytes/list/dict-list uses typed dict helper", testListTuple4StringBytesListDictListValueUsesTypedDictHelper)
    ("List tuple4 closure/string/list/dict-list uses typed dict helper", testListTuple4ClosureStringListDictListValueUsesTypedDictHelper)
    ("List tuple4 closure/bytes/list/dict-list uses typed dict helper", testListTuple4ClosureBytesListDictListValueUsesTypedDictHelper)
    ("List dict-list uses typed dict helper", testListDictListValueUsesTypedDictHelper)
    ("List nested tuple dict-list uses typed dict helper", testListNestedTupleDictListValueUsesTypedDictHelper)
]
