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

let private rcMetadataWithSumShapes (sumShapes: ANF.RcSumShapeRegistry) (typ: AST.Type) : ANF.RcMetadata =
    {
        ANF.ReleasePlan = Some (ANF.rcReleasePlanOfTypeWithSums Map.empty sumShapes typ)
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

let private makeEmptyFunction
    (name: string)
    (typedParams: LIR.TypedLIRParam list)
    : LIR.Function =
    let label = LIR.Label $"{name}_entry"
    {
        Name = name
        TypedParams = typedParams
        CFG = {
            Entry = label
            Blocks = Map.ofList [
                label,
                {
                    Label = label
                    Instrs = []
                    Terminator = LIR.Ret
                }
            ]
        }
        StackSize = 0
        UsedCalleeSaved = []
    }

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

let testListTuple2NestedTupleDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TInt64
            AST.TTuple [
                AST.TString
                AST.TBytes
                AST.TList AST.TInt64
                AST.TDict (AST.TInt64, AST.TList AST.TInt64)
            ]
        ])
        "List of tuple(int, tuple(string, bytes, list<int>, dict<int, list<int>>))"

let testListTuple4NestedTupleDynamicDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TInt64
            AST.TInt64
            AST.TInt64
            AST.TTuple [
                AST.TString
                AST.TList AST.TInt64
                AST.TDict (AST.TInt64, AST.TList AST.TInt64)
            ]
        ])
        "List of tuple(int, int, int, tuple(string, list<int>, dict<int, list<int>>))"

let testListTuple4NestedTupleClosureDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TInt64
            AST.TInt64
            AST.TInt64
            AST.TTuple [
                AST.TFunction ([ AST.TInt64 ], AST.TInt64)
                AST.TString
                AST.TList AST.TInt64
                AST.TDict (AST.TInt64, AST.TList AST.TInt64)
            ]
        ])
        "List of tuple(int, int, int, tuple(closure, string, list<int>, dict<int, list<int>>))"

let private assertListSumPayloadUsesTypedDictListHelper (payloadType: AST.Type) (caseName: string) : TestResult =
    let sanitizedName =
        caseName
            .Replace(" ", "")
            .Replace(",", "")
            .Replace("(", "")
            .Replace(")", "")
            .Replace("<", "")
            .Replace(">", "")
            .Replace("-", "")
    let sumName = $"ARM64{sanitizedName}"
    let sumType = AST.TSum (sumName, [])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = $"{sumName}Case"; Tag = 0; Payload = Some payloadType }
                    ] })
        ]
    let sumShapes : ANF.RcSumShapeRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Payloads = [ 0, Some payloadType ] })
        ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadataWithSumShapes sumShapes (AST.TList sumType)))
            ]
            variants

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
            Error $"{caseName} sum payload did not emit typed dict-list value release helper"

let testListSumTuple3DictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [ AST.TString; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TList AST.TInt64) ])
        "sum tuple3 string list dict-list"

let testListSumTuple4DictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [ AST.TString; AST.TBytes; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TList AST.TInt64) ])
        "sum tuple4 string bytes list dict-list"

let testListSumTuple3ClosureDictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "sum tuple3 closure list dict-list"

let testListSumTuple4ClosureDictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TBytes
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "sum tuple4 closure bytes list dict-list"

let testListSumTuple4ClosureStringDictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TString
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "sum tuple4 closure string list dict-list"

let testDictDictListValueUsesTypedDictHelper () : TestResult =
    let dictType = AST.TDict (AST.TInt64, AST.TDict (AST.TInt64, AST.TList AST.TInt64))
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.DictHeap,
                    Some (rcMetadata dictType))
            ]
            Map.empty

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsTypedNestedDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_dict_refcount_dec_dict_list_value_helper" ->
                    true
                | _ ->
                    false)
        if callsTypedNestedDictListHelper then
            Ok ()
        else
            Error "Dict<int, dict<int, list<int>>> did not emit typed nested dict-list value release helper"

let testGenericFixedBlockNestedBytesFieldUsesReleasePlan () : TestResult =
    let nestedType = AST.TTuple [ AST.TBytes ]
    let parentType = AST.TTuple [ nestedType ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    8,
                    LIR.GenericHeap,
                    Some (rcMetadata parentType))
            ]
            Map.empty

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let releasesNestedBytesField =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X12, ARM64.X11, 0s) ->
                    true
                | _ ->
                    false)
        let preservesNestedBaseRegister =
            instrs
            |> List.exists (function
                | ARM64Symbolic.STP_pre (ARM64.X10, ARM64.X11, ARM64.SP, -48s) ->
                    true
                | _ ->
                    false)
        if not releasesNestedBytesField then
            Error "Generic fixed-block nested bytes field release did not consume the nested release plan"
        elif not preservesNestedBaseRegister then
            Error "Generic fixed-block nested release did not preserve X11 while using it as child base"
        else
            Ok ()

let testGenericFixedBlockNestedImmediateFieldReleasesChildRoot () : TestResult =
    let nestedType = AST.TTuple [ AST.TInt64 ]
    let parentType = AST.TTuple [ nestedType ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    8,
                    LIR.GenericHeap,
                    Some (rcMetadata parentType))
            ]
            Map.empty

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let releasesNestedRoot =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X12, ARM64.X0, 0s) ->
                    true
                | _ ->
                    false)
        if releasesNestedRoot then
            Ok ()
        else
            Error "Generic fixed-block nested immediate field release did not release the child root"

let testGenericFixedBlockNestedMixedBoxedSumBytesPayloadUsesVariantDispatch () : TestResult =
    let sumName = "Arm64NestedFixedBlockSumBytes"
    let sumType = AST.TSum (sumName, [])
    let parentType = AST.TTuple [ sumType ]
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "Arm64NestedFixedBlockNoPayload"; Tag = 0; Payload = None }
                        { Name = "Arm64NestedFixedBlockSumBytesPayload"; Tag = 1; Payload = Some AST.TBytes }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { ANF.TypeParams = typeVariants.TypeParams
              ANF.Payloads =
                typeVariants.Variants
                |> List.sortBy (fun variant -> variant.Tag)
                |> List.map (fun variant -> variant.Tag, variant.Payload) })
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    8,
                    LIR.GenericHeap,
                    Some (rcMetadataWithSumShapes sumShapes parentType))
            ]
            variants

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let loadsNestedSumTag =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X10, ARM64.X11, 0s) ->
                    true
                | _ ->
                    false)
        if loadsNestedSumTag then
            Ok ()
        else
            Error "Generic fixed-block nested mixed boxed-sum payload release did not dispatch on the child variant tag"

let testGenericMixedBoxedSumPayloadDispatchSkipsRemainingCases () : TestResult =
    let sumName = "Arm64MixedSumPayloadDispatch"
    let sumType = AST.TSum (sumName, [])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "Arm64MixedSumBytesPayload"; Tag = 0; Payload = Some AST.TBytes }
                        { Name = "Arm64MixedSumListPayload"; Tag = 1; Payload = Some (AST.TList AST.TInt64) }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { ANF.TypeParams = typeVariants.TypeParams
              ANF.Payloads =
                typeVariants.Variants
                |> List.sortBy (fun variant -> variant.Tag)
                |> List.map (fun variant -> variant.Tag, variant.Payload) })
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    16,
                    LIR.GenericHeap,
                    Some (rcMetadataWithSumShapes sumShapes sumType))
            ]
            variants

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let rec branchAppearsBeforeSecondCase (seenFirstCase: bool) (remaining: ARM64Symbolic.Instr list) : bool =
            match remaining with
            | [] ->
                false
            | ARM64Symbolic.CMP_imm (ARM64.X10, 0us) :: rest ->
                branchAppearsBeforeSecondCase true rest
            | ARM64Symbolic.CMP_imm (ARM64.X10, 1us) :: _ when seenFirstCase ->
                false
            | ARM64Symbolic.B _ :: _ when seenFirstCase ->
                true
            | _ :: rest ->
                branchAppearsBeforeSecondCase seenFirstCase rest

        let emitsBranchAfterMatchedPayload =
            branchAppearsBeforeSecondCase false instrs

        if emitsBranchAfterMatchedPayload then
            Ok ()
        else
            Error "Generic mixed boxed-sum payload release did not branch past remaining variant cases after a match"

let testClosureCaptureNestedFixedBlockBytesFieldUsesReleasePlan () : TestResult =
    let nestedType = AST.TTuple [ AST.TBytes ]
    let captureType = AST.TTuple [ nestedType ]
    let closureParamType = AST.TTuple [ AST.TInt64; captureType ]
    let capturedFunc =
        makeEmptyFunction
            "arm64_nested_tuple_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureParamType }]
    let main =
        match
            makeSimpleProgramWithVariants
                [
                    LIR.ClosureAlloc (
                        LIR.Physical LIR.X1,
                        "arm64_nested_tuple_capture_fn",
                        [LIR.Reg (LIR.Physical LIR.X2)])
                    LIR.RefCountDec (
                        LIR.Physical LIR.X1,
                        16,
                        LIR.ClosureHeap,
                        Some (rcMetadata (AST.TFunction ([AST.TInt64], AST.TInt64))))
                ]
                Map.empty
        with
        | LIR.Program ([func], variants, records) ->
            LIR.Program ([func; capturedFunc], variants, records)
        | other ->
            other

    match CodeGen.generateARM64 main with
    | Error e ->
        Error e
    | Ok instrs ->
        let releasesNestedBytesField =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X12, ARM64.X11, 0s) ->
                    true
                | _ ->
                    false)
        if releasesNestedBytesField then
            Ok ()
        else
            Error "Closure capture nested fixed-block bytes field release did not consume the nested release plan"

let testClosureCaptureBoxedSumBytesPayloadUsesReleasePlan () : TestResult =
    let sumName = "Arm64ClosureCaptureSumBytes"
    let sumType = AST.TSum (sumName, [])
    let closureParamType = AST.TTuple [ AST.TInt64; sumType ]
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "Arm64ClosureCaptureSumBytesPayload"; Tag = 0; Payload = Some AST.TBytes }
                    ] })
        ]
    let capturedFunc =
        makeEmptyFunction
            "arm64_sum_bytes_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureParamType }]
    let main =
        match
            makeSimpleProgramWithVariants
                [
                    LIR.ClosureAlloc (
                        LIR.Physical LIR.X1,
                        "arm64_sum_bytes_capture_fn",
                        [LIR.Reg (LIR.Physical LIR.X2)])
                    LIR.RefCountDec (
                        LIR.Physical LIR.X1,
                        16,
                        LIR.ClosureHeap,
                        Some (rcMetadata (AST.TFunction ([AST.TInt64], AST.TInt64))))
                ]
                variants
        with
        | LIR.Program ([func], programVariants, records) ->
            LIR.Program ([func; capturedFunc], programVariants, records)
        | other ->
            other

    match CodeGen.generateARM64 main with
    | Error e ->
        Error e
    | Ok instrs ->
        let releasesSumBytesPayload =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X12, ARM64.X8, 8s) ->
                    true
                | _ ->
                    false)
        if releasesSumBytesPayload then
            Ok ()
        else
            Error "Closure capture boxed-sum bytes payload release did not consume the variant release plan"

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
    ("List tuple2 nested tuple dict-list uses typed dict helper", testListTuple2NestedTupleDictListValueUsesTypedDictHelper)
    ("List tuple4 nested tuple dynamic dict-list uses typed dict helper", testListTuple4NestedTupleDynamicDictListValueUsesTypedDictHelper)
    ("List tuple4 nested tuple closure dict-list uses typed dict helper", testListTuple4NestedTupleClosureDictListValueUsesTypedDictHelper)
    ("List sum tuple3 dict-list uses typed dict helper", testListSumTuple3DictListValueUsesTypedDictHelper)
    ("List sum tuple4 dict-list uses typed dict helper", testListSumTuple4DictListValueUsesTypedDictHelper)
    ("List sum tuple3 closure dict-list uses typed dict helper", testListSumTuple3ClosureDictListValueUsesTypedDictHelper)
    ("List sum tuple4 closure dict-list uses typed dict helper", testListSumTuple4ClosureDictListValueUsesTypedDictHelper)
    ("List sum tuple4 closure string dict-list uses typed dict helper", testListSumTuple4ClosureStringDictListValueUsesTypedDictHelper)
    ("Dict dict-list uses typed dict helper", testDictDictListValueUsesTypedDictHelper)
    ("Generic fixed-block nested bytes field uses release plan", testGenericFixedBlockNestedBytesFieldUsesReleasePlan)
    ("Generic fixed-block nested immediate field releases child root", testGenericFixedBlockNestedImmediateFieldReleasesChildRoot)
    ("Generic fixed-block nested mixed boxed-sum bytes payload uses variant dispatch", testGenericFixedBlockNestedMixedBoxedSumBytesPayloadUsesVariantDispatch)
    ("Generic mixed boxed-sum payload dispatch skips remaining cases", testGenericMixedBoxedSumPayloadDispatchSkipsRemainingCases)
    ("Closure capture nested fixed-block bytes field uses release plan", testClosureCaptureNestedFixedBlockBytesFieldUsesReleasePlan)
    ("Closure capture boxed-sum bytes payload uses release plan", testClosureCaptureBoxedSumBytesPayloadUsesReleasePlan)
]
