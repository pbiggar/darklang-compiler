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

let private rcMetadataWithRecords (records: LIR.RecordRegistry) (typ: AST.Type) : ANF.RcMetadata =
    {
        ANF.ReleasePlan = Some (ANF.rcReleasePlanOfTypeWithSums records Map.empty typ)
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

let private makeSimpleProgramWithRecords
    (instrs: LIR.Instr list)
    (records: LIR.RecordRegistry)
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
    LIR.Program ([func], Map.empty, records)

let private emitsPlannedListHelperLabel (instrs: ARM64Symbolic.Instr list) : bool =
    instrs
    |> List.exists (function
        | ARM64Symbolic.Label label
        | ARM64Symbolic.BL label ->
            label.StartsWith("__dark_list_refcount_dec_plan_")
        | _ ->
            false)

let private uint64ZeroBranchTargetsDigit (instrs: ARM64.Instr list) : bool =
    instrs
    |> List.mapi (fun index instr -> index, instr)
    |> List.tryPick (fun (index, instr) ->
        match instr with
        | ARM64.CBZ_offset (ARM64.X2, offset) -> Some (index + offset)
        | _ -> None)
    |> Option.bind (fun targetIndex -> List.tryItem targetIndex instrs)
    |> Option.exists (function
        | ARM64.MOVZ (ARM64.X2, 48us, 0) -> true
        | _ -> false)

let testPrintUInt64RuntimeZeroBranches () : TestResult =
    let withNewline = Runtime.generatePrintUInt64NoExit ()
    let withoutNewline = Runtime.generatePrintUInt64NoNewline ()

    if not (uint64ZeroBranchTargetsDigit withNewline) then
        Error "ARM64 UInt64 newline printer zero branch does not target the zero digit handler"
    else if not (uint64ZeroBranchTargetsDigit withoutNewline) then
        Error "ARM64 UInt64 no-newline printer zero branch does not target the zero digit handler"
    else
        Ok ()

let testPrintUInt64RuntimePreservesNewline () : TestResult =
    let preservesNewline =
        Runtime.generatePrintUInt64NoExit ()
        |> List.windowed 3
        |> List.exists (function
            | [ ARM64.MOVZ (ARM64.X3, 10us, 0)
                ARM64.STRB (ARM64.X3, ARM64.X1, 0)
                ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us) ] -> true
            | _ -> false)

    if preservesNewline then
        Ok ()
    else
        Error "ARM64 UInt64 newline printer does not move the digit cursor before conversion"

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

/// Test: malformed ARM64 CFGs should be reported as codegen errors instead of silently dropping the entry.
let testReportsMissingEntryBlock () : TestResult =
    let entryLabel = LIR.Label "_start_entry"
    let bodyLabel = LIR.Label "_start_body"
    let bodyBlock : LIR.BasicBlock = {
        Label = bodyLabel
        Instrs = []
        Terminator = LIR.Ret
    }
    let func : LIR.Function = {
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = entryLabel
            Blocks = Map.ofList [(bodyLabel, bodyBlock)]
        }
        StackSize = 0
        UsedCalleeSaved = []
    }
    let program = LIR.Program ([func], Map.empty, Map.empty)

    match CodeGen.generateARM64 program with
    | Error e when e.Contains "missing entry block" -> Ok ()
    | Error e -> Error $"Expected missing entry block error, got '{e}'"
    | Ok _ -> Error "Expected ARM64 codegen to reject a CFG whose entry block is absent"

let private convertRawAlloc
    (dest: LIR.PhysReg)
    (numBytes: LIR.PhysReg)
    : Result<ARM64Symbolic.Instr list, string> =
    let ctx : CodeGen.CodeGenContext = {
        Options = CodeGen.defaultOptions
        SumShapeRegistry = Map.empty
        RecordRegistry = Map.empty
        ClosurePayloadSizes = Map.empty
        ClosureCaptureTypes = Map.empty
        FunctionName = "test"
        StackSize = 0
        UsedCalleeSaved = []
        HeapOverflowLabel = "__heap_oom_test"
    }
    CodeGen.convertInstr ctx (LIR.RawAlloc (LIR.Physical dest, LIR.Physical numBytes))

let testGeneratedCodeEliminatesSelfMoves () : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.Mov (LIR.Physical LIR.X1, LIR.Reg (LIR.Physical LIR.X1))
                LIR.FMov (LIR.FPhysical LIR.D1, LIR.FPhysical LIR.D1)
            ]
            Map.empty

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let hasSelfMove =
            instrs
            |> List.exists (function
                | ARM64Symbolic.MOV_reg (dest, src) when dest = src ->
                    true
                | ARM64Symbolic.FMOV_reg (dest, src) when dest = src ->
                    true
                | _ ->
                    false)

        if hasSelfMove then
            Error "Generated ARM64 code contains a redundant self-move"
        else
            Ok ()

let testArm64FLoadEncodableConstantsUseImmediate () : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.FLoad (LIR.FPhysical LIR.D2, 1.0)
                LIR.FLoad (LIR.FPhysical LIR.D3, 4.0)
            ]
            Map.empty

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let hasOneImmediate =
            instrs
            |> List.exists (function
                | ARM64Symbolic.FMOV_imm (ARM64.D2, 1.0) ->
                    true
                | _ ->
                    false)
        let hasFourImmediate =
            instrs
            |> List.exists (function
                | ARM64Symbolic.FMOV_imm (ARM64.D3, 4.0) ->
                    true
                | _ ->
                    false)
        let hasLiteralLoad =
            instrs
            |> List.exists (function
                | ARM64Symbolic.ADRP (_, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 1.0))
                | ARM64Symbolic.ADD_label (_, _, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 1.0))
                | ARM64Symbolic.LDR_fp (ARM64.D2, ARM64.X9, 0s) ->
                    true
                | ARM64Symbolic.ADRP (_, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 4.0))
                | ARM64Symbolic.ADD_label (_, _, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 4.0))
                | ARM64Symbolic.LDR_fp (ARM64.D3, ARM64.X9, 0s) ->
                    true
                | _ ->
                    false)

        if not hasOneImmediate then
            Error "FLoad 1.0 did not emit a floating-point immediate"
        elif not hasFourImmediate then
            Error "FLoad 4.0 did not emit a floating-point immediate"
        elif hasLiteralLoad then
            Error "Encodable FLoad used a literal-pool load instead of an immediate"
        else
            Ok ()

/// RawAlloc should branch to a shared overflow label, rather than inlining
/// the full overflow trap sequence at each allocation site.
let testRawAllocUsesSharedHeapOverflowPath () : TestResult =
    match convertRawAlloc LIR.X0 LIR.X1 with
    | Error e -> Error $"Failed to convert RawAlloc: {e}"
    | Ok instrs ->
        let hasHeapEndCmp =
            instrs
            |> List.exists (function
                | ARM64Symbolic.CMP_reg (ARM64.X14, ARM64.X11) -> true
                | _ -> false)

        let hasInlineHeapEndRecompute =
            instrs
            |> List.exists (function
                | ARM64Symbolic.MOVZ (ARM64.X11, imm, 16) when imm = 0x2000us -> true
                | _ -> false)

        let hasOverflowLabelBranch =
            instrs
            |> List.exists (function
                | ARM64Symbolic.B_cond_label (ARM64.GT, _) -> true
                | _ -> false)

        let hasInlinedOverflowTrap =
            instrs
            |> List.exists (function
                | ARM64Symbolic.SVC _ -> true
                | _ -> false)

        if not hasHeapEndCmp then
            Error "Expected RawAlloc bounds check to compare next pointer against computed heap end in X11"
        else if not hasInlineHeapEndRecompute then
            Error "Expected RawAlloc bounds check to compute heap end in X11"
        else if not hasOverflowLabelBranch then
            Error "Expected RawAlloc bounds check to branch to shared overflow label (B_cond_label GT)"
        else if hasInlinedOverflowTrap then
            Error "RawAlloc still inlines overflow trap path (found SVC in fast path conversion)"
        else
            Ok ()

let testRuntimePrintStringLengthUsesFullImmediate () : TestResult =
    let instrs = Runtime.generatePrintString 65537

    let hasLowerLengthChunk =
        instrs
        |> List.exists (function
            | ARM64.MOVZ (ARM64.X2, 1us, 0) ->
                true
            | _ ->
                false)

    let hasUpperLengthChunk =
        instrs
        |> List.exists (function
            | ARM64.MOVK (ARM64.X2, 1us, 16) ->
                true
            | _ ->
                false)

    let truncatesLengthToLowChunkOnly =
        instrs
        |> List.forall (function
            | ARM64.MOVK (ARM64.X2, _, _) ->
                false
            | _ ->
                true)
        && instrs
        |> List.exists (function
            | ARM64.MOVZ (ARM64.X2, 0us, 0) ->
                true
            | ARM64.MOVZ (ARM64.X2, 1us, 0) ->
                true
            | _ ->
                false)

    if hasLowerLengthChunk && hasUpperLengthChunk then
        Ok ()
    elif truncatesLengthToLowChunkOnly then
        Error "Runtime print string length truncated 65537 bytes to a 16-bit low chunk"
    else
        Error "Runtime print string length did not emit both length chunks"

let testRawSlotInitPureEnumDoesNotEmitGenericRetain () : TestResult =
    let enumType = AST.TSum ("RawSlotInitPureEnum", [AST.TString])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            ("RawSlotInitPureEnum",
                { TypeParams = ["a"]
                  Variants =
                    [
                        { Name = "RawSlotInitPureA"; Tag = 0; Payload = None }
                        { Name = "RawSlotInitPureB"; Tag = 1; Payload = None }
                    ] })
        ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RawSlotInit (
                    LIR.Physical LIR.X0,
                    LIR.Physical LIR.X1,
                    LIR.Physical LIR.X3,
                    enumType)
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
            Error "RawSlotInit of a generic pure enum emitted a generic heap retain"
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

let testListTuple4NestedRecordMiddleDictListValueUsesTypedDictHelper () : TestResult =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let recordName = "ARM64ListRcNestedRecordMiddleStringListDictList"
    let nestedRecordType = AST.TRecord (recordName, [])
    let tupleType = AST.TTuple [ AST.TInt64; AST.TInt64; nestedRecordType; AST.TInt64 ]
    let records =
        Map.ofList [
            (recordName, [ ("name", AST.TString); ("items", listType); ("lookup", dictType) ])
        ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadataWithRecords records (AST.TList tupleType)))
            ]
            records

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
            Error "List of tuple(int, int, record(string, list, dict<int, list<int>>), int) did not emit typed dict-list value release helper"

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

let testDictDictListValueUsesPlannedDictHelper () : TestResult =
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
        let callsPlannedDictHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL label when label.StartsWith("__dark_dict_refcount_dec_plan_") -> true
                | _ ->
                    false)
        let callsMatrixNestedDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_dict_refcount_dec_dict_list_value_helper" -> true
                | _ -> false)
        if not callsPlannedDictHelper then
            Error "Dict<int, dict<int, list<int>>> did not emit a planned dict release helper"
        elif callsMatrixNestedDictListHelper then
            Error "Dict<int, dict<int, list<int>>> still emitted the typed nested dict-list helper"
        else
            Ok ()

let private assertDictRefCountDecUsesPlannedDictHelper
    (dictType: AST.Type)
    (caseName: string)
    : TestResult =
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
        let callsPlannedDictHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL label when label.StartsWith("__dark_dict_refcount_dec_plan_") -> true
                | _ -> false)

        if callsPlannedDictHelper then
            Ok ()
        else
            Error $"{caseName} did not emit a planned dict release helper"

let testDictStringKeyUsesPlannedDictHelper () : TestResult =
    assertDictRefCountDecUsesPlannedDictHelper
        (AST.TDict (AST.TString, AST.TInt64))
        "Dict<string, int>"

let testDictStringValueUsesPlannedDictHelper () : TestResult =
    assertDictRefCountDecUsesPlannedDictHelper
        (AST.TDict (AST.TInt64, AST.TString))
        "Dict<int, string>"

let testDictStringKeyListValueUsesPlannedDictHelper () : TestResult =
    assertDictRefCountDecUsesPlannedDictHelper
        (AST.TDict (AST.TString, AST.TList AST.TInt64))
        "Dict<string, list<int>>"

let testDictStringKeyTupleValueUsesPlannedDictHelper () : TestResult =
    assertDictRefCountDecUsesPlannedDictHelper
        (AST.TDict (AST.TString, AST.TTuple [ AST.TString; AST.TList AST.TInt64 ]))
        "Dict<string, tuple<string, list<int>>>"

let testDictStringKeyValuePlannedHelperReleasesCollisionPayloads () : TestResult =
    let dictType = AST.TDict (AST.TString, AST.TString)
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
        let hasCollisionPayloadLoop =
            instrs
            |> List.exists (function
                | ARM64Symbolic.Label label
                    when label.Contains("collision_payload_loop") ->
                    true
                | _ ->
                    false)

        if hasCollisionPayloadLoop then
            Ok ()
        else
            Error "Dict<string, string> planned helper did not emit a collision payload release loop"

let testDictListValuePlannedHelperReleasesCollisionPayloads () : TestResult =
    let dictType = AST.TDict (AST.TInt64, AST.TList AST.TInt64)
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
        let hasCollisionRootPayloadLoop =
            instrs
            |> List.exists (function
                | ARM64Symbolic.Label label
                    when label.Contains("collision_root_payload_loop") ->
                    true
                | _ ->
                    false)

        if hasCollisionRootPayloadLoop then
            Ok ()
        else
            Error "Dict<int, list<int>> planned helper did not emit a collision root payload release loop"

let testDictTupleValuePlannedHelperReleasesCollisionPayloads () : TestResult =
    let dictType = AST.TDict (AST.TInt64, AST.TTuple [ AST.TString; AST.TList AST.TInt64 ])
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
        let hasCollisionGenericPayloadLoop =
            instrs
            |> List.exists (function
                | ARM64Symbolic.Label label
                    when label.Contains("collision_generic_payload_loop") ->
                    true
                | _ ->
                    false)

        if hasCollisionGenericPayloadLoop then
            Ok ()
        else
            Error "Dict<int, tuple<string, list<int>>> planned helper did not emit a collision generic payload release loop"

let testDictStringKeyTupleValuePlannedHelperReleasesCollisionPayloads () : TestResult =
    let dictType = AST.TDict (AST.TString, AST.TTuple [ AST.TString; AST.TList AST.TInt64 ])
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
        let hasCollisionGenericPayloadLoop =
            instrs
            |> List.exists (function
                | ARM64Symbolic.Label label
                    when label.Contains("collision_generic_payload_loop") ->
                    true
                | _ ->
                    false)

        if hasCollisionGenericPayloadLoop then
            Ok ()
        else
            Error "Dict<string, tuple<string, list<int>>> planned helper did not emit a collision generic payload release loop"

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

let testPlannedListGenericLeafReleaseReloadsBlockPointer () : TestResult =
    let listType = AST.TList (AST.TTuple [ AST.TString; AST.TInt64 ])
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata listType))
            ]
            Map.empty

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let reloadsGenericLeafPointer =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X8, ARM64.X3, 0s) ->
                    true
                | _ ->
                    false)

        if reloadsGenericLeafPointer then
            Ok ()
        else
            Error "ARM64 planned list generic release did not reload the leaf pointer before freeing it"

let testPlannedListNestedGenericReleasePreservesBlockPointer () : TestResult =
    let listType = AST.TList (AST.TTuple [ AST.TTuple [ AST.TString; AST.TInt64 ]; AST.TInt64 ])
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata listType))
            ]
            Map.empty

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        let preservesNestedGenericBlockPointer =
            instrs
            |> List.exists (function
                | ARM64Symbolic.STP_pre (ARM64.X12, ARM64.X30, ARM64.SP, -16s) ->
                    true
                | _ ->
                    false)

        if preservesNestedGenericBlockPointer then
            Ok ()
        else
            Error "ARM64 planned list nested generic release did not preserve the block pointer across nested field releases"

let testPlannedListTuplePayloadUsesPlannedHelper () : TestResult =
    let tupleType =
        AST.TTuple [ AST.TString; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TInt64) ]
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
        if emitsPlannedListHelperLabel instrs then
            Ok ()
        else
            Error "ARM64 tuple list payload did not emit a planned list helper"

let testPlannedListRecordPayloadUsesPlannedHelper () : TestResult =
    let recordType = AST.TRecord ("ARM64PlannedListRecordPayload", [])
    let records =
        Map.ofList [
            ("ARM64PlannedListRecordPayload", [ ("name", AST.TString); ("items", AST.TList AST.TInt64) ])
        ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadataWithRecords records (AST.TList recordType)))
            ]
            records

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        if emitsPlannedListHelperLabel instrs then
            Ok ()
        else
            Error "ARM64 record list payload did not emit a planned list helper"

let testPlannedListTuple5PayloadUsesPlannedHelper () : TestResult =
    let tupleType =
        AST.TTuple [
            AST.TString
            AST.TBytes
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
            AST.TFunction ([AST.TInt64], AST.TInt64)
        ]
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
        if emitsPlannedListHelperLabel instrs then
            Ok ()
        else
            Error "ARM64 tuple5 list payload did not emit a planned list helper"

let testPlannedListRecord5PayloadUsesPlannedHelper () : TestResult =
    let recordType = AST.TRecord ("ARM64PlannedListRecord5Payload", [])
    let records =
        Map.ofList [
            ("ARM64PlannedListRecord5Payload",
                [
                    ("name", AST.TString)
                    ("blob", AST.TBytes)
                    ("items", AST.TList AST.TInt64)
                    ("lookup", AST.TDict (AST.TInt64, AST.TList AST.TInt64))
                    ("fn", AST.TFunction ([AST.TInt64], AST.TInt64))
                ])
        ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadataWithRecords records (AST.TList recordType)))
            ]
            records

    match CodeGen.generateARM64 program with
    | Error e ->
        Error e
    | Ok instrs ->
        if emitsPlannedListHelperLabel instrs then
            Ok ()
        else
            Error "ARM64 record5 list payload did not emit a planned list helper"

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
    ("LIR ARM64 codegen reports missing entry block", testReportsMissingEntryBlock)
    ("Generated ARM64 code eliminates self-moves", testGeneratedCodeEliminatesSelfMoves)
    ("ARM64 UInt64 runtime zero branches target digit handlers", testPrintUInt64RuntimeZeroBranches)
    ("ARM64 UInt64 runtime preserves trailing newline", testPrintUInt64RuntimePreservesNewline)
    ("ARM64 FLoad encodable constants use immediate", testArm64FLoadEncodableConstantsUseImmediate)
    ("RawAlloc uses shared heap overflow path", testRawAllocUsesSharedHeapOverflowPath)
    ("Runtime print string length uses full immediate", testRuntimePrintStringLengthUsesFullImmediate)
    ("RawSlotInit pure enum skips generic retain", testRawSlotInitPureEnumDoesNotEmitGenericRetain)
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
    ("List tuple4 nested record middle dict-list uses typed dict helper", testListTuple4NestedRecordMiddleDictListValueUsesTypedDictHelper)
    ("List tuple4 nested tuple closure dict-list uses typed dict helper", testListTuple4NestedTupleClosureDictListValueUsesTypedDictHelper)
    ("List sum tuple3 dict-list uses typed dict helper", testListSumTuple3DictListValueUsesTypedDictHelper)
    ("List sum tuple4 dict-list uses typed dict helper", testListSumTuple4DictListValueUsesTypedDictHelper)
    ("List sum tuple3 closure dict-list uses typed dict helper", testListSumTuple3ClosureDictListValueUsesTypedDictHelper)
    ("List sum tuple4 closure dict-list uses typed dict helper", testListSumTuple4ClosureDictListValueUsesTypedDictHelper)
    ("List sum tuple4 closure string dict-list uses typed dict helper", testListSumTuple4ClosureStringDictListValueUsesTypedDictHelper)
    ("Dict dict-list uses planned dict helper", testDictDictListValueUsesPlannedDictHelper)
    ("Dict string key uses planned dict helper", testDictStringKeyUsesPlannedDictHelper)
    ("Dict string value uses planned dict helper", testDictStringValueUsesPlannedDictHelper)
    ("Dict string key list value uses planned dict helper", testDictStringKeyListValueUsesPlannedDictHelper)
    ("Dict string key tuple value uses planned dict helper", testDictStringKeyTupleValueUsesPlannedDictHelper)
    ("Dict string key/value planned helper releases collision payloads", testDictStringKeyValuePlannedHelperReleasesCollisionPayloads)
    ("Dict list value planned helper releases collision payloads", testDictListValuePlannedHelperReleasesCollisionPayloads)
    ("Dict tuple value planned helper releases collision payloads", testDictTupleValuePlannedHelperReleasesCollisionPayloads)
    ("Dict string key tuple value planned helper releases collision payloads", testDictStringKeyTupleValuePlannedHelperReleasesCollisionPayloads)
    ("Generic fixed-block nested bytes field uses release plan", testGenericFixedBlockNestedBytesFieldUsesReleasePlan)
    ("Planned list generic leaf release reloads block pointer", testPlannedListGenericLeafReleaseReloadsBlockPointer)
    ("Planned list nested generic release preserves block pointer", testPlannedListNestedGenericReleasePreservesBlockPointer)
    ("Planned list tuple payload uses planned helper", testPlannedListTuplePayloadUsesPlannedHelper)
    ("Planned list record payload uses planned helper", testPlannedListRecordPayloadUsesPlannedHelper)
    ("Planned list tuple5 payload uses planned helper", testPlannedListTuple5PayloadUsesPlannedHelper)
    ("Planned list record5 payload uses planned helper", testPlannedListRecord5PayloadUsesPlannedHelper)
    ("Generic fixed-block nested immediate field releases child root", testGenericFixedBlockNestedImmediateFieldReleasesChildRoot)
    ("Generic fixed-block nested mixed boxed-sum bytes payload uses variant dispatch", testGenericFixedBlockNestedMixedBoxedSumBytesPayloadUsesVariantDispatch)
    ("Generic mixed boxed-sum payload dispatch skips remaining cases", testGenericMixedBoxedSumPayloadDispatchSkipsRemainingCases)
    ("Closure capture nested fixed-block bytes field uses release plan", testClosureCaptureNestedFixedBlockBytesFieldUsesReleasePlan)
    ("Closure capture boxed-sum bytes payload uses release plan", testClosureCaptureBoxedSumBytesPayloadUsesReleasePlan)
]
