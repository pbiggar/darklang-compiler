// MemoryPlanning.fs - ANF-independent memory representation and release contracts.

module MemoryPlanning

open MemoryModel

/// Classify a source type into its current runtime RC representation shape.
///
/// The classifier is intentionally pure and side-effect free. Ownership
/// insertion and backend helper selection use this as the source of truth for
/// runtime retain/release shape decisions.
let rec rcShapeOfType (typeReg: Map<string, (string * AST.Type) list>) (t: AST.Type) : RcShape =
    match t with
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TBool
    | AST.TFloat64
    | AST.TDateTime
    | AST.TUnit
    | AST.TRuntimeError
    | AST.TVar _ ->
        Immediate
    // Arbitrary Int uses tagged immediates or a limb buffer. Fixed-width 128-bit
    // values are immutable two-limb blocks with the refcount after the payload.
    | AST.TInt -> DynamicInt
    | AST.TInt128
    | AST.TUInt128 -> FixedBlock (16, [])
    | AST.TTuple elemTypes ->
        let fieldShapes = elemTypes |> List.map (rcShapeOfType typeReg)
        FixedBlock (List.length elemTypes * 8, fieldShapes)
    | AST.TRecord (name, _) ->
        match Map.tryFind name typeReg with
        | Some fields ->
            let fieldShapes =
                fields
                |> List.map (fun (_, fieldType) -> rcShapeOfType typeReg fieldType)
            FixedBlock (List.length fields * 8, fieldShapes)
        | None ->
            Crash.crash $"rcShapeOfType: Record type '{name}' not found in typeReg"
    | AST.TSum (_, []) ->
        Immediate
    | AST.TSum (_, [payloadType]) ->
        BoxedSum (16, [(8, rcShapeOfType typeReg payloadType)], [])
    | AST.TSum _ ->
        BoxedSum (16, [], [])
    | AST.TList elemType ->
        TaggedListShape (rcShapeOfType typeReg elemType)
    | AST.TStream _ -> StreamRoot
    | AST.TDict (keyType, valueType) ->
        DictRoot (rcShapeOfType typeReg keyType, rcShapeOfType typeReg valueType)
    | AST.TString
    | AST.TChar ->
        DynamicString
    | AST.TBlob ->
        DynamicBlob
    | AST.TFunction _ ->
        ClosureShape []
    | AST.TRawPtr ->
        RawUnmanaged

let private rcShapeTypeSubstitution (typeParams: string list) (typeArgs: AST.Type list) : Map<string, AST.Type> =
    if List.isEmpty typeParams then
        Map.empty
    elif List.length typeParams = List.length typeArgs then
        List.zip typeParams typeArgs |> Map.ofList
    else
        Crash.crash $"rcShapeOfTypeWithSums: sum type argument mismatch: params={typeParams.Length}, args={typeArgs.Length}"

let private collectTypeVarsInOrder (typ: AST.Type) : string list =
    let rec collect t =
        match t with
        | AST.TVar name -> [name]
        | AST.TTuple elemTypes -> elemTypes |> List.collect collect
        | AST.TRecord (_, typeArgs) -> typeArgs |> List.collect collect
        | AST.TList elemType -> collect elemType
        | AST.TStream elemType -> collect elemType
        | AST.TDict (keyType, valueType) -> collect keyType @ collect valueType
        | AST.TSum (_, typeArgs) -> typeArgs |> List.collect collect
        | AST.TFunction (paramTypes, returnType) ->
            (paramTypes |> List.collect collect) @ collect returnType
        | AST.TInt8
        | AST.TInt16
        | AST.TInt32
        | AST.TInt64
        | AST.TInt128
        | AST.TInt
        | AST.TUInt8
        | AST.TUInt16
        | AST.TUInt32
        | AST.TUInt64
        | AST.TUInt128
        | AST.TBool
        | AST.TFloat64
        | AST.TString
        | AST.TBlob
        | AST.TChar
        | AST.TDateTime
        | AST.TUnit
        | AST.TRawPtr
        | AST.TRuntimeError ->
            []
    collect typ |> List.distinct

let inferredRecordTypeParamsRegistry
    (typeReg: Map<string, (string * AST.Type) list>)
    : Map<string, string list> =
    typeReg
    |> Map.map (fun _ fields ->
        fields
        |> List.collect (fun (_, fieldType) -> collectTypeVarsInOrder fieldType)
        |> List.distinct)

let rec private applyRcShapeTypeSubstitution (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
    match typ with
    | AST.TVar name ->
        match Map.tryFind name subst with
        | Some concrete -> concrete
        | None -> typ
    | AST.TTuple elemTypes ->
        AST.TTuple (elemTypes |> List.map (applyRcShapeTypeSubstitution subst))
    | AST.TRecord (name, typeArgs) ->
        AST.TRecord (name, typeArgs |> List.map (applyRcShapeTypeSubstitution subst))
    | AST.TList elemType ->
        AST.TList (applyRcShapeTypeSubstitution subst elemType)
    | AST.TStream elemType ->
        AST.TStream (applyRcShapeTypeSubstitution subst elemType)
    | AST.TDict (keyType, valueType) ->
        AST.TDict (applyRcShapeTypeSubstitution subst keyType, applyRcShapeTypeSubstitution subst valueType)
    | AST.TSum (name, typeArgs) ->
        AST.TSum (name, typeArgs |> List.map (applyRcShapeTypeSubstitution subst))
    | AST.TFunction (paramTypes, returnType) ->
        AST.TFunction (
            paramTypes |> List.map (applyRcShapeTypeSubstitution subst),
            applyRcShapeTypeSubstitution subst returnType
        )
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TInt128
    | AST.TInt
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128
    | AST.TBool
    | AST.TFloat64
    | AST.TString
    | AST.TBlob
    | AST.TChar
    | AST.TDateTime
    | AST.TUnit
    | AST.TRawPtr
    | AST.TRuntimeError ->
        typ

/// Classify a source type using record metadata and optional named-sum metadata.
let rcShapeOfTypeWithSums
    (typeReg: Map<string, (string * AST.Type) list>)
    (recordTypeParams: Map<string, string list>)
    (sumReg: RcSumShapeRegistry)
    (t: AST.Type)
    : RcShape =
    let rec classify (expandingSums: Set<string>) (t: AST.Type) : RcShape =
        match t with
        | AST.TTuple elemTypes ->
            FixedBlock (List.length elemTypes * 8, elemTypes |> List.map (classify expandingSums))
        | AST.TRecord (name, typeArgs) ->
            match Map.tryFind name typeReg with
            | Some fields ->
                let typeParams =
                    match Map.tryFind name recordTypeParams with
                    | Some declared -> declared
                    | None -> Crash.crash $"rcShapeOfTypeWithSums: Record metadata '{name}' not found"
                let subst = rcShapeTypeSubstitution typeParams typeArgs
                let fieldShapes =
                    fields
                    |> List.map (fun (_, fieldType) ->
                        fieldType |> applyRcShapeTypeSubstitution subst |> classify expandingSums)
                FixedBlock (List.length fields * 8, fieldShapes)
            | None when Map.containsKey name sumReg ->
                // Bare nominal references are parsed before constructor
                // metadata is available. Classify the equivalent internal sum
                // spelling here so recursive JSON trees retain correctly.
                classify expandingSums (AST.TSum (name, typeArgs))
            | None ->
                Crash.crash $"rcShapeOfTypeWithSums: Record type '{name}' not found in typeReg"
        | AST.TSum (name, typeArgs) ->
            if Set.contains name expandingSums then
                RecursiveSumRef (AST.TSum (name, typeArgs))
            else
                match Map.tryFind name sumReg with
                | Some sumInfo ->
                    let subst = rcShapeTypeSubstitution sumInfo.TypeParams typeArgs
                    let expandingSums = Set.add name expandingSums

                    let variantShapes =
                        sumInfo.Payloads
                        |> List.map (fun maybePayload ->
                            match maybePayload with
                            | tag, Some payload ->
                                let payloadShape = payload |> applyRcShapeTypeSubstitution subst |> classify expandingSums
                                { Tag = tag; FieldShapes = [(8, payloadShape)] }
                            | tag, None ->
                                { Tag = tag; FieldShapes = [] })

                    let hasPayloadVariant =
                        sumInfo.Payloads
                        |> List.exists (fun (_, payload) -> Option.isSome payload)

                    if hasPayloadVariant then
                        let fieldShapes =
                            variantShapes
                            |> List.collect (fun variant -> variant.FieldShapes)

                        BoxedSum (16, fieldShapes, variantShapes)
                    else
                        Immediate
                | None when Map.containsKey name typeReg ->
                    classify expandingSums (AST.TRecord (name, typeArgs))
                | None ->
                    Crash.crash $"rcShapeOfTypeWithSums: Sum type '{name}' not found in sumReg"
        | AST.TList elemType ->
            TaggedListShape (classify expandingSums elemType)
        | AST.TStream _ -> StreamRoot
        | AST.TDict (keyType, valueType) ->
            DictRoot (classify expandingSums keyType, classify expandingSums valueType)
        | AST.TFunction _ ->
            ClosureShape []
        | AST.TString
        | AST.TChar ->
            DynamicString
        | AST.TInt ->
            DynamicInt
        | AST.TInt128
        | AST.TUInt128 ->
            FixedBlock (16, [])
        | AST.TBlob ->
            DynamicBlob
        | AST.TRawPtr ->
            RawUnmanaged
        | AST.TInt8
        | AST.TInt16
        | AST.TInt32
        | AST.TInt64
        | AST.TUInt8
        | AST.TUInt16
        | AST.TUInt32
        | AST.TUInt64
        | AST.TBool
        | AST.TFloat64
        | AST.TDateTime
        | AST.TUnit
        | AST.TRuntimeError
        | AST.TVar _ ->
            Immediate

    classify Set.empty t

/// True when a runtime shape can own managed memory that must be released when
/// an owning binding leaves scope.
let rcShapeNeedsOwnedScopeRelease (shape: RcShape) : bool =
    match shape with
    | Immediate
    | StaticString
    | RawUnmanaged ->
        false
    | DynamicString
    | DynamicBlob
    | DynamicInt
    | FixedBlock _
    | StreamRoot
    | BoxedSum _
    | RecursiveSumRef _
    | TaggedListShape _
    | DictRoot _
    | ClosureShape _ ->
        true

/// True when a shape is managed through a fixed-size or tagged RC root rather
/// than a dynamic-buffer helper or an unmanaged representation.
let rcShapeIsRootManaged (shape: RcShape) : bool =
    match shape with
    | FixedBlock _
    | StreamRoot
    | BoxedSum _
    | RecursiveSumRef _
    | TaggedListShape _
    | DictRoot _
    | ClosureShape _ ->
        true
    | Immediate
    | DynamicString
    | DynamicBlob
    | DynamicInt
    | StaticString
    | RawUnmanaged ->
        false

/// True when releasing a value of this shape can require walking owned payload
/// fields, captures, list leaves, or dict leaf entries in addition to releasing
/// the root allocation itself.
let rec rcShapeNeedsRecursiveRelease (shape: RcShape) : bool =
    match shape with
    | FixedBlock (_, fieldShapes) ->
        fieldShapes |> List.exists rcShapeNeedsOwnedScopeRelease
    | StreamRoot -> true
    | BoxedSum (_, fieldShapes, _) ->
        fieldShapes
        |> List.exists (fun (_, fieldShape) -> rcShapeNeedsOwnedScopeRelease fieldShape)
    | RecursiveSumRef _ ->
        true
    | TaggedListShape elementShape ->
        rcShapeNeedsOwnedScopeRelease elementShape
    | DictRoot (keyShape, valueShape) ->
        rcShapeNeedsOwnedScopeRelease keyShape
        || rcShapeNeedsOwnedScopeRelease valueShape
    | ClosureShape captureShapes ->
        captureShapes |> List.exists rcShapeNeedsOwnedScopeRelease
    | Immediate
    | DynamicString
    | DynamicBlob
    | DynamicInt
    | StaticString
    | RawUnmanaged ->
        false

/// Dispatch kind for fixed-size/tagged RC roots. Dynamic buffers use their own
/// string/bytes operations, so they intentionally do not have a root kind here.
let rcShapeRootKind (shape: RcShape) : RcKind option =
    match shape with
    | FixedBlock _
    | BoxedSum _
    | RecursiveSumRef _ ->
        Some GenericHeap
    | StreamRoot -> Some StreamHeap
    | TaggedListShape _ ->
        Some TaggedList
    | DictRoot _ ->
        Some DictHeap
    | ClosureShape _ ->
        Some ClosureHeap
    | Immediate
    | DynamicString
    | DynamicBlob
    | DynamicInt
    | StaticString
    | RawUnmanaged ->
        None

/// Payload size for fixed-size/tagged RC roots.
let rcShapePayloadSize (shape: RcShape) : int option =
    match shape with
    | FixedBlock (payloadSize, _)
    | BoxedSum (payloadSize, _, _) ->
        Some payloadSize
    | StreamRoot -> Some 24
    | RecursiveSumRef _ ->
        Some 16
    | TaggedListShape _ ->
        Some 24
    | DictRoot _ ->
        Some 8
    | ClosureShape _ ->
        Some 0
    | Immediate
    | DynamicString
    | DynamicBlob
    | DynamicInt
    | StaticString
    | RawUnmanaged ->
        None

/// Storage class for deciding whether a value is unmanaged, managed by a
/// dynamic-buffer helper, or managed by a fixed/tagged RC root helper.
let rcShapeStorageClass (shape: RcShape) : RcStorageClass =
    match shape with
    | DynamicString ->
        ManagedDynamicBuffer DynamicStringBuffer
    | DynamicBlob ->
        ManagedDynamicBuffer DynamicBlobBuffer
    | DynamicInt ->
        ManagedDynamicBuffer DynamicIntBuffer
    | _ ->
        match rcShapePayloadSize shape, rcShapeRootKind shape with
        | Some payloadSize, Some kind ->
            ManagedRcRoot (payloadSize, kind)
        | _ ->
            UnmanagedStorage

/// True when a value is represented by an RC root whose ownership can be
/// transferred to another aggregate or helper call.
let rcShapeIsOwnershipTransferRoot (shape: RcShape) : bool =
    match rcShapeStorageClass shape with
    | ManagedRcRoot _ ->
        true
    | ManagedDynamicBuffer _
    | UnmanagedStorage ->
        false

/// Retain operation for an owned or borrowed value of the given shape.
let rcShapeRetainOperation (shape: RcShape) : RcOperation option =
    match rcShapeStorageClass shape with
    | ManagedDynamicBuffer operation ->
        Some operation
    | ManagedRcRoot (payloadSize, kind) ->
        Some (FixedSizeRoot (payloadSize, kind))
    | UnmanagedStorage ->
        None

/// Release operation for an owned value of the given shape.
let rcShapeReleaseOperation (shape: RcShape) : RcOperation option =
    if rcShapeNeedsOwnedScopeRelease shape then
        rcShapeRetainOperation shape
    else
        None

/// True when a borrowed value of this shape needs a retain before it can be
/// returned or otherwise materialized as a new owned value.
let rcShapeNeedsBorrowedRetain (shape: RcShape) : bool =
    rcShapeRetainOperation shape |> Option.isSome

/// True when a normal owning binding of this shape should receive an automatic
/// decrement from RC insertion. Closure roots are handled by closure-producing
/// expressions so aliases of function-typed values do not double-release.
let rcShapeNeedsAutomaticBindingDec (shape: RcShape) : bool =
    match shape with
    | ClosureShape _ ->
        false
    | _ ->
        rcShapeNeedsOwnedScopeRelease shape

/// True when a borrowed alias of this shape carries a managed root identity
/// that should be preserved by type inference. Closure aliases are excluded
/// because closure-producing expressions own their lifetime separately.
let rcShapeNeedsManagedAliasRootPreservation (shape: RcShape) : bool =
    match rcShapeStorageClass shape with
    | ManagedRcRoot (_, ClosureHeap) ->
        false
    | ManagedRcRoot _ ->
        true
    | ManagedDynamicBuffer _
    | UnmanagedStorage ->
        false

/// Release plan for a value with the given runtime shape.
let rec rcShapeReleasePlan (shape: RcShape) : RcReleasePlan =
    let releasePlansAtOffsets (fields: (int * RcShape) list) : RcFieldRelease list =
        fields
        |> List.choose (fun (offset, fieldShape) ->
            match rcShapeReleasePlan fieldShape with
            | NoReleasePlan ->
                None
            | plan ->
                Some (FieldRelease (offset, plan)))

    let fieldReleasePlans (fieldShapes: RcShape list) : RcFieldRelease list =
        fieldShapes
        |> List.mapi (fun index fieldShape -> (index * 8, fieldShape))
        |> releasePlansAtOffsets

    let rootPayloadPlan (rootShape: RcShape) : RcPayloadReleasePlan =
        match rootShape with
        | FixedBlock (payloadSize, fieldShapes) ->
            FixedBlockPayloadRelease (payloadSize, fieldReleasePlans fieldShapes)
        | StreamRoot ->
            FixedBlockPayloadRelease (24, fieldReleasePlans [Immediate; ClosureShape []; ClosureShape []])
        | BoxedSum (payloadSize, fieldShapes, variants) ->
            let variantReleases =
                variants
                |> List.map (fun variant ->
                    let releases =
                        variant.FieldShapes
                        |> releasePlansAtOffsets

                    { Tag = variant.Tag; FieldReleases = releases })

            BoxedSumPayloadRelease (payloadSize, releasePlansAtOffsets fieldShapes, variantReleases)
        | TaggedListShape elementShape ->
            TaggedListPayloadRelease (rcShapeReleasePlan elementShape)
        | DictRoot (keyShape, valueShape) ->
            DictPayloadRelease (rcShapeReleasePlan keyShape, rcShapeReleasePlan valueShape)
        | ClosureShape captureShapes ->
            ClosurePayloadRelease (fieldReleasePlans captureShapes)
        | RecursiveSumRef _ ->
            NoPayloadRelease
        | Immediate
        | DynamicString
        | DynamicBlob
        | DynamicInt
        | StaticString
        | RawUnmanaged ->
            NoPayloadRelease

    match rcShapeStorageClass shape with
    | ManagedRcRoot (payloadSize, kind) ->
        match shape with
        | RecursiveSumRef sourceType -> RecursiveRelease sourceType
        | _ -> RootRelease (payloadSize, kind, rootPayloadPlan shape)
    | UnmanagedStorage ->
        NoReleasePlan
    | ManagedDynamicBuffer operation ->
        DynamicBufferRelease operation

/// Release plan for a source type using the current representation registry.
let rec rcReleasePlanOfType (typeReg: Map<string, (string * AST.Type) list>) (t: AST.Type) : RcReleasePlan =
    t |> rcShapeOfType typeReg |> rcShapeReleasePlan

/// Release plan for a source type using record and named-sum metadata.
let rec rcReleasePlanOfTypeWithSums
    (typeReg: Map<string, (string * AST.Type) list>)
    (sumReg: RcSumShapeRegistry)
    (t: AST.Type)
    : RcReleasePlan =
    t
    |> rcShapeOfTypeWithSums typeReg (inferredRecordTypeParamsRegistry typeReg) sumReg
    |> rcShapeReleasePlan

/// Collect the concrete recursive sum roots referenced by a finite release plan.
let rec recursiveReleaseTypes (releasePlan: RcReleasePlan) : Set<AST.Type> =
    let fromFields fieldReleases =
        fieldReleases
        |> List.map (fun (FieldRelease (_, fieldPlan)) -> recursiveReleaseTypes fieldPlan)
        |> List.fold Set.union Set.empty

    match releasePlan with
    | RecursiveRelease sourceType ->
        Set.singleton sourceType
    | RootRelease (_, _, FixedBlockPayloadRelease (_, fieldReleases))
    | RootRelease (_, _, BoxedSumPayloadRelease (_, fieldReleases, _))
    | RootRelease (_, _, ClosurePayloadRelease fieldReleases) ->
        fromFields fieldReleases
    | RootRelease (_, _, TaggedListPayloadRelease elementRelease) ->
        recursiveReleaseTypes elementRelease
    | RootRelease (_, _, DictPayloadRelease (keyRelease, valueRelease)) ->
        Set.union (recursiveReleaseTypes keyRelease) (recursiveReleaseTypes valueRelease)
    | RootRelease (_, _, NoPayloadRelease)
    | DynamicBufferRelease _
    | NoReleasePlan ->
        Set.empty
