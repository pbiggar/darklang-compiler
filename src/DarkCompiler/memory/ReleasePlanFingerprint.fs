// ReleasePlanFingerprint.fs - ANF-independent memory representation and release contracts.

module ReleasePlanFingerprint

open MemoryModel

type private RcReleasePlanFingerprintState = { mutable Hash: uint64 }

let private newRcReleasePlanFingerprintState () : RcReleasePlanFingerprintState =
    { Hash = 14695981039346656037UL }

let private addRcReleasePlanFingerprintByte
    (state: RcReleasePlanFingerprintState)
    (value: byte)
    : unit =
    state.Hash <- (state.Hash ^^^ uint64 value) * 1099511628211UL

let private addRcReleasePlanFingerprintInt
    (state: RcReleasePlanFingerprintState)
    (value: int)
    : unit =
    state.Hash <- (state.Hash ^^^ uint64 (int64 value)) * 1099511628211UL

let private addRcReleasePlanFingerprintString
    (state: RcReleasePlanFingerprintState)
    (value: string)
    : unit =
    addRcReleasePlanFingerprintInt state value.Length
    for ch in value do
        state.Hash <- (state.Hash ^^^ uint64 (uint16 ch)) * 1099511628211UL

let private addRcReleasePlanFingerprintKind
    (state: RcReleasePlanFingerprintState)
    (kind: RcKind)
    : unit =
    addRcReleasePlanFingerprintByte state (
        match kind with
        | GenericHeap -> 0uy
        | StreamHeap -> 1uy
        | TaggedList -> 2uy
        | DictHeap -> 3uy
        | ClosureHeap -> 4uy)

let private finishRcReleasePlanFingerprint
    (state: RcReleasePlanFingerprintState)
    : string =
    state.Hash.ToString("x16")

/// Deterministic identity for an RC source type. A compilation has one record
/// and sum registry, so a canonical source type uniquely selects its release
/// plan without traversing the registry-expanded plan itself.
let rcSourceTypeFingerprint (sourceType: AST.Type) : string =
    let state = newRcReleasePlanFingerprintState ()
    let addByte = addRcReleasePlanFingerprintByte state
    let addInt = addRcReleasePlanFingerprintInt state
    let addString = addRcReleasePlanFingerprintString state

    let rec addType typ =
        match typ with
        | AST.TInt8 -> addByte 0uy
        | AST.TInt16 -> addByte 1uy
        | AST.TInt32 -> addByte 2uy
        | AST.TInt64 -> addByte 3uy
        | AST.TInt128 -> addByte 4uy
        | AST.TInt -> addByte 5uy
        | AST.TUInt8 -> addByte 6uy
        | AST.TUInt16 -> addByte 7uy
        | AST.TUInt32 -> addByte 8uy
        | AST.TUInt64 -> addByte 9uy
        | AST.TUInt128 -> addByte 10uy
        | AST.TBool -> addByte 11uy
        | AST.TFloat64 -> addByte 12uy
        | AST.TString -> addByte 13uy
        | AST.TBlob -> addByte 14uy
        | AST.TChar -> addByte 15uy
        | AST.TDateTime -> addByte 16uy
        | AST.TUnit -> addByte 17uy
        | AST.TRuntimeError -> addByte 18uy
        | AST.TFunction (paramTypes, returnType) ->
            addByte 19uy
            addTypes paramTypes
            addType returnType
        | AST.TTuple elementTypes ->
            addByte 20uy
            addTypes elementTypes
        | AST.TRecord (name, typeArgs) ->
            addByte 22uy
            addString name
            addTypes typeArgs
        | AST.TSum (name, typeArgs) ->
            addByte 23uy
            addString name
            addTypes typeArgs
        | AST.TList elementType ->
            addByte 24uy
            addType elementType
        | AST.TStream elementType ->
            addByte 25uy
            addType elementType
        | AST.TVar name ->
            addByte 26uy
            addString name
        | AST.TRawPtr -> addByte 27uy
        | AST.TDict (keyType, valueType) ->
            addByte 28uy
            addType keyType
            addType valueType

    and addTypes types =
        addInt (List.length types)
        types |> List.iter addType

    addType sourceType
    finishRcReleasePlanFingerprint state

/// Build one release-plan fingerprint node from already-fingerprinted direct
/// children. Keeping the hash compositional lets consumers that already walk
/// a plan compute every nested helper identity in one bottom-up pass.
let rcReleasePlanFingerprintHashFromChildren
    (releasePlan: RcReleasePlan)
    (childFingerprints: uint64 list)
    : uint64 =
    let state = newRcReleasePlanFingerprintState ()
    let addByte = addRcReleasePlanFingerprintByte state
    let addInt = addRcReleasePlanFingerprintInt state
    let addString = addRcReleasePlanFingerprintString state
    let addKind = addRcReleasePlanFingerprintKind state

    let addFields fields =
        addInt (List.length fields)
        fields
        |> List.iter (fun (FieldRelease (offset, _)) -> addInt offset)

    let addPayload payload =
        match payload with
        | NoPayloadRelease ->
            addByte 0uy
        | FixedBlockPayloadRelease (payloadSize, fields) ->
            addByte 1uy
            addInt payloadSize
            addFields fields
        | BoxedSumPayloadRelease (payloadSize, fields, variants) ->
            addByte 2uy
            addInt payloadSize
            addFields fields
            addInt (List.length variants)
            variants
            |> List.iter (fun variant ->
                addInt variant.Tag
                addFields variant.FieldReleases)
        | TaggedListPayloadRelease _ ->
            addByte 3uy
        | DictPayloadRelease _ ->
            addByte 4uy
        | ClosurePayloadRelease captureReleases ->
            addByte 5uy
            addFields captureReleases

    match releasePlan with
    | NoReleasePlan ->
        addByte 0uy
    | DynamicBufferRelease operation ->
        addByte 1uy
        match operation with
        | FixedSizeRoot (payloadSize, kind) ->
            addByte 0uy
            addInt payloadSize
            addKind kind
        | DynamicStringBuffer -> addByte 1uy
        | DynamicBlobBuffer -> addByte 2uy
        | DynamicIntBuffer -> addByte 3uy
    | RecursiveRelease sourceType ->
        addByte 2uy
        addString (rcSourceTypeFingerprint sourceType)
    | RootRelease (payloadSize, kind, payload) ->
        addByte 3uy
        addInt payloadSize
        addKind kind
        addPayload payload

    addInt (List.length childFingerprints)
    childFingerprints
    |> List.fold
        (fun hash childFingerprint ->
            (hash ^^^ childFingerprint) * 1099511628211UL)
        state.Hash

let rcReleasePlanFingerprintString (fingerprint: uint64) : string =
    fingerprint.ToString("x16")

let rec rcReleasePlanFingerprintHash (releasePlan: RcReleasePlan) : uint64 =
    let childFingerprints =
        match releasePlan with
        | RootRelease (_, _, payload) ->
            match payload with
            | NoPayloadRelease -> []
            | FixedBlockPayloadRelease (_, fields)
            | ClosurePayloadRelease fields ->
                fields
                |> List.map (fun (FieldRelease (_, childPlan)) ->
                    rcReleasePlanFingerprintHash childPlan)
            | BoxedSumPayloadRelease (_, fields, variants) ->
                let fieldChildren =
                    fields
                    |> List.map (fun (FieldRelease (_, childPlan)) ->
                        rcReleasePlanFingerprintHash childPlan)
                let variantChildren =
                    variants
                    |> List.collect (fun variant ->
                        variant.FieldReleases
                        |> List.map (fun (FieldRelease (_, childPlan)) ->
                            rcReleasePlanFingerprintHash childPlan))
                fieldChildren @ variantChildren
            | TaggedListPayloadRelease elementRelease ->
                [rcReleasePlanFingerprintHash elementRelease]
            | DictPayloadRelease (keyRelease, valueRelease) ->
                [rcReleasePlanFingerprintHash keyRelease
                 rcReleasePlanFingerprintHash valueRelease]
        | NoReleasePlan
        | DynamicBufferRelease _
        | RecursiveRelease _ -> []
    rcReleasePlanFingerprintHashFromChildren releasePlan childFingerprints

/// Deterministic allocation-light identity for a release plan.
let rcReleasePlanFingerprint (releasePlan: RcReleasePlan) : string =
    releasePlan
    |> rcReleasePlanFingerprintHash
    |> rcReleasePlanFingerprintString

/// True once a release plan contains more than the requested number of nodes.
/// Traversal stops at the limit so callers can cheaply choose a compact memo
/// key without fully walking an already-large plan.
let rcReleasePlanExceedsNodeCount
    (nodeLimit: int)
    (releasePlan: RcReleasePlan)
    : bool =
    let mutable nodes = 0
    let visitNode () =
        nodes <- nodes + 1
        nodes > nodeLimit

    let rec visitPlan plan =
        if visitNode () then
            true
        else
            match plan with
            | RootRelease (_, _, payload) -> visitPayload payload
            | NoReleasePlan
            | DynamicBufferRelease _
            | RecursiveRelease _ -> false

    and visitFields fields =
        match fields with
        | [] -> false
        | FieldRelease (_, plan) :: rest ->
            visitNode () || visitPlan plan || visitFields rest

    and visitVariants variants =
        match variants with
        | [] -> false
        | variant :: rest ->
            visitNode () || visitFields variant.FieldReleases || visitVariants rest

    and visitPayload payload =
        if visitNode () then
            true
        else
            match payload with
            | NoPayloadRelease -> false
            | FixedBlockPayloadRelease (_, fields)
            | ClosurePayloadRelease fields -> visitFields fields
            | BoxedSumPayloadRelease (_, fields, variants) ->
                visitFields fields || visitVariants variants
            | TaggedListPayloadRelease elementRelease -> visitPlan elementRelease
            | DictPayloadRelease (keyRelease, valueRelease) ->
                visitPlan keyRelease || visitPlan valueRelease

    visitPlan releasePlan

let rcReleasePlanCompactKeyNodeThreshold = 24

/// Compact key only for plans large enough that structural map comparisons are
/// measurably more expensive than hashing their canonical source type.
let rcReleasePlanCacheKey
    (sourceType: AST.Type)
    (releasePlan: RcReleasePlan)
    : string option =
    if rcReleasePlanExceedsNodeCount rcReleasePlanCompactKeyNodeThreshold releasePlan then
        Some (rcSourceTypeFingerprint sourceType)
    else
        None
