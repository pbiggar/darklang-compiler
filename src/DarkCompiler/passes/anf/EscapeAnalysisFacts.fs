// EscapeAnalysisFacts.fs - Type and destruction proofs shared by SSA escape analysis.

module EscapeAnalysisFacts

open ANF

type private ScalarAggregate = {
    Fields: Atom list
}

let isScalarType (typ: AST.SemanticType) : bool =
    match typ with
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
    | AST.TNever -> true
    | _ -> false

/// Prove that releasing a displaced field cannot run a language-visible
/// finalizer. Nominal records and sums use complete registry metadata and
/// concrete type arguments. Regular recursive cycles are admitted
/// coinductively; sums are considered only for boxed-sum reuse candidates, and
/// type-growing recursion and closures fail closed.
let hasNonObservableDestruction
    (typeReg: TypeRegistries.TypeRegistry)
    (sumReg: MemoryModel.RcSumShapeRegistry)
    (allowSums: bool)
    (typ: AST.SemanticType)
    : bool =
    let rec prove
        (expandingRecords: Map<string, AST.SemanticType>)
        (expandingSums: Map<string, AST.SemanticType>)
        typ
        =
        isScalarType typ
        || match typ with
           | AST.TString | AST.TBlob | AST.TInt -> true
           | AST.TTuple elements ->
               List.forall (prove expandingRecords expandingSums) elements
           | AST.TList element -> prove expandingRecords expandingSums element
           | AST.TDict (key, value) ->
               prove expandingRecords expandingSums key
               && prove expandingRecords expandingSums value
           | AST.TRecord (name, typeArgs) ->
               let recordType = AST.TRecord (name, typeArgs)
               match Map.tryFind name expandingRecords with
               | Some expandingType -> expandingType = recordType
               | None ->
                   match Map.tryFind name typeReg with
                   | Some info when List.length info.TypeParams = List.length typeArgs ->
                       let subst = List.zip info.TypeParams typeArgs |> Map.ofList
                       let expandingRecords = Map.add name recordType expandingRecords
                       info.Fields
                       |> List.forall (fun (_, fieldType) ->
                           fieldType
                           |> TypeSubstitution.applySubstToType subst
                           |> prove expandingRecords expandingSums)
                   | None when allowSums && Map.containsKey name sumReg ->
                       prove expandingRecords expandingSums (AST.TSum (name, typeArgs))
                   | _ -> false
           | AST.TSum (name, typeArgs) when allowSums ->
               let sumType = AST.TSum (name, typeArgs)
               match Map.tryFind name expandingSums with
               | Some expandingType -> expandingType = sumType
               | None ->
                   match Map.tryFind name sumReg with
                   | Some info when List.length info.TypeParams = List.length typeArgs ->
                       let subst = List.zip info.TypeParams typeArgs |> Map.ofList
                       let expandingSums = Map.add name sumType expandingSums
                       info.Payloads
                       |> List.forall (fun (_, payload) ->
                           payload
                           |> Option.forall (fun payloadType ->
                               payloadType
                               |> TypeSubstitution.applySubstToType subst
                               |> prove expandingRecords expandingSums))
                   | _ -> false
           | _ -> false
    prove Map.empty Map.empty typ

let descriptorHasNonObservableDestruction
    (typeReg: TypeRegistries.TypeRegistry)
    (sumReg: MemoryModel.RcSumShapeRegistry)
    (descriptor: RecordDescriptor)
    : bool =
    let allowSums =
        match descriptor.ValueType with
        | AST.TSum _ -> true
        | _ -> false
    descriptor.Fields
    |> List.forall (snd >> hasNonObservableDestruction typeReg sumReg allowSums)
