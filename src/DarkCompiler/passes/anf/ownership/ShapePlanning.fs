// ShapePlanning.fs - Select canonical representation shapes for ANF ownership decisions.

module RcShapePlanning

open MemoryModel
open ReleasePlanFingerprint
open MemoryPlanning
open ANF
open TypeRegistries
open LiftExpressions
open LiftFunctions
open RcTypeFacts

let private canonicalRcTypeForShape (ctx: TypeContext) (typ: AST.SemanticType) : AST.SemanticType =
    let canonicalBareSum name =
        AST.TSum (name, [])

    let rec canonicalize typ =
        match typ with
        | AST.TRecord (name, []) when Map.containsKey name ctx.SumShapeReg ->
            canonicalBareSum name
        | AST.TSum (name, []) when Map.containsKey name ctx.SumShapeReg ->
            canonicalBareSum name
        | AST.TRecord (name, typeArgs) ->
            AST.TRecord (name, List.map canonicalize typeArgs)
        | AST.TSum (name, typeArgs) ->
            AST.TSum (name, List.map canonicalize typeArgs)
        | AST.TFunction (paramTypes, returnType) ->
            AST.TFunction (List.map canonicalize paramTypes, canonicalize returnType)
        | AST.TTuple elemTypes ->
            AST.TTuple (List.map canonicalize elemTypes)
        | AST.TList elemType ->
            AST.TList (canonicalize elemType)
        | AST.TStream elemType ->
            AST.TStream (canonicalize elemType)
        | AST.TDict (keyType, valueType) ->
            AST.TDict (canonicalize keyType, canonicalize valueType)
        | AST.TVar _ | AST.TInferenceVar _ | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TInt
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
        | AST.TBool | AST.TFloat64 | AST.TString | AST.TBlob | AST.TChar | AST.TDateTime
        | AST.TUnit | AST.TInternalRawPtr | AST.TNever ->
            typ

    canonicalize typ

let private canonicalRcSourceType (ctx: TypeContext) (typ: AST.SemanticType) : AST.SemanticType =
    let rec canonicalize typ =
        match typ with
        | AST.TRecord (name, []) when Map.containsKey name ctx.SumShapeReg ->
            AST.TSum (name, [])
        | AST.TRecord (name, typeArgs) ->
            AST.TRecord (name, List.map canonicalize typeArgs)
        | AST.TSum (name, typeArgs) ->
            AST.TSum (name, List.map canonicalize typeArgs)
        | AST.TFunction (paramTypes, returnType) ->
            AST.TFunction (List.map canonicalize paramTypes, canonicalize returnType)
        | AST.TTuple elemTypes ->
            AST.TTuple (List.map canonicalize elemTypes)
        | AST.TList elemType ->
            AST.TList (canonicalize elemType)
        | AST.TStream elemType ->
            AST.TStream (canonicalize elemType)
        | AST.TDict (keyType, valueType) ->
            AST.TDict (canonicalize keyType, canonicalize valueType)
        | AST.TVar _ | AST.TInferenceVar _ | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TInt
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
        | AST.TBool | AST.TFloat64 | AST.TString | AST.TBlob | AST.TChar | AST.TDateTime
        | AST.TUnit | AST.TInternalRawPtr | AST.TNever ->
            typ

    canonicalize typ

let internal rcShapeForType (ctx: TypeContext) (typ: AST.SemanticType) : RcShape =
    match ctx.TypePlanning.Shapes.TryGetValue typ with
    | true, shape -> shape
    | false, _ ->
        let (recordFieldsReg, recordTypeParamsReg) =
            match ctx.TypePlanning.RecordRegistries with
            | Some registries -> registries
            | None ->
                let registries =
                    (recordFieldsRegistry ctx.TypeReg,
                     recordTypeParamsRegistry ctx.TypeReg)
                ctx.TypePlanning.RecordRegistries <- Some registries
                registries
        let shape =
            typ
            |> canonicalRcTypeForShape ctx
            |> rcShapeOfTypeWithSums
                recordFieldsReg
                recordTypeParamsReg
                ctx.SumShapeReg
        ctx.TypePlanning.Shapes.[typ] <- shape
        shape

let internal rcMetadataForTypeAndShape
    (ctx: TypeContext)
    (typ: AST.SemanticType)
    (shape: RcShape)
    : RcMetadata =
    let canonicalType = canonicalRcSourceType ctx typ
    match ctx.TypePlanning.Metadata.TryGetValue canonicalType with
    | true, metadata -> metadata
    | false, _ ->
        let releasePlan = rcShapeReleasePlan shape
        let metadata = {
            ReleasePlanCacheKey = rcReleasePlanCacheKey canonicalType releasePlan
            ReleasePlan = Some releasePlan
            SourceType = Some canonicalType
        }
        ctx.TypePlanning.Metadata.[canonicalType] <- metadata
        metadata

let internal shapeNeedsManagedAliasRootPreservation (ctx: TypeContext) (typ: AST.SemanticType) : bool =
    typ |> rcShapeForType ctx |> rcShapeNeedsManagedAliasRootPreservation

let internal bindingNeedsShapeAutomaticDec
    (ctx: TypeContext)
    (cexpr: CExpr)
    (typ: AST.SemanticType)
    (shape: RcShape)
    : bool =
    rcShapeNeedsAutomaticBindingDec shape
    || match typ, cexpr with
       | AST.TFunction _, ClosureAlloc _ -> true
       | AST.TFunction _, Call (funcName, _) ->
           match Map.tryFind funcName ctx.FuncReg with
           | Some (name, _) -> not (name.StartsWith("Darklang.Stdlib."))
           | None -> true
       | AST.TFunction _, ClosureCall _ -> true
       | _ -> false

/// Values whose runtime representation is known to fail the RC helper's
/// dynamic-root guard, so emitting the helper call cannot affect ownership.
let internal cexprProducesNonRcSentinel (cexpr: CExpr) : bool =
    match cexpr with
    | Atom (StringLiteral _)
    | TypedAtom (StringLiteral _, _) ->
        true
    | TypedAtom (IntLiteral (Int64 0L), AST.TList _) ->
        true
    | _ ->
        false

/// Carry fixed-root metadata with the ownership obligation that created it.
/// One obligation can be emitted on several return paths; deriving the same
/// canonical type and recursive release plan at each use is duplicate work.
