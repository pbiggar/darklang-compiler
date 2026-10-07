(* RcShapePlanning.ml - Select canonical representation shapes for ANF ownership decisions. *)
[@@@warning "-4"]

module F = RcTypeFacts

let canonicalRcType ctx typ =
  let rec canonicalize = function
    | AST.TRecord (name, []) when StringOrder.Map.mem name ctx.F.sumShapeReg ->
        AST.TSum (name, [])
    | AST.TSum (name, []) when StringOrder.Map.mem name ctx.F.sumShapeReg ->
        AST.TSum (name, [])
    | AST.TRecord (name, args) -> AST.TRecord (name, List.map canonicalize args)
    | AST.TSum (name, args) -> AST.TSum (name, List.map canonicalize args)
    | AST.TFunction (args, ret) ->
        AST.TFunction (List.map canonicalize args, canonicalize ret)
    | AST.TTuple elems -> AST.TTuple (List.map canonicalize elems)
    | AST.TList elem -> AST.TList (canonicalize elem)
    | AST.TStream elem -> AST.TStream (canonicalize elem)
    | AST.TDict (key, value) -> AST.TDict (canonicalize key, canonicalize value)
    | ( AST.TVar _ | AST.TInferenceVar _ | AST.TInt8 | AST.TInt16 | AST.TInt32
      | AST.TInt64 | AST.TInt128 | AST.TInt | AST.TUInt8 | AST.TUInt16
      | AST.TUInt32 | AST.TUInt64 | AST.TUInt128 | AST.TBool | AST.TFloat64
      | AST.TString | AST.TBlob | AST.TChar | AST.TDateTime | AST.TUnit
      | AST.TInternalRawPtr | AST.TNever ) as typ ->
        typ
  in
  canonicalize typ

let canonicalRcTypeForShape = canonicalRcType
let canonicalRcSourceType = canonicalRcType

let rcShapeForType ctx typ =
  match Hashtbl.find_opt ctx.F.typePlanning.F.shapes typ with
  | Some shape -> shape
  | None ->
      let records, params =
        match ctx.F.typePlanning.F.recordRegistries with
        | Some registries -> registries
        | None ->
            let registries =
              ( TypeRegistries.recordFieldsRegistry ctx.F.typeReg,
                TypeRegistries.recordTypeParamsRegistry ctx.F.typeReg )
            in
            ctx.F.typePlanning.F.recordRegistries <- Some registries;
            registries
      in
      let shape =
        MemoryPlanning.rcShapeOfTypeWithSums records params ctx.F.sumShapeReg
          (canonicalRcTypeForShape ctx typ)
      in
      Hashtbl.replace ctx.F.typePlanning.F.shapes typ shape;
      shape

let rcMetadataForTypeAndShape ctx typ shape =
  let typ = canonicalRcSourceType ctx typ in
  match Hashtbl.find_opt ctx.F.typePlanning.F.metadata typ with
  | Some metadata -> metadata
  | None ->
      let releasePlan = MemoryPlanning.rcShapeReleasePlan shape in
      let metadata =
        {
          MemoryModel.releasePlanCacheKey =
            ReleasePlanFingerprint.rcReleasePlanCacheKey typ releasePlan;
          releasePlan = Some releasePlan;
          sourceType = Some typ;
        }
      in
      Hashtbl.replace ctx.F.typePlanning.F.metadata typ metadata;
      metadata

let shapeNeedsManagedAliasRootPreservation ctx typ =
  MemoryPlanning.rcShapeNeedsManagedAliasRootPreservation
    (rcShapeForType ctx typ)

(*
   Raw list reads retain their result before returning it. A caller
   that consumes the closure must release that returned ownership.
*)
let bindingNeedsShapeAutomaticDec ctx expr typ shape =
  MemoryPlanning.rcShapeNeedsAutomaticBindingDec shape
  ||
  match (typ, expr) with
  | AST.TFunction _, ANF.ClosureAlloc _ -> true
  | AST.TFunction _, ANF.Call (func, _) -> (
      match FunctionIdMap.tryFind func ctx.F.funcReg with
      | Some (name, _)
        when String.starts_with ~prefix:"Darklang.Stdlib.List.__treeValue" name
        ->
          true
      | Some (name, _) ->
          not (String.starts_with ~prefix:"Darklang.Stdlib." name)
      | None -> true)
  | AST.TFunction _, ANF.ClosureCall _ -> true
  | _ -> false

(*
   Values whose runtime representation is known to fail the RC helper's
   dynamic-root guard, so emitting the helper call cannot affect ownership.
   Carry fixed-root metadata with the ownership obligation that created it.
   One obligation can be emitted on several return paths; deriving the same
   canonical type and recursive release plan at each use is duplicate work.
*)
let cexprProducesNonRcSentinel = function
  | ANF.Atom (ANF.StringLiteral _)
  | ANF.TypedAtom (ANF.StringLiteral _, _)
  | ANF.TypedAtom (ANF.IntLiteral (ANF.Int64 0L), AST.TList _) ->
      true
  | _ -> false
