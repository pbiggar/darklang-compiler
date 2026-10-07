(* EscapeAnalysisFacts.ml - Type and destruction proofs shared by SSA escape analysis. *)
[@@@warning "-4"]
module M = StringOrder.Map
type scalarAggregate = {fields : ANF.atom list [@warning "-69"]} [@@warning "-34"]
let isScalarType = function
 | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
 | AST.TBool | AST.TFloat64 | AST.TDateTime | AST.TUnit | AST.TNever -> true
 | _ -> false
(* Prove that releasing a displaced field cannot run a language-visible
   finalizer. Nominal records and sums use complete registry metadata and
   concrete type arguments. Regular recursive cycles are admitted
   coinductively; sums are considered only for boxed-sum reuse candidates, and
   type-growing recursion and closures fail closed. *)
let hasNonObservableDestruction (typeReg : TypeRegistries.typeRegistry)
 (sumReg : MemoryModel.rcSumShapeRegistry) allowSums typ =
 let rec prove expandingRecords expandingSums typ =
  isScalarType typ || match typ with
  | AST.TString | AST.TBlob | AST.TInt -> true
  | AST.TTuple elements -> List.for_all (prove expandingRecords expandingSums) elements
  | AST.TList element -> prove expandingRecords expandingSums element
  | AST.TDict (key, value) ->
    prove expandingRecords expandingSums key && prove expandingRecords expandingSums value
  | AST.TRecord (name, typeArgs) ->
    let recordType = AST.TRecord (name, typeArgs) in
    (match M.find_opt name expandingRecords with
    | Some expandingType -> AST.compareSemanticType expandingType recordType = 0
    | None -> (match M.find_opt name typeReg with
      | Some info when List.length info.TypeRegistries.typeParams = List.length typeArgs ->
        let subst = M.of_list (List.combine info.TypeRegistries.typeParams typeArgs) in
        let expandingRecords = M.add name recordType expandingRecords in
        List.for_all (fun (_, fieldType) ->
          prove expandingRecords expandingSums (TypeSubstitution.applySubstToType subst fieldType)) info.TypeRegistries.fields
      | None when allowSums && M.mem name sumReg ->
        prove expandingRecords expandingSums (AST.TSum (name, typeArgs))
      | _ -> false))
  | AST.TSum (name, typeArgs) when allowSums ->
    let sumType = AST.TSum (name, typeArgs) in
    (match M.find_opt name expandingSums with
    | Some expandingType -> AST.compareSemanticType expandingType sumType = 0
    | None -> (match M.find_opt name sumReg with
      | Some info when List.length info.MemoryModel.typeParams = List.length typeArgs ->
        let subst = M.of_list (List.combine info.MemoryModel.typeParams typeArgs) in
        let expandingSums = M.add name sumType expandingSums in
        List.for_all (fun (_, payload) -> match payload with
          | None -> true
          | Some payloadType -> prove expandingRecords expandingSums (TypeSubstitution.applySubstToType subst payloadType)) info.MemoryModel.payloads
      | _ -> false))
  | _ -> false in
 prove M.empty M.empty typ
let descriptorHasNonObservableDestruction typeReg sumReg (descriptor : ANF.recordDescriptor) =
 let allowSums = match descriptor.ANF.valueType with AST.TSum _ -> true | _ -> false in
 List.for_all (fun (_, typ) -> hasNonObservableDestruction typeReg sumReg allowSums typ) descriptor.ANF.fields
