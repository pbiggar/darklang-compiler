open Dark_compiler
open AST
open! MemoryModel
module P = MemoryPlanning
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let typ = SemanticAST.semanticType
let shape = SemanticANF.memoryModel_rcShape
let plan = SemanticANF.memoryModel_rcReleasePlan
let typeSet values = `Assoc ["set", list typ (P.SemanticTypeSet.elements values)]
let facts value =
 let release = P.rcShapeReleasePlan value in
 tuple [shape value; `Bool (P.rcShapeNeedsOwnedScopeRelease value); `Bool (P.rcShapeIsRootManaged value); `Bool (P.rcShapeNeedsRecursiveRelease value);
 option SemanticANF.memoryModel_rcKind (P.rcShapeRootKind value); option SemanticJson.int32 (P.rcShapePayloadSize value);
 SemanticANF.memoryModel_rcStorageClass (P.rcShapeStorageClass value); `Bool (P.rcShapeIsOwnershipTransferRoot value);
 option SemanticANF.memoryModel_rcOperation (P.rcShapeRetainOperation value); option SemanticANF.memoryModel_rcOperation (P.rcShapeReleaseOperation value);
 `Bool (P.rcShapeNeedsBorrowedRetain value); `Bool (P.rcShapeNeedsAutomaticBindingDec value); `Bool (P.rcShapeNeedsManagedAliasRootPreservation value); plan release; typeSet (P.recursiveReleaseTypes release)]
let observe source =
 let records = M.of_list ["R", [source, TString; "next", TRecord ("R", [])]; "G", ["value", TVar "a"; "next", TList (TRecord ("G", [TVar "a"]))]] in
 let parameters = M.of_list ["R", []; "G", ["a"]] in
 let info parameters payloads unary = {typeParams = parameters; payloads; unaryPayloadTags = IntSet.of_list unary} in
 let sums = M.of_list ["Empty", info [] [] []; "Null", info ["a"] [4, Some (TVar "a"); 3, None] [4];
 "One", info ["a"] [7, Some (TVar "a")] [7]; "Many", info [] [5, Some (TTuple [TString; TSum ("Many", [])]); 2, None; 8, Some TInt128] [8];
 "Binary", info [] [2, Some (TTuple [TString; TInt64])] []; "Unknown", info [] [0, Some (TVar "unresolved")] [0]] in
 let primitives = [TInt8; TInt16; TInt32; TInt64; TInt128; TInt; TUInt8; TUInt16; TUInt32; TUInt64; TUInt128; TBool; TFloat64; TDateTime; TUnit; TNever; TVar source; TInferenceVar (source, "id"); TString; TChar; TBlob; TInternalRawPtr; TFunction ([TString], TInt64); TList TString; TStream TString; TDict (TString, TInt128); TTuple [TString; TInt64]; TRecord ("R", [])] in
 let sumTypes = List.concat_map (fun value -> [TSum ("Null", [value]); TSum ("One", [value])]) primitives @ [TSum ("Empty", []); TSum ("Many", []); TSum ("Binary", []); TSum ("Unknown", []); TRecord ("Many", []); TSum ("R", []); TRecord ("G", [TString])] in
 let basic = primitives @ [TSum ("X", []); TSum ("X", [TString]); TSum ("X", [TString; TInt64])] in
 let simpleShapes = [Immediate; StaticString; RawUnmanaged; DynamicString; DynamicBlob; DynamicInt; StreamRoot; FixedBlock (32, [Immediate; DynamicString; RecursiveNominalRef (TRecord (source, [])); ClosureShape []]);
 BoxedSum (16, [8, DynamicString; 8, RecursiveNominalRef (TSum (source, []))], [{tag=3;fieldShapes=[8, RecursiveNominalRef (TSum ("OnlyVariant", []))]}]);
 TaggedListShape (RecursiveNominalRef (TRecord (source, []))); DictRoot (DynamicString, FixedBlock (8, [DynamicBlob])); ClosureShape [DynamicString; RecursiveNominalRef (TRecord (source, []))]; RecursiveNominalRef (TSum (source, []))] in
 tuple [list (fun value -> tuple [typ value; `Bool (P.canUseTransparentSumPayload value); facts (P.rcShapeOfType records value); plan (P.rcReleasePlanOfType records value)]) basic;
 list (fun value -> tuple [typ value; option typ (P.nullablePointerSumPayloadType sums value); `Bool (P.isNullablePointerSumType sums value); `Bool (P.isSpareImmediateSumType sums value); facts (P.rcShapeOfTypeWithSums records parameters sums value); plan (P.rcReleasePlanOfTypeWithSums records sums value)]) (primitives @ sumTypes);
 list facts simpleShapes;
 `Assoc ["map", list (fun (name, values) -> tuple [SemanticJson.string name; list SemanticJson.string values]) (M.bindings (P.inferredRecordTypeParamsRegistry records))]]
