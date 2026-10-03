(* MemoryShapeTests.fs - Verify memory shapes and stable recursive release plans. *)
[@@@warning "-4-42"]
open Dark_compiler
open MemoryModel
open MemoryPlanning
open ReleasePlanFingerprint
type testResult = (unit,string) result
module F = ANFTestFormatting
let format = HostStructuralFormat.format
let shape x=format (F.memoryModel_rcShape x)
let plan x=format (F.memoryModel_rcReleasePlan x)
let typ = HostStructuralFormat.semanticType
let option enc value=format (match value with None -> StructuralValue.Union ("None",[]) | Some value -> StructuralValue.Union ("Some",[enc value]))
let testRcShapeConstructionAndEquality () =
 let tupleShape=FixedBlock (16,[Immediate;DynamicString]) in let dictShape=DictRoot (DynamicString,TaggedListShape Immediate) in
 let closureShape=ClosureShape [tupleShape;dictShape] in let expected=ClosureShape [FixedBlock (16,[Immediate;DynamicString]);DictRoot (DynamicString,TaggedListShape Immediate)] in
 if closureShape=expected then Ok () else Error ("Expected RcShape equality to use structural representation, got: " ^ shape closureShape)
let testRcShapeClassifiesPrimitivesAsImmediate () =
 let types=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TBool;AST.TFloat64;AST.TUnit;AST.TNever;AST.TVar "a"] in
 match List.find_opt (fun t -> rcShapeOfType StringOrder.Map.empty t<>Immediate) types with None -> Ok () | Some t -> Error ("Expected primitive type " ^ typ t ^ " to classify as Immediate")
let testRcShapeClassifiesManagedIntegerBuffers () =
 let samples=[AST.TInt,DynamicInt;AST.TInt128,FixedBlock (16,[]);AST.TUInt128,FixedBlock (16,[])] in
 match List.find_opt (fun (t,expected) -> rcShapeOfType StringOrder.Map.empty t<>expected) samples with None -> Ok () | Some (t,expected) -> Error ("Expected integer buffer type " ^ typ t ^ " to classify as " ^ shape expected)
let testRcShapeClassifiesTuplesAndRecordsAsFixedBlocks () =
 let reg=StringOrder.Map.singleton "Pair" ["left",AST.TInt64;"right",AST.TString] in
 let tupleShape=rcShapeOfType reg (AST.TTuple [AST.TInt64;AST.TString;AST.TBool]) and recordShape=rcShapeOfType reg (AST.TRecord ("Pair",[])) in
 match tupleShape,recordShape with FixedBlock (24,[Immediate;DynamicString;Immediate]),FixedBlock (16,[Immediate;DynamicString]) -> Ok () | _ -> Error ("Unexpected fixed-block shapes. tuple=" ^ shape tupleShape ^ "; record=" ^ shape recordShape)
let testRcShapeClassifiesRemainingRuntimeShapes () =
 let samples=[AST.TString,DynamicString;AST.TChar,DynamicString;AST.TBlob,DynamicBlob;AST.TInternalRawPtr,RawUnmanaged;AST.TFunction ([AST.TInt64],AST.TString),ClosureShape [];AST.TSum ("Color",[]),Immediate;AST.TSum ("Option",[AST.TString]),BoxedSum (16,[8,DynamicString],[]);AST.TList AST.TString,TaggedListShape DynamicString;AST.TDict (AST.TString,AST.TList AST.TInt64),DictRoot (DynamicString,TaggedListShape Immediate)] in
 match List.find_opt (fun (t,expected) -> rcShapeOfType StringOrder.Map.empty t<>expected) samples with None -> Ok () | Some (t,expected) -> Error ("Expected " ^ typ t ^ " to classify as " ^ shape expected ^ ", got " ^ shape (rcShapeOfType StringOrder.Map.empty t))
let testRcShapeClassifiesSumsWithVariantMetadata () =
 let reg=StringOrder.Map.singleton "PayloadRecord" ["name",AST.TString] in
 let variants : rcSumShapeRegistry = StringOrder.Map.of_list ["Enum",{typeParams=[];payloads=[0,None;1,None];unaryPayloadTags=IntSet.empty};"Maybe",{typeParams=["a"];payloads=[0,None;1,Some (AST.TVar "a")];unaryPayloadTags=IntSet.singleton 1};"Packet",{typeParams=[];payloads=[0,Some (AST.TRecord ("PayloadRecord",[]));1,Some AST.TBlob];unaryPayloadTags=IntSet.of_list [0;1]}] in
 let samples=[AST.TSum ("Enum",[]),Immediate;AST.TSum ("Maybe",[AST.TString]),DynamicInt;AST.TSum ("Packet",[]),BoxedSum (16,[8,FixedBlock (8,[DynamicString]);8,DynamicBlob],[{tag=0;fieldShapes=[8,FixedBlock (8,[DynamicString])]};{tag=1;fieldShapes=[8,DynamicBlob]}])] in
 let parameters=inferredRecordTypeParamsRegistry reg in
 match List.find_opt (fun (t,expected) -> rcShapeOfTypeWithSums reg parameters variants t<>expected) samples with None -> Ok () | Some (t,expected) -> Error ("Expected variant-aware shape for " ^ typ t ^ " to be " ^ shape expected ^ ", got " ^ shape (rcShapeOfTypeWithSums reg parameters variants t))
let testRcShapeOwnershipHelpersClassifyManagedRoots () =
 let managed=[DynamicString;DynamicBlob;FixedBlock (16,[Immediate;DynamicString]);BoxedSum (16,[],[]);TaggedListShape DynamicString;DictRoot (DynamicString,Immediate);ClosureShape [DynamicString]] in let unmanaged=[Immediate;StaticString;RawUnmanaged] in
 match List.find_opt (fun s -> not (rcShapeNeedsOwnedScopeRelease s)) managed with Some s -> Error ("Expected managed shape " ^ shape s ^ " to need owned scope release") | None -> match List.find_opt rcShapeNeedsOwnedScopeRelease unmanaged with Some s -> Error ("Expected unmanaged shape " ^ shape s ^ " to skip owned scope release") | None -> Ok ()
let testRcShapeOwnershipHelpersClassifyAutomaticBindingDecs () =
 let automatic=[DynamicString;DynamicBlob;FixedBlock (16,[Immediate;DynamicString]);BoxedSum (16,[],[]);TaggedListShape DynamicString;DictRoot (DynamicString,Immediate)] in let skipped=[Immediate;StaticString;RawUnmanaged;ClosureShape [DynamicString]] in
 match List.find_opt (fun s -> not (rcShapeNeedsAutomaticBindingDec s)) automatic with Some s -> Error ("Expected shape " ^ shape s ^ " to need automatic binding dec") | None -> match List.find_opt rcShapeNeedsAutomaticBindingDec skipped with Some s -> Error ("Expected shape " ^ shape s ^ " to skip automatic binding dec") | None -> Ok ()
let testRcShapeOwnershipHelpersClassifyBorrowedRetains () =
 let retained=[DynamicString;DynamicBlob;FixedBlock (16,[Immediate;DynamicString]);BoxedSum (16,[],[]);TaggedListShape DynamicString;DictRoot (DynamicString,Immediate);ClosureShape [DynamicString]] in let skipped=[Immediate;StaticString;RawUnmanaged] in
 match List.find_opt (fun s -> not (rcShapeNeedsBorrowedRetain s)) retained with Some s -> Error ("Expected borrowed shape " ^ shape s ^ " to need retain when materializing ownership") | None -> match List.find_opt rcShapeNeedsBorrowedRetain skipped with Some s -> Error ("Expected borrowed shape " ^ shape s ^ " to skip retain") | None -> Ok ()
let testRcShapeOwnershipHelpersSelectRootDispatch () =
 let samples=[FixedBlock (16,[DynamicString]),Some GenericHeap;BoxedSum (16,[],[]),Some GenericHeap;TaggedListShape DynamicString,Some TaggedList;TaggedListShape (ClosureShape []),Some TaggedList;DictRoot (Immediate,DynamicString),Some DictHeap;ClosureShape [DynamicString],Some ClosureHeap;Immediate,None;DynamicString,None;DynamicBlob,None;RawUnmanaged,None] in
 match List.find_opt (fun (s,expected) -> rcShapeRootKind s<>expected) samples with None -> Ok () | Some (s,expected) -> Error ("Expected shape " ^ shape s ^ " to use root kind " ^ option F.memoryModel_rcKind expected ^ ", got " ^ option F.memoryModel_rcKind (rcShapeRootKind s))
let testRcShapeOwnershipHelpersSelectRetainReleaseOperations () =
 let samples=[FixedBlock (16,[DynamicString]),Some (FixedSizeRoot (16,GenericHeap));BoxedSum (16,[],[]),Some (FixedSizeRoot (16,GenericHeap));TaggedListShape DynamicString,Some (FixedSizeRoot (24,TaggedList));TaggedListShape (ClosureShape []),Some (FixedSizeRoot (24,TaggedList));DictRoot (Immediate,DynamicString),Some (FixedSizeRoot (8,DictHeap));ClosureShape [DynamicString],Some (FixedSizeRoot (0,ClosureHeap));DynamicString,Some DynamicStringBuffer;DynamicBlob,Some DynamicBlobBuffer;Immediate,None;StaticString,None;RawUnmanaged,None] in
 match List.find_opt (fun (s,expected) -> rcShapeRetainOperation s<>expected) samples with
 | Some (s,expected) -> Error ("Expected shape " ^ shape s ^ " to use retain operation " ^ option F.memoryModel_rcOperation expected ^ ", got " ^ option F.memoryModel_rcOperation (rcShapeRetainOperation s))
 | None -> match List.find_opt (fun (s,expected) -> rcShapeReleaseOperation s<>expected) samples with Some (s,expected) -> Error ("Expected shape " ^ shape s ^ " to use release operation " ^ option F.memoryModel_rcOperation expected ^ ", got " ^ option F.memoryModel_rcOperation (rcShapeReleaseOperation s)) | None -> Ok ()
let testRcShapeOwnershipHelpersClassifyStorage () =
 let samples=[FixedBlock (16,[DynamicString]),ManagedRcRoot (16,GenericHeap);BoxedSum (16,[],[]),ManagedRcRoot (16,GenericHeap);TaggedListShape DynamicString,ManagedRcRoot (24,TaggedList);TaggedListShape (ClosureShape []),ManagedRcRoot (24,TaggedList);DictRoot (Immediate,DynamicString),ManagedRcRoot (8,DictHeap);ClosureShape [DynamicString],ManagedRcRoot (0,ClosureHeap);DynamicString,ManagedDynamicBuffer DynamicStringBuffer;DynamicBlob,ManagedDynamicBuffer DynamicBlobBuffer;Immediate,UnmanagedStorage;StaticString,UnmanagedStorage;RawUnmanaged,UnmanagedStorage] in
 match List.find_opt (fun (s,expected) -> rcShapeStorageClass s<>expected) samples with None -> Ok () | Some (s,expected) -> Error ("Expected shape " ^ shape s ^ " to use storage class " ^ format (F.memoryModel_rcStorageClass expected) ^ ", got " ^ format (F.memoryModel_rcStorageClass (rcShapeStorageClass s)))
let testRcShapeOwnershipHelpersClassifyRootManagement () =
 let managed=[FixedBlock (16,[Immediate;DynamicString]);BoxedSum (16,[],[]);TaggedListShape DynamicString;DictRoot (DynamicString,TaggedListShape Immediate);ClosureShape [DynamicString]] in let nonRoot=[Immediate;DynamicString;DynamicBlob;StaticString;RawUnmanaged] in
 match List.find_opt (fun s -> not (rcShapeIsRootManaged s)) managed with Some s -> Error ("Expected shape " ^ shape s ^ " to be a managed RC root") | None -> match List.find_opt rcShapeIsRootManaged nonRoot with Some s -> Error ("Expected shape " ^ shape s ^ " not to be a managed RC root") | None -> Ok ()
let testRcShapeOwnershipHelpersClassifyOwnershipTransferRoots () =
 let roots=[FixedBlock (16,[Immediate;DynamicString]);BoxedSum (16,[],[]);TaggedListShape DynamicString;DictRoot (DynamicString,Immediate);ClosureShape [DynamicString]] in let nonRoots=[Immediate;DynamicString;DynamicBlob;StaticString;RawUnmanaged] in
 match List.find_opt (fun s -> not (rcShapeIsOwnershipTransferRoot s)) roots with Some s -> Error ("Expected shape " ^ shape s ^ " to be an ownership-transfer root") | None -> match List.find_opt rcShapeIsOwnershipTransferRoot nonRoots with Some s -> Error ("Expected shape " ^ shape s ^ " not to be an ownership-transfer root") | None -> Ok ()
let testRcShapeOwnershipHelpersClassifyRecursiveRelease () =
 let recursive=[FixedBlock (16,[Immediate;DynamicString]);BoxedSum (16,[8,DynamicString],[]);TaggedListShape (FixedBlock (8,[DynamicString]));DictRoot (DynamicString,TaggedListShape Immediate);ClosureShape [DynamicString]] in let nonRecursive=[Immediate;DynamicString;DynamicBlob;StaticString;RawUnmanaged;FixedBlock (8,[Immediate]);TaggedListShape Immediate;DictRoot (Immediate,Immediate);ClosureShape []] in
 match List.find_opt (fun s -> not (rcShapeNeedsRecursiveRelease s)) recursive with Some s -> Error ("Expected shape " ^ shape s ^ " to need recursive release") | None -> match List.find_opt rcShapeNeedsRecursiveRelease nonRecursive with Some s -> Error ("Expected shape " ^ shape s ^ " not to need recursive release") | None -> Ok ()
let testRcShapeReleasePlanClassifiesFieldCleanup () =
 let samples=[Immediate,NoReleasePlan;StaticString,NoReleasePlan;RawUnmanaged,NoReleasePlan;DynamicString,DynamicBufferRelease DynamicStringBuffer;DynamicBlob,DynamicBufferRelease DynamicBlobBuffer;TaggedListShape DynamicString,RootRelease (24,TaggedList,TaggedListPayloadRelease (DynamicBufferRelease DynamicStringBuffer));DictRoot (DynamicString,FixedBlock (8,[DynamicBlob])),RootRelease (8,DictHeap,DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer,RootRelease (8,GenericHeap,FixedBlockPayloadRelease (8,[FieldRelease (0,DynamicBufferRelease DynamicBlobBuffer)]))));FixedBlock (16,[Immediate;DynamicString]),RootRelease (16,GenericHeap,FixedBlockPayloadRelease (16,[FieldRelease (8,DynamicBufferRelease DynamicStringBuffer)]));ClosureShape [DynamicString],RootRelease (0,ClosureHeap,ClosurePayloadRelease [FieldRelease (0,DynamicBufferRelease DynamicStringBuffer)]);BoxedSum (16,[],[]),RootRelease (16,GenericHeap,BoxedSumPayloadRelease (16,[],[]))] in
 match List.find_opt (fun (s,expected) -> rcShapeReleasePlan s<>expected) samples with None -> Ok () | Some (s,expected) -> Error ("Expected shape " ^ shape s ^ " to use release plan " ^ plan expected ^ ", got " ^ plan (rcShapeReleasePlan s))
let testRcSourceTypeFingerprintIsStructuralAndStable () =
 let samples=[AST.TInt64;AST.TString;AST.TList AST.TString;AST.TTuple [AST.TString;AST.TInt64];AST.TTuple [AST.TInt64;AST.TString];AST.TRecord ("Pair",[AST.TString;AST.TInt64]);AST.TSum ("Pair",[AST.TString;AST.TInt64]);AST.TDict (AST.TString,AST.TList AST.TBlob)] in
 let fingerprints=List.map rcSourceTypeFingerprint samples in
 if fingerprints<>List.map rcSourceTypeFingerprint samples then Error "RC source-type fingerprints were not deterministic"
 else if List.length (List.sort_uniq StringOrder.compare fingerprints)<>List.length samples then Error ("Distinct RC source types produced duplicate fingerprints: " ^ format (StructuralValue.Sequence (List.map2 (fun t hash -> StructuralValue.Tuple [HostStructuralFormat.semanticValue t;StructuralValue.Text hash]) samples fingerprints))) else Ok ()
let testRcReleasePlanFingerprintIsCompositionalAndStable () =
 let nestedList=RootRelease (24,TaggedList,TaggedListPayloadRelease (DynamicBufferRelease DynamicStringBuffer)) in
 let samples=[NoReleasePlan;DynamicBufferRelease DynamicBlobBuffer;RecursiveRelease (AST.TSum ("Tree",[AST.TString]));nestedList;RootRelease (16,DictHeap,DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer,nestedList));RootRelease (24,GenericHeap,BoxedSumPayloadRelease (24,[FieldRelease (8,nestedList)],[{tag=0;fieldReleases=[]};{tag=1;fieldReleases=[FieldRelease (16,DynamicBufferRelease DynamicBlobBuffer)]}]))] in
 let directChildren = function RootRelease (_,_,payload) -> (match payload with
 | NoPayloadRelease -> []
 | FixedBlockPayloadRelease (_,fields) | ClosurePayloadRelease fields -> List.map (fun (FieldRelease (_,child)) -> child) fields
 | BoxedSumPayloadRelease (_,fields,variants) -> let fieldChildren=List.map (fun (FieldRelease (_,child)) -> child) fields in let variantChildren=List.concat_map (fun (variant : rcBoxedSumVariantRelease) -> List.map (fun (FieldRelease (_,child)) -> child) variant.fieldReleases) variants in fieldChildren @ variantChildren
 | TaggedListPayloadRelease element -> [element] | DictPayloadRelease (key,value) -> [key;value]) | NoReleasePlan | DynamicBufferRelease _ | RecursiveRelease _ -> [] in
 let composed=List.map (fun p -> rcReleasePlanFingerprintString (rcReleasePlanFingerprintHashFromChildren p (List.map rcReleasePlanFingerprintHash (directChildren p)))) samples in
 let recursive=List.map rcReleasePlanFingerprint samples in
 if composed<>recursive then Error "Composed RC release-plan fingerprints differed from recursive fingerprints"
 else if recursive<>List.map rcReleasePlanFingerprint samples then Error "RC release-plan fingerprints were not deterministic"
 else if List.length (List.sort_uniq StringOrder.compare recursive)<>List.length samples then Error ("Distinct RC release plans produced duplicate fingerprints: " ^ format (StructuralValue.Sequence (List.map2 (fun p hash -> StructuralValue.Tuple [F.memoryModel_rcReleasePlan p;StructuralValue.Text hash]) samples recursive))) else Ok ()
let testRcReleasePlanCacheKeyOnlyFingerprintsLargePlans () =
 let smallType=AST.TTuple [AST.TString;AST.TInt64] in let smallPlan=rcReleasePlanOfType StringOrder.Map.empty smallType in
 let largeType=AST.TTuple (List.init 30 (fun _ -> AST.TString)) in let largePlan=rcReleasePlanOfType StringOrder.Map.empty largeType in
 match rcReleasePlanCacheKey smallType smallPlan,rcReleasePlanCacheKey largeType largePlan with None,Some key when key=rcSourceTypeFingerprint largeType -> Ok () | small,large -> Error ("Expected only the large release plan to use a compact key, got small=" ^ option (fun x -> StructuralValue.Text x) small ^ ", large=" ^ option (fun x -> StructuralValue.Text x) large)
let testRcReleasePlanOfTypeUsesRecordMetadata () =
 let reg=StringOrder.Map.singleton "Packet" ["header",AST.TInt64;"body",AST.TString;"tail",AST.TBlob] in
 let expected=RootRelease (24,GenericHeap,FixedBlockPayloadRelease (24,[FieldRelease (8,DynamicBufferRelease DynamicStringBuffer);FieldRelease (16,DynamicBufferRelease DynamicBlobBuffer)])) in
 let actual=rcReleasePlanOfType reg (AST.TRecord ("Packet",[])) in if actual=expected then Ok () else Error ("Expected record type to use release plan " ^ plan expected ^ ", got " ^ plan actual)
let testRcReleasePlanOfTypeUsesSumPayloadMetadata () =
 let sumType=AST.TSum ("MaybeString",[AST.TString]) in let expected=RootRelease (16,GenericHeap,BoxedSumPayloadRelease (16,[FieldRelease (8,DynamicBufferRelease DynamicStringBuffer)],[])) in
 let actual=rcReleasePlanOfType StringOrder.Map.empty sumType in if actual=expected then Ok () else Error ("Expected sum type to use release plan " ^ plan expected ^ ", got " ^ plan actual)
let testRcReleasePlanOfTypeWithSumsUsesVariantMetadata () =
 let reg=StringOrder.Map.singleton "PayloadRecord" ["name",AST.TString;"blob",AST.TBlob] in
 let sums : rcSumShapeRegistry = StringOrder.Map.of_list ["Color",{typeParams=[];payloads=[0,None;1,None;2,None];unaryPayloadTags=IntSet.empty};"Maybe",{typeParams=["a"];payloads=[0,None;1,Some (AST.TVar "a")];unaryPayloadTags=IntSet.singleton 1};"Packet",{typeParams=[];payloads=[0,Some (AST.TRecord ("PayloadRecord",[]));1,Some (AST.TList AST.TString)];unaryPayloadTags=IntSet.of_list [0;1]}] in
 let recordPlan=RootRelease (16,GenericHeap,FixedBlockPayloadRelease (16,[FieldRelease (0,DynamicBufferRelease DynamicStringBuffer);FieldRelease (8,DynamicBufferRelease DynamicBlobBuffer)])) in
 let listPlan=RootRelease (24,TaggedList,TaggedListPayloadRelease (DynamicBufferRelease DynamicStringBuffer)) in
 let samples=[AST.TSum ("Color",[]),NoReleasePlan;AST.TSum ("Maybe",[AST.TString]),DynamicBufferRelease DynamicIntBuffer;AST.TSum ("Packet",[]),RootRelease (16,GenericHeap,BoxedSumPayloadRelease (16,[FieldRelease (8,recordPlan);FieldRelease (8,listPlan)],[{tag=0;fieldReleases=[FieldRelease (8,recordPlan)]};{tag=1;fieldReleases=[FieldRelease (8,listPlan)]}]))] in
 match List.find_opt (fun (t,expected) -> rcReleasePlanOfTypeWithSums reg sums t<>expected) samples with None -> Ok () | Some (t,expected) -> Error ("Expected sum-aware release plan for " ^ typ t ^ " to be " ^ plan expected ^ ", got " ^ plan (rcReleasePlanOfTypeWithSums reg sums t))
let testRecursiveSumReleasePlanUsesTypedBackEdge () =
 let treeType=AST.TSum ("Tree",[AST.TInt64]) in
 let sums : rcSumShapeRegistry = StringOrder.Map.singleton "Tree" {typeParams=["a"];payloads=[0,Some (AST.TVar "a");1,Some (AST.TTuple [AST.TSum ("Tree",[AST.TVar "a"]);AST.TSum ("Tree",[AST.TVar "a"])])];unaryPayloadTags=IntSet.singleton 0} in
 let release=rcReleasePlanOfTypeWithSums StringOrder.Map.empty sums treeType in
 if SemanticTypeSet.equal (recursiveReleaseTypes release) (SemanticTypeSet.singleton treeType) then Ok () else Error ("Expected recursive Tree release plan to contain one typed back-edge, got " ^ plan release)
let testRecursiveRecordReleasePlanUsesTypedBackEdge () =
 let nodeType=AST.TRecord ("RecursiveNode",[]) in let reg=StringOrder.Map.singleton "RecursiveNode" ["value",AST.TInt64;"children",AST.TList nodeType] in
 let release=rcReleasePlanOfTypeWithSums reg StringOrder.Map.empty nodeType in
 if SemanticTypeSet.equal (recursiveReleaseTypes release) (SemanticTypeSet.singleton nodeType) then Ok () else Error ("Expected recursive RecursiveNode release plan to contain one typed back-edge, got " ^ plan release)
let testRcReleasePlanOfTypeClassifiesRemainingRootKinds () =
 let samples=[AST.TSum ("Color",[]),NoReleasePlan;AST.TSum ("MaybeString",[AST.TString]),RootRelease (16,GenericHeap,BoxedSumPayloadRelease (16,[FieldRelease (8,DynamicBufferRelease DynamicStringBuffer)],[]));AST.TFunction ([AST.TInt64],AST.TString),RootRelease (0,ClosureHeap,ClosurePayloadRelease []);AST.TString,DynamicBufferRelease DynamicStringBuffer;AST.TBlob,DynamicBufferRelease DynamicBlobBuffer;AST.TDict (AST.TString,AST.TBlob),RootRelease (8,DictHeap,DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer,DynamicBufferRelease DynamicBlobBuffer));AST.TInternalRawPtr,NoReleasePlan] in
 match List.find_opt (fun (t,expected) -> rcReleasePlanOfType StringOrder.Map.empty t<>expected) samples with None -> Ok () | Some (t,expected) -> Error ("Expected type " ^ typ t ^ " to use release plan " ^ plan expected ^ ", got " ^ plan (rcReleasePlanOfType StringOrder.Map.empty t))
let contains text needle =
 let rec loop i=if i+String.length needle>String.length text then false else String.sub text i (String.length needle)=needle || loop (i+1) in loop 0
let testRcShapeRequiresRecordMetadata () =
 try let _=rcShapeOfType StringOrder.Map.empty (AST.TRecord ("MissingRecordMetadata",[])) in Error "Expected missing record metadata to fail before ownership decisions can fall back to source-level heap checks" with ex when contains (Printexc.to_string ex) "MissingRecordMetadata" -> Ok ()
let testRcShapeWithSumsRequiresSumMetadata () =
 try let _=rcShapeOfTypeWithSums StringOrder.Map.empty StringOrder.Map.empty StringOrder.Map.empty (AST.TSum ("MissingSumMetadata",[])) in Error "Expected missing sum metadata to fail before ownership decisions can fall back to generic boxed sums" with ex when contains (Printexc.to_string ex) "MissingSumMetadata" -> Ok ()

let tests = [
 "RcShape supports structural construction and equality",testRcShapeConstructionAndEquality;
 "RcShape classifies primitives as immediate",testRcShapeClassifiesPrimitivesAsImmediate;
 "RcShape classifies managed integer buffers",testRcShapeClassifiesManagedIntegerBuffers;
 "RcShape classifies tuples and records as fixed blocks",testRcShapeClassifiesTuplesAndRecordsAsFixedBlocks;
 "RcShape classifies remaining runtime shapes",testRcShapeClassifiesRemainingRuntimeShapes;
 "RcShape classifies sums with variant metadata",testRcShapeClassifiesSumsWithVariantMetadata;
 "RcShape ownership helpers classify managed roots",testRcShapeOwnershipHelpersClassifyManagedRoots;
 "RcShape ownership helpers classify automatic binding decs",testRcShapeOwnershipHelpersClassifyAutomaticBindingDecs;
 "RcShape ownership helpers classify borrowed retains",testRcShapeOwnershipHelpersClassifyBorrowedRetains;
 "RcShape ownership helpers select root dispatch",testRcShapeOwnershipHelpersSelectRootDispatch;
 "RcShape ownership helpers select retain/release operations",testRcShapeOwnershipHelpersSelectRetainReleaseOperations;
 "RcShape ownership helpers classify storage",testRcShapeOwnershipHelpersClassifyStorage;
 "RcShape ownership helpers classify managed RC roots",testRcShapeOwnershipHelpersClassifyRootManagement;
 "RcShape ownership helpers classify ownership-transfer roots",testRcShapeOwnershipHelpersClassifyOwnershipTransferRoots;
 "RcShape ownership helpers classify recursive release",testRcShapeOwnershipHelpersClassifyRecursiveRelease;
 "RcShape release plan classifies field cleanup",testRcShapeReleasePlanClassifiesFieldCleanup;
 "Rc source type fingerprints are structural and stable",testRcSourceTypeFingerprintIsStructuralAndStable;
 "Rc release-plan fingerprints are compositional and stable",testRcReleasePlanFingerprintIsCompositionalAndStable;
 "Rc release-plan cache keys are compact only for large plans",testRcReleasePlanCacheKeyOnlyFingerprintsLargePlans;
 "RcReleasePlan of type uses record metadata",testRcReleasePlanOfTypeUsesRecordMetadata;
 "RcReleasePlan of type uses sum payload metadata",testRcReleasePlanOfTypeUsesSumPayloadMetadata;
 "RcReleasePlan of type with sums uses variant metadata",testRcReleasePlanOfTypeWithSumsUsesVariantMetadata;
 "recursive sum release plan uses typed back-edge",testRecursiveSumReleasePlanUsesTypedBackEdge;
 "recursive record release plan uses typed back-edge",testRecursiveRecordReleasePlanUsesTypedBackEdge;
 "RcReleasePlan of type classifies remaining root kinds",testRcReleasePlanOfTypeClassifiesRemainingRootKinds;
 "RcShape requires record metadata",testRcShapeRequiresRecordMetadata;
 "RcShape with sums requires sum metadata",testRcShapeWithSumsRequiresSumMetadata;
]
