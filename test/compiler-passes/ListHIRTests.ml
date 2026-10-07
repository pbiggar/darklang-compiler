(* ListHIRTests.fs - Region eligibility, ownership accounting, and storage budgets. *)
[@@@warning "-4-42"]
open Dark_compiler
module C=CheckedAST
module A=ANF
module L=ListRegion
module H=HIR
module O=OwnedIR
module D=Destruction
module B=C.BindingIdMap
module S=SpecializationIdentity.FunctionSet
module M=StringOrder.Map
module V=StructuralValue
let (let*)=Result.bind
let call name args=C.Call (TestIds.functionIdForName name,NonEmptyList.fromList args)
let binding name=HostText.scalars name |> Array.fold_left (fun hash ch->Int32.add (Int32.mul hash 31l) (Int32.of_int ch)) 17l |> Int32.to_int |> AST.bindingId
let local name=C.Local (binding name)
let bind name value body=C.Let (C.LPVariable (binding name),value,body)
let values count=C.ListLiteral (List.init (max 0 count) (fun index->C.Int64Literal (Int64.of_int (index+1))))
let map input=call "Darklang.Stdlib.List.map_i64_i64" [input;C.Closure (TestIds.functionIdForName "mapCallback",[])]
let reverse input=call "Darklang.Stdlib.List.reverse_i64" [input]
let ownershipBoundary input=call "Darklang.Stdlib.List.__arrayOwnershipBoundary_i64" [input]
let fold input=call "Darklang.Stdlib.List.fold_i64_i64" [input;C.Int64Literal 0L;C.Closure (TestIds.functionIdForName "foldCallback",[])]
let repeat=call "Darklang.Stdlib.List.repeatUnsafe_i64" [C.BigIntLiteral (Z.of_int 3);C.Int64Literal 7L]
let bytes constant:L.allocationBytes={L.constantBytes=constant;runtimeBuffers=H.ValueMap.empty}
let runtimeBytes constant terms:L.allocationBytes={L.constantBytes=constant;runtimeBuffers=H.ValueMap.of_list terms}
let functions=[
 "mapCallback",AST.TFunction ([AST.TInternalRawPtr;AST.TInt64],AST.TInt64);
 "foldCallback",AST.TFunction ([AST.TInternalRawPtr;AST.TInt64;AST.TInt64],AST.TInt64);
 "Darklang.Stdlib.List.map_i64_i64",AST.TFunction ([AST.TList AST.TInt64;AST.TFunction ([AST.TInt64],AST.TInt64)],AST.TList AST.TInt64);
 "Darklang.Stdlib.List.reverse_i64",AST.TFunction ([AST.TList AST.TInt64],AST.TList AST.TInt64);
 "Darklang.Stdlib.List.fold_i64_i64",AST.TFunction ([AST.TList AST.TInt64;AST.TInt64;AST.TFunction ([AST.TInt64;AST.TInt64],AST.TInt64)],AST.TInt64);
 "Darklang.Stdlib.List.__arrayOwnershipBoundary_i64",AST.TFunction ([AST.TList AST.TInt64],AST.TList AST.TInt64);
 "Darklang.Stdlib.List.repeatUnsafe_i64",AST.TFunction ([AST.TInt;AST.TInt64],AST.TList AST.TInt64)] |> List.map (fun (name,typ)->TestIds.functionIdForName name,(name,typ)) |> FunctionIdMap.ofList
let extractWithParameters parameterTypes expression=
 let names=FunctionIdMap.map (fun _ (name,_)->name) functions in
 let infer types expr=LoweringTypeInference.inferTypeCore (LoweringPrimitives.sumMetadataFromVariantLookup M.empty) TypeRegistries.emptyTypeNames expr types M.empty M.empty functions names M.empty in
 ExtractListRegions.tryExtract (S.of_list [TestIds.functionIdForName "mapCallback";TestIds.functionIdForName "foldCallback"]) names parameterTypes infer (fun expr->ClosureAnalysis.freeVars expr ClosureAnalysis.BindingSet.empty) expression
let extract expression=extractWithParameters B.empty expression
let valueId (H.ValueId id)=V.Union ("ValueId",[V.Scalar (string_of_int id)])
let allocationBytes (b:L.allocationBytes)=V.Record ["ConstantBytes",V.Scalar (Int64.to_string b.L.constantBytes);"RuntimeBuffers",V.Union ("map",[V.Sequence (H.ValueMap.bindings b.L.runtimeBuffers |> List.map (fun (id,bytes)->V.Tuple [valueId id;V.Scalar (Int64.to_string bytes)]))])]
let summary (s:L.allocationSummary)=V.Record ["Allocations",V.Scalar (string_of_int s.L.allocations);"AllocatedBytes",allocationBytes s.L.allocatedBytes;"Copies",V.Scalar (string_of_int s.L.copies);"ReusedTransforms",V.Scalar (string_of_int s.L.reusedTransforms);"Releases",V.Scalar (string_of_int s.L.releases)]
let rec budget=function L.Complete s->V.Union ("Complete",[summary s])|L.Conditional (s,a,b,c)->V.Union ("Conditional",[summary s;budget a;budget b;budget c])|L.RuntimeConditional (s,a,b,c)->V.Union ("RuntimeConditional",[summary s;budget a;budget b;budget c])
let equalSummary (a:L.allocationSummary) (b:L.allocationSummary)=a.L.allocations=b.L.allocations && a.L.allocatedBytes.L.constantBytes=b.L.allocatedBytes.L.constantBytes && H.ValueMap.equal Int64.equal a.L.allocatedBytes.L.runtimeBuffers b.L.allocatedBytes.L.runtimeBuffers && a.L.copies=b.L.copies && a.L.reusedTransforms=b.L.reusedTransforms && a.L.releases=b.L.releases
let rec equalBudget a b=match a,b with L.Complete a,L.Complete b->equalSummary a b|L.Conditional (s,a,b,c),L.Conditional (t,x,y,z)|L.RuntimeConditional (s,a,b,c),L.RuntimeConditional (t,x,y,z)->equalSummary s t && equalBudget a x && equalBudget b y && equalBudget c z|_->false
let checkBudget expression expected ()=match extract expression with None->Error "Expected a closed List<Int64> region"|Some region->
 let* ()=ListLiveness.verifyFunctional region in let owned=region |> SelectListStorage.selectStorage |> ElaborateListOwnership.elaborateOwnership in
 let* ()=VerifyListOwnership.verify owned in let actual=ListAllocationBudget.allocationBudget owned in if equalBudget actual expected then Ok () else Error ("Storage budget: expected "^HostStructuralFormat.format (budget expected)^", got "^HostStructuralFormat.format (budget actual))
let checkSummary expression expected=checkBudget expression (L.Complete expected)
let unique=bind "xs" (values 3) (bind "ys" (map (local "xs")) (fold (reverse (local "ys"))))
let shared=bind "xs" (values 3) (bind "ys" (map (local "xs")) (bind "old" (fold (local "xs")) (fold (local "ys"))))
let runtimeUnique=fold (map (ownershipBoundary (values 3)))
let runtimeShared=bind "xs" (values 3) (bind "alias" (local "xs") (bind "ys" (map (ownershipBoundary (local "xs"))) (bind "old" (fold (local "alias")) (fold (local "ys")))))
let rejects expression ()=match extract expression with None->Ok ()|Some _->Error "An unsupported or escaping list entered array storage"
let testLoweredBudget ()=
 let* body,_=AST_to_ANF.toANF unique A.initialVarGen B.empty M.empty M.empty functions M.empty in
 let rec counts=function A.Jump _|A.Return _->0,0|A.Join (_,yes,no)|A.If (_,yes,no)->let a,b=counts yes in let c,d=counts no in a+c,b+d|A.Let (_,operation,tail)->let allocations,releases=counts tail in match operation with A.RawAlloc _->allocations+1,releases|A.RefCountDec _->allocations,releases+1|_->allocations,releases in
 let actual=counts body in if actual=(1,1) then Ok () else let a,b=actual in Error (Printf.sprintf "Expected one native array allocation and release, got (%d, %d)" a b)
let rejectsOwnership operations ()=
 let result:H.value={H.id=H.ValueId 1000;typ=AST.TInt64} in let block:L.ownedBlock={O.body={H.parameters=[];operations=List.concat operations;result}} in
 match VerifyListOwnership.verifyBlockOwnership block with Error _->Ok ()|Ok ()->Error "Ownership verifier accepted an invalid lifetime"
let root=H.ValueId 0
let rootValue:H.value={H.id=root;typ=AST.TList AST.TInt64}
let drops values=List.map (fun id->O.Drop id) values
let construct releases:L.ownedOperation list=O.Evaluate (H.Leaf (L.Construct (rootValue,L.Literal [])))::drops releases
let zero:L.allocationSummary={L.allocations=0;allocatedBytes=bytes 0L;copies=0;reusedTransforms=0;releases=0}
let allocated={zero with L.allocations=1;allocatedBytes=bytes 56L}
let choice yes no=C.If (C.BoolLiteral true,yes,no)
let branchUses=bind "xs" (values 3) (choice (fold (map (local "xs"))) (fold (reverse (local "xs"))))
let branchJoin=bind "xs" (values 3) (bind "selected" (choice (fold (reverse (local "xs"))) (C.Int64Literal 7L)) (fold (local "xs")))
let manyBranches count=bind "xs" (values 3) (List.fold_right (fun index body->bind ("branch"^string_of_int index) (choice (fold (local "xs")) (C.Int64Literal 7L)) body) (List.init count (fun index->index+1)) (fold (local "xs")))
let ownedBlock releases operations:L.ownedBlock=let result:H.value={H.id=H.ValueId 1001;typ=AST.TInt64} in {O.body={H.parameters=[];operations=drops releases@List.concat operations;result}}
let ownedBranch yes no:L.ownedOperation list=let result:H.value={H.id=H.ValueId 1002;typ=AST.TInt64} in let condition:H.operand={H.expression=C.BoolLiteral true;typ=AST.TBool;inputs=B.empty} in [O.Evaluate (H.Branch (result,condition,yes,no))]
let transform releases:L.ownedOperation list=O.Evaluate (H.Leaf (L.Transform ({H.id=H.ValueId 1;typ=AST.TList AST.TInt64},rootValue,(L.Reverse,L.Consume))))::drops releases
let runtimeSharedBlock ():L.ownedBlock=
 let output:H.value={H.id=H.ValueId 1;typ=AST.TList AST.TInt64} in let result:H.value={H.id=H.ValueId 2;typ=AST.TUnit} in
 let element value:H.operand={H.expression=C.Int64Literal value;typ=AST.TInt64;inputs=B.empty} in
 let operations:L.ownedOperation list=[O.Evaluate (H.Leaf (L.Construct (rootValue,L.Literal [element 1L;element 2L;element 3L])));O.Dup root;O.Evaluate (H.Leaf (L.Transform (output,rootValue,(L.Reverse,L.ConsumeOrCopy))));O.Drop root;O.Drop output.H.id] in {O.body={H.parameters=[];operations;result}}
let testRuntimeSharedOwnership ()=runtimeSharedBlock () |> VerifyListOwnership.verifyBlockOwnership
let formatValue (v:H.value)=V.Record ["Id",valueId v.H.id;"Type",HostStructuralFormat.semanticValue v.H.typ]
let formatOperand (v:H.operand)=V.Record ["Expression",CheckedStructuralFormat.value v.H.expression;"Type",HostStructuralFormat.semanticValue v.H.typ;"Inputs",V.Union ("map",[V.Sequence (B.bindings v.H.inputs |> List.map (fun (id,v)->V.Tuple [AST.DiagnosticFormatting.binding id;formatValue v]))])]
let formatContract (c:H.primitiveContract)=
 let alias=function H.NoManagedAlias->V.Union ("NoManagedAlias",[])|H.UnknownManagedAlias->V.Union ("UnknownManagedAlias",[])|H.FreshManaged->V.Union ("FreshManaged",[])|H.MayReuseInput v->V.Union ("MayReuseInput",[formatValue v])|H.MayAliasInputs (v,vs)->V.Union ("MayAliasInputs",[formatValue v;V.Sequence (List.map formatValue vs)]) in
 let formatEffect value=V.Union ((match value with H.MayEvaluateOpaqueSource->"MayEvaluateOpaqueSource"|H.MayAllocate->"MayAllocate"|H.MayFail->"MayFail"|H.MayInvokeUserCode->"MayInvokeUserCode"|H.ReadsOwnedStorage->"ReadsOwnedStorage"|H.WritesOwnedStorage->"WritesOwnedStorage"),[]) in
 V.Record ["Inputs",V.Sequence (List.map formatValue c.H.inputs);"Operands",V.Sequence (List.map formatOperand c.H.operands);"Outputs",V.Sequence (List.map (fun (o:H.outputContract)->V.Record ["Value",formatValue o.H.value;"Alias",alias o.H.alias]) c.H.outputs);"Effects",V.Union ("set",[V.Sequence (List.map formatEffect (H.EffectSet.elements c.H.effects))])]
let testPrimitiveContracts ()=
 let listValue id:H.value={H.id=H.ValueId id;typ=AST.TList AST.TInt64} in let scalarValue:H.value={H.id=H.ValueId 2;typ=AST.TInt64} in let input=listValue 0 in let output=listValue 1 in
 let scalar:H.operand={H.expression=C.Int64Literal 0L;typ=AST.TInt64;inputs=B.empty} in let callback:H.operand={H.expression=C.Closure (TestIds.functionIdForName "mapCallback",[]);typ=AST.TFunction ([AST.TInt64],AST.TInt64);inputs=B.empty} in
 let construct=L.primitiveContract (L.Construct (output,L.Literal [scalar])) in let transform=L.primitiveContract (L.Transform (output,input,(L.Map callback,L.StaticReuse))) in let fold=L.primitiveContract (L.Fold (scalarValue,input,scalar,callback)) in
 let alias (c:H.primitiveContract)=List.map (fun (o:H.outputContract)->o.H.alias) c.H.outputs in
 let effects c xs=H.EffectSet.equal c.H.effects (H.EffectSet.of_list xs) in
 if alias construct<>[H.FreshManaged] || not (effects construct [H.MayEvaluateOpaqueSource;H.MayAllocate]) then Error ("Unexpected construction contract: "^HostStructuralFormat.format (formatContract construct))
 else if alias transform<>[H.MayReuseInput input] || not (effects transform [H.MayEvaluateOpaqueSource;H.MayAllocate;H.MayInvokeUserCode;H.ReadsOwnedStorage;H.WritesOwnedStorage]) then Error ("Unexpected transformation contract: "^HostStructuralFormat.format (formatContract transform))
 else if alias fold<>[H.NoManagedAlias] || not (effects fold [H.MayEvaluateOpaqueSource;H.MayInvokeUserCode;H.ReadsOwnedStorage]) then Error ("Unexpected fold contract: "^HostStructuralFormat.format (formatContract fold)) else Ok ()
let testScopeTransitive ()=
 let fid=TestIds.functionIdForName in let contract local calls:D.functionScopeContract={D.localDestruction=local;calls=List.map fid calls |> S.of_list} in
 let contracts=["resource",contract D.UnprovenScope [];"indirect",contract D.InertScope ["resource"];"caller",contract D.InertScope ["indirect"];"unknown",contract D.InertScope ["external"];"left",contract D.InertScope ["right"];"right",contract D.InertScope ["left";"Builtin.printLine"]] |> List.map (fun (name,value)->fid name,value) |> FunctionIdMap.ofList in
 let ids=["resource";"indirect";"caller";"unknown";"external";"left";"right";"Builtin.print";"Builtin.printLine"] |> List.map (fun name->name,fid name) |> M.of_list in
 let actual=D.inertFunctionScopes ids contracts in let expected=List.map fid ["left";"right";"Builtin.print";"Builtin.printLine"] |> S.of_list in
 if S.equal actual expected then Ok () else Error ("Unexpected inert scopes: "^HostStructuralFormat.format (V.Union ("set",[V.Sequence (List.map AST.DiagnosticFormatting.func (S.elements actual))])))
let testScopeRevoke ()=
 let fid=TestIds.functionIdForName in let safe:D.functionScopeContract={D.localDestruction=D.InertScope;calls=S.empty} in
 let contracts=FunctionIdMap.ofList [fid "callee",safe;fid "caller",{safe with D.calls=S.singleton (fid "callee")}] in let replaced=FunctionIdMap.add (fid "callee") {safe with D.localDestruction=D.UnprovenScope} contracts in
 let ids=M.of_list ["callee",fid "callee";"caller",fid "caller"] in
 if S.mem (fid "caller") (D.inertFunctionScopes ids contracts) && not (S.mem (fid "caller") (D.inertFunctionScopes ids replaced)) then Ok () else Error "Replacing a definition did not revoke its transitive scope proof"
let testShadowedPrimitive ()=
 let id=TestIds.functionIdForName "Builtin.printLine" in let contracts=FunctionIdMap.ofList [id,{D.localDestruction=D.UnprovenScope;calls=S.empty}] in
 if S.mem id (D.inertFunctionScopes (M.of_list ["Builtin.printLine",id]) contracts) then Error "Shadowed primitive retained its built-in contract" else Ok ()
let cases=[
 testScopeTransitive;
 testScopeRevoke;
 testShadowedPrimitive;
 (fun ()->match extract (manyBranches 64) with None->Error "Expected a region with sixty-four scalar joins"|Some region->region |> SelectListStorage.selectStorage |> ElaborateListOwnership.elaborateOwnership |> VerifyListOwnership.verify);
 rejects (bind "xs" (values 3) (bind "selected" (choice (reverse (local "xs")) (local "xs")) (fold (local "selected"))));
 rejects (bind "xs" (values 3) (choice (fold (call "Darklang.Stdlib.List.map_i64_i64" [local "xs";C.Closure (TestIds.functionIdForName "mapCallback",[local "xs"])])) (C.Int64Literal 7L)));
 checkBudget branchUses (L.Conditional (allocated,L.Complete {zero with L.reusedTransforms=1;releases=1},L.Complete {zero with L.reusedTransforms=1;releases=1},L.Complete zero));
 checkBudget branchJoin (L.Conditional (allocated,L.Complete {allocated with L.copies=1;releases=1},L.Complete zero,L.Complete {zero with L.releases=1}));
 checkBudget (bind "xs" (values 3) (choice (fold (local "xs")) (C.Int64Literal 7L))) (L.Conditional (allocated,L.Complete {zero with L.releases=1},L.Complete {zero with L.releases=1},L.Complete zero));
 checkBudget (bind "xs" (values 3) (choice (fold repeat) (fold (values 3)))) (L.Conditional ({allocated with L.releases=1},L.Complete {zero with L.allocations=1;allocatedBytes=runtimeBytes 0L [H.ValueId 1,1L];releases=1},L.Complete {allocated with L.releases=1},L.Complete zero));
 rejectsOwnership [construct [];ownedBranch (ownedBlock [root] []) (ownedBlock [] [])];
 rejectsOwnership [ownedBranch (ownedBlock [] [construct []]) (ownedBlock [] [])];
 rejectsOwnership [ownedBranch (ownedBlock [] [construct [root]]) (ownedBlock [] [construct [root]])];
 rejectsOwnership [construct [];ownedBranch (ownedBlock [root;root] []) (ownedBlock [root] [])];
 testRuntimeSharedOwnership;
 rejectsOwnership [construct [];[O.Dup root];transform [root;H.ValueId 1]];
 checkBudget runtimeUnique (L.RuntimeConditional (allocated,L.Complete {zero with L.reusedTransforms=1},L.Complete {allocated with L.copies=1},L.Complete {zero with L.releases=1}));
 checkBudget runtimeShared (L.RuntimeConditional (allocated,L.Complete {zero with L.reusedTransforms=1},L.Complete {allocated with L.copies=1},L.Complete {zero with L.releases=2}));
 checkSummary unique {L.allocations=1;allocatedBytes=bytes 56L;copies=0;reusedTransforms=2;releases=1};
 checkSummary shared {L.allocations=2;allocatedBytes=bytes 112L;copies=1;reusedTransforms=0;releases=2};
 checkSummary (bind "xs" (values 3) (bind "alias" (local "xs") (fold (reverse (local "alias"))))) {L.allocations=1;allocatedBytes=bytes 56L;copies=0;reusedTransforms=1;releases=1};
 checkSummary (bind "xs" (values 3) (C.Int64Literal 1L)) {L.allocations=1;allocatedBytes=bytes 56L;copies=0;reusedTransforms=0;releases=1};
 checkSummary (fold (reverse (values 28))) {L.allocations=1;allocatedBytes=bytes 256L;copies=0;reusedTransforms=1;releases=1};
 checkSummary (fold (reverse (map repeat))) {L.allocations=1;allocatedBytes=runtimeBytes 0L [root,1L];copies=0;reusedTransforms=2;releases=1};
 checkSummary (bind "xs" repeat (bind "alias" (local "xs") (bind "ys" (map (local "xs")) (bind "old" (fold (local "alias")) (fold (local "ys")))))) {L.allocations=2;allocatedBytes=runtimeBytes 0L [root,2L];copies=1;reusedTransforms=0;releases=2};
 checkSummary (let outerId=AST.bindingId 1001 in let innerId=AST.bindingId 1002 in C.Let (C.LPVariable outerId,repeat,C.Let (C.LPVariable innerId,repeat,fold (reverse (C.Local innerId))))) {L.allocations=2;allocatedBytes=runtimeBytes 0L [root,1L;H.ValueId 1,1L];copies=0;reusedTransforms=1;releases=2};
 checkSummary (bind "ys" (map repeat) (bind "zs" (map (local "ys")) (bind "old" (fold (local "ys")) (fold (local "zs"))))) {L.allocations=2;allocatedBytes=runtimeBytes 0L [root,2L];copies=1;reusedTransforms=1;releases=2};
 testLoweredBudget;
 rejects (bind "xs" (values 3) (local "xs"));
 checkSummary (fold (reverse (values 29))) {L.allocations=1;allocatedBytes=bytes 272L;copies=0;reusedTransforms=1;releases=1};
 rejects (fold (reverse (local "external")));
 (fun ()->let expression=C.Let (C.LPWildcard,reverse (local "external"),C.Int64Literal 1L) in match extractWithParameters (B.singleton (binding "external") (AST.TList AST.TInt64)) expression with None->Ok ()|Some _->Error "A scalar wrapper admitted a borrowed list parameter");
 testPrimitiveContracts;
 rejects (bind "xs" (C.ListLiteral [C.StringLiteral "a"]) (C.Int64Literal 0L));
 rejects (bind "xs" (values 3) (fold (call "Darklang.Stdlib.List.map_i64_i64" [local "xs";C.Closure (TestIds.functionIdForName "mapCallback",[local "xs"])])));
 rejectsOwnership [construct [root;root]];
 rejectsOwnership [construct []];
 rejectsOwnership [construct [root];construct [root]];
 rejectsOwnership [construct [root];transform [H.ValueId 1]]
]
let tests=List.combine ["Scope destruction rejects transitive callers and accepts safe recursive components";"Scope destruction replacement revokes caller proofs";"Scope destruction does not trust a shadowed primitive";"List HIR accepts deep shared continuations";"List HIR rejects list-valued branch joins";"List HIR rejects branch callbacks hiding aliases";"List HIR consumes independently on mutually exclusive paths";"List HIR preserves a source needed after the join";"List HIR releases unused inputs on the other edge";"List HIR budgets branch-local constructors separately";"List HIR verifier rejects mismatched branch ownership";"List HIR verifier rejects branch-local leaked values";"List HIR verifier rejects duplicate identities across branches";"List HIR verifier rejects double edge cleanup";"List HIR verifies runtime copy-on-write after ownership duplication";"List HIR rejects static consumption after ownership duplication";"List HIR selects runtime reuse for an ownership boundary";"List HIR protects aliases across a runtime ownership boundary";"List HIR consumes unique map/reverse storage";"List HIR copies a surviving source version";"List HIR normalizes aliases before last-use solving";"List HIR releases unused construction";"List HIR supports the largest recyclable array";"List HIR budgets runtime construction and consuming transforms";"List HIR budgets runtime copies through aliases";"List HIR keeps independent runtime extents distinct";"List HIR retains runtime origin when copying a consumed transform";"List HIR preserves native allocation budget";"List HIR rejects escaping lists";"List HIR reclaims arrays beyond the fixed heap classes";"List HIR rejects borrowed input lists";"List HIR rejects scalar wrappers around borrowed managed inputs";"List HIR declares primitive effects and alias provenance";"List HIR rejects managed elements";"List HIR rejects callbacks capturing region lists";"List HIR verifier rejects duplicate drop";"List HIR verifier rejects leaked roots";"List HIR verifier rejects reused identities";"List HIR verifier rejects mutation after drop"] cases
