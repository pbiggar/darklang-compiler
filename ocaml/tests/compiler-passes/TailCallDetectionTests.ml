(*
   Ensures non-self tailcall conversion does not strand RefCountDec operations
   after TailCall (which would be unreachable).
*)
(* TailCallDetectionTests.fs - Unit tests for tailcall conversion and cleanup ordering. *)
[@@@warning "-4-42"]
open Dark_compiler
open MemoryModel
open ANF
open TailCallDetection
let fn = TestIds.functionIdForName
let isTailCallWithUnreachableCleanup funcName = function TailCall (target,_) when target <> funcName -> true | IndirectTailCall _ | ClosureTailCall _ -> true | _ -> false
let isCleanupDec = function RefCountDec _ | RefCountDecString _ | RefCountDecBlob _ -> true | _ -> false
let rec hasDecAfterNonSelfTailCall funcName = function
 | Jump _ | Return _ -> false
 | Let (_,cexpr,Let (_,cleanup,_)) when isTailCallWithUnreachableCleanup funcName cexpr && isCleanupDec cleanup -> true
 | Let (_,_,body) -> hasDecAfterNonSelfTailCall funcName body
 | Join (_,yes,no) | If (_,yes,no) -> hasDecAfterNonSelfTailCall funcName yes || hasDecAfterNonSelfTailCall funcName no
let tupleType = AST.TTuple [AST.TInt64;AST.TInt64]
let metadata typ =
 let plan = MemoryPlanning.rcReleasePlanOfType StringOrder.Map.empty typ in
 {releasePlanCacheKey=ReleasePlanFingerprint.rcReleasePlanCacheKey typ plan;releasePlan=Some plan;sourceType=Some typ}
let tupleMetadata = metadata tupleType
let function_ name typedParams returnType body : functionDef = {id=fn name;name;typedParams;returnType;returnOwnership=OwnedReturn;body}
let parameter id typ : typedParam = {id=TempId id;typ}
let testNonSelfTailCallMovesDecBeforeTailCall () =
 let p0 = TempId 0 and tupleTmp = TempId 1 and callTmp = TempId 2 and decTmp = TempId 3 in
 let caller = function_ "caller" [parameter 0 AST.TInt64] AST.TInt64
  (Let (tupleTmp,TupleAlloc [Var p0;IntLiteral (Int64 1L)],Let (callTmp,Call (fn "callee",[Var p0]),Let (decTmp,RefCountDec (Var tupleTmp,16,GenericHeap,Some tupleMetadata),Return (Var callTmp))))) in
 let transformed = detectTailCallsInFunction caller in
 if hasDecAfterNonSelfTailCall transformed.id transformed.body then Error "Found RefCountDec after non-self TailCall; cleanup should run before tailcall" else Ok ()
let testIndirectTailCallMovesDecBeforeTailCall () =
 let p0 = TempId 0 and funcTmp = TempId 1 and tupleTmp = TempId 2 and callTmp = TempId 3 and decTmp = TempId 4 in
 let caller = function_ "caller" [parameter 0 AST.TInt64] AST.TInt64
  (Let (funcTmp,Atom (FuncRef (fn "callee")),Let (tupleTmp,TupleAlloc [Var p0;IntLiteral (Int64 1L)],Let (callTmp,IndirectCall (Var funcTmp,[Var p0]),Let (decTmp,RefCountDec (Var tupleTmp,16,GenericHeap,Some tupleMetadata),Return (Var callTmp)))))) in
 let transformed = detectTailCallsInFunction caller in
 if hasDecAfterNonSelfTailCall transformed.id transformed.body then Error "Found RefCountDec after IndirectTailCall; cleanup should run before tailcall" else Ok ()
let testClosureCallInTailPositionBecomesClosureTailCall () =
 let closure = TempId 0 and value = TempId 1 and result = TempId 2 in
 let caller = function_ "closureCaller" [parameter 0 (AST.TFunction ([AST.TInt64],AST.TInt64));parameter 1 AST.TInt64] AST.TInt64 (Let (result,ClosureCall (Var closure,[Var value]),Return (Var result))) in
 match (detectTailCallsInFunction caller).body with
 | Let (bound,ClosureTailCall (Var target,[Var argument]),Return (Var returned)) when bound=result && target=closure && argument=value && returned=result -> Ok ()
 | body -> Error ("Expected a tail-position closure invocation to form ClosureTailCall, got " ^ ANFTestFormatting.expr body)
let testOwnedTransferDeclinesMismatchedArity () =
 let p0 = TempId 0 and retainTmp = TempId 1 and releaseTmp = TempId 2 and callTmp = TempId 3 and cleanupTmp = TempId 4 in
 let dec atom = RefCountDec (atom,16,GenericHeap,Some tupleMetadata) in
 let caller = function_ "caller" [parameter 0 tupleType] tupleType
  (Let (retainTmp,RefCountInc (Var p0,16,GenericHeap,Some tupleMetadata),Let (releaseTmp,dec (Var p0),Let (callTmp,Call (fn "caller",[Var p0;IntLiteral (Int64 1L);IntLiteral (Int64 2L)]),Let (cleanupTmp,dec (Var p0),Return (Var callTmp)))))) in
 match (detectTailCallsInFunction caller).body with
 | Let (_,_,Let (_,_,Let (_,Call (name,_),_))) when name=fn "caller" -> Ok ()
 | _ -> Error "Mismatched-arity owned transfer should preserve the ordinary call cleanup path"
let ownedTransferTestFunction secondArgument secondCleanup =
 let p0 = TempId 0 and p1 = TempId 1 and replacement0 = TempId 2 in
 let inc tid = RefCountInc (Var tid,16,GenericHeap,Some tupleMetadata) and dec tid = RefCountDec (Var tid,16,GenericHeap,Some tupleMetadata) in
 let terminal = Option.fold ~none:(Return (Var (TempId 8))) ~some:(fun (tid,cleanup) -> Let (tid,cleanup,Return (Var (TempId 8)))) secondCleanup in
 function_ "caller" [parameter 0 tupleType;parameter 1 tupleType] AST.TInt64
  (Let (TempId 4,inc p0,Let (TempId 5,inc p1,Let (TempId 6,dec p0,Let (TempId 7,dec p1,Let (TempId 8,Call (fn "caller",[Var replacement0;Var secondArgument]),Let (TempId 9,dec replacement0,terminal)))))))
let testOwnedTransferRequiresOneToOneCleanupAccounting () =
 let transformed = detectTailCallsInFunction (ownedTransferTestFunction (TempId 2) None) in
 let rec containsOrdinarySelfCall = function
 | Let (_,Call (id,_),_) when id=fn "caller" -> true
 | Let (_,_,body) -> containsOrdinarySelfCall body
 | Join (_,yes,no) | If (_,yes,no) -> containsOrdinarySelfCall yes || containsOrdinarySelfCall no
 | Return _ | Jump _ -> false in
 if containsOrdinarySelfCall transformed.body then Ok () else Error "One replacement edge must not transfer into two owned loop parameters"
let testOwnedTransferAcceptsMultipleExactlyMatchedCleanups () =
 let replacement1 = TempId 3 and cleanup1 = TempId 10 in
 let transformed = detectTailCallsInFunction (ownedTransferTestFunction replacement1 (Some (cleanup1,RefCountDec (Var replacement1,16,GenericHeap,Some tupleMetadata)))) in
 let rec containsSelfTailCall = function
 | Let (_,TailCall (id,_),_) when id=fn "caller" -> true
 | Let (_,_,body) -> containsSelfTailCall body
 | Join (_,yes,no) | If (_,yes,no) -> containsSelfTailCall yes || containsSelfTailCall no
 | Return _ | Jump _ -> false in
 if containsSelfTailCall transformed.body then Ok () else Error "Two exactly matched replacement edges should transfer into two owned loop parameters"
let testRetainedProjectionAllowsSelfTailCall () =
 let current = TempId 0 and source = TempId 1 and projected = TempId 2 and retain = TempId 3 and callResult = TempId 4 and releaseSource = TempId 5 in
 let listType = AST.TList AST.TInt64 in let tupleType = AST.TTuple [listType] in
 let func = function_ "loop" [parameter 0 listType] AST.TInt64
  (Let (source,TupleAlloc [Var current],Let (projected,TupleGet (Var source,0),Let (retain,RefCountInc (Var projected,24,TaggedList,Some (metadata listType)),Let (callResult,Call (fn "loop",[Var projected]),Let (releaseSource,RefCountDec (Var source,8,GenericHeap,Some (metadata tupleType)),Return (Var callResult))))))) in
 let rec containsSelfTailCall = function
 | Let (_,TailCall (id,_),_) when id=fn "loop" -> true
 | Let (_,_,body) -> containsSelfTailCall body
 | Join (_,yes,no) | If (_,yes,no) -> containsSelfTailCall yes || containsSelfTailCall no
 | Return _ | Jump _ -> false in
 if containsSelfTailCall (detectTailCallsInFunction func).body then Ok () else Error "An explicitly retained projection should transfer safely into a self tail call"
let tests = [
 "non-self tailcall moves dec before tailcall",testNonSelfTailCallMovesDecBeforeTailCall;
 "indirect tailcall moves dec before tailcall",testIndirectTailCallMovesDecBeforeTailCall;
 "tail-position closure call forms ClosureTailCall",testClosureCallInTailPositionBecomesClosureTailCall;
 "owned transfer declines mismatched arity",testOwnedTransferDeclinesMismatchedArity;
 "owned transfer requires one-to-one cleanup accounting",testOwnedTransferRequiresOneToOneCleanupAccounting;
 "owned transfer accepts multiple exactly matched cleanups",testOwnedTransferAcceptsMultipleExactlyMatchedCleanups;
 "retained projection allows self tailcall",testRetainedProjectionAllowsSelfTailCall]
