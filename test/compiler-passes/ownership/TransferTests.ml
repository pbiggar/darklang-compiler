(*
   TransferTests.ml - Verify recursive ownership transfers and borrowed projections.
*)
[@@@warning "-4-42"]

open Dark_compiler
open ANF
module M = StringOrder.Map
module T = RcTypeFacts.TempMap

let fid = TestIds.functionIdForName
let ( let* ) = Result.bind
let require condition error = if condition then Ok () else Error error

let functionRegistry entries =
  List.map (fun (name, typ) -> (fid name, (name, typ))) entries
  |> FunctionIdMap.ofList

let context funcReg : RcTypeFacts.typeContext =
  {
    RcTypeFacts.typeReg = M.empty;
    variantLookup = M.empty;
    sumShapeReg = M.empty;
    funcReg;
    funcParams = M.empty;
    tempTypes = T.empty;
    closureFuncs = T.empty;
    typePlanning = RcTypeFacts.createRcTypePlanningContext ();
  }

let functionWith name typedParams returnType body : ANF.functionDef =
  {
    ANF.id = fid name;
    name;
    typedParams;
    returnType;
    returnOwnership = OwnedReturn;
    body;
  }

let transform ctx func =
  let transformed, _, _ =
    RefCountInsertion.insertRCInFunction ctx func initialVarGen
  in
  transformed.ANF.body

let param id typ : ANF.typedParam = { ANF.id; typ }
let int n = IntLiteral (Int64 n)
let call name args = Call (fid name, args)
let closureType = AST.TFunction ([ AST.TInt64 ], AST.TInt64)
let sourceListType = AST.TList AST.TInt64
let mappedListType = AST.TList closureType
let mapperType = AST.TFunction ([ AST.TInt64 ], closureType)

let mapParams () =
  [
    param (TempId 0) sourceListType;
    param (TempId 1) mapperType;
    param (TempId 2) mappedListType;
  ]

let mapSignature =
  AST.TFunction ([ sourceListType; mapperType; mappedListType ], mappedListType)

let testMapHelperAccumulatorReturnDoesNotRetainOwnedAccumulator () =
  let name = "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_to_i64" in
  let acc = TempId 2 in
  let body =
    transform
      (context (functionRegistry [ (name, mapSignature) ]))
      (functionWith name (mapParams ()) mappedListType (Return (Var acc)))
  in
  require
    (not (CleanupTests.hasRefCountIncForTemp acc body))
    "Darklang.Stdlib.List.__mapHelper should transfer its owned accumulator \
     return without retaining it"

let testMapHelperSelfTailCallReleasesReplacedAccumulator () =
  let name = "Darklang.Stdlib.List.__mapHelper"
  and specialized = "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_to_i64"
  and push = "Darklang.Stdlib.List.__pushBack_fn_i64_to_i64" in
  let ctx =
    context
      (functionRegistry
         [
           (name, mapSignature);
           (specialized, mapSignature);
           ("mappedClosure", closureType);
           ( push,
             AST.TFunction ([ mappedListType; closureType ], mappedListType) );
         ])
  in
  let source = TempId 0
  and mapper = TempId 1
  and acc = TempId 2
  and closure = TempId 3
  and newAcc = TempId 4
  and tail = TempId 5 in
  let body =
    Let
      ( closure,
        ClosureAlloc (fid "mappedClosure", []),
        Let
          ( newAcc,
            call push [ Var acc; Var closure ],
            Let
              ( tail,
                TailCall
                  (fid specialized, [ Var source; Var mapper; Var newAcc ]),
                Return (Var tail) ) ) )
  in
  require
    (CleanupTests.hasRefCountDecForTemp acc
       (transform ctx (functionWith name (mapParams ()) mappedListType body)))
    "Darklang.Stdlib.List.__mapHelper self tail-call should release the \
     replaced owned accumulator"

let state1Type = AST.TTuple [ AST.TInt64; AST.TInt64; AST.TInt64 ]
let state2Type = AST.TTuple [ AST.TInt64; AST.TInt64 ]
let resultType = AST.TTuple [ state1Type; state2Type ]

let loopParams () =
  [
    param (TempId 0) state1Type;
    param (TempId 1) state2Type;
    param (TempId 2) AST.TInt64;
  ]

let loopContext () =
  let signature =
    AST.TFunction ([ state1Type; state2Type; AST.TInt64 ], resultType)
  in
  context (functionRegistry [ ("loop", signature); ("round", signature) ])

let testBorrowedProjectionRecursiveArgsAreRetained recursiveCExpr =
  let s1 = TempId 0
  and s2 = TempId 1
  and i = TempId 2
  and result = TempId 3
  and next1 = TempId 4
  and next2 = TempId 5
  and tail = TempId 6 in
  let body =
    Let
      ( result,
        call "round" [ Var s1; Var s2; Var i ],
        Let
          ( next1,
            TupleGet (Var result, 0),
            Let
              ( next2,
                TupleGet (Var result, 1),
                Let
                  ( tail,
                    recursiveCExpr (fid "loop") [ Var next1; Var next2; Var i ],
                    Return (Var tail) ) ) ) )
  in
  require
    (CleanupTests.pathHasRetainsBeforeDec [ next1; next2 ] result
       (transform (loopContext ())
          (functionWith "loop" (loopParams ()) resultType body)))
    "Borrowed tuple projections passed as self-tail-call accumulators should \
     be retained before parent cleanup"

let testBorrowedProjectionSelfTailCallArgsAreRetained () =
  testBorrowedProjectionRecursiveArgsAreRetained (fun id args ->
      TailCall (id, args))

let testBorrowedProjectionSelfRecursiveCallArgsAreRetained () =
  testBorrowedProjectionRecursiveArgsAreRetained (fun id args ->
      Call (id, args))

let testBorrowedProjectionAliasSelfRecursiveCallArgsAreRetained () =
  let s1 = TempId 0
  and s2 = TempId 1
  and i = TempId 2
  and result = TempId 3
  and next1 = TempId 4
  and next2 = TempId 5
  and alias1 = TempId 6
  and alias2 = TempId 7
  and tail = TempId 8 in
  let body =
    Let
      ( result,
        call "round" [ Var s1; Var s2; Var i ],
        Let
          ( next1,
            TupleGet (Var result, 0),
            Let
              ( next2,
                TupleGet (Var result, 1),
                Let
                  ( alias1,
                    TypedAtom (Var next1, state1Type),
                    Let
                      ( alias2,
                        TypedAtom (Var next2, state2Type),
                        Let
                          ( tail,
                            call "loop" [ Var alias1; Var alias2; Var i ],
                            Return (Var tail) ) ) ) ) ) )
  in
  let body =
    transform (loopContext ())
      (functionWith "loop" (loopParams ()) resultType body)
  in
  require
    (CleanupTests.hasRefCountIncForTemp next1 body
    && CleanupTests.hasRefCountIncForTemp next2 body)
    "Borrowed tuple projection aliases passed as self-recursive accumulators \
     should be retained before parent cleanup"

let testBorrowedProjectionIfBranchSelfRecursiveCallArgsAreRetained () =
  let s1 = TempId 0
  and s2 = TempId 1
  and i = TempId 2
  and base = TempId 3
  and result = TempId 4
  and alias = TempId 5
  and next1 = TempId 6
  and alias1 = TempId 7
  and alias1b = TempId 8
  and next2 = TempId 9
  and alias2 = TempId 10
  and alias2b = TempId 11
  and nextI = TempId 12
  and recursiveResult = TempId 13 in
  let branch =
    Let
      ( result,
        call "round" [ Var s1; Var s2; Var i ],
        Let
          ( alias,
            Atom (Var result),
            Let
              ( next1,
                TupleGet (Var alias, 0),
                Let
                  ( alias1,
                    TypedAtom (Var next1, state1Type),
                    Let
                      ( alias1b,
                        TypedAtom (Var alias1, state1Type),
                        Let
                          ( next2,
                            TupleGet (Var alias, 1),
                            Let
                              ( alias2,
                                TypedAtom (Var next2, state2Type),
                                Let
                                  ( alias2b,
                                    TypedAtom (Var alias2, state2Type),
                                    Let
                                      ( nextI,
                                        Prim (Add, Var i, int 1L),
                                        Let
                                          ( recursiveResult,
                                            call "loop"
                                              [
                                                Var alias1b;
                                                Var alias2b;
                                                Var nextI;
                                              ],
                                            Return (Var recursiveResult) ) ) )
                              ) ) ) ) ) ) )
  in
  let body =
    If
      ( Var i,
        Let (base, TupleAlloc [ Var s1; Var s2 ], Return (Var base)),
        branch )
  in
  require
    (CleanupTests.pathHasRetainsBeforeDec [ next1; next2 ] result
       (transform (loopContext ())
          (functionWith "loop" (loopParams ()) resultType body)))
    "Borrowed tuple projections in recursive if branches should be retained \
     before parent cleanup"

let testBorrowedProjectionFromParameterSelfRecursiveCallStaysBorrowed () =
  let childType = AST.TTuple [ AST.TInt64 ] in
  let parentType = AST.TTuple [ childType ] in
  let parent = TempId 0
  and child = TempId 1
  and projected = TempId 2
  and result = TempId 3 in
  let ctx =
    context
      (functionRegistry
         [ ("loop", AST.TFunction ([ parentType; childType ], childType)) ])
  in
  let body =
    Let
      ( projected,
        TupleGet (Var parent, 0),
        Let
          ( result,
            call "loop" [ Var parent; Var projected ],
            Return (Var result) ) )
  in
  require
    (not
       (CleanupTests.hasRefCountIncForTemp projected
          (transform ctx
             (functionWith "loop"
                [ param parent parentType; param child childType ]
                childType body))))
    "Borrowed projection from a parameter should not be retained solely \
     because it feeds a self-recursive call"

let testMapHelperClosureProducingCallRetainsBorrowedSource () =
  let name = "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_to_i64" in
  let source = TempId 0
  and mapper = TempId 1
  and acc = TempId 2
  and mapped = TempId 3 in
  let body =
    Let
      ( mapped,
        call name [ Var source; Var mapper; Var acc ],
        Return (Var mapped) )
  in
  require
    (CleanupTests.hasRefCountIncForTemp source
       (transform
          (context (functionRegistry [ (name, mapSignature) ]))
          (functionWith "caller" (mapParams ()) mappedListType body)))
    "Callers entering closure-producing Stdlib.List.__mapHelper should retain \
     the borrowed source list"

let testMapHelperClosureSourceToValueKeepsSourceBorrowed () =
  let sourceType = AST.TList closureType in
  let mappedType = AST.TList AST.TInt64 in
  let mapperType = AST.TFunction ([ closureType ], AST.TInt64) in
  let name = "Darklang.Stdlib.List.__mapHelper_fn_i64_to_i64_i64" in
  let source = TempId 0 and mapper = TempId 1 and acc = TempId 2 in
  let ctx =
    context
      (functionRegistry
         [
           ( name,
             AST.TFunction ([ sourceType; mapperType; mappedType ], mappedType)
           );
         ])
  in
  let body =
    transform ctx
      (functionWith name
         [
           param source sourceType;
           param mapper mapperType;
           param acc mappedType;
         ]
         mappedType (Return (Var acc)))
  in
  let* () =
    require
      (not (CleanupTests.hasRefCountIncForTemp source body))
      "Darklang.Stdlib.List.__mapHelper over closure source to value should \
       not retain an unreturned borrowed source parameter"
  in
  require
    (not (CleanupTests.hasRefCountDecForTemp source body))
    "Darklang.Stdlib.List.__mapHelper over closure source to value should not \
     release a borrowed source parameter"

let testClosurePushBackRetainsImmediateClosureCallResult () =
  let makerType = AST.TFunction ([ AST.TInt64 ], closureType) in
  let listType = AST.TList closureType in
  let push = "Darklang.Stdlib.List.__pushBack_fn_i64_to_i64" in
  let ctx =
    context
      (functionRegistry
         [
           ("makeClosure", makerType);
           ("mappedClosure", closureType);
           ("returnedClosure", closureType);
           (push, AST.TFunction ([ listType; closureType ], listType));
         ])
  in
  let maker = TempId 0
  and returned = TempId 1
  and list = TempId 2
  and pushed = TempId 3 in
  let body =
    Let
      ( maker,
        ClosureAlloc (fid "makeClosure", []),
        Let
          ( returned,
            ClosureCall (Var maker, [ int 5L ]),
            Let
              (pushed, call push [ Var list; Var returned ], Return (Var pushed))
          ) )
  in
  require
    (CleanupTests.hasRefCountDecForTemp returned
       (transform ctx
          (functionWith "caller" [ param list listType ] listType body)))
    "ClosureCall result passed directly to typed closure-list pushBack should \
     get a local dec because raw storage retains the edge"
