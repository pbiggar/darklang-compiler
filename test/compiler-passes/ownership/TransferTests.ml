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

let transform = SSAOwnershipTestHelpers.transform
let param id typ : ANF.typedParam = { ANF.id; typ }
let int n = IntLiteral (Int64 n)
let call name args = Call (fid name, args)
let closureType = AST.TFunction ([ AST.TInt64 ], AST.TInt64)

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
  let* transformed =
    transform ctx
      (functionWith "loop"
         [ param parent parentType; param child childType ]
         childType body)
  in
  require
    (not (SSAOwnershipTestHelpers.hasRefCountIncForTemp projected transformed))
    "Borrowed projection from a parameter should not be retained solely \
     because it feeds a self-recursive call"

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
  let* body =
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
      (not (SSAOwnershipTestHelpers.hasRefCountIncForTemp source body))
      "Darklang.Stdlib.List.__mapHelper over closure source to value should \
       not retain an unreturned borrowed source parameter"
  in
  require
    (not (SSAOwnershipTestHelpers.hasRefCountDecForTemp source body))
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
  let* transformed =
    transform ctx (functionWith "caller" [ param list listType ] listType body)
  in
  require
    (SSAOwnershipTestHelpers.hasRefCountDecForTemp returned transformed)
    "ClosureCall result passed directly to typed closure-list pushBack should \
     get a local dec because raw storage retains the edge"
