(*
   CallTests.ml - Verify call-result ownership and canonical source metadata.
*)
[@@@warning "-4-42"]

open Dark_compiler
open ANF
module M = StringOrder.Map
module T = RcTypeFacts.TempMap
module TS = RcReturnAnalysis.TempSet

let ( let* ) = Result.bind
let require condition error = if condition then Ok () else Error error

let functionRegistry entries =
  List.map
    (fun (name, typ) -> (TestIds.functionIdForName name, (name, typ)))
    entries
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
    ANF.id = TestIds.functionIdForName name;
    name;
    typedParams;
    returnType;
    returnOwnership = OwnedReturn;
    body;
  }

let transform = SSAOwnershipTestHelpers.transform
let param id typ : ANF.typedParam = { ANF.id; typ }
let int n = IntLiteral (Int64 n)
let call name args = Call (TestIds.functionIdForName name, args)

let testBorrowedCallMaterializesOwnedLocal () =
  let nodeType = AST.TList AST.TInt64 in
  let project = "Darklang.Stdlib.List.__node2GetChild_i64"
  and measure = "Darklang.Stdlib.List.__nodeMeasure_i64" in
  let ctx =
    context
      (functionRegistry
         [
           ("consumer", AST.TFunction ([ nodeType; AST.TInt64 ], AST.TInt64));
           (project, AST.TFunction ([ nodeType; AST.TInt64 ], nodeType));
           (measure, AST.TFunction ([ nodeType ], AST.TInt64));
         ])
  in
  let node = TempId 0
  and index = TempId 1
  and child = TempId 2
  and measured = TempId 3 in
  let body =
    Let
      ( child,
        BorrowedCall (TestIds.functionIdForName project, [ Var node; Var index ]),
        Let (measured, call measure [ Var child ], Return (Var measured)) )
  in
  let* body =
    transform ctx
      (functionWith "consumer"
         [ param node nodeType; param index AST.TInt64 ]
         AST.TInt64 body)
  in
  let* () =
    require
      (SSAOwnershipTestHelpers.hasRefCountIncForTemp child body)
      "BorrowedCall must retain its borrowed result when materializing a local \
       value"
  in
  require
    (SSAOwnershipTestHelpers.hasRefCountDecForTemp child body)
    "Materialized BorrowedCall local must release its retained ownership edge"

let testReturnedBorrowedCallMaterializesOwnership () =
  let nodeType = AST.TList AST.TInt64 in
  let node = TempId 0 and child = TempId 1 in
  let ctx =
    context
      (functionRegistry
         [
           ("project", AST.TFunction ([ nodeType ], nodeType));
           ("borrowChild", AST.TFunction ([ nodeType ], nodeType));
         ])
  in
  let body =
    Let
      ( child,
        BorrowedCall (TestIds.functionIdForName "borrowChild", [ Var node ]),
        Return (Var child) )
  in
  let* transformed =
    transform ctx (functionWith "project" [ param node nodeType ] nodeType body)
  in
  require
    (SSAOwnershipTestHelpers.hasRefCountIncForTemp child transformed)
    "Returning a BorrowedCall result must retain it to materialize owned \
     return storage"

let testCallReturningClosureGetsAutoDecAfterUse () =
  let closureType = AST.TFunction ([ AST.TInt64 ], AST.TInt64) in
  let closure = TempId 0 and result = TempId 1 in
  let ctx =
    context
      (functionRegistry [ ("makeClosure", AST.TFunction ([], closureType)) ])
  in
  let body =
    Let
      ( closure,
        call "makeClosure" [],
        Let (result, ClosureCall (Var closure, [ int 5L ]), Return (Var result))
      )
  in
  let* transformed = transform ctx (functionWith "caller" [] AST.TInt64 body) in
  require
    (SSAOwnershipTestHelpers.hasRefCountDecForTemp closure transformed)
    "Call result with function type should receive automatic closure \
     RefCountDec after use"

let testClosureCallReturningClosureGetsAutoDecAfterUse () =
  let returnedClosureType = AST.TFunction ([ AST.TInt64 ], AST.TInt64) in
  let makerClosureType = AST.TFunction ([ AST.TInt64 ], returnedClosureType) in
  let maker = TempId 0 and returned = TempId 1 and result = TempId 2 in
  let ctx =
    context
      (functionRegistry
         [
           ("makeClosure", makerClosureType);
           ("returnedClosure", returnedClosureType);
         ])
  in
  let body =
    Let
      ( maker,
        ClosureAlloc (TestIds.functionIdForName "makeClosure", []),
        Let
          ( returned,
            ClosureCall (Var maker, [ int 5L ]),
            Let (result, Atom (int 0L), Return (Var result)) ) )
  in
  let* transformed = transform ctx (functionWith "caller" [] AST.TInt64 body) in
  require
    (SSAOwnershipTestHelpers.hasRefCountDecForTemp returned transformed)
    "ClosureCall result with function return type should receive automatic \
     closure RefCountDec after use"

let enumBinding generic =
  let name = if generic then "Phantom" else "Color" in
  let typeParams = if generic then [ "a" ] else [] in
  let shape : MemoryModel.rcSumShapeInfo =
    {
      MemoryModel.typeParams;
      payloads = [ (0, None); (1, None) ];
      unaryPayloadTags = MemoryModel.IntSet.empty;
    }
  in
  let ctx =
    {
      (context FunctionIdMap.empty) with
      RcTypeFacts.sumShapeReg = M.singleton name shape;
      variantLookup =
        (if generic then
           M.of_list
             [
               ("Left", (name, typeParams, 0, []));
               ("Right", (name, typeParams, 1, []));
             ]
         else M.empty);
    }
  in
  let enum = TempId 0 and result = TempId 1 in
  let enumType = AST.TSum (name, if generic then [ AST.TString ] else []) in
  let body =
    Let
      ( enum,
        TypedAtom (int 0L, enumType),
        Let (result, Atom (int 1L), Return (Var result)) )
  in
  let fixture =
    if generic then "genericPureEnumBinding" else "pureEnumBinding"
  in
  let* transformed = transform ctx (functionWith fixture [] AST.TInt64 body) in
  Ok (transformed, enum)

let testPureEnumBindingDoesNotGetAutomaticDec () =
  let* body, enum = enumBinding false in
  require
    (not (SSAOwnershipTestHelpers.hasRefCountDecForTemp enum body))
    "Pure enum binding should classify as immediate and must not get automatic \
     RefCountDec"

let testGenericPureEnumBindingDoesNotGetAutomaticDec () =
  let* body, enum = enumBinding true in
  require
    (not (SSAOwnershipTestHelpers.hasRefCountDecForTemp enum body))
    "Generic pure enum binding should classify from variant metadata and must \
     not get automatic RefCountDec"

let conversion func funcReg : AST_to_ANF.conversionResult =
  {
    AST_to_ANF.program = Program ([ func ], Return UnitLiteral);
    ownershipContracts = FunctionIdMap.empty;
    recursiveMembers = FunctionIdMap.empty;
    typeReg = M.empty;
    recordFieldsReg = M.empty;
    recordTypeParamsReg = M.empty;
    variantLookup = M.empty;
    rcSumShapeReg = M.empty;
    funcReg;
    funcParams = M.empty;
    moduleRegistry = M.empty;
  }

let testProgramRcFreshTempsFollowExistingProgramTemps () =
  let low = TempId 1000 and high = TempId 6000 in
  let body =
    Let
      (low, Atom (int 0L), Let (high, call "makeString" [], Return UnitLiteral))
  in
  let input =
    conversion
      (functionWith "freshTempBoundary" [] AST.TUnit body)
      (functionRegistry [ ("makeString", AST.TFunction ([], AST.TString)) ])
  in
  let func = functionWith "freshTempBoundary" [] AST.TUnit body in
  let* transformed = transform (RcTypeFacts.createContext input) func in
  let definitions =
    SSAANF.LabelMap.bindings transformed.SSAANF.blocks
    |> List.concat_map (fun (_, (block : SSAANF.block)) ->
        List.map
          (fun (parameter : ANF.typedParam) -> parameter.ANF.id)
          block.SSAANF.parameters
        @ List.map fst block.SSAANF.operations)
  in
  let* () =
    require
      (TS.cardinal (TS.of_list definitions) = List.length definitions)
      "SSA RC insertion reused an existing TempId"
  in
  require
    (SSAANF.LabelMap.exists
       (fun _ (block : SSAANF.block) ->
         List.exists
           (function
             | TempId id, RefCountDecString (Var source) ->
                 source = high && id > 6000
             | _ -> false)
           block.SSAANF.operations)
       transformed.SSAANF.blocks)
    "SSA RC insertion must allocate fresh temporaries beyond source t6000"

let testProgramRcRejectsDriftedOwnershipContract () =
  let func =
    functionWith "driftedOwnership"
      [ param (TempId 0) AST.TInt64 ]
      AST.TInt64 (Return (Var (TempId 0)))
  in
  let input =
    conversion func
      (functionRegistry
         [ ("driftedOwnership", AST.TFunction ([ AST.TInt64 ], AST.TInt64)) ])
  in
  let signature : OwnedIR.callSignature =
    {
      OwnedIR.parameters = [ OwnedIR.UniqueCallParameter ];
      result = OwnedIR.UnmanagedCallResult;
    }
  in
  let input =
    {
      input with
      AST_to_ANF.ownershipContracts =
        FunctionIdMap.ofList [ (func.ANF.id, signature) ];
    }
  in
  match
    RefCountInsertion.verifyOwnershipContracts
      (RcTypeFacts.createContext input)
      input.AST_to_ANF.ownershipContracts input.AST_to_ANF.program
  with
  | Error message
    when Text.contains message
           "Ownership contract parameter representation changed" ->
      Ok ()
  | Error message ->
      Error
        ("Expected RC insertion to reject ownership-contract drift, got Error "
       ^ message)
  | Ok _ ->
      Error "Expected RC insertion to reject ownership-contract drift, got Ok"

let testBareSumTypeRefsAreCanonicalizedForRcSourceTypes () =
  let payloadType = AST.TRecord ("Payload", []) in
  let dictType = AST.TDict (AST.TInt64, payloadType) in
  let shape : MemoryModel.rcSumShapeInfo =
    {
      MemoryModel.typeParams = [];
      payloads = [ (0, None); (1, Some AST.TString) ];
      unaryPayloadTags = MemoryModel.IntSet.singleton 1;
    }
  in
  let ctx =
    {
      (context (functionRegistry [ ("mkDict", AST.TFunction ([], dictType)) ])) with
      RcTypeFacts.variantLookup =
        M.of_list
          [
            ("Empty", ("Payload", [], 0, []));
            ("SomePayload", ("Payload", [], 1, [ AST.TString ]));
          ];
      sumShapeReg = M.singleton "Payload" shape;
    }
  in
  let dict = TempId 0 and result = TempId 1 in
  let body =
    Let
      (dict, call "mkDict" [], Let (result, Atom (int 1L), Return (Var result)))
  in
  let* transformed =
    transform ctx (functionWith "canonicalBareSum" [] AST.TInt64 body)
  in
  match
    SSAOwnershipTestHelpers.tryRefCountDecSourceTypeForTemp dict transformed
  with
  | Some (AST.TDict (AST.TInt64, AST.TSum ("Payload", []))) -> Ok ()
  | Some other ->
      Error
        ("Expected dict dec source type to canonicalize Payload as a sum, got "
        ^ StructuralFormat.semanticType other)
  | None -> Error "Expected dict binding to receive automatic RefCountDec"
