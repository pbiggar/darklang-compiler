(* OwnershipCallFactsTests.ml - Call-point uniqueness laws and specialization handoff. *)
[@@@warning "-4-42"]

open Dark_compiler
module H = HIR
module O = OwnedIR

module Identity = struct
  type t = H.valueId

  let compare = Stdlib.compare

  module Set = H.ValueSet
end

module V = VerifyOwnedHIR.Make (Identity)
module C = V.Ownership
module W = VerifyOwnership.Make (Identity)
module I = InferOwnedFunctionGroups.Make (Identity)
module S = SelectOwnershipVariants
module Selector = S.Make (Identity)
module M = MaterializeOwnershipVariants
module Materialize = M.Make (Identity)
module F = OwnershipTestFormatting
module SV = StructuralValue
open O
open C

type leaf = Fresh of H.value
type externalRegistration = H.functionSignature * O.callSignature

let ( let* ) = Result.bind
let fid = TestIds.functionIdForName
let managed id : H.value = { H.id = H.ValueId id; typ = AST.TList AST.TInt64 }
let scalar id : H.value = { H.id = H.ValueId id; typ = AST.TUnit }
let unitValue = scalar 99

let parameter (value : H.value) : H.parameter =
  let (H.ValueId id) = value.H.id in
  { H.binding = AST.bindingId id; value }

let block parameters operations result : (leaf, H.valueId) O.block =
  { O.body = { H.parameters; operations; result } }

let definition name parameters operations result resultOwnership :
    (leaf, H.valueId) O.functionDef =
  {
    O.definition =
      {
        H.id = fid name;
        name;
        body =
          block
            (List.map (fun (value, _) -> parameter value) parameters)
            operations result;
      };
    ownership =
      { O.parameters = List.map snd parameters; result = resultOwnership };
  }

let unitFunction name parameters operations =
  definition name
    (parameters @ [ (unitValue, UnmanagedParameter) ])
    operations unitValue UnmanagedResult

let call target arguments result : H.functionCall =
  { H.target = fid target; arguments; result }

let invoke call = Evaluate (H.Call call)

let external_ (call : H.functionCall) modes result :
    AST.functionId * externalRegistration =
  ( call.H.target,
    ( {
        H.parameters = List.map (fun (v : H.value) -> v.H.typ) call.H.arguments;
        result = call.H.result.H.typ;
      },
      { O.parameters = modes; result } ) )

let ownership registrations aliases : leaf C.semantics =
  let argument (value : H.value) =
    match value.H.typ with
    | AST.TList _ ->
        Managed
          (Option.value ~default:value.H.id
             (H.ValueMap.find_opt value.H.id aliases))
    | _ -> Unmanaged
  in
  let uses (operand : H.operand) =
    CheckedAST.BindingIdMap.bindings operand.H.inputs
    |> List.filter_map (fun (_, value) ->
        match argument value with Managed id -> Some id | Unmanaged -> None)
    |> H.ValueSet.of_list
  in
  {
    C.leaf = (fun (Fresh value) -> { O.inputs = []; outputs = [ value.H.id ] });
    leafUniqueness =
      (fun (Fresh value) ->
        {
          C.requiredInputs = H.ValueSet.empty;
          uniqueOutputs = H.ValueSet.singleton value.H.id;
        });
    callOwnership =
      (fun (call : H.functionCall) ->
        Option.map snd (FunctionIdMap.tryFind call.H.target registrations));
    scalarUses = uses;
    scalarEscapes = uses;
    blockArgument = argument;
  }

let hir registrations : leaf VerifyOwnedHIR.hirContracts =
  {
    VerifyOwnedHIR.leaf =
      (fun (Fresh value) ->
        {
          H.inputs = [];
          operands = [];
          outputs = [ { H.value; alias = H.FreshManaged } ];
          effects = H.EffectSet.singleton H.MayAllocate;
        });
    callSignature =
      (fun target ->
        Option.map fst (FunctionIdMap.tryFind target registrations));
    callContract =
      (fun (call : H.functionCall) ->
        let alias =
          match (call.H.result.H.typ, call.H.arguments) with
          | AST.TList _, first :: rest -> H.MayAliasInputs (first, rest)
          | AST.TList _, [] -> H.FreshManaged
          | _ -> H.NoManagedAlias
        in
        Some
          {
            H.inputs = call.H.arguments;
            operands = [];
            outputs = [ { H.value = call.H.result; alias } ];
            effects = H.EffectSet.singleton H.MayInvokeUserCode;
          });
  }

let analyze registrations aliases definitions =
  let registrations = FunctionIdMap.ofList registrations in
  V.analyzeFunctions (hir registrations)
    (ownership registrations aliases)
    definitions

let svId (H.ValueId id) = SV.Union ("ValueId", [ SV.Scalar (string_of_int id) ])

let svSet values =
  SV.Union
    ( "set",
      [
        SV.Sequence
          (List.map
             (fun i -> SV.Scalar (string_of_int i))
             (O.IntSet.elements values));
      ] )

let svCall (call : H.functionCall) =
  SV.Record
    [
      ("Target", AST.DiagnosticFormatting.func call.H.target);
      ("Arguments", SV.Sequence (List.map F.value call.H.arguments));
      ("Result", F.value call.H.result);
    ]

let svFacts (facts : O.callSiteFacts) =
  SV.Record
    [
      ("Caller", AST.DiagnosticFormatting.func facts.O.caller);
      ("Call", svCall facts.O.call);
      ("Established", F.callSignature facts.O.established);
      ("UniqueArguments", svSet facts.O.uniqueArguments);
    ]

let svFactsList facts = SV.Sequence (List.map svFacts facts)

let svError = function
  | V.HIRVerificationFailed error ->
      SV.Union ("HIRVerificationFailed", [ VerifyHIR.errorValue error ])
  | V.OwnershipVerificationFailed error ->
      SV.Union ("OwnershipVerificationFailed", [ C.errorValue svId error ])

let svResult encode encodeError = function
  | Ok value -> SV.Union ("Ok", [ encode value ])
  | Error error -> SV.Union ("Error", [ encodeError error ])

let showResult result =
  StructuralFormat.format (svResult svFactsList svError result)

let showFacts facts = StructuralFormat.format (svFactsList facts)

let expect expected result =
  match result with
  | Error error ->
      Error
        ("Unexpected analysis failure: "
        ^ StructuralFormat.format (svError error))
  | Ok facts ->
      let actual =
        List.map
          (fun (fact : O.callSiteFacts) ->
            (fact.O.call.H.result.H.id, fact.O.uniqueArguments))
          facts
      in
      let same =
        List.length actual = List.length expected
        && List.for_all2
             (fun (id, set) (other, otherSet) ->
               id = other && O.IntSet.equal set otherSet)
             actual expected
      in
      if same then Ok ()
      else
        let show xs =
          StructuralFormat.format
            (SV.Sequence
               (List.map (fun (id, set) -> SV.Tuple [ svId id; svSet set ]) xs))
        in
        Error ("Expected call facts " ^ show expected ^ ", got " ^ show actual)

let at (call : H.functionCall) positions =
  (call.H.result.H.id, O.IntSet.of_list positions)

let empty = H.ValueMap.empty

let testParameterModes () =
  let unique = managed 0 in
  let consumed = managed 1 in
  let borrowed = managed 2 in
  let invocation =
    call "inspect" [ unitValue; unique; consumed; borrowed ] (scalar 10)
  in
  let modes =
    [
      UnmanagedCallParameter;
      BorrowedCallParameter;
      BorrowedCallParameter;
      BorrowedCallParameter;
    ]
  in
  let registration = external_ invocation modes UnmanagedCallResult in
  let caller =
    unitFunction "caller"
      [
        (unique, UniqueParameter unique.H.id);
        (consumed, ConsumedParameter consumed.H.id);
        (borrowed, BorrowedParameter borrowed.H.id);
      ]
      [ invoke invocation; Drop unique.H.id; Drop consumed.H.id ]
  in
  let result =
    Result.bind (analyze [ registration ] empty [ caller ]) (function
      | [ fact ]
        when fact.O.caller = caller.O.definition.H.id
             && fact.O.call = invocation
             && fact.O.established = snd (snd registration)
             && callSiteIdentity fact
                = {
                    O.caller = caller.O.definition.H.id;
                    result = invocation.H.result.H.id;
                  } ->
          Ok [ fact ]
      | _ ->
          Error (V.OwnershipVerificationFailed InconsistentFunctionParameters))
  in
  expect [ at invocation [ 1 ] ] result

let testPreTransferFacts () =
  let input = managed 0 in
  let output = managed 1 in
  let transfer = call "transfer" [ input ] output in
  let inspect = call "inspect" [ output ] (scalar 10) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke transfer; invoke inspect; Drop output.H.id ]
  in
  expect
    [ at transfer [ 0 ]; at inspect [] ]
    (analyze
       [
         external_ transfer [ ConsumedCallParameter ] ProducedCallResult;
         external_ inspect [ BorrowedCallParameter ] UnmanagedCallResult;
       ]
       empty [ caller ])

let testDupDrop () =
  let input = managed 0 in
  let before = call "inspect" [ input ] (scalar 10) in
  let during = call "inspect" [ input ] (scalar 11) in
  let after = call "inspect" [ input ] (scalar 12) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [
        invoke before;
        Dup input.H.id;
        invoke during;
        Drop input.H.id;
        invoke after;
        Drop input.H.id;
      ]
  in
  expect
    [ at before [ 0 ]; at during []; at after [ 0 ] ]
    (analyze
       [ external_ before [ BorrowedCallParameter ] UnmanagedCallResult ]
       empty [ caller ])

let testRepeatedConsumption () =
  let input = managed 0 in
  let invocation = call "consumeBoth" [ input; input ] (scalar 10) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ Dup input.H.id; invoke invocation ]
  in
  expect
    [ at invocation [] ]
    (analyze
       [
         external_ invocation
           [ ConsumedCallParameter; ConsumedCallParameter ]
           UnmanagedCallResult;
       ]
       empty [ caller ])

let testFreshAndUniqueResults () =
  let fresh = managed 0 in
  let produced = managed 1 in
  let inspectFresh = call "inspect" [ fresh ] (scalar 10) in
  let make = call "make" [] produced in
  let inspectProduced = call "inspect" [ produced ] (scalar 11) in
  let caller =
    unitFunction "caller" []
      [
        Evaluate (H.Leaf (Fresh fresh));
        invoke inspectFresh;
        Drop fresh.H.id;
        invoke make;
        invoke inspectProduced;
        Drop produced.H.id;
      ]
  in
  expect
    [ at inspectFresh [ 0 ]; at make []; at inspectProduced [ 0 ] ]
    (analyze
       [
         external_ inspectFresh [ BorrowedCallParameter ] UnmanagedCallResult;
         external_ make [] UniqueProducedCallResult;
       ]
       empty [ caller ])

let operand expression inputs : H.operand =
  { H.expression; typ = AST.TUnit; inputs }

let condition truth : H.operand =
  {
    H.expression = CheckedAST.BoolLiteral truth;
    typ = AST.TBool;
    inputs = CheckedAST.BindingIdMap.empty;
  }

let testEscapes () =
  let input = managed 0 in
  let before = call "inspect" [ input ] (scalar 10) in
  let after = call "inspect" [ input ] (scalar 11) in
  let escape =
    operand CheckedAST.UnitLiteral
      (CheckedAST.BindingIdMap.singleton (AST.bindingId 0) input)
  in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [
        invoke before;
        Evaluate (H.ScalarBinding (scalar 12, escape));
        Dup input.H.id;
        Drop input.H.id;
        invoke after;
        Drop input.H.id;
      ]
  in
  expect
    [ at before [ 0 ]; at after [] ]
    (analyze
       [ external_ before [ BorrowedCallParameter ] UnmanagedCallResult ]
       empty [ caller ])

let testBorrowedAlias () =
  let input = managed 0 in
  let alias = managed 1 in
  let borrow = call "borrow" [ input ] alias in
  let inspect = call "inspect" [ alias ] (scalar 10) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke borrow; invoke inspect; Drop input.H.id ]
  in
  expect
    [ at borrow [ 0 ]; at inspect [ 0 ] ]
    (analyze
       [
         external_ borrow [ BorrowedCallParameter ] (BorrowedCallResult 0);
         external_ inspect [ BorrowedCallParameter ] UnmanagedCallResult;
       ]
       (H.ValueMap.singleton alias.H.id input.H.id)
       [ caller ])

let branchFixture bothUnique =
  let yesValue = managed 0 in
  let noValue = managed 1 in
  let joined = managed 2 in
  let yesInspect = call "inspect" [ yesValue ] (scalar 10) in
  let noInspect = call "inspect" [ noValue ] (scalar 11) in
  let after = call "inspect" [ joined ] (scalar 12) in
  let make = call "make" [] noValue in
  let yes =
    block [] [ Evaluate (H.Leaf (Fresh yesValue)); invoke yesInspect ] yesValue
  in
  let no = block [] [ invoke make; invoke noInspect ] noValue in
  let caller =
    unitFunction "caller" []
      [
        Evaluate (H.Branch (joined, condition true, yes, no));
        invoke after;
        Drop joined.H.id;
      ]
  in
  ( caller,
    [
      external_ yesInspect [ BorrowedCallParameter ] UnmanagedCallResult;
      external_ make []
        (if bothUnique then UniqueProducedCallResult else ProducedCallResult);
    ],
    [
      at yesInspect [ 0 ];
      at make [];
      at noInspect (if bothUnique then [ 0 ] else []);
      at after (if bothUnique then [ 0 ] else []);
    ] )

let testBranchJoins bothUnique () =
  let caller, registrations, expected = branchFixture bothUnique in
  expect expected (analyze registrations empty [ caller ])

let testBranchStateIsolation () =
  let input = managed 0 in
  let yesCall = call "inspect" [ input ] (scalar 10) in
  let noCall = call "inspect" [ input ] (scalar 11) in
  let after = call "inspect" [ input ] (scalar 12) in
  let yes =
    block [] [ Dup input.H.id; invoke yesCall; Drop input.H.id ] unitValue
  in
  let no = block [] [ invoke noCall ] unitValue in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [
        Evaluate (H.Branch (scalar 13, condition false, yes, no));
        invoke after;
        Drop input.H.id;
      ]
  in
  expect
    [ at yesCall []; at noCall [ 0 ]; at after [ 0 ] ]
    (analyze
       [ external_ yesCall [ BorrowedCallParameter ] UnmanagedCallResult ]
       empty [ caller ])

let testBranchEscapeIntersectsUniqueness () =
  let input = managed 0 in
  let after = call "inspect" [ input ] (scalar 12) in
  let escape =
    operand CheckedAST.UnitLiteral
      (CheckedAST.BindingIdMap.singleton (AST.bindingId 0) input)
  in
  let yes =
    block [] [ Evaluate (H.ScalarBinding (scalar 13, escape)) ] unitValue
  in
  let no = block [] [] unitValue in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [
        Evaluate (H.Branch (scalar 14, condition true, yes, no));
        invoke after;
        Drop input.H.id;
      ]
  in
  expect
    [ at after [] ]
    (analyze
       [ external_ after [ BorrowedCallParameter ] UnmanagedCallResult ]
       empty [ caller ])

let testAliasedArguments () =
  let input = managed 0 in
  let alias = managed 1 in
  let borrow = call "borrow" [ input ] alias in
  let both = call "inspectBoth" [ input; alias ] (scalar 10) in
  let after = call "inspect" [ input ] (scalar 11) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke borrow; invoke both; invoke after; Drop input.H.id ]
  in
  expect
    [ at borrow [ 0 ]; at both []; at after [ 0 ] ]
    (analyze
       [
         external_ borrow [ BorrowedCallParameter ] (BorrowedCallResult 0);
         external_ both
           [ BorrowedCallParameter; BorrowedCallParameter ]
           UnmanagedCallResult;
         external_ after [ BorrowedCallParameter ] UnmanagedCallResult;
       ]
       (H.ValueMap.singleton alias.H.id input.H.id)
       [ caller ])

let testRejectsUniqueAliasedArgument () =
  let input = managed 0 in
  let invocation = call "consumeAndBorrow" [ input; input ] (scalar 10) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke invocation ]
  in
  match
    analyze
      [
        external_ invocation
          [ UniqueCallParameter; BorrowedCallParameter ]
          UnmanagedCallResult;
      ]
      empty [ caller ]
  with
  | Error (V.OwnershipVerificationFailed (NonUniqueUse id)) when id = input.H.id
    ->
      Ok ()
  | actual ->
      Error
        ("Expected an argument also borrowed by the call to reject unique \
          consumption, got " ^ showResult actual)

let testFailureDiscardsFacts () =
  let input = managed 0 in
  let invocation = call "inspect" [ input ] (scalar 10) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke invocation ]
  in
  let registrations =
    FunctionIdMap.ofList
      [ external_ invocation [ BorrowedCallParameter ] UnmanagedCallResult ]
  in
  let semantics = ownership registrations empty in
  let first = W.analyzeFunction semantics caller in
  let second =
    W.verifyFunction semantics caller.O.ownership caller.O.definition.H.body
  in
  let third = V.analyzeFunctions (hir registrations) semantics [ caller ] in
  match (first, second, third) with
  | Error a, Error b, Error (V.OwnershipVerificationFailed c)
    when a = UndroppedValues (H.ValueSet.singleton input.H.id) && a = b && b = c
    ->
      Ok ()
  | _ ->
      Error
        ("Expected no fact result after final ownership validation fails, got "
        ^ StructuralFormat.format
            (SV.Tuple
               [
                 svResult svFactsList (C.errorValue svId) first;
                 svResult (fun () -> SV.Scalar "()") (C.errorValue svId) second;
                 svResult svFactsList svError third;
               ]))

let testOwnedResultFromBorrow resultMode expectedPositions () =
  let input = managed 0 in
  let output = managed 1 in
  let produce = call "produce" [ input ] output in
  let inspect = call "inspect" [ input ] (scalar 10) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke produce; invoke inspect; Drop output.H.id; Drop input.H.id ]
  in
  expect
    [ at produce [ 0 ]; at inspect expectedPositions ]
    (analyze
       [
         external_ produce [ BorrowedCallParameter ] resultMode;
         external_ inspect [ BorrowedCallParameter ] UnmanagedCallResult;
       ]
       empty [ caller ])

let testOwnedResultFromDuplicatedInput () =
  let input = managed 0 in
  let output = managed 1 in
  let transfer = call "transfer" [ input ] output in
  let beforeDrop = call "inspect" [ input ] (scalar 10) in
  let afterDrop = call "inspect" [ input ] (scalar 11) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [
        Dup input.H.id;
        invoke transfer;
        invoke beforeDrop;
        Drop output.H.id;
        invoke afterDrop;
        Drop input.H.id;
      ]
  in
  expect
    [ at transfer []; at beforeDrop []; at afterDrop [] ]
    (analyze
       [
         external_ transfer [ ConsumedCallParameter ] ProducedCallResult;
         external_ beforeDrop [ BorrowedCallParameter ] UnmanagedCallResult;
       ]
       empty [ caller ])

let testRejectsUniqueUseAfterAliasingResult () =
  let input = managed 0 in
  let output = managed 1 in
  let produce = call "produce" [ input ] output in
  let consume = call "consumeUnique" [ input ] (scalar 10) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke produce; invoke consume; Drop output.H.id ]
  in
  let registrations =
    FunctionIdMap.ofList
      [
        external_ produce [ BorrowedCallParameter ] ProducedCallResult;
        external_ consume [ UniqueCallParameter ] UnmanagedCallResult;
      ]
  in
  let semantics = ownership registrations empty in
  let first = W.analyzeFunction semantics caller in
  let second =
    W.verifyFunction semantics caller.O.ownership caller.O.definition.H.body
  in
  match (first, second) with
  | Error (NonUniqueUse a), Error (NonUniqueUse b) when a = input.H.id && a = b
    ->
      Ok ()
  | _ ->
      Error
        ("Expected both verification and analysis to reject an aliased unique \
          input, got "
        ^ StructuralFormat.format
            (SV.Tuple
               [
                 svResult svFactsList (C.errorValue svId) first;
                 svResult (fun () -> SV.Scalar "()") (C.errorValue svId) second;
               ]))

let testFunctionScopedSites () =
  let input = managed 0 in
  let invocation = call "inspect" [ input ] (scalar 10) in
  let first =
    unitFunction "first"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke invocation; Drop input.H.id ]
  in
  let second =
    unitFunction "second"
      [ (input, ConsumedParameter input.H.id) ]
      [ invoke invocation; Drop input.H.id ]
  in
  match
    analyze
      [ external_ invocation [ BorrowedCallParameter ] UnmanagedCallResult ]
      empty [ first; second ]
  with
  | Ok [ a; b ]
    when O.IntSet.equal a.O.uniqueArguments (O.IntSet.singleton 0)
         && O.IntSet.is_empty b.O.uniqueArguments
         && callSiteIdentity a <> callSiteIdentity b ->
      Ok ()
  | actual ->
      Error
        ("Expected function-local values to retain distinct call-site \
          identities, got " ^ showResult actual)

let testRecursiveContracts self () =
  let input = managed 0 in
  let output = managed 1 in
  let firstCall = call (if self then "first" else "second") [ input ] output in
  let secondCall = call "first" [ input ] output in
  let recursive name invocation =
    definition name
      [ (input, UniqueParameter input.H.id) ]
      [ invoke invocation ]
      output (UniqueProducedResult output.H.id)
  in
  let definitions =
    if self then [ recursive "first" firstCall ]
    else [ recursive "first" firstCall; recursive "second" secondCall ]
  in
  match analyze [] empty definitions with
  | Ok facts
    when List.length facts = List.length definitions
         && List.for_all
              (fun (fact : O.callSiteFacts) ->
                O.IntSet.equal fact.O.uniqueArguments (O.IntSet.singleton 0)
                && fact.O.established
                   = {
                       O.parameters = [ UniqueCallParameter ];
                       result = UniqueProducedCallResult;
                     })
              facts ->
      Ok ()
  | actual ->
      Error
        ("Expected recursive call facts to use the group's derived boundaries, \
          got " ^ showResult actual)

let testConflictingRegistration () =
  let input = managed 0 in
  let identity =
    definition "identity"
      [ (input, ConsumedParameter input.H.id) ]
      [] input (ProducedResult input.H.id)
  in
  let invocation = call "identity" [ input ] input in
  match
    analyze
      [ external_ invocation [ BorrowedCallParameter ] ProducedCallResult ]
      empty [ identity ]
  with
  | Error
      (V.OwnershipVerificationFailed
         (InconsistentRegisteredCallOwnership target))
    when target = identity.O.definition.H.id ->
      Ok ()
  | actual ->
      Error
        ("Expected conflicting registered ownership to prevent analysis, got "
       ^ showResult actual)

let testTypedFailure () =
  let input = managed 0 in
  let invocation = call "inspect" [ input ] (scalar 10) in
  let caller =
    unitFunction "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke invocation; Drop input.H.id ]
  in
  let registrations =
    FunctionIdMap.ofList
      [ external_ invocation [ BorrowedCallParameter ] UnmanagedCallResult ]
  in
  let contracts =
    {
      (hir registrations) with
      VerifyOwnedHIR.callSignature =
        (fun _ -> Some { H.parameters = [ AST.TInt64 ]; result = AST.TUnit });
    }
  in
  let semantics = ownership registrations empty in
  let first = V.analyzeFunctions contracts semantics [ caller ] in
  let second = V.verifyFunctions contracts semantics [ caller ] in
  match (first, second) with
  | Error (V.HIRVerificationFailed a), Error (V.HIRVerificationFailed b)
    when a = b ->
      Ok ()
  | _ ->
      Error
        ("Expected the same typed-HIR error before collecting facts, got "
        ^ StructuralFormat.format
            (SV.Tuple
               [
                 svResult svFactsList svError first;
                 svResult (fun () -> SV.Scalar "()") svError second;
               ]))

let svSelectionError = function
  | S.DuplicateFunctionName name ->
      StructuralValue.Union
        ("DuplicateFunctionName", [ StructuralValue.Text name ])
  | S.UnknownFunction name ->
      StructuralValue.Union ("UnknownFunction", [ StructuralValue.Text name ])
  | S.InvalidUniqueArgumentIndex (name, index) ->
      StructuralValue.Union
        ( "InvalidUniqueArgumentIndex",
          [
            StructuralValue.Text name;
            StructuralValue.Scalar (string_of_int index);
          ] )
  | S.MissingEstablishedUniqueArgument (name, index) ->
      StructuralValue.Union
        ( "MissingEstablishedUniqueArgument",
          [
            StructuralValue.Text name;
            StructuralValue.Scalar (string_of_int index);
          ] )
  | S.InconsistentEstablishedBoundary name ->
      StructuralValue.Union
        ("InconsistentEstablishedBoundary", [ StructuralValue.Text name ])

let showSelectionError error = StructuralFormat.format (svSelectionError error)

let showInferenceError = function
  | I.FunctionGroupingFailed (OwnedFunctionGroups.DuplicateFunctionName id) ->
      "FunctionGroupingFailed (DuplicateFunctionName "
      ^ StructuralFormat.format (AST.DiagnosticFormatting.func id)
      ^ ")"
  | I.DemandTargetMissing id ->
      "DemandTargetMissing "
      ^ StructuralFormat.format (AST.DiagnosticFormatting.func id)
  | I.GroupInferenceFailed (names, cause) ->
      let cause =
        match cause with
        | I.Uniqueness.VariantLimitExceeded (count, maximum) ->
            Printf.sprintf "VariantLimitExceeded (%d, %d)" count maximum
        | I.Uniqueness.RecursiveFunctionRequiresGroupInference id ->
            "RecursiveFunctionRequiresGroupInference "
            ^ StructuralFormat.format (AST.DiagnosticFormatting.func id)
        | I.Uniqueness.NoVerifiedBoundary error ->
            "NoVerifiedBoundary ("
            ^ C.errorToString (fun id -> svId id) error
            ^ ")"
        | I.Uniqueness.NoVerifiedFunctionGroup error ->
            "NoVerifiedFunctionGroup ("
            ^ C.errorToString (fun id -> svId id) error
            ^ ")"
      in
      "GroupInferenceFailed ("
      ^ StructuralFormat.format
          (StructuralValue.Record
             [
               ("Head", StructuralValue.Text names.NonEmptyList.head);
               ( "Tail",
                 StructuralValue.Sequence
                   (List.map
                      (fun name -> StructuralValue.Text name)
                      names.NonEmptyList.tail) );
             ])
      ^ ", " ^ cause ^ ")"

let svSite (site : O.callSiteIdentity) =
  StructuralValue.Record
    [
      ("Caller", AST.DiagnosticFormatting.func site.O.caller);
      ("Result", svId site.O.result);
    ]

let svVerification = svError

let svMaterialError = function
  | Materialize.GroupingFailed (OwnedFunctionGroups.DuplicateFunctionName id) ->
      StructuralValue.Union
        ( "GroupingFailed",
          [
            StructuralValue.Union
              ("DuplicateFunctionName", [ AST.DiagnosticFormatting.func id ]);
          ] )
  | Materialize.InvalidOriginalProgram error ->
      StructuralValue.Union ("InvalidOriginalProgram", [ svVerification error ])
  | Materialize.InvalidMaterializedProgram error ->
      StructuralValue.Union
        ("InvalidMaterializedProgram", [ svVerification error ])
  | Materialize.MissingGroupMember name ->
      StructuralValue.Union ("MissingGroupMember", [ StructuralValue.Text name ])
  | Materialize.GroupMembershipMismatch name ->
      StructuralValue.Union
        ("GroupMembershipMismatch", [ StructuralValue.Text name ])
  | Materialize.BoundaryMismatch name ->
      StructuralValue.Union ("BoundaryMismatch", [ StructuralValue.Text name ])
  | Materialize.SymbolCollision name ->
      StructuralValue.Union ("SymbolCollision", [ StructuralValue.Text name ])
  | Materialize.MissingCallSite site ->
      StructuralValue.Union ("MissingCallSite", [ svSite site ])
  | Materialize.DuplicateCallSite site ->
      StructuralValue.Union ("DuplicateCallSite", [ svSite site ])
  | Materialize.StaleCallSite site ->
      StructuralValue.Union ("StaleCallSite", [ svSite site ])
  | Materialize.MixedRecursiveCandidate site ->
      StructuralValue.Union ("MixedRecursiveCandidate", [ svSite site ])

let reportMaterial result =
  Result.map_error
    (fun error -> StructuralFormat.format (svMaterialError error))
    result

let testSpecializationHandoff () =
  let input = managed 0 in
  let output = managed 1 in
  let identity =
    definition "identity"
      [ (input, ConsumedParameter input.H.id) ]
      [] input (ProducedResult input.H.id)
  in
  let invocation = call "identity" [ input ] output in
  let caller =
    definition "caller"
      [ (input, UniqueParameter input.H.id) ]
      [ invoke invocation ]
      output (ProducedResult output.H.id)
  in
  let definitions = [ identity; caller ] in
  let semantics = ownership FunctionIdMap.empty empty in
  let contracts = hir FunctionIdMap.empty in
  let* facts =
    V.analyzeFunctions contracts semantics definitions
    |> Result.map_error (fun error -> StructuralFormat.format (svError error))
  in
  match facts with
  | [ fact ] -> (
      let* groups =
        I.infer semantics [ identity ] |> Result.map_error showInferenceError
      in
      let* catalog = S.create groups |> Result.map_error showSelectionError in
      let* selection =
        Selector.select catalog
          {
            S.target = identity.O.definition.H.name;
            established = fact.O.established;
            uniqueArguments = fact.O.uniqueArguments;
          }
        |> Result.map_error showSelectionError
      in
      let* plan =
        Materialize.materialize contracts semantics FunctionIdMap.empty
          definitions
          [ { M.caller = fact.O.caller; call = fact.O.call; selection } ]
        |> reportMaterial
      in
      let* specializedFacts =
        V.analyzeFunctions
          (M.hirContracts plan contracts)
          (Materialize.ownershipSemantics plan semantics)
          (M.functions plan)
        |> Result.map_error (fun error ->
            StructuralFormat.format (svError error))
      in
      match specializedFacts with
      | [ specialized ]
        when callSiteIdentity specialized = callSiteIdentity fact
             && O.IntSet.equal specialized.O.uniqueArguments
                  (O.IntSet.singleton 0)
             && specialized.O.established
                = {
                    O.parameters = [ UniqueCallParameter ];
                    result = UniqueProducedCallResult;
                  } ->
          Ok ()
      | actual ->
          Error
            ("Expected analyzed facts to establish a verified unique call \
              boundary, got " ^ showFacts actual))
  | actual ->
      Error
        ("Expected the original caller's single call fact, got "
       ^ showFacts actual)

let tests =
  [
    ( "Call facts distinguish unmanaged borrowed consumed and unique parameters",
      testParameterModes );
    ("Call facts are captured before argument transfer", testPreTransferFacts);
    ( "Call facts suspend and restore uniqueness across dup and drop",
      testDupDrop );
    ( "Call facts retain multiplicity for repeated consuming arguments",
      testRepeatedConsumption );
    ( "Call facts recognize fresh values and verified unique results",
      testFreshAndUniqueResults );
    ("Call facts permanently revoke escaped provenance", testEscapes);
    ("Call facts resolve borrowed result aliases", testBorrowedAlias);
    ("Call facts intersect uniqueness at managed joins", testBranchJoins false);
    ( "Call facts retain uniqueness when both managed arms are unique",
      testBranchJoins true );
    ( "Call facts keep sibling ownership states independent",
      testBranchStateIsolation );
    ( "Call facts intersect exclusivity when one unmanaged branch escapes",
      testBranchEscapeIntersectsUniqueness );
    ( "Call facts account for aliases passed to the same call",
      testAliasedArguments );
    ( "Call verification rejects unique arguments aliased in the same call",
      testRejectsUniqueAliasedArgument );
    ( "Call analysis returns no partial facts after validation failure",
      testFailureDiscardsFacts );
    ( "Call facts revoke borrowed provenance for potentially aliasing owned \
       results",
      testOwnedResultFromBorrow ProducedCallResult [] );
    ( "Call facts preserve borrowed provenance for independently unique results",
      testOwnedResultFromBorrow UniqueProducedCallResult [ 0 ] );
    ( "Call facts revoke surviving consumed provenance for potentially \
       aliasing owned results",
      testOwnedResultFromDuplicatedInput );
    ( "Call verification rejects uniqueness after an owned result may alias",
      testRejectsUniqueUseAfterAliasingResult );
    ("Call facts scope identities to their caller", testFunctionScopedSites);
    ("Call facts derive self-recursive boundaries", testRecursiveContracts true);
    ( "Call facts derive mutually recursive boundaries",
      testRecursiveContracts false );
    ( "Call analysis rejects conflicting group registrations",
      testConflictingRegistration );
    ("Call analysis preserves typed HIR verification failures", testTypedFailure);
    ( "Analyzed facts drive verified selection and materialization",
      testSpecializationHandoff );
  ]
