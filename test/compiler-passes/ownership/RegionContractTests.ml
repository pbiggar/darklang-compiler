(* RegionContractTests.ml - Unit ownership laws independent of collection layout and codegen. *)
[@@@warning "-4-42"]

open Dark_compiler
module H = HIR
module O = OwnedIR

module Identity = struct
  type t = string

  let compare = StringOrder.compare

  module Set = StringOrder.Set
end

module V = VerifyOwnership.Make (Identity)
module C = V.Ownership
open O
open C

let fid = TestIds.functionIdForName

let identity = function
  | "a" -> H.ValueId 0
  | "b" -> H.ValueId 1
  | "c" -> H.ValueId 2
  | "aAlias" -> H.ValueId 3
  | "scalar" -> H.ValueId 4
  | name -> Crash.crash ("Unsupported ownership fixture identity " ^ name)

let binding = function
  | "a" -> AST.bindingId 0
  | "b" -> AST.bindingId 1
  | "c" -> AST.bindingId 2
  | "aAlias" -> AST.bindingId 3
  | "scalar" -> AST.bindingId 4
  | "escape" -> AST.bindingId 5
  | name -> Crash.crash ("Unsupported ownership fixture binding " ^ name)

let bindingName id =
  match
    List.find_opt
      (fun name -> binding name = id)
      [ "a"; "b"; "c"; "aAlias"; "scalar"; "escape" ]
  with
  | Some name -> name
  | None -> Crash.crash "Unsupported ownership fixture binding identity"

let value name : H.value = { H.id = identity name; typ = AST.TInt64 }
let unitValue : H.value = { H.id = H.ValueId 100; typ = AST.TUnit }

let reference name : H.operand =
  let id = binding name in
  {
    H.expression = CheckedAST.Local id;
    typ = AST.TInt64;
    inputs = CheckedAST.BindingIdMap.singleton id (value name);
  }

let condition : H.operand =
  {
    H.expression = CheckedAST.BoolLiteral true;
    typ = AST.TBool;
    inputs = CheckedAST.BindingIdMap.empty;
  }

type testLeaf = {
  ownership : string O.contract;
  uniqueness : C.uniquenessContract;
}

let semantics : testLeaf C.semantics =
  {
    C.leaf = (fun leaf -> leaf.ownership);
    leafUniqueness = (fun leaf -> leaf.uniqueness);
    callOwnership =
      (fun (call : H.functionCall) ->
        if call.H.target = fid "borrow" then
          Some
            {
              O.parameters = [ BorrowedCallParameter ];
              result = BorrowedCallResult 0;
            }
        else if call.H.target = fid "consume" then
          Some
            {
              O.parameters = [ ConsumedCallParameter ];
              result = ProducedCallResult;
            }
        else if call.H.target = fid "produce" then
          Some { O.parameters = []; result = ProducedCallResult }
        else if call.H.target = fid "consumeUnique" then
          Some
            {
              O.parameters = [ UniqueCallParameter ];
              result = UnmanagedCallResult;
            }
        else if call.H.target = fid "produceUnique" then
          Some { O.parameters = []; result = UniqueProducedCallResult }
        else if call.H.target = fid "consumeTwice" then
          Some
            {
              O.parameters = [ ConsumedCallParameter; ConsumedCallParameter ];
              result = UnmanagedCallResult;
            }
        else if call.H.target = fid "discard" then
          Some
            {
              O.parameters = [ ConsumedCallParameter ];
              result = UnmanagedCallResult;
            }
        else if call.H.target = fid "invalidBorrow" then
          Some
            {
              O.parameters = [ ConsumedCallParameter ];
              result = BorrowedCallResult 0;
            }
        else if call.H.target = fid "negativeBorrow" then
          Some
            {
              O.parameters = [ BorrowedCallParameter ];
              result = BorrowedCallResult (-1);
            }
        else None);
    scalarUses =
      (fun (v : H.operand) ->
        CheckedAST.BindingIdMap.bindings v.H.inputs
        |> List.map (fun (id, _) -> bindingName id)
        |> Identity.Set.of_list);
    scalarEscapes =
      (fun (v : H.operand) ->
        match v.H.expression with
        | CheckedAST.Local id when id = binding "escape" ->
            CheckedAST.BindingIdMap.bindings v.H.inputs
            |> List.map (fun (id, _) -> bindingName id)
            |> Identity.Set.of_list
        | _ -> Identity.Set.empty);
    blockArgument =
      (fun (v : H.value) ->
        match v.H.id with
        | H.ValueId 0 -> Managed "a"
        | H.ValueId 1 -> Managed "b"
        | H.ValueId 2 -> Managed "c"
        | H.ValueId 3 -> Managed "a"
        | _ -> Unmanaged);
  }

let drops values = List.map (fun id -> Drop id) values

let leaf inputs outputs required uniqueOutputs =
  {
    ownership = { O.inputs; outputs };
    uniqueness =
      {
        C.requiredInputs = Identity.Set.of_list required;
        uniqueOutputs = Identity.Set.of_list uniqueOutputs;
      };
  }

let stepWithUniqueness inputs outputs required uniqueOutputs releases =
  Evaluate (H.Leaf (leaf inputs outputs required uniqueOutputs))
  :: drops releases

let step inputs outputs releases =
  stepWithUniqueness inputs outputs [] outputs releases

let blockResult entry operations result : (testLeaf, string) O.block =
  {
    O.body =
      {
        H.parameters = [];
        operations = drops entry @ List.concat operations;
        result;
      };
  }

let block entry operations = blockResult entry operations unitValue

let functionBlock parameters operations result : (testLeaf, string) O.block =
  {
    O.body =
      {
        H.parameters =
          List.map
            (fun name -> { H.binding = binding name; value = value name })
            parameters;
        operations = List.concat operations;
        result;
      };
  }

let branch predicate yes no =
  [
    Evaluate
      (H.Branch ({ H.id = H.ValueId 101; typ = AST.TUnit }, predicate, yes, no));
  ]

let managedBranch result predicate yes no =
  [ Evaluate (H.Branch (result, predicate, yes, no)) ]

let read name releases =
  Evaluate
    (H.ScalarBinding ({ H.id = H.ValueId 102; typ = AST.TInt64 }, reference name))
  :: drops releases

let escape name releases =
  let operand =
    { (reference name) with H.expression = CheckedAST.Local (binding "escape") }
  in
  Evaluate
    (H.ScalarBinding ({ H.id = H.ValueId 103; typ = AST.TInt64 }, operand))
  :: drops releases

let call target arguments result =
  [ Evaluate (H.Call { H.target = fid target; arguments; result }) ]

let duplicate value = [ Dup value ]
let dropOne value = [ Drop value ]

let showResult encode result =
  let open StructuralValue in
  let value =
    match result with
    | Ok value -> Union ("Ok", [ encode value ])
    | Error error -> Union ("Error", [ C.errorValue (fun id -> Text id) error ])
  in
  StructuralFormat.format value

let mismatch encode expected actual =
  Error
    ("Expected: " ^ showResult encode expected ^ "\nActual:   "
   ^ showResult encode actual)

let unitResultValue () = StructuralValue.Scalar "()"

let check expected region () =
  let actual = V.verifyClosed semantics region in
  if actual = expected then Ok () else mismatch unitResultValue expected actual

let checkFunction expected signature body () =
  let actual = V.verifyFunction semantics signature body in
  if actual = expected then Ok () else mismatch unitResultValue expected actual

let checkCallSignature expected signature () =
  let actual = V.callSignatureOfFunction signature in
  if actual = expected then Ok ()
  else mismatch OwnershipTestFormatting.callSignature expected actual

let tests =
  [
    ( "Ownership mismatch diagnostics retain expected errors and actual success",
      fun () ->
        match
          check
            (Error (UndroppedValues (Identity.Set.singleton "a")))
            (block [] []) ()
        with
        | Error
            "Expected: Error (UndroppedValues (set [\"a\"]))\nActual:   Ok ()"
          ->
            Ok ()
        | Error message -> Error ("Unexpected mismatch diagnostic: " ^ message)
        | Ok () -> Error "Expected an ownership mismatch diagnostic" );
    ( "Ownership mismatch diagnostics retain both distinct errors",
      fun () ->
        match check (Error (InvalidDrop "b")) (block [ "a" ] []) () with
        | Error
            "Expected: Error (InvalidDrop \"b\")\n\
             Actual:   Error (InvalidDrop \"a\")" ->
            Ok ()
        | Error message -> Error ("Unexpected mismatch diagnostic: " ^ message)
        | Ok () -> Error "Expected an ownership mismatch diagnostic" );
    ( "Call signature mismatch diagnostics retain parameters and result \
       positions",
      fun () ->
        match
          checkCallSignature (Error InconsistentFunctionParameters)
            {
              O.parameters = [ BorrowedParameter "a" ];
              result = BorrowedResult "a";
            }
            ()
        with
        | Error
            "Expected: Error InconsistentFunctionParameters\n\
             Actual:   Ok {Parameters = [BorrowedCallParameter]; Result = \
             BorrowedCallResult 0}" ->
            Ok ()
        | Error message -> Error ("Unexpected mismatch diagnostic: " ^ message)
        | Ok () -> Error "Expected a call signature mismatch diagnostic" );
    ( "Function ownership signatures permit borrowed results from borrowed \
       parameters",
      checkFunction (Ok ())
        {
          O.parameters = [ BorrowedParameter "a" ];
          result = BorrowedResult "a";
        }
        (functionBlock [ "a" ] [] (value "a")) );
    ( "Function ownership signatures cover unmanaged parameters explicitly",
      checkFunction (Ok ())
        { O.parameters = [ UnmanagedParameter ]; result = UnmanagedResult }
        (functionBlock [ "scalar" ] [] unitValue) );
    ( "Function ownership signatures reject omitted unmanaged parameters",
      checkFunction (Error InconsistentFunctionParameters)
        { O.parameters = []; result = UnmanagedResult }
        (functionBlock [ "scalar" ] [] unitValue) );
    ( "Function ownership signatures reject ownership in the wrong parameter \
       position",
      checkFunction (Error InconsistentFunctionParameters)
        {
          O.parameters = [ BorrowedParameter "a"; UnmanagedParameter ];
          result = UnmanagedResult;
        }
        (functionBlock [ "scalar"; "a" ] [ dropOne "a" ] unitValue) );
    ( "Function ownership signatures derive positional call contracts",
      checkCallSignature
        (Ok
           {
             O.parameters =
               [
                 UnmanagedCallParameter;
                 BorrowedCallParameter;
                 UniqueCallParameter;
               ];
             result = BorrowedCallResult 1;
           })
        {
          O.parameters =
            [ UnmanagedParameter; BorrowedParameter "a"; UniqueParameter "b" ];
          result = BorrowedResult "a";
        } );
    ( "Function ownership signatures reject borrowed calls from transferred \
       parameters",
      checkCallSignature (Error (InvalidBorrowedResult "a"))
        {
          O.parameters = [ ConsumedParameter "a" ];
          result = BorrowedResult "a";
        } );
    ( "Function ownership signatures reject consuming borrowed parameters",
      checkFunction (Error (InvalidDrop "a"))
        { O.parameters = [ BorrowedParameter "a" ]; result = UnmanagedResult }
        (functionBlock [ "a" ] [ step [ Consumed "a" ] [] [] ] unitValue) );
    ( "Function ownership signatures transfer consumed parameters into \
       produced results",
      checkFunction (Ok ())
        {
          O.parameters = [ ConsumedParameter "a" ];
          result = ProducedResult "a";
        }
        (functionBlock [ "a" ] [] (value "a")) );
    ( "Function ownership signatures transfer locally produced results",
      checkFunction (Ok ())
        { O.parameters = []; result = ProducedResult "a" }
        (functionBlock [] [ step [] [ "a" ] [] ] (value "a")) );
    ( "Unique function results accept locally exclusive values",
      checkFunction (Ok ())
        { O.parameters = []; result = UniqueProducedResult "a" }
        (functionBlock [] [ step [] [ "a" ] [] ] (value "a")) );
    ( "Unique function results reject ordinary consumed parameters",
      checkFunction (Error (NonUniqueUse "a"))
        {
          O.parameters = [ ConsumedParameter "a" ];
          result = UniqueProducedResult "a";
        }
        (functionBlock [ "a" ] [] (value "a")) );
    ( "Unique function parameters satisfy unique-consuming operations",
      checkFunction (Ok ())
        { O.parameters = [ UniqueParameter "a" ]; result = UnmanagedResult }
        (functionBlock [ "a" ]
           [ stepWithUniqueness [ Consumed "a" ] [] [ "a" ] [] [] ]
           unitValue) );
    ( "Ordinary consumed parameters do not satisfy unique-consuming operations",
      checkFunction (Error (NonUniqueUse "a"))
        { O.parameters = [ ConsumedParameter "a" ]; result = UnmanagedResult }
        (functionBlock [ "a" ]
           [ stepWithUniqueness [ Consumed "a" ] [] [ "a" ] [] [] ]
           unitValue) );
    ( "Function ownership signatures require consumed parameters to leave the \
       function",
      checkFunction
        (Error (UndroppedValues (Identity.Set.singleton "a")))
        { O.parameters = [ ConsumedParameter "a" ]; result = UnmanagedResult }
        (functionBlock [ "a" ] [] unitValue) );
    ( "Function ownership signatures reject producing borrowed parameters",
      checkFunction (Error (InvalidProducedResult "a"))
        {
          O.parameters = [ BorrowedParameter "a" ];
          result = ProducedResult "a";
        }
        (functionBlock [ "a" ] [] (value "a")) );
    ( "Explicit dup permits producing ownership from borrowed parameters",
      checkFunction (Ok ())
        {
          O.parameters = [ BorrowedParameter "a" ];
          result = ProducedResult "a";
        }
        (functionBlock [ "a" ] [ duplicate "a" ] (value "a")) );
    ( "Function ownership signatures reject borrowing consumed parameters",
      checkFunction (Error (InvalidBorrowedResult "a"))
        {
          O.parameters = [ ConsumedParameter "a" ];
          result = BorrowedResult "a";
        }
        (functionBlock [ "a" ] [] (value "a")) );
    ( "Function ownership signatures cover every managed parameter",
      checkFunction (Error InconsistentFunctionParameters)
        { O.parameters = []; result = UnmanagedResult }
        (functionBlock [ "a" ] [] unitValue) );
    ( "Function ownership signatures reject duplicate parameters",
      checkFunction (Error (DuplicateParameter "a"))
        {
          O.parameters = [ BorrowedParameter "a"; BorrowedParameter "a" ];
          result = UnmanagedResult;
        }
        (functionBlock [ "a" ] [] unitValue) );
    ( "Function ownership signatures agree with the managed result identity",
      checkFunction (Error InconsistentFunctionResult)
        {
          O.parameters = [ ConsumedParameter "a" ];
          result = ProducedResult "b";
        }
        (functionBlock [ "a" ] [] (value "a")) );
    ( "Call ownership signatures preserve borrowed arguments and results",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             call "borrow" [ value "a" ] (value "aAlias");
             step [ Consumed "a" ] [] [];
           ]) );
    ( "Call ownership signatures transfer consumed arguments into produced \
       results",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             call "consume" [ value "a" ] (value "b");
             step [ Consumed "b" ] [] [];
           ]) );
    ( "Unique call parameters reject results without exclusivity provenance",
      check (Error (NonUniqueUse "a"))
        (block []
           [
             call "produce" [] (value "a");
             call "consumeUnique" [ value "a" ] unitValue;
           ]) );
    ( "Unique call results satisfy unique call parameters",
      check (Ok ())
        (block []
           [
             call "produceUnique" [] (value "a");
             call "consumeUnique" [ value "a" ] unitValue;
           ]) );
    ( "Explicit dup satisfies repeated consumed call arguments",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             duplicate "a";
             call "consumeTwice" [ value "a"; value "aAlias" ] unitValue;
           ]) );
    ( "Repeated consumed call arguments reject missing dup",
      check (Error (InvalidDrop "a"))
        (block []
           [
             step [] [ "a" ] [];
             call "consumeTwice" [ value "a"; value "aAlias" ] unitValue;
           ]) );
    ( "Call ownership signatures reject unavailable consumed arguments",
      check (Error (InvalidUse "a"))
        (block [] [ call "discard" [ value "a" ] unitValue ]) );
    ( "Call ownership signatures reject unknown targets",
      check
        (Error (UnknownCallOwnership (fid "opaque")))
        (block [] [ call "opaque" [ value "a" ] unitValue ]) );
    ( "Call ownership signatures reject borrowed results from consumed \
       parameters",
      check
        (Error (InvalidBorrowedCallResult (fid "invalidBorrow", 0)))
        (block []
           [
             step [] [ "a" ] []; call "invalidBorrow" [ value "a" ] (value "a");
           ]) );
    ( "Call ownership signatures reject negative borrowed-result parameters",
      check
        (Error (InvalidBorrowedCallResult (fid "negativeBorrow", -1)))
        (block []
           [
             step [] [ "a" ] [];
             call "negativeBorrow" [ value "a" ] (value "aAlias");
           ]) );
    ( "Ownership contracts borrow before consuming multiple inputs",
      check (Ok ())
        (block []
           [
             step [] [ "a"; "b" ] [];
             step [ Borrowed "a"; Consumed "a"; Consumed "b" ] [ "c" ] [ "c" ];
           ]) );
    ( "Ownership contracts reject duplicate consumed units",
      check (Error (InvalidDrop "a"))
        (block []
           [ step [] [ "a" ] []; step [ Consumed "a"; Consumed "a" ] [] [] ]) );
    ( "Explicit dup permits multiple consuming uses",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             duplicate "a";
             step [ Consumed "a"; Consumed "a" ] [] [];
           ]) );
    ( "Fresh outputs satisfy unique-consuming operations",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             stepWithUniqueness [ Consumed "a" ] [] [ "a" ] [] [];
           ]) );
    ( "Explicit dup temporarily prevents unique use",
      check (Error (NonUniqueUse "a"))
        (block []
           [
             step [] [ "a" ] [];
             duplicate "a";
             stepWithUniqueness [ Consumed "a" ] [] [ "a" ] [] [];
           ]) );
    ( "Explicit drop restores unique use after dup",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             duplicate "a";
             dropOne "a";
             stepWithUniqueness [ Consumed "a" ] [] [ "a" ] [] [];
           ]) );
    ( "Explicit dup of a borrowed parameter creates a droppable unit",
      checkFunction (Ok ())
        { O.parameters = [ BorrowedParameter "a" ]; result = UnmanagedResult }
        (functionBlock [ "a" ]
           [ duplicate "a"; step [ Consumed "a" ] [] [] ]
           unitValue) );
    ( "Ownership contracts reject duplicate result identities",
      check (Error (DuplicateDefinition "a"))
        (block [] [ step [] [ "a"; "a" ] [] ]) );
    ( "Ownership contracts reject unknown borrows",
      check (Error (InvalidUse "a")) (block [] [ step [ Borrowed "a" ] [] [] ])
    );
    ( "Ownership contracts reject drop after consume",
      check (Error (InvalidDrop "a"))
        (block [] [ step [] [ "a" ] []; step [ Consumed "a" ] [] [ "a" ] ]) );
    ( "Ownership contracts account for scalar operand reads",
      check (Ok ()) (block [] [ step [] [ "a" ] []; read "a" [ "a" ] ]) );
    ( "Scalar escape revokes exclusivity provenance",
      check (Error (NonUniqueUse "a"))
        (block []
           [
             step [] [ "a" ] [];
             escape "a" [];
             stepWithUniqueness [ Consumed "a" ] [] [ "a" ] [] [];
           ]) );
    ( "Ownership contracts reject scalar use after release",
      check (Error (InvalidUse "a"))
        (block [] [ step [] [ "a" ] [ "a" ]; read "a" [] ]) );
    ( "Ownership contracts check branch conditions",
      check (Error (InvalidUse "a"))
        (block [] [ branch (reference "a") (block [] []) (block [] []) ]) );
    ( "Ownership contracts check block results",
      check (Error (InvalidUse "a"))
        (blockResult [] [ step [] [ "a" ] [ "a" ] ] (value "a")) );
    ( "Ownership contracts allow consumption on exclusive paths",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             branch condition
               (block [] [ step [ Consumed "a" ] [] [] ])
               (block [] [ step [ Consumed "a" ] [] [] ]);
           ]) );
    ( "Ownership contracts preserve values through nested joins",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             branch condition
               (block []
                  [ branch condition (block [] [ read "a" [] ]) (block [] []) ])
               (block [] [ read "a" [] ]);
             step [ Consumed "a" ] [] [];
           ]) );
    ( "Explicit dup and drop balance across joins",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             branch condition
               (block [] [ duplicate "a"; dropOne "a" ])
               (block [] []);
             step [ Consumed "a" ] [] [];
           ]) );
    ( "Explicit dup counts must agree across joins",
      check (Error InconsistentJoin)
        (block []
           [
             step [] [ "a" ] [];
             branch condition (block [] [ duplicate "a" ]) (block [] []);
           ]) );
    ( "Ownership contracts reject inconsistent joins",
      check (Error InconsistentJoin)
        (block []
           [
             step [] [ "a" ] [];
             branch condition (block [ "a" ] []) (block [] []);
           ]) );
    ( "Ownership contracts enforce global branch identity freshness",
      check (Error (DuplicateDefinition "a"))
        (block []
           [
             branch condition
               (block [] [ step [] [ "a" ] [ "a" ] ])
               (block [] [ step [] [ "a" ] [ "a" ] ]);
           ]) );
    ( "Ownership contracts reject double edge drop",
      check (Error (InvalidDrop "a"))
        (block []
           [
             step [] [ "a" ] [];
             branch condition (block [ "a"; "a" ] []) (block [ "a" ] []);
           ]) );
    ( "Ownership contracts transfer distinct managed block arguments",
      check (Ok ())
        (block []
           [
             managedBranch (value "c") condition
               (blockResult [] [ step [] [ "a" ] [] ] (value "a"))
               (blockResult [] [ step [] [ "b" ] [] ] (value "b"));
             step [ Consumed "c" ] [] [];
           ]) );
    ( "Managed joins preserve exclusivity from both incoming values",
      check (Ok ())
        (block []
           [
             managedBranch (value "c") condition
               (blockResult [] [ step [] [ "a" ] [] ] (value "a"))
               (blockResult [] [ step [] [ "b" ] [] ] (value "b"));
             stepWithUniqueness [ Consumed "c" ] [] [ "c" ] [] [];
           ]) );
    ( "Managed joins reject uniqueness from only one incoming value",
      check (Error (NonUniqueUse "c"))
        (block []
           [
             managedBranch (value "c") condition
               (blockResult [] [ step [] [ "a" ] [] ] (value "a"))
               (blockResult []
                  [ stepWithUniqueness [] [ "b" ] [] [] [] ]
                  (value "b"));
             stepWithUniqueness [ Consumed "c" ] [] [ "c" ] [] [];
           ]) );
    ( "Ownership contracts rename one incoming value on exclusive edges",
      check (Ok ())
        (block []
           [
             step [] [ "a" ] [];
             managedBranch (value "c") condition
               (blockResult [] [] (value "a"))
               (blockResult [] [] (value "a"));
             step [ Consumed "c" ] [] [];
           ]) );
    ( "Ownership contracts reject mixed managed block arguments",
      check (Error InconsistentBlockArgument)
        (block []
           [
             managedBranch (value "c") condition
               (blockResult [] [ step [] [ "a" ] [] ] (value "a"))
               (block [] []);
           ]) );
    ( "Ownership contracts reject a released block argument",
      check (Error (InvalidUse "a"))
        (block []
           [
             managedBranch (value "c") condition
               (blockResult [] [ step [] [ "a" ] [ "a" ] ] (value "a"))
               (blockResult [] [ step [] [ "b" ] [] ] (value "b"));
           ]) );
    ( "Ownership contracts reject incoming roots in closed regions",
      check (Error (InvalidDrop "a")) (block [ "a" ] []) );
    ( "Ownership contracts reject leaked units",
      check
        (Error (UndroppedValues (Identity.Set.singleton "a")))
        (block [] [ step [] [ "a" ] [] ]) );
    ( "Ownership contracts reject undropped duplicate units",
      check
        (Error (UndroppedValues (Identity.Set.singleton "a")))
        (block []
           [ step [] [ "a" ] []; duplicate "a"; step [ Consumed "a" ] [] [] ])
    );
  ]
