(* Typed structural descriptions for the original ownership test diagnostics. *)
[@@@warning "-4-42"]

open Dark_compiler
open StructuralValue
module H = HIR
module O = OwnedIR

let valueId (H.ValueId id) = Union ("ValueId", [ Scalar (string_of_int id) ])

let value (value : H.value) =
  Record
    [
      ("Id", valueId value.H.id);
      ("Type", StructuralFormat.semanticValue value.H.typ);
    ]

let operand (operand : H.operand) =
  Record
    [
      ("Expression", CheckedStructuralFormat.value operand.H.expression);
      ("Type", StructuralFormat.semanticValue operand.H.typ);
      ( "Inputs",
        Union
          ( "map",
            [
              Sequence
                (List.map
                   (fun (id, input) ->
                     Tuple [ AST.DiagnosticFormatting.binding id; value input ])
                   (CheckedAST.BindingIdMap.bindings operand.H.inputs));
            ] ) );
    ]

let parameter (parameter : H.parameter) =
  Record
    [
      ("Binding", AST.DiagnosticFormatting.binding parameter.H.binding);
      ("Value", value parameter.H.value);
    ]

let signature id (signature : 'id O.functionSignature) =
  let one name value = Union (name, [ id value ]) in
  let parameter = function
    | O.UnmanagedParameter -> Union ("UnmanagedParameter", [])
    | O.BorrowedParameter value -> one "BorrowedParameter" value
    | O.ConsumedParameter value -> one "ConsumedParameter" value
    | O.UniqueParameter value -> one "UniqueParameter" value
  in
  let result =
    match signature.O.result with
    | O.UnmanagedResult -> Union ("UnmanagedResult", [])
    | O.BorrowedResult value -> one "BorrowedResult" value
    | O.ProducedResult value -> one "ProducedResult" value
    | O.UniqueProducedResult value -> one "UniqueProducedResult" value
  in
  Record
    [
      ("Parameters", Sequence (List.map parameter signature.O.parameters));
      ("Result", result);
    ]

let call (call : H.functionCall) =
  Record
    [
      ("Target", AST.DiagnosticFormatting.func call.H.target);
      ("Arguments", Sequence (List.map value call.H.arguments));
      ("Result", value call.H.result);
    ]

let rec block leaf id (block : ('leaf, 'id) O.block) =
  Record
    [
      ( "Body",
        Record
          [
            ( "Parameters",
              Sequence (List.map parameter block.O.body.H.parameters) );
            ("Operations", steps leaf id block.O.body.H.operations);
            ("Result", value block.O.body.H.result);
          ] );
    ]

and operation leaf id = function
  | H.Leaf primitive -> Union ("Leaf", [ leaf primitive ])
  | H.ScalarBinding (output, input) ->
      Union ("ScalarBinding", [ value output; operand input ])
  | H.Call input -> Union ("Call", [ call input ])
  | H.Branch (output, condition, yes, no) ->
      Union
        ( "Branch",
          [
            value output; operand condition; block leaf id yes; block leaf id no;
          ] )

and steps leaf id values =
  Sequence
    (List.map
       (function
         | O.Evaluate input -> Union ("Evaluate", [ operation leaf id input ])
         | O.Dup input -> Union ("Dup", [ id input ])
         | O.Drop input -> Union ("Drop", [ id input ]))
       values)

let functionDef leaf id (definition : ('leaf, 'id) O.functionDef) =
  Record
    [
      ( "Definition",
        Record
          [
            ("Id", AST.DiagnosticFormatting.func definition.O.definition.H.id);
            ("Name", Text definition.O.definition.H.name);
            ("Body", block leaf id definition.O.definition.H.body);
          ] );
      ("Ownership", signature id definition.O.ownership);
    ]

let option encode = function
  | None -> Union ("None", [])
  | Some value -> Union ("Some", [ encode value ])

let callSignature (signature : O.callSignature) =
  let parameter = function
    | O.UnmanagedCallParameter -> Union ("UnmanagedCallParameter", [])
    | O.BorrowedCallParameter -> Union ("BorrowedCallParameter", [])
    | O.ConsumedCallParameter -> Union ("ConsumedCallParameter", [])
    | O.UniqueCallParameter -> Union ("UniqueCallParameter", [])
  in
  let result =
    match signature.O.result with
    | O.UnmanagedCallResult -> Union ("UnmanagedCallResult", [])
    | O.BorrowedCallResult index ->
        Union ("BorrowedCallResult", [ Scalar (string_of_int index) ])
    | O.ProducedCallResult -> Union ("ProducedCallResult", [])
    | O.UniqueProducedCallResult -> Union ("UniqueProducedCallResult", [])
  in
  Record
    [
      ("Parameters", Sequence (List.map parameter signature.O.parameters));
      ("Result", result);
    ]
