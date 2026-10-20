(* AsyncStorageKind.ml - Compare full instantiated source layouts, not nominal owners alone. *)
[@@@warning "-4"]

type t =
  | TupleStorage
  | RecordStorage of ANF.recordDescriptor
  | ListStorage of AST.semanticType
  | DictStorage of AST.semanticType * AST.semanticType
  | SumStorage of AsyncSumStorage.layout

let compatible left right =
  match left, right with
  | TupleStorage, TupleStorage -> true
  | ListStorage left, ListStorage right -> left = right
  | DictStorage (leftKey, leftValue), DictStorage (rightKey, rightValue) ->
      leftKey = rightKey && leftValue = rightValue
  | SumStorage left, SumStorage right -> left = right
  | RecordStorage left, RecordStorage right ->
      left.ANF.valueType = right.ANF.valueType && left.ANF.fields = right.ANF.fields
  | _ -> false

let sourceFields kind source =
  match kind, source with
  | TupleStorage, AST.TTuple types -> types
  | ListStorage _, AST.TList element -> [element]
  | DictStorage (expectedKey, expectedValue), AST.TDict (key, value)
    when expectedKey = key && expectedValue = value -> [value]
  | RecordStorage descriptor, AST.TRecord _ -> List.map snd descriptor.ANF.fields
  | SumStorage layout, AST.TSum (name, args)
    when name = layout.AsyncSumStorage.source.ANF.sumName
         && args = layout.AsyncSumStorage.source.ANF.sumArgs ->
      List.map snd layout.AsyncSumStorage.payloads
  | _ -> Crash.crash "Async transport source type does not match its storage"
