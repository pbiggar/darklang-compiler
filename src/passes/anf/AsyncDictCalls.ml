(* AsyncDictCalls.ml - Classify dictionary operations by the values they store or inspect. *)
type argument = Dictionary | Key | Value

type result =
  | DictionaryResult
  | ScalarResult
  | LookupResult
  | ValuesResult
  | EntriesResult

type operation = { callee : string; arguments : argument list; result : result }

let sourceConstructor name =
  List.find_opt
    (fun base -> String.starts_with ~prefix:(base ^ "_") name)
    [
      "Darklang.Stdlib.Dict.fromList";
      "Darklang.Stdlib.Dict.fromListOverwritingDuplicates";
    ]

let operation name =
  let operations =
    [
      ("singleton", [ Key; Value ], DictionaryResult);
      ("__singleton", [ Key; Value ], DictionaryResult);
      ("set", [ Dictionary; Key; Value ], DictionaryResult);
      ("setOverridingDuplicates", [ Dictionary; Key; Value ], DictionaryResult);
      ("__setOverwriting", [ Dictionary; Key; Value ], DictionaryResult);
      ("merge", [ Dictionary; Dictionary ], DictionaryResult);
      ("__merge", [ Dictionary; Dictionary ], DictionaryResult);
      ("remove", [ Dictionary; Key ], DictionaryResult);
      ("__remove", [ Dictionary; Key ], DictionaryResult);
      ("size", [ Dictionary ], ScalarResult);
      ("__size", [ Dictionary ], ScalarResult);
      ("isEmpty", [ Dictionary ], ScalarResult);
      ("__isEmpty", [ Dictionary ], ScalarResult);
      ("member", [ Dictionary; Key ], ScalarResult);
      ("__contains", [ Dictionary; Key ], ScalarResult);
      ("get", [ Dictionary; Key ], LookupResult);
      ("__get", [ Dictionary; Key ], LookupResult);
      ("keys", [ Dictionary ], ScalarResult);
      ("__keys", [ Dictionary ], ScalarResult);
      ("values", [ Dictionary ], ValuesResult);
      ("__values", [ Dictionary ], ValuesResult);
      ("toList", [ Dictionary ], EntriesResult);
      ("__entries", [ Dictionary ], EntriesResult);
    ]
  in
  List.find_map
    (fun (base, arguments, result) ->
      let callee = "Darklang.Stdlib.Dict." ^ base in
      if String.starts_with ~prefix:(callee ^ "_") name then
        Some { callee; arguments; result }
      else None)
    operations
