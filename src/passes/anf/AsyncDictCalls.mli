(* AsyncDictCalls.mli - Classify dictionary operations by the values they store or inspect. *)
type argument = Dictionary | Key | Value

type result =
  | DictionaryResult
  | ScalarResult
  | LookupResult
  | ValuesResult
  | EntriesResult

type operation = { callee : string; arguments : argument list; result : result }

val operation : string -> operation option
