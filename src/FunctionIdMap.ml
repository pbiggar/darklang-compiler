(*
   FunctionIdMap.fs - Keep sparse function tables typed while comparing scalar ordinals.
*)
(* FunctionIdMap.ml - Keep sparse function tables typed while comparing scalar ordinals. *)
(* The representation is private: compiler passes supply semantic identities,
   while the persistent tree compares uint64 keys without boxing their wrappers. *)
module Ordinals = Map.Make(struct type t = int64 let compare = Int64.unsigned_compare end)
type 'a t = 'a Ordinals.t
let empty = Ordinals.empty
let isEmpty = Ordinals.is_empty
let count = Ordinals.cardinal
let add id value entries = Ordinals.add (AST.functionIdValue id) value entries
let remove id entries = Ordinals.remove (AST.functionIdValue id) entries
let change id update entries = Ordinals.update (AST.functionIdValue id) update entries
let tryFind id entries = Ordinals.find_opt (AST.functionIdValue id) entries
let containsKey id entries = Ordinals.mem (AST.functionIdValue id) entries
let unsigned value = if value < 0L then Z.to_string (Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)) else Int64.to_string value
let find id entries = match tryFind id entries with
  | Some value -> value | None -> Crash.crash ("Required function identity " ^ unsigned (AST.functionIdValue id) ^ " is absent")
let ofSeq entries = Seq.fold_left (fun table (id, value) -> add id value table) empty entries
let ofList entries = ofSeq (List.to_seq entries)
let ofArray entries = ofSeq (Array.to_seq entries)
let toSeq entries = Seq.map (fun (ordinal, value) -> AST.functionId ordinal, value) (Ordinals.to_seq entries)
let toList entries = List.map (fun (ordinal, value) -> AST.functionId ordinal, value) (Ordinals.bindings entries)
let keys entries = Seq.map (fun (ordinal, _) -> AST.functionId ordinal) (Ordinals.to_seq entries)
let values entries = Seq.map snd (Ordinals.to_seq entries)
let fold folder state entries = Ordinals.fold (fun ordinal value state -> folder state (AST.functionId ordinal) value) entries state
let iter action entries = Ordinals.iter (fun ordinal value -> action (AST.functionId ordinal) value) entries
let map mapping entries = Ordinals.mapi (fun ordinal value -> mapping (AST.functionId ordinal) value) entries
let filter predicate entries = Ordinals.filter (fun ordinal value -> predicate (AST.functionId ordinal) value) entries
let exists predicate entries = Ordinals.exists (fun ordinal value -> predicate (AST.functionId ordinal) value) entries
let forall predicate entries = Ordinals.for_all (fun ordinal value -> predicate (AST.functionId ordinal) value) entries
let maxKeyValue entries = match Ordinals.max_binding_opt entries with
  | None -> Crash.crash "Empty function table has no greatest identity" | Some (ordinal, value) -> AST.functionId ordinal, value
(* Overlay entries take precedence, matching the catalog merge convention. *)
let merge baseEntries overlay = fold (fun entries id value -> add id value entries) baseEntries overlay
