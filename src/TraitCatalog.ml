(* TraitCatalog.ml - Immutable compile-time trait obligations retained with checked units. *)
module M = StringOrder.Map

type obligation = string * AST.semanticType list
type methodTarget = { name : string; typeParams : string list }
type dispatch = { traitName : string; methodName : string; arity : int }

type implementation = {
  identity : string;
  traitName : string;
  typeParams : string list;
  typeArgs : AST.semanticType list;
  bounds : obligation list;
  methods : methodTarget M.t;
}

type t = {
  dispatches : dispatch M.t;
  implementations : implementation M.t;
  functionBounds : (string list * obligation list) M.t;
  typeBounds : (string list * obligation list) M.t;
}

let empty =
  {
    dispatches = M.empty;
    implementations = M.empty;
    functionBounds = M.empty;
    typeBounds = M.empty;
  }

let mergeMap source target =
  M.fold
    (fun name value combined ->
      match M.find_opt name combined with
      | None -> M.add name value combined
      | Some existing when existing = value -> combined
      | Some _ ->
          Crash.crash ("Inconsistent checked trait catalog entry '" ^ name ^ "'"))
    source target

let merge source target =
  {
    dispatches = mergeMap source.dispatches target.dispatches;
    implementations = mergeMap source.implementations target.implementations;
    functionBounds = mergeMap source.functionBounds target.functionBounds;
    typeBounds = mergeMap source.typeBounds target.typeBounds;
  }
